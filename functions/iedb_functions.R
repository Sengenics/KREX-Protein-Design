# functions/iedb_uniprot_search.R
# ------------------------------------------------------------------------------
# IEDB API Functions - UniProt-based antigen search
# More reliable than bcell_search endpoint
# ------------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Core IEDB query function with pagination
iq_query <- function(endpoint, 
                     query_params, 
                     base_uri = 'https://query-api.iedb.org/',
                     page_size = 10000) {
  
  cat("\n=== IEDB IQ-API Query ===\n")
  cat("Endpoint:", endpoint, "\n")
  
  # Initialize
  get_text <- 'NA'
  final_tbl <- tibble()
  url <- paste0(base_uri, endpoint)
  offset <- 0
  
  # Set initial offset
  query_params[['offset']] <- format(offset, scientific = FALSE)
  
  while(get_text != '[]') {
    cat("Fetching offset:", query_params[['offset']], "\n")
    
    tryCatch({
      get_1 <- GET(url, query = query_params, timeout(30))
      
      if (status_code(get_1) != 200) {
        cat("HTTP", status_code(get_1), "error\n")
        break
      }
      
      get_text <- content(get_1, 'text')
      
      if (get_text == '[]') {
        cat("No more results\n")
        break
      }
      
      resp_tbl <- tibble(fromJSON(get_text, flatten = TRUE))
      final_tbl <- bind_rows(final_tbl, resp_tbl)
      
      offset <- offset + page_size
      query_params[['offset']] <- format(offset, scientific = FALSE)
      
      # Be nice to the server
      Sys.sleep(0.5)
      
    }, error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      break
    })
  }
  
  cat("Total rows fetched:", nrow(final_tbl), "\n")
  return(final_tbl)
}

# Search for antigens by UniProt ID
search_antigen_by_uniprot <- function(uniprot_id) {
  
  cat("\n🔍 Searching IEDB antigens for UniProt:", uniprot_id, "\n")
  
  params <- list()
  params[['parent_source_antigen_iri']] <- paste0('eq.UNIPROT:', uniprot_id)
  params[['select']] <- paste(
    'parent_source_antigen_id',
    'parent_source_antigen_iri',
    'parent_source_antigen_name',
    'parent_source_antigen_source_org_iri',
    'parent_source_antigen_source_org_name',
    sep = ','
  )
  params[['order']] <- 'parent_source_antigen_id'
  
  result <- iq_query('antigen_search', params, page_size = 1000)
  
  return(result)
}

# Search for B-cell epitopes by parent antigen IRI
search_bcell_by_antigen_iri <- function(antigen_iri, 
                                        host_organism = "Homo sapiens") {
  
  cat("\n🔍 Searching B-cell epitopes for antigen:", antigen_iri, "\n")
  
  params <- list()
  params[['parent_source_antigen_iri']] <- paste0('eq.', antigen_iri)
  params[['qualitative_measure']] <- 'eq.Positive'
  
  # Filter for human host
  if (!is.null(host_organism)) {
    # Note: We may need to filter this client-side
    params[['host_organism_name']] <- paste0('ilike.*', host_organism, '*')
  }
  
  params[['select']] <- paste(
    'bcell_id',
    'structure_id',
    'linear_sequence',
    'structure_type',
    'structure_description',
    'reference_id',
    'pubmed_id',
    'qualitative_measure',
    'assay_names',
    'host_organism_name',
    'source_organism_name',
    'curated_source_antigen.starting_position',
    'curated_source_antigen.ending_position',
    'curated_source_antigen.name',
    'curated_source_antigen.accession',
    sep = ','
  )
  params[['order']] <- 'structure_id'
  
  result <- iq_query('bcell_search', params, page_size = 1000)
  
  # Filter for human host organism client-side if needed
  if (!is.null(result) && nrow(result) > 0 && !is.null(host_organism)) {
    cat("\nFiltering for host organism:", host_organism, "\n")
    result <- result %>%
      filter(!is.na(host_organism_name),
             grepl(host_organism, host_organism_name, ignore.case = TRUE))
    cat("After host filter:", nrow(result), "results\n")
  }
  
  return(result)
}

# Main function: Get autoimmune epitopes by UniProt ID
get_autoimmune_epitopes_by_uniprot <- function(uniprot_id) {
  
  # Step 1: Find the antigen in IEDB
  antigen_data <- search_antigen_by_uniprot(uniprot_id)
  
  if (is.null(antigen_data) || nrow(antigen_data) == 0) {
    return(list(
      success = FALSE,
      data = data.frame(),
      count = 0,
      error = paste("UniProt ID", uniprot_id, "not found in IEDB antigen database")
    ))
  }
  
  # Check if it's a human protein
  human_antigens <- antigen_data %>%
    filter(grepl("Homo sapiens", parent_source_antigen_source_org_name, 
                 ignore.case = TRUE))
  
  if (nrow(human_antigens) == 0) {
    return(list(
      success = FALSE,
      data = data.frame(),
      count = 0,
      error = paste("UniProt ID", uniprot_id, "exists but is not a human protein")
    ))
  }
  
  cat("\n✓ Found", nrow(human_antigens), "human antigen(s) in IEDB\n")
  print(human_antigens %>% select(parent_source_antigen_name, 
                                  parent_source_antigen_source_org_name))
  
  # Step 2: Get B-cell epitopes for this antigen
  all_epitopes <- data.frame()
  
  for (i in 1:nrow(human_antigens)) {
    antigen_iri <- human_antigens$parent_source_antigen_iri[i]
    
    epitopes <- search_bcell_by_antigen_iri(antigen_iri, 
                                            host_organism = "Homo sapiens")
    
    if (!is.null(epitopes) && nrow(epitopes) > 0) {
      all_epitopes <- bind_rows(all_epitopes, epitopes)
    }
  }
  
  if (nrow(all_epitopes) == 0) {
    return(list(
      success = FALSE,
      data = data.frame(),
      count = 0,
      error = paste("No autoimmune epitopes found for", uniprot_id, 
                    "(human antibodies against this human protein)")
    ))
  }
  
  # Step 3: Process into standard format
  processed <- process_epitope_data(all_epitopes, uniprot_id)
  
  return(list(
    success = TRUE,
    data = processed,
    count = nrow(processed),
    error = NULL,
    source = "IEDB API"
  ))
}

# Process epitope data into standard format
process_epitope_data <- function(epitope_df, uniprot_id) {
  
  cat("\n=== Processing", nrow(epitope_df), "epitopes ===\n")
  
  processed <- epitope_df %>%
    mutate(
      # Epitope sequence
      Epitope_Sequence = if ("linear_sequence" %in% names(.)) {
        ifelse(!is.na(linear_sequence) & nchar(linear_sequence) > 0,
               as.character(linear_sequence),
               as.character(structure_description))
      } else {
        as.character(structure_description)
      },
      
      # Position
      Position = if (all(c("curated_source_antigen.starting_position", 
                           "curated_source_antigen.ending_position") %in% names(.))) {
        start <- `curated_source_antigen.starting_position`
        end <- `curated_source_antigen.ending_position`
        ifelse(!is.na(start) & !is.na(end),
               paste0(start, "-", end),
               "Unknown")
      } else {
        "Unknown"
      },
      
      # Type
      Epitope_Type = if ("structure_type" %in% names(.)) {
        type_val <- tolower(as.character(structure_type))
        ifelse(grepl("linear", type_val), "linear",
               ifelse(grepl("discontinuous", type_val), "conformational", "linear"))
      } else {
        "linear"
      },
      
      # Evidence level
      Evidence_Level = if ("qualitative_measure" %in% names(.)) {
        qual <- tolower(as.character(qualitative_measure))
        ifelse(grepl("positive-high|positive.high", qual), 5,
               ifelse(grepl("positive-intermediate|positive-medium", qual), 4, 3))
      } else {
        3
      },
      
      # Citation
      Citation = if ("pubmed_id" %in% names(.)) {
        pmid <- pubmed_id
        ref_id <- if("reference_id" %in% names(.)) reference_id else NA
        ifelse(!is.na(pmid), 
               paste0("PMID: ", pmid),
               paste0("IEDB Ref: ", ref_id))
      } else {
        "IEDB"
      },
      
      # Antibody context
      Antibody_Context = if ("assay_names" %in% names(.)) {
        assay_info <- sapply(assay_names, function(x) {
          if (is.list(x) && length(x) > 0) {
            paste(unlist(x), collapse = "; ")
          } else if (is.character(x)) {
            x
          } else {
            "B-cell assay"
          }
        })
        paste0("Human autoantibody; ", assay_info)
      } else {
        "Human autoantibody; B-cell assay"
      },
      
      uniprot_id = uniprot_id,
      Source = "IEDB API"
    )
  
  # Select and clean
  final <- processed %>%
    select(uniprot_id, Epitope_Sequence, Position, Epitope_Type,
           Evidence_Level, Citation, Antibody_Context, Source) %>%
    filter(!is.na(Epitope_Sequence),
           nchar(as.character(Epitope_Sequence)) > 0,
           !Epitope_Sequence %in% c("Unknown", "NA", "")) %>%
    distinct(Epitope_Sequence, Position, .keep_all = TRUE) %>%
    arrange(desc(Evidence_Level), Position)
  
  cat("Processed to", nrow(final), "unique autoimmune epitopes\n")
  
  return(final)
}