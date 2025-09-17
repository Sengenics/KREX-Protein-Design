# UniProt Field Selector and Enhanced Search Function
library(shiny)
library(jsonlite)

# Complete list of UniProt fields available via API
# Fixed UniProt Field Selector - separates display names from field codes

# Human-readable field descriptions (for display only)
uniprot_fields_display <- list(
  # Basic Information
  "accession" = "Accession ID (UniProt)",
  "id" = "Entry ID",
  "protein_name" = "Protein Name",
  "gene_names" = "Gene Symbol",
  "organism_name" = "Organism of Origin",
  "organism_id" = "Taxonomy ID",
  "reviewed" = "Reviewed Status (SwissProt/TrEMBL)",
  
  # Sequence Information
  "sequence" = "Protein Sequence",
  "length" = "Protein Length (aa)",
  "mass" = "Molecular Weight",
  "ft_signal" = "Signal Peptide",
  "ft_propep" = "Propeptide",
  "ft_transit" = "Transit Peptide",
  "ft_transmem" = "Transmembrane Domain",
  "ft_topo_dom" = "Topological Domain",
  "ft_intramem" = "Intramembrane Region",
  
  # Functional Information
  "cc_function" = "Function",
  "cc_activity_regulation" = "Activity Regulation",
  "cc_catalytic_activity" = "Catalytic Activity",
  "cc_cofactor" = "Cofactor",
  "cc_pathway" = "Pathway",
  "ft_act_site" = "Active Site",
  "ft_binding" = "Binding Site",
  "ft_site" = "Site",
  "ft_metal" = "Metal Binding",
  
  # Structural Information
  "cc_subunit" = "Subunit Structure/Multimerization",
  "ft_domain" = "Domain",
  "ft_region" = "Region",
  "ft_motif" = "Motif",
  "ft_repeat" = "Repeat",
  "ft_coiled" = "Coiled Coil",
  "ft_compbias" = "Compositional Bias",
  "structure_3d" = "3D Structure",
  
  # Subcellular Location
  "cc_subcellular_location" = "Subcellular Location",
  "cc_membrane" = "Membrane Information",
  "ft_membrane" = "Membrane Features",
  
  # Expression and Interactions
  "cc_tissue_specificity" = "Tissue Specificity",
  "cc_developmental_stage" = "Developmental Stage",
  "cc_induction" = "Induction",
  "cc_interaction" = "Protein Interactions",
  
  # Disease and Variants
  "cc_disease" = "Disease Association",
  "ft_variant" = "Natural Variants",
  "ft_mutagen" = "Mutagenesis",
  "ft_conflict" = "Sequence Conflicts",
  
  # Post-translational Modifications
  "ft_mod_res" = "Modified Residue",
  "ft_lipid" = "Lipidation",
  "ft_carbohyd" = "Glycosylation",
  "ft_disulfid" = "Disulfide Bond",
  "ft_crosslnk" = "Cross-link",
  
  # Immunological Information
  "ft_antigen" = "Antigenic Regions",
  "cc_allergen" = "Allergen Information",
  "cc_toxic_dose" = "Toxic Dose",
  
  # Cross-references
  "xref_embl" = "EMBL/GenBank References",
  "xref_pdb" = "PDB Structure References",
  "xref_go" = "Gene Ontology",
  "xref_interpro" = "InterPro Domains",
  "xref_pfam" = "Pfam Domains",
  
  # Additional Fields
  "annotation_score" = "Annotation Score",
  "protein_existence" = "Protein Evidence",
  "tools" = "Analysis Tools",
  "keywords" = "Keywords",
  "ec" = "EC Number",
  "absorption" = "Absorption Properties",
  "kinetics" = "Kinetic Parameters",
  "ph_dependence" = "pH Dependence",
  "redox_potential" = "Redox Potential",
  "temperature_dependence" = "Temperature Dependence"
)


# base_fields = c(
#   "accession",              # Accession ID (e.g., UniProt)
#   "gene_names" = "Gene Symbol",
# )
# Default fields for protein design (field codes only)
default_selected_fields <- c(
  "protein_name",           # Protein (Name/Symbol)
  "accession",              # Accession ID (e.g., UniProt)
  "organism_name",          # Organism of Origin
  "cc_subcellular_location", # For determining if secreted
  "cc_membrane",            # Membrane Protein information
  "ft_transmem",            # Transmembrane Domain Present
  "length",                 # Original Protein Length (aa)
  "ft_signal",              # Signal Peptide Present
  "ft_propep",              # Proprotein information
  "cc_subunit",             # Functional Multimer information
  "ft_domain",              # Known Active Sites/Functional Domains
  "ft_act_site",            # Active sites
  "ft_antigen",             # Known Immunodominant Epitope Locations
  "sequence",               # Protein sequence
  "xref_embl"               # For nucleotide sequence references
)

# Fixed UI for field selector - displays human names, returns field codes
uniprotFieldSelectorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Select UniProt Fields to Retrieve"),
    
    div(
      style = "margin-bottom: 15px;",
      actionButton(ns("select_default"), "Select Protein Design Fields", 
                   class = "btn-primary btn-sm"),
      actionButton(ns("select_all"), "Select All Fields", 
                   class = "btn-secondary btn-sm"),
      actionButton(ns("clear_all"), "Clear All", 
                   class = "btn-outline-secondary btn-sm")
    ),
    
    # KEY FIX: Use names() as choices, values as names
    selectizeInput(
      ns("selected_fields"),
      "Available UniProt Fields:",
      choices = setNames(names(uniprot_fields_display), unlist(uniprot_fields_display)),
      selected = default_selected_fields,
      multiple = TRUE,
      options = list(
        placeholder = "Type to search fields...",
        plugins = list("remove_button"),
        maxItems = NULL
      )
    ),
    
    div(
      style = "color: #666; font-size: 0.9em; margin-top: 5px;",
      textOutput(ns("field_count"))
    )
  )
}

# Fixed server for field selector - returns actual field codes
uniprotFieldSelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Quick selection handlers
    observeEvent(input$select_default, {
      updateSelectizeInput(session, "selected_fields", selected = default_selected_fields)
    })
    
    observeEvent(input$select_all, {
      updateSelectizeInput(session, "selected_fields", selected = names(uniprot_fields_display))
    })
    
    observeEvent(input$clear_all, {
      updateSelectizeInput(session, "selected_fields", selected = character(0))
    })
    
    # Show field count
    output$field_count <- renderText({
      count <- length(input$selected_fields)
      paste("Selected:", count, "fields")
    })
    
    # Return selected field codes (not display names)
    return(reactive({ 
      selected_codes <- input$selected_fields
      cat("Selected field codes:", paste(selected_codes, collapse = ", "), "\n")  # Debug
      selected_codes
    }))
  })
}

# Test function to verify the fix
test_field_selector <- function() {
  # Test the choices setup
  choices <- setNames(names(uniprot_fields_display), unlist(uniprot_fields_display))
  
  cat("Example choices (first 3):\n")
  print(head(choices, 3))
  
  cat("\nField codes (values):", names(choices)[1:3], "\n")
  cat("Display names (names):", as.character(choices)[1:3], "\n")
  
  return(choices)
}

# Run test
# test_field_selector()
# uniprotFieldSelectorServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     
#     # DEBUG: Print when server starts
#     cat("=== SERVER STARTED for module:", id, "===\n")
#     
#     # DEBUG: Monitor all inputs
#     observe({
#       cat("Current selected_fields length:", length(input$selected_fields), "\n")
#       cat("Current selected_fields:", paste(input$selected_fields, collapse = ", "), "\n")
#     })
#     
#     # Quick selection handlers with extensive debugging
#     observeEvent(input$select_default, {
#       cat(">>> DEFAULT BUTTON CLICKED <<<\n")
#       cat("About to update with:", length(default_selected_fields), "fields\n")
#       
#       tryCatch({
#         updateSelectizeInput(session, "selected_fields", selected = default_selected_fields)
#         cat("Update completed successfully\n")
#       }, error = function(e) {
#         cat("ERROR in update:", e$message, "\n")
#       })
#     })
#     
#     observeEvent(input$select_all, {
#       cat(">>> SELECT ALL BUTTON CLICKED <<<\n")
#       all_fields <- names(uniprot_fields)
#       cat("About to update with:", length(all_fields), "fields\n")
#       
#       tryCatch({
#         updateSelectizeInput(session, "selected_fields", selected = all_fields)
#         cat("Update completed successfully\n")
#       }, error = function(e) {
#         cat("ERROR in update:", e$message, "\n")
#       })
#     })
#     
#     observeEvent(input$clear_all, {
#       cat(">>> CLEAR ALL BUTTON CLICKED <<<\n")
#       
#       tryCatch({
#         updateSelectizeInput(session, "selected_fields", selected = character(0))
#         cat("Clear completed successfully\n")
#       }, error = function(e) {
#         cat("ERROR in clear:", e$message, "\n")
#       })
#     })
#     
#     # Show field count
#     output$field_count <- renderText({
#       count <- length(input$selected_fields)
#       paste("Selected:", count, "fields")
#     })
#     
#     # Return selected fields
#     return(reactive({ input$selected_fields }))
#   })
# }

uniprotFieldSelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$select_default, {
      updateCheckboxGroupInput(session, "selected_fields", selected = default_selected_fields)
    })
    
    observeEvent(input$select_all, {
      all_choices <- c("accession", "protein_name", "gene_names", "organism_name", 
                       "length", "sequence", "cc_subcellular_location", 
                       "ft_signal", "ft_transmem", "ft_domain")
      updateCheckboxGroupInput(session, "selected_fields", selected = all_choices)
    })
    
    return(reactive({ input$selected_fields }))
  })
}

# Function to update your server.R
update_server_search_section <- function() {
  # Replace your search_section reactive with this:
  search_section <- reactive({
    req(input$uniprot_select)
    
    uniprot_ids <- input$uniprot_select
    selected_fields <- if(!is.null(input$field_selector)) {
      input$field_selector
    } else {
      default_selected_fields
    }
    
    # Use the new simplified function
    result_df <- get_uniprot_info_simple(uniprot_ids, selected_fields)
    return(result_df)
  })
}

  
fetch_uniprot_info <- function(uniprot_id) {
    base_url <- "https://rest.uniprot.org/uniprotkb/search"
    
    # Choose the fields you need
    fields <- paste(c("accession", "id", "protein_name", "organism_name",
                      "length", "comment(FUNCTION)"), collapse = ",")
    
    url <- paste0(base_url, "?query=accession:", uniprot_id,
                  "&fields=", fields, "&format=json")
    
    resp <- GET(url)
    if (status_code(resp) != 200) {
      return(data.frame(
        uniprot_id = uniprot_id,
        error = paste("Request failed with status", status_code(resp)),
        stringsAsFactors = FALSE
      ))
    }
    
    txt <- content(resp, "text", encoding = "UTF-8")
    js <- fromJSON(txt)
    
    if (length(js$results) == 0) {
      return(data.frame(
        uniprot_id = uniprot_id,
        error = paste("UniProt ID not found:", uniprot_id),
        stringsAsFactors = FALSE
      ))
    }
    
    # Flatten selected fields
    res <- js$results[[1]]
    data.frame(
      uniprot_id   = res$primaryAccession,
      entry_id     = res$uniProtkbId,
      protein_name = paste(res$proteinDescription$recommendedName$fullName$value, collapse = "; "),
      organism     = res$organism$scientificName#,
      # length       = res$sequence$length,
      # function     = if (!is.null(res$comments) && nrow(res$comments) > 0) {
      #   paste(res$comments$value, collapse = " ")
      # } else NA_character_,
      # stringsAsFactors = FALSE
    )
  }
  



# Simplified and more reliable UniProt search function with better debugging
get_uniprot_info_data <- function(uniprot_id) {
  tryCatch({
    # Clean the input
    uniprot_id <- trimws(uniprot_id)
    
    # UniProt REST API endpoint
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    
    # Make the API request
    response <- url(url)
    on.exit(close(response))
    
    json_text <- readLines(response, warn = FALSE)
    
    if (length(json_text) == 0) {
      return(data.frame(
        uniprot_id = uniprot_id,
        error = paste("UniProt ID not found:", uniprot_id),
        stringsAsFactors = FALSE
      ))
    }
    
    # Parse JSON response
    data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
  })
}
  
  # safe_collapse <- function(x, sep = "; ") {
  #   if (is.null(x) || length(x) == 0) {
  #     return("None")
  #   }
  #   paste(x, collapse = sep)
  # }
  # 
  # 
  # safe_collapse_synonyms <- function(synonyms_list, sep = "; ") {
  #   tryCatch({
  #     if (is.null(synonyms_list) || length(synonyms_list) == 0) {
  #       return("None")
  #     }
  #     
  #     # Extract the first element (should be a data frame)
  #     synonym_df <- synonyms_list[[1]]
  #     
  #     if (is.null(synonym_df) || !("value" %in% names(synonym_df))) {
  #       return("None")
  #     }
  #     
  #     # Extract the value column and collapse
  #     values <- synonym_df$value
  #     if (length(values) == 0) {
  #       return("None")
  #     }
  #     
  #     paste(values, collapse = sep)
  #     
  #   }, error = function(e) {
  #     return("Error parsing synonyms")
  #   })
  # }
  # 
  uniprot_data_parse = function(data){
    df = data.frame(
      entryType = safe_extract(data$entryType),
      primaryAccession = safe_extract(data$primaryAccession),
      secondaryAccessions = safe_collapse(data$secondaryAccessions, "; "),
      uniProtkbId = safe_extract(data$uniProtkbId),
      organism_scientificName = safe_extract(data$organism$scientificName),
      organism_commonName = safe_extract(data$organism$commonName),
      organism_taxonId = safe_extract(data$organism$taxonId),
      protein_name = safe_extract(data$proteinDescription$recommendedName$fullName$value),
      cdAntigenNames = safe_extract(data$proteinDescription$cdAntigenNames),
      genes_geneName = safe_extract(data$genes$geneName$value),
      genes_synonyms = safe_collapse_synonyms(data$genes$synonyms, '; '),
      protein_sequence = safe_extract(data$sequence$value),
      protein_length = safe_extract(data$sequence$length),
      protein_molWeight = safe_extract(data$sequence$molWeight),
      EMBL = safe_collapse(data$uniProtKBCrossReferences$id[data$uniProtKBCrossReferences$database == "EMBL"], "; "),
      PDB = safe_collapse(data$uniProtKBCrossReferences$id[data$uniProtKBCrossReferences$database == "PDB"], "; "),
      AlphaFoldDB = safe_collapse(data$uniProtKBCrossReferences$id[data$uniProtKBCrossReferences$database == "AlphaFoldDB"], "; "),

      stringsAsFactors = F
    )
    df
  }

# Robust UniProt Data Extraction Functions
# These functions safely handle NULL values from UniProt API responses

# Safe extraction function - returns default value if field is NULL or empty
safe_extract <- function(value, default = "Not available") {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(default)
  }
  return(value)
}

# Safe collapse function for lists/vectors
safe_collapse <- function(values, sep = "; ", default = "Not available") {
  if (is.null(values) || length(values) == 0 || all(is.na(values))) {
    return(default)
  }
  # Remove NAs and empty strings
  clean_values <- values[!is.na(values) & values != ""]
  if (length(clean_values) == 0) {
    return(default)
  }
  return(paste(clean_values, collapse = sep))
}

# Safe collapse for gene synonyms (handles nested structure)
safe_collapse_synonyms <- function(synonyms_list, sep = "; ", default = "Not available") {
  if (is.null(synonyms_list) || length(synonyms_list) == 0) {
    return(default)
  }
  
  # Extract values from nested structure
  values <- tryCatch({
    if (is.list(synonyms_list)) {
      unlist(lapply(synonyms_list, function(x) x$value))
    } else {
      synonyms_list
    }
  }, error = function(e) NULL)
  
  return(safe_collapse(values, sep, default))
}

# Safe extraction for nested protein name
safe_extract_protein_name <- function(protein_desc, default = "Not available") {
  tryCatch({
    # Try recommended name first
    if (!is.null(protein_desc$recommendedName$fullName$value)) {
      return(protein_desc$recommendedName$fullName$value)
    }
    # Try submitted names
    if (!is.null(protein_desc$submissionNames) && length(protein_desc$submissionNames) > 0) {
      if (!is.null(protein_desc$submissionNames[[1]]$fullName$value)) {
        return(protein_desc$submissionNames[[1]]$fullName$value)
      }
    }
    # Try alternative names
    if (!is.null(protein_desc$alternativeNames) && length(protein_desc$alternativeNames) > 0) {
      if (!is.null(protein_desc$alternativeNames[[1]]$fullName$value)) {
        return(protein_desc$alternativeNames[[1]]$fullName$value)
      }
    }
    return(default)
  }, error = function(e) default)
}

# Safe extraction for gene name (handles nested structure)
safe_extract_gene_name <- function(genes_list, default = "Not available") {
  tryCatch({
    if (is.null(genes_list) || length(genes_list) == 0) {
      return(default)
    }
    
    # Get first gene entry
    first_gene <- genes_list[[1]]
    if (!is.null(first_gene$geneName$value)) {
      return(first_gene$geneName$value)
    }
    
    # Try ordered locus names if gene name not available
    if (!is.null(first_gene$orderedLocusNames) && length(first_gene$orderedLocusNames) > 0) {
      if (!is.null(first_gene$orderedLocusNames[[1]]$value)) {
        return(first_gene$orderedLocusNames[[1]]$value)
      }
    }
    
    return(default)
  }, error = function(e) default)
}

# Safe extraction for cross-references by database
safe_extract_xrefs <- function(cross_refs, database_name, sep = "; ", default = "Not available") {
  tryCatch({
    if (is.null(cross_refs) || length(cross_refs) == 0) {
      return(default)
    }
    
    # Filter by database
    matching_refs <- cross_refs[cross_refs$database == database_name, ]
    
    if (nrow(matching_refs) == 0) {
      return(default)
    }
    
    return(safe_collapse(matching_refs$id, sep, default))
    
  }, error = function(e) default)
}

# Enhanced get_uniprot_info function with robust error handling
# uniprot_data_parse <- function(data) {
# 
#     
#     # Create robust data frame with safe extractions
#     df <- data.frame(
#       uniprot_id = uniprot_id,
#       entryType = safe_extract(data$entryType),
#       primaryAccession = safe_extract(data$primaryAccession),
#       secondaryAccessions = safe_collapse(data$secondaryAccessions),
#       uniProtkbId = safe_extract(data$uniProtkbId),
#       organism_scientificName = safe_extract(data$organism$scientificName),
#       organism_commonName = safe_extract(data$organism$commonName),
#       organism_taxonId = safe_extract(data$organism$taxonId, NA_integer_),
#       protein_name = safe_extract_protein_name(data$proteinDescription$),
#       cdAntigenNames = safe_collapse(data$proteinDescription$cdAntigenNames),
#       genes_geneName = safe_extract(data$genes$geneName$value),
#       genes_synonyms = safe_collapse_synonyms(data$genes$synonyms),
#       protein_sequence = safe_extract(data$sequence$value),
#       protein_length = safe_extract(data$sequence$length, NA_integer_),
#       protein_molWeight = safe_extract(data$sequence$molWeight, NA_real_),
#       EMBL = safe_extract_xrefs(data$uniProtKBCrossReferences, "EMBL"),
#       PDB = safe_extract_xrefs(data$uniProtKBCrossReferences, "PDB"),
#       AlphaFoldDB = safe_extract_xrefs(data$uniProtKBCrossReferences, "AlphaFoldDB"),
#       stringsAsFactors = FALSE
#     )
#     
#     return(df)
#     
# 
# }

# Batch processing function
get_uniprot_batch_robust <- function(uniprot_ids) {
  # Remove empty/NA values
  uniprot_ids <- uniprot_ids[!is.na(uniprot_ids) & uniprot_ids != ""]
  
  if (length(uniprot_ids) == 0) {
    return(data.frame())
  }
  
  # Process each ID
  results_list <- lapply(uniprot_ids, get_uniprot_info_robust)
  
  # Combine all data frames
  combined_df <- do.call(rbind, results_list)
  
  return(combined_df)
}

# Example usage:
# Single protein
# result <- get_uniprot_info_robust("P04637")
# print(result)

# Multiple proteins
# batch_result <- get_uniprot_batch_robust(c("P04637", "Q9Y6K9", "P53_HUMAN"))
# print(batch_result)
  
  truncate_long_text <- function(df, max_chars = 100, exclude_cols = c()) {
    for (col in colnames(df)) {
      if (!col %in% exclude_cols && is.character(df[[col]])) {
        df[[col]] <- sapply(df[[col]], function(x) {
          if (!is.na(x) && nchar(x) > max_chars) {
            paste0(substr(x, 1, max_chars), "...")
          } else {
            x
          }
        })
      }
    }
    return(df)
  }
    
# uniprot_old = function(){
#     # Debug: print selected fields
#     cat("Processing fields for", uniprot_id, ":\n")
#     cat("Selected fields:", paste(selected_fields, collapse = ", "), "\n")
#     
#     # Initialize result data frame with uniprot_id
#     result_df <- data.frame(uniprot_id = uniprot_id, stringsAsFactors = FALSE)
#     
#     # Extract data based on selected fields - with explicit debugging
#     for (field in selected_fields) {
#       cat("Processing field:", field, "\n")
#       field_value <- extract_uniprot_field_debug(data, field)
#       cat("Result for", field, ":", field_value, "\n")
#       result_df[[field]] <- field_value
#     }
#     
#     return(result_df)
#     
#   }, error = function(e) {
#     cat("Error occurred:", e$message, "\n")
#     return(data.frame(
#       uniprot_id = uniprot_id,
#       error = paste("Error retrieving data:", e$message),
#       stringsAsFactors = FALSE
#     ))
#   })
# }

# Debug version of extraction function
extract_uniprot_field_debug <- function(data, field) {
  cat("  -> Extracting field:", field, "\n")
  
  # Direct test of basic fields first
  if (field == "accession") {
    result <- if (!is.null(data$primaryAccession)) data$primaryAccession else "Not available"
    cat("    Primary accession found:", result, "\n")
    return(result)
  }
  
  if (field == "id") {
    result <- if (!is.null(data$uniProtkbId)) data$uniProtkbId else "Not available"
    cat("    UniProt ID found:", result, "\n")
    return(result)
  }
  
  if (field == "protein_name") {
    if (!is.null(data$proteinDescription) && 
        !is.null(data$proteinDescription$recommendedName) &&
        !is.null(data$proteinDescription$recommendedName$fullName) &&
        !is.null(data$proteinDescription$recommendedName$fullName$value)) {
      result <- data$proteinDescription$recommendedName$fullName$value
      cat("    Protein name found:", result, "\n")
      return(result)
    } else {
      cat("    Protein name structure not found\n")
      return("Not available")
    }
  }
  
  if (field == "gene_names") {
    if (!is.null(data$genes) && length(data$genes) > 0 &&
        !is.null(data$genes[[1]]$geneName) &&
        !is.null(data$genes[[1]]$geneName$value)) {
      result <- data$genes[[1]]$geneName$value
      cat("    Gene name found:", result, "\n")
      return(result)
    } else {
      cat("    Gene name structure not found\n")
      return("Not available")
    }
  }
  
  if (field == "organism_name") {
    if (!is.null(data$organism) && !is.null(data$organism$scientificName)) {
      result <- data$organism$scientificName
      cat("    Organism name found:", result, "\n")
      return(result)
    } else {
      cat("    Organism name not found\n")
      return("Not available")
    }
  }
  
  if (field == "organism_id") {
    if (!is.null(data$organism) && !is.null(data$organism$taxonId)) {
      result <- as.character(data$organism$taxonId)
      cat("    Organism ID found:", result, "\n")
      return(result)
    } else {
      cat("    Organism ID not found\n")
      return("Not available")
    }
  }
  
  # For any other field
  cat("    Field not implemented:", field, "\n")
  return(paste("Field not implemented:", field))
}

# Fixed extraction function based on actual UniProt JSON structure
extract_uniprot_field_simple <- function(data, field) {
  result <- tryCatch({
    switch(field,
           "accession" = {
             if (!is.null(data$primaryAccession)) {
               data$primaryAccession
             } else "Not available"
           },
           "id" = {
             if (!is.null(data$uniProtkbId)) {
               data$uniProtkbId
             } else "Not available"
           },
           "protein_name" = {
             # Based on your debug output: proteinDescription$recommendedName$fullName$value
             if (!is.null(data$proteinDescription) && 
                 !is.null(data$proteinDescription$recommendedName) &&
                 !is.null(data$proteinDescription$recommendedName$fullName) &&
                 !is.null(data$proteinDescription$recommendedName$fullName$value)) {
               data$proteinDescription$recommendedName$fullName$value
             } else if (!is.null(data$proteinDescription$alternativeNames) && 
                        length(data$proteinDescription$alternativeNames) > 0 &&
                        !is.null(data$proteinDescription$alternativeNames[[1]]$fullName$value)) {
               data$proteinDescription$alternativeNames[[1]]$fullName$value
             } else "Not available"
           },
           "gene_names" = {
             if (!is.null(data$genes) && length(data$genes) > 0 &&
                 !is.null(data$genes[[1]]$geneName) &&
                 !is.null(data$genes[[1]]$geneName$value)) {
               data$genes[[1]]$geneName$value
             } else "Not available"
           },
           "organism_name" = {
             if (!is.null(data$organism) && !is.null(data$organism$scientificName)) {
               data$organism$scientificName
             } else "Not available"
           },
           "organism_id" = {
             if (!is.null(data$organism) && !is.null(data$organism$taxonId)) {
               as.character(data$organism$taxonId)
             } else "Not available"
           },
           "reviewed" = {
             if (!is.null(data$entryType)) {
               data$entryType
             } else "Not available"
           },
           "sequence" = {
             if (!is.null(data$sequence) && !is.null(data$sequence$value)) {
               data$sequence$value
             } else "Not available"
           },
           "length" = {
             if (!is.null(data$sequence) && !is.null(data$sequence$length)) {
               data$sequence$length
             } else NA_integer_
           },
           "mass" = {
             if (!is.null(data$sequence) && !is.null(data$sequence$molWeight)) {
               data$sequence$molWeight
             } else NA_real_
           },
           "cc_function" = {
             # Extract FUNCTION comment
             extract_comment_by_type(data, "FUNCTION")
           },
           "cc_subcellular_location" = {
             # Extract SUBCELLULAR LOCATION comment
             extract_comment_by_type(data, "SUBCELLULAR LOCATION")
           },
           "ft_signal" = {
             # Extract SIGNAL feature
             extract_feature_by_type(data, "SIGNAL")
           },
           "ft_transmem" = {
             # Extract TRANSMEM feature
             extract_feature_by_type(data, "TRANSMEM")
           },
           "xref_embl" = {
             # Extract EMBL cross-references
             extract_cross_ref_by_db(data, "EMBL")
           },
           # Default for unimplemented fields
           "Not implemented yet"
    )
  }, error = function(e) {
    paste("Error extracting", field, ":", e$message)
  })
  
  return(result)
}

# Helper function to extract comments by type
extract_comment_by_type <- function(data, comment_type) {
  if (!is.null(data$comments) && length(data$comments) > 0) {
    for (i in seq_along(data$comments)) {
      comment <- data$comments[[i]]
      if (!is.null(comment$commentType) && comment$commentType == comment_type) {
        if (!is.null(comment$texts) && length(comment$texts) > 0 &&
            !is.null(comment$texts[[1]]$value)) {
          return(comment$texts[[1]]$value)
        }
      }
    }
  }
  return("Not available")
}

# Helper function to extract features by type  
extract_feature_by_type <- function(data, feature_type) {
  if (!is.null(data$features) && length(data$features) > 0) {
    locations <- c()
    for (i in seq_along(data$features)) {
      feature <- data$features[[i]]
      if (!is.null(feature$type) && feature$type == feature_type) {
        start <- feature$location$start$value
        end <- feature$location$end$value
        if (!is.null(start) && !is.null(end)) {
          locations <- c(locations, paste0(start, "-", end))
        }
      }
    }
    if (length(locations) > 0) {
      return(paste(locations, collapse = "; "))
    }
  }
  return("Not available")
}

# Helper function to extract cross-references by database
extract_cross_ref_by_db <- function(data, database) {
  if (!is.null(data$uniProtKBCrossReferences) && length(data$uniProtKBCrossReferences) > 0) {
    ids <- c()
    for (i in seq_along(data$uniProtKBCrossReferences)) {
      ref <- data$uniProtKBCrossReferences[[i]]
      if (!is.null(ref$database) && ref$database == database) {
        ids <- c(ids, ref$id)
      }
    }
    if (length(ids) > 0) {
      return(paste(ids, collapse = "; "))
    }
  }
  return("Not available")
}

# Helper function to extract specific fields from UniProt data
extract_uniprot_field <- function(data, field) {
  switch(field,
         "accession" = ifelse(length(data$primaryAccession) > 0, data$primaryAccession, "Not available"),
         "id" = ifelse(length(data$uniProtkbId) > 0, data$uniProtkbId, "Not available"),
         "protein_name" = extract_protein_name(data),
         "gene_names" = extract_gene_names(data),
         "organism_name" = ifelse(length(data$organism$scientificName) > 0, data$organism$scientificName, "Not available"),
         "organism_id" = ifelse(length(data$organism$taxonId) > 0, data$organism$taxonId, "Not available"),
         "reviewed" = ifelse(length(data$entryType) > 0, data$entryType, "Not available"),
         "sequence" = ifelse(length(data$sequence$value) > 0, data$sequence$value, "Not available"),
         "length" = ifelse(length(data$sequence$length) > 0, data$sequence$length, NA),
         "mass" = ifelse(length(data$sequence$molWeight) > 0, data$sequence$molWeight, NA),
         "ft_signal" = extract_feature(data, "Signal peptide"),
         "ft_transmem" = extract_feature(data, "Transmembrane"),
         "ft_domain" = extract_feature(data, "Domain"),
         "ft_act_site" = extract_feature(data, "Active site"),
         "cc_function" = extract_comment(data, "FUNCTION"),
         "cc_subcellular_location" = extract_comment(data, "SUBCELLULAR LOCATION"),
         "cc_subunit" = extract_comment(data, "SUBUNIT"),
         "xref_embl" = extract_cross_references(data, "EMBL"),
         # Default case
         "Not available"
  )
}

# Helper functions for specific extractions
extract_protein_name <- function(data) {
  tryCatch({
    # Try recommended name first
    if (!is.null(data$proteinDescription$recommendedName$fullName$value)) {
      return(data$proteinDescription$recommendedName$fullName$value)
    }
    # Try submitted names
    if (!is.null(data$proteinDescription$submissionNames) && length(data$proteinDescription$submissionNames) > 0) {
      return(data$proteinDescription$submissionNames[[1]]$fullName$value)
    }
    # Try alternative names
    if (!is.null(data$proteinDescription$alternativeNames) && length(data$proteinDescription$alternativeNames) > 0) {
      return(data$proteinDescription$alternativeNames[[1]]$fullName$value)
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_gene_names <- function(data) {
  tryCatch({
    if (!is.null(data$genes) && length(data$genes) > 0) {
      # Get primary gene name
      if (!is.null(data$genes[[1]]$geneName$value)) {
        return(data$genes[[1]]$geneName$value)
      }
      # Try ordered locus names
      if (!is.null(data$genes[[1]]$orderedLocusNames) && length(data$genes[[1]]$orderedLocusNames) > 0) {
        return(data$genes[[1]]$orderedLocusNames[[1]]$value)
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_feature <- function(data, feature_type) {
  tryCatch({
    if (!is.null(data$features) && length(data$features) > 0) {
      # Filter features by type
      matching_features <- data$features[sapply(data$features, function(f) f$type == feature_type)]
      
      if (length(matching_features) > 0) {
        locations <- sapply(matching_features, function(feature) {
          start <- feature$location$start$value
          end <- feature$location$end$value
          if (!is.null(start) && !is.null(end)) {
            return(paste0(start, "-", end))
          } else if (!is.null(start)) {
            return(as.character(start))
          }
          return("unknown")
        })
        return(paste(locations, collapse = "; "))
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_comment <- function(data, comment_type) {
  tryCatch({
    if (!is.null(data$comments) && length(data$comments) > 0) {
      # Find matching comment type
      matching_comments <- data$comments[sapply(data$comments, function(c) c$commentType == comment_type)]
      
      if (length(matching_comments) > 0) {
        comment <- matching_comments[[1]]
        
        # Handle different comment structures
        if (!is.null(comment$texts) && length(comment$texts) > 0) {
          return(comment$texts[[1]]$value)
        } else if (!is.null(comment$subcellularLocations)) {
          # Special handling for subcellular location
          locations <- sapply(comment$subcellularLocations, function(loc) {
            if (!is.null(loc$location$value)) {
              return(loc$location$value)
            }
            return("unknown")
          })
          return(paste(locations, collapse = "; "))
        }
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_cross_references <- function(data, database) {
  tryCatch({
    if (!is.null(data$uniProtKBCrossReferences) && length(data$uniProtKBCrossReferences) > 0) {
      # Filter by database
      matching_refs <- data$uniProtKBCrossReferences[sapply(data$uniProtKBCrossReferences, function(ref) ref$database == database)]
      
      if (length(matching_refs) > 0) {
        ids <- sapply(matching_refs, function(ref) ref$id)
        return(paste(ids, collapse = "; "))
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

determine_secreted_status <- function(subcellular_location) {
  if (is.na(subcellular_location) || subcellular_location == "Not available") {
    return("Unknown")
  }
  
  secreted_keywords <- c("Secreted", "Extracellular", "Cell surface", "Membrane; Single-pass")
  
  if (any(sapply(secreted_keywords, function(x) grepl(x, subcellular_location, ignore.case = TRUE)))) {
    return("Y")
  }
  
  return("N")
}

# Example usage in your Shiny app:
# 
# ui <- fluidPage(
#   uniprotFieldSelectorUI("field_selector"),
#   hr(),
#   textInput("uniprot_id", "UniProt ID:", value = "P04637"),
#   actionButton("search", "Search"),
#   hr(),
#   DT::dataTableOutput("results")
# )
# 
# server <- function(input, output, session) {
#   selected_fields <- uniprotFieldSelectorServer("field_selector")
#   
#   observeEvent(input$search, {
#     result <- get_uniprot_info_enhanced(input$uniprot_id, selected_fields())
#     output$results <- DT::renderDataTable({
#       DT::datatable(result, options = list(scrollX = TRUE))
#     })
#   })
# }


# Main function to get UniProt data using field-specific API
get_uniprot_info_simple <- function(uniprot_ids, selected_fields = default_selected_fields) {
  # Handle single ID or multiple IDs
  if (length(uniprot_ids) == 1) {
    uniprot_ids <- c(uniprot_ids)
  }
  
  # Clean inputs
  uniprot_ids <- trimws(uniprot_ids)
  uniprot_ids <- uniprot_ids[uniprot_ids != ""]
  
  if (length(uniprot_ids) == 0) {
    return(data.frame())
  }
  
  tryCatch({
    # Convert our field names to UniProt API field names
    api_fields <- uniprot_api_fields[selected_fields]
    api_fields <- api_fields[!is.na(api_fields)]
    
    # Create the fields parameter for the API
    fields_param <- paste(api_fields, collapse = ",")
    
    # Create query string for multiple IDs
    ids_param <- paste(uniprot_ids, collapse = " OR ")
    
    # UniProt search API endpoint (TSV format is easier to parse)
    url <- "https://rest.uniprot.org/uniprotkb/search"
    
    # Make the API request
    response <- GET(url, query = list(
      query = ids_param,
      fields = fields_param,
      format = "tsv"
    ))
    
    if (status_code(response) != 200) {
      warning("UniProt API request failed with status: ", status_code(response))
      return(data.frame(
        uniprot_id = uniprot_ids,
        error = paste("API request failed with status:", status_code(response)),
        stringsAsFactors = FALSE
      ))
    }
    
    # Parse TSV response
    content_text <- content(response, "text", encoding = "UTF-8")
    
    if (nchar(trimws(content_text)) == 0) {
      return(data.frame(
        uniprot_id = uniprot_ids,
        error = "No data returned from UniProt",
        stringsAsFactors = FALSE
      ))
    }
    
    # Read TSV data
    result_df <- read.table(text = content_text, 
                            header = TRUE, 
                            sep = "\t", 
                            quote = "", 
                            stringsAsFactors = FALSE,
                            fill = TRUE,
                            comment.char = "")
    
    # Rename columns to match our field names
    colnames(result_df) <- selected_fields[1:ncol(result_df)]
    
    # Add any missing requested IDs as rows with errors
    found_ids <- result_df$accession
    missing_ids <- setdiff(uniprot_ids, found_ids)
    
    if (length(missing_ids) > 0) {
      missing_df <- data.frame(
        accession = missing_ids,
        error = paste("ID not found:", missing_ids),
        stringsAsFactors = FALSE
      )
      
      # Add empty columns for other fields
      for (field in selected_fields) {
        if (!field %in% names(missing_df)) {
          missing_df[[field]] <- NA
        }
      }
      
      result_df <- bind_rows(result_df, missing_df)
    }
    
    return(result_df)
    
  }, error = function(e) {
    return(data.frame(
      uniprot_id = uniprot_ids,
      error = paste("Error retrieving data:", e$message),
      stringsAsFactors = FALSE
    ))
  })
}

# Required supporting objects and functions:

# UniProt API field mappings - these are the actual field names UniProt accepts
uniprot_api_fields <- list(
  # Basic Information
  "accession" = "accession",
  "id" = "id", 
  "protein_name" = "protein_name",
  "gene_names" = "gene_names",
  "organism_name" = "organism_name",
  "organism_id" = "organism_id",
  "reviewed" = "reviewed",
  
  # Sequence Information
  "sequence" = "sequence",
  "length" = "length",
  "mass" = "mass",
  
  # Features
  "ft_signal" = "ft_signal",
  "ft_propep" = "ft_propep", 
  "ft_transit" = "ft_transit",
  "ft_transmem" = "ft_transmem",
  "ft_domain" = "ft_domain",
  "ft_region" = "ft_region",
  "ft_motif" = "ft_motif",
  "ft_act_site" = "ft_act_site",
  "ft_binding" = "ft_binding",
  "ft_site" = "ft_site",
  "ft_metal" = "ft_metal",
  
  # Comments
  "cc_function" = "cc_function",
  "cc_subcellular_location" = "cc_subcellular_location",
  "cc_subunit" = "cc_subunit",
  "cc_tissue_specificity" = "cc_tissue_specificity",
  "cc_disease" = "cc_disease",
  
  # Cross-references
  "xref_embl" = "xref_embl",
  "xref_pdb" = "xref_pdb",
  "xref_go" = "xref_go",
  "xref_interpro" = "xref_interpro",
  "xref_pfam" = "xref_pfam",
  
  # Additional
  "keywords" = "keyword",
  "ec" = "ec",
  "protein_existence" = "protein_existence"
)

# Default fields for protein design
default_selected_fields <- c(
  "accession",
  "protein_name", 
  "gene_names",
  "organism_name",
  "length",
  "cc_subcellular_location",
  "ft_signal",
  "ft_transmem",
  "ft_domain",
  "sequence"
)

