# Multi-Source Protein Structure Download
# Tries PDB first (experimental), then AlphaFold (predicted)
# Now checks for existing files and maintains URL list

library(httr)
library(jsonlite)

# Main function: Download structure from best available source
download_protein_structure <- function(uniprot_id, 
                                       output_dir = "structures",
                                       overwrite = FALSE,
                                       update_url_list = TRUE) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  
  result <- list(
    uniprot_id = uniprot_id,
    source = NA,
    pdb_id = NA,
    file_path = NA,
    url = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  # Check if any structure file already exists for this protein
  existing_file <- check_existing_structure(uniprot_id, output_dir)
  
  if (!is.null(existing_file) && !overwrite) {
    # File exists and we don't want to overwrite
    result$file_path <- existing_file
    result$success <- TRUE
    result$already_existed <- TRUE
    
    # Determine source and URL from filename
    if (grepl("_PDB\\.pdb$", existing_file)) {
      result$source <- "PDB"
      result$pdb_id <- gsub(".*_([A-Z0-9]+)_PDB\\.pdb$", "\\1", existing_file)
      result$url <- paste0("https://www.rcsb.org/structure/", result$pdb_id)
    } else if (grepl("_AlphaFold\\.pdb$", existing_file)) {
      result$source <- "AlphaFold"
      result$pdb_id <- uniprot_id
      result$url <- paste0("https://alphafold.ebi.ac.uk/entry/", uniprot_id)
    } else if (grepl("_ESMFold\\.pdb$", existing_file)) {
      result$source <- "ESMFold"
      result$pdb_id <- uniprot_id
      result$url <- paste0("https://esmatlas.com/resources?id=", uniprot_id)
    }
    
    result$message <- paste("Structure already exists (not re-downloading):", basename(existing_file))
    
    # Update URL list even for existing files if requested
    if (update_url_list && !is.na(result$url)) {
      update_url_list_file(uniprot_id, result$url, url_list_path)
    }
    print(result$url)
    
    return(result)
  }
  
  # If overwrite=TRUE and file exists, delete it first
  if (!is.null(existing_file) && overwrite) {
    file.remove(existing_file)
  }
  
  # Try PDB first (experimental structures - best quality)
  pdb_result <- download_from_pdb(uniprot_id, output_dir)
  if (pdb_result$success) {
    return(pdb_result)
  }
  
  # Try AlphaFold (predicted structures)
  af_result <- download_from_alphafold_url(uniprot_id, output_dir)
  if (af_result$success) {
    return(af_result)
  }
  
  # Try ESMFold as last resort
  esm_result <- download_from_esmfold(uniprot_id, output_dir)
  if (esm_result$success) {
    return(esm_result)
  }
  
  # No structure found
  result$message <- "No structure available from PDB, AlphaFold, or ESMFold"
  return(result)
}

# Helper function: Check if structure already exists
check_existing_structure <- function(uniprot_id, output_dir) {
  # Possible file patterns
  patterns <- c(
    paste0(uniprot_id, "_*_PDB.pdb"),      # PDB files
    paste0(uniprot_id, "_AlphaFold.pdb"),  # AlphaFold files
    paste0(uniprot_id, "_ESMFold.pdb")     # ESMFold files
  )
  
  # Check each pattern
  for (pattern in patterns) {
    files <- list.files(output_dir, pattern = glob2rx(pattern), full.names = TRUE)
    if (length(files) > 0) {
      # Return the first matching file
      return(files[1])
    }
  }
  
  return(NULL)
}

# Helper function: Update URL list file
update_url_list_file <- function(uniprot_id, url, url_list_path) {
  tryCatch({
    # Load existing list or create new one
    if (file.exists(url_list_path)) {
      url_list <- readRDS(url_list_path)
    } else {
      url_list <- list()
    }
    
    # Add/update URL for this protein
    url_list[[uniprot_id]] <- url
    
    # Save back to file
    saveRDS(url_list, url_list_path)
    
  }, error = function(e) {
    warning("Could not update URL list: ", e$message)
  })
}

# Helper function: Load URL list
load_url_list <- function(url_list_path = "Data/pdb_structure_url.rds") {
  if (file.exists(url_list_path)) {
    return(readRDS(url_list_path))
  } else {
    return(list())
  }
}

# Helper function: Get URL for a specific protein
get_structure_url <- function(uniprot_id, url_list_path = "Data/pdb_structure_url.rds") {
  url_list <- load_url_list(url_list_path)
  
  if (uniprot_id %in% names(url_list)) {
    return(url_list[[uniprot_id]])
  } else {
    return(NA)
  }
}

# Function 1: Download from PDB (experimental structures)
download_from_pdb <- function(uniprot_id, output_dir, update_url_list = TRUE, url_list_path = NULL) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "PDB",
    pdb_id = NA,
    file_path = NA,
    url = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  tryCatch({
    # Query PDB for structures associated with this UniProt ID
    search_url <- paste0(
      "https://search.rcsb.org/rcsbsearch/v2/query?json=",
      URLencode(sprintf(
        '{"query":{"type":"terminal","service":"text","parameters":{"attribute":"rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession","operator":"exact_match","value":"%s"}}}',
        uniprot_id
      ))
    )
    
    response <- GET(search_url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (!is.null(data$result_set) && length(data$result_set) > 0) {
        # Get the first (usually best resolution) PDB ID
        pdb_id <- data$result_set[[1]]$identifier
        
        # Create URL for browser access
        browser_url <- paste0("https://www.rcsb.org/structure/", pdb_id)
        
        # Download the PDB file
        pdb_url <- paste0("https://files.rcsb.org/download/", pdb_id, ".pdb")
        pdb_file <- file.path(output_dir, paste0(uniprot_id, "_", pdb_id, "_PDB.pdb"))
        
        download.file(pdb_url, pdb_file, mode = "wb", quiet = TRUE)
        
        result$pdb_id <- pdb_id
        result$file_path <- pdb_file
        result$url <- browser_url
        result$success <- TRUE
        result$message <- paste("Downloaded experimental structure from PDB:", pdb_id)
        
        # Update URL list
        if (update_url_list && !is.null(url_list_path)) {
          update_url_list_file(uniprot_id, browser_url, url_list_path)
        }
        
        return(result)
      }
    }
    
    result$message <- "No experimental structure found in PDB"
    return(result)
    
  }, error = function(e) {
    result$message <- paste("PDB download error:", e$message)
    return(result)
  })
}

# Function 2: Download from AlphaFold
download_from_alphafold_url <- function(uniprot_id, output_dir, update_url_list = TRUE, url_list_path = NULL) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "AlphaFold",
    pdb_id = uniprot_id,
    file_path = NA,
    url = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  tryCatch({
    # AlphaFold URL format
    af_url <- paste0(
      "https://alphafold.ebi.ac.uk/files/AF-",
      uniprot_id,
      "-F1-model_v4.pdb"
    )
    
    # Browser URL
    browser_url <- paste0("https://alphafold.ebi.ac.uk/entry/", uniprot_id)
    
    af_file <- file.path(output_dir, paste0(uniprot_id, "_AlphaFold.pdb"))
    
    # Try to download
    response <- GET(af_url)
    
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), af_file)
      
      result$file_path <- af_file
      result$url <- browser_url
      result$success <- TRUE
      result$message <- "Downloaded predicted structure from AlphaFold"
      
      # Update URL list
      if (update_url_list && !is.null(url_list_path)) {
        update_url_list_file(uniprot_id, browser_url, url_list_path)
      }
      
      return(result)
    } else {
      result$message <- "No AlphaFold structure available"
      return(result)
    }
    
  }, error = function(e) {
    result$message <- paste("AlphaFold download error:", e$message)
    return(result)
  })
}

# Function 3: Download from ESMFold (Meta's prediction service)
download_from_esmfold <- function(uniprot_id, output_dir, update_url_list = TRUE, url_list_path = NULL) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "ESMFold",
    pdb_id = uniprot_id,
    file_path = NA,
    url = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  tryCatch({
    # Get protein sequence first from UniProt
    uniprot_url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".fasta")
    response <- GET(uniprot_url)
    
    if (status_code(response) != 200) {
      result$message <- "Could not retrieve sequence from UniProt"
      return(result)
    }
    
    fasta_content <- content(response, "text")
    # Extract just the sequence (remove header line)
    sequence <- paste(strsplit(fasta_content, "\n")[[1]][-1], collapse = "")
    
    # Browser URL
    browser_url <- paste0("https://esmatlas.com/resources?id=", uniprot_id)
    
    # ESMFold API (note: this might have rate limits)
    esm_url <- "https://api.esmatlas.com/foldSequence/v1/pdb/"
    
    response <- POST(
      esm_url,
      body = sequence,
      encode = "raw",
      content_type("text/plain")
    )
    
    if (status_code(response) == 200) {
      esm_file <- file.path(output_dir, paste0(uniprot_id, "_ESMFold.pdb"))
      writeBin(content(response, "raw"), esm_file)
      
      result$file_path <- esm_file
      result$url <- browser_url
      result$success <- TRUE
      result$message <- "Predicted structure from ESMFold"
      
      # Update URL list
      if (update_url_list && !is.null(url_list_path)) {
        update_url_list_file(uniprot_id, browser_url, url_list_path)
      }
      
      return(result)
    } else {
      result$message <- "ESMFold prediction failed"
      return(result)
    }
    
  }, error = function(e) {
    result$message <- paste("ESMFold error:", e$message)
    return(result)
  })
}

# Batch download function with URL list option
download_structures_batch <- function(uniprot_ids, 
                                      output_dir = "structures",
                                      overwrite = FALSE,
                                      update_url_list = TRUE,
                                      url_list_path = "Data/pdb_structure_url.rds") {
  results_list <- list()
  
  cat("Downloading structures for", length(uniprot_ids), "proteins...\n")
  if (!overwrite) {
    cat("Mode: Skip existing files (overwrite = FALSE)\n")
  } else {
    cat("Mode: Replace existing files (overwrite = TRUE)\n")
  }
  if (update_url_list) {
    cat("URL tracking: Enabled (saving to", url_list_path, ")\n")
  } else {
    cat("URL tracking: Disabled\n")
  }
  cat("\n")
  
  for (i in seq_along(uniprot_ids)) {
    cat(sprintf("[%d/%d] Processing %s... ", i, length(uniprot_ids), uniprot_ids[i]))
    
    result <- download_protein_structure(
      uniprot_ids[i], 
      output_dir, 
      overwrite,
      update_url_list,
      url_list_path
    )
    results_list[[i]] <- result
    
    cat(result$message, "\n")
    if (update_url_list && !is.na(result$url)) {
      cat("  URL:", result$url, "\n")
    }
    
    # Be nice to the APIs (only if we actually downloaded something new)
    if (!result$already_existed) {
      Sys.sleep(0.5)
    }
  }
  
  # Convert to dataframe
  results_df <- do.call(rbind, lapply(results_list, as.data.frame))
  
  return(results_df)
}

# Summary function with URL list info
summarize_structure_downloads <- function(results_df, url_list_path = "Data/pdb_structure_url.rds") {
  cat("\n=== Structure Download Summary ===\n")
  cat("Total proteins:", nrow(results_df), "\n")
  cat("Successful downloads:", sum(results_df$success), "\n")
  cat("Failed:", sum(!results_df$success), "\n")
  
  if ("already_existed" %in% names(results_df)) {
    cat("Already existed (skipped):", sum(results_df$already_existed, na.rm = TRUE), "\n")
    cat("Newly downloaded:", sum(results_df$success & !results_df$already_existed, na.rm = TRUE), "\n")
  }
  
  if ("url" %in% names(results_df)) {
    cat("URLs captured:", sum(!is.na(results_df$url)), "\n")
  }
  cat("\n")
  
  if (sum(results_df$success) > 0) {
    cat("By source:\n")
    print(table(results_df$source[results_df$success]))
    cat("\n")
  }
  
  if (sum(!results_df$success) > 0) {
    cat("Proteins without structures:\n")
    print(results_df$uniprot_id[!results_df$success])
    cat("\n")
  }
  
  if (file.exists(url_list_path)) {
    url_list <- readRDS(url_list_path)
    cat("Total URLs in database:", length(url_list), "\n")
  }
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Example 1: Download with URL tracking (default)
# result <- download_protein_structure("P04637", "my_structures")
# print(result$url)  # Browser URL

# Example 2: Download without URL tracking
# result <- download_protein_structure("P04637", "my_structures", update_url_list = FALSE)

# Example 3: Batch download with URL tracking
# uniprot_ids <- c("P04637", "P69905", "P01308", "Q9Y6K9")
# results <- download_structures_batch(
#   uniprot_ids, 
#   "my_structures",
#   overwrite = FALSE,
#   update_url_list = TRUE
# )
# summarize_structure_downloads(results)

# Example 4: Load URL list and access URLs
# url_list <- load_url_list("Data/pdb_structure_url.rds")
# print(url_list[["P04637"]])  # Get URL for specific protein

# Example 5: Get URL for single protein
# url <- get_structure_url("P04637")
# browseURL(url)  # Open in browser

# Example 6: Integration with Shiny (reactiveValues style)
# In your Shiny server:
# observeEvent(input$download_structures, {
#   uniprot_ids <- filtered_data()$uniprot_id
#   
#   results <- download_structures_batch(
#     uniprot_ids,
#     output_dir = "structures",
#     overwrite = input$overwrite_checkbox,
#     update_url_list = input$track_urls_checkbox,
#     url_list_path = "Data/pdb_structure_url.rds"
#   )
#   
#   # Load URL list into reactive values
#   url_list <- load_url_list("Data/pdb_structure_url.rds")
#   for (id in names(url_list)) {
#     values$pdb_structure_url[[id]] <- url_list[[id]]
#   }
#   
#   output$structure_table <- renderDT({
#     results %>%
#       mutate(
#         view_link = ifelse(
#           !is.na(url),
#           sprintf('<a href="%s" target="_blank">View</a>', url),
#           ""
#         )
#       ) %>%
#       datatable(escape = FALSE)  # Allow HTML links
#   })
# })# Multi-Source Protein Structure Download
# Tries PDB first (experimental), then AlphaFold (predicted)
# Now checks for existing files before downloading

library(httr)
library(jsonlite)

# Main function: Download structure from best available source
download_protein_structure <- function(uniprot_id, 
                                       output_dir = "structures",
                                       overwrite = FALSE) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  result <- list(
    uniprot_id = uniprot_id,
    source = NA,
    pdb_id = NA,
    file_path = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  # Check if any structure file already exists for this protein
  existing_file <- check_existing_structure(uniprot_id, output_dir)
  
  if (!is.null(existing_file) && !overwrite) {
    # File exists and we don't want to overwrite
    result$file_path <- existing_file
    result$success <- TRUE
    result$already_existed <- TRUE
    
    # Determine source from filename
    if (grepl("_PDB\\.pdb$", existing_file)) {
      result$source <- "PDB"
      result$pdb_id <- gsub(".*_([A-Z0-9]+)_PDB\\.pdb$", "\\1", existing_file)
    } else if (grepl("_AlphaFold\\.pdb$", existing_file)) {
      result$source <- "AlphaFold"
      result$pdb_id <- uniprot_id
    } else if (grepl("_ESMFold\\.pdb$", existing_file)) {
      result$source <- "ESMFold"
      result$pdb_id <- uniprot_id
    }
    
    result$message <- paste("Structure already exists (not re-downloading):", basename(existing_file))
    return(result)
  }
  
  # If overwrite=TRUE and file exists, delete it first
  if (!is.null(existing_file) && overwrite) {
    file.remove(existing_file)
  }
  
  # Try PDB first (experimental structures - best quality)
  pdb_result <- download_from_pdb(uniprot_id, output_dir)
  if (pdb_result$success) {
    return(pdb_result)
  }
  
  # Try AlphaFold (predicted structures)
  af_result <- download_from_alphafold(uniprot_id, output_dir)
  if (af_result$success) {
    return(af_result)
  }
  
  # Try ESMFold as last resort
  esm_result <- download_from_esmfold(uniprot_id, output_dir)
  if (esm_result$success) {
    return(esm_result)
  }
  
  # No structure found
  result$message <- "No structure available from PDB, AlphaFold, or ESMFold"
  return(result)
}

# Helper function: Check if structure already exists
check_existing_structure <- function(uniprot_id, output_dir) {
  # Possible file patterns
  patterns <- c(
    paste0(uniprot_id, "_*_PDB.pdb"),      # PDB files
    paste0(uniprot_id, "_AlphaFold.pdb"),  # AlphaFold files
    paste0(uniprot_id, "_ESMFold.pdb")     # ESMFold files
  )
  
  # Check each pattern
  for (pattern in patterns) {
    files <- list.files(output_dir, pattern = glob2rx(pattern), full.names = TRUE)
    if (length(files) > 0) {
      # Return the first matching file
      return(files[1])
    }
  }
  
  return(NULL)
}

# Function 1: Download from PDB (experimental structures)
download_from_pdb <- function(uniprot_id, output_dir) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "PDB",
    pdb_id = NA,
    file_path = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  tryCatch({
    # Query PDB for structures associated with this UniProt ID
    search_url <- paste0(
      "https://search.rcsb.org/rcsbsearch/v2/query?json=",
      URLencode(sprintf(
        '{"query":{"type":"terminal","service":"text","parameters":{"attribute":"rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession","operator":"exact_match","value":"%s"}}}',
        uniprot_id
      ))
    )
    
    response <- GET(search_url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (!is.null(data$result_set) && length(data$result_set) > 0) {
        # Get the first (usually best resolution) PDB ID
        pdb_id <- data$result_set[[1]]$identifier
        
        # Download the PDB file
        pdb_url <- paste0("https://files.rcsb.org/download/", pdb_id, ".pdb")
        pdb_file <- file.path(output_dir, paste0(uniprot_id, "_", pdb_id, "_PDB.pdb"))
        
        download.file(pdb_url, pdb_file, mode = "wb", quiet = TRUE)
        
        result$pdb_id <- pdb_id
        result$file_path <- pdb_file
        result$success <- TRUE
        result$message <- paste("Downloaded experimental structure from PDB:", pdb_id)
        
        return(result)
      }
    }
    
    result$message <- "No experimental structure found in PDB"
    return(result)
    
  }, error = function(e) {
    result$message <- paste("PDB download error:", e$message)
    return(result)
  })
}

# Function 2: Download from AlphaFold
download_from_alphafold <- function(uniprot_id, output_dir) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "AlphaFold",
    pdb_id = uniprot_id,
    file_path = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  tryCatch({
    # AlphaFold URL format
    af_url <- paste0(
      "https://alphafold.ebi.ac.uk/files/AF-",
      uniprot_id,
      "-F1-model_v4.pdb"
    )
    
    af_file <- file.path(output_dir, paste0(uniprot_id, "_AlphaFold.pdb"))
    
    # Try to download
    response <- GET(af_url)
    
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), af_file)
      
      result$file_path <- af_file
      result$success <- TRUE
      result$message <- "Downloaded predicted structure from AlphaFold"
      
      return(result)
    } else {
      result$message <- "No AlphaFold structure available"
      return(result)
    }
    
  }, error = function(e) {
    result$message <- paste("AlphaFold download error:", e$message)
    return(result)
  })
}

# Function 3: Download from ESMFold (Meta's prediction service)
download_from_esmfold <- function(uniprot_id, output_dir) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "ESMFold",
    pdb_id = uniprot_id,
    file_path = NA,
    success = FALSE,
    message = "",
    already_existed = FALSE
  )
  
  tryCatch({
    # Get protein sequence first from UniProt
    uniprot_url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".fasta")
    response <- GET(uniprot_url)
    
    if (status_code(response) != 200) {
      result$message <- "Could not retrieve sequence from UniProt"
      return(result)
    }
    
    fasta_content <- content(response, "text")
    # Extract just the sequence (remove header line)
    sequence <- paste(strsplit(fasta_content, "\n")[[1]][-1], collapse = "")
    
    # ESMFold API (note: this might have rate limits)
    esm_url <- "https://api.esmatlas.com/foldSequence/v1/pdb/"
    
    response <- POST(
      esm_url,
      body = sequence,
      encode = "raw",
      content_type("text/plain")
    )
    
    if (status_code(response) == 200) {
      esm_file <- file.path(output_dir, paste0(uniprot_id, "_ESMFold.pdb"))
      writeBin(content(response, "raw"), esm_file)
      
      result$file_path <- esm_file
      result$success <- TRUE
      result$message <- "Predicted structure from ESMFold"
      
      return(result)
    } else {
      result$message <- "ESMFold prediction failed"
      return(result)
    }
    
  }, error = function(e) {
    result$message <- paste("ESMFold error:", e$message)
    return(result)
  })
}

# Batch download function with overwrite option
download_structures_batch <- function(uniprot_ids, 
                                      output_dir = "structures",
                                      overwrite = FALSE) {
  results_list <- list()
  
  cat("Downloading structures for", length(uniprot_ids), "proteins...\n")
  if (!overwrite) {
    cat("Mode: Skip existing files (overwrite = FALSE)\n")
  } else {
    cat("Mode: Replace existing files (overwrite = TRUE)\n")
  }
  cat("\n")
  
  for (i in seq_along(uniprot_ids)) {
    cat(sprintf("[%d/%d] Processing %s... ", i, length(uniprot_ids), uniprot_ids[i]))
    
    result <- download_protein_structure(uniprot_ids[i], output_dir, overwrite)
    results_list[[i]] <- result
    
    cat(result$message, "\n")
    
    # Be nice to the APIs (only if we actually downloaded something new)
    if (!result$already_existed) {
      Sys.sleep(0.5)
    }
  }
  
  # Convert to dataframe
  results_df <- do.call(rbind, lapply(results_list, as.data.frame))
  
  return(results_df)
}

# Summary function with enhanced statistics
summarize_structure_downloads <- function(results_df) {
  cat("\n=== Structure Download Summary ===\n")
  cat("Total proteins:", nrow(results_df), "\n")
  cat("Successful downloads:", sum(results_df$success), "\n")
  cat("Failed:", sum(!results_df$success), "\n")
  
  if ("already_existed" %in% names(results_df)) {
    cat("Already existed (skipped):", sum(results_df$already_existed, na.rm = TRUE), "\n")
    cat("Newly downloaded:", sum(results_df$success & !results_df$already_existed, na.rm = TRUE), "\n")
  }
  cat("\n")
  
  if (sum(results_df$success) > 0) {
    cat("By source:\n")
    print(table(results_df$source[results_df$success]))
    cat("\n")
  }
  
  if (sum(!results_df$success) > 0) {
    cat("Proteins without structures:\n")
    print(results_df$uniprot_id[!results_df$success])
  }
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Example 1: Download single structure (default: don't overwrite)
# result <- download_protein_structure("P04637", "my_structures")
# print(result)

# Example 2: Download single structure (force overwrite)
# result <- download_protein_structure("P04637", "my_structures", overwrite = TRUE)
# print(result)

# Example 3: Batch download (skip existing)
# uniprot_ids <- c("P04637", "P69905", "P01308", "Q9Y6K9")
# results <- download_structures_batch(uniprot_ids, "my_structures", overwrite = FALSE)
# summarize_structure_downloads(results)

# Example 4: Batch download (replace all)
# results <- download_structures_batch(uniprot_ids, "my_structures", overwrite = TRUE)
# summarize_structure_downloads(results)

# Example 5: Check what files exist before downloading
# existing <- check_existing_structure("P04637", "my_structures")
# if (!is.null(existing)) {
#   cat("Found existing file:", existing, "\n")
# } else {
#   cat("No existing file found\n")
# }

# Example 6: Integration with Shiny app
# In your Shiny app:
# observeEvent(input$download_structures, {
#   uniprot_ids <- filtered_data()$uniprot_id
#   overwrite_mode <- input$overwrite_checkbox  # Add checkbox to UI
#   
#   withProgress(message = 'Downloading structures...', value = 0, {
#     results <- download_structures_batch(
#       uniprot_ids, 
#       "structures",
#       overwrite = overwrite_mode
#     )
#   })
#   
#   summarize_structure_downloads(results)
#   
#   output$structure_results <- renderTable({
#     results
#   })
# })

# Function 1: Download from PDB (experimental structures)
download_from_pdb <- function(uniprot_id, output_dir) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "PDB",
    pdb_id = NA,
    file_path = NA,
    success = FALSE,
    message = ""
  )
  
  tryCatch({
    # Query PDB for structures associated with this UniProt ID
    search_url <- paste0(
      "https://search.rcsb.org/rcsbsearch/v2/query?json=",
      URLencode(sprintf(
        '{"query":{"type":"terminal","service":"text","parameters":{"attribute":"rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession","operator":"exact_match","value":"%s"}}}',
        uniprot_id
      ))
    )
    
    response <- GET(search_url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (!is.null(data$result_set) && length(data$result_set) > 0) {
        # Get the first (usually best resolution) PDB ID
        pdb_id <- data$result_set[[1]]$identifier
        
        # Download the PDB file
        pdb_url <- paste0("https://files.rcsb.org/download/", pdb_id, ".pdb")
        pdb_file <- file.path(output_dir, paste0(uniprot_id, "_", pdb_id, "_PDB.pdb"))
        
        download.file(pdb_url, pdb_file, mode = "wb", quiet = TRUE)
        
        result$pdb_id <- pdb_id
        result$file_path <- pdb_file
        result$success <- TRUE
        result$message <- paste("Downloaded experimental structure from PDB:", pdb_id)
        
        return(result)
      }
    }
    
    result$message <- "No experimental structure found in PDB"
    return(result)
    
  }, error = function(e) {
    result$message <- paste("PDB download error:", e$message)
    return(result)
  })
}

# Function 2: Download from AlphaFold
download_from_alphafold <- function(uniprot_id, output_dir) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "AlphaFold",
    pdb_id = uniprot_id,
    file_path = NA,
    success = FALSE,
    message = ""
  )
  
  tryCatch({
    # AlphaFold URL format
    af_url <- paste0(
      "https://alphafold.ebi.ac.uk/files/AF-",
      uniprot_id,
      "-F1-model_v4.pdb"
    )
    
    af_file <- file.path(output_dir, paste0(uniprot_id, "_AlphaFold.pdb"))
    
    # Try to download
    response <- GET(af_url)
    
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), af_file)
      
      result$file_path <- af_file
      result$success <- TRUE
      result$message <- "Downloaded predicted structure from AlphaFold"
      
      return(result)
    } else {
      result$message <- "No AlphaFold structure available"
      return(result)
    }
    
  }, error = function(e) {
    result$message <- paste("AlphaFold download error:", e$message)
    return(result)
  })
}

# Function 3: Download from ESMFold (Meta's prediction service)
download_from_esmfold <- function(uniprot_id, output_dir) {
  result <- list(
    uniprot_id = uniprot_id,
    source = "ESMFold",
    pdb_id = uniprot_id,
    file_path = NA,
    success = FALSE,
    message = ""
  )
  
  tryCatch({
    # Get protein sequence first from UniProt
    uniprot_url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".fasta")
    response <- GET(uniprot_url)
    
    if (status_code(response) != 200) {
      result$message <- "Could not retrieve sequence from UniProt"
      return(result)
    }
    
    fasta_content <- content(response, "text")
    # Extract just the sequence (remove header line)
    sequence <- paste(strsplit(fasta_content, "\n")[[1]][-1], collapse = "")
    
    # ESMFold API (note: this might have rate limits)
    esm_url <- "https://api.esmatlas.com/foldSequence/v1/pdb/"
    
    response <- POST(
      esm_url,
      body = sequence,
      encode = "raw",
      content_type("text/plain")
    )
    
    if (status_code(response) == 200) {
      esm_file <- file.path(output_dir, paste0(uniprot_id, "_ESMFold.pdb"))
      writeBin(content(response, "raw"), esm_file)
      
      result$file_path <- esm_file
      result$success <- TRUE
      result$message <- "Predicted structure from ESMFold"
      
      return(result)
    } else {
      result$message <- "ESMFold prediction failed"
      return(result)
    }
    
  }, error = function(e) {
    result$message <- paste("ESMFold error:", e$message)
    return(result)
  })
}

# Batch download function
download_structures_batch <- function(uniprot_ids, output_dir = "structures") {
  results_list <- list()
  
  cat("Downloading structures for", length(uniprot_ids), "proteins...\n")
  
  for (i in seq_along(uniprot_ids)) {
    cat(sprintf("[%d/%d] Processing %s... ", i, length(uniprot_ids), uniprot_ids[i]))
    
    result <- download_protein_structure(uniprot_ids[i], output_dir)
    results_list[[i]] <- result
    
    cat(result$message, "\n")
    
    # Be nice to the APIs
    Sys.sleep(0.5)
  }
  
  # Convert to dataframe
  results_df <- do.call(rbind, lapply(results_list, as.data.frame))
  
  return(results_df)
}

# Summary function
summarize_structure_downloads <- function(results_df) {
  cat("\n=== Structure Download Summary ===\n")
  cat("Total proteins:", nrow(results_df), "\n")
  cat("Successful downloads:", sum(results_df$success), "\n")
  cat("Failed:", sum(!results_df$success), "\n\n")
  
  if (sum(results_df$success) > 0) {
    cat("By source:\n")
    print(table(results_df$source[results_df$success]))
  }
  
  if (sum(!results_df$success) > 0) {
    cat("\nProteins without structures:\n")
    print(results_df$uniprot_id[!results_df$success])
  }
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Example 1: Download single structure
# result <- download_protein_structure("P04637", "my_structures")
# print(result)

# Example 2: Batch download
# uniprot_ids <- c("P04637", "P69905", "P01308", "Q9Y6K9")
# results <- download_structures_batch(uniprot_ids, "my_structures")
# print(results)
# summarize_structure_downloads(results)

# Example 3: Integration with your existing code
# In your Shiny app:
# observeEvent(input$download_structures, {
#   uniprot_ids <- filtered_data()$uniprot_id
#   
#   withProgress(message = 'Downloading structures...', value = 0, {
#     results <- download_structures_batch(uniprot_ids, "structures")
#   })
#   
#   summarize_structure_downloads(results)
#   
#   output$structure_results <- renderTable({
#     results
#   })
# })