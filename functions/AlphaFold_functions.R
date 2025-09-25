# AlphaFold API Integration for R
# Access AlphaFold protein structure predictions via their REST API

library(httr)
library(jsonlite)
library(dplyr)

# AlphaFold API base URLs
ALPHAFOLD_API_BASE <- "https://alphafold.ebi.ac.uk/api"
ALPHAFOLD_FILES_BASE <- "https://alphafold.ebi.ac.uk/files"

# Function to get AlphaFold prediction metadata by UniProt ID
get_alphafold_prediction <- function(uniprot_id) {
  tryCatch({
    url <- paste0(ALPHAFOLD_API_BASE, "/prediction/", uniprot_id)
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      return(data)
    } else if (status_code(response) == 404) {
      return(list(error = paste("No AlphaFold prediction found for", uniprot_id)))
    } else {
      return(list(error = paste("API error:", status_code(response))))
    }
    
  }, error = function(e) {
    return(list(error = paste("Error:", e$message)))
  })
}

# Function to get AlphaFold predictions for multiple UniProt IDs
get_alphafold_batch <- function(uniprot_ids) {
  results <- list()
  
  for (i in seq_along(uniprot_ids)) {
    cat("Fetching AlphaFold data for", uniprot_ids[i], "...\n")
    
    result <- get_alphafold_prediction(uniprot_ids[i])
    results[[uniprot_ids[i]]] <- result
    
    # Be nice to the API
    if (i < length(uniprot_ids)) {
      Sys.sleep(0.2)
    }
  }
  
  return(results)
}

# Function to extract key AlphaFold data into a data frame
parse_alphafold_data <- function(alphafold_results) {
  df_list <- list()
  
  for (uniprot_id in names(alphafold_results)) {
    data <- alphafold_results[[uniprot_id]]
    
    if ("error" %in% names(data)) {
      df_list[[uniprot_id]] <- data.frame(
        uniprot_id = uniprot_id,
        alphafold_available = FALSE,
        error = data$error,
        stringsAsFactors = FALSE
      )
    } else {
      df_list[[uniprot_id]] <- data.frame(
        uniprot_id = uniprot_id,
        alphafold_available = TRUE,
        alphafold_version = data$modelVersion %||% NA,
        model_confidence = data$modelConfidence %||% NA,
        structure_url = data$pdbUrl %||% NA,
        mmcif_url = data$mmcifUrl %||% NA,
        pae_image_url = data$paeImageUrl %||% NA,
        pae_data_url = data$paeDocUrl %||% NA,
        model_created = data$modelCreatedDate %||% NA,
        sequence_length = data$uniprotEnd - data$uniprotStart + 1,
        coverage_start = data$uniprotStart %||% NA,
        coverage_end = data$uniprotEnd %||% NA,
        error = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(do.call(rbind, df_list))
}

# Enhanced UniProt function that includes AlphaFold data
get_uniprot_with_alphafold <- function(uniprot_ids, selected_fields = default_selected_fields, include_alphafold = TRUE) {
  
  # Get UniProt data first
  cat("Fetching UniProt data...\n")
  uniprot_df <- get_uniprot_info_simple(uniprot_ids, selected_fields)
  
  if (!include_alphafold) {
    return(uniprot_df)
  }
  
  # Get AlphaFold predictions
  cat("Fetching AlphaFold predictions...\n")
  alphafold_results <- get_alphafold_batch(uniprot_ids)
  alphafold_df <- parse_alphafold_data(alphafold_results)
  
  # Merge the data
  if ("accession" %in% colnames(uniprot_df)) {
    merged_df <- merge(uniprot_df, alphafold_df, 
                       by.x = "accession", by.y = "uniprot_id", 
                       all.x = TRUE)
  } else {
    # If no accession column, just bind columns
    merged_df <- cbind(uniprot_df, alphafold_df[match(uniprot_ids, alphafold_df$uniprot_id), ])
  }
  
  return(merged_df)
}

# Function to download AlphaFold structure files
download_alphafold_structure <- function(uniprot_id, format = "pdb", output_dir = "alphafold_structures") {
  tryCatch({
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Get prediction metadata first
    prediction <- get_alphafold_prediction(uniprot_id)
    
    if ("error" %in% names(prediction)) {
      return(list(success = FALSE, error = prediction$error))
    }
    
    # Choose URL based on format
    if (format == "pdb") {
      download_url <- prediction$pdbUrl
      file_ext <- ".pdb"
    } else if (format == "cif" || format == "mmcif") {
      download_url <- prediction$mmcifUrl
      file_ext <- ".cif"
    } else {
      return(list(success = FALSE, error = "Format must be 'pdb' or 'cif'"))
    }
    
    if (is.null(download_url)) {
      return(list(success = FALSE, error = "Download URL not available"))
    }
    
    # Download the file
    filename <- paste0("AF-", uniprot_id, "-F1-model_v4", file_ext)
    filepath <- file.path(output_dir, filename)
    
    response <- GET(download_url, write_disk(filepath, overwrite = TRUE))
    
    if (status_code(response) == 200) {
      return(list(success = TRUE, filepath = filepath, filename = filename))
    } else {
      return(list(success = FALSE, error = paste("Download failed:", status_code(response))))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Error:", e$message)))
  })
}

# Function to get AlphaFold confidence summary
get_alphafold_confidence_summary <- function(uniprot_id) {
  tryCatch({
    # Get prediction data
    prediction <- get_alphafold_prediction(uniprot_id)
    
    if ("error" %in% names(prediction)) {
      return(list(error = prediction$error))
    }
    
    # Download PAE data if available
    if (!is.null(prediction$paeDocUrl)) {
      pae_response <- GET(prediction$paeDocUrl)
      
      if (status_code(pae_response) == 200) {
        pae_data <- fromJSON(content(pae_response, "text"))
        
        # Extract confidence metrics
        confidences <- pae_data$confidenceScore
        
        summary <- list(
          uniprot_id = uniprot_id,
          mean_confidence = mean(confidences, na.rm = TRUE),
          median_confidence = median(confidences, na.rm = TRUE),
          high_confidence_residues = sum(confidences > 90, na.rm = TRUE),
          confident_residues = sum(confidences > 70, na.rm = TRUE),
          low_confidence_residues = sum(confidences < 50, na.rm = TRUE),
          total_residues = length(confidences),
          high_confidence_percent = round(100 * sum(confidences > 90, na.rm = TRUE) / length(confidences), 1),
          model_version = prediction$modelVersion
        )
        
        return(summary)
      }
    }
    
    # If PAE data not available, return basic info
    return(list(
      uniprot_id = uniprot_id,
      model_version = prediction$modelVersion,
      error = "PAE data not available"
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error:", e$message)))
  })
}

# Function to search AlphaFold by sequence similarity
search_alphafold_by_sequence <- function(sequence, threshold = 0.8) {
  # Note: This would require implementing BLAST search against AlphaFold DB
  # The API supports this but requires more complex implementation
  warning("Sequence similarity search not yet implemented. Use UniProt ID lookup instead.")
  return(NULL)
}

# Utility function for null coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Example usage functions
demo_alphafold_api <- function() {
  cat("Demo: AlphaFold API Integration\n")
  cat("===============================\n\n")
  
  # Test with p53
  cat("1. Getting AlphaFold prediction for P04637 (p53)...\n")
  p53_prediction <- get_alphafold_prediction("P04637")
  if (!"error" %in% names(p53_prediction)) {
    cat("✓ Found AlphaFold prediction\n")
    cat("  Model version:", p53_prediction$modelVersion, "\n")
    cat("  Confidence:", p53_prediction$modelConfidence, "\n")
  } else {
    cat("✗", p53_prediction$error, "\n")
  }
  
  cat("\n2. Getting confidence summary...\n")
  confidence <- get_alphafold_confidence_summary("P04637")
  if (!"error" %in% names(confidence)) {
    cat("✓ Mean confidence:", round(confidence$mean_confidence, 1), "\n")
    cat("  High confidence residues:", confidence$high_confidence_percent, "%\n")
  }
  
  cat("\n3. Testing batch retrieval...\n")
  batch_data <- get_uniprot_with_alphafold(c("P04637", "P53_HUMAN"), include_alphafold = TRUE)
  cat("✓ Retrieved data for", nrow(batch_data), "proteins\n")
  
  return(batch_data)
}

# Integration with your existing workflow
integrate_alphafold_to_search <- function() {
  # This would go in your server.R
  search_section_with_alphafold <- reactive({
    req(input$uniprot_select)
    
    uniprot_ids <- input$uniprot_select
    selected_fields <- default_selected_fields
    
    # Get combined UniProt + AlphaFold data
    result_df <- get_uniprot_with_alphafold(
      uniprot_ids, 
      selected_fields,
      include_alphafold = input$include_alphafold  # Add checkbox in UI
    )
    
    return(result_df)
  })
  
  return(search_section_with_alphafold)
}

# Example: Download structures for your proteins
download_structures_for_proteins <- function(uniprot_ids, format = "pdb") {
  results <- list()
  
  for (id in uniprot_ids) {
    cat("Downloading structure for", id, "...\n")
    result <- download_alphafold_structure(id, format)
    results[[id]] <- result
    
    if (result$success) {
      cat("✓ Downloaded:", result$filename, "\n")
    } else {
      cat("✗ Failed:", result$error, "\n")
    }
  }
  
  return(results)
}