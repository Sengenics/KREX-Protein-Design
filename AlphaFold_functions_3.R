# denovo_structure_prediction.R
# Function to predict protein structure from sequence using ColabFold API

library(httr)
library(jsonlite)

# Function to submit a sequence for de-novo structure prediction
predict_structure_from_sequence <- function(sequence, job_name = NULL) {
  
  # Validate input
  if (is.null(sequence) || nchar(sequence) == 0) {
    stop("Sequence cannot be empty")
  }
  
  # Clean sequence (remove spaces, newlines, numbers)
  sequence <- gsub("[^ACDEFGHIKLMNPQRSTVWY]", "", toupper(sequence))
  
  # Validate amino acid sequence
  valid_aa <- "ACDEFGHIKLMNPQRSTVWY"
  if (!all(strsplit(sequence, "")[[1]] %in% strsplit(valid_aa, "")[[1]])) {
    stop("Invalid amino acid sequence. Only standard 20 amino acids are allowed.")
  }
  
  # Check sequence length (ColabFold has practical limits)
  if (nchar(sequence) > 1000) {
    warning("Sequence is very long (>1000 residues). Prediction may take a very long time or fail.")
  }
  
  if (nchar(sequence) < 10) {
    warning("Sequence is very short (<10 residues). Prediction quality may be poor.")
  }
  
  # Generate job name if not provided
  if (is.null(job_name)) {
    job_name <- paste0("structure_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  tryCatch({
    
    # ColabFold batch API endpoint
    api_url <- "https://api.colabfold.com/submit"
    
    # Prepare the request body
    request_data <- list(
      name = job_name,
      sequence = sequence,
      # Optional parameters
      num_relax = 1,  # Number of relaxation steps
      num_models = 5, # Number of models to generate
      use_amber = TRUE, # Use AMBER relaxation
      use_templates = TRUE # Use template information
    )
    
    # Submit job
    response <- POST(
      api_url,
      body = request_data,
      encode = "json",
      add_headers(
        "Content-Type" = "application/json",
        "Accept" = "application/json"
      )
    )
    
    if (status_code(response) != 200) {
      stop(paste("API request failed with status:", status_code(response)))
    }
    
    # Parse response
    result <- fromJSON(content(response, "text"))
    
    # Return job information
    job_info <- list(
      job_id = result$id,
      job_name = job_name,
      sequence = sequence,
      sequence_length = nchar(sequence),
      submission_time = Sys.time(),
      status_url = paste0("https://api.colabfold.com/status/", result$id),
      result_url = paste0("https://api.colabfold.com/result/", result$id),
      estimated_time = estimate_prediction_time(nchar(sequence)),
      message = "Job submitted successfully. Use check_prediction_status() to monitor progress."
    )
    
    return(job_info)
    
  }, error = function(e) {
    
    # Fallback: provide information about alternative methods
    fallback_info <- list(
      error = paste("ColabFold API submission failed:", e$message),
      sequence = sequence,
      sequence_length = nchar(sequence),
      alternatives = list(
        "Use ColabFold Notebook" = "https://colab.research.google.com/github/deepmind/alphafold/blob/main/notebooks/AlphaFold.ipynb",
        "ChimeraX AlphaFold" = "https://www.cgl.ucsf.edu/chimerax/docs/user/tools/alphafold.html",
        "Fold and Function Assignment System (FFAS)" = "https://ffas.burnham.org/",
        "I-TASSER" = "https://zhanglab.ccmb.med.umich.edu/I-TASSER/",
        "ESMFold (Meta)" = "https://esmatlas.net/"
      ),
      local_options = list(
        "ColabFold local installation" = "pip install colabfold[alphafold]",
        "AlphaFold2 local" = "https://github.com/deepmind/alphafold"
      ),
      message = "Consider using one of the alternative prediction services listed above."
    )
    
    return(fallback_info)
  })
}

# Helper function to estimate prediction time
estimate_prediction_time <- function(sequence_length) {
  if (sequence_length < 50) {
    return("~5-10 minutes")
  } else if (sequence_length < 100) {
    return("~10-20 minutes") 
  } else if (sequence_length < 300) {
    return("~20-60 minutes")
  } else if (sequence_length < 500) {
    return("~1-3 hours")
  } else {
    return("~3+ hours (very long)")
  }
}

# Function to check prediction status
check_prediction_status <- function(job_id) {
  tryCatch({
    status_url <- paste0("https://api.colabfold.com/status/", job_id)
    
    response <- GET(status_url)
    
    if (status_code(response) != 200) {
      return(list(
        job_id = job_id,
        status = "unknown",
        error = paste("Status check failed with code:", status_code(response))
      ))
    }
    
    result <- fromJSON(content(response, "text"))
    
    status_info <- list(
      job_id = job_id,
      status = result$status,
      progress = ifelse(is.null(result$progress), "unknown", result$progress),
      estimated_remaining = ifelse(is.null(result$eta), "unknown", result$eta),
      message = case_when(
        result$status == "running" ~ "Prediction is running...",
        result$status == "completed" ~ "Prediction completed! Use get_prediction_results() to download.",
        result$status == "failed" ~ "Prediction failed.",
        result$status == "queued" ~ "Job is queued and waiting to start.",
        TRUE ~ paste("Status:", result$status)
      )
    )
    
    return(status_info)
    
  }, error = function(e) {
    return(list(
      job_id = job_id,
      status = "error",
      error = paste("Error checking status:", e$message)
    ))
  })
}

# Function to get prediction results
get_prediction_results <- function(job_id, download_dir = "predicted_structures") {
  tryCatch({
    
    # Create download directory if it doesn't exist
    if (!dir.exists(download_dir)) {
      dir.create(download_dir, recursive = TRUE)
    }
    
    result_url <- paste0("https://api.colabfold.com/result/", job_id)
    
    response <- GET(result_url)
    
    if (status_code(response) != 200) {
      return(list(
        job_id = job_id,
        error = paste("Results download failed with code:", status_code(response))
      ))
    }
    
    result <- fromJSON(content(response, "text"))
    
    # Download structure files
    downloaded_files <- list()
    
    if (!is.null(result$pdb_files)) {
      for (i in seq_along(result$pdb_files)) {
        pdb_url <- result$pdb_files[i]
        filename <- file.path(download_dir, paste0(job_id, "_model_", i, ".pdb"))
        
        download.file(pdb_url, filename, mode = "wb")
        downloaded_files[[paste0("model_", i, "_pdb")]] <- filename
      }
    }
    
    # Download confidence scores if available
    if (!is.null(result$confidence_file)) {
      conf_filename <- file.path(download_dir, paste0(job_id, "_confidence.json"))
      download.file(result$confidence_file, conf_filename, mode = "wb")
      downloaded_files[["confidence"]] <- conf_filename
    }
    
    results_info <- list(
      job_id = job_id,
      status = "completed",
      downloaded_files = downloaded_files,
      download_directory = download_dir,
      num_models = length(result$pdb_files),
      best_model = ifelse(length(downloaded_files) > 0, downloaded_files[[1]], NA),
      message = paste("Downloaded", length(downloaded_files), "files to", download_dir)
    )
    
    return(results_info)
    
  }, error = function(e) {
    return(list(
      job_id = job_id,
      error = paste("Error downloading results:", e$message)
    ))
  })
}

# Simple all-in-one function for immediate prediction (synchronous, for short sequences only)
predict_structure_simple <- function(sequence, wait_for_completion = FALSE, max_wait_minutes = 30) {
  
  cat("Submitting sequence for structure prediction...\n")
  job_info <- predict_structure_from_sequence(sequence)
  
  if ("error" %in% names(job_info)) {
    return(job_info)
  }
  
  cat("Job submitted with ID:", job_info$job_id, "\n")
  cat("Estimated time:", job_info$estimated_time, "\n")
  
  if (!wait_for_completion) {
    cat("Use check_prediction_status('", job_info$job_id, "') to monitor progress.\n", sep = "")
    return(job_info)
  }
  
  # Wait for completion
  cat("Waiting for completion...\n")
  start_time <- Sys.time()
  max_wait_seconds <- max_wait_minutes * 60
  
  while (TRUE) {
    status <- check_prediction_status(job_info$job_id)
    
    cat("Status:", status$status, "\n")
    
    if (status$status == "completed") {
      cat("Prediction completed! Downloading results...\n")
      results <- get_prediction_results(job_info$job_id)
      return(results)
    }
    
    if (status$status == "failed") {
      return(list(
        job_id = job_info$job_id,
        error = "Prediction failed on server"
      ))
    }
    
    # Check timeout
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed > max_wait_seconds) {
      return(list(
        job_id = job_info$job_id,
        status = "timeout",
        message = paste("Timeout after", max_wait_minutes, "minutes. Check status manually.")
      ))
    }
    
    # Wait before next check
    Sys.sleep(30)
  }
}

# Example usage:
# 
# # Submit a job
# sequence <- "MKTVRQERLKSIVRILERSKEPVSGAQLAEELSVSRQVIVQDIAYLRSLGYNIVATPRGYVLAGG"
# job <- predict_structure_from_sequence(sequence, "my_protein")
# 
# # Check status
# status <- check_prediction_status(job$job_id)
# 
# # Get results when ready
# results <- get_prediction_results(job$job_id)
# 
# # Or do everything at once for short sequences
# result <- predict_structure_simple(sequence, wait_for_completion = TRUE)