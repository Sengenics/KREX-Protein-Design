# AlphaFold Structure Generation and Comparison
library(httr)
library(jsonlite)
library(dplyr)

# Function to submit sequence to ColabFold (free AlphaFold service)
submit_to_colabfold <- function(sequence, job_name = "protein") {
  tryCatch({
    # ColabFold batch API endpoint
    url <- "https://api.colabfold.com/submit"
    
    # Prepare the request
    body <- list(
      sequences = sequence,
      mode = "AlphaFold2",
      jobname = job_name
    )
    
    # Submit job
    response <- POST(
      url,
      body = body,
      encode = "json",
      add_headers("Content-Type" = "application/json")
    )
    
    if (status_code(response) == 200) {
      result <- fromJSON(content(response, "text"))
      return(list(
        job_id = result$id,
        status = "submitted",
        sequence = sequence
      ))
    } else {
      return(list(
        error = paste("Submission failed:", status_code(response)),
        sequence = sequence
      ))
    }
    
  }, error = function(e) {
    return(list(
      error = paste("Error:", e$message),
      sequence = sequence
    ))
  })
}

# Function to check job status
check_colabfold_status <- function(job_id) {
  tryCatch({
    url <- paste0("https://api.colabfold.com/status/", job_id)
    response <- GET(url)
    
    if (status_code(response) == 200) {
      result <- fromJSON(content(response, "text"))
      return(result)
    } else {
      return(list(status = "error", message = "Failed to check status"))
    }
    
  }, error = function(e) {
    return(list(status = "error", message = e$message))
  })
}

# Function to process a protein with 3 sequences
process_protein_structures <- function(original_seq, n_term_seq, c_term_seq, protein_name = "protein") {
  cat("Processing protein:", protein_name, "\n")
  
  # Submit all three sequences
  jobs <- list(
    original = submit_to_colabfold(original_seq, paste0(protein_name, "_original")),
    n_terminal = submit_to_colabfold(n_term_seq, paste0(protein_name, "_nterm")),
    c_terminal = submit_to_colabfold(c_term_seq, paste0(protein_name, "_cterm"))
  )
  
  # Return job information
  results <- data.frame(
    protein_name = protein_name,
    variant = c("original", "n_terminal", "c_terminal"),
    sequence = c(original_seq, n_term_seq, c_term_seq),
    job_id = c(
      ifelse("job_id" %in% names(jobs$original), jobs$original$job_id, NA),
      ifelse("job_id" %in% names(jobs$n_terminal), jobs$n_terminal$job_id, NA),
      ifelse("job_id" %in% names(jobs$c_terminal), jobs$c_terminal$job_id, NA)
    ),
    status = c(
      ifelse("status" %in% names(jobs$original), jobs$original$status, "error"),
      ifelse("status" %in% names(jobs$n_terminal), jobs$n_terminal$status, "error"),
      ifelse("status" %in% names(jobs$c_terminal), jobs$c_terminal$status, "error")
    ),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Function to monitor multiple jobs
monitor_jobs <- function(job_df, check_interval = 30) {
  cat("Monitoring", nrow(job_df), "jobs...\n")
  
  completed_jobs <- data.frame()
  
  while(nrow(job_df) > 0) {
    cat("Checking", nrow(job_df), "remaining jobs...\n")
    
    for(i in seq_len(nrow(job_df))) {
      if(!is.na(job_df$job_id[i])) {
        status <- check_colabfold_status(job_df$job_id[i])
        
        if(status$status == "DONE") {
          cat("✓ Completed:", job_df$protein_name[i], job_df$variant[i], "\n")
          job_df$status[i] <-