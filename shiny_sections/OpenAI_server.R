# shiny_sections/openai_analysis.R
# Complete OpenAI Analysis Server - Literature Search

# Source OpenAI functions
source("functions/openai_functions.R", local = TRUE)

# ============================================================================
# UPDATE PROTEIN SELECTOR DROPDOWN
# ============================================================================

observe({
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    updateSelectizeInput(session, 'openai_protein_select', 
                         choices = c("No proteins available" = ""))
    return()
  }
  
  # Create named list with protein names and IDs
  choices <- setNames(current_ids, sapply(current_ids, function(id) {
    protein_name <- if (!is.null(values$uniprot_list[[id]]$protein_name)) {
      values$uniprot_list[[id]]$protein_name
    } else if (!is.null(values$uniprot_list[[id]]$name)) {
      values$uniprot_list[[id]]$name
    } else {
      id
    }
    paste0(protein_name, " (", id, ")")
  }))
  
  # Update dropdown
  updateSelectizeInput(session, 'openai_protein_select', 
                       choices = choices,
                       selected = current_ids[1],
                       server = TRUE)
})

# ============================================================================
# HELPER FUNCTION: PROCESS EPITOPE SINGLE
# ============================================================================

# REPLACE process_epitope_single with this debugging version

process_epitope_single <- function(protein_id, model) {
  
  cat("\n=== Processing Epitope Search for", protein_id, "===\n")
  
  # Get protein info
  protein_info <- values$uniprot_list[[protein_id]]
  
  if (is.null(protein_info)) {
    cat("ERROR: No UniProt data found\n")
    return(list(success = FALSE, error = "No UniProt data found"))
  }
  
  # Build prompt
  epitope_prompt <- build_epitope_prompt(protein_info)
  
  # Call OpenAI
  cat("Calling OpenAI API for epitopes...\n")
  result <- openai_request(epitope_prompt, model = model, max_tokens = 10000)
  
  # Initialize storage
  if (is.null(values$openai)) {
    cat("Initializing values$openai list\n")
    values$openai <- list()
  }
  
  if (is.null(values$openai[[protein_id]])) {
    cat("Initializing values$openai[[", protein_id, "]]\n")
    values$openai[[protein_id]] <- list()
  }
  
  if (result$success) {
    cat("OpenAI response received successfully\n")
    cat("Response length:", nchar(result$content), "characters\n")
    cat("First 300 characters of response:\n")
    cat(substr(result$content, 1, 300), "\n")
    cat("Last 200 characters of response:\n")
    cat(substr(result$content, nchar(result$content) - 200, nchar(result$content)), "\n\n")
    
    cat("Parsing JSON...\n")
    
    # Parse with expected type
    epitopes <- parse_json_safely(result$content, expected_type = "epitopes")
    
    cat("Parse complete. Result class:", class(epitopes), "\n")
    cat("Result dimensions:", nrow(epitopes), "rows x", ncol(epitopes), "columns\n")
    
    if (nrow(epitopes) > 0) {
      cat("Column names:", paste(names(epitopes), collapse = ", "), "\n")
      cat("First row:\n")
      print(epitopes[1, ])
    }
    
    # Check if parse returned error structure
    if ("Error" %in% names(epitopes)) {
      cat("❌ Parse returned ERROR structure\n")
      cat("Error message:", epitopes$Error[1], "\n")
      
      # Mark as searched but with parse error
      values$openai[[protein_id]]$epitopes <- get_empty_structure("epitopes")
      values$openai[[protein_id]]$epitopes_searched <- TRUE
      values$openai[[protein_id]]$epitopes_timestamp <- as.character(Sys.time())
      values$openai[[protein_id]]$epitopes_error <- "JSON parse error"
      values$openai[[protein_id]]$epitopes_raw <- result$content  # Save raw response
      
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$openai, 'Data/openai.rds')
      
      return(list(success = FALSE, error = "JSON parse error", count = 0))
      
    } else if (nrow(epitopes) == 0) {
      cat("✓ Parse successful but NO RESULTS FOUND (empty array)\n")
      
      # This is valid - protein has no known epitopes
      values$openai[[protein_id]]$epitopes <- epitopes  # Save empty but valid structure
      values$openai[[protein_id]]$epitopes_searched <- TRUE
      values$openai[[protein_id]]$epitopes_timestamp <- as.character(Sys.time())
      values$openai[[protein_id]]$epitopes_model <- model
      values$openai[[protein_id]]$epitopes_result <- "No epitopes found in literature"
      
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$openai, 'Data/openai.rds')
      
      # Log token usage
      if (!is.null(result$usage) && exists("log_token_usage")) {
        log_token_usage("epitope", protein_id, result$usage)
      }
      
      return(list(success = TRUE, count = 0, message = "No epitopes found"))
      
    } else {
      cat("✓ Parse successful with", nrow(epitopes), "results\n")
      
      # Store valid results
      values$openai[[protein_id]]$epitopes <- epitopes
      values$openai[[protein_id]]$epitopes_searched <- TRUE
      values$openai[[protein_id]]$epitopes_timestamp <- as.character(Sys.time())
      values$openai[[protein_id]]$epitopes_model <- model
      
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$openai, 'Data/openai.rds')
      
      # Log token usage
      if (!is.null(result$usage) && exists("log_token_usage")) {
        log_token_usage("epitope", protein_id, result$usage)
      }
      
      return(list(success = TRUE, count = nrow(epitopes)))
    }
    
  } else {
    cat("❌ OpenAI API error:", result$error, "\n")
    
    # Mark as searched with error
    values$openai[[protein_id]]$epitopes <- get_empty_structure("epitopes")
    values$openai[[protein_id]]$epitopes_searched <- TRUE
    values$openai[[protein_id]]$epitopes_timestamp <- as.character(Sys.time())
    values$openai[[protein_id]]$epitopes_error <- result$error
    
    if (!dir.exists('Data')) dir.create('Data')
    saveRDS(values$openai, 'Data/openai.rds')
    
    return(list(success = FALSE, error = result$error))
  }
}

# ============================================================================
# HELPER FUNCTION: PROCESS EXPRESSION SINGLE
# ============================================================================

# REPLACE process_expression_single with this version that shows full debugging

process_expression_single <- function(protein_id, model) {
  
  cat("\n=== Processing Expression Search for", protein_id, "===\n")
  
  # Get protein info
  protein_info <- values$uniprot_list[[protein_id]]
  
  if (is.null(protein_info)) {
    cat("ERROR: No UniProt data found\n")
    return(list(success = FALSE, error = "No UniProt data found"))
  }
  
  # Build prompt
  expression_prompt <- build_expression_prompt(protein_info)
  
  # Call OpenAI
  cat("Calling OpenAI API for expression data...\n")
  result <- openai_request(expression_prompt, model = model, max_tokens = 10000)
  
  # Initialize storage
  if (is.null(values$openai)) {
    cat("Initializing values$openai list\n")
    values$openai <- list()
  }
  
  if (is.null(values$openai[[protein_id]])) {
    cat("Initializing values$openai[[", protein_id, "]]\n")
    values$openai[[protein_id]] <- list()
  }
  
  if (result$success) {
    cat("OpenAI response received successfully\n")
    cat("Response length:", nchar(result$content), "characters\n")
    cat("First 300 characters of response:\n")
    cat(substr(result$content, 1, 300), "\n")
    cat("Last 200 characters of response:\n")
    cat(substr(result$content, nchar(result$content) - 200, nchar(result$content)), "\n\n")
    
    cat("Parsing JSON...\n")
    
    # Parse with expected type
    expression_data <- parse_json_safely(result$content, expected_type = "expression")
    
    cat("Parse complete. Result class:", class(expression_data), "\n")
    cat("Result dimensions:", nrow(expression_data), "rows x", ncol(expression_data), "columns\n")
    
    if (nrow(expression_data) > 0) {
      cat("Column names:", paste(names(expression_data), collapse = ", "), "\n")
      cat("First row:\n")
      print(expression_data[1, ])
    }
    
    # Check if parse returned error structure
    if ("Error" %in% names(expression_data)) {
      cat("❌ Parse returned ERROR structure\n")
      cat("Error message:", expression_data$Error[1], "\n")
      
      # Mark as searched but with parse error
      values$openai[[protein_id]]$expression <- get_empty_structure("expression")
      values$openai[[protein_id]]$expression_searched <- TRUE
      values$openai[[protein_id]]$expression_timestamp <- as.character(Sys.time())
      values$openai[[protein_id]]$expression_error <- "JSON parse error"
      values$openai[[protein_id]]$expression_raw <- result$content  # Save raw response
      
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$openai, 'Data/openai.rds')
      
      return(list(success = FALSE, error = "JSON parse error", count = 0))
      
    } else if (nrow(expression_data) == 0) {
      cat("✓ Parse successful but NO RESULTS FOUND (empty array)\n")
      
      # This is valid - protein has no expression data in literature
      values$openai[[protein_id]]$expression <- expression_data  # Save empty but valid structure
      values$openai[[protein_id]]$expression_searched <- TRUE
      values$openai[[protein_id]]$expression_timestamp <- as.character(Sys.time())
      values$openai[[protein_id]]$expression_model <- model
      values$openai[[protein_id]]$expression_result <- "No expression data found in literature"
      
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$openai, 'Data/openai.rds')
      
      # Log token usage
      if (!is.null(result$usage) && exists("log_token_usage")) {
        log_token_usage("expression", protein_id, result$usage)
      }
      
      return(list(success = TRUE, count = 0, message = "No expression data found"))
      
    } else {
      cat("✓ Parse successful with", nrow(expression_data), "results\n")
      
      # Store valid results
      values$openai[[protein_id]]$expression <- expression_data
      values$openai[[protein_id]]$expression_searched <- TRUE
      values$openai[[protein_id]]$expression_timestamp <- as.character(Sys.time())
      values$openai[[protein_id]]$expression_model <- model
      
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$openai, 'Data/openai.rds')
      
      # Log token usage
      if (!is.null(result$usage) && exists("log_token_usage")) {
        log_token_usage("expression", protein_id, result$usage)
      }
      
      return(list(success = TRUE, count = nrow(expression_data)))
    }
    
  } else {
    cat("❌ OpenAI API error:", result$error, "\n")
    
    # Mark as searched with error
    values$openai[[protein_id]]$expression <- get_empty_structure("expression")
    values$openai[[protein_id]]$expression_searched <- TRUE
    values$openai[[protein_id]]$expression_timestamp <- as.character(Sys.time())
    values$openai[[protein_id]]$expression_error <- result$error
    
    if (!dir.exists('Data')) dir.create('Data')
    saveRDS(values$openai, 'Data/openai.rds')
    
    return(list(success = FALSE, error = result$error))
  }
}

# ============================================================================
# BUTTON: EPITOPE SINGLE
# ============================================================================

observeEvent(input$openai_epitope_single, {
  
  cat("\n>>> openai_epitope_single button clicked <<<\n")
  
  selected_id <- input$openai_protein_select
  
  cat("Selected ID:", selected_id, "\n")
  
  if (is.null(selected_id) || !nzchar(selected_id)) {
    showNotification("Please select a protein", type = "error")
    return()
  }
  
  # Check if already searched
  if (input$openai_epitope_skip_searched && 
      !is.null(values$openai[[selected_id]]$epitopes_searched) &&
      values$openai[[selected_id]]$epitopes_searched == TRUE) {
    
    epitope_count <- if (!is.null(values$openai[[selected_id]]$epitopes)) {
      nrow(values$openai[[selected_id]]$epitopes)
    } else {
      0
    }
    
    showNotification(
      paste0(selected_id, " already searched (", epitope_count, " epitopes). ",
             "Uncheck 'Skip already searched' to re-search."),
      type = "warning",
      duration = 5
    )
    return()
  }
  
  withProgress(message = paste("Searching epitopes for", selected_id), value = 0, {
    
    incProgress(0.3, detail = "Querying OpenAI...")
    
    result <- process_epitope_single(selected_id, input$openai_model)
    
    incProgress(1, detail = "Complete!")
    
    if (result$success) {
      showNotification(
        paste("✓ Found", result$count, "epitopes for", selected_id), 
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste("Error:", result$error), 
        type = "error", 
        duration = 10
      )
    }
  })
})

# ============================================================================
# BUTTON: EPITOPE BATCH
# ============================================================================

observeEvent(input$openai_epitope_batch, {
  
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    showNotification("No proteins selected", type = "error")
    return()
  }
  
  # Filter out already searched
  proteins_to_search <- current_ids
  
  if (input$openai_epitope_skip_searched && !is.null(values$openai)) {
    already_searched <- names(values$openai)[sapply(values$openai, function(x) {
      !is.null(x$epitopes_searched) && x$epitopes_searched == TRUE
    })]
    
    proteins_to_search <- setdiff(current_ids, already_searched)
    
    if (length(proteins_to_search) == 0) {
      showNotification(
        "All proteins already searched! Uncheck 'Skip already searched' to re-search.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    showNotification(
      paste("Skipping", length(already_searched), "proteins. Searching", 
            length(proteins_to_search), "new proteins..."), 
      type = "message"
    )
  }
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Searching epitopes...", value = 0)
  
  success_count <- 0
  error_count <- 0
  
  for (i in seq_along(proteins_to_search)) {
    protein_id <- proteins_to_search[i]
    
    progress$set(
      message = paste("Protein", i, "of", length(proteins_to_search)),
      detail = protein_id,
      value = (i - 1) / length(proteins_to_search)
    )
    
    result <- process_epitope_single(protein_id, input$openai_model)
    
    if (result$success) {
      success_count <- success_count + 1
    } else {
      error_count <- error_count + 1
    }
    
    if (i < length(proteins_to_search)) {
      Sys.sleep(input$openai_delay)
    }
  }
  
  progress$set(value = 1, message = "Complete!")
  
  showNotification(
    paste0("Epitope search complete! Success: ", success_count, 
           ", Errors: ", error_count),
    type = if(error_count == 0) "message" else "warning",
    duration = 10
  )
})

# ============================================================================
# BUTTON: EXPRESSION SINGLE
# ============================================================================

observeEvent(input$openai_expression_single, {
  
  cat("\n>>> openai_expression_single button clicked <<<\n")
  
  selected_id <- input$openai_protein_select
  
  cat("Selected ID:", selected_id, "\n")
  
  if (is.null(selected_id) || !nzchar(selected_id)) {
    showNotification("Please select a protein", type = "error")
    return()
  }
  
  # Check if already searched
  if (input$openai_expression_skip_searched && 
      !is.null(values$openai[[selected_id]]$expression_searched) &&
      values$openai[[selected_id]]$expression_searched == TRUE) {
    
    expression_count <- if (!is.null(values$openai[[selected_id]]$expression)) {
      nrow(values$openai[[selected_id]]$expression)
    } else {
      0
    }
    
    showNotification(
      paste0(selected_id, " already searched (", expression_count, " reports). ",
             "Uncheck 'Skip already searched' to re-search."),
      type = "warning",
      duration = 5
    )
    return()
  }
  
  withProgress(message = paste("Searching expression for", selected_id), value = 0, {
    
    incProgress(0.3, detail = "Querying OpenAI...")
    
    result <- process_expression_single(selected_id, input$openai_model)
    
    incProgress(1, detail = "Complete!")
    
    if (result$success) {
      showNotification(
        paste("✓ Found", result$count, "expression reports for", selected_id), 
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste("Error:", result$error), 
        type = "error", 
        duration = 10
      )
    }
  })
})

# ============================================================================
# BUTTON: EXPRESSION BATCH
# ============================================================================

observeEvent(input$openai_expression_batch, {
  
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    showNotification("No proteins selected", type = "error")
    return()
  }
  
  # Filter out already searched
  proteins_to_search <- current_ids
  
  if (input$openai_expression_skip_searched && !is.null(values$openai)) {
    already_searched <- names(values$openai)[sapply(values$openai, function(x) {
      !is.null(x$expression_searched) && x$expression_searched == TRUE
    })]
    
    proteins_to_search <- setdiff(current_ids, already_searched)
    
    if (length(proteins_to_search) == 0) {
      showNotification(
        "All proteins already searched! Uncheck 'Skip already searched' to re-search.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    showNotification(
      paste("Skipping", length(already_searched), "proteins. Searching", 
            length(proteins_to_search), "new proteins..."), 
      type = "message"
    )
  }
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Searching expression data...", value = 0)
  
  success_count <- 0
  error_count <- 0
  
  for (i in seq_along(proteins_to_search)) {
    protein_id <- proteins_to_search[i]
    
    progress$set(
      message = paste("Protein", i, "of", length(proteins_to_search)),
      detail = protein_id,
      value = (i - 1) / length(proteins_to_search)
    )
    
    result <- process_expression_single(protein_id, input$openai_model)
    
    if (result$success) {
      success_count <- success_count + 1
    } else {
      error_count <- error_count + 1
    }
    
    if (i < length(proteins_to_search)) {
      Sys.sleep(input$openai_delay)
    }
  }
  
  progress$set(value = 1, message = "Complete!")
  
  showNotification(
    paste0("Expression search complete! Success: ", success_count, 
           ", Errors: ", error_count),
    type = if(error_count == 0) "message" else "warning",
    duration = 10
  )
})

# ============================================================================
# BUTTON: BOTH SINGLE
# ============================================================================

observeEvent(input$openai_both_single, {
  
  cat("\n>>> openai_both_single button clicked <<<\n")
  
  selected_id <- input$openai_protein_select
  
  cat("Selected ID:", selected_id, "\n")
  
  if (is.null(selected_id) || !nzchar(selected_id)) {
    showNotification("Please select a protein", type = "error")
    return()
  }
  
  # Check what needs to be searched
  need_epitope <- TRUE
  need_expression <- TRUE
  
  if (input$openai_both_skip_searched && !is.null(values$openai[[selected_id]])) {
    if (!is.null(values$openai[[selected_id]]$epitopes_searched) && 
        values$openai[[selected_id]]$epitopes_searched == TRUE) {
      need_epitope <- FALSE
    }
    
    if (!is.null(values$openai[[selected_id]]$expression_searched) && 
        values$openai[[selected_id]]$expression_searched == TRUE) {
      need_expression <- FALSE
    }
  }
  
  if (!need_epitope && !need_expression) {
    showNotification(
      paste(selected_id, "already fully searched. Uncheck 'Skip already searched' to re-search."),
      type = "warning",
      duration = 5
    )
    return()
  }
  
  withProgress(message = paste("Running analyses for", selected_id), value = 0, {
    
    epitope_result <- NULL
    expression_result <- NULL
    
    # Run epitope search if needed
    if (need_epitope) {
      incProgress(0.2, detail = "Searching epitopes...")
      epitope_result <- process_epitope_single(selected_id, input$openai_model)
      Sys.sleep(input$openai_delay)
    } else {
      incProgress(0.2, detail = "Epitopes already searched")
    }
    
    # Run expression search if needed
    if (need_expression) {
      incProgress(0.5, detail = "Searching expression data...")
      expression_result <- process_expression_single(selected_id, input$openai_model)
    } else {
      incProgress(0.5, detail = "Expression already searched")
    }
    
    incProgress(1, detail = "Complete!")
    
    # Build notification
    messages <- c()
    
    if (need_epitope && !is.null(epitope_result)) {
      if (epitope_result$success) {
        messages <- c(messages, paste("Epitopes:", epitope_result$count))
      } else {
        messages <- c(messages, paste("Epitope error"))
      }
    }
    
    if (need_expression && !is.null(expression_result)) {
      if (expression_result$success) {
        messages <- c(messages, paste("Expression:", expression_result$count))
      } else {
        messages <- c(messages, paste("Expression error"))
      }
    }
    
    showNotification(
      paste(selected_id, "-", paste(messages, collapse = ", ")),
      type = "message",
      duration = 7
    )
  })
})

# ============================================================================
# BUTTON: BOTH BATCH
# ============================================================================

observeEvent(input$openai_both_batch, {
  
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    showNotification("No proteins selected", type = "error")
    return()
  }
  
  # Determine which proteins need which searches
  proteins_need_epitope <- current_ids
  proteins_need_expression <- current_ids
  
  if (input$openai_both_skip_searched && !is.null(values$openai)) {
    # Check epitope status
    already_searched_epitope <- names(values$openai)[sapply(values$openai, function(x) {
      !is.null(x$epitopes_searched) && x$epitopes_searched == TRUE
    })]
    proteins_need_epitope <- setdiff(current_ids, already_searched_epitope)
    
    # Check expression status
    already_searched_expression <- names(values$openai)[sapply(values$openai, function(x) {
      !is.null(x$expression_searched) && x$expression_searched == TRUE
    })]
    proteins_need_expression <- setdiff(current_ids, already_searched_expression)
    
    if (length(proteins_need_epitope) == 0 && length(proteins_need_expression) == 0) {
      showNotification(
        "All proteins already searched! Uncheck 'Skip already searched' to re-search.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    showNotification(
      paste("Epitope:", length(proteins_need_epitope), "proteins.",
            "Expression:", length(proteins_need_expression), "proteins."), 
      type = "message"
    )
  }
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Running OpenAI analyses...", value = 0)
  
  all_proteins <- unique(c(proteins_need_epitope, proteins_need_expression))
  total_searches <- length(proteins_need_epitope) + length(proteins_need_expression)
  current_search <- 0
  
  epitope_success <- 0
  epitope_errors <- 0
  expression_success <- 0
  expression_errors <- 0
  
  for (protein_id in all_proteins) {
    
    # Run epitope if needed
    if (protein_id %in% proteins_need_epitope) {
      current_search <- current_search + 1
      progress$set(
        message = paste("Search", current_search, "of", total_searches),
        detail = paste(protein_id, "- epitopes"),
        value = (current_search - 1) / total_searches
      )
      
      result <- process_epitope_single(protein_id, input$openai_model)
      
      if (result$success) {
        epitope_success <- epitope_success + 1
      } else {
        epitope_errors <- epitope_errors + 1
      }
      
      Sys.sleep(input$openai_delay)
    }
    
    # Run expression if needed
    if (protein_id %in% proteins_need_expression) {
      current_search <- current_search + 1
      progress$set(
        message = paste("Search", current_search, "of", total_searches),
        detail = paste(protein_id, "- expression"),
        value = (current_search - 1) / total_searches
      )
      
      result <- process_expression_single(protein_id, input$openai_model)
      
      if (result$success) {
        expression_success <- expression_success + 1
      } else {
        expression_errors <- expression_errors + 1
      }
      
      Sys.sleep(input$openai_delay)
    }
  }
  
  progress$set(value = 1, message = "Complete!")
  
  showNotification(
    paste0("OpenAI analysis complete!\n",
           "Epitopes: ", epitope_success, " success, ", epitope_errors, " errors\n",
           "Expression: ", expression_success, " success, ", expression_errors, " errors"),
    type = if(epitope_errors == 0 && expression_errors == 0) "message" else "warning",
    duration = 10
  )
})

# ============================================================================
# BUTTON: CLEAR ALL DATA
# ============================================================================

observeEvent(input$openai_clear_all, {
  
  n_proteins <- if (!is.null(values$openai)) length(values$openai) else 0
  
  if (n_proteins == 0) {
    showNotification("No OpenAI data to clear", type = "warning")
    return()
  }
  
  showModal(modalDialog(
    title = icon("exclamation-triangle", style = "color: #dc3545;"),
    tags$h4("Clear All OpenAI Data?"),
    tags$p(
      "You are about to delete all OpenAI analysis results for",
      tags$strong(n_proteins), "proteins."
    ),
    tags$p(
      style = "color: #dc3545;",
      icon("exclamation-circle"),
      strong(" This includes:")
    ),
    tags$ul(
      tags$li("All epitope search results"),
      tags$li("All expression search results"),
      tags$li("Search timestamps and metadata")
    ),
    tags$p(
      style = "background-color: #fff3cd; padding: 10px; border-radius: 5px;",
      icon("info-circle"),
      " The saved file (Data/openai.rds) will be deleted. ",
      "You will need to re-run searches to get this data back."
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("openai_confirm_clear", 
                   "Yes, Delete All", 
                   class = "btn-danger",
                   icon = icon("trash-alt"))
    )
  ))
})

observeEvent(input$openai_confirm_clear, {
  
  n_proteins <- if (!is.null(values$openai)) length(values$openai) else 0
  
  # Clear memory
  values$openai <- list()
  
  # Delete file
  if (file.exists('Data/openai.rds')) {
    file.remove('Data/openai.rds')
    cat("Deleted Data/openai.rds\n")
  }
  
  removeModal()
  
  showNotification(
    paste0("✓ Cleared OpenAI data for ", n_proteins, " proteins"),
    type = "message",
    duration = 5
  )
  
  cat("OpenAI data cleared successfully\n")
})

# ============================================================================
# STATUS DISPLAY
# ============================================================================

output$openai_status_ui <- renderUI({
  
  if (is.null(values$openai) || length(values$openai) == 0) {
    return(
      wellPanel(
        h4(icon("info-circle"), " Status: No OpenAI Analysis Run Yet"),
        p("Select a protein and click a search button to start.")
      )
    )
  }
  
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_openai <- values$openai[names(values$openai) %in% current_ids]
  
  if (length(current_openai) == 0) {
    return(
      wellPanel(
        h4(icon("info-circle"), " Status: No Analysis for Current Proteins"),
        p("Click a search button to analyze the selected proteins.")
      )
    )
  }
  
  # Count status
  epitope_searched <- sum(sapply(current_openai, function(x) {
    !is.null(x$epitopes_searched) && x$epitopes_searched == TRUE
  }))
  
  expression_searched <- sum(sapply(current_openai, function(x) {
    !is.null(x$expression_searched) && x$expression_searched == TRUE
  }))
  
  epitope_with_results <- sum(sapply(current_openai, function(x) {
    !is.null(x$epitopes) && nrow(x$epitopes) > 0
  }))
  
  expression_with_results <- sum(sapply(current_openai, function(x) {
    !is.null(x$expression) && nrow(x$expression) > 0
  }))
  
  wellPanel(
    style = "background-color: #d4edda; border-color: #c3e6cb;",
    h4(icon("check-circle"), " OpenAI Analysis Status"),
    
    fluidRow(
      column(6,
             h5(icon("bullseye"), " Epitope Search"),
             tags$ul(
               tags$li(strong("Searched:"), epitope_searched, "/", length(current_ids), "proteins"),
               tags$li(strong("With results:"), epitope_with_results, "proteins"),
               tags$li(strong("Total epitopes:"), sum(sapply(current_openai, function(x) {
                 if (!is.null(x$epitopes)) nrow(x$epitopes) else 0
               })))
             )
      ),
      column(6,
             h5(icon("flask"), " Expression Search"),
             tags$ul(
               tags$li(strong("Searched:"), expression_searched, "/", length(current_ids), "proteins"),
               tags$li(strong("With results:"), expression_with_results, "proteins"),
               tags$li(strong("Total reports:"), sum(sapply(current_openai, function(x) {
                 if (!is.null(x$expression)) nrow(x$expression) else 0
               })))
             )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(6,
             downloadButton('openai_download_epitopes', 
                            'Download Epitopes',
                            class = "btn-success btn-sm btn-block")
      ),
      column(6,
             downloadButton('openai_download_expression', 
                            'Download Expression',
                            class = "btn-success btn-sm btn-block")
      )
    )
  )
})

# ============================================================================
# OUTPUT DISPLAY
# ============================================================================

output$openai_output_ui <- renderUI({
  
  if (is.null(values$openai) || length(values$openai) == 0) {
    return(NULL)
  }
  
  current_ids <- uniprot_ids()
  current_openai <- values$openai[names(values$openai) %in% current_ids]
  
  if (length(current_openai) == 0) {
    return(NULL)
  }
  
  tagList(
    h3("OpenAI Analysis Results"),
    
    tabsetPanel(
      tabPanel("Epitopes",
               br(),
               DT::dataTableOutput('openai_epitope_table')
      ),
      
      tabPanel("Expression History",
               br(),
               DT::dataTableOutput('openai_expression_table')
      ),
      
      tabPanel("Search Summary",
               br(),
               DT::dataTableOutput('openai_search_summary_table')
      )
    )
  )
})

# ============================================================================
# RENDER TABLES
# ============================================================================

output$openai_epitope_table <- DT::renderDataTable({
  
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_openai <- values$openai[names(values$openai) %in% current_ids]
  if (length(current_openai) == 0) return(NULL)
  
  # Combine all epitopes
  epitope_list <- lapply(current_openai, function(x) {
    if (!is.null(x$epitopes) && nrow(x$epitopes) > 0) {
      x$epitopes
    } else {
      NULL
    }
  })
  epitope_list <- epitope_list[!sapply(epitope_list, is.null)]
  
  if (length(epitope_list) == 0) {
    return(data.frame(Message = "No epitopes found"))
  }
  
  epitope_df <- data.table::rbindlist(epitope_list, fill = TRUE)
  
  DT::datatable(
    epitope_df,
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      order = list(list(4, 'desc'))  # Sort by Evidence_Level
    ),
    filter = 'top',
    rownames = FALSE
  ) %>%
    DT::formatStyle(
      'Evidence_Level',
      backgroundColor = DT::styleInterval(
        c(2, 3, 4),
        c('#fee', '#fff3cd', '#d4edda', '#c3e6cb')
      ),
      fontWeight = DT::styleInterval(
        c(3.5),
        c('normal', 'bold')
      )
    )
})

output$openai_expression_table <- DT::renderDataTable({
  
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_openai <- values$openai[names(values$openai) %in% current_ids]
  if (length(current_openai) == 0) return(NULL)
  
  # Combine all expression data
  expression_list <- lapply(current_openai, function(x) {
    if (!is.null(x$expression) && nrow(x$expression) > 0) {
      x$expression
    } else {
      NULL
    }
  })
  expression_list <- expression_list[!sapply(expression_list, is.null)]
  
  if (length(expression_list) == 0) {
    return(data.frame(Message = "No expression data found"))
  }
  
  expression_df <- data.table::rbindlist(expression_list, fill = TRUE)
  
  DT::datatable(
    expression_df,
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      order = list(list(7, 'desc'))  # Sort by Evidence_Score
    ),
    filter = 'top',
    rownames = FALSE
  ) %>%
    DT::formatStyle(
      'Evidence_Score',
      backgroundColor = DT::styleInterval(
        c(2, 3, 4),
        c('#fee', '#fff3cd', '#d4edda', '#c3e6cb')
      )
    )
})

output$openai_search_summary_table <- DT::renderDataTable({
  
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_openai <- values$openai[names(values$openai) %in% current_ids]
  if (length(current_openai) == 0) return(NULL)
  
  # Build summary
  summary_df <- data.frame(
    uniprot_id = current_ids,
    epitope_status = sapply(current_ids, function(id) {
      if (id %in% names(current_openai)) {
        if (!is.null(current_openai[[id]]$epitopes) && 
            nrow(current_openai[[id]]$epitopes) > 0) {
          "✓ Results found"
        } else if (!is.null(current_openai[[id]]$epitopes_searched) && 
                   current_openai[[id]]$epitopes_searched == TRUE) {
          "✓ Searched (none)"
        } else {
          "✗ Not searched"
        }
      } else {
        "✗ Not searched"
      }
    }),
    epitopes_found = sapply(current_ids, function(id) {
      if (id %in% names(current_openai) && 
          !is.null(current_openai[[id]]$epitopes)) {
        nrow(current_openai[[id]]$epitopes)
      } else {
        0
      }
    }),
    expression_status = sapply(current_ids, function(id) {
      if (id %in% names(current_openai)) {
        if (!is.null(current_openai[[id]]$expression) && 
            nrow(current_openai[[id]]$expression) > 0) {
          "✓ Results found"
        } else if (!is.null(current_openai[[id]]$expression_searched) && 
                   current_openai[[id]]$expression_searched == TRUE) {
          "✓ Searched (none)"
        } else {
          "✗ Not searched"
        }
      } else {
        "✗ Not searched"
      }
    }),
    expression_found = sapply(current_ids, function(id) {
      if (id %in% names(current_openai) && 
          !is.null(current_openai[[id]]$expression)) {
        nrow(current_openai[[id]]$expression)
      } else {
        0
      }
    }),
    last_search = sapply(current_ids, function(id) {
      if (id %in% names(current_openai)) {
        timestamps <- c(
          current_openai[[id]]$epitopes_timestamp,
          current_openai[[id]]$expression_timestamp
        )
        timestamps <- timestamps[!sapply(timestamps, is.null)]
        if (length(timestamps) > 0) {
          max(timestamps)
        } else {
          NA
        }
      } else {
        NA
      }
    }),
    stringsAsFactors = FALSE
  )
  
  DT::datatable(
    summary_df,
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      order = list(list(5, 'desc'))
    ),
    rownames = FALSE
  ) %>%
    DT::formatStyle(
      'epitope_status',
      backgroundColor = DT::styleEqual(
        c("✓ Results found", "✓ Searched (none)", "✗ Not searched"),
        c('#c3e6cb', '#fff3cd', '#f8d7da')
      )
    ) %>%
    DT::formatStyle(
      'expression_status',
      backgroundColor = DT::styleEqual(
        c("✓ Results found", "✓ Searched (none)", "✗ Not searched"),
        c('#c3e6cb', '#fff3cd', '#f8d7da')
      )
    )
})

# ============================================================================
# DOWNLOAD HANDLERS
# ============================================================================

output$openai_download_epitopes <- downloadHandler(
  filename = function() {
    paste0("openai_epitopes_", Sys.Date(), ".csv")
  },
  content = function(file) {
    current_ids <- uniprot_ids()
    current_openai <- values$openai[names(values$openai) %in% current_ids]
    
    epitope_list <- lapply(current_openai, function(x) {
      if (!is.null(x$epitopes) && nrow(x$epitopes) > 0) x$epitopes else NULL
    })
    epitope_list <- epitope_list[!sapply(epitope_list, is.null)]
    
    if (length(epitope_list) > 0) {
      epitope_df <- data.table::rbindlist(epitope_list, fill = TRUE)
      write.csv(epitope_df, file, row.names = FALSE)
    }
  }
)

output$openai_download_expression <- downloadHandler(
  filename = function() {
    paste0("openai_expression_", Sys.Date(), ".csv")
  },
  content = function(file) {
    current_ids <- uniprot_ids()
    current_openai <- values$openai[names(values$openai) %in% current_ids]
    
    expression_list <- lapply(current_openai, function(x) {
      if (!is.null(x$expression) && nrow(x$expression) > 0) x$expression else NULL
    })
    expression_list <- expression_list[!sapply(expression_list, is.null)]
    
    if (length(expression_list) > 0) {
      expression_df <- data.table::rbindlist(expression_list, fill = TRUE)
      write.csv(expression_df, file, row.names = FALSE)
    }
  }
)