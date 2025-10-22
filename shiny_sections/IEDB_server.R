# shiny_sections/IEDB_server.R

## IEDB Analysis - Autoimmune Epitope Search #####

# Source IEDB functions
source("functions/iedb_functions.R", local = TRUE)

# Show startup message if data exists - FIXED
observeEvent(reactiveValuesToList(values), {
  if (!is.null(values$iedb) && length(values$iedb) > 0) {
    cat("IEDB: Loaded data for", length(values$iedb), "proteins\n")
    
    showNotification(
      paste("Previously searched IEDB data loaded for", 
            length(values$iedb), "proteins"),
      type = "message",
      duration = 5
    )
  }
}, once = TRUE, ignoreInit = FALSE)

# Update protein selector dropdown
observe({
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    updateSelectizeInput(session, 'iedb_protein_select', 
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
  updateSelectizeInput(session, 'iedb_protein_select', 
                       choices = choices,
                       selected = current_ids[1],
                       server = TRUE)
})

# Helper function to process IEDB search for a single protein
process_iedb_single <- function(uniprot_id, max_results) {
  
  # Get protein info
  protein_info <- values$uniprot_list[[uniprot_id]]
  if (is.null(protein_info)) {
    return(list(success = FALSE, error = "No UniProt data found"))
  }
  
  # Call IEDB API
  iedb_result <- tryCatch({
    get_autoimmune_epitopes_by_uniprot(uniprot_id)
  }, error = function(e) {
    list(
      success = FALSE,
      data = data.frame(),
      count = 0,
      error = paste("IEDB API error:", conditionMessage(e))
    )
  })
  
  # Initialize if doesn't exist
  if (is.null(values$iedb)) {
    values$iedb <- list()
  }
  
  if (is.null(values$iedb[[uniprot_id]])) {
    values$iedb[[uniprot_id]] <- list()
  }
  
  # ALWAYS store a record, even if no epitopes found
  if (iedb_result$success) {
    # Store epitope data (even if empty)
    epitope_df <- iedb_result$data
    
    if (nrow(epitope_df) > 0) {
      values$iedb[[uniprot_id]]$epitopes <- epitope_df
    } else {
      # Store empty dataframe with proper structure
      values$iedb[[uniprot_id]]$epitopes <- data.frame(
        uniprot_id = character(),
        Epitope_Sequence = character(),
        Position = character(),
        Epitope_Type = character(),
        Evidence_Level = numeric(),
        Citation = character(),
        Antibody_Context = character(),
        Source = character(),
        stringsAsFactors = FALSE
      )
    }
    
    values$iedb[[uniprot_id]]$timestamp <- Sys.time()
    values$iedb[[uniprot_id]]$source <- iedb_result$source %||% "IEDB API"
    values$iedb[[uniprot_id]]$searched <- TRUE
    values$iedb[[uniprot_id]]$epitope_count <- nrow(epitope_df)
    
    # Save to disk
    if (!dir.exists('Data')) dir.create('Data')
    saveRDS(values$iedb, 'Data/iedb.rds')
    
    return(list(success = TRUE, count = nrow(epitope_df)))
    
  } else {
    # Even on error, record that we attempted the search
    values$iedb[[uniprot_id]]$epitopes <- data.frame(
      uniprot_id = character(),
      Epitope_Sequence = character(),
      Position = character(),
      Epitope_Type = character(),
      Evidence_Level = numeric(),
      Citation = character(),
      Antibody_Context = character(),
      Source = character(),
      stringsAsFactors = FALSE
    )
    values$iedb[[uniprot_id]]$timestamp <- Sys.time()
    values$iedb[[uniprot_id]]$source <- "IEDB API"
    values$iedb[[uniprot_id]]$searched <- TRUE
    values$iedb[[uniprot_id]]$epitope_count <- 0
    values$iedb[[uniprot_id]]$error <- iedb_result$error
    
    # Save even failed searches
    if (!dir.exists('Data')) dir.create('Data')
    saveRDS(values$iedb, 'Data/iedb.rds')
    
    return(list(success = FALSE, error = iedb_result$error))
  }
}

## Single Protein Search ##

observeEvent(input$iedb_search_single, {
  selected_id <- input$iedb_protein_select
  
  if (is.null(selected_id) || !nzchar(selected_id)) {
    showNotification("Please select a protein", type = "error")
    return()
  }
  
  withProgress(message = paste("Searching IEDB for", selected_id), value = 0, {
    
    incProgress(0.3, detail = "Querying IEDB antigen database...")
    
    result <- process_iedb_single(selected_id, input$iedb_max_results)
    
    incProgress(1, detail = "Complete!")
    
    if (result$success) {
      if (result$count > 0) {
        showNotification(
          paste("✓ Found", result$count, "autoimmune epitopes for", selected_id), 
          type = "message",
          duration = 5
        )
      } else {
        showNotification(
          paste("No autoimmune epitopes found for", selected_id, "(search recorded)"), 
          type = "warning",
          duration = 5
        )
      }
    } else {
      showNotification(
        paste("Error:", result$error, "(search recorded)"), 
        type = "error", 
        duration = 10
      )
    }
  })
})

## Batch Search ##

observeEvent(input$iedb_search_batch, {
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    showNotification("No proteins selected", type = "error")
    return()
  }
  
  # Filter out already searched if checkbox is selected
  if (input$iedb_skip_searched && !is.null(values$iedb)) {
    already_searched <- names(values$iedb)[sapply(values$iedb, function(x) {
      !is.null(x$searched) && x$searched == TRUE
    })]
    
    proteins_to_search <- setdiff(current_ids, already_searched)
    
    if (length(proteins_to_search) == 0) {
      showNotification(
        "All proteins have already been searched! Uncheck 'Skip already searched' to re-search.",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    showNotification(
      paste("Skipping", length(already_searched), "already searched proteins.",
            "Searching", length(proteins_to_search), "new proteins..."), 
      type = "message"
    )
  } else {
    proteins_to_search <- current_ids
    
    showNotification(
      paste("Starting IEDB search for", length(proteins_to_search), "proteins..."), 
      type = "message"
    )
  }
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Searching IEDB...", value = 0)
  
  success_count <- 0
  total_epitopes <- 0
  proteins_with_epitopes <- 0
  
  for (i in seq_along(proteins_to_search)) {
    protein_id <- proteins_to_search[i]
    
    progress$set(
      message = paste("IEDB: protein", i, "of", length(proteins_to_search)),
      detail = protein_id,
      value = (i - 1) / length(proteins_to_search)
    )
    
    result <- process_iedb_single(protein_id, input$iedb_max_results)
    
    if (result$success) {
      success_count <- success_count + 1
      if (result$count > 0) {
        total_epitopes <- total_epitopes + result$count
        proteins_with_epitopes <- proteins_with_epitopes + 1
      }
    }
  }
  
  progress$set(value = 1, message = "Complete!")
  
  showNotification(
    paste0("IEDB search complete! ", success_count, "/", length(proteins_to_search), 
           " proteins queried, ", proteins_with_epitopes, " proteins with epitopes, ",
           total_epitopes, " total epitopes found"),
    type = "message",
    duration = 10
  )
})

## Clear Results ##

observeEvent(input$iedb_clear_results, {
  
  showModal(modalDialog(
    title = "Clear IEDB Results",
    "Are you sure you want to clear all IEDB epitope data?",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("iedb_confirm_clear", "Yes, Clear All", class = "btn-danger")
    )
  ))
})

observeEvent(input$iedb_confirm_clear, {
  values$iedb <- list()
  
  if (file.exists('Data/iedb.rds')) {
    file.remove('Data/iedb.rds')
  }
  
  removeModal()
  showNotification("IEDB results cleared", type = "message")
})

## Reactive Data for Display ##

iedb_data <- reactive({   
  req(values$iedb)
  
  iedb_list <- values$iedb
  current_ids <- uniprot_ids()
  
  # Filter to current proteins
  filtered_list <- iedb_list[names(iedb_list) %in% current_ids]
  
  if (length(filtered_list) == 0) {
    return(list(
      summary = data.frame(),
      epitopes = data.frame()
    ))
  }
  
  # Combine all epitope results (skip empty ones)
  epitope_list <- lapply(filtered_list, function(x) {
    if (!is.null(x$epitopes) && nrow(x$epitopes) > 0) {
      x$epitopes
    } else {
      NULL
    }
  })
  epitope_list <- epitope_list[!sapply(epitope_list, is.null)]
  
  if (length(epitope_list) > 0) {
    epitope_df <- data.table::rbindlist(epitope_list, fill = TRUE)
  } else {
    epitope_df <- data.frame()
  }
  
  # Create summary
  summary_df <- data.frame(
    uniprot_id = names(filtered_list),
    searched = sapply(filtered_list, function(x) {
      if(!is.null(x$searched)) x$searched else FALSE
    }),
    epitopes_found = sapply(filtered_list, function(x) {
      if(!is.null(x$epitope_count)) x$epitope_count else 0
    }),
    timestamp = sapply(filtered_list, function(x) {
      if(!is.null(x$timestamp)) as.character(x$timestamp) else NA
    }),
    source = sapply(filtered_list, function(x) {
      if(!is.null(x$source)) x$source else "IEDB API"
    }),
    error = sapply(filtered_list, function(x) {
      if(!is.null(x$error)) x$error else NA
    }),
    stringsAsFactors = FALSE
  )
  
  list(
    summary = summary_df,
    epitopes = epitope_df
  )
})

## Status UI ##

output$iedb_status_ui <- renderUI({
  data <- iedb_data()
  
  if (nrow(data$summary) == 0) {
    return(
      wellPanel(
        h4(icon("info-circle"), " Status: No IEDB Analysis Run Yet"),
        p("Select a protein and click a search button to start."),
        p(strong("Note:"), "IEDB searches for human autoantibodies against human proteins.")
      )
    )
  }
  
  n_proteins <- nrow(data$summary)
  n_epitopes <- nrow(data$epitopes)
  n_with_epitopes <- sum(data$summary$epitopes_found > 0)
  n_searched <- sum(data$summary$searched)
  
  wellPanel(
    style = "background-color: #d4edda; border-color: #c3e6cb;",
    h4(icon("check-circle"), " IEDB Analysis Status"),
    fluidRow(
      column(4,
             tags$ul(
               tags$li(strong("Proteins searched:"), n_searched),
               tags$li(strong("With epitopes:"), n_with_epitopes)
             )
      ),
      column(4,
             tags$ul(
               tags$li(strong("Total epitopes:"), n_epitopes),
               tags$li(strong("Data source:"), "IEDB API")
             )
      ),
      column(4,
             downloadButton('iedb_download_summary', 
                            'Download Summary',
                            class = "btn-success btn-sm"),
             br(), br(),
             downloadButton('iedb_download_epitopes', 
                            'Download All Epitopes',
                            class = "btn-success btn-sm")
      )
    )
  )
})

## Output UI - Tables ##

output$iedb_output_ui <- renderUI({
  data <- iedb_data()
  
  if (nrow(data$summary) == 0) {
    return(NULL)
  }
  
  tagList(
    h3("IEDB Results"),
    
    # Summary tab
    h4(icon("table"), " Summary by Protein"),
    DT::dataTableOutput('iedb_summary_table'),
    
    hr(),
    
    # Epitopes tab
    h4(icon("dna"), " All Autoimmune Epitopes Found"),
    DT::dataTableOutput('iedb_epitope_table')
  )
})

## Render Tables ##

output$iedb_summary_table <- DT::renderDataTable({
  df <- iedb_data()$summary
  if (nrow(df) == 0) return(NULL)
  
  # Add a status column
  df$status <- ifelse(df$epitopes_found > 0, 
                      "✓ Epitopes found",
                      ifelse(df$searched & df$epitopes_found == 0,
                             "✓ Searched (none found)",
                             "✗ Not searched"))
  
  # Reorder columns
  df <- df %>%
    select(uniprot_id, status, epitopes_found, timestamp, source, error)
  
  DT::datatable(
    df, 
    options = list(
      scrollX = TRUE, 
      pageLength = 25,
      order = list(list(2, 'desc'))  # Sort by epitopes_found
    ),
    rownames = FALSE
  ) %>%
    DT::formatStyle(
      'status',
      backgroundColor = DT::styleEqual(
        c("✓ Epitopes found", "✓ Searched (none found)", "✗ Not searched"),
        c('#c3e6cb', '#fff3cd', '#f8d7da')
      )
    ) %>%
    DT::formatStyle(
      'epitopes_found',
      backgroundColor = DT::styleInterval(
        c(0, 1),
        c('#fff3cd', '#d4edda', '#c3e6cb')
      )
    )
})

output$iedb_epitope_table <- DT::renderDataTable({
  df <- iedb_data()$epitopes
  if (nrow(df) == 0) return(data.frame(Message = "No epitopes found"))
  
  DT::datatable(
    df, 
    options = list(
      scrollX = TRUE, 
      pageLength = 25,
      columnDefs = list(
        list(width = '80px', targets = which(names(df) == "Evidence_Level") - 1),
        list(width = '100px', targets = which(names(df) == "Position") - 1)
      )
    ),
    filter = 'top',
    rownames = FALSE
  ) %>%
    DT::formatStyle(
      'Evidence_Level',
      backgroundColor = DT::styleInterval(
        c(2, 3, 4),
        c('#fee', '#fff3cd', '#d4edda', '#c3e6cb')
      )
    )
})

## Download Handlers ##

output$iedb_download_summary <- downloadHandler(
  filename = function() {
    paste0("iedb_summary_", Sys.Date(), ".csv")
  },
  content = function(file) {
    df <- iedb_data()$summary
    write.csv(df, file, row.names = FALSE)
  }
)

output$iedb_download_epitopes <- downloadHandler(
  filename = function() {
    paste0("iedb_all_epitopes_", Sys.Date(), ".csv")
  },
  content = function(file) {
    df <- iedb_data()$epitopes
    write.csv(df, file, row.names = FALSE)
  }
)