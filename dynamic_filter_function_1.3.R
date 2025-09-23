# Dynamic Protein Filtering System for Shiny
# This creates an expandable filtering interface

library(shiny)
library(DT)
library(dplyr)

# UI Module for Dynamic Filtering
dynamicFilterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Protein Filtering System"),
    
    # Single dynamic filters section with pre-populated defaults
    wellPanel(
      h5("Filter Conditions"),
      helpText("Add filter conditions to narrow down proteins. New filter rows appear as you complete existing ones."),
      
      # This will contain the dynamic filter rows (including defaults)
      div(id = ns("filter_container"),
          uiOutput(ns("dynamic_filters"))
      ),
      
      br(),
      actionButton(ns("reset_filters"), "Clear All Filters", class = "btn-warning btn-sm")
    ),
    
    # Filter summary and results
    wellPanel(
      h5("Filter Summary"),
      verbatimTextOutput(ns("filter_summary")),
      
      h5("Filtered Results"),
      DTOutput(ns("filtered_table"))
    )
  )
}

# Server Module for Dynamic Filtering  
dynamicFilterServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to track filter conditions
    values <- reactiveValues(
      filter_count = 1,  # Start with 1 empty filter
      filter_conditions = list(),
      max_filters = 20,
      preserve_inputs = list()  # Store inputs to prevent reset
    )
    
    # Get column information from data
    get_column_info <- reactive({
      req(data())
      df <- data()
      
      col_info <- list()
      for (col_name in names(df)) {
        col_data <- df[[col_name]]
        if (is.numeric(col_data)) {
          col_info[[col_name]] <- list(
            type = "numeric",
            min_val = min(col_data, na.rm = TRUE),
            max_val = max(col_data, na.rm = TRUE),
            unique_count = length(unique(col_data[!is.na(col_data)]))
          )
        } else {
          unique_vals <- unique(col_data[!is.na(col_data) & col_data != ""])
          col_info[[col_name]] <- list(
            type = "discrete", 
            values = sort(unique_vals),
            unique_count = length(unique_vals)
          )
        }
      }
      return(col_info)
    })
    
    # Store current input values before UI regeneration
    observe({
      for (i in 1:values$filter_count) {
        # Store column selection
        col_input <- input[[paste0("column_", i)]]
        if (!is.null(col_input)) {
          values$preserve_inputs[[paste0("column_", i)]] <- col_input
        }
        
        # Store numeric inputs
        numeric_op <- input[[paste0("numeric_op_", i)]]
        if (!is.null(numeric_op)) {
          values$preserve_inputs[[paste0("numeric_op_", i)]] <- numeric_op
        }
        
        numeric_val <- input[[paste0("numeric_val_", i)]]
        if (!is.null(numeric_val)) {
          values$preserve_inputs[[paste0("numeric_val_", i)]] <- numeric_val
        }
        
        # Store discrete inputs
        discrete_val <- input[[paste0("discrete_val_", i)]]
        if (!is.null(discrete_val)) {
          values$preserve_inputs[[paste0("discrete_val_", i)]] <- discrete_val
        }
      }
    })
    
    # Generate dynamic filter UI
    output$dynamic_filters <- renderUI({
      req(data())
      col_info <- get_column_info()
      
      filter_rows <- list()
      
      for (i in 1:values$filter_count) {
        filter_rows[[i]] <- create_filter_row(i, col_info, ns)
      }
      
      do.call(tagList, filter_rows)
    })
    
    # Create individual filter row with preserved values
    create_filter_row <- function(row_num, col_info, ns) {
      # Get preserved values
      selected_column <- values$preserve_inputs[[paste0("column_", row_num)]] %||% ""
      
      div(
        id = paste0("filter_row_", row_num),
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
        
        fluidRow(
          column(4,
                 selectInput(
                   ns(paste0("column_", row_num)), 
                   paste("Filter", row_num, "- Column:"),
                   choices = c("Select column..." = "", names(col_info)),
                   selected = selected_column
                 )
          ),
          column(6,
                 uiOutput(ns(paste0("value_input_", row_num)))
          ),
          column(2,
                 br(),
                 actionButton(ns(paste0("remove_", row_num)), "Remove", 
                              class = "btn-danger btn-sm")
          )
        )
      )
    }
    
    # Generate value input based on column selection with preserved values
    observe({
      req(data())
      col_info <- get_column_info()
      
      for (i in 1:values$filter_count) {
        local({
          row_num <- i
          
          output[[paste0("value_input_", row_num)]] <- renderUI({
            column_selected <- input[[paste0("column_", row_num)]] %||% 
              values$preserve_inputs[[paste0("column_", row_num)]]
            
            if (is.null(column_selected) || column_selected == "") {
              return(div())
            }
            
            if (!column_selected %in% names(col_info)) {
              return(div("Column not found"))
            }
            
            col_data <- col_info[[column_selected]]
            
            if (col_data$type == "numeric") {
              # Get preserved values
              selected_op <- values$preserve_inputs[[paste0("numeric_op_", row_num)]] %||% "lte"
              selected_val <- values$preserve_inputs[[paste0("numeric_val_", row_num)]] %||% col_data$max_val
              
              tagList(
                selectInput(ns(paste0("numeric_op_", row_num)), "Operator:",
                            choices = c("≤ (less than or equal)" = "lte",
                                        "≥ (greater than or equal)" = "gte",
                                        "= (equal to)" = "eq",
                                        "≠ (not equal to)" = "neq"),
                            selected = selected_op),
                numericInput(ns(paste0("numeric_val_", row_num)), "Value:",
                             value = selected_val,
                             min = col_data$min_val,
                             max = col_data$max_val)
              )
            } else {
              # Get preserved values
              selected_vals <- values$preserve_inputs[[paste0("discrete_val_", row_num)]] %||% 
                col_data$values[1]
              
              selectInput(ns(paste0("discrete_val_", row_num)), "Select values:",
                          choices = col_data$values,
                          multiple = TRUE,
                          selected = selected_vals)
            }
          })
        })
      }
    })
    
    # Add new filter row when current row is completed
    observe({
      if (values$filter_count >= values$max_filters) return()
      
      # Check if the last row is complete
      last_row <- values$filter_count
      column_selected <- input[[paste0("column_", last_row)]]
      
      if (!is.null(column_selected) && column_selected != "") {
        # Check if value is also selected
        col_info <- get_column_info()
        if (column_selected %in% names(col_info)) {
          col_data <- col_info[[column_selected]]
          
          value_complete <- FALSE
          if (col_data$type == "numeric") {
            numeric_val <- input[[paste0("numeric_val_", last_row)]]
            value_complete <- !is.null(numeric_val)
          } else {
            discrete_val <- input[[paste0("discrete_val_", last_row)]]
            value_complete <- !is.null(discrete_val) && length(discrete_val) > 0
          }
          
          if (value_complete && values$filter_count < values$max_filters) {
            values$filter_count <- values$filter_count + 1
          }
        }
      }
    })
    
    # Handle remove buttons
    observe({
      for (i in 2:values$max_filters) {
        local({
          row_num <- i
          observeEvent(input[[paste0("remove_", row_num)]], {
            if (values$filter_count > 1) {
              values$filter_count <- values$filter_count - 1
            }
          })
        })
      }
    })
    
    # Reset all filters
    observeEvent(input$reset_filters, {
      values$filter_count <- 1
      # Clear all inputs by updating them
      for (i in 1:values$max_filters) {
        updateSelectInput(session, paste0("column_", i), selected = "")
      }
    })
    
    # Apply filters and return filtered data
    filtered_data <- reactive({
      req(data())
      df <- data()
      
      # Apply default filters
      if (input$apply_length) {
        if ("Length" %in% names(df) || "length" %in% names(df)) {
          length_col <- if ("Length" %in% names(df)) "Length" else "length"
          df <- df[df[[length_col]] <= input$max_length, ]
        }
      }
      
      if (input$exclude_secreted) {
        if ("Secreted" %in% names(df)) {
          df <- df[df$Secreted != "Yes", ]
        }
      }
      
      if (input$exclude_homodimer) {
        if ("Multimeric" %in% names(df)) {
          df <- df[df$Multimeric != "Homodimer", ]
        }
      }
      
      # Apply dynamic filters
      col_info <- get_column_info()
      
      for (i in 1:values$filter_count) {
        column_selected <- input[[paste0("column_", i)]]
        
        if (!is.null(column_selected) && column_selected != "" && column_selected %in% names(col_info)) {
          col_data <- col_info[[column_selected]]
          
          if (col_data$type == "numeric") {
            operator <- input[[paste0("numeric_op_", i)]]
            value <- input[[paste0("numeric_val_", i)]]
            
            if (!is.null(operator) && !is.null(value)) {
              if (operator == "lte") {
                df <- df[df[[column_selected]] <= value, ]
              } else if (operator == "gte") {
                df <- df[df[[column_selected]] >= value, ]
              } else if (operator == "eq") {
                df <- df[df[[column_selected]] == value, ]
              } else if (operator == "neq") {
                df <- df[df[[column_selected]] != value, ]
              }
            }
          } else {
            selected_values <- input[[paste0("discrete_val_", i)]]
            if (!is.null(selected_values) && length(selected_values) > 0) {
              df <- df[df[[column_selected]] %in% selected_values, ]
            }
          }
        }
      }
      
      return(df)
    })
    
    # Filter summary - updated for single section
    output$filter_summary <- renderText({
      req(data())
      
      # Ensure data exists and has rows
      original_data <- data()
      if (is.null(original_data) || nrow(original_data) == 0) {
        return("No data available")
      }
      
      original_count <- nrow(original_data)
      
      # Get filtered data safely
      filtered_df <- filtered_data()
      if (is.null(filtered_df)) {
        return("Error: Unable to filter data")
      }
      
      filtered_count <- nrow(filtered_df)
      
      filters_applied <- c()
      
      # Only proceed if we have column info
      tryCatch({
        col_info <- get_column_info()
        
        # Add dynamic filters to summary
        for (i in 1:values$filter_count) {
          column_selected <- input[[paste0("column_", i)]] %||% 
            values$preserve_inputs[[paste0("column_", i)]]
          
          if (!is.null(column_selected) && column_selected != "" && column_selected %in% names(col_info)) {
            col_data <- col_info[[column_selected]]
            if (col_data$type == "numeric") {
              operator <- input[[paste0("numeric_op_", i)]] %||% 
                values$preserve_inputs[[paste0("numeric_op_", i)]]
              value <- input[[paste0("numeric_val_", i)]] %||% 
                values$preserve_inputs[[paste0("numeric_val_", i)]]
              if (!is.null(operator) && !is.null(value) && !is.na(value)) {
                op_symbol <- switch(operator, "lte" = "≤", "gte" = "≥", "eq" = "=", "neq" = "≠")
                filters_applied <- c(filters_applied, paste(column_selected, op_symbol, value))
              }
            } else {
              selected_values <- input[[paste0("discrete_val_", i)]] %||% 
                values$preserve_inputs[[paste0("discrete_val_", i)]]
              if (!is.null(selected_values) && length(selected_values) > 0) {
                if (length(selected_values) <= 3) {
                  filters_applied <- c(filters_applied, 
                                       paste(column_selected, "∈", paste(selected_values, collapse = ", ")))
                } else {
                  filters_applied <- c(filters_applied, 
                                       paste(column_selected, "∈", paste(selected_values[1:3], collapse = ", "), 
                                             paste("... (", length(selected_values), "total )")))
                }
              }
            }
          }
        }
      }, error = function(e) {
        # If there's an error getting column info, just continue without filter details
      })
      
      summary_text <- paste0(
        "Original proteins: ", original_count, "\n",
        "After filtering: ", filtered_count, "\n",
        "Filters applied: ", if (length(filters_applied) > 0) {
          paste(filters_applied, collapse = "; ")
        } else {
          "None"
        }
      )
      
      return(summary_text)
    })
    
    # Add null-coalescing operator helper
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # Filtered results table
    output$filtered_table <- renderDT({
      # Get filtered data safely
      df <- filtered_data()
      
      # Check if data exists and has rows
      if (is.null(df) || nrow(df) == 0) {
        # Return empty datatable with message
        return(datatable(
          data.frame(Message = "No proteins match the current filter criteria"),
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 't'  # Only show table, no other controls
          ),
          rownames = FALSE
        ))
      }
      
      # Apply text truncation safely
      tryCatch({
        df_truncated <- truncate_long_text(df, 25)
      }, error = function(e) {
        # If truncation fails, use original data
        df_truncated <- df
      })
      
      datatable(
        df_truncated,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      )
    })
    
    # Return filtered data for use by parent module
    return(reactive({ filtered_data() }))
  })
}

# Example usage in your main app:
# 
# # In UI:
# dynamicFilterUI("protein_filter")
# 
# # In Server:
# # Assuming you have result_df as a reactive
# filtered_proteins <- dynamicFilterServer("protein_filter", reactive({ result_df() }))
# 
# # You can then use filtered_proteins() in other parts of your app