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
    
    # Default filters section
    wellPanel(
      h5("Default Filters (always applied)"),
      checkboxInput(ns("apply_length"), "Protein Length ≤ 1200", value = TRUE),
      checkboxInput(ns("exclude_secreted"), "Exclude Secreted Proteins", value = TRUE),
      checkboxInput(ns("exclude_homodimer"), "Exclude Homodimers", value = TRUE),
      
      # Allow customization of default values
      conditionalPanel(
        condition = paste0("input['", ns("apply_length"), "']"),
        numericInput(ns("max_length"), "Maximum Length:", value = 1200, min = 1, max = 5000)
      )
    ),
    
    # Dynamic filters section
    wellPanel(
      h5("Additional Filters"),
      helpText("Add custom filter conditions. New filter rows appear as you complete existing ones."),
      
      # This will contain the dynamic filter rows
      div(id = ns("filter_container"),
          uiOutput(ns("dynamic_filters"))
      ),
      
      br(),
      actionButton(ns("reset_filters"), "Reset All Filters", class = "btn-warning btn-sm")
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
      filter_count = 1,
      filter_conditions = list(),
      max_filters = 20  # Reasonable limit
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
    
    # Create individual filter row
    create_filter_row <- function(row_num, col_info, ns) {
      div(
        id = paste0("filter_row_", row_num),
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
        
        fluidRow(
          column(4,
                 selectInput(
                   ns(paste0("column_", row_num)), 
                   paste("Filter", row_num, "- Column:"),
                   choices = c("Select column..." = "", names(col_info)),
                   selected = ""
                 )
          ),
          column(6,
                 uiOutput(ns(paste0("value_input_", row_num)))
          ),
          column(2,
                 br(),
                 if (row_num > 1) {
                   actionButton(ns(paste0("remove_", row_num)), "Remove", 
                                class = "btn-danger btn-sm")
                 }
          )
        )
      )
    }
    
    # Generate value input based on column selection
    observe({
      req(data())
      col_info <- get_column_info()
      
      for (i in 1:values$filter_count) {
        local({
          row_num <- i
          
          output[[paste0("value_input_", row_num)]] <- renderUI({
            column_selected <- input[[paste0("column_", row_num)]]
            
            if (is.null(column_selected) || column_selected == "") {
              return(div())
            }
            
            col_data <- col_info[[column_selected]]
            
            if (col_data$type == "numeric") {
              tagList(
                selectInput(ns(paste0("numeric_op_", row_num)), "Operator:",
                            choices = c("≤ (less than or equal)" = "lte",
                                        "≥ (greater than or equal)" = "gte",
                                        "= (equal to)" = "eq",
                                        "≠ (not equal to)" = "neq"),
                            selected = "lte"),
                numericInput(ns(paste0("numeric_val_", row_num)), "Value:",
                             value = col_data$max_val,
                             min = col_data$min_val,
                             max = col_data$max_val)
              )
            } else {
              selectInput(ns(paste0("discrete_val_", row_num)), "Select values:",
                          choices = col_data$values,
                          multiple = TRUE,
                          selected = col_data$values[1])
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
    
    # Filter summary
    output$filter_summary <- renderText({
      req(data())
      original_count <- nrow(data())
      filtered_count <- nrow(filtered_data())
      
      filters_applied <- c()
      
      if (input$apply_length) {
        filters_applied <- c(filters_applied, paste("Length ≤", input$max_length))
      }
      if (input$exclude_secreted) {
        filters_applied <- c(filters_applied, "Exclude secreted")
      }
      if (input$exclude_homodimer) {
        filters_applied <- c(filters_applied, "Exclude homodimers")
      }
      
      # Add dynamic filters to summary
      col_info <- get_column_info()
      for (i in 1:values$filter_count) {
        column_selected <- input[[paste0("column_", i)]]
        if (!is.null(column_selected) && column_selected != "") {
          if (column_selected %in% names(col_info)) {
            col_data <- col_info[[column_selected]]
            if (col_data$type == "numeric") {
              operator <- input[[paste0("numeric_op_", i)]]
              value <- input[[paste0("numeric_val_", i)]]
              if (!is.null(operator) && !is.null(value)) {
                op_symbol <- switch(operator, "lte" = "≤", "gte" = "≥", "eq" = "=", "neq" = "≠")
                filters_applied <- c(filters_applied, paste(column_selected, op_symbol, value))
              }
            } else {
              selected_values <- input[[paste0("discrete_val_", i)]]
              if (!is.null(selected_values) && length(selected_values) > 0) {
                filters_applied <- c(filters_applied, 
                                     paste(column_selected, "in", paste(selected_values, collapse = ", ")))
              }
            }
          }
        }
      }
      
      summary_text <- paste0(
        "Original proteins: ", original_count, "\n",
        "After filtering: ", filtered_count, "\n",
        "Filters applied: ", ifelse(length(filters_applied) > 0, 
                                    paste(filters_applied, collapse = "; "), 
                                    "None")
      )
      
      return(summary_text)
    })
    
    # Filtered results table
    output$filtered_table <- renderDT({
      datatable(
        truncate_long_text(filtered_data()),
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