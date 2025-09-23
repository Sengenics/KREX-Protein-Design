# Simplified Dynamic Protein Filtering System for Shiny

library(shiny)
library(DT)
library(dplyr)

# UI Module for Dynamic Filtering - SIMPLIFIED
dynamicFilterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Protein Filtering System"),
    
    wellPanel(
      h5("Filter Conditions"),
      helpText("Add filter conditions to narrow down proteins."),
      
      div(id = ns("filter_container"),
          uiOutput(ns("dynamic_filters"))
      ),
      
      br(),
      actionButton(ns("add_filter"), "Add Filter", class = "btn-primary btn-sm"),
      actionButton(ns("clear_filters"), "Clear All", class = "btn-warning btn-sm")
    ),
    
    wellPanel(
      h5("Filter Summary"),
      verbatimTextOutput(ns("filter_summary")),
      
      h5("Filtered Results"),
      DTOutput(ns("filtered_table"))
    )
  )
}

# Server Module for Dynamic Filtering - SIMPLIFIED
dynamicFilterServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Simple reactive values
    values <- reactiveValues(
      filter_count = 0,
      max_filters = 10,
      stored_inputs = list()  # Store input values to prevent reset
    )
    
    # Store current input values before UI changes
    observe({
      for (i in 1:values$filter_count) {
        column_val <- input[[paste0("filter_column_", i)]]
        value_val <- input[[paste0("filter_value_", i)]]
        
        if (!is.null(column_val)) {
          values$stored_inputs[[paste0("filter_column_", i)]] <- column_val
        }
        if (!is.null(value_val)) {
          values$stored_inputs[[paste0("filter_value_", i)]] <- value_val
        }
      }
    })
    # Always return the data, with or without filters
    filtered_data <- reactive({
      df <- data()
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      
      # If no filters, return original data
      if (values$filter_count == 0) {
        return(df)
      }
      
      # Apply each filter one by one
      for (i in 1:values$filter_count) {
        # Get values from input or stored values
        column_name <- input[[paste0("filter_column_", i)]] %||% 
          values$stored_inputs[[paste0("filter_column_", i)]]
        filter_value <- input[[paste0("filter_value_", i)]] %||% 
          values$stored_inputs[[paste0("filter_value_", i)]]
        
        # Skip if column not selected or no value
        if (is.null(column_name) || column_name == "" || 
            is.null(filter_value) || length(filter_value) == 0) {
          next
        }
        
        # Skip if column doesn't exist in data
        if (!column_name %in% names(df)) {
          next
        }
        
        # Apply filter based on column type
        if (is.numeric(df[[column_name]])) {
          # For numeric: filter_value is the max value to keep
          if (is.numeric(filter_value) && !is.na(filter_value)) {
            df <- df[!is.na(df[[column_name]]) & df[[column_name]] <= filter_value, ]
          }
        } else {
          # For character/factor: keep only selected values
          df <- df[!is.na(df[[column_name]]) & df[[column_name]] %in% filter_value, ]
        }
        
        # If no rows left, return empty data frame with same structure
        if (nrow(df) == 0) {
          return(df[0, ])
        }
      }
      
      return(df)
    })
    
    # Generate filter UI
    output$dynamic_filters <- renderUI({
      if (values$filter_count == 0) {
        return(div("No filters applied. Click 'Add Filter' to start."))
      }
      
      filter_list <- list()
      for (i in 1:values$filter_count) {
        filter_list[[i]] <- create_single_filter(i, ns, data())
      }
      
      do.call(tagList, filter_list)
    })
    
    # Create a single filter row
    create_single_filter <- function(filter_num, ns, df) {
      if (is.null(df) || ncol(df) == 0) {
        return(div("No data available"))
      }
      
      # Get stored values to preserve selections
      stored_column <- values$stored_inputs[[paste0("filter_column_", filter_num)]]
      selected_column <- if (!is.null(stored_column)) stored_column else ""
      
      div(
        style = "border: 1px solid #ccc; padding: 10px; margin: 5px 0;",
        fluidRow(
          column(4,
                 selectInput(ns(paste0("filter_column_", filter_num)), 
                             paste("Filter", filter_num, "Column:"),
                             choices = c("Select..." = "", names(df)),
                             selected = selected_column)
          ),
          column(6,
                 uiOutput(ns(paste0("filter_value_", filter_num)))
          ),
          column(2,
                 br(),
                 actionButton(ns(paste0("remove_", filter_num)), "Remove", 
                              class = "btn-danger btn-sm")
          )
        )
      )
    }
    
    # Generate value input based on selected column
    observe({
      df <- data()
      if (is.null(df)) return()
      
      for (i in 1:values$max_filters) {
        local({
          filter_num <- i
          
          output[[paste0("filter_value_", filter_num)]] <- renderUI({
            if (filter_num > values$filter_count) return(div())
            
            column_name <- input[[paste0("filter_column_", filter_num)]] %||%
              values$stored_inputs[[paste0("filter_column_", filter_num)]]
            
            if (is.null(column_name) || column_name == "" || !column_name %in% names(df)) {
              return(div())
            }
            
            column_data <- df[[column_name]]
            stored_value <- values$stored_inputs[[paste0("filter_value_", filter_num)]]
            
            if (is.numeric(column_data)) {
              # Use stored value or default to max
              default_val <- if (!is.null(stored_value) && is.numeric(stored_value)) {
                stored_value
              } else {
                max(column_data, na.rm = TRUE)
              }
              
              numericInput(ns(paste0("filter_value_", filter_num)), 
                           "Max value:",
                           value = default_val,
                           min = min(column_data, na.rm = TRUE),
                           max = max(column_data, na.rm = TRUE))
            } else {
              unique_values <- unique(column_data[!is.na(column_data)])
              
              # Use stored values or default to all
              default_vals <- if (!is.null(stored_value) && all(stored_value %in% unique_values)) {
                stored_value
              } else {
                unique_values
              }
              
              selectInput(ns(paste0("filter_value_", filter_num)), 
                          "Select values:",
                          choices = unique_values,
                          selected = default_vals,
                          multiple = TRUE)
            }
          })
        })
      }
    })
    
    # Add filter button
    observeEvent(input$add_filter, {
      if (values$filter_count < values$max_filters) {
        values$filter_count <- values$filter_count + 1
      }
    })
    
    # Remove filter buttons
    observe({
      for (i in 1:values$max_filters) {
        local({
          filter_num <- i
          observeEvent(input[[paste0("remove_", filter_num)]], {
            if (values$filter_count > 0) {
              values$filter_count <- values$filter_count - 1
            }
          })
        })
      }
    })
    
    # Clear all filters
    observeEvent(input$clear_filters, {
      values$filter_count <- 0
      values$stored_inputs <- list()  # Clear stored inputs too
    })
    
    # Add null coalescing operator
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # Filter summary
    output$filter_summary <- renderText({
      df <- data()
      if (is.null(df)) return("No data")
      
      original_count <- nrow(df)
      filtered_count <- nrow(filtered_data())
      
      paste0("Original: ", original_count, " proteins\n",
             "Filtered: ", filtered_count, " proteins\n",
             "Active filters: ", values$filter_count)
    })
    
    # Results table
    output$filtered_table <- renderDT({
      df <- filtered_data()
      
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(
          data.frame(Message = "No data or no matches"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      datatable(
        df,
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
    
    # Return filtered data
    return(reactive({ filtered_data() }))
  })
}