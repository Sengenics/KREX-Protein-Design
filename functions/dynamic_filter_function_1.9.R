# Fixed Dynamic Protein Filtering System - Auto-saves defaults to InputData folder

# Default file path for storing filter defaults
DEFAULTS_FILE_PATH <- "../InputData/FilterDefaults.rds"

# DEFAULT FILTER VALUES
# These are used only if FilterDefaults.rds doesn't exist
DEFAULT_FILTER_VALUES <- list(
  protein_length = 1200,
  length = 1200,
  sequence_length = 1200,
  molecular_weight = 150000,
  mass = 150000
)

# Helper function to load defaults from file
load_defaults_from_file <- function(file_path = DEFAULTS_FILE_PATH) {
  if (file.exists(file_path)) {
    tryCatch({
      loaded <- readRDS(file_path)
      if (is.list(loaded) && !is.null(names(loaded))) {
        return(loaded)
      }
    }, error = function(e) {
      message("Error loading defaults file: ", e$message)
    })
  }
  return(DEFAULT_FILTER_VALUES)
}

# Helper function to save defaults to file
save_defaults_to_file <- function(defaults_list, file_path = DEFAULTS_FILE_PATH) {
  # Create directory if it doesn't exist
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  
  tryCatch({
    saveRDS(defaults_list, file_path)
    return(TRUE)
  }, error = function(e) {
    message("Error saving defaults file: ", e$message)
    return(FALSE)
  })
}

# UI Module
dynamicFilterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Protein Filtering System"),
    
    wellPanel(
      h5("Default Filter Values"),
      fluidRow(
        column(6,
               actionButton(ns("edit_defaults"), "Edit Defaults", 
                            class = "btn-info")
        ),
        column(6,
               actionButton(ns("reload_defaults"), "Reload from File", 
                            class = "btn-secondary")
        )
      ),
      tags$small(paste("Defaults file:", DEFAULTS_FILE_PATH))
    ),
    
    wellPanel(
      h5("Filter Conditions"),
      helpText("Add filter conditions to narrow down proteins. Default values will be applied automatically."),
      
      div(id = ns("filter_container"),
          uiOutput(ns("dynamic_filters"))
      ),
      
      br(),
      actionButton(ns("add_filter"), "Add Filter", class = "btn-primary btn-sm"),
      actionButton(ns("clear_filters"), "Clear All", class = "btn-warning btn-sm"),
      actionButton(ns("reset_defaults"), "Apply Defaults", class = "btn-info btn-sm")
    ),
    
    wellPanel(
      h5("Filter Summary"),
      verbatimTextOutput(ns("filter_summary")),
      
      h6("Active Defaults:"),
      verbatimTextOutput(ns("defaults_display"))
    )
  )
}

# Server Module
dynamicFilterServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Null coalescing operator
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    # Reactive values
    values <- reactiveValues(
      filter_count = 0,
      max_filters = 10,
      stored_inputs = list(),
      defaults = load_defaults_from_file(),
      initialized = FALSE,
      apply_defaults_trigger = 0
    )
    
    # Auto-apply defaults on startup
    observe({
      req(data())
      df <- data()
      
      # Only run once on initialization
      if (!values$initialized && !is.null(df) && nrow(df) > 0) {
        # Get columns that have defaults and exist in the data
        default_cols <- names(values$defaults)
        available_cols <- intersect(default_cols, names(df))
        
        # Create a filter for each default column that exists in the data
        if (length(available_cols) > 0) {
          values$filter_count <- min(length(available_cols), values$max_filters)
          
          # Trigger to apply defaults after UI renders
          Sys.sleep(0.1)  # Small delay to ensure UI is ready
          values$apply_defaults_trigger <- values$apply_defaults_trigger + 1
          
          showNotification(
            paste("Applying", values$filter_count, "default filters..."),
            type = "message",
            duration = 2
          )
        }
        
        values$initialized <- TRUE
      }
    })
    
    # Apply defaults to filter dropdowns after UI renders
    observe({
      req(values$apply_defaults_trigger > 0)
      req(data())
      df <- data()
      
      if (!is.null(df) && nrow(df) > 0) {
        default_cols <- names(values$defaults)
        available_cols <- intersect(default_cols, names(df))
        
        # Update each filter column selection
        for (i in seq_along(available_cols)) {
          if (i <= values$filter_count) {
            col <- available_cols[i]
            
            # Update the column selection
            updateSelectInput(
              session,
              paste0("filter_column_", i),
              selected = col
            )
            
            # Store for later use
            values$stored_inputs[[paste0("filter_column_", i)]] <- col
            
            # The value will be set by the reactive output
          }
        }
      }
    })
    
    # Helper function to get default value for a column
    get_default_value <- function(column_name, column_data) {
      if (column_name %in% names(values$defaults)) {
        default_val <- values$defaults[[column_name]]
        
        if (is.numeric(column_data) && is.numeric(default_val)) {
          max_val <- max(column_data, na.rm = TRUE)
          min_val <- min(column_data, na.rm = TRUE)
          
          if (default_val >= min_val && default_val <= max_val) {
            return(default_val)
          } else {
            return(max_val)
          }
        }
        return(default_val)
      }
      
      if (is.numeric(column_data)) {
        return(max(column_data, na.rm = TRUE))
      } else {
        unique_values <- unique(column_data[!is.na(column_data)])
        if (any(is.na(column_data))) {
          unique_values <- c(unique_values, "(NA)")
        }
        return(unique_values)
      }
    }
    
    # Display current defaults
    output$defaults_display <- renderText({
      if (length(values$defaults) == 0) {
        return("No defaults set")
      }
      
      defaults_text <- sapply(names(values$defaults), function(col) {
        val <- values$defaults[[col]]
        if (is.numeric(val)) {
          paste0(col, " = ", val)
        } else {
          paste0(col, " = ", paste(val, collapse = ", "))
        }
      })
      
      paste(defaults_text, collapse = "\n")
    })
    
    # Reload defaults from file
    observeEvent(input$reload_defaults, {
      values$defaults <- load_defaults_from_file()
      showNotification("Defaults reloaded from file", type = "success")
    })
    
    # Edit defaults modal
    observeEvent(input$edit_defaults, {
      showModal(modalDialog(
        title = "Edit Default Filter Values",
        size = "l",
        
        fluidPage(
          helpText("Enter default values for filter columns. These will be applied when adding new filters."),
          helpText("Changes are automatically saved to: ", DEFAULTS_FILE_PATH),
          tags$hr(),
          
          uiOutput(ns("defaults_editor_content")),
          
          br(),
          h5("Add New Default:"),
          fluidRow(
            column(5,
                   textInput(ns("new_default_column"), "Column name:")
            ),
            column(5,
                   textInput(ns("new_default_value"), "Default value:"),
                   helpText("For text, separate multiple values with commas")
            ),
            column(2,
                   br(),
                   actionButton(ns("add_default"), "Add", class = "btn-primary btn-sm")
            )
          )
        ),
        
        footer = tagList(
          actionButton(ns("save_defaults_modal"), "Save Changes", class = "btn-success"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Generate defaults editor content
    output$defaults_editor_content <- renderUI({
      if (length(values$defaults) == 0) {
        return(div("No defaults set. Add defaults using the fields below."))
      }
      
      default_rows <- lapply(names(values$defaults), function(col) {
        val <- values$defaults[[col]]
        display_val <- if (is.numeric(val)) {
          as.character(val)
        } else {
          paste(val, collapse = ", ")
        }
        
        fluidRow(
          column(4, strong(col)),
          column(6, 
                 textInput(ns(paste0("default_edit_", col)), 
                           label = NULL,
                           value = display_val)
          ),
          column(2,
                 actionButton(ns(paste0("remove_default_", col)), 
                              "Remove", 
                              class = "btn-danger btn-sm")
          ),
          style = "margin-bottom: 10px;"
        )
      })
      
      do.call(tagList, default_rows)
    })
    
    # Add new default
    observeEvent(input$add_default, {
      req(input$new_default_column, input$new_default_value)
      
      col_name <- trimws(input$new_default_column)
      val_text <- trimws(input$new_default_value)
      
      if (col_name == "" || val_text == "") {
        showNotification("Please enter both column name and value", type = "warning")
        return()
      }
      
      parsed_val <- suppressWarnings({
        num_val <- as.numeric(val_text)
        if (!is.na(num_val)) {
          num_val
        } else {
          trimws(strsplit(val_text, ",")[[1]])
        }
      })
      
      values$defaults[[col_name]] <- parsed_val
      
      updateTextInput(session, "new_default_column", value = "")
      updateTextInput(session, "new_default_value", value = "")
      
      showNotification(paste("Added default for", col_name), type = "message")
    })
    
    # Remove default buttons
    observe({
      for (col in names(values$defaults)) {
        local({
          col_name <- col
          observeEvent(input[[paste0("remove_default_", col_name)]], {
            values$defaults[[col_name]] <- NULL
            showNotification(paste("Removed default for", col_name), type = "message")
          })
        })
      }
    })
    
    # Save defaults from modal
    observeEvent(input$save_defaults_modal, {
      for (col in names(values$defaults)) {
        input_val <- input[[paste0("default_edit_", col)]]
        if (!is.null(input_val) && nzchar(input_val)) {
          parsed_val <- suppressWarnings({
            num_val <- as.numeric(input_val)
            if (!is.na(num_val)) {
              num_val
            } else {
              trimws(strsplit(input_val, ",")[[1]])
            }
          })
          values$defaults[[col]] <- parsed_val
        }
      }
      
      if (save_defaults_to_file(values$defaults)) {
        showNotification("Defaults saved successfully", type = "message")
      } else {
        showNotification("Error saving defaults file", type = "error")
      }
      
      removeModal()
    })
    
    # Store current input values
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
    
    # Filtered data
    filtered_data <- reactive({
      req(data())
      df <- data()
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      
      if (values$filter_count == 0) {
        return(df)
      }
      
      for (i in 1:values$filter_count) {
        column_name <- input[[paste0("filter_column_", i)]] %||% 
          values$stored_inputs[[paste0("filter_column_", i)]]
        filter_value <- input[[paste0("filter_value_", i)]] %||% 
          values$stored_inputs[[paste0("filter_value_", i)]]
        
        if (is.null(column_name) || column_name == "" || 
            is.null(filter_value) || length(filter_value) == 0) {
          next
        }
        
        if (!column_name %in% names(df)) {
          next
        }
        
        if (is.numeric(df[[column_name]])) {
          if (is.numeric(filter_value) && !is.na(filter_value)) {
            df <- df[!is.na(df[[column_name]]) & df[[column_name]] <= filter_value, ]
          }
        } else {
          if ("(NA)" %in% filter_value) {
            df <- df[is.na(df[[column_name]]) | 
                       (!is.na(df[[column_name]]) & df[[column_name]] %in% filter_value), ]
          } else {
            df <- df[!is.na(df[[column_name]]) & df[[column_name]] %in% filter_value, ]
          }
        }
        
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
    
    # Create single filter row
    create_single_filter <- function(filter_num, ns, df) {
      if (is.null(df) || ncol(df) == 0) {
        return(div("No data available"))
      }
      
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
    
    # Generate value inputs
    observe({
      req(data())
      req(nrow(data()) > 0)
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
              default_val <- if (!is.null(stored_value) && is.numeric(stored_value)) {
                stored_value
              } else {
                get_default_value(column_name, column_data)
              }
              
              numericInput(ns(paste0("filter_value_", filter_num)), 
                           "Max value:",
                           value = default_val,
                           min = min(column_data, na.rm = TRUE),
                           max = max(column_data, na.rm = TRUE))
            } else {
              unique_values <- unique(column_data[!is.na(column_data)])
              
              # Add "(NA)" as a choice if there are any NAs
              has_na <- any(is.na(column_data))
              if (has_na) {
                unique_values <- c(unique_values, "(NA)")
              }
              
              default_vals <- if (!is.null(stored_value)) {
                stored_value
              } else {
                get_default_value(column_name, column_data)
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
      values$stored_inputs <- list()
    })
    
    # Reset to defaults button - now triggers automatic application
    observeEvent(input$reset_defaults, {
      req(data())
      df <- data()
      
      if (!is.null(df) && nrow(df) > 0) {
        # Get columns that have defaults and exist in the data
        default_cols <- names(values$defaults)
        available_cols <- intersect(default_cols, names(df))
        
        if (length(available_cols) > 0) {
          # Clear existing filters first
          values$stored_inputs <- list()
          
          # Set filter count
          values$filter_count <- min(length(available_cols), values$max_filters)
          
          # Trigger application after UI updates
          values$apply_defaults_trigger <- values$apply_defaults_trigger + 1
          
          showNotification(
            paste("Applying", values$filter_count, "default filters..."),
            type = "message",
            duration = 2
          )
        } else {
          showNotification(
            "No default columns found in current data",
            type = "warning"
          )
        }
      }
    })
    
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
    
    # Return filtered data
    return(reactive({ filtered_data() }))
  })
}