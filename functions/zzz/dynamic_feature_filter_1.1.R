# Feature-based Exclusion Filter for Proteins
# Removes proteins based on feature types and chain length mismatches

library(shiny)
library(DT)
library(dplyr)

# UI Module for Feature Exclusion Filter
featureExclusionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Feature-based Protein Exclusion"),
    
    wellPanel(
      h5("Exclusion Criteria"),
      
      # Feature type exclusions
      div(
        h6("Exclude proteins with these feature types:"),
        helpText("Default exclusions: Signal Peptide, Transit Peptide, Propeptide, Peptide, Non-terminal Residue"),
        uiOutput(ns("feature_type_selector"))
      ),
      
      br(),
      
      # Chain length mismatch
      div(
        checkboxInput(ns("exclude_chain_mismatch"), 
                      "Exclude proteins where chain end ≠ protein length", 
                      value = TRUE),
        helpText("Removes proteins where the chain doesn't span the full protein length")
      ),
      
      # Additional full-length protein filters
      div(
        h6("Additional Full-Length Protein Filters:"),
        checkboxInput(ns("exclude_fragments"), 
                      "Exclude protein fragments", 
                      value = TRUE),
        helpText("Removes proteins with 'Fragment' or 'Partial' annotations"),
        
        checkboxInput(ns("exclude_short_proteins"), 
                      "Exclude very short proteins (< 50 amino acids)", 
                      value = TRUE),
        helpText("Removes peptides and very short sequences"),
        
        checkboxInput(ns("require_met_start"), 
                      "Require proteins starting with Methionine", 
                      value = TRUE),
        helpText("Ensures proteins start with initiating Met (full N-terminus)"),
        
        checkboxInput(ns("exclude_multiple_chains"), 
                      "Exclude proteins with multiple chain features", 
                      value = TRUE),
        helpText("Removes proteins with >1 Chain feature (cleaved/processed proteins)")
      ),
      
      br(),
      
      # Additional feature-based filters
      div(
        h6("Additional Feature Filters:"),
        uiOutput(ns("additional_filters"))
      ),
      
      br(),
      actionButton(ns("add_feature_filter"), "Add Feature Filter", class = "btn-primary btn-sm"),
      actionButton(ns("clear_feature_filters"), "Clear All", class = "btn-warning btn-sm")
    ),
    
    wellPanel(
      h5("Exclusion Summary"),
      verbatimTextOutput(ns("exclusion_summary")),
      
      h5("Remaining Proteins"),
      DTOutput(ns("filtered_proteins"))
    )
  )
}

# Server Module for Feature Exclusion Filter
featureExclusionServer <- function(id, protein_data, feature_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for additional filters
    values <- reactiveValues(
      additional_filter_count = 0,
      max_additional_filters = 5
    )
    
    # Get unique feature types for selection
    feature_types <- reactive({
      req(feature_data())
      feature_df <- feature_data()  # Call the reactive
      if (is.null(feature_df) || nrow(feature_df) == 0) {
        return(character(0))
      }
      unique(feature_df$type[!is.na(feature_df$type)])
    })
    
    # Feature type selector
    output$feature_type_selector <- renderUI({
      types <- feature_types()
      if (length(types) == 0) {
        return(div("No feature types available"))
      }
      
      # Default exclusions
      default_exclusions <- c("SIGNAL", "TRANSIT", "PROPEP", "PEPTIDE", "NON_TER")
      
      # Only select defaults that exist in the data
      selected_defaults <- intersect(default_exclusions, types)
      
      selectInput(ns("excluded_feature_types"), 
                  "Select feature types to exclude:",
                  choices = types,
                  multiple = TRUE,
                  selected = selected_defaults)
    })
    
    # Additional feature filters UI
    output$additional_filters <- renderUI({
      if (values$additional_filter_count == 0) {
        return(div("No additional filters. Click 'Add Feature Filter' to create custom exclusions."))
      }
      
      filter_list <- list()
      for (i in 1:values$additional_filter_count) {
        filter_list[[i]] <- create_feature_filter_row(i, ns, feature_data())
      }
      
      do.call(tagList, filter_list)
    })
    
    # Create individual feature filter row
    create_feature_filter_row <- function(filter_num, ns, feature_df) {
      if (is.null(feature_df) || nrow(feature_df) == 0) {
        return(div("No feature data available"))
      }
      
      # Get unique values for each column
      numeric_cols <- names(feature_df)[sapply(feature_df, is.numeric)]
      text_cols <- names(feature_df)[sapply(feature_df, function(x) is.character(x) || is.factor(x))]
      
      div(
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 3px;",
        fluidRow(
          column(3,
                 selectInput(ns(paste0("feature_filter_column_", filter_num)),
                             paste("Column", filter_num, ":"),
                             choices = c("Select column..." = "", names(feature_df)))
          ),
          column(3,
                 selectInput(ns(paste0("feature_filter_operator_", filter_num)),
                             "Exclude if:",
                             choices = c("Select..." = "",
                                         "equals" = "eq",
                                         "greater than" = "gt", 
                                         "less than" = "lt",
                                         "contains" = "contains"))
          ),
          column(4,
                 uiOutput(ns(paste0("feature_filter_value_", filter_num)))
          ),
          column(2,
                 br(),
                 actionButton(ns(paste0("remove_feature_filter_", filter_num)), 
                              "Remove", class = "btn-danger btn-sm")
          )
        )
      )
    }
    
    # Generate value input for feature filters
    observe({
      req(feature_data())
      feature_df <- feature_data()  # Call the reactive
      if (is.null(feature_df)) return()
      
      for (i in 1:values$max_additional_filters) {
        local({
          filter_num <- i
          
          output[[paste0("feature_filter_value_", filter_num)]] <- renderUI({
            if (filter_num > values$additional_filter_count) return(div())
            
            column_name <- input[[paste0("feature_filter_column_", filter_num)]]
            operator <- input[[paste0("feature_filter_operator_", filter_num)]]
            
            if (is.null(column_name) || column_name == "" || 
                is.null(operator) || operator == "") {
              return(div())
            }
            
            if (!column_name %in% names(feature_df)) {
              return(div("Column not found"))
            }
            
            column_data <- feature_df[[column_name]]
            
            if (is.numeric(column_data)) {
              numericInput(ns(paste0("feature_filter_value_", filter_num)),
                           "Value:",
                           value = median(column_data, na.rm = TRUE),
                           min = min(column_data, na.rm = TRUE),
                           max = max(column_data, na.rm = TRUE))
            } else {
              if (operator == "contains") {
                textInput(ns(paste0("feature_filter_value_", filter_num)),
                          "Text to find:")
              } else {
                unique_values <- unique(column_data[!is.na(column_data)])
                selectInput(ns(paste0("feature_filter_value_", filter_num)),
                            "Value:",
                            choices = unique_values)
              }
            }
          })
        })
      }
    })
    
    # Main filtering logic
    filtered_proteins <- reactive({
      req(protein_data(), feature_data())
      
      proteins <- protein_data()  # Call the reactive
      features <- feature_data()  # Call the reactive
      
      if (is.null(proteins) || nrow(proteins) == 0) {
        return(data.frame())
      }
      
      if (is.null(features) || nrow(features) == 0) {
        return(proteins)  # Return all proteins if no feature data
      }
      
      # Start with all proteins
      proteins_to_exclude <- character(0)
      
      # 1. Exclude based on feature types
      excluded_types <- input$excluded_feature_types
      if (!is.null(excluded_types) && length(excluded_types) > 0) {
        proteins_with_excluded_features <- features %>%
          filter(type %in% excluded_types) %>%
          pull(uniprot_id) %>%
          unique()
        
        proteins_to_exclude <- c(proteins_to_exclude, proteins_with_excluded_features)
      }
      
      # 2. Exclude chain length mismatches
      if (isTRUE(input$exclude_chain_mismatch)) {
        # Look for length column in proteins data
        length_col <- NULL
        if ("length" %in% names(proteins)) {
          length_col <- "length"
        } else if ("Length" %in% names(proteins)) {
          length_col <- "Length"
        } else if ("sequence_length" %in% names(proteins)) {
          length_col <- "sequence_length"
        }
        
        if (!is.null(length_col)) {
          chain_mismatches <- features %>%
            filter(type == "Chain") %>%
            left_join(proteins %>% select(uniprot_id, protein_length = !!sym(length_col)), 
                      by = "uniprot_id") %>%
            filter(!is.na(protein_length) & end != protein_length) %>%
            pull(uniprot_id) %>%
            unique()
          
          proteins_to_exclude <- c(proteins_to_exclude, chain_mismatches)
        }
      }
      
      # 3. Exclude protein fragments
      if (isTRUE(input$exclude_fragments)) {
        if ("protein_name" %in% names(proteins)) {
          fragments <- proteins %>%
            filter(grepl("fragment|partial|truncated", protein_name, ignore.case = TRUE)) %>%
            pull(uniprot_id)
          
          proteins_to_exclude <- c(proteins_to_exclude, fragments)
        }
      }
      
      # 4. Exclude very short proteins
      if (isTRUE(input$exclude_short_proteins)) {
        length_col <- NULL
        if ("length" %in% names(proteins)) {
          length_col <- "length"
        } else if ("Length" %in% names(proteins)) {
          length_col <- "Length" 
        } else if ("sequence_length" %in% names(proteins)) {
          length_col <- "sequence_length"
        }
        
        if (!is.null(length_col)) {
          short_proteins <- proteins %>%
            filter(.data[[length_col]] < 50) %>%
            pull(uniprot_id)
          
          proteins_to_exclude <- c(proteins_to_exclude, short_proteins)
        }
      }
      
      # 5. Require Methionine start
      if (isTRUE(input$require_met_start)) {
        if ("sequence" %in% names(proteins)) {
          non_met_start <- proteins %>%
            filter(!grepl("^M", sequence)) %>%
            pull(uniprot_id)
          
          proteins_to_exclude <- c(proteins_to_exclude, non_met_start)
        }
      }
      
      # 6. Exclude multiple chain proteins
      if (isTRUE(input$exclude_multiple_chains)) {
        multiple_chains <- features %>%
          filter(type == "Chain") %>%
          count(uniprot_id) %>%
          filter(n > 1) %>%
          pull(uniprot_id)
        
        proteins_to_exclude <- c(proteins_to_exclude, multiple_chains)
      }
      
      # 7. Apply additional feature filters
      for (i in 1:values$additional_filter_count) {
        column_name <- input[[paste0("feature_filter_column_", i)]]
        operator <- input[[paste0("feature_filter_operator_", i)]]
        filter_value <- input[[paste0("feature_filter_value_", i)]]
        
        if (!is.null(column_name) && column_name != "" &&
            !is.null(operator) && operator != "" &&
            !is.null(filter_value)) {
          
          if (column_name %in% names(features)) {
            excluded_by_filter <- character(0)
            
            tryCatch({
              if (operator == "eq") {
                excluded_by_filter <- features %>%
                  filter(.data[[column_name]] == filter_value) %>%
                  pull(uniprot_id)
              } else if (operator == "gt" && is.numeric(features[[column_name]])) {
                excluded_by_filter <- features %>%
                  filter(.data[[column_name]] > filter_value) %>%
                  pull(uniprot_id)
              } else if (operator == "lt" && is.numeric(features[[column_name]])) {
                excluded_by_filter <- features %>%
                  filter(.data[[column_name]] < filter_value) %>%
                  pull(uniprot_id)
              } else if (operator == "contains" && is.character(features[[column_name]])) {
                excluded_by_filter <- features %>%
                  filter(grepl(filter_value, .data[[column_name]], ignore.case = TRUE)) %>%
                  pull(uniprot_id)
              }
            }, error = function(e) {
              # Skip this filter if there's an error
            })
            
            proteins_to_exclude <- c(proteins_to_exclude, excluded_by_filter)
          }
        }
      }
      
      # Remove duplicates and filter out excluded proteins
      proteins_to_exclude <- unique(proteins_to_exclude)
      
      # Return proteins NOT in the exclusion list
      remaining_proteins <- proteins %>%
        filter(!uniprot_id %in% proteins_to_exclude)
      
      return(remaining_proteins)
    })
    
    # Add feature filter
    observeEvent(input$add_feature_filter, {
      if (values$additional_filter_count < values$max_additional_filters) {
        values$additional_filter_count <- values$additional_filter_count + 1
      }
    })
    
    # Remove feature filter buttons
    observe({
      for (i in 1:values$max_additional_filters) {
        local({
          filter_num <- i
          observeEvent(input[[paste0("remove_feature_filter_", filter_num)]], {
            if (values$additional_filter_count > 0) {
              values$additional_filter_count <- values$additional_filter_count - 1
            }
          })
        })
      }
    })
    
    # Clear all feature filters
    observeEvent(input$clear_feature_filters, {
      values$additional_filter_count <- 0
      
      # Reset to default exclusions
      types <- feature_types()
      if (length(types) > 0) {
        default_exclusions <- c("SIGNAL", "TRANSIT", "PROPEP", "PEPTIDE", "NON_TER")
        selected_defaults <- intersect(default_exclusions, types)
        updateSelectInput(session, "excluded_feature_types", selected = selected_defaults)
      } else {
        updateSelectInput(session, "excluded_feature_types", selected = character(0))
      }
      
      updateCheckboxInput(session, "exclude_chain_mismatch", value = TRUE)
    })
    
    # Exclusion summary
    output$exclusion_summary <- renderText({
      req(protein_data())
      
      proteins <- protein_data()  # Call the reactive
      if (is.null(proteins) || nrow(proteins) == 0) {
        return("No protein data available")
      }
      
      original_count <- nrow(proteins)
      
      filtered <- filtered_proteins()  # Call the reactive
      if (is.null(filtered)) {
        return("Error in filtering")
      }
      
      remaining_count <- nrow(filtered)
      excluded_count <- original_count - remaining_count
      
      exclusions <- c()
      
      # Feature type exclusions
      if (!is.null(input$excluded_feature_types) && length(input$excluded_feature_types) > 0) {
        exclusions <- c(exclusions, paste("Feature types:", paste(input$excluded_feature_types, collapse = ", ")))
      }
      
      # All the full-length protein filters
      if (isTRUE(input$exclude_chain_mismatch)) {
        exclusions <- c(exclusions, "Chain length mismatches")
      }
      
      if (isTRUE(input$exclude_fragments)) {
        exclusions <- c(exclusions, "Protein fragments")
      }
      
      if (isTRUE(input$exclude_short_proteins)) {
        exclusions <- c(exclusions, "Short proteins (<50 aa)")
      }
      
      if (isTRUE(input$require_met_start)) {
        exclusions <- c(exclusions, "Non-Met starting proteins")
      }
      
      if (isTRUE(input$exclude_multiple_chains)) {
        exclusions <- c(exclusions, "Multiple chain proteins")
      }
      
      # Additional filters
      if (values$additional_filter_count > 0) {
        exclusions <- c(exclusions, paste("Additional feature filters:", values$additional_filter_count))
      }
      
      paste0("Original proteins: ", original_count, "\n",
             "Excluded proteins: ", excluded_count, "\n", 
             "Remaining proteins: ", remaining_count, "\n",
             "Exclusion criteria: ", if(length(exclusions) > 0) paste(exclusions, collapse = "; ") else "None")
    })
    
    # Results table
    output$filtered_proteins <- renderDT({
      df <- filtered_proteins()
      
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(
          data.frame(Message = "All proteins excluded by current filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      datatable(
        truncate_long_text(df,25),
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
    return(reactive({ filtered_proteins() }))
  })
}

# Example usage:
# In UI:
# featureExclusionUI("feature_filter")
# 
# In Server:
# final_proteins <- featureExclusionServer("feature_filter", 
#                                          reactive({ protein_df }),
#                                          reactive({ feature_df }))