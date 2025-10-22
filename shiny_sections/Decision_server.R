# shiny_sections/decision_server.R
# Complete decision server with rule-based and AI comparison

# Source decision functions
source("functions/decision_functions.R", local = TRUE)
source("functions/disorder_calculation.R", local = TRUE)

library(r3dmol)
library(bio3d)

## AI Decision System #####

# Track currently displayed protein
current_decision_protein <- reactiveVal(NULL)

# Update protein selector dropdown when uniprot_ids changes
observe({
  current_ids <- uniprot_ids()
  
  if (length(current_ids) == 0) {
    updateSelectInput(session, 'decision_protein_select', 
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
  updateSelectInput(session, 'decision_protein_select', 
                    choices = choices,
                    selected = if(!is.null(current_decision_protein())) {
                      current_decision_protein()
                    } else {
                      current_ids[1]
                    })
})

# Update current protein when selection changes
observeEvent(input$decision_protein_select, {
  if (!is.null(input$decision_protein_select) && 
      nzchar(input$decision_protein_select)) {
    current_decision_protein(input$decision_protein_select)
  }
})

# Navigate to previous protein
observeEvent(input$decision_prev, {
  current_ids <- uniprot_ids()
  if (length(current_ids) <= 1) return()
  
  current_id <- current_decision_protein()
  if (is.null(current_id)) current_id <- current_ids[1]
  
  current_index <- which(current_ids == current_id)
  if (length(current_index) == 0) current_index <- 1
  
  new_index <- if(current_index == 1) length(current_ids) else current_index - 1
  current_decision_protein(current_ids[new_index])
})

# Navigate to next protein
observeEvent(input$decision_next, {
  current_ids <- uniprot_ids()
  if (length(current_ids) <= 1) return()
  
  current_id <- current_decision_protein()
  if (is.null(current_id)) current_id <- current_ids[1]
  
  current_index <- which(current_ids == current_id)
  if (length(current_index) == 0) current_index <- 1
  
  new_index <- if(current_index == length(current_ids)) 1 else current_index + 1
  current_decision_protein(current_ids[new_index])
})

# Display current protein
output$decision_current_protein <- renderText({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) {
    return("No proteins selected")
  }
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
    current_decision_protein(current_id)
  }
  
  protein_name <- if (!is.null(values$uniprot_list[[current_id]]$protein_name)) {
    values$uniprot_list[[current_id]]$protein_name
  } else {
    current_id
  }
  
  current_index <- which(current_ids == current_id)
  
  paste0(protein_name, "\n(", current_id, ")\n",
         "Protein ", current_index, " of ", length(current_ids))
})

# Check available data for currently selected protein
decision_data_status <- reactive({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) {
    return(list(
      uniprot = FALSE,
      features = FALSE,
      epitopes = FALSE,
      expression = FALSE,
      iedb = FALSE,
      ready = FALSE
    ))
  }
  
  current_id <- input$decision_protein_select
  if (is.null(current_id) || !nzchar(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  has_uniprot <- !is.null(values$uniprot_list[[current_id]])
  has_features <- !is.null(values$feature_list[[current_id]])
  has_epitopes <- !is.null(values$openai[[current_id]]$epitopes)
  has_expression <- !is.null(values$openai[[current_id]]$expression)
  has_iedb <- !is.null(values$iedb[[current_id]]$epitopes)
  
  list(
    uniprot = has_uniprot,
    features = has_features,
    epitopes = has_epitopes,
    expression = has_expression,
    iedb = has_iedb,
    ready = has_uniprot && has_features
  )
})

output$decision_data_status <- renderUI({
  status <- decision_data_status()
  
  make_status_badge <- function(label, available) {
    color <- if(available) "success" else "secondary"
    icon_name <- if(available) "check" else "times"
    tags$span(
      class = paste0("badge badge-", color),
      style = "margin: 2px;",
      icon(icon_name), " ", label
    )
  }
  
  tagList(
    make_status_badge("UniProt", status$uniprot),
    make_status_badge("Features", status$features),
    make_status_badge("OpenAI Epitopes", status$epitopes),
    make_status_badge("IEDB Epitopes", status$iedb),
    make_status_badge("Expression", status$expression)
  )
})

# Helper function to process a single protein decision
process_single_protein_decision <- function(protein_id, model, n_buffer, c_buffer) {
  
  tryCatch({
    cat("\n=== Starting Decision Analysis for", protein_id, "===\n")
    
    # Get data
    uniprot_data <- values$uniprot_list[[protein_id]]
    features_df <- values$feature_list[[protein_id]]
    
    # Get epitopes from BOTH OpenAI AND IEDB - PRIORITIZE IEDB
    epitope_df <- NULL
    epitope_source <- "none"
    
    # Check IEDB first (more specific for autoimmune)
    if (!is.null(values$iedb[[protein_id]]$epitopes) && 
        nrow(values$iedb[[protein_id]]$epitopes) > 0) {
      epitope_df <- values$iedb[[protein_id]]$epitopes
      epitope_source <- "IEDB"
      cat("Using IEDB epitopes:", nrow(epitope_df), "epitopes\n")
    } 
    # Fall back to OpenAI if no IEDB data
    else if(!is.null(values$openai[[protein_id]]$epitopes) && 
            nrow(values$openai[[protein_id]]$epitopes) > 0) {
      epitope_df <- values$openai[[protein_id]]$epitopes
      epitope_source <- "OpenAI"
      cat("Using OpenAI epitopes:", nrow(epitope_df), "epitopes\n")
    } else {
      cat("No epitope data found\n")
    }
    
    # Print epitope dataframe structure for debugging
    if (!is.null(epitope_df)) {
      cat("Epitope data structure:\n")
      cat("  Rows:", nrow(epitope_df), "\n")
      cat("  Columns:", paste(names(epitope_df), collapse = ", "), "\n")
      if ("Position" %in% names(epitope_df)) {
        cat("  Sample positions:", paste(head(epitope_df$Position, 3), collapse = ", "), "\n")
      }
      if ("Epitope_Type" %in% names(epitope_df)) {
        cat("  Epitope types:", paste(unique(epitope_df$Epitope_Type), collapse = ", "), "\n")
      }
    }
    
    expression_df <- if(!is.null(values$openai[[protein_id]])) {
      values$openai[[protein_id]]$expression
    } else {
      NULL
    }
    
    # Check if we have the minimum data
    if (is.null(uniprot_data) || is.null(features_df)) {
      return(list(success = FALSE, error = "Missing required protein data"))
    }
    
    # Extract sequence length
    sequence_length <- NULL
    if (!is.null(uniprot_data$sequence_length)) {
      sequence_length <- uniprot_data$sequence_length
    } else if (!is.null(uniprot_data$length)) {
      sequence_length <- uniprot_data$length
    } else if (!is.null(uniprot_data$Length)) {
      sequence_length <- uniprot_data$Length
    } else if (nrow(features_df) > 0) {
      sequence_length <- max(features_df$end, na.rm = TRUE)
    }
    
    if (is.null(sequence_length) || !is.finite(sequence_length)) {
      return(list(success = FALSE, error = "Could not determine sequence length"))
    }
    
    cat("Sequence length:", sequence_length, "\n")
    
    # Extract protein name
    protein_name <- NULL
    if (!is.null(uniprot_data$protein_name)) {
      protein_name <- uniprot_data$protein_name
    } else if (!is.null(uniprot_data$name)) {
      protein_name <- uniprot_data$name
    } else if (!is.null(uniprot_data$Name)) {
      protein_name <- uniprot_data$Name
    } else {
      protein_name <- protein_id
    }
    
    # Extract organism
    organism <- NULL
    if (!is.null(uniprot_data$organism)) {
      organism <- uniprot_data$organism
    } else if (!is.null(uniprot_data$Organism)) {
      organism <- uniprot_data$Organism
    }
    
    protein_info <- list(
      uniprot_id = protein_id,
      protein_name = protein_name,
      organism = organism,
      sequence_length = sequence_length,
      n_term_buffer = n_buffer,
      c_term_buffer = c_buffer
    )
    
    # STEP 1: Detect expression blockers
    cat("Detecting expression blockers...\n")
    expression_blockers <- detect_expression_blockers(features_df, sequence_length)
    cat("  Signal peptide:", expression_blockers$has_signal_peptide, "\n")
    cat("  Transmembrane:", expression_blockers$has_transmembrane, "\n")
    cat("  Too large:", expression_blockers$too_large, "\n")
    
    # STEP 1.5: Disorder analysis ### NEW ###
    cat("Analyzing terminus disorder...\n")
    
    protein_sequence <- uniprot_data$protein_sequence
    
    if (is.null(protein_sequence) || nchar(protein_sequence) == 0) {
      cat("⚠️  No sequence available for disorder analysis\n")
      disorder_analysis <- NULL
    } else {
      # Pass signal peptide end position (if detected)
      signal_end <- if(expression_blockers$has_signal_peptide) {
        expression_blockers$signal_peptide_end
      } else {
        NULL
      }
      
      disorder_analysis <- predict_terminus_disorder(
        sequence = protein_sequence,
        signal_peptide_end = signal_end,
        n_term_length = n_buffer,
        c_term_length = c_buffer
      )
      
      cat("  Disorder recommendation:", disorder_analysis$recommendation, "\n")
      cat("  N-term score:", disorder_analysis$n_terminus$disorder_score, "\n")
      cat("  C-term score:", disorder_analysis$c_terminus$disorder_score, "\n")
    }
    
    # STEP 2: Analyze terminus features
    cat("Analyzing terminus features...\n")
    terminus_analysis <- analyze_terminus_features_advanced(
      features_df, sequence_length, epitope_df, n_buffer, c_buffer
    )
    cat("  N-term score:", terminus_analysis$n_terminus$score, "\n")
    cat("  C-term score:", terminus_analysis$c_terminus$score, "\n")
    
    # STEP 3: Analyze epitopes
    cat("Analyzing epitopes...\n")
    epitope_analysis <- analyze_epitope_impact(
      epitope_df, sequence_length, n_buffer, c_buffer
    )
    cat("  Total epitopes:", epitope_analysis$total_epitopes, "\n")
    
    # STEP 4: Analyze expression history
    cat("Analyzing expression history...\n")
    expression_history <- analyze_expression_history(expression_df)
    cat("  Has prior success:", expression_history$successful_expression, "\n")
    
    # STEP 5: Build prompt
    cat("Building AI prompt...\n")
    prompt <- build_decision_prompt(
      protein_info, 
      terminus_analysis, 
      epitope_analysis,
      expression_history, 
      expression_blockers
    )
    
    # STEP 6: Call OpenAI
    cat("Calling OpenAI API...\n")
    result <- openai_request(prompt, model = model, max_tokens = 10000)
    
    if (result$success) {
      cat("AI response received, parsing...\n")
      decision <- parse_decision_response(result$content)
      
      # STEP 7: Create rule-based decision
      cat("Creating rule-based decision...\n")
      rule_based_decision <- list(
        recommended_construct = if(expression_blockers$has_transmembrane || 
                                   (expression_blockers$too_large && 
                                    terminus_analysis$n_terminus$score > 40 && 
                                    terminus_analysis$c_terminus$score > 40)) {
          "Truncated"
        } else {
          "Full-length"
        },
        
        construct_range = paste0(
          expression_blockers$recommended_start, "-", sequence_length
        ),
        
        preferred_tag = "6xHis",
        preferred_tag_position = terminus_analysis$preferred_tag_position,
        reasoning = terminus_analysis$preferred_reasoning,
        n_term_score = terminus_analysis$n_terminus$score,
        c_term_score = terminus_analysis$c_terminus$score
      )
      
      # STEP 8: Compare decisions
      ai_tag_pos <- tolower(decision$Preferred_Tag_Position %||% "")
      rule_tag_pos <- tolower(terminus_analysis$preferred_tag_position)
      
      decisions_match <- grepl(gsub("-terminal", "", rule_tag_pos), ai_tag_pos)
      
      rule_based_decision$decisions_match <- decisions_match
      
      comparison <- list(
        agreement = decisions_match,
        summary = if(decisions_match) {
          "✓ Rule-based and AI recommendations AGREE"
        } else {
          "⚠ Rule-based and AI recommendations DIFFER - review carefully"
        }
      )
      
      # STEP 9: Store results
      if (is.null(values$decision)) {
        values$decision <- list()
      }
      
      values$decision[[protein_id]] <- list(
        protein_info = protein_info,
        terminus_analysis = terminus_analysis,
        epitope_analysis = epitope_analysis,
        expression_history = expression_history,
        expression_blockers = expression_blockers,
        disorder_analysis = disorder_analysis,  # ← ADD THIS LINE
        ai_decision = decision,
        rule_based_decision = rule_based_decision,
        comparison = comparison,
        raw_response = result$content,
        timestamp = Sys.time()
      )
      
      # Save to disk
      if (!dir.exists('Data')) dir.create('Data')
      saveRDS(values$decision, 'Data/decision.rds')
      
      cat("✓ Decision stored successfully\n")
      
      return(list(success = TRUE, error = NULL))
      
    } else {
      cat("✗ OpenAI API error:", result$error, "\n")
      return(list(success = FALSE, error = result$error))
    }
    
  }, error = function(e) {
    cat("✗ ERROR in process_single_protein_decision:", conditionMessage(e), "\n")
    print(traceback())
    return(list(success = FALSE, error = as.character(e)))
  })
}

# Run AI decision analysis for SINGLE protein
observeEvent(input$run_decision, {
  
  selected_id <- input$decision_protein_select
  
  if (is.null(selected_id) || !nzchar(selected_id)) {
    showNotification("Please select a protein", type = "error")
    return()
  }
  
  if (is.null(values$uniprot_list[[selected_id]]) || 
      is.null(values$feature_list[[selected_id]])) {
    showNotification("Selected protein missing UniProt or Feature data. Please fetch protein data first.", 
                     type = "error", duration = 5)
    return()
  }
  
  current_decision_protein(selected_id)
  
  withProgress(message = paste("Analyzing", selected_id), {
    result <- process_single_protein_decision(selected_id, input$decision_model, 
                                              input$decision_n_term_buffer, 
                                              input$decision_c_term_buffer)
    
    if (result$success) {
      showNotification(paste("Strategy generated for", selected_id), type = "message")
    } else {
      showNotification(paste("Error:", result$error), type = "error", duration = 10)
    }
  })
})

# Run AI decision analysis for ALL selected proteins (BATCH)
observeEvent(input$run_decision_batch, {
  
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) {
    showNotification("No proteins selected", type = "error")
    return()
  }
  
  ready_count <- 0
  for (id in current_ids) {
    if (!is.null(values$uniprot_list[[id]]) && !is.null(values$feature_list[[id]])) {
      ready_count <- ready_count + 1
    }
  }
  
  if (ready_count == 0) {
    showNotification("No proteins have the required UniProt and Feature data", 
                     type = "error", duration = 5)
    return()
  }
  
  showNotification(paste("Starting batch analysis for", ready_count, "proteins..."), 
                   type = "message")
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Batch Processing...", value = 0)
  
  success_count <- 0
  error_count <- 0
  
  for (i in seq_along(current_ids)) {
    protein_id <- current_ids[i]
    
    if (is.null(values$uniprot_list[[protein_id]]) || 
        is.null(values$feature_list[[protein_id]])) {
      error_count <- error_count + 1
      next
    }
    
    progress$set(
      message = paste("Processing protein", i, "of", length(current_ids)),
      detail = protein_id,
      value = (i - 1) / length(current_ids)
    )
    
    result <- process_single_protein_decision(protein_id, input$decision_model,
                                              input$decision_n_term_buffer,
                                              input$decision_c_term_buffer)
    
    if (result$success) {
      success_count <- success_count + 1
    } else {
      error_count <- error_count + 1
    }
    
    if (i < length(current_ids)) {
      Sys.sleep(2)
    }
  }
  
  progress$set(value = 1, message = "Complete!")
  
  showNotification(
    paste0("Batch complete! Success: ", success_count, ", Errors: ", error_count),
    type = if(error_count == 0) "message" else "warning",
    duration = 10
  )
})



# Display AI recommendation
output$decision_recommendation_ui <- renderUI({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
    current_decision_protein(current_id)
  }
  
  decision_data <- values$decision[[current_id]]
  
  if (is.null(decision_data)) {
    return(
      wellPanel(
        h4("No recommendation yet"),
        p("Click 'Generate Strategy' to create a recommendation"),
        p("Make sure you have:"),
        tags$ul(
          tags$li("Fetched UniProt data for this protein"),
          tags$li("Run IEDB or OpenAI analysis (optional but recommended)")
        )
      )
    )
  }
  
  decision <- decision_data$ai_decision
  exp_hist <- decision_data$expression_history  # ← ADD THIS LINE HERE (around line 531)
  
  if (!is.null(decision$Error)) {
    return(
      wellPanel(
        style = "background-color: #f8d7da;",
        h4(icon("exclamation-triangle"), " Error"),
        p(decision$Error)
      )
    )
  }
  
  tagList(
    wellPanel(
      style = "background-color: #f8f9fa;",
      h3(icon("flask"), " AI Recommended Expression Strategy"),
      h5(tags$span(class = "badge badge-info", decision$Expression_System %||% "Sf9/Sf21 Insect Cells")),
      
      fluidRow(
        column(6,
               h4("Construct Design"),
               tags$ul(
                 tags$li(strong("Type: "), decision$Recommended_Construct %||% "Not specified"),
                 tags$li(strong("Range: "), decision$Construct_Range %||% "Not specified"),
                 tags$li(strong("Original Length: "), decision_data$protein_info$sequence_length, " aa"),
                 if(!is.null(decision$Signal_Peptide_Handling)) {
                   tags$li(strong("Signal Peptide: "), decision$Signal_Peptide_Handling)
                 } else { NULL }
               ),
               if(!is.null(decision$Justification_For_Truncation) && 
                  nchar(decision$Justification_For_Truncation) > 0) {
                 tags$div(
                   style = "background-color: #fff3cd; padding: 10px; margin-top: 10px; border-radius: 5px;",
                   strong(icon("exclamation-triangle"), " Truncation Justification:"),
                   tags$p(decision$Justification_For_Truncation)
                 )
               } else { NULL }
        ),
        column(6,
               h4("Tagging Strategy"),
               tags$ul(
                 tags$li(strong("Primary Tag: "), 
                         tags$span(class = "badge badge-success", decision$Preferred_Tag %||% "6xHis")),
                 tags$li(strong("Position: "), decision$Preferred_Tag_Position %||% "Not specified"),
                 tags$li(strong("Backup Position: "), decision$Alternative_Tag_Position %||% "Not specified")
               ),
               
               # NEW: Literature Support Badge
               tags$div(
                 style = paste0(
                   "background-color: ", 
                   if(exp_hist$successful_expression) "#d4edda" else "#fff3cd",
                   "; padding: 10px; margin-top: 10px; border-radius: 5px; border-left: 3px solid ",
                   if(exp_hist$successful_expression) "#28a745" else "#ffc107"
                 ),
                 tags$div(
                   style = "display: flex; align-items: center; justify-content: space-between;",
                   tags$small(
                     icon(if(exp_hist$successful_expression) "check-circle" else "search"),
                     strong(" Literature Support:")
                   ),
                   tags$span(
                     class = if(exp_hist$successful_expression) "badge badge-success" else "badge badge-warning",
                     if(exp_hist$successful_expression) "FOUND" else "NOT FOUND"
                   )
                 ),
                 if(exp_hist$successful_expression) {
                   tags$div(
                     style = "margin-top: 8px; font-size: 0.85em;",
                     tags$div(
                       icon("flask", style = "color: #28a745; margin-right: 5px;"),
                       exp_hist$best_system, " | ",
                       exp_hist$recommended_tag, " (",
                       exp_hist$tag_position, ")"
                     ),
                     tags$div(
                       style = "color: #6c757d; margin-top: 3px;",
                       icon("star", style = "color: #ffc107;"),
                       " Evidence: ", exp_hist$evidence_score, "/5"
                     )
                   )
                 } else {
                   tags$div(
                     style = "margin-top: 5px; font-size: 0.85em; color: #856404;",
                     icon("lightbulb", style = "margin-right: 3px;"),
                     "Decision based on structural analysis"
                   )
                 }
               ),
               
               tags$div(
                 style = "background-color: #d1ecf1; padding: 10px; margin-top: 10px; border-radius: 5px;",
                 tags$small(icon("info-circle"), " Available tags: 6xHis or cMyc only")
               )
        )
      ),
      
      hr(),
      
      h4(icon("brain"), " AI Reasoning"),
      tags$p(style = "text-align: justify;", decision$Reasoning %||% "No reasoning provided"),
      
      hr(),
      
      fluidRow(
        column(6,
               h5(icon("exclamation-triangle"), " Key Considerations"),
               if(!is.null(decision$Key_Considerations) && length(decision$Key_Considerations) > 0) {
                 tags$ul(
                   lapply(decision$Key_Considerations, function(x) tags$li(x))
                 )
               } else {
                 tags$p("None specified")
               }
        ),
        column(6,
               h5(icon("warning"), " Potential Challenges"),
               if(!is.null(decision$Potential_Challenges) && length(decision$Potential_Challenges) > 0) {
                 tags$ul(
                   lapply(decision$Potential_Challenges, function(x) tags$li(x))
                 )
               } else {
                 tags$p("None specified")
               }
        )
      ),
      
      hr(),
      
      tags$p(
        strong("Confidence Score: "),
        tags$span(
          class = if(!is.null(decision$Confidence_Score) && decision$Confidence_Score >= 4) {
            "badge badge-success"
          } else {
            "badge badge-warning"
          },
          paste(decision$Confidence_Score %||% "?", "/ 5")
        )
      )
    )
  )
})

# Display rule-based recommendation
output$decision_rule_based_ui <- renderUI({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  decision_data <- values$decision[[current_id]]
  
  if (is.null(decision_data)) {
    return(
      wellPanel(
        h4("No analysis yet"),
        p("Run 'Generate Strategy' first")
      )
    )
  }
  
  rule <- decision_data$rule_based_decision
  term <- decision_data$terminus_analysis
  
  wellPanel(
    style = "background-color: #f0f8ff;",
    h3(icon("calculator"), " Rule-Based Recommendation"),
    h5(tags$span(class = "badge badge-secondary", "Algorithm-Based Decision")),
    
    fluidRow(
      column(6,
             h4("Construct Design"),
             tags$ul(
               tags$li(strong("Type: "), rule$recommended_construct),
               tags$li(strong("Range: "), rule$construct_range, " aa"),
               tags$li(strong("N-term score: "), term$n_terminus$score, " (", 
                       term$n_terminus$recommendation, ")"),
               tags$li(strong("C-term score: "), term$c_terminus$score, " (", 
                       term$c_terminus$recommendation, ")")
             )
      ),
      column(6,
             h4("Tagging Strategy"),
             tags$ul(
               tags$li(strong("Tag: "), rule$preferred_tag),
               tags$li(strong("Position: "), rule$preferred_tag_position),
               tags$li(strong("Reasoning: "), rule$reasoning)
             )
      )
    ),
    
    hr(),
    
    h4("Scoring Logic"),
    tags$p("Lower scores = better for tagging"),
    tags$ul(
      tags$li("Score < 0: Excellent (flexible regions)"),
      tags$li("Score 0-20: OK for tagging"),
      tags$li("Score 20-40: Caution advised"),
      tags$li("Score 40-100: Avoid tagging"),
      tags$li("Score ≥100: Blocked (signal/TM domain)")
    ),
    
    hr(),
    
    h4("N-Terminus Issues"),
    if(length(term$n_terminus$issues) > 0) {
      tags$ul(
        lapply(term$n_terminus$issues, function(x) tags$li(x))
      )
    } else {
      tags$p("No issues detected")
    },
    
    h4("C-Terminus Issues"),
    if(length(term$c_terminus$issues) > 0) {
      tags$ul(
        lapply(term$c_terminus$issues, function(x) tags$li(x))
      )
    } else {
      tags$p("No issues detected")
    }
  )
})


# Add this new output after the rule_based_ui output (around line 850):

# Display disorder analysis
output$decision_disorder_ui <- renderUI({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  decision_data <- values$decision[[current_id]]
  
  if (is.null(decision_data)) {
    return(
      wellPanel(
        h4("No analysis yet"),
        p("Run 'Generate Strategy' first")
      )
    )
  }
  
  disorder <- decision_data$disorder_analysis
  
  if (is.null(disorder)) {
    return(
      wellPanel(
        style = "background-color: #fff3cd;",
        h4(icon("exclamation-triangle"), " Disorder Analysis Not Available"),
        p("Sequence data was not available for disorder prediction.")
      )
    )
  }
  
  wellPanel(
    style = "background-color: #f0f8ff;",
    h3(icon("wave-square"), " Terminus Disorder Analysis"),
    h5(tags$span(class = "badge badge-info", "Intrinsic Disorder Prediction")),
    
    tags$div(
      style = "background-color: #e7f3ff; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
      tags$h5(icon("info-circle"), " What is Disorder Analysis?"),
      tags$p(
        "Intrinsically disordered regions (IDRs) are protein segments that don't fold into stable 3D structures. ",
        "They remain flexible and dynamic. ", tags$strong("Flexible termini are ideal for tagging"), 
        " because tags won't disrupt protein folding."
      ),
      tags$ul(
        tags$li(tags$strong("High disorder (>0.7):"), " Flexible/floppy - ", 
                tags$span(style="color: #28a745;", "EXCELLENT for tagging")),
        tags$li(tags$strong("Moderate disorder (0.4-0.7):"), " Partially flexible - ", 
                tags$span(style="color: #7cb342;", "GOOD for tagging")),
        tags$li(tags$strong("Low disorder (<0.4):"), " Structured/rigid - ", 
                tags$span(style="color: #dc3545;", "AVOID tagging if possible"))
      )
    ),
    
    if(disorder$signal_removed) {
      tags$div(
        style = "background-color: #fff3cd; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
        icon("scissors"),
        strong(" Signal Peptide Detected:"),
        " Native signal peptide (aa 1-", disorder$signal_peptide_end, 
        ") removed from analysis. Disorder scores reflect the ",
        tags$strong("mature protein"), " that will be expressed after signal cleavage."
      )
    },
    
    hr(),
    
    fluidRow(
      column(6,
             wellPanel(
               style = paste0("background-color: white; border-left: 5px solid ", 
                              disorder_score_color(disorder$n_terminus$disorder_score)),
               h4(icon("arrow-right"), " N-Terminus"),
               if(disorder$signal_removed) {
                 tags$p(
                   tags$small(
                     style = "color: #856404;",
                     icon("info-circle"),
                     " Analysis starts at position ", disorder$mature_start_position, 
                     " (after signal peptide removal)"
                   )
                 )
               },
               tags$div(
                 style = "text-align: center; margin: 20px 0;",
                 tags$h1(
                   style = paste0("color: ", disorder_score_color(disorder$n_terminus$disorder_score), ";"),
                   round(disorder$n_terminus$disorder_score, 2)
                 ),
                 tags$p(
                   style = "font-size: 1.1em; font-weight: bold;",
                   disorder$n_terminus$interpretation
                 )
               ),
               tags$hr(),
               tags$p(
                 strong("Analyzed region:"), " First ", disorder$n_terminus$length, " residues"
               ),
               tags$p(
                 strong("Sequence:"),
                 tags$br(),
                 tags$code(
                   style = "background-color: #f8f9fa; padding: 5px; display: block; margin-top: 5px;",
                   disorder$n_terminus$sequence
                 )
               ),
               tags$div(
                 style = "margin-top: 15px;",
                 if(disorder$n_terminus$disorder_score > 0.7) {
                   tags$span(
                     class = "badge badge-success",
                     style = "font-size: 0.9em;",
                     icon("check"), " EXCELLENT for N-terminal tagging"
                   )
                 } else if(disorder$n_terminus$disorder_score > 0.5) {
                   tags$span(
                     class = "badge badge-success",
                     style = "font-size: 0.9em;",
                     icon("check"), " GOOD for N-terminal tagging"
                   )
                 } else if(disorder$n_terminus$disorder_score > 0.3) {
                   tags$span(
                     class = "badge badge-warning",
                     style = "font-size: 0.9em;",
                     icon("exclamation-triangle"), " CAUTION - partially structured"
                   )
                 } else {
                   tags$span(
                     class = "badge badge-danger",
                     style = "font-size: 0.9em;",
                     icon("times"), " AVOID - appears structured"
                   )
                 }
               )
             )
      ),
      
      column(6,
             wellPanel(
               style = paste0("background-color: white; border-left: 5px solid ", 
                              disorder_score_color(disorder$c_terminus$disorder_score)),
               h4(icon("arrow-left"), " C-Terminus"),
               tags$div(
                 style = "text-align: center; margin: 20px 0;",
                 tags$h1(
                   style = paste0("color: ", disorder_score_color(disorder$c_terminus$disorder_score), ";"),
                   round(disorder$c_terminus$disorder_score, 2)
                 ),
                 tags$p(
                   style = "font-size: 1.1em; font-weight: bold;",
                   disorder$c_terminus$interpretation
                 )
               ),
               tags$hr(),
               tags$p(
                 strong("Analyzed region:"), " Last ", disorder$c_terminus$length, " residues"
               ),
               tags$p(
                 strong("Sequence:"),
                 tags$br(),
                 tags$code(
                   style = "background-color: #f8f9fa; padding: 5px; display: block; margin-top: 5px;",
                   disorder$c_terminus$sequence
                 )
               ),
               tags$div(
                 style = "margin-top: 15px;",
                 if(disorder$c_terminus$disorder_score > 0.7) {
                   tags$span(
                     class = "badge badge-success",
                     style = "font-size: 0.9em;",
                     icon("check"), " EXCELLENT for C-terminal tagging"
                   )
                 } else if(disorder$c_terminus$disorder_score > 0.5) {
                   tags$span(
                     class = "badge badge-success",
                     style = "font-size: 0.9em;",
                     icon("check"), " GOOD for C-terminal tagging"
                   )
                 } else if(disorder$c_terminus$disorder_score > 0.3) {
                   tags$span(
                     class = "badge badge-warning",
                     style = "font-size: 0.9em;",
                     icon("exclamation-triangle"), " CAUTION - partially structured"
                   )
                 } else {
                   tags$span(
                     class = "badge badge-danger",
                     style = "font-size: 0.9em;",
                     icon("times"), " AVOID - appears structured"
                   )
                 }
               )
             )
      )
    ),
    
    hr(),
    
    wellPanel(
      style = paste0(
        "background-color: ", 
        if(disorder$confidence == "High") "#d4edda" 
        else if(disorder$confidence == "Medium") "#fff3cd" 
        else "#f8d7da",
        ";"
      ),
      h4(icon("thumbs-up"), " Disorder-Based Recommendation"),
      tags$p(
        tags$span(
          class = if(disorder$confidence == "High") "badge badge-success"
          else if(disorder$confidence == "Medium") "badge badge-warning"
          else "badge badge-danger",
          paste("Confidence:", disorder$confidence)
        )
      ),
      tags$h5(
        style = "margin-top: 15px;",
        strong("Recommended Position: "), disorder$recommendation
      ),
      tags$p(
        style = "margin-top: 10px; text-align: justify;",
        disorder$reasoning
      )
    ),
    
    hr(),
    
    tags$div(
      style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px;",
      tags$h5(icon("book"), " Interpretation Guide"),
      tags$p(
        strong("How disorder prediction works:"), 
        " The algorithm analyzes amino acid composition in terminal regions. ",
        "Charged residues (E, K, D, R), proline (P), and small residues (G, S, A) promote disorder. ",
        "Hydrophobic (W, F, Y, I, L, V) and structured residues (C, N) promote order."
      ),
      tags$p(
        strong("Why it matters:"), 
        " Tags placed on structured termini can prevent proper protein folding, ",
        "leading to expression in inclusion bodies or low yields. ",
        "Flexible termini tolerate tags without affecting the protein core."
      )
    )
  )
})


# ===========================================================================
# STRUCTURE VALIDATION TAB - Keep M for Predictions, Remove for Export
# ===========================================================================

# ===========================================================================
# LOAD REQUIRED FUNCTIONS FOR STRUCTURE PREDICTION
# ===========================================================================

# Source the construct builder
source("functions/tag_constructs_builder.R", local = TRUE)

# Source the ESM3 prediction function  
source("functions/structure_prediction.R", local = TRUE)

# ===========================================================================
# STRUCTURE PREDICTION UI OUTPUTS
# ===========================================================================

# Show sequence preview before prediction
output$structure_sequence_preview <- renderText({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return("No protein selected")
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  uniprot_data <- values$uniprot_list[[current_id]]
  if (is.null(uniprot_data)) return("No UniProt data available")
  
  protein_name <- if(!is.null(uniprot_data$protein_name)) {
    uniprot_data$protein_name
  } else {
    current_id
  }
  
  seq <- uniprot_data$protein_sequence
  if (is.null(seq) || seq == "Not available" || nchar(seq) == 0) {
    return("Sequence not available")
  }
  
  # Check for signal peptide
  features <- values$feature_list[[current_id]]
  signal_info <- NULL
  
  if (!is.null(features) && nrow(features) > 0) {
    signal_rows <- features %>% filter(type == "Signal")
    if (nrow(signal_rows) > 0) {
      signal_info <- signal_rows[1, ]
    }
  }
  
  # Build preview
  preview <- paste0(protein_name, " (", current_id, ")\n",
                    "Full sequence: ", nchar(seq), " aa\n",
                    "Starts with: ", substr(seq, 1, 10), "...\n\n")
  
  if (!is.null(signal_info)) {
    sp_end <- signal_info$end
    sp_seq <- substr(seq, 1, sp_end)
    mature_seq <- substr(seq, sp_end + 1, nchar(seq))
    
    preview <- paste0(preview,
                      "⚠️ Native signal peptide: 1-", sp_end, " (", sp_end, " aa)\n",
                      "   ", sp_seq, "\n",
                      "   Mature protein: ", nchar(mature_seq), " aa\n",
                      "   Starts with: ", substr(mature_seq, 1, 10), "...\n\n",
                      "📋 Will predict 3 structures:\n",
                      "   1. Untagged: mature protein\n",
                      "   2. pPRO30A-SP (N-term): Insect SP + tag + mature\n",
                      "   3. pPRO8 (C-term): Insect SP + mature + tag\n\n",
                      "✓ All structures include M for accurate folding prediction\n")
  } else {
    starts_with_m <- substr(seq, 1, 1) == "M"
    
    preview <- paste0(preview,
                      if(starts_with_m) "✓ Starts with M\n\n" else "⚠️ Does not start with M (will add for prediction)\n\n",
                      "📋 Will predict 3 structures:\n",
                      "   1. Untagged: full protein\n",
                      "   2. pPRO30A (N-term): M + tag + protein (M removed from protein)\n",
                      "   3. pPRO8 (C-term): protein + tag\n\n",
                      "✓ All structures include M for accurate folding prediction\n")
  }
  
  preview
})

# Check if structure results exist
output$structure_has_results <- reactive({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(FALSE)
  
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(FALSE)
  
  !is.null(values$structure_prediction[[current_id]])
})
outputOptions(output, "structure_has_results", suspendWhenHidden = FALSE)


# ===========================================================================
# RUN STRUCTURE PREDICTION (ESM3 API WITH RMSD)
# ===========================================================================

observeEvent(input$run_structure_prediction, {
  
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) {
    showNotification("No protein selected", type = "error")
    return()
  }
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  # Get UniProt data
  uniprot_data <- values$uniprot_list[[current_id]]
  if (is.null(uniprot_data)) {
    showNotification("No UniProt data for this protein", type = "error")
    return()
  }
  
  protein_seq <- uniprot_data$protein_sequence
  if (is.null(protein_seq) || protein_seq == "Not available" || nchar(protein_seq) == 0) {
    showNotification("Sequence not available for this protein", type = "error")
    return()
  }
  
  # Check for ESM Forge token
  forge_token <- input$esm_forge_token
  if (is.null(forge_token) || forge_token == "") {
    forge_token <- Sys.getenv("ESM_FORGE_TOKEN")
  }
  
  if (is.null(forge_token) || forge_token == "") {
    showNotification(
      "Please enter your ESM Forge API token",
      type = "error",
      duration = 10
    )
    return()
  }
  
  # Get signal peptide info from features
  signal_peptide_end <- NULL
  features <- values$feature_list[[current_id]]
  
  if (!is.null(features) && nrow(features) > 0) {
    signal_rows <- features %>% filter(type == "Signal")
    if (nrow(signal_rows) > 0) {
      signal_peptide_end <- signal_rows$end[1]
      cat("Found signal peptide: 1-", signal_peptide_end, "\n")
    }
  }
  
  # Build all 3 constructs
  constructs <- build_all_constructs(protein_seq, signal_peptide_end)
  
  # Get protein name
  protein_name <- if(!is.null(uniprot_data$protein_name)) {
    uniprot_data$protein_name
  } else {
    current_id
  }
  clean_name <- gsub("[^A-Za-z0-9_-]", "_", protein_name)
  
  # Run predictions with progress bar
  withProgress(message = 'Predicting Structures with ESM3', value = 0, {
    
    results <- list()
    
    # === PREDICT 1: UNTAGGED ===
    incProgress(0.1, detail = "Predicting untagged structure...")
    
    results$untagged <- predict_structure_esm3(
      sequence = constructs$untagged$sequence,
      construct_name = paste0(clean_name, "_untagged"),
      forge_token = forge_token
    )
    
    if (!results$untagged$success) {
      showNotification(
        paste0("Untagged prediction failed: ", results$untagged$error),
        type = "error",
        duration = 10
      )
      return()
    }
    
    cat("\n✓ Untagged structure predicted\n")
    Sys.sleep(1)
    
    # === PREDICT 2: N-TAGGED ===
    incProgress(0.4, detail = paste0("Predicting ", constructs$n_tagged$name, "..."))
    
    results$n_tagged <- predict_structure_esm3(
      sequence = constructs$n_tagged$sequence,
      construct_name = paste0(clean_name, "_", constructs$n_tagged$vector_name),
      forge_token = forge_token
    )
    
    if (!results$n_tagged$success) {
      showNotification(
        paste0("N-tagged prediction failed: ", results$n_tagged$error),
        type = "warning",
        duration = 10
      )
    } else {
      cat("\n✓ N-tagged structure predicted\n")
    }
    
    Sys.sleep(1)
    
    # === PREDICT 3: C-TAGGED ===
    incProgress(0.7, detail = "Predicting C-tagged structure...")
    
    results$c_tagged <- predict_structure_esm3(
      sequence = constructs$c_tagged$sequence,
      construct_name = paste0(clean_name, "_pPRO8"),
      forge_token = forge_token
    )
    
    if (!results$c_tagged$success) {
      showNotification(
        paste0("C-tagged prediction failed: ", results$c_tagged$error),
        type = "warning",
        duration = 10
      )
    } else {
      cat("\n✓ C-tagged structure predicted\n")
    }
    
    incProgress(0.85, detail = "Calculating RMSD and TM-score...")
    
    # === CALCULATE RMSD AND TM-SCORE ===
    cat("\n=== CALCULATING STRUCTURAL SIMILARITY ===\n")
    
    # Initialize variables
    n_rmsd <- NA
    c_rmsd <- NA
    n_tmscore <- NA
    c_tmscore <- NA
    rmsd_diff <- NA
    
    # Check if all predictions succeeded
    if (results$untagged$success && results$n_tagged$success && results$c_tagged$success) {
      
      tryCatch({
        
        cat("Reading PDB files...\n")
        ref_pdb <- bio3d::read.pdb(results$untagged$pdb_file)
        n_pdb <- bio3d::read.pdb(results$n_tagged$pdb_file)
        c_pdb <- bio3d::read.pdb(results$c_tagged$pdb_file)
        
        cat("Structures loaded:\n")
        cat("  Reference: ", ref_pdb$calpha, " CA atoms\n")
        cat("  N-tagged: ", n_pdb$calpha, " CA atoms\n")
        cat("  C-tagged: ", c_pdb$calpha, " CA atoms\n")
        
        # ===== N-TAGGED COMPARISON =====
        cat("\n--- Aligning N-tagged construct ---\n")
        
        n_result <- tryCatch({
          
          prot_region <- constructs$n_tagged$regions$protein
          cat("Protein region: residues ", prot_region$start, "-", prot_region$end, "\n")
          
          # Select CA atoms
          ref_ca_sel <- bio3d::atom.select(ref_pdb, "calpha")
          n_prot_resno <- prot_region$start:prot_region$end
          n_ca_prot_sel <- bio3d::atom.select(n_pdb, "calpha", resno = n_prot_resno)
          
          n_atoms_ref <- length(ref_ca_sel$atom)
          n_atoms_n <- length(n_ca_prot_sel$atom)
          min_atoms <- min(n_atoms_ref, n_atoms_n)
          
          cat("Comparing ", min_atoms, " CA atoms\n")
          
          if (min_atoms < 10) {
            return(list(rmsd = NA, tmscore = NA, error = "Too few atoms"))
          }
          
          # Get coordinates
          ref_xyz_inds <- ref_ca_sel$xyz[1:(min_atoms * 3)]
          ref_xyz <- ref_pdb$xyz[ref_xyz_inds]
          
          n_xyz_inds <- n_ca_prot_sel$xyz[1:(min_atoms * 3)]
          n_xyz <- n_pdb$xyz[n_xyz_inds]
          
          # Fit and calculate RMSD
          all_inds <- 1:(min_atoms * 3)
          fit_result <- bio3d::fit.xyz(
            fixed = ref_xyz,
            mobile = n_xyz,
            fixed.inds = all_inds,
            mobile.inds = all_inds
          )
          
          # Extract RMSD - fit_result is a list
          rmsd_val <- fit_result$rmsd
          
          # Get aligned coordinates - this is the KEY FIX
          # fit_result$xyz contains the ALIGNED mobile coordinates
          aligned_n_xyz <- fit_result$xyz
          
          # Calculate TM-score
          L_target <- min_atoms
          d0 <- 1.24 * (L_target - 15)^(1/3) - 1.8
          
          # Reshape coordinates to matrix (3 columns: x, y, z)
          # Each row is one atom
          ref_coords <- matrix(ref_xyz, ncol = 3, byrow = TRUE)
          n_coords <- matrix(aligned_n_xyz, ncol = 3, byrow = TRUE)
          
          # Calculate per-atom distances
          distances <- sqrt(rowSums((ref_coords - n_coords)^2))
          
          # TM-score calculation
          tm_score <- sum(1 / (1 + (distances / d0)^2)) / L_target
          
          cat("✓ N-tagged RMSD: ", round(rmsd_val, 2), " Å\n")
          cat("✓ N-tagged TM-score: ", round(tm_score, 3), "\n")
          
          list(rmsd = rmsd_val, tmscore = tm_score, error = NULL)
          
        }, error = function(e) {
          cat("❌ N-tag comparison failed: ", conditionMessage(e), "\n")
          list(rmsd = NA, tmscore = NA, error = conditionMessage(e))
        })
        
        n_rmsd <- n_result$rmsd
        n_tmscore <- n_result$tmscore
        
        # ===== C-TAGGED COMPARISON =====
        cat("\n--- Aligning C-tagged construct ---\n")
        
        c_result <- tryCatch({
          
          prot_region <- constructs$c_tagged$regions$protein
          cat("Protein region: residues ", prot_region$start, "-", prot_region$end, "\n")
          
          ref_ca_sel <- bio3d::atom.select(ref_pdb, "calpha")
          c_prot_resno <- prot_region$start:prot_region$end
          c_ca_prot_sel <- bio3d::atom.select(c_pdb, "calpha", resno = c_prot_resno)
          
          n_atoms_ref <- length(ref_ca_sel$atom)
          n_atoms_c <- length(c_ca_prot_sel$atom)
          min_atoms <- min(n_atoms_ref, n_atoms_c)
          
          cat("Comparing ", min_atoms, " CA atoms\n")
          
          if (min_atoms < 10) {
            return(list(rmsd = NA, tmscore = NA, error = "Too few atoms"))
          }
          
          # Get coordinates
          ref_xyz_inds <- ref_ca_sel$xyz[1:(min_atoms * 3)]
          ref_xyz <- ref_pdb$xyz[ref_xyz_inds]
          
          c_xyz_inds <- c_ca_prot_sel$xyz[1:(min_atoms * 3)]
          c_xyz <- c_pdb$xyz[c_xyz_inds]
          
          # Fit and calculate RMSD
          all_inds <- 1:(min_atoms * 3)
          fit_result <- bio3d::fit.xyz(
            fixed = ref_xyz,
            mobile = c_xyz,
            fixed.inds = all_inds,
            mobile.inds = all_inds
          )
          
          rmsd_val <- fit_result$rmsd
          aligned_c_xyz <- fit_result$xyz
          
          # Calculate TM-score
          L_target <- min_atoms
          d0 <- 1.24 * (L_target - 15)^(1/3) - 1.8
          
          ref_coords <- matrix(ref_xyz, ncol = 3, byrow = TRUE)
          c_coords <- matrix(aligned_c_xyz, ncol = 3, byrow = TRUE)
          
          distances <- sqrt(rowSums((ref_coords - c_coords)^2))
          tm_score <- sum(1 / (1 + (distances / d0)^2)) / L_target
          
          cat("✓ C-tagged RMSD: ", round(rmsd_val, 2), " Å\n")
          cat("✓ C-tagged TM-score: ", round(tm_score, 3), "\n")
          
          list(rmsd = rmsd_val, tmscore = tm_score, error = NULL)
          
        }, error = function(e) {
          cat("❌ C-tag comparison failed: ", conditionMessage(e), "\n")
          list(rmsd = NA, tmscore = NA, error = conditionMessage(e))
        })
        
        c_rmsd <- c_result$rmsd
        c_tmscore <- c_result$tmscore
        
        # ===== SUMMARY =====
        if (!is.na(n_rmsd) && !is.na(c_rmsd)) {
          rmsd_diff <- abs(n_rmsd - c_rmsd)
          
          cat("\n")
          cat(rep("=", 60), "\n", sep = "")
          cat("STRUCTURAL SIMILARITY SUMMARY\n")
          cat(rep("=", 60), "\n", sep = "")
          cat(sprintf("%-20s %10s %12s\n", "Construct", "RMSD (Å)", "TM-score"))
          cat(rep("-", 60), "\n", sep = "")
          cat(sprintf("%-20s %10.2f %12.3f\n", "N-tagged", n_rmsd, 
                      if(!is.na(n_tmscore)) n_tmscore else 0))
          cat(sprintf("%-20s %10.2f %12.3f\n", "C-tagged", c_rmsd, 
                      if(!is.na(c_tmscore)) c_tmscore else 0))
          cat(rep("-", 60), "\n", sep = "")
          cat("RMSD Difference: ", sprintf("%.2f", rmsd_diff), " Å\n")
          if (!is.na(n_tmscore) && !is.na(c_tmscore)) {
            cat("TM-score Difference: ", sprintf("%.3f", abs(n_tmscore - c_tmscore)), "\n")
          }
          cat(rep("=", 60), "\n", sep = "")
          
          # Interpretation
          cat("\nInterpretation:\n")
          
          # For large proteins (>500 aa), RMSD threshold is more lenient
          protein_size <- constructs$untagged$total_length
          rmsd_threshold_good <- if(protein_size > 500) 5.0 else 3.0
          rmsd_threshold_ok <- if(protein_size > 500) 8.0 else 5.0
          
          if (n_rmsd < rmsd_threshold_good && c_rmsd < rmsd_threshold_good) {
            cat("  RMSD: Both tags preserve structure well (<", rmsd_threshold_good, " Å for ", 
                protein_size, " aa protein)\n")
          } else if (n_rmsd < rmsd_threshold_ok && c_rmsd < rmsd_threshold_ok) {
            cat("  RMSD: Moderate structural changes (<", rmsd_threshold_ok, " Å)\n")
          } else {
            cat("  RMSD: Significant structural changes (>", rmsd_threshold_ok, " Å)\n")
            cat("  Note: For large proteins (", protein_size, " aa), higher RMSD is expected\n")
          }
          
          # TM-score interpretation
          if (!is.na(n_tmscore) && !is.na(c_tmscore)) {
            if (n_tmscore > 0.9 && c_tmscore > 0.9) {
              cat("  TM-score: Both tags preserve fold excellently (>0.9)\n")
            } else if (n_tmscore > 0.7 && c_tmscore > 0.7) {
              cat("  TM-score: Both tags maintain similar fold (>0.7)\n")
            } else if (n_tmscore > 0.5 && c_tmscore > 0.5) {
              cat("  TM-score: Moderate fold similarity (>0.5)\n")
            } else {
              cat("  TM-score: Significant fold changes detected (<0.5)\n")
            }
            
            # Winner based on TM-score (higher is better)
            tm_diff <- abs(n_tmscore - c_tmscore)
            if (tm_diff > 0.05) {
              if (c_tmscore > n_tmscore) {
                cat("\n✓ C-terminal tag shows better structural preservation\n")
                cat("  (TM-score: ", round(c_tmscore, 3), " vs ", round(n_tmscore, 3), ")\n")
              } else {
                cat("\n✓ N-terminal tag shows better structural preservation\n")
                cat("  (TM-score: ", round(n_tmscore, 3), " vs ", round(c_tmscore, 3), ")\n")
              }
            } else {
              cat("\n✓ Both tags show similar structural impact (TM-score difference < 0.05)\n")
            }
          }
          
          cat("\n")
          
        } else {
          cat("\n⚠️ RMSD/TM-score calculation incomplete\n")
          if (!is.null(n_result$error)) {
            cat("N-tagged error: ", n_result$error, "\n")
          }
          if (!is.null(c_result$error)) {
            cat("C-tagged error: ", c_result$error, "\n")
          }
        }
        
      }, error = function(e) {
        cat("\n❌ Structural comparison error: ", conditionMessage(e), "\n")
        print(e)
      })
      
    } else {
      cat("⚠️ Skipping structural comparison - not all predictions succeeded\n")
    }
    
    incProgress(0.95, detail = "Finalizing recommendation...")
    
    # Store in reactive values
    if (is.null(values$structure_prediction)) {
      values$structure_prediction <- list()
    }
    
    # Make recommendation WITH RMSD
    recommendation <- make_tagging_recommendation_esm3(results, n_rmsd, c_rmsd)
    
    values$structure_prediction[[current_id]] <- list(
      success = TRUE,
      protein_name = protein_name,
      uniprot_id = current_id,
      constructs = constructs,
      results = results,
      rmsd = list(
        n_terminal = n_rmsd,
        c_terminal = c_rmsd,
        difference = rmsd_diff
      ),
      recommendation = recommendation$recommendation,
      reasoning = recommendation$reasoning,
      confidence = recommendation$confidence,
      timestamp = Sys.time()
    )
    
    # Save to disk
    saveRDS(values$structure_prediction, 'Data/structure_prediction.rds')
    
    incProgress(1, detail = "Complete!")
    
    cat("\n")
    cat(rep("=", 70), "\n", sep = "")
    cat("RECOMMENDATION\n")
    cat(rep("=", 70), "\n", sep = "")
    cat("Decision:", recommendation$recommendation, "\n")
    cat("Confidence:", recommendation$confidence, "\n")
    if (!is.na(n_rmsd) && !is.na(c_rmsd)) {
      cat("RMSD: N=", round(n_rmsd, 2), "Å, C=", round(c_rmsd, 2), "Å (diff=", round(rmsd_diff, 2), "Å)\n")
    }
    cat(rep("=", 70), "\n", sep = "")
    
    showNotification(
      paste0("✓ All structures predicted! ", 
             recommendation$recommendation,
             if(!is.na(n_rmsd) && !is.na(c_rmsd)) 
               paste0(" (RMSD: N=", round(n_rmsd, 2), "Å, C=", round(c_rmsd, 2), "Å)")
             else ""),
      type = "message",
      duration = 10
    )
  })
})


# ===========================================================================
# HELPER: Make tagging recommendation from ESM3 results WITH RMSD
# ===========================================================================

make_tagging_recommendation_esm3 <- function(results, rmsd_n = NA, rmsd_c = NA) {
  
  # Extract confidence scores
  untagged_conf <- if(results$untagged$success) results$untagged$mean_plddt else NA
  n_tagged_conf <- if(results$n_tagged$success) results$n_tagged$mean_plddt else NA
  c_tagged_conf <- if(results$c_tagged$success) results$c_tagged$mean_plddt else NA
  
  # Get terminal confidence from untagged
  untagged_n_term <- if(results$untagged$success) results$untagged$n_term_plddt else NA
  untagged_c_term <- if(results$untagged$success) results$untagged$c_term_plddt else NA
  
  cat("\n--- Analysis Scores ---\n")
  cat("Confidence (pLDDT):\n")
  cat("  Untagged overall:", round(untagged_conf, 1), "\n")
  cat("  Untagged N-term:", round(untagged_n_term, 1), "\n")
  cat("  Untagged C-term:", round(untagged_c_term, 1), "\n")
  cat("  N-tagged overall:", round(n_tagged_conf, 1), "\n")
  cat("  C-tagged overall:", round(c_tagged_conf, 1), "\n")
  
  if (!is.na(rmsd_n) && !is.na(rmsd_c)) {
    cat("\nStructural Deviation (RMSD):\n")
    cat("  N-tagged:", round(rmsd_n, 2), "Å\n")
    cat("  C-tagged:", round(rmsd_c, 2), "Å\n")
    cat("  Difference:", round(abs(rmsd_n - rmsd_c), 2), "Å\n")
  }
  
  # PRIORITY 1: Use RMSD if available (most reliable for structure comparison)
  if (!is.na(rmsd_n) && !is.na(rmsd_c)) {
    
    rmsd_diff <- abs(rmsd_n - rmsd_c)
    
    if (rmsd_c < rmsd_n - 1.0) {
      # C-terminal significantly better
      recommendation <- "C-terminal (pPRO8)"
      reasoning <- paste0(
        "C-terminal construct shows significantly less structural deviation (", 
        round(rmsd_c, 2), " Å vs ", round(rmsd_n, 2), " Å RMSD from native). ",
        "Protein core maintains native-like fold better with C-terminal tag."
      )
      confidence <- "High"
      
    } else if (rmsd_n < rmsd_c - 1.0) {
      # N-terminal significantly better
      recommendation <- "N-terminal (pPRO30A)"
      reasoning <- paste0(
        "N-terminal construct shows significantly less structural deviation (", 
        round(rmsd_n, 2), " Å vs ", round(rmsd_c, 2), " Å RMSD from native). ",
        "Protein core maintains native-like fold better with N-terminal tag."
      )
      confidence <- "High"
      
    } else if (rmsd_c < rmsd_n) {
      # C-terminal moderately better
      recommendation <- "C-terminal (pPRO8)"
      reasoning <- paste0(
        "C-terminal construct shows moderately less structural deviation (", 
        round(rmsd_c, 2), " Å vs ", round(rmsd_n, 2), " Å RMSD). ",
        "Both positions acceptable, but C-terminal slightly better."
      )
      confidence <- "Medium"
      
    } else if (rmsd_n < rmsd_c) {
      # N-terminal moderately better
      recommendation <- "N-terminal (pPRO30A)"
      reasoning <- paste0(
        "N-terminal construct shows moderately less structural deviation (", 
        round(rmsd_n, 2), " Å vs ", round(rmsd_c, 2), " Å RMSD). ",
        "Both positions acceptable, but N-terminal slightly better."
      )
      confidence <- "Medium"
      
    } else {
      # Equal RMSD
      recommendation <- "Either position (equivalent RMSD)"
      reasoning <- paste0(
        "Both constructs show similar structural impact (", 
        round(rmsd_n, 2), " Å vs ", round(rmsd_c, 2), " Å RMSD). ",
        "Choose based on other factors (disorder, epitopes, expression history)."
      )
      confidence <- "Medium"
    }
    
  } else {
    # PRIORITY 2: Fall back to pLDDT terminal analysis
    cat("\n⚠️ RMSD not available, using pLDDT terminal analysis\n")
    
    if (!is.na(untagged_n_term) && !is.na(untagged_c_term)) {
      
      if (untagged_c_term > untagged_n_term + 10) {
        recommendation <- "C-terminal (pPRO8)"
        reasoning <- paste0(
          "C-terminus is more flexible (pLDDT ", round(untagged_c_term, 1), 
          " vs N-term ", round(untagged_n_term, 1), "). ",
          "Tag less likely to disrupt folding. Note: RMSD comparison not available."
        )
        confidence <- "Medium"
        
      } else if (untagged_n_term > untagged_c_term + 10) {
        recommendation <- "N-terminal (pPRO30A)"
        reasoning <- paste0(
          "N-terminus is more flexible (pLDDT ", round(untagged_n_term, 1), 
          " vs C-term ", round(untagged_c_term, 1), "). ",
          "Tag less likely to disrupt folding. Note: RMSD comparison not available."
        )
        confidence <- "Medium"
        
      } else {
        recommendation <- "Either terminus suitable"
        reasoning <- paste0(
          "Both termini have similar flexibility (N=", round(untagged_n_term, 1),
          ", C=", round(untagged_c_term, 1), "). ",
          "RMSD comparison not available. Consider other factors."
        )
        confidence <- "Low"
      }
      
      # Check if tagged structures show issues
      if (!is.na(n_tagged_conf) && !is.na(c_tagged_conf)) {
        if (c_tagged_conf > n_tagged_conf + 5) {
          recommendation <- "C-terminal (pPRO8)"
          reasoning <- paste0(reasoning, "\n\nTagged structure confidence: C-term=",
                              round(c_tagged_conf, 1), " > N-term=", 
                              round(n_tagged_conf, 1))
        } else if (n_tagged_conf > c_tagged_conf + 5) {
          recommendation <- "N-terminal (pPRO30A)"
          reasoning <- paste0(reasoning, "\n\nTagged structure confidence: N-term=",
                              round(n_tagged_conf, 1), " > C-term=", 
                              round(c_tagged_conf, 1))
        }
      }
      
    } else {
      recommendation <- "Insufficient data"
      reasoning <- "Could not perform RMSD comparison or terminal analysis"
      confidence <- "Low"
    }
  }
  
  return(list(
    recommendation = recommendation,
    reasoning = reasoning,
    summary = recommendation,
    confidence = confidence,
    scores = list(
      untagged = untagged_conf,
      untagged_n_term = untagged_n_term,
      untagged_c_term = untagged_c_term,
      n_tagged = n_tagged_conf,
      c_tagged = c_tagged_conf
    ),
    rmsd = list(
      n_terminal = rmsd_n,
      c_terminal = rmsd_c,
      difference = if(!is.na(rmsd_n) && !is.na(rmsd_c)) abs(rmsd_n - rmsd_c) else NA
    )
  ))
}




# ===========================================================================
# DISPLAY STRUCTURE PREDICTION RESULTS
# ===========================================================================

output$structure_results_ui <- renderUI({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  prediction <- values$structure_prediction[[current_id]]
  
  if (is.null(prediction)) {
    return(
      wellPanel(
        style = "text-align: center; padding: 50px;",
        icon("cube", style = "font-size: 4em; color: #ccc;"),
        h4("No Structure Prediction Yet"),
        p("Click 'Predict All 3 Structures' to begin"),
        tags$hr(),
        tags$div(
          style = "text-align: left; display: inline-block; max-width: 600px;",
          tags$h5("What This Does:"),
          tags$ul(
            tags$li("Builds 3 constructs based on signal peptide"),
            tags$li("Predicts 3D structures using ESM3 AI"),
            tags$li("Calculates RMSD (structural deviation)"),
            tags$li("Analyzes confidence scores (pLDDT)"),
            tags$li("Recommends best tag position"),
            tags$li("Takes 1-3 minutes total")
          )
        )
      )
    )
  }
  
  if (!prediction$success) {
    return(
      wellPanel(
        style = "background-color: #f8d7da;",
        h4(icon("exclamation-triangle"), " Prediction Failed"),
        p("Please try again or check your API token")
      )
    )
  }
  
  results <- prediction$results
  rmsd <- prediction$rmsd
  
  # Determine panel color based on confidence
  panel_color <- if(prediction$confidence == "High") "#d4edda"
  else if(prediction$confidence == "Medium") "#fff3cd"
  else "#f8d7da"
  
  tagList(
    # Recommendation panel
    wellPanel(
      style = paste0("background-color: ", panel_color, "; border: 2px solid #28a745;"),
      h3(icon("check-circle"), " Structure-Based Recommendation"),
      h4(prediction$recommendation),
      tags$p(
        tags$span(
          class = paste0("badge badge-",
                         if(prediction$confidence == "High") "success"
                         else if(prediction$confidence == "Medium") "warning"
                         else "danger"),
          paste(prediction$confidence, "Confidence")
        )
      ),
      p(prediction$reasoning),
      tags$hr(),
      
      fluidRow(
        column(6,
               h5(icon("chart-line"), " Confidence Scores (pLDDT):"),
               tags$ul(
                 tags$li("Untagged: ", round(results$untagged$mean_plddt, 1),
                         " (", results$untagged$confidence_level, ")"),
                 tags$li("  N-term region: ", round(results$untagged$n_term_plddt, 1)),
                 tags$li("  C-term region: ", round(results$untagged$c_term_plddt, 1)),
                 tags$li("N-tagged: ", round(results$n_tagged$mean_plddt, 1),
                         " (", results$n_tagged$confidence_level, ")"),
                 tags$li("C-tagged: ", round(results$c_tagged$mean_plddt, 1),
                         " (", results$c_tagged$confidence_level, ")")
               ),
               tags$small(
                 style = "color: #666;",
                 "pLDDT >90=Very High, 70-90=High, 50-70=Moderate, <50=Low"
               )
        ),
        column(6,
               h5(icon("ruler"), " Structural Deviation (RMSD):"),
               if (!is.na(rmsd$n_terminal) && !is.na(rmsd$c_terminal)) {
                 tagList(
                   tags$ul(
                     tags$li("N-tagged: ", 
                             tags$span(
                               style = paste0("font-weight: bold; color: ",
                                              if(rmsd$n_terminal < 1.5) "#28a745"
                                              else if(rmsd$n_terminal < 3.0) "#ffc107"
                                              else "#dc3545"),
                               round(rmsd$n_terminal, 2), " Å"
                             )),
                     tags$li("C-tagged: ", 
                             tags$span(
                               style = paste0("font-weight: bold; color: ",
                                              if(rmsd$c_terminal < 1.5) "#28a745"
                                              else if(rmsd$c_terminal < 3.0) "#ffc107"
                                              else "#dc3545"),
                               round(rmsd$c_terminal, 2), " Å"
                             )),
                     tags$li(tags$strong("Difference: ", round(rmsd$difference, 2), " Å"))
                   ),
                   tags$small(
                     style = "color: #666;",
                     "<1.5Å=Excellent, 1.5-3.0Å=Acceptable, >3.0Å=Significant change"
                   )
                 )
               } else {
                 tags$p(
                   style = "color: #856404;",
                   icon("exclamation-triangle"),
                   " RMSD calculation not available"
                 )
               }
        )
      )
    ),
    
    # Construct details
    wellPanel(
      h4(icon("dna"), " Predicted Structures"),
      fluidRow(
        column(4,
               wellPanel(
                 style = "background-color: #e9ecef;",
                 h5("Untagged (Reference)"),
                 tags$ul(
                   tags$li("Length: ", prediction$constructs$untagged$total_length, " aa"),
                   tags$li("pLDDT: ", round(results$untagged$mean_plddt, 1)),
                   tags$li("Type: ", prediction$constructs$untagged$description)
                 ),
                 tags$small(
                   style = "color: #666;",
                   "File: ", basename(results$untagged$pdb_file)
                 )
               )
        ),
        column(4,
               wellPanel(
                 style = paste0("background-color: ",
                                if(prediction$recommendation == "N-terminal (pPRO30A)") "#d4edda" else "#fff"),
                 h5(prediction$constructs$n_tagged$name),
                 tags$ul(
                   tags$li("Length: ", prediction$constructs$n_tagged$total_length, " aa"),
                   tags$li("pLDDT: ", round(results$n_tagged$mean_plddt, 1)),
                   if(!is.na(rmsd$n_terminal)) tags$li("RMSD: ", round(rmsd$n_terminal, 2), " Å")
                 ),
                 tags$small(
                   style = "color: #666;",
                   "File: ", basename(results$n_tagged$pdb_file)
                 ),
                 if(prediction$recommendation == "N-terminal (pPRO30A)") {
                   tags$div(
                     style = "margin-top: 10px;",
                     tags$span(class = "badge badge-success", "✓ RECOMMENDED")
                   )
                 }
               )
        ),
        column(4,
               wellPanel(
                 style = paste0("background-color: ",
                                if(prediction$recommendation == "C-terminal (pPRO8)") "#d4edda" else "#fff"),
                 h5("pPRO8"),
                 tags$ul(
                   tags$li("Length: ", prediction$constructs$c_tagged$total_length, " aa"),
                   tags$li("pLDDT: ", round(results$c_tagged$mean_plddt, 1)),
                   if(!is.na(rmsd$c_terminal)) tags$li("RMSD: ", round(rmsd$c_terminal, 2), " Å")
                 ),
                 tags$small(
                   style = "color: #666;",
                   "File: ", basename(results$c_tagged$pdb_file)
                 ),
                 if(prediction$recommendation == "C-terminal (pPRO8)") {
                   tags$div(
                     style = "margin-top: 10px;",
                     tags$span(class = "badge badge-success", "✓ RECOMMENDED")
                   )
                 }
               )
        )
      ),
      tags$hr(),
      tags$div(
        style = "text-align: center;",
        tags$small(
          style = "color: #666;",
          "Predicted: ", format(prediction$timestamp, "%Y-%m-%d %H:%M:%S"),
          " | Method: ESM3 Forge API | Analyst: MichMullins"
        )
      )
    )
  )
})


cat("\n✓ ESM3 Structure Prediction module loaded successfully\n")
cat("  - Automatic 3-construct comparison\n")
cat("  - ESM3 structure prediction via Forge API\n")
cat("  - RMSD-based structural deviation analysis\n")
cat("  - pLDDT confidence scoring\n")
cat("  - Intelligent tag position recommendation\n\n")

# REPLACE the existing decision_comparison_ui output (around line 900) with this expanded version:

output$decision_comparison_ui <- renderUI({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  decision_data <- values$decision[[current_id]]
  
  if (is.null(decision_data)) {
    return(
      wellPanel(
        h4("No analysis yet"),
        p("Run 'Generate Strategy' first")
      )
    )
  }
  
  ai <- decision_data$ai_decision
  rule <- decision_data$rule_based_decision
  disorder <- decision_data$disorder_analysis
  comp <- decision_data$comparison
  
  # Extract tag positions from each method
  ai_tag_pos <- tolower(ai$Preferred_Tag_Position %||% "")
  rule_tag_pos <- tolower(rule$preferred_tag_position)
  disorder_tag_pos <- if(!is.null(disorder)) tolower(disorder$recommendation) else "unknown"
  
  # Determine consensus
  positions <- c(ai_tag_pos, rule_tag_pos, disorder_tag_pos)
  n_count <- sum(grepl("n-term", positions))
  c_count <- sum(grepl("c-term", positions))
  
  consensus <- if(c_count >= 2) {
    "C-terminal"
  } else if(n_count >= 2) {
    "N-terminal"
  } else {
    "No consensus"
  }
  
  consensus_color <- if(c_count == 3 || n_count == 3) {
    "#d4edda"  # All agree - green
  } else if(c_count >= 2 || n_count >= 2) {
    "#fff3cd"  # Majority - yellow
  } else {
    "#f8d7da"  # No consensus - red
  }
  
  tagList(
    wellPanel(
      style = paste0("background-color: ", consensus_color, "; border: 2px solid ",
                     if(consensus != "No consensus") "#28a745" else "#dc3545", ";"),
      h3(icon("balance-scale"), " Three-Way Decision Comparison"),
      
      tags$div(
        style = "text-align: center; margin: 20px 0;",
        tags$h4("CONSENSUS RECOMMENDATION:"),
        tags$h2(
          style = paste0("color: ", if(consensus != "No consensus") "#28a745" else "#dc3545", ";"),
          consensus,
          if(consensus != "No consensus") {
            tags$span(
              style = "font-size: 0.6em; margin-left: 10px;",
              if(c_count == 3 || n_count == 3) "✓✓✓ All 3 methods agree"
              else paste0("✓✓ ", max(c_count, n_count), " of 3 methods agree")
            )
          } else {
            tags$span(
              style = "font-size: 0.6em; margin-left: 10px; color: #dc3545;",
              "⚠ Methods disagree - manual review needed"
            )
          }
        )
      ),
      
      hr(),
      
      fluidRow(
        # AI Recommendation
        column(4,
               wellPanel(
                 style = "background-color: white; min-height: 400px;",
                 h4(icon("robot"), " AI Recommendation"),
                 tags$div(
                   style = "text-align: center; margin: 20px 0;",
                   tags$h3(
                     style = paste0("color: ", 
                                    if(grepl("c-term", ai_tag_pos)) "#007bff" else "#6f42c1"),
                     if(grepl("n-term", ai_tag_pos)) "N-Terminal"
                     else if(grepl("c-term", ai_tag_pos)) "C-Terminal"
                     else "Not specified"
                   )
                 ),
                 tags$hr(),
                 tags$ul(
                   tags$li(strong("Tag: "), ai$Preferred_Tag %||% "Not specified"),
                   tags$li(strong("Position: "), ai$Preferred_Tag_Position %||% "Not specified"),
                   tags$li(strong("Construct: "), ai$Recommended_Construct %||% "Not specified"),
                   tags$li(strong("Confidence: "), ai$Confidence_Score %||% "?", "/5")
                 ),
                 tags$div(
                   style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; font-size: 0.85em;",
                   tags$strong("Key reasoning: "),
                   tags$br(),
                   substr(ai$Reasoning %||% "No reasoning provided", 1, 150), "..."
                 ),
                 tags$div(
                   style = "margin-top: 10px;",
                   tags$span(
                     class = if(!is.null(ai$Confidence_Score) && ai$Confidence_Score >= 4) 
                       "badge badge-success" else "badge badge-warning",
                     if(!is.null(ai$Confidence_Score) && ai$Confidence_Score >= 4) 
                       "High Confidence" else "Medium Confidence"
                   )
                 )
               )
        ),
        
        # Rule-Based Recommendation
        column(4,
               wellPanel(
                 style = "background-color: white; min-height: 400px;",
                 h4(icon("calculator"), " Rule-Based"),
                 tags$div(
                   style = "text-align: center; margin: 20px 0;",
                   tags$h3(
                     style = paste0("color: ", 
                                    if(grepl("c-term", rule_tag_pos)) "#007bff" else "#6f42c1"),
                     if(grepl("n-term", rule_tag_pos)) "N-Terminal"
                     else if(grepl("c-term", rule_tag_pos)) "C-Terminal"
                     else "Not specified"
                   )
                 ),
                 tags$hr(),
                 tags$ul(
                   tags$li(strong("Tag: "), rule$preferred_tag),
                   tags$li(strong("Position: "), rule$preferred_tag_position),
                   tags$li(strong("Construct: "), rule$recommended_construct),
                   tags$li(strong("N-term score: "), decision_data$terminus_analysis$n_terminus$score),
                   tags$li(strong("C-term score: "), decision_data$terminus_analysis$c_terminus$score)
                 ),
                 tags$div(
                   style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; font-size: 0.85em;",
                   tags$strong("Reasoning: "),
                   tags$br(),
                   rule$reasoning
                 ),
                 tags$div(
                   style = "margin-top: 10px;",
                   tags$span(
                     class = "badge badge-info",
                     "Algorithm-Based"
                   )
                 )
               )
        ),
        
        # Disorder-Based Recommendation
        column(4,
               wellPanel(
                 style = paste0("background-color: white; min-height: 400px;",
                                if(is.null(disorder)) " opacity: 0.5;" else ""),
                 h4(icon("wave-square"), " Disorder-Based"),
                 if(!is.null(disorder)) {
                   tagList(
                     tags$div(
                       style = "text-align: center; margin: 20px 0;",
                       tags$h3(
                         style = paste0("color: ", 
                                        if(grepl("c-term", disorder_tag_pos)) "#007bff" else "#6f42c1"),
                         if(grepl("n-term", disorder_tag_pos)) "N-Terminal"
                         else if(grepl("c-term", disorder_tag_pos)) "C-Terminal"
                         else "Not specified"
                       )
                     ),
                     tags$hr(),
                     tags$ul(
                       tags$li(strong("N-term score: "), round(disorder$n_terminus$disorder_score, 2),
                               " (", disorder$n_terminus$interpretation, ")"),
                       tags$li(strong("C-term score: "), round(disorder$c_terminus$disorder_score, 2),
                               " (", disorder$c_terminus$interpretation, ")"),
                       if(disorder$signal_removed) {
                         tags$li(
                           icon("scissors"),
                           strong(" Signal removed: "), "aa 1-", disorder$signal_peptide_end
                         )
                       }
                     ),
                     tags$div(
                       style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; font-size: 0.85em;",
                       tags$strong("Reasoning: "),
                       tags$br(),
                       substr(disorder$reasoning, 1, 150), "..."
                     ),
                     tags$div(
                       style = "margin-top: 10px;",
                       tags$span(
                         class = if(disorder$confidence == "High") "badge badge-success"
                         else if(disorder$confidence == "Medium") "badge badge-warning"
                         else "badge badge-danger",
                         paste(disorder$confidence, "Confidence")
                       )
                     )
                   )
                 } else {
                   tags$div(
                     style = "text-align: center; padding: 50px 20px; color: #6c757d;",
                     icon("exclamation-circle", style = "font-size: 3em;"),
                     tags$h5("Not Available"),
                     tags$p("Disorder analysis was not performed for this protein.")
                   )
                 }
               )
        )
      ),
      
      hr(),
      
      h4(icon("lightbulb"), " Final Recommendation"),
      wellPanel(
        style = "background-color: white;",
        if(consensus != "No consensus") {
          tagList(
            tags$p(
              tags$span(class = "badge badge-success", "PROCEED WITH CONFIDENCE"),
              style = "font-size: 1.1em;"
            ),
            tags$p(
              "Multiple independent methods agree on ", 
              tags$strong(consensus), " tagging. ",
              "This provides strong confidence in the recommendation."
            ),
            tags$ul(
              if(grepl("c-term", ai_tag_pos)) tags$li("✓ AI: C-terminal"),
              if(grepl("n-term", ai_tag_pos)) tags$li("✓ AI: N-terminal"),
              if(grepl("c-term", rule_tag_pos)) tags$li("✓ Rule-based: C-terminal"),
              if(grepl("n-term", rule_tag_pos)) tags$li("✓ Rule-based: N-terminal"),
              if(!is.null(disorder)) {
                if(grepl("c-term", disorder_tag_pos)) tags$li("✓ Disorder: C-terminal")
                else tags$li("✓ Disorder: N-terminal")
              }
            )
          )
        } else {
          tagList(
            tags$p(
              tags$span(class = "badge badge-warning", "MANUAL REVIEW NEEDED"),
              style = "font-size: 1.1em;"
            ),
            tags$p(
              "Methods disagree on tag position. Review each analysis carefully:"
            ),
            tags$ul(
              tags$li("AI (", if(grepl("n-term", ai_tag_pos)) "N-terminal" else "C-terminal", 
                      "): Considers all factors holistically"),
              tags$li("Rule-based (", if(grepl("n-term", rule_tag_pos)) "N-terminal" else "C-terminal", 
                      "): Based on feature scoring"),
              if(!is.null(disorder)) {
                tags$li("Disorder (", if(grepl("n-term", disorder_tag_pos)) "N-terminal" else "C-terminal", 
                        "): Based on terminus flexibility")
              }
            ),
            tags$p(
              strong("Recommendation: "),
              "When methods disagree, prioritize: (1) High AI confidence, (2) Disorder analysis, (3) Rule-based."
            )
          )
        }
      )
    )
  )
})

# Display feature summary
output$decision_feature_summary_ui <- renderUI({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  decision_data <- values$decision[[current_id]]
  
  if (is.null(decision_data)) return(NULL)
  
  term <- decision_data$terminus_analysis
  epi <- decision_data$epitope_analysis
  expr <- decision_data$expression_history
  blockers <- decision_data$expression_blockers
  
  # DEBUG: Print epitope info to console
  cat("\n=== Feature Analysis UI Debug ===\n")
  cat("Protein:", current_id, "\n")
  cat("Epitope analysis results:\n")
  cat("  Total epitopes:", epi$total_epitopes, "\n")
  cat("  With positions:", epi$total_with_positions %||% "not available", "\n")
  cat("  N-terminus epitopes:", epi$n_terminus_epitopes, "\n")
  cat("  C-terminus epitopes:", epi$c_terminus_epitopes, "\n")
  cat("  High-value epitopes:", epi$high_value_epitopes, "\n")
  
  tagList(
    h4("Expression Feasibility Check"),
    wellPanel(
      fluidRow(
        column(4,
               tags$p(
                 strong("Signal Peptide: "),
                 if(blockers$has_signal_peptide) {
                   tags$span(class = "badge badge-success", 
                             icon("check"), " Present - will be replaced")
                 } else {
                   tags$span(class = "badge badge-info", icon("info"), " None detected")
                 }
               ),
               if(blockers$has_signal_peptide) {
                 tags$small("Ends at position ", blockers$signal_peptide_end, 
                            ". Will be replaced with insect signal (HA).")
               }
        ),
        column(4,
               tags$p(
                 strong("Size: "),
                 if(blockers$too_large) {
                   tags$span(class = "badge badge-danger", 
                             icon("exclamation-triangle"), " Large - consider truncation")
                 } else {
                   tags$span(class = "badge badge-success", icon("check"), " Acceptable")
                 }
               ),
               tags$small(decision_data$protein_info$sequence_length, " aa (~", 
                          round(decision_data$protein_info$sequence_length * 0.11), " kDa)")
        ),
        column(4,
               tags$p(
                 strong("Transmembrane: "),
                 if(blockers$has_transmembrane) {
                   tags$span(class = "badge badge-danger", 
                             blockers$transmembrane_count, " domain(s) - problematic")
                 } else {
                   tags$span(class = "badge badge-success", icon("check"), " None")
                 }
               )
        )
      )
    ),
    
    hr(),
    
    h4("Terminus Feature Analysis"),
    fluidRow(
      column(6,
             wellPanel(
               h5("N-Terminus"),
               tags$p(strong("Risk Level: "), 
                      tags$span(class = paste0("badge badge-", 
                                               if(term$n_terminus$recommendation == "OK" || term$n_terminus$recommendation == "EXCELLENT") "success" 
                                               else if(term$n_terminus$recommendation == "CAUTION") "warning" 
                                               else "danger"),
                                term$n_terminus$recommendation
                      )
               ),
               tags$ul(
                 tags$li("Total features: ", term$n_terminus$total_features),
                 tags$li("High-impact: ", term$n_terminus$high_impact),
                 tags$li("Epitopes: ", term$n_terminus$epitopes, 
                         " (", term$n_terminus$high_value_epitopes, " high-value)"),
                 tags$li("Score: ", term$n_terminus$score)
               )
             )
      ),
      column(6,
             wellPanel(
               h5("C-Terminus"),
               tags$p(strong("Risk Level: "), 
                      tags$span(class = paste0("badge badge-", 
                                               if(term$c_terminus$recommendation == "OK" || term$c_terminus$recommendation == "EXCELLENT") "success" 
                                               else if(term$c_terminus$recommendation == "CAUTION") "warning" 
                                               else "danger"),
                                term$c_terminus$recommendation
                      )
               ),
               tags$ul(
                 tags$li("Total features: ", term$c_terminus$total_features),
                 tags$li("High-impact: ", term$c_terminus$high_impact),
                 tags$li("Epitopes: ", term$c_terminus$epitopes,
                         " (", term$c_terminus$high_value_epitopes, " high-value)"),
                 tags$li("Score: ", term$c_terminus$score)
               )
             )
      )
    ),
    
    hr(),
    
    h4("Epitope Coverage"),
    wellPanel(
      tags$ul(
        tags$li("Total epitopes found: ", epi$total_epitopes),
        if(!is.null(epi$total_with_positions)) {
          tags$li("With valid positions: ", epi$total_with_positions)
        },
        tags$li("High-value epitopes (evidence ≥4): ", epi$high_value_epitopes),
        tags$li("N-terminus (first ", decision_data$protein_info$n_term_buffer, " aa): ", 
                epi$n_terminus_epitopes),
        tags$li("C-terminus (last ", decision_data$protein_info$c_term_buffer, " aa): ", 
                epi$c_terminus_epitopes)
      ),
      tags$p(strong("Recommendation: "), epi$recommendation),
      
      # Add debugging info if no epitopes in termini
      if(epi$total_epitopes > 0 && epi$n_terminus_epitopes == 0 && epi$c_terminus_epitopes == 0) {
        tags$div(
          style = "background-color: #fff3cd; padding: 10px; margin-top: 10px; border-radius: 5px;",
          tags$small(
            icon("info-circle"), 
            " Note: Epitopes found but none within the N/C-terminal regions. ",
            "They may be in the core of the protein."
          )
        )
      }
    ),
    
    hr(),
    
    h4("Expression History"),
    wellPanel(
      if(expr$successful_expression) {
        tagList(
          tags$p(
            tags$span(class = "badge badge-success", icon("check"), " Prior Success")
          ),
          tags$ul(
            tags$li("Best system: ", expr$best_system),
            tags$li("Recommended tag: ", expr$recommended_tag),
            tags$li("Position: ", expr$tag_position),
            tags$li("Evidence score: ", expr$evidence_score, "/5")
          )
        )
      } else {
        tags$p(
          tags$span(class = "badge badge-warning", icon("info-circle")),
          " ", expr$summary
        )
      }
    )
  )
})

# Full report
output$decision_full_report <- renderText({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) {
    return("No protein selected")
  }
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
  }
  
  decision_data <- values$decision[[current_id]]
  
  if (is.null(decision_data)) {
    return("No analysis available. Click 'Generate Strategy' to begin.")
  }
  
  jsonlite::toJSON(decision_data$ai_decision, pretty = TRUE, auto_unbox = TRUE)
})

# Protein plot with decision overlay
output$decision_protein_plot <- plotly::renderPlotly({
  current_ids <- uniprot_ids()
  if (length(current_ids) == 0) return(NULL)
  
  current_id <- current_decision_protein()
  if (is.null(current_id) || !current_id %in% current_ids) {
    current_id <- current_ids[1]
    current_decision_protein(current_id)
  }
  
  features_df <- values$feature_list[[current_id]]
  decision_data <- values$decision[[current_id]]
  
  # Get epitope data from IEDB or OpenAI
  epitope_df <- NULL
  if(!is.null(values$iedb[[current_id]]$epitopes)) {
    epitope_df <- values$iedb[[current_id]]$epitopes
  } else if(!is.null(values$openai[[current_id]]$epitopes)) {
    epitope_df <- values$openai[[current_id]]$epitopes
  }
  
  if (is.null(features_df) || nrow(features_df) == 0) {
    return(plotly::plot_ly() %>%
             plotly::add_annotations(
               text = "No feature data available. Please fetch protein data first.",
               x = 0.5, y = 0.5,
               xref = "paper", yref = "paper",
               showarrow = FALSE,
               font = list(size = 16)
             ))
  }
  
  sequence_length <- if(!is.null(decision_data)) {
    decision_data$protein_info$sequence_length
  } else {
    max(features_df$end, na.rm = TRUE)
  }
  
  n_buffer <- if(!is.null(decision_data)) {
    decision_data$protein_info$n_term_buffer
  } else {
    30
  }
  
  c_buffer <- if(!is.null(decision_data)) {
    decision_data$protein_info$c_term_buffer
  } else {
    30
  }
  
  # Use plotting function with epitopes
  plot_protein_features_with_epitopes(
    features_df,
    epitope_df = epitope_df,
    uniprot_id = current_id,
    sequence_length = sequence_length,
    n_term_buffer = n_buffer,
    c_term_buffer = c_buffer
  )
})






# ===========================================================================
# EXPERT REVIEW TAB OUTPUTS
# ===========================================================================

# Protein header
output$expert_review_protein_header <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(NULL)
  
  uniprot_data <- values$uniprot_list[[current_id]]
  if (is.null(uniprot_data)) return(NULL)
  
  tagList(
    tags$p(
      style = "margin: 0; font-size: 1.1em;",
      tags$strong(uniprot_data$protein_name),
      tags$span(
        style = "color: #666; margin-left: 10px;",
        "(", current_id, ")"
      )
    ),
    tags$p(
      style = "margin: 5px 0 0 0; color: #666;",
      nchar(uniprot_data$protein_sequence), " aa | ",
      "~", round(nchar(uniprot_data$protein_sequence) * 0.11, 0), " kDa"
    )
  )
})

# Timestamp
output$expert_review_timestamp <- renderText({
  format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")
})

# AI Executive Summary
output$expert_review_ai_summary <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(tags$p("No analysis available"))
  
  decision_data <- values$decision[[current_id]]
  if (is.null(decision_data)) return(tags$p("Run 'Generate Strategy' first"))
  
  # Generate AI summary using GPT
  summary <- generate_executive_summary(decision_data, current_id)
  
  tags$div(
    style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; border-radius: 5px;",
    HTML(markdown::markdownToHTML(text = summary, fragment.only = TRUE))
  )
})

# Consensus recommendation
output$expert_review_consensus <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(NULL)
  
  decision_data <- values$decision[[current_id]]
  structure_data <- values$structure_prediction[[current_id]]
  
  if (is.null(decision_data)) return(tags$p("No analysis available"))
  
  # Extract recommendations
  ai_rec <- decision_data$ai_decision$Preferred_Tag_Position
  rule_rec <- decision_data$rule_based_decision$preferred_tag_position
  disorder_rec <- if(!is.null(decision_data$disorder_analysis)) {
    decision_data$disorder_analysis$recommendation
  } else {
    "Not available"
  }
  structure_rec <- if(!is.null(structure_data)) {
    structure_data$recommendation
  } else {
    "Not available"
  }
  
  # Count votes
  methods <- list(
    AI = ai_rec,
    `Rule-Based` = rule_rec,
    Disorder = disorder_rec,
    Structure = structure_rec
  )
  
  # Determine consensus
  n_votes <- sum(grepl("N-term", unlist(methods), ignore.case = TRUE))
  c_votes <- sum(grepl("C-term", unlist(methods), ignore.case = TRUE))
  
  consensus <- if(c_votes >= 3) {
    list(position = "C-terminal", color = "#28a745", vector = "pPRO8")
  } else if(n_votes >= 3) {
    list(position = "N-terminal", color = "#6f42c1", vector = "pPRO30A-SP or pPRO30A")
  } else {
    list(position = "No consensus", color = "#ffc107", vector = "Manual review required")
  }
  
  tagList(
    # Methods table
    tags$table(
      class = "table table-sm",
      style = "margin-bottom: 20px;",
      tags$thead(
        tags$tr(
          tags$th("Method"),
          tags$th("Recommendation"),
          tags$th("Confidence")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(icon("robot"), " AI Decision"),
          tags$td(
            style = paste0("color: ", if(grepl("C-term", ai_rec, ignore.case = TRUE)) "#007bff" else "#6f42c1"),
            ai_rec
          ),
          tags$td(decision_data$ai_decision$Confidence_Score, "/5")
        ),
        tags$tr(
          tags$td(icon("calculator"), " Rule-Based"),
          tags$td(
            style = paste0("color: ", if(grepl("C-term", rule_rec, ignore.case = TRUE)) "#007bff" else "#6f42c1"),
            rule_rec
          ),
          tags$td(decision_data$rule_based_decision$confidence)
        ),
        if(!is.null(decision_data$disorder_analysis)) {
          tags$tr(
            tags$td(icon("wave-square"), " Disorder"),
            tags$td(
              style = paste0("color: ", if(grepl("C-term", disorder_rec, ignore.case = TRUE)) "#007bff" else "#6f42c1"),
              disorder_rec
            ),
            tags$td(decision_data$disorder_analysis$confidence)
          )
        },
        if(!is.null(structure_data)) {
          tags$tr(
            tags$td(icon("cube"), " Structure"),
            tags$td(
              style = paste0("color: ", if(grepl("C-term", structure_rec, ignore.case = TRUE)) "#007bff" else "#6f42c1"),
              structure_rec
            ),
            tags$td(structure_data$confidence)
          )
        }
      )
    ),
    
    # Consensus box
    tags$div(
      style = paste0("background-color: ", consensus$color, "20; border: 2px solid ", consensus$color, "; padding: 15px; border-radius: 5px; text-align: center;"),
      h4(
        style = paste0("color: ", consensus$color, "; margin: 0;"),
        icon("bullseye"),
        " CONSENSUS: ", toupper(consensus$position)
      ),
      tags$p(
        style = "margin: 10px 0 0 0;",
        if(c_votes + n_votes >= 3) {
          paste0(max(c_votes, n_votes), " of ", sum(!is.na(unlist(methods))), " methods agree")
        } else {
          "Methods disagree - manual review recommended"
        }
      )
    )
  )
})

# Evidence table
output$expert_review_evidence_table <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(tags$p("No protein selected"))
  
  decision_data <- values$decision[[current_id]]
  structure_data <- values$structure_prediction[[current_id]]
  
  if (is.null(decision_data)) return(tags$p("Run 'Generate Strategy' first"))
  
  # Safely extract evidence with defaults
  term <- decision_data$terminus_analysis
  epi <- decision_data$epitope_analysis
  disorder <- decision_data$disorder_analysis
  
  # Check if we have minimum data
  if (is.null(term) || is.null(epi)) {
    return(tags$div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      " Incomplete analysis data. Please run 'Generate Strategy' first."
    ))
  }
  
  # Safe extraction with defaults
  n_epitopes <- epi$n_terminus_epitopes %||% 0
  c_epitopes <- epi$c_terminus_epitopes %||% 0
  
  n_disorder_score <- if(!is.null(disorder) && !is.null(disorder$n_terminus)) {
    disorder$n_terminus$disorder_score %||% NA
  } else {
    NA
  }
  
  c_disorder_score <- if(!is.null(disorder) && !is.null(disorder$c_terminus)) {
    disorder$c_terminus$disorder_score %||% NA
  } else {
    NA
  }
  
  n_disorder_interp <- if(!is.null(disorder) && !is.null(disorder$n_terminus)) {
    disorder$n_terminus$interpretation %||% "Unknown"
  } else {
    "Not calculated"
  }
  
  c_disorder_interp <- if(!is.null(disorder) && !is.null(disorder$c_terminus)) {
    disorder$c_terminus$interpretation %||% "Unknown"
  } else {
    "Not calculated"
  }
  
  n_features <- term$n_terminus$total_features %||% 0
  c_features <- term$c_terminus$total_features %||% 0
  n_high_impact <- term$n_terminus$high_impact %||% 0
  c_high_impact <- term$c_terminus$high_impact %||% 0
  n_ptms <- term$n_terminus$ptms %||% 0
  c_ptms <- term$c_terminus$ptms %||% 0
  
  # Structure data (if available)
  n_rmsd <- if(!is.null(structure_data) && !is.null(structure_data$rmsd)) {
    structure_data$rmsd$n_terminal %||% NA
  } else {
    NA
  }
  
  c_rmsd <- if(!is.null(structure_data) && !is.null(structure_data$rmsd)) {
    structure_data$rmsd$c_terminal %||% NA
  } else {
    NA
  }
  
  n_plddt <- if(!is.null(structure_data) && !is.null(structure_data$results$n_tagged)) {
    structure_data$results$n_tagged$mean_plddt %||% NA
  } else {
    NA
  }
  
  c_plddt <- if(!is.null(structure_data) && !is.null(structure_data$results$c_tagged)) {
    structure_data$results$c_tagged$mean_plddt %||% NA
  } else {
    NA
  }
  
  # Build table
  tags$table(
    class = "table table-bordered",
    style = "text-align: center;",
    tags$thead(
      tags$tr(
        tags$th(style = "width: 30%; text-align: center;", "N-Terminus"),
        tags$th(style = "width: 40%; text-align: center;", "Metric"),
        tags$th(style = "width: 30%; text-align: center;", "C-Terminus")
      )
    ),
    tags$tbody(
      # Epitopes
      tags$tr(
        tags$td(
          style = if(n_epitopes == 0) "background-color: #d4edda;" else "background-color: #fff3cd;",
          n_epitopes, " epitope", if(n_epitopes != 1) "s" else "",
          if(n_epitopes > 0) tags$span(" ", icon("exclamation-triangle")) else NULL
        ),
        tags$td(tags$strong("Epitopes")),
        tags$td(
          style = if(c_epitopes == 0) "background-color: #d4edda;" else "background-color: #fff3cd;",
          c_epitopes, " epitope", if(c_epitopes != 1) "s" else "",
          if(c_epitopes > 0) tags$span(" ", icon("exclamation-triangle")) else NULL
        )
      ),
      
      # Disorder (only if available)
      if(!is.null(disorder)) {
        tags$tr(
          tags$td(
            if(!is.na(n_disorder_score)) {
              tagList(
                round(n_disorder_score, 2),
                tags$br(),
                tags$small("(", n_disorder_interp, ")")
              )
            } else {
              "N/A"
            }
          ),
          tags$td(tags$strong("Disorder Score")),
          tags$td(
            if(!is.na(c_disorder_score)) {
              tagList(
                round(c_disorder_score, 2),
                tags$br(),
                tags$small("(", c_disorder_interp, ")")
              )
            } else {
              "N/A"
            }
          )
        )
      },
      
      # Features
      tags$tr(
        tags$td(
          n_features, " feature", if(n_features != 1) "s" else "",
          if(n_high_impact > 0) {
            tagList(tags$br(), tags$small("(", n_high_impact, " high-impact)"))
          }
        ),
        tags$td(tags$strong("UniProt Features")),
        tags$td(
          c_features, " feature", if(c_features != 1) "s" else "",
          if(c_high_impact > 0) {
            tagList(tags$br(), tags$small("(", c_high_impact, " high-impact)"))
          }
        )
      ),
      
      # PTMs
      tags$tr(
        tags$td(
          style = if(n_ptms == 0) "background-color: #d4edda;" else "background-color: #fff3cd;",
          n_ptms, " PTM", if(n_ptms != 1) "s" else "",
          if(n_ptms > 0) tags$span(" ", icon("exclamation-triangle")) else NULL
        ),
        tags$td(tags$strong("Post-translational Modifications")),
        tags$td(
          style = if(c_ptms == 0) "background-color: #d4edda;" else "background-color: #fff3cd;",
          c_ptms, " PTM", if(c_ptms != 1) "s" else "",
          if(c_ptms > 0) tags$span(" ", icon("exclamation-triangle")) else NULL
        )
      ),
      
      # Structure (only if available)
      if(!is.null(structure_data) && !is.na(n_rmsd) && !is.na(c_rmsd)) {
        tagList(
          tags$tr(
            tags$td(
              round(n_rmsd, 2), " Å",
              if(n_rmsd < c_rmsd) tags$span(" ", icon("check", style = "color: green;")) else NULL
            ),
            tags$td(tags$strong("Structure Impact (RMSD)")),
            tags$td(
              round(c_rmsd, 2), " Å",
              if(c_rmsd < n_rmsd) tags$span(" ", icon("check", style = "color: green;")) else NULL
            )
          ),
          tags$tr(
            tags$td(
              if(!is.na(n_plddt)) paste0(round(n_plddt, 1), " pLDDT") else "N/A"
            ),
            tags$td(tags$strong("Structure Confidence")),
            tags$td(
              if(!is.na(c_plddt)) paste0(round(c_plddt, 1), " pLDDT") else "N/A"
            )
          )
        )
      }
    )
  )
})

# Key considerations
output$expert_review_considerations <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(tags$p("No protein selected"))
  
  decision_data <- values$decision[[current_id]]
  if (is.null(decision_data)) return(tags$p("Run 'Generate Strategy' first"))
  
  considerations <- character()
  
  # Signal peptide (safe check)
  if(!is.null(decision_data$expression_blockers) && 
     !is.null(decision_data$expression_blockers$has_signal_peptide) &&
     isTRUE(decision_data$expression_blockers$has_signal_peptide)) {
    
    sp_end <- decision_data$expression_blockers$signal_peptide_end %||% "?"
    considerations <- c(considerations, 
                        paste0("• Signal peptide present (1-", sp_end, 
                               ") - will be replaced with insect signal for secretion"))
  }
  
  # High-value epitopes (safe check)
  if(!is.null(decision_data$epitope_analysis) && 
     !is.null(decision_data$epitope_analysis$high_value_epitopes)) {
    
    hv_epi <- decision_data$epitope_analysis$high_value_epitopes
    if(!is.na(hv_epi) && hv_epi > 0) {
      considerations <- c(considerations,
                          paste0("• ", hv_epi, " high-value epitope", 
                                 if(hv_epi > 1) "s" else "", 
                                 " detected - ensure accessibility"))
    }
  }
  
  # PTMs (safe check)
  if(!is.null(decision_data$terminus_analysis)) {
    n_ptms <- decision_data$terminus_analysis$n_terminus$ptms %||% 0
    c_ptms <- decision_data$terminus_analysis$c_terminus$ptms %||% 0
    
    if(n_ptms > 0 || c_ptms > 0) {
      considerations <- c(considerations,
                          paste0("• Post-translational modifications: N-term=", n_ptms, ", C-term=", c_ptms))
    }
  }
  
  # Transmembrane (safe check)
  if(!is.null(decision_data$expression_blockers) && 
     !is.null(decision_data$expression_blockers$has_transmembrane) &&
     isTRUE(decision_data$expression_blockers$has_transmembrane)) {
    
    tm_count <- decision_data$expression_blockers$transmembrane_count %||% "?"
    considerations <- c(considerations,
                        paste0("⚠️ ", tm_count, " transmembrane domain", 
                               if(tm_count != 1) "s" else "", 
                               " - expression may be challenging"))
  }
  
  # Size (safe check)
  if(!is.null(decision_data$expression_blockers) && 
     !is.null(decision_data$expression_blockers$too_large) &&
     isTRUE(decision_data$expression_blockers$too_large)) {
    
    seq_len <- decision_data$protein_info$sequence_length %||% "?"
    considerations <- c(considerations,
                        paste0("⚠️ Large protein (", seq_len, " aa) - consider truncation or alternative systems"))
  }
  
  # Expression history (safe check)
  if(!is.null(decision_data$expression_history)) {
    if(!is.null(decision_data$expression_history$successful_expression) &&
       isTRUE(decision_data$expression_history$successful_expression)) {
      
      best_sys <- decision_data$expression_history$best_system %||% "unknown system"
      rec_tag <- decision_data$expression_history$recommended_tag %||% "unknown tag"
      
      considerations <- c(considerations,
                          paste0("✓ Prior successful expression: ", best_sys, " with ", rec_tag))
    } else {
      considerations <- c(considerations,
                          "• No prior expression history found in database")
    }
  } else {
    considerations <- c(considerations,
                        "• No prior expression history found in database")
  }
  
  # Structure analysis (if available)
  structure_data <- values$structure_prediction[[current_id]]
  if(!is.null(structure_data) && 
     !is.null(structure_data$rmsd) &&
     !is.na(structure_data$rmsd$n_terminal) && 
     !is.na(structure_data$rmsd$c_terminal)) {
    
    n_rmsd <- structure_data$rmsd$n_terminal
    c_rmsd <- structure_data$rmsd$c_terminal
    
    considerations <- c(considerations,
                        paste0("✓ Structure validation completed: N-term RMSD=", round(n_rmsd, 2), 
                               " Å, C-term RMSD=", round(c_rmsd, 2), " Å"))
  }
  
  # If no considerations found
  if(length(considerations) == 0) {
    return(tags$p(
      style = "color: #666; font-style: italic;",
      "No special considerations identified. Standard expression conditions should apply."
    ))
  }
  
  tags$ul(
    style = "line-height: 1.8;",
    lapply(considerations, function(x) tags$li(HTML(x)))
  )
})

# Protein characteristics
output$expert_review_characteristics <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(NULL)
  
  uniprot_data <- values$uniprot_list[[current_id]]
  decision_data <- values$decision[[current_id]]
  
  if (is.null(uniprot_data)) return(NULL)
  
  # Extract function safely (function is a reserved word)
  protein_function <- uniprot_data$protein_function %||% 
    uniprot_data$`function` %||% 
    "Not specified"
  
  # Truncate if too long
  if (nchar(protein_function) > 200) {
    protein_function <- paste0(substr(protein_function, 1, 200), "...")
  }
  
  tags$ul(
    tags$li(tags$strong("Length: "), decision_data$protein_info$sequence_length, " aa"),
    tags$li(tags$strong("Molecular Weight: "), "~", round(decision_data$protein_info$sequence_length * 0.11, 0), " kDa"),
    tags$li(tags$strong("Organism: "), uniprot_data$organism %||% "Not specified"),
    tags$li(tags$strong("Function: "), protein_function),
    if(!is.null(uniprot_data$subcellular_location)) {
      tags$li(tags$strong("Localization: "), uniprot_data$subcellular_location)
    }
  )
})

# Vector selection with recommendation highlighted
output$expert_review_vector_selection <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(NULL)
  
  decision_data <- values$decision[[current_id]]
  if (is.null(decision_data)) return(NULL)
  
  # Determine recommended vector
  rule_rec <- decision_data$rule_based_decision$preferred_tag_position
  has_signal <- decision_data$expression_blockers$has_signal_peptide
  
  recommended <- if(grepl("C-term", rule_rec, ignore.case = TRUE)) {
    "pPRO8"
  } else if(has_signal) {
    "pPRO30A-SP"
  } else {
    "pPRO30A"
  }
  
  choices <- c(
    "pPRO8 (C-terminal: GGGGSGGGGS-BCCP-GGSGSG-cMyc-10xHis)" = "pPRO8",
    "pPRO30A-SP (N-terminal with insect signal: Insect SP-cMyc-GGSGSG-BCCP-GGGGSGGGGS)" = "pPRO30A-SP",
    "pPRO30A (N-terminal, no signal: M-cMyc-GGSGSG-BCCP-GGGGSGGGGS)" = "pPRO30A"
  )
  
  # Add star to recommended
  names(choices)[which(choices == recommended)] <- paste0("⭐ ", names(choices)[which(choices == recommended)], " - RECOMMENDED")
  
  radioButtons('selected_vector',
               NULL,
               choices = choices,
               selected = recommended)
})

# Generate executive summary with GPT
generate_executive_summary <- function(decision_data, uniprot_id) {
  
  tryCatch({
    
    prompt <- paste0(
      "You are an expert protein expression consultant. Review this data and provide a concise 2-3 paragraph executive summary for a lab scientist.\n\n",
      "Protein: ", decision_data$protein_info$protein_name, " (", uniprot_id, ")\n",
      "Length: ", decision_data$protein_info$sequence_length, " aa\n\n",
      
      "ANALYSIS RESULTS:\n",
      "- AI Recommendation: ", decision_data$ai_decision$Preferred_Tag_Position, " (Confidence: ", decision_data$ai_decision$Confidence_Score, "/5)\n",
      "- Rule-Based: ", decision_data$rule_based_decision$preferred_tag_position, "\n",
      "- Disorder Analysis: ", if(!is.null(decision_data$disorder_analysis)) decision_data$disorder_analysis$recommendation else "N/A", "\n\n",
      
      "KEY FINDINGS:\n",
      "- N-terminus: ", decision_data$terminus_analysis$n_terminus$total_features, " features, ", 
      decision_data$terminus_analysis$n_terminus$epitopes, " epitopes, ",
      decision_data$terminus_analysis$n_terminus$ptms, " PTMs\n",
      "- C-terminus: ", decision_data$terminus_analysis$c_terminus$total_features, " features, ", 
      decision_data$terminus_analysis$c_terminus$epitopes, " epitopes, ",
      decision_data$terminus_analysis$c_terminus$ptms, " PTMs\n",
      "- Signal peptide: ", if(decision_data$expression_blockers$has_signal_peptide) "Present" else "Absent", "\n",
      "- Expression blockers: ", if(decision_data$expression_blockers$has_transmembrane) "Transmembrane domains" else "None", "\n\n",
      
      "Provide:\n",
      "1. Brief summary of key findings\n",
      "2. Main concerns or considerations\n",
      "3. Overall confidence in the recommendation\n\n",
      "Keep it professional, concise, and actionable."
    )
    
    response <- openai_api_call(prompt, model = "gpt-4o-mini")
    
    return(response$choices[[1]]$message$content)
    
  }, error = function(e) {
    return(paste0(
      "**Summary:** Based on automated analysis, ",
      decision_data$rule_based_decision$preferred_tag_position,
      " is recommended.\n\n",
      "**Key Findings:** ",
      decision_data$rule_based_decision$reasoning,
      "\n\n**Confidence:** ",
      decision_data$rule_based_decision$confidence
    ))
  })
}

# Approve design button
observeEvent(input$approve_design, {
  
  current_id <- current_decision_protein()
  if (is.null(current_id)) return()
  
  decision_data <- values$decision[[current_id]]
  uniprot_data <- values$uniprot_list[[current_id]]
  
  if (is.null(decision_data) || is.null(uniprot_data)) {
    showNotification("No analysis data available", type = "error")
    return()
  }
  
  # Get signal peptide info
  signal_peptide_end <- if(decision_data$expression_blockers$has_signal_peptide) {
    decision_data$expression_blockers$signal_peptide_end
  } else {
    NULL
  }
  
  # Build construct
  withProgress(message = 'Generating Construct', value = 0, {
    
    incProgress(0.3, detail = "Building sequences...")
    
    # Build all 3 constructs
    constructs <- build_all_constructs(
      uniprot_data$protein_sequence,
      signal_peptide_end
    )
    
    incProgress(0.6, detail = "Selecting approved vector...")
    
    # Get selected construct
    selected_vector <- input$selected_vector
    selected_construct <- if(selected_vector == "pPRO8") {
      constructs$c_tagged
    } else if(selected_vector == "pPRO30A-SP") {
      constructs$n_tagged  # Will be pPRO30A-SP if signal present
    } else {
      constructs$n_tagged  # Will be pPRO30A if no signal
    }
    
    incProgress(0.9, detail = "Saving design...")
    
    # Store approved design
    if (is.null(values$approved_designs)) {
      values$approved_designs <- list()
    }
    
    values$approved_designs[[current_id]] <- list(
      uniprot_id = current_id,
      protein_name = decision_data$protein_info$protein_name,
      selected_vector = selected_vector,
      construct = selected_construct,
      expert_notes = input$expert_notes,
      approved_by = "MichMullins",
      approved_at = Sys.time(),
      decision_data = decision_data
    )
    
    # Save to disk
    saveRDS(values$approved_designs, 'Data/approved_designs.rds')
    
    incProgress(1, detail = "Complete!")
    
    showNotification(
      paste0("✓ Design approved! Construct generated for ", selected_vector),
      type = "message",
      duration = 10
    )
  })
})

# Show approved design
output$design_approved <- reactive({
  current_id <- current_decision_protein()
  !is.null(values$approved_designs[[current_id]])
})
outputOptions(output, "design_approved", suspendWhenHidden = FALSE)

output$approved_design_output <- renderUI({
  current_id <- current_decision_protein()
  if (is.null(current_id)) return(NULL)
  
  approved <- values$approved_designs[[current_id]]
  if (is.null(approved)) return(NULL)
  
  construct <- approved$construct
  
  wellPanel(
    style = "background-color: #d4edda; border: 2px solid #28a745;",
    h4(icon("check-circle"), " Design Approved & Construct Generated"),
    
    tags$p(
      tags$strong("Vector: "), approved$selected_vector, br(),
      tags$strong("Approved by: "), approved$approved_by, br(),
      tags$strong("Date: "), format(approved$approved_at, "%Y-%m-%d %H:%M:%S UTC")
    ),
    
    if(nchar(approved$expert_notes) > 0) {
      tags$div(
        style = "background-color: white; padding: 10px; margin: 10px 0; border-radius: 5px;",
        tags$strong("Expert Notes:"), br(),
        approved$expert_notes
      )
    },
    
    hr(),
    
    h5("Construct Details:"),
    tags$ul(
      tags$li(tags$strong("Total Length: "), construct$total_length, " aa"),
      tags$li(tags$strong("Export Length: "), nchar(construct$export_sequence), " aa"),
      tags$li(tags$strong("Description: "), construct$description)
    ),
    
    h5("Export Sequence (for cloning):"),
    tags$pre(
      style = "background-color: white; padding: 10px; border: 1px solid #ccc; max-height: 200px; overflow-y: auto; font-family: monospace; font-size: 0.9em;",
      ">", approved$protein_name, "_", approved$selected_vector, "\n",
      construct$export_sequence
    ),
    
    tags$p(
      tags$em("Note: ", construct$export_note)
    ),
    
    downloadButton('download_approved_construct', 
                   'Download FASTA',
                   class = "btn-primary"),
    downloadButton('download_approved_report', 
                   'Download Full Report',
                   class = "btn-info",
                   style = "margin-left: 10px;")
  )
})

# Download handlers
output$download_approved_construct <- downloadHandler(
  filename = function() {
    current_id <- current_decision_protein()
    approved <- values$approved_designs[[current_id]]
    paste0(approved$protein_name, "_", approved$selected_vector, "_", 
           format(Sys.time(), "%Y%m%d"), ".fasta")
  },
  content = function(file) {
    current_id <- current_decision_protein()
    approved <- values$approved_designs[[current_id]]
    construct <- approved$construct
    
    fasta_content <- paste0(
      ">", approved$protein_name, "_", approved$selected_vector, " | ",
      "Approved by ", approved$approved_by, " on ", format(approved$approved_at, "%Y-%m-%d"), "\n",
      construct$export_sequence
    )
    
    writeLines(fasta_content, file)
  }
)
