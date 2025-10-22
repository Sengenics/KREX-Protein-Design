# shiny_sections/decision_ui.R
# Complete decision UI with comparison tabs

tagList(
  fluidRow(
    column(12,
           h3(icon("lightbulb"), "AI Expression Strategy Advisor"),
           p("Combines UniProt features, IEDB/OpenAI epitope data, and expression history to recommend optimal expression strategy")
    )
  ),
  
  fluidRow(
    column(4,
           wellPanel(
             h4("Analysis Settings"),
             
             numericInput('decision_n_term_buffer', 'N-terminus region (aa)',
                          value = 30, min = 10, max = 150, step = 10),
             
             numericInput('decision_c_term_buffer', 'C-terminus region (aa)',
                          value = 30, min = 10, max = 150, step = 10),
             
             selectInput('decision_model', 'AI Model',
                         choices = c("GPT-4o" = "gpt-4o",
                                     "GPT-4o Mini" = "gpt-4o-mini",
                                     "GPT-5 Mini" = "gpt-5-mini",
                                     "GPT-5" = "gpt-5"),
                         selected = "gpt-5"),
             
             hr(),
             
             h5("Select Protein to Analyze:"),
             selectInput('decision_protein_select', NULL,
                         choices = NULL,
                         multiple = FALSE),
             
             actionButton('run_decision', 'Generate Strategy', 
                          class = "btn-success btn-block",
                          icon = icon("play")),
             
             br(),
             
             actionButton('run_decision_batch', 'Generate for All Selected', 
                          class = "btn-primary btn-block",
                          icon = icon("list")),
             
             hr(),
             
             h5("Current Protein:"),
             fluidRow(
               column(12,
                      actionButton('decision_prev', '', icon = icon("arrow-left"), 
                                   class = "btn-sm", style = "margin: 2px;"),
                      actionButton('decision_next', '', icon = icon("arrow-right"), 
                                   class = "btn-sm", style = "margin: 2px;")
               )
             ),
             verbatimTextOutput('decision_current_protein'),
             
             h5("Data Status:"),
             uiOutput('decision_data_status')
           )
    ),
    
    column(8,
           tabsetPanel(
             tabPanel("AI Recommendation",
                      br(),
                      uiOutput('decision_recommendation_ui')
             ),
             
             tabPanel("Rule-Based Analysis",
                      br(),
                      uiOutput('decision_rule_based_ui')
             ),
             
             tabPanel("Disorder Score",
                      br(),
                      uiOutput('decision_disorder_ui')
             ),
             
             # ========== UPDATED STRUCTURE VALIDATION TAB ==========
             tabPanel("Structure Validation",
                      br(),
                      fluidRow(
                        column(4,
                               wellPanel(
                                 h4(icon("cube"), " ESM3 Structure Prediction"),
                                 
                                 tags$div(
                                   style = "background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                                   tags$small(
                                     icon("info-circle"),
                                     " Uses ESM3 AI to predict 3D structures and calculate RMSD (structural deviation)"
                                   )
                                 ),
                                 
                                 # API Token Input
                                 passwordInput("esm_forge_token", 
                                               "ESM Forge API Token:",
                                               placeholder = "Enter your token",
                                               value = Sys.getenv("ESM_FORGE_TOKEN")),
                                 tags$small(
                                   "Get your token at ",
                                   tags$a("forge.evolutionaryscale.ai", 
                                          href = "https://forge.evolutionaryscale.ai", 
                                          target = "_blank",
                                          style = "color: #007bff;")
                                 ),
                                 
                                 hr(),
                                 
                                 h5("Current Protein:"),
                                 wellPanel(
                                   style = "background-color: #f8f9fa; max-height: 200px; overflow-y: auto;",
                                   verbatimTextOutput('structure_sequence_preview', placeholder = TRUE)
                                 ),
                                 
                                 hr(),
                                 
                                 actionButton('run_structure_prediction', 
                                              'Predict All 3 Structures', 
                                              class = "btn-primary btn-lg btn-block",
                                              icon = icon("rocket")),
                                 
                                 tags$div(
                                   style = "background-color: #fff3cd; padding: 10px; margin-top: 10px; border-radius: 5px;",
                                   tags$small(
                                     icon("clock"),
                                     tags$strong(" Expected time: 1-3 minutes"),
                                     br(),
                                     "• Predicts untagged (reference)",
                                     br(),
                                     "• Predicts N-terminal tag",
                                     br(),
                                     "• Predicts C-terminal tag",
                                     br(),
                                     "• Calculates RMSD & recommends"
                                   )
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "output.structure_has_results",
                                   hr(),
                                   h5(icon("download"), " Download Results:"),
                                   downloadButton('download_structure_pdbs', 
                                                  'All PDB Files (ZIP)',
                                                  class = "btn-success btn-block",
                                                  style = "margin-bottom: 5px;"),
                                   downloadButton('download_structure_report', 
                                                  'Comparison Report',
                                                  class = "btn-info btn-block")
                                 )
                               )
                        ),
                        
                        column(8,
                               uiOutput('structure_results_ui')
                        )
                      )
             ),
             
             tabPanel("Comparison",
                      br(),
                      uiOutput('decision_comparison_ui')
             ),
             
             tabPanel("Expert Review",
                      br(),
                      fluidRow(
                        column(12,
                               # Protein header
                               wellPanel(
                                 style = "background-color: #f8f9fa;",
                                 fluidRow(
                                   column(8,
                                          h3(icon("user-md"), " Expert Review - Final Decision Checkpoint"),
                                          uiOutput('expert_review_protein_header')
                                   ),
                                   column(4,
                                          tags$div(
                                            style = "text-align: right; padding-top: 20px;",
                                            tags$span(
                                              style = "font-size: 0.9em; color: #666;",
                                              "Reviewed by: ", tags$strong("MichMullins"),
                                              br(),
                                              textOutput("expert_review_timestamp", inline = TRUE)
                                            )
                                          )
                                   )
                                 )
                               ),
                               
                               # AI Executive Summary
                               wellPanel(
                                 h4(icon("robot"), " AI Executive Summary"),
                                 uiOutput('expert_review_ai_summary')
                               ),
                               
                               # Consensus recommendation
                               wellPanel(
                                 h4(icon("bullseye"), " Consensus Recommendation"),
                                 uiOutput('expert_review_consensus')
                               ),
                               
                               # Evidence table
                               wellPanel(
                                 h4(icon("table"), " Evidence Summary"),
                                 uiOutput('expert_review_evidence_table')
                               ),
                               
                               # Key considerations
                               wellPanel(
                                 h4(icon("exclamation-triangle"), " Key Considerations"),
                                 uiOutput('expert_review_considerations')
                               ),
                               
                               # Protein characteristics
                               wellPanel(
                                 h4(icon("dna"), " Protein Characteristics"),
                                 uiOutput('expert_review_characteristics')
                               ),
                               
                               hr(),
                               
                               # Decision section
                               wellPanel(
                                 style = "background-color: #e7f3ff; border: 2px solid #007bff;",
                                 h3(icon("check-circle"), " Approve Design & Generate Construct"),
                                 
                                 fluidRow(
                                   column(6,
                                          h5("Select Vector for Production:"),
                                          uiOutput('expert_review_vector_selection')
                                   ),
                                   column(6,
                                          h5("Expert Notes (optional):"),
                                          textAreaInput('expert_notes', 
                                                        NULL,
                                                        placeholder = "Add any manual override reasoning or special considerations...",
                                                        rows = 4,
                                                        width = "100%")
                                   )
                                 ),
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   column(6,
                                          actionButton('approve_design', 
                                                       'Approve & Generate Construct',
                                                       icon = icon("check-circle"),
                                                       class = "btn-success btn-lg btn-block",
                                                       style = "font-size: 18px;")
                                   ),
                                   column(6,
                                          actionButton('reject_design', 
                                                       'Reject - Needs Further Review',
                                                       icon = icon("times-circle"),
                                                       class = "btn-danger btn-lg btn-block",
                                                       style = "font-size: 18px;")
                                   )
                                 ),
                                 
                                 # Show result after approval
                                 conditionalPanel(
                                   condition = "output.design_approved",
                                   br(),
                                   uiOutput('approved_design_output')
                                 )
                               )
                        )
                      )
             ),
             
             
             
             tabPanel("Raw Output",
                      br(),
                      verbatimTextOutput('decision_full_report')
             )
           )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           h4("Visual Analysis with Epitopes"),
           plotly::plotlyOutput('decision_protein_plot', height = "600px")
    )
  )
)