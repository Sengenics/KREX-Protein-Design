# shiny_sections/OpenAI_ui.R
# Complete OpenAI Analysis UI - Literature Search for Epitopes & Expression

tagList(
  fluidRow(
    column(12,
           h3(icon("brain"), "OpenAI Literature Analysis"),
           p("Search scientific literature for epitope data and expression protocols using AI")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(4,
           wellPanel(
             h4(icon("cog"), " Settings"),
             
             selectInput('openai_model', 'AI Model',
                         choices = c("GPT-5 Mini (Recommended)" = "gpt-5-mini",
                                     "GPT-5 (More Expensive)" = "gpt-5",
                                     "GPT-4o Mini" = "gpt-4o-mini",
                                     "GPT-4o" = "gpt-4o"),
                         selected = "gpt-5"),
             
             numericInput('openai_delay', 
                          'Delay between API requests (seconds)', 
                          value = 2, min = 1, max = 10, step = 1),
             
             hr(),
             
             h5(icon("flask"), " Select Protein:"),
             selectizeInput('openai_protein_select', 
                            NULL,
                            choices = NULL,
                            multiple = FALSE,
                            options = list(
                              placeholder = 'Select a protein...',
                              onInitialize = I('function() { this.setValue(""); }')
                            )),
             
             tags$small(
               style = "color: #666;",
               "Select a protein to run individual searches"
             )
           )
    ),
    
    column(8,
           wellPanel(
             h4(icon("play-circle"), " Analysis Controls"),
             
             # EPITOPE SEARCH
             fluidRow(
               column(12,
                      h5(icon("bullseye"), " Epitope Search", 
                         style = "color: #007bff;"),
                      tags$p(
                        style = "font-size: 0.9em; color: #666;",
                        "Search literature for autoimmune epitopes and antibody binding sites"
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      checkboxInput('openai_epitope_skip_searched', 
                                    'Skip already searched proteins', 
                                    value = TRUE)
               ),
               column(6,
                      tags$div(
                        style = "text-align: right;",
                        tags$small(
                          style = "color: #666;",
                          "Estimated: ~520 tokens/protein"
                        )
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      actionButton('openai_epitope_single', 
                                   'Search Selected Protein', 
                                   class = "btn-info btn-block",
                                   icon = icon("search"))
               ),
               column(6,
                      actionButton('openai_epitope_batch', 
                                   'Search All Proteins', 
                                   class = "btn-primary btn-block",
                                   icon = icon("list"))
               )
             ),
             
             hr(),
             
             # EXPRESSION SEARCH
             fluidRow(
               column(12,
                      h5(icon("flask"), " Expression Search", 
                         style = "color: #28a745;"),
                      tags$p(
                        style = "font-size: 0.9em; color: #666;",
                        "Search literature for recombinant expression protocols and success reports"
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      checkboxInput('openai_expression_skip_searched', 
                                    'Skip already searched proteins', 
                                    value = TRUE)
               ),
               column(6,
                      tags$div(
                        style = "text-align: right;",
                        tags$small(
                          style = "color: #666;",
                          "Estimated: ~670 tokens/protein"
                        )
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      actionButton('openai_expression_single', 
                                   'Search Selected Protein', 
                                   class = "btn-info btn-block",
                                   icon = icon("search"))
               ),
               column(6,
                      actionButton('openai_expression_batch', 
                                   'Search All Proteins', 
                                   class = "btn-success btn-block",
                                   icon = icon("list"))
               )
             ),
             
             hr(),
             
             # COMBINED SEARCH
             fluidRow(
               column(12,
                      h5(icon("rocket"), " Run Both Analyses", 
                         style = "color: #fd7e14;"),
                      tags$p(
                        style = "font-size: 0.9em; color: #666;",
                        "Run epitope and expression searches together"
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      checkboxInput('openai_both_skip_searched', 
                                    'Skip already searched proteins', 
                                    value = TRUE)
               ),
               column(6,
                      tags$div(
                        style = "text-align: right;",
                        tags$small(
                          style = "color: #666;",
                          "Estimated: ~1,190 tokens/protein"
                        )
                      )
               )
             ),
             
             fluidRow(
               column(6,
                      actionButton('openai_both_single', 
                                   'Run Both (Selected)', 
                                   class = "btn-warning btn-block",
                                   icon = icon("play"))
               ),
               column(6,
                      actionButton('openai_both_batch', 
                                   'Run Both (All)', 
                                   class = "btn-warning btn-block",
                                   icon = icon("forward"))
               )
             ),
             
             hr(),
             
             # CLEAR DATA
             fluidRow(
               column(12,
                      h5(icon("trash"), " Data Management", 
                         style = "color: #dc3545;"),
                      tags$p(
                        style = "font-size: 0.9em; color: #666;",
                        "Remove all OpenAI search results to start fresh"
                      ),
                      actionButton('openai_clear_all', 
                                   'Clear All OpenAI Data', 
                                   class = "btn-danger btn-block",
                                   icon = icon("trash-alt"))
               )
             )
           )
    )
  ),
  
  hr(),
  
  # STATUS SECTION
  fluidRow(
    column(12,
           uiOutput('openai_status_ui')
    )
  ),
  
  hr(),
  
  # RESULTS SECTION
  fluidRow(
    column(12,
           uiOutput('openai_output_ui')
    )
  )
)