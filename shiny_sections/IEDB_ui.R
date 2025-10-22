# shiny_sections/IEDB_ui.R

tagList(
  fluidRow(
    column(4,
           wellPanel(
             h4("IEDB Settings"),
             helpText("Search for autoimmune B-cell epitopes (human antibodies against human proteins)"),
             
             numericInput('iedb_max_results', 'Max results per protein',
                          value = 5000, min = 1000, max = 10000, step = 1000),
             
             hr(),
             
             h5("Select Protein:"),
             selectizeInput('iedb_protein_select', 
                            NULL,
                            choices = NULL,
                            multiple = FALSE,
                            options = list(
                              placeholder = 'Search for a protein...',
                              onInitialize = I('function() { this.setValue(""); }')
                            )),
             
             helpText("Note: IEDB searches require UniProt ID")
           )
    ),
    
    column(8,
           wellPanel(
             h4("IEDB Search Controls"),
             p("Search IEDB for autoimmune epitopes on your selected proteins"),
             
             fluidRow(
               column(6,
                      h5(icon("dna"), " Single Protein Search"),
                      actionButton('iedb_search_single', 
                                   'Search IEDB (Selected)', 
                                   class = "btn-info btn-block",
                                   icon = icon("search"))
               ),
               column(6,
                      h5(icon("list"), " Batch Search"),
                      checkboxInput('iedb_skip_searched', 
                                    'Skip already searched proteins', 
                                    value = TRUE),
                      actionButton('iedb_search_batch', 
                                   'Search IEDB (All)', 
                                   class = "btn-primary btn-block",
                                   icon = icon("list"))
               )
             ),
             
             hr(),
             
             fluidRow(
               column(12,
                      actionButton('iedb_clear_results',
                                   'Clear All IEDB Results',
                                   class = "btn-secondary btn-block btn-sm",
                                   icon = icon("trash"))
               )
             )
           )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           uiOutput('iedb_status_ui')
    )
  ),
  
  hr(),
  
  uiOutput('iedb_output_ui')
)