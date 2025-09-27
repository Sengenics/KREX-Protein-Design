# ui.R
# User interface

OpenAI_ui = source('shiny_sections/OpenAI_ui.R',local = T)$value

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://3Dmol.csb.pitt.edu/build/3Dmol-min.js")
  ),
  tags$head(
    tags$style(HTML("
    .dataTables_wrapper td {
      vertical-align: top;
      padding: 8px !important;
    }
    
    /* Hover effect to show full content */
    td span[title]:hover {
      position: relative;
      background-color: #fff3cd;
    }
    
    /* Responsive design */
    @media (max-width: 768px) {
      .dataTables_wrapper td {
        max-width: 150px !important;
        font-size: 12px;
      }
    }
  "))
  ),
  titlePanel("Protein Design — Upload & Preview"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("debug", "Debug"),
      hr(),
      
      h5("Data source"),
      fileInput(
        "excel",
        "Upload Excel file (.xlsx or .xls)",
        accept = c(".xlsx", ".xls")
      ),
      strong("Active file:"),
      verbatimTextOutput("filePath", placeholder = TRUE),
      uiOutput("sheetPicker"),
      numericInput('row_number','Start Row',0),
      uiOutput('uniprot_col_ui'),
      hr(),
      numericInput('c_term_buffer','C-term Buffer',30),
      numericInput('n_term_buffer','N-term Buffer',30)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Input',
                 tabsetPanel(
                   tabPanel('Input Table',
                     
                     h4("Proteins table"),
                     DTOutput("proteinTable")
                   ),
      
                  tabPanel('Add Uniprot ID',
                           tags$h5('Please select a gene symbol column as input in the Uniprot Column menu, then run Add Uniprot. The app will search the uniprot db for uniprot entries matching the gene symbol. 
                                   Save this file and import it as the new input file.'),
                           actionButton("add_uniprot", "Add Uniprot"),
                           uiOutput('add_uniprot_ui'))
        )),
        # tabPanel('Uniprot',
        #          uniprotFieldsManagerUI("fields_manager")
        #          ),
        tabPanel("Search Uniprot", 
                 
                 
                 radioButtons('uniprot_data_selection','Data',c("Full DB",'All',"Missing",'Subset'),'All',inline = T),
                 uiOutput('uniprot_select_ui'),
                 tabsetPanel(
                   tabPanel('Uniprot',
                     #uniprotFieldSelectorUI("field_selector"),
                     #hr(),
                     #textInput("uniprot_id", "UniProt ID:", value = "P04637"),
                    
                     selectInput('database_search','Databases',c('Uniprot','AlphaFold','AlphaFold PDB'),'Uniprot',multiple = T),
                     radioButtons('uniprot_rerun','Rerun',c(F,T),F,inline = T),
                    
                     actionButton("search", "Search"),
                     hr(),
                     uiOutput('result_output_ui')
                   ),
                   tabPanel("Open AI",
                            OpenAI_ui
                            # actionButton('openai_search','Search'),
                            # uiOutput('openai_input_ui'),
                            # uiOutput('openai_output_ui')
                            )
                 )
                
      ),

      tabPanel('Filter',
      # In your tabPanel("Search", ...)
        dynamicFilterUI("protein_filter"),
        featureExclusionUI("feature_filter"),
        uiOutput('dynamic_filter_input_ui'),
        uiOutput('dynamic_filter_output_ui')
      ),
      tabPanel('Uniprot Features',
               proteinReportUI("protein_report"),
               uiOutput('uniprot_features_input_ui'),
               uiOutput('uniprot_features_output_ui')),
      tabPanel("AlphaFold Structure",
               fluidRow(
                 column(4, uiOutput("alphafold_info")),
                 column(8, plotlyOutput("alphafold_plot", height = "700px"))
               )
      ),

    
    )
  )
)
)
