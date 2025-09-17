# ui.R
# User interface

ui <- fluidPage(
  
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
      uiOutput("sheetPicker"),
      uiOutput('uniprot_col_ui'),
      hr(),
      
      strong("Active file:"),
      verbatimTextOutput("filePath", placeholder = TRUE),
      helpText("If no file is uploaded, the app uses: Data/Protein Design Example.xlsx (sheet 'Proteins' or sheet 3).")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Input',
                 h4("Proteins table"),
                 DTOutput("proteinTable")
        ),
        # tabPanel('Uniprot',
        #          uniprotFieldsManagerUI("fields_manager")
        #          ),
        tabPanel("Search", 
                 radioButtons('uniprot_data_selection','Data',c("Full DB",'All',"Missing",'Subset'),'Missing',inline = T),
                 uiOutput('uniprot_select_ui'),
                 #uniprotFieldSelectorUI("field_selector"),
                 #hr(),
                 #textInput("uniprot_id", "UniProt ID:", value = "P04637"),
                
                 selectInput('database_search','Databases',c('Uniprot','AlphaFold','AlphaFold PDB'),'Uniprot',multiple = T),
                 radioButtons('uniprot_rerun','Rerun',c(F,T),F,inline = T),
                 actionButton("search", "Search"),
                 hr(),
                 uiOutput('result_output_ui')
                
      )
    
    )
  )
)
)
