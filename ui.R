# ui.R
# User interface

ui <- fluidPage(
  titlePanel("Protein Design — Upload & Preview"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("debug", "Browser"),
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
        tabPanel("Search",
                 uiOutput('uniprot_select_ui'),
                 uniprotFieldSelectorUI("field_selector"),
                 hr(),
                 textInput("uniprot_id", "UniProt ID:", value = "P04637"),
                 actionButton("search", "Search"),
                 hr(),
                 DT::dataTableOutput("results"),
                 dataTableOutput('uniprot_results'))
      )
    
    )
  )
)
