# server.R
# Server logic

server <- function(input, output, session) {
  
  # ---- Debug button → browser() ----
  observeEvent(input$debug, {
    browser()
  })
  
  # Current path: uploaded file (if present) else example file
  current_path <- reactive({
    if (!is.null(input$excel) && nzchar(input$excel$datapath)) {
      input$excel$datapath
    } else {
      example_file
    }
  })
  
  # List available sheets for the current file
  sheets <- reactive({
    excel_sheets_safe(current_path())
  })
  
  # Initialize/update sheet picker whenever file changes
  observe({
    sh <- sheets()
    # Default: "Proteins" (case-insensitive), else 3rd, else 1st
    default_sheet <- pick_default_sheet(sh)
    # Build UI (or message if no sheets)
    if (is.null(sh)) {
      output$sheetPicker <- renderUI({
        tags$div(style = "color:#a94442;", "⚠️ Unable to read sheets from the selected file.")
      })
    } else {
      output$sheetPicker <- renderUI({
        selectInput("sheet", "Sheet", choices = sh, selected = default_sheet)
      })
      # If there is no input$sheet yet, set it to default
      if (is.null(input$sheet) && !is.null(default_sheet)) {
        updateSelectInput(session, "sheet", selected = default_sheet)
      }
    }
  })
  
  # Read the chosen sheet (reactive)
  protein_data <- reactive({
    req(current_path())
    # If user hasn't picked yet, choose default for current file
    sheet_to_use <- if (!is.null(input$sheet) && nzchar(input$sheet)) {
      input$sheet
    } else {
      pick_default_sheet(sheets())
    }
    df <- read_excel_safe(current_path(), sheet_to_use)
   # validate(need(!is.null(df), "Could not read the selected sheet from the file."))
    df %>% as.data.frame()
  })
  
  # Detect UniProt column for the current data
  output$uniprot_col_ui <- renderUI({
    df = protein_data() %>% 
      as.data.frame()
    selection = colnames(df)
    selected = grep('uniprot',selection,value = T,ignore.case = T)
    selectInput('uniprot_column','Uniprot Column',selection,selected[1])
  })
  
  # Show path to the active file
  output$filePath <- renderText({
    # Prefer the original filename for uploads; else show the example path
    if (!is.null(input$excel) && nzchar(input$excel$name)) {
      input$excel$name
    } else {
      example_file
    }
  })
  
  # Show UniProt detection status
  output$uniprotColStatus <- renderUI({
    col <- uniprot_col()
    if (is.na(col)) {
      tags$div(style = "color:#a94442;",
               "⚠️ Could not auto-detect a UniProt ID column.")
    } else {
      tags$div(
        "Detected UniProt column: ",
        tags$strong(col)
      )
    }
  })
  
  # Data to show (either full sheet or only UniProt column if available)
  table_data <- reactive({ 
    df <- protein_data()
    col <- input$uniprot_column
    if (isTRUE(input$show_only_uniprot) && !is.na(col) && col %in% names(df)) {
      df %>% select(all_of(col))
    } else {
      df
    }
  })
  
  # Render table
  output$proteinTable <- renderDT({
    datatable(
      table_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$uniprot_select_ui = renderUI({
    df = table_data() 
    uniprot_col = input$uniprot_column
    selection = df %>% 
      pull(!!sym(uniprot_col))
    selectInput('uniprot_select','Select Proteins',selection,selection[1:2],multiple = T)
  })
  
  search_section = reactive({
    
    uniprot_ids = input$uniprot_select
    uniprot_id = uniprot_ids[1]
    df = get_uniprot_info(uniprot_id) 
    df
  })
  
  
  output$uniprot_results = renderDataTable({
    as.data.frame(search_section())
  })
  
    selected_fields <- uniprotFieldSelectorServer("field_selector")

    observeEvent(input$search, {
      result <- get_uniprot_info_enhanced(input$uniprot_id, selected_fields())
      output$results <- DT::renderDataTable({
        DT::datatable(result, options = list(scrollX = TRUE))
      })
    })
  
  
  
}
