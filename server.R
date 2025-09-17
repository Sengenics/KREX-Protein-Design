# server.R
# Server logic

server <- function(input, output, session) {
  
  # ---- Debug button → browser() ----
  observeEvent(input$debug, {
    browser()
  })
  
  # Current path: uploaded file (if present) else example file
  current_path <- reactive({ 
    upload()
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
    
    (selection = df %>%
      filter(!is.na(!!sym(uniprot_col))) %>% 
      pull(!!sym(uniprot_col)) %>% 
      unique())
    print(length(selection))
    
    db = names(values$uniprot_list)
    print(length(db))
    
    if(input$uniprot_data_selection == 'Full DB'){
      selected = db
    }
    if(input$uniprot_data_selection == 'All'){
      selected = selection
    }
    if(input$uniprot_data_selection == 'Missing'){
      selected = selection[!selection %in% db]
    }
    if(input$uniprot_data_selection == 'Subset'){
      selected = c()
    }
    if(length(selected) < 20){
      values$selected = 'selected'
      selectInput('uniprot_select','Select Proteins',selection,selected,multiple = T)
    }else{
      values$selected = selected
      print(paste(length(selected),'Uniprot IDs'))
    }
  })
  
  uniprot_ids = reactive({ 
    
    if(length(values$selected) == 1){
      uniprot_ids = input$uniprot_select
    }else{
      # df = table_data() 
      # uniprot_col = input$uniprot_column
      # uniprot_ids = df %>% 
      #   pull(!!sym(uniprot_col))
      uniprot_ids = values$selected
    }
    uniprot_ids

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
  
  # search_section <- reactive({
  #   req(input$uniprot_select) 
  #   
  #   uniprot_ids <- input$uniprot_select
  #   selected_fields <- if(exists("selected_fields") && !is.null(selected_fields())) {
  #     selected_fields()
  #   } else {
  #     default_selected_fields
  #   }
  #   
  #   # Use the new simplified function
  #   result_df <- get_uniprot_info_simple(uniprot_ids, selected_fields)
  #   return(result_df)
  # })
  
    selected_fields <- uniprotFieldSelectorServer("field_selector")

    
    values = reactiveValues()
    
    upload = reactive({
      if(file.exists('Data/uniprot_list.rds')){
        values$uniprot_list = readRDS('Data/uniprot_list.rds')
      }else{
        values$uniprot_list = list()
      }
      if(file.exists('Data/feature_list.rds')){
        values$feature_list = readRDS('Data/feature_list.rds')
      }else{
        values$feature_list = list()
      }
      if(file.exists('Data/alpha_list.rds')){
        values$alpha_list = readRDS('Data/alpha_list.rds')
      }else{
        values$alpha_list = list()
      }
      if(file.exists('Data/data_list.rds')){
        values$data_list = readRDS('Data/data_list.rds')
      }else{
        values$data_list = list()
      }
 
    })
    
    observeEvent(input$add_uniprot,{
 
        
      original_df = table_data()  
      
      uniprot_col = input$uniprot_column
      
      (uniprot_ids = original_df %>%
          filter(!is.na(!!sym(uniprot_col))) %>% 
          pull(!!sym(uniprot_col)) %>% 
          unique())
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())  # Ensure progress bar closes even if error occurs
      
      progress$set(message = "Initializing protein analysis...", value = 0)
      total_steps <- length(uniprot_ids)
      
      id_list = list()
      for (i in seq_along(uniprot_ids)) {
        tryCatch({
          (uniprot_id <- uniprot_ids[i])
          # Initialize progress bar
          
          print(uniprot_id)
          print(paste(grep(uniprot_id,uniprot_ids),'of',length(uniprot_ids)))
          
          # Update progress bar
          progress$set(
            message = paste("Processing protein", i, "of", total_steps),
            detail = paste("Current protein:", uniprot_id),
            value = (i - 1) / total_steps
          )
          
          data <- search_uniprot_by_gene(uniprot_id)
          
          uniprot_primary = safe_extract(data$primaryAccession[[1]])
          uniprot_secondary = safe_collapse(data$secondaryAccessions[[1]])
          
          (df = data.frame(search_id = uniprot_id,
                          uniprot_primaryAccession = uniprot_primary,
                          uniprot_secondaryAccession = uniprot_secondary))
          
          id_list[[uniprot_id]] = df
        
        }, error = function(e) {
          # Show error in progress detail instead of console
          progress$set(detail = paste("Error processing", uniprot_id, "- continuing..."))
          Sys.sleep(0.5)  # Brief pause to show error message
        })
      }
      
      full_df = rbindlist(id_list)
      colnames(full_df)[1] = uniprot_col
      
      df = original_df %>% 
        left_join(full_df)
      
      df
      
      values$add_uniprot = df
      
      
    })
    
    output$add_uniprot_ui = renderUI({
      if(!is.null(values$add_uniprot)){
        df = values$add_uniprot
        
        output$add_uniprot_table = renderDataTable({
          df
        })
        
        output$download_add_uniprot_xlsx <- downloadHandler(
          filename = function() {
            paste0("add_uniprot_data_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            data <- df
            if (!is.null(data) && nrow(data) > 0) {
              write.xlsx(data, file, rowNames = FALSE)
            } else {
              write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
            }
          }
        )
        
        lst = list(
          downloadButton("download_add_uniprot_xlsx", "Excel", class = "btn-sm btn-outline-success"),
          dataTableOutput('add_uniprot_table')
        )
        do.call(tagList,lst)
      }
    })
    
    observeEvent(input$search, {       
      #req(input$uniprot_select)       
      #uniprot_ids <- input$uniprot_select
       
      (uniprot_ids_all = uniprot_ids())
     
      
      
      uniprot_list = values$uniprot_list
      if(input$uniprot_rerun == F){
       (uniprot_ids = uniprot_ids_all[!uniprot_ids_all %in% names(uniprot_list)])
      }else{
        uniprot_ids = uniprot_ids_all
      }
      
      # Initialize progress bar
      progress <- shiny::Progress$new()
      on.exit(progress$close())  # Ensure progress bar closes even if error occurs
      
      progress$set(message = "Initializing protein analysis...", value = 0)
      total_steps <- length(uniprot_ids)
      
    
      if(length(uniprot_ids) > 0){
        test = F
        if(test == T){
          uniprot_ids = sample(uniprot_ids,30)
        }
        uniprot_id = uniprot_ids[1]
       # for(uniprot_id in uniprot_ids){
          
        for (i in seq_along(uniprot_ids)) {
          uniprot_id <- uniprot_ids[i]
          
        tryCatch({
            if('Uniprot' %in% input$database_search){
              print(uniprot_id)
              print(paste(grep(uniprot_id,uniprot_ids),'of',length(uniprot_ids)))
              
              # Update progress bar
              progress$set(
                message = paste("Processing protein", i, "of", total_steps),
                detail = paste("Current protein:", uniprot_id),
                value = (i - 1) / total_steps
              )
              
              data <- get_uniprot_info_data(uniprot_id)
              
              values$data_list[[uniprot_id]] = data
              saveRDS(values$data_list,'Data/data_list.rds')
              
              uniprot_features = data.frame(
                type = data$features$type,
                start = data$features$location$start$value,
                start_m = data$features$location$start$modifier,
                end = data$features$location$end$value,
                end_m = data$features$location$end$modifier
              ) %>% 
                mutate(start_distance = start - 1,
                       end_distance = max(end) - end,
                       length = end - start) %>% 
                mutate(uniprot_id = uniprot_id) %>% 
                dplyr::select(uniprot_id,everything())
              
              values$feature_list[[uniprot_id]] = uniprot_features
              
              saveRDS(values$feature_list,'Data/feature_list.rds')
              # if(is.null(feature_df)){
              #   feature_df = uniprot_features
              # }else{
              #   feature_df = rbind(feature_df,uniprot_features)
              # }
              result_df = uniprot_data_parse(data) %>% 
                mutate(uniprot_id = uniprot_id) %>% 
                dplyr::select(uniprot_id,everything())
              
              values$uniprot_list[[uniprot_id]] = result_df
              saveRDS(values$uniprot_list,'Data/uniprot_list.rds')
              
              # if(is.null(uniprot_df)){
              #   uniprot_df = result_df
              # }else{
              #   uniprot_df = rbind(uniprot_df,result_df)
              # }
            }
            
            if('AlphaFold' %in% input$database_search){
              #a_prediction = get_alphafold_batch(uniprot_id) 
              a_prediction = parse_alphafold_data(get_alphafold_batch(uniprot_id)) %>% 
                mutate(uniprot_id = uniprot_id) %>% 
                dplyr::select(uniprot_id,everything())
              
              #a_prediction = uniprot_data_parse(data)
              # if(is.null(alpha_df)){
              #   alpha_df = a_prediction
              # }else{
              #   alpha_df = rbind(alpha_df,a_prediction)
              # }
              values$alpha_list[[uniprot_id]] = a_prediction
              saveRDS(values$alpha_list,'Data/alpha_list.rds')
            }
            if('AlphaFold PDB' %in% input$database_search){
              alpha_fold_structure = download_alphafold_structure(uniprot_id)
            }
            
          }, error = function(e) {
            # Show error in progress detail instead of console
            progress$set(detail = paste("Error processing", uniprot_id, "- continuing..."))
            Sys.sleep(0.5)  # Brief pause to show error message
          })
        }
        progress$set(message = "Finalizing results...", value = 1, detail = "Almost done!")
      }else{
        showNotification(
          "No new uniprot ids",
          type = "warning",
          duration = 3  # Duration in seconds
        )
      }
    })
    
    consolidate_data = reactive({   
      
      feature_list = values$feature_list
      uniprot_list = values$uniprot_list
      alpha_list = values$alpha_list
      
      library('data.table')
      feature_df = rbindlist(feature_list,fill = T) %>% 
        filter(uniprot_id %in% uniprot_ids())
      uniprot_df = rbindlist(uniprot_list,fill = T) %>% 
        filter(uniprot_id %in% uniprot_ids())
      alpha_df = rbindlist(alpha_list,fill = T) %>% 
        filter(uniprot_id %in% uniprot_ids())
      
        
        output$uniprot_features <- DT::renderDataTable({
          DT::datatable(feature_df, options = list(scrollX = TRUE))
        })
        #result_df = uniprot_data_parse(data)
        #a_prediction = get_alphafold_batch(uniprot_id)
        truncated_result_df = truncate_long_text(uniprot_df,30)
        output$uniprot_results <- DT::renderDataTable({
          DT::datatable(truncated_result_df, options = list(scrollX = TRUE))
        })
        #a_prediction = parse_alphafold_data(get_alphafold_batch(uniprot_id))
        truncated_alpha_df = truncate_long_text(alpha_df,50)
        output$alphafold_results <- DT::renderDataTable({
          DT::datatable(truncated_alpha_df, options = list(scrollX = TRUE))
        })
        
        
        output$download_uniprot_xlsx <- downloadHandler(
          filename = function() {
            paste0("uniprot_data_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            data <- uniprot_df
            if (!is.null(data) && nrow(data) > 0) {
              write.xlsx(data, file, rowNames = FALSE)
            } else {
              write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
            }
          }
        )
        
        output$download_features_xlsx <- downloadHandler(
          filename = function() {
            paste0("uniprot_features_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            data <- feature_df
            if (!is.null(data) && nrow(data) > 0) {
              write.xlsx(data, file, rowNames = FALSE)
            } else {
              write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
            }
          }
        )
        
        output$download_alphafold_xlsx <- downloadHandler(
          filename = function() {
            paste0("alphafold_data_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            data <- alpha_df
            if (!is.null(data) && nrow(data) > 0) {
              write.xlsx(data, file, rowNames = FALSE)
            } else {
              write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
            }
          }
        )
        
      #   #alpha_fold_structure = download_alphafold_structure(uniprot_id)
      # list(data_list = data_list,
      #      uniprot_df = uniprot_df,
      #      alpha_df = alpha_df,
      #      feature_df = feature_df)
    })
    
    output$result_output_ui = renderUI({
      consolidate_data()
      
      lst = list(
        tabsetPanel(
          tabPanel("Description",
            tags$h3('Uniprot'),
            downloadButton("download_uniprot_xlsx", "Excel", class = "btn-sm btn-outline-success"),
            DT::dataTableOutput('uniprot_results'),
            tags$h3("Alpha Fold"),
            downloadButton("download_alphafold_xlsx", "Excel", class = "btn-sm btn-outline-success"),
            DT::dataTableOutput('alphafold_results')
          ),
          tabPanel('Features',
                   downloadButton("download_features_xlsx", "Excel", class = "btn-sm btn-outline-success"),
                   DT::dataTableOutput('uniprot_features')      
                   )
        )
      )
      
      do.call(tagList,lst)
      
      
    })
  
    uniprot_fields <- uniprotFieldsManagerServer("fields_manager")
  
}


