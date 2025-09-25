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
    excel_sheets(current_path())
  })
  
  output$sheetPicker <- renderUI({
    #excel_sheets_safe(current_path())
    sheets = excel_sheets(current_path())
    selectInput("sheet", "Sheet", choices = sheets)
  })
  
  # Initialize/update sheet picker whenever file changes
  # observe({
  #   sh <- sheets()
  #   # Default: "Proteins" (case-insensitive), else 3rd, else 1st
  #   default_sheet <- pick_default_sheet(sh)
  #   # Build UI (or message if no sheets)
  #   if (is.null(sh)) {
  #     output$sheetPicker <- renderUI({
  #       tags$div(style = "color:#a94442;", "⚠️ Unable to read sheets from the selected file.")
  #     })
  #   } else {
  #     output$sheetPicker <- renderUI({
  #       selectInput("sheet", "Sheet", choices = sh, selected = default_sheet)
  #     })
  #     # If there is no input$sheet yet, set it to default
  #     if (is.null(input$sheet) && !is.null(default_sheet)) {
  #       updateSelectInput(session, "sheet", selected = default_sheet)
  #     }
  #   }
  # })
  
  # Read the chosen sheet (reactive)
  protein_data_o = reactive({
    read_excel_safe(current_path(), sheet = 1)
  })
  
  protein_data <- reactive({
    req(current_path())
    # If user hasn't picked yet, choose default for current file
    sheet_to_use <- if (!is.null(input$sheet) && nzchar(input$sheet)) {
      input$sheet
    } else {
      pick_default_sheet(sheets())
    }
    #df <- read_excel_safe(current_path(), sheet_to_use,)
    df = read_excel(current_path(),sheet = sheet_to_use,skip = input$row_number)
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

    # Uniprot #####
    ## ReactiveValues ####
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
      
      if(file.exists('Data/impact_issues.rds')){
        values$impact_issues = readRDS('Data/impact_issues.rds')
      }else{
        values$impact_issues = list()
      }
      
      if(file.exists('Data/openai.rds')){
        values$openai= readRDS('Data/openai.rds')
      }else{
        values$openai = list()
      }
 
    })
    
    
    # Add Uniprot ####
    
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
      
      test = F
      if(test == T){
        uniprot_ids = sample(uniprot_ids,5)
        i = 1
      }
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
          #data$genes$geneName$value
          #data$genes[[2]]$geneName$value
          # uniprot_primary = safe_extract(data$primaryAccession[[1]])
          # uniprot_secondary = safe_collapse(data$secondaryAccessions[[1]])
          # gene_primary = safe_extract(data$genes[[1]]$geneName$value)
          
          
          (df = data.frame(search_id = uniprot_id,
                          uniprot_primaryAccession = safe_extract(data$primaryAccession),
                          uniprot_secondaryAccession = safe_collapse(data$secondaryAccessions),
                          geneNames_primary = safe_extract(data$genes$geneName$value)
                          ))
          
          id_list[[uniprot_id]] = df
        
        }, error = function(e) {
          # Show error in progress detail instead of console
          progress$set(detail = paste("Error processing", uniprot_id, "- continuing..."))
          Sys.sleep(0.5)  # Brief pause to show error message
        })
      }
      
      full_df = rbindlist(id_list) %>% 
        mutate(test = ifelse(search_id == geneNames_primary,T,F))
      colnames(full_df)[1] = uniprot_col
      
      df = original_df %>% 
        left_join(full_df)
      
      df
      values$add_uniprot = full_df
      values$add_uniprot_original = df
      
      
    })
    
    output$add_uniprot_ui = renderUI({
      if(!is.null(values$add_uniprot)){
        df = values$add_uniprot
        
        output$add_uniprot_original_table = renderDataTable({
          values$add_uniprot_original
        })
        
        output$download_add_uniprot_original_xlsx <- downloadHandler(
          filename = function() {
            paste0("add_uniprot_data_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            data <- values$add_uniprot_original
            if (!is.null(data) && nrow(data) > 0) {
              write.xlsx(data, file, rowNames = FALSE)
            } else {
              write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
            }
          }
        )
        
        output$add_uniprot_table = renderDataTable({
          values$add_uniprot
        })
        
        output$add_uniprot_mismatch_table = renderDataTable({
          values$add_uniprot %>% 
            filter(test == FALSE)
        })
        
        output$download_add_uniprot_xlsx <- downloadHandler(
          filename = function() {
            paste0("add_uniprot_data_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            data <- values$add_uniprot
            if (!is.null(data) && nrow(data) > 0) {
              write.xlsx(data, file, rowNames = FALSE)
            } else {
              write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
            }
          }
        )
        
        lst = list(
         
          tabsetPanel(
            tabPanel('Add Uniprot',
                     tags$h4('Mismatch'),
                     dataTableOutput('add_uniprot_mismatch_table'),
                     tags$h4('Full'),
                     downloadButton("download_add_uniprot_xlsx", "Excel", class = "btn-sm btn-outline-success"),
                     dataTableOutput('add_uniprot_table')),
            tabPanel('Add to Original',
                     downloadButton("download_add_uniprot_original_xlsx", "Excel", class = "btn-sm btn-outline-success"),
                     dataTableOutput('add_uniprot_original_table')
                     )
          )
         
        )
        do.call(tagList,lst)
      }
    })
    
    # Search Uniprot ####
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
        (uniprot_id = uniprot_ids[1])
       # for(uniprot_id in uniprot_ids){
        i = 1
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
              
              
              if(uniprot_id %in% names(values$data_list)){
                data = values$data_list[[uniprot_id]]
              }else{
                data <- get_uniprot_info_data(uniprot_id)
                values$data_list[[uniprot_id]] = data
              }
              
              #analyze_current_for_homo_multimer(data)
              #multimer = get_simple_multimer_status(data)
              #saveRDS(values$data_list,'Data/data_list.rds')
              
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
              
              #subunit_text <- extract_subunit_structure_corrected(data)
              #multimer_status <- determine_multimeric_status_refined(subunit_text)
              
              impact_analysis <- analyze_terminal_tag_impact(uniprot_features, uniprot_id = uniprot_id,
                                                             c_term_buffer = input$c_term_buffer,
                                                             n_term_buffer = input$n_term_buffer)
              impact_df = as.data.frame(impact_analysis[names(impact_analysis) != 'summary'])
              names(impact_analysis)
              
              impact_issues_df = impact_analysis$c_terminal_issues %>% 
                mutate('uniprot_id' = uniprot_id,
                       'Terminal' = 'C-terminal',
                       'buffer' = input$c_term_buffer) %>% 
                rbind(impact_analysis$n_terminal_issues %>% 
                            mutate('uniprot_id' = uniprot_id,
                                   'Terminal' = 'N-terminal',
                                   'buffer' = input$n_term_buffer)) %>% 
                dplyr::select(uniprot_id,Terminal,everything())
              
              values$impact_issues[[uniprot_id]] = impact_issues_df
              saveRDS(values$impact_issues,'Data/impact_issues.rds')
                
              uniprot_df_ind = uniprot_data_parse(data) %>%
                mutate('SUBCELLULAR LOCATION' = extract_subcellular_location_df(data)) %>% 
                mutate('Secreted' = determine_secretion_from_data(data)) %>%
                mutate('SubUnits' = extract_subunit_structure_corrected(data)) %>% 
                mutate('Multimeric' = get_simple_multimer_status(data)) %>% 
                mutate('c_term_buffer' = input$c_term_buffer,
                       #'c_terminal_issues' = impact_analysis$c_terminal_issues,
                       'c_terminal_score' = impact_analysis$c_terminal_score,
                       'n_term_buffer' = input$n_term_buffer,
                       #'n_terminal_issues' = impact_analysis$n_terminal_issues,
                       'n_terminal_score' = impact_analysis$n_terminal_score,
                       'preferred_terminus' = impact_analysis$preferred_terminus,
                       'recommendation' = impact_analysis$recommendation
                       ) %>% 
                mutate(uniprot_id = uniprot_id) %>% 
                dplyr::select(uniprot_id,everything())
            
              
              values$uniprot_list[[uniprot_id]] = uniprot_df_ind
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
      
      #library('data.table')
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
        
        output$download_uniprot_tsv <- downloadHandler(
          filename = function() {
            paste0("uniprot_data_", Sys.Date(), ".tsv")
          },
          content = function(file) {
            data <- uniprot_df
            if (!is.null(data) && nrow(data) > 0) {
              data.table::fwrite(data, file,sep = '\t')
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
      list(#data_list = data_list,
           uniprot_df = uniprot_df,
           alpha_df = alpha_df,
           feature_df = feature_df)
    })
    
    ## Sequences #####
    
    sequences = reactive({ 
      uniprot_df = consolidate_data()$uniprot_df
      feature_df = consolidate_data()$feature_df
      
      colnames(uniprot_df)
      sequence_df = uniprot_df %>% 
        dplyr::select(one_of(c('uniprot_id','primaryAccession','protein_sequence')))
      
      colnames(feature_df)
      signal_df = feature_df %>% 
        filter(type == "Signal") %>% 
        dplyr::select(one_of('uniprot_id','type','start','end','length')) 
        #renames(Signal = type) %>% 
        
      
      unique(signal_df$Signal)
      
      sequence_signal_df = sequence_df %>% 
        left_join(signal_df) %>% 
        mutate(Signal = ifelse(type == 'Signal','TRUE','FALSE')) %>% 
        mutate(Signal = ifelse(is.na(Signal),'FALSE',Signal))
      unique(sequence_signal_df$Signal)
      vector
      colnames(vector)
      unique(vector$Vector)
      (pR030A = vector %>% 
        filter(Vector == 'pPRO30A') %>% 
        pull(tag) 
      )
      pR030A = gsub("[^\x01-\x7F]", "", pR030A)
      
      (pPRO30A_SP = vector %>% 
          filter(Vector == 'pPRO30A-SP​') %>% 
        pull(tag))
      pPRO30A_SP = gsub("[^\x01-\x7F]", "", pPRO30A_SP)
      
      (pPRO8 = vector %>% 
          filter(Vector == 'pPRO8') %>% 
          pull(tag))
      pPRO8 = gsub("[^\x01-\x7F]", "", pPRO8) 
      
      # 
      # sequence_signal_add = sequence_signal_df %>% 
      #   mutate(signal_protein = ifelse(protein_sequence[1] == 'M',protein_sequence[2:],protein_sequence)) %>% 
      #   mutate(pR030A = paste0(pR030A,protein_sequence)) %>% 
      #   mutate(pPRO30A_SP = paste0(pPRO30A_SP,protein_sequence)) %>% 
      #   mutate(pPRO8 = paste0(protein_sequence,pPRO8))
      
      sequence_signal_add <- sequence_signal_df %>% 
        mutate(protein_sequence = gsub("[^\x01-\x7F]", "", trimws(protein_sequence))) %>% 
        mutate(signal_protein = ifelse(substr(protein_sequence, 1, 1) == 'M',
                                       substr(protein_sequence, 2, nchar(protein_sequence)), 
                                       protein_sequence)) %>% 
        mutate(pR030A = paste0(trimws(pR030A), protein_sequence)) %>%  # Use signal_protein here
        mutate(pPRO30A_SP = paste0(trimws(pPRO30A_SP), signal_protein)) %>% 
        mutate(pPRO8 = paste0(protein_sequence, trimws(pPRO8)))
      
      #View(sequence_signal_add)
      #truncate_long_text
      output$sequences_table <- DT::renderDataTable({
        DT::datatable(truncate_long_text(sequence_signal_add,150), options = list(scrollX = TRUE))
      })
      
      output$download_sequences_xlsx <- downloadHandler(
        filename = function() {
          paste0("sequences_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          data <- sequence_signal_add
          if (!is.null(data) && nrow(data) > 0) {
            write.xlsx(data, file, rowNames = FALSE)
          } else {
            write.xlsx(data.frame(Message = "No data available"), file, rowNames = FALSE)
          }
        }
      )
      
      output$download_sequences_tsv <- downloadHandler(
        filename = function() {
          paste0("sequences_", Sys.Date(), ".tsv")
        },
        content = function(file) {
          data <- sequence_signal_add
          if (!is.null(data) && nrow(data) > 0) {
            data.table::fwrite(data, file, sep = '\t')
          }
        }
      )
      
      
      sequence_signal_add
  })
    
    
    ### Features ####
    
  output$uniprot_features_input_ui = renderUI({
    
    #uniprot_ids = unique(consolidate_data()$feature_df$uniprot_id)
    #final_proteins()
    (uniprot_ids = unique(final_proteins()$uniprot_id))
    selectInput('individual_uniprots','Single Protein',uniprot_ids)
  })
    
  # features = reactive({
  #   feature_df = consolidate_data()$feature_df
  #   
  #   uniprot_ids = unique(feature_df$uniprot_id)
  #   uniprot_id = uniprot_ids[1]
  #   uniprot_id = input$individual_uniprots
  #   
  #   plots <- visualize_protein(feature_df, uniprot_id)
  #    # plots$feature_map  # Interactive feature map
  #    # plots$domain_arch  # Domain architecture
  #    # plots$summary_table  # Summary table
  #   
  #   # # For all proteins
  #    feature_distribution <- plot_feature_distribution(feature_df)
  #   # 
  #   # # Just the feature map
  #   feature_plot <- plot_protein_features(feature_df, uniprot_id)
  #   feature_plot
  #   
  #   output$features_feature_map = renderPlotly({
  #     plots$feature_map
  #   })
  #   output$features_domain_arch = renderPlotly({
  #     plots$domain_arch
  #   })
  #   
  #   output$features_summary_table = renderDataTable({
  #     plots$summary_table
  #   })
  # 
  # })
  
  observe({ 
    req(input$individual_uniprots)   #  Wait for input to be available
    
    
    feature_df = consolidate_data()$feature_df
    uniprot_id = input$individual_uniprots
    ind_uniprot_id = uniprot_id
    ind_feature_df = feature_df %>% 
      dplyr::filter(uniprot_id == ind_uniprot_id)
    
    output$sequence_text = renderText({
      sequence = sequences() %>% filter(uniprot_id == ind_uniprot_id) %>% 
        pull(protein_sequence)
      sequence
    })
    # Create the plots
    plots <- visualize_protein(ind_feature_df, uniprot_id)
    
    # Replace your current plotting call with:
    # plots = plot_protein_features_with_impact(feature_df, uniprot_id = "P08514",
    #                                   show_impact = TRUE)
    # 
    # # Generate impact report:
    # impact_report <- generate_impact_report(feature_df, uniprot_id = "P08514")
    # print(impact_report$summary)
    
    # Render the outputs
    output$features_feature_map = renderPlotly({
      plots$feature_map
    })
    
    output$features_domain_arch = renderPlotly({
      plots$domain_arch
    })
    
    output$features_summary_table = renderDataTable({
      plots$summary_table
    })
    
    output$features_df_individual_table = renderDataTable({
      
      feature_df %>% dplyr::filter(uniprot_id == ind_uniprot_id)
    })
  })
  
  # output$uniprot_features_output_ui = renderUI({
  #   features()
  #   lst = list(
  #     plotlyOutput('features_feature_map'),
  #     plotlyOutput('features_domain_arch'),
  #     dataTableOutput('features_summary_table')
  #   )
  # })
  
  output$uniprot_features_output_ui = renderUI({
    req(input$individual_uniprots)  # Only show when protein is selected
    
    tagList(
      h4(paste("Features for", input$individual_uniprots)),
      tabsetPanel(
        
        tabPanel('Feature Map',
          h5("Interactive Feature Map"),
          plotlyOutput('features_feature_map', height = "400px")
        ),
        tabPanel('Domain Architecture',
            
          h5("Domain Architecture"),
          plotlyOutput('features_domain_arch', height = "300px")
        ),
        tabPanel("Summary",
          br(),
          h5("Feature Summary"),
          dataTableOutput('features_summary_table')
        ), 
        tabPanel('Features Full',
                 dataTableOutput('features_df_individual_table')
                 )
      )
    )
  })
    
    ## Alpha Fold #####
    
    AlphaFold = reactive({
      sequence_df = sequences()
      
      sequence = sequence_df$protein_sequence[1]
      n_sequence = toupper(sequence_df$pR030A[1])
      
      a_seq = predict_structure_from_sequence(n_sequence)
      
      a_hit = submit_to_colabfold(sequence)
    })
    
    output$result_output_ui = renderUI({
      result_list = consolidate_data() 
      names(result_list)
      sequences()
      
      
      uniprot_df = result_list$uniprot_df
      colnames(uniprot_df)
      

      h = ggplot(uniprot_df) + 
        geom_histogram(aes(x = protein_length),binwidth = 10) 
      (h_z = h + 
        coord_cartesian(xlim = c(0,2000)))
      
      output$uniprot_protein_size_full = renderPlot({
        h
      })
      output$uniprot_protein_size_zoom = renderPlot({
        h_z
      })
      
      output$feature_list_text = renderText({
        feature_text = paste0(unique(consolidate_data()$feature_df$type),collapse = ', ')
      })
      
      lst = list(
        tabsetPanel(
          tabPanel("Description",
            tags$h3('Uniprot'),
            downloadButton("download_uniprot_xlsx", "Excel", class = "btn-sm btn-outline-success"),
            downloadButton("download_uniprot_tsv", "tsv", class = "btn-sm btn-outline-success"),
            DT::dataTableOutput('uniprot_results'),
            tags$h3("Alpha Fold"),
            downloadButton("download_alphafold_xlsx", "Excel", class = "btn-sm btn-outline-success"),
            DT::dataTableOutput('alphafold_results')
          ),
          tabPanel('Features',
                   downloadButton("download_features_xlsx", "Excel", class = "btn-sm btn-outline-success"),
                   textOutput('feature_list_text'),
                   DT::dataTableOutput('uniprot_features')      
                   ),
          tabPanel("Sequences",
                   downloadButton("download_sequences_xlsx", "Excel", class = "btn-sm btn-outline-success"),
                   downloadButton("download_sequences_tsv", "tsv", class = "btn-sm btn-outline-success"),
                   DT::dataTableOutput('sequences_table')  
                   ),
          tabPanel('Protein Sizes',
                   plotOutput('uniprot_protein_size_full'),
                   plotOutput('uniprot_protein_size_zoom')
                   ),
          tabPanel("3D Structure",
                   h5("AlphaFold Structure"),
                   verbatimTextOutput('structure_info'),
                   br(),
                   r3dmolOutput('protein_structure', height = "500px")
          )
        )
      )
      
      do.call(tagList,lst)
      
      
    })
  
    #uniprot_fields <- uniprotFieldsManagerServer("fields_manager")
    
    
    ## PDB VIS #####
    # Add this to your server.R
    observe({  
      req(input$individual_uniprots) 
      
      feature_df = consolidate_data()$feature_df  
      uniprot_id_ind = input$individual_uniprots
      feature_df = consolidate_data()$feature_df %>% 
        filter(uniprot_id == uniprot_id_ind)
      print(unique(feature_df$type))
      # Create the plots (your existing code)
      uniprot_id = uniprot_id_ind
      features_df = feature_df
      uniprot_df = consolidate_data()$uniprot_df
      colnames(uniprot_df)
      
      impact_df = rbindlist(values$impact_issues) %>% 
        
        filter(uniprot_id == uniprot_id_ind)
      output$uniprot_term_table_ind = renderTable({
        uniprot_df %>%
          filter(uniprot_id == uniprot_id_ind) %>% 
          dplyr::select(one_of('uniprot_id',"c_term_buffer", "c_terminal_score","n_term_buffer" , "n_terminal_score",       
                               "preferred_terminus", "recommendation"))
      })
      
      output$uniprot_term_detail_table_ind = renderTable({
        impact_df
      })
      
      output$uniprot_summary_table_ind = renderTable({
        uniprot_df %>%
          filter(uniprot_id == uniprot_id_ind) %>% 
          dplyr::select(one_of('uniprot_id',"primaryAccession","genes_geneName",
                               "protein_length","protein_name",
                               'Secreted','Multimeric',"preferred_terminus"))
      })
      
      plots <- visualize_protein(feature_df, uniprot_id_ind, 
                                 c_term_buffer = input$c_term_buffer,
                                 n_term_buffer = input$n_term_buffer)
      
      output$tagging_report <- renderUI({
        #req(input$uniprot_select) 
        
        impact_analysis <- analyze_terminal_tag_impact(feature_df, uniprot_id = uniprot_id_ind)
        
        report_text <- create_tagging_report(impact_analysis)
        report_text
        # Convert to HTML with line breaks
        report_html <- gsub("\n", "<br>", report_text)
        
        div(
          style = "font-family: 'Courier New', monospace; 
             background-color: #f8f9fa; 
             padding: 15px; 
             border-radius: 8px;",
          HTML(report_html)
        )
      })
      
      # output$tagging_report <- renderText({ 
      #   #req(input$uniprot_select)  # or whatever triggers the analysis
      #   
      #   # Run your analysis
      #   impact_analysis <- analyze_terminal_tag_impact(feature_df, uniprot_id = uniprot_id_ind)
      #   
      #   # Get the formatted report
      #   report_text <- create_tagging_report(impact_analysis)
      #   
      #   report_text
      # })
      
      # Render feature outputs (your existing code)
      output$features_feature_map = renderPlotly({
        plots$feature_map
      })
      
      
      output$features_domain_arch = renderPlotly({
        plots$domain_arch
      })
      
      
      output$features_full_table = renderDataTable({
        feature_df
      })
      output$features_summary_table = renderDataTable({
        plots$summary_table
      })
      
      # ADD THIS: Render 3D structure
      # output$protein_structure <- renderR3dmol({  
      #   #visualize_pdb_structure(uniprot_id, "alphafold_structures")
      #   render_structure_robust(input$individual_uniprots, "alphafold_structures", method = "auto")
      #   
      # })
      
      # output$protein_structure <- renderR3dmol({ 
      #   req(input$individual_uniprots)
      #   
      #   uniprot_id <- input$individual_uniprots
      #   pdb_file <- file.path("alphafold_structures", paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))
      #   
      #   if (file.exists(pdb_file)) {
      #     pdb_content <- readLines(pdb_file, warn = FALSE)
      #     pdb_string <- paste(pdb_content, collapse = "\n")
      #     
      #     r3dmol(elementId = paste0("structure_", uniprot_id)) %>%
      #       m_add_model(data = pdb_string, format = "pdb") %>%
      #       m_set_style(style = list(cartoon = list(color = "spectrum"))) %>%
      #       m_zoom_to() %>%
      #       m_spin()
      #   }
      # })
      
      output$protein_structure <- renderR3dmol({
        req(input$individual_uniprots)

        # Add small delay to ensure container is ready
        Sys.sleep(0.1)

        uniprot_id <- input$individual_uniprots
        pdb_file <- file.path("alphafold_structures", paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))

        if (file.exists(pdb_file)) {
          pdb_content <- readLines(pdb_file, warn = FALSE)
          pdb_string <- paste(pdb_content, collapse = "\n")

          r3dmol() %>%  # Remove elementId - let Shiny handle it
            m_add_model(data = pdb_string, format = "pdb") %>%
            m_set_style(style = list(cartoon = list(color = "spectrum"))) %>%
            m_zoom_to() %>%
            m_spin()
        } else {
          # Return empty widget if file not found
          r3dmol()
        }
      })
      
      # output$protein_structure <- renderR3dmol({ 
      #   r3dmol() %>%
      #     m_add_model(data = "ATOM      1  CA  ALA A   1      20.154  16.967  10.000", format = "pdb") %>%
      #     m_set_style(style = list(sphere = list())) %>%
      #     m_zoom_to()
      # })
      
      # ADD THIS: Structure info
      output$structure_info <- renderText({
        info <- get_pdb_info(uniprot_id, "alphafold_structures")
        if (info$status == "found") {
          paste("File:", info$file_name, "| Residues:", info$residue_count, "| Atoms:", info$atom_count)
        } else {
          info$message
        }
      })
    })
    
    
    proteinFeaturesHelpServer("help")
    
    # Update your UI output
    output$uniprot_features_output_ui = renderUI({
      req(input$individual_uniprots)
      
      tagList(
        tableOutput('uniprot_summary_table_ind'),
        tableOutput('uniprot_term_table_ind'),
        tableOutput('uniprot_term_detail_table_ind'),
        #h4(paste("Analysis for", input$individual_uniprots)),
        #textOutput('sequence_text'),
        
        # div(
        #   style = "font-family: 'Courier New', monospace;
        #    white-space: pre-wrap;
        #    background-color: #f8f9fa;
        #    padding: 15px;
        #    border: 1px solid #dee2e6;
        #    border-radius: 8px;
        #    overflow-x: auto;",
        #   textOutput("tagging_report")
        # ),
        #html("tagging_report"),
        #uiOutput("tagging_report"),
        
        div(
          style = "word-wrap: break-word; white-space: pre-wrap; overflow-wrap: break-word;",
          textOutput("sequence_text")
        ),
        # Your existing feature tabs
        tabsetPanel(
          tabPanel("Feature Map",
                   plotlyOutput('features_feature_map', height = "400px")
          ),
          tabPanel("Domain Architecture", 
                   plotlyOutput('features_domain_arch', height = "300px")
          ),
          tabPanel("Feature Summary",
                   dataTableOutput('features_summary_table')
          ),
          tabPanel('Feature Full',
                   dataTableOutput('features_full_table')),
          tabPanel("Feature Guide", proteinFeaturesHelpUI("help")),
          # ADD THIS: New 3D structure tab
          tabPanel("3D Structure",
                   h5("AlphaFold Structure"),
                   verbatimTextOutput('structure_info'),
                   br(),
                   #r3dmolOutput('protein_structure', height = "500px")
                   div(style = "height: 500px; width: 100%;",
                       r3dmolOutput('protein_structure', height = "500px", width = "100%"))
                   
                   
          )
        )
      )
    })
    
    # output$protein_structure <- renderR3dmol({ 
    #   r3dmol() %>%
    #     m_add_model(data = "ATOM      1  CA  ALA A   1      20.154  16.967  10.000", format = "pdb") %>%
    #     m_set_style(style = list(sphere = list())) %>%
    #     m_zoom_to()
    # })
    
    # ### PDB Viewer ####
    # 
    output$alphafold_info <- renderUI({
      req(input$uniprot_select)
      viewer_data <- create_alphafold_viewer(search_section(), input$uniprot_select[1])
      viewer_data$info_card
    })

    output$alphafold_plot <- renderPlotly({
      req(input$uniprot_select)
      viewer_data <- create_alphafold_viewer(search_section(), input$uniprot_select[1])
      viewer_data$plot_3d
    })
  file = 'alphafold_structures/AF-O95477-F1-model_v4.pdb'
  
  ## Dynamic Filter ####
  # After you create result_df
  result_df <- reactive({   
    df = consolidate_data()$uniprot_df %>% 
      left_join(sequences(), by = 'uniprot_id')
    

    df
  })
  
  feature_df = reactive({
    consolidate_data()$feature_df
  })
  
  filtered_proteins <- dynamicFilterServer("protein_filter", result_df)
  
  final_proteins <- featureExclusionServer("feature_filter",
                                           filtered_proteins,      # Already reactive
                                           feature_df) 
  
  output$dynamic_filter_input_ui = renderUI({
    df = consolidate_data()$uniprot_df
    (col_names = colnames(df))

  })
  
  output$dynamic_filter_output_ui = renderUI({
    
  })
  
  ## Report ####
  
  # In your server, call it like this (simplified - no plots parameter needed):
  proteinReportServer("protein_report",
                      reactive({ input$individual_uniprots }),
                      consolidate_data,
                      reactive({ values$impact_issues }),
                      reactive({ input$c_term_buffer }),  # Optional: buffer settings
                      reactive({ input$n_term_buffer }))  # Optional: buffer settings
  
  
  # Replace your existing proteinReportServer call with this debug version
  proteinReportServer_debug <- function(id, uniprot_id_reactive, consolidate_data_func, 
                                        impact_issues_reactive) {
    moduleServer(id, function(input, output, session) {
      
      output$download_report <- downloadHandler(
        filename = function() {
          uniprot_id <- uniprot_id_reactive()
          paste0("Debug_Protein_Report_", uniprot_id, "_", Sys.Date(), ".docx")
        },
        
        content = function(file) {
          uniprot_id <- uniprot_id_reactive()
          
          withProgress(message = 'Generating debug report...', value = 0, {
            
            # Generate debug report
            temp_file <- generate_protein_report_debug(
              uniprot_id = uniprot_id,
              consolidate_data_func = consolidate_data_func,
              impact_issues_list = impact_issues_reactive()
            )
            
            incProgress(1, detail = "Complete!")
            
            # Copy to download location
            file.copy(temp_file, file)
          })
        },
        
        contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      )
    })
  }
  
  ## OpenAI #####
  
  observeEvent(input$openai_search, { 
    (uniprot_ids = uniprot_ids())
    
    length(uniprot_ids)
   
   # openai_list
    
    # Initialize progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())  # Ensure progress bar closes even if error occurs
    
    progress$set(message = "Initializing protein analysis...", value = 0)
    total_steps <- length(uniprot_ids)
    

    i = 1
    for (i in seq_along(uniprot_ids)) {
      uniprot_id <- uniprot_ids[i]
      print(uniprot_id)
      #print(uniprot_id)
      print(paste(grep(uniprot_id,uniprot_ids),'of',length(uniprot_ids)))
      
      # Update progress bar
      progress$set(
        message = paste("Processing protein", i, "of", total_steps),
        detail = paste("Current protein:", uniprot_id),
        value = (i - 1) / total_steps
      )
      
      
      
      openai_df = data.frame('uniprot_id' = uniprot_id)
      
      
      
      values$openai[[uniprot_id]] = openai_df
      saveRDS(values$openai,'Data/openai.rds')
      
    }
    
    # open_ai_df = rbindlist(openai_list,fill = T) %>% 
    #   filter(uniprot_id %in% uniprot_ids())
  })
  
  open_data = reactive({   
    
    openai_list = values$openai
 
    openai_df = rbindlist(openai_list,fill = T) %>% 
      filter(uniprot_id %in% uniprot_ids())

    output$openai_table <- DT::renderDataTable({
      DT::datatable(openai_df, options = list(scrollX = TRUE))
    })
      
    openai_df
  })
  
  output$openai_input_ui = renderUI({
    
  })
  
  output$openai_output_ui = renderUI({
    df = open_data()
    lst = list(
      dataTableOutput('openai_table')
    )
    do.call(tagList,lst)
  })
  
  
  # Now you can use filtered_proteins() anywhere in your app
}


