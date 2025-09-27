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