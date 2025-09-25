# UniProt Search Module for R Shiny
# This module searches UniProt and retrieves basic protein information

library(shiny)
library(DT)
library(httr)
library(jsonlite)
library(shinycssloaders)

# Function to query UniProt API
get_uniprot_info <- function(uniprot_id) {
  tryCatch({
    # Clean the input
    uniprot_id <- trimws(uniprot_id)
    
    # UniProt REST API endpoint
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    
    # Make the API request
    response <- GET(url)
    
    if (status_code(response) != 200) {
      return(list(error = paste("UniProt ID not found:", uniprot_id)))
    }
    
    # Parse JSON response
    data <- fromJSON(content(response, "text"))
    
    # Extract relevant information
    protein_info <- list(
      uniprot_id = uniprot_id,
      protein_name = ifelse(length(data$proteinDescription$recommendedName$fullName$value) > 0,
                            data$proteinDescription$recommendedName$fullName$value,
                            "Not available"),
      gene_symbol = ifelse(length(data$genes) > 0 && length(data$genes[[1]]$geneName$value) > 0,
                           data$genes[[1]]$geneName$value,
                           "Not available"),
      species = ifelse(length(data$organism$scientificName) > 0,
                       data$organism$scientificName,
                       "Not available"),
      taxonomy_id = ifelse(length(data$organism$taxonId) > 0,
                           data$organism$taxonId,
                           "Not available"),
      protein_sequence = ifelse(length(data$sequence$value) > 0,
                                data$sequence$value,
                                "Not available"),
      sequence_length = ifelse(length(data$sequence$length) > 0,
                               data$sequence$length,
                               0)
    )
    
    # Get nucleotide sequence (requires additional API call to cross-references)
    # This is more complex as it requires finding the corresponding gene entry
    nucleotide_sequence <- get_nucleotide_sequence(uniprot_id, data)
    protein_info$nucleotide_sequence <- nucleotide_sequence
    
    return(protein_info)
    
  }, error = function(e) {
    return(list(error = paste("Error retrieving data:", e$message)))
  })
}

# Helper function to get nucleotide sequence
get_nucleotide_sequence <- function(uniprot_id, uniprot_data) {
  tryCatch({
    # Look for cross-references to nucleotide databases
    if (length(uniprot_data$uniProtKBCrossReferences) > 0) {
      # Find EMBL/GenBank references
      embl_refs <- uniprot_data$uniProtKBCrossReferences[
        uniprot_data$uniProtKBCrossReferences$database == "EMBL", ]
      
      if (nrow(embl_refs) > 0) {
        # For simplicity, return the first EMBL ID found
        # In a production app, you might want to query the actual sequence
        return(paste("EMBL ID:", embl_refs$id[1], "(sequence retrieval requires additional API call)"))
      }
    }
    return("Not available - no nucleotide cross-references found")
  }, error = function(e) {
    return("Error retrieving nucleotide sequence")
  })
}

# UI Module
uniprotSearchUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("UniProt Protein Information Search"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Search Parameters"),
        textInput(ns("uniprot_id"), 
                  "UniProt ID:", 
                  value = "",
                  placeholder = "e.g., P53_HUMAN or P04637"),
        
        actionButton(ns("search_btn"), 
                     "Search UniProt", 
                     class = "btn-primary"),
        
        br(), br(),
        
        # Option to search multiple IDs
        h5("Batch Search"),
        textAreaInput(ns("batch_ids"), 
                      "Multiple UniProt IDs (one per line):",
                      rows = 4,
                      placeholder = "P04637\nP53_HUMAN\nQ9Y6K9"),
        
        actionButton(ns("batch_search_btn"), 
                     "Batch Search", 
                     class = "btn-secondary")
      ),
      
      mainPanel(
        # Results display
        conditionalPanel(
          condition = paste0("input['", ns("search_btn"), "'] > 0 || input['", ns("batch_search_btn"), "'] > 0"),
          
          tabsetPanel(
            tabPanel("Protein Information", 
                     withSpinner(DT::dataTableOutput(ns("results_table")))),
            
            tabPanel("Protein Sequence", 
                     h4("Protein Sequence"),
                     withSpinner(verbatimTextOutput(ns("protein_seq")))),
            
            tabPanel("Nucleotide Information", 
                     h4("Nucleotide Sequence Information"),
                     withSpinner(verbatimTextOutput(ns("nucleotide_info"))))
          )
        )
      )
    )
  )
}

# Server Module
uniprotSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store results
    search_results <- reactiveVal(data.frame())
    
    # Single search
    observeEvent(input$search_btn, {
      req(input$uniprot_id)
      
      showModal(modalDialog(
        "Searching UniProt database...", 
        footer = NULL, 
        easyClose = FALSE
      ))
      
      result <- get_uniprot_info(input$uniprot_id)
      
      if ("error" %in% names(result)) {
        removeModal()
        showNotification(result$error, type = "error")
      } else {
        # Convert to data frame
        df <- data.frame(
          UniProt_ID = result$uniprot_id,
          Protein_Name = result$protein_name,
          Gene_Symbol = result$gene_symbol,
          Species = result$species,
          Taxonomy_ID = result$taxonomy_id,
          Sequence_Length = result$sequence_length,
          stringsAsFactors = FALSE
        )
        
        search_results(df)
        removeModal()
        showNotification("Search completed!", type = "success")
      }
    })
    
    # Batch search
    observeEvent(input$batch_search_btn, {
      req(input$batch_ids)
      
      ids <- trimws(strsplit(input$batch_ids, "\n")[[1]])
      ids <- ids[ids != ""]  # Remove empty lines
      
      if (length(ids) == 0) {
        showNotification("Please enter at least one UniProt ID", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        paste("Searching", length(ids), "proteins..."), 
        footer = NULL, 
        easyClose = FALSE
      ))
      
      # Process each ID
      results_list <- list()
      for (i in seq_along(ids)) {
        result <- get_uniprot_info(ids[i])
        if (!"error" %in% names(result)) {
          results_list[[i]] <- data.frame(
            UniProt_ID = result$uniprot_id,
            Protein_Name = result$protein_name,
            Gene_Symbol = result$gene_symbol,
            Species = result$species,
            Taxonomy_ID = result$taxonomy_id,
            Sequence_Length = result$sequence_length,
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (length(results_list) > 0) {
        combined_results <- do.call(rbind, results_list)
        search_results(combined_results)
        removeModal()
        showNotification(paste("Found", nrow(combined_results), "proteins"), type = "success")
      } else {
        removeModal()
        showNotification("No valid results found", type = "warning")
      }
    })
    
    # Results table
    output$results_table <- DT::renderDataTable({
      req(nrow(search_results()) > 0)
      
      DT::datatable(
        search_results(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        selection = 'single'
      )
    })
    
    # Protein sequence display
    output$protein_seq <- renderText({
      req(input$results_table_rows_selected)
      
      selected_id <- search_results()[input$results_table_rows_selected, "UniProt_ID"]
      result <- get_uniprot_info(selected_id)
      
      if (!"error" %in% names(result) && result$protein_sequence != "Not available") {
        # Format sequence with line breaks every 60 characters
        seq <- result$protein_sequence
        formatted_seq <- paste(
          substring(seq, seq(1, nchar(seq), 60), seq(60, nchar(seq), 60)),
          collapse = "\n"
        )
        paste0("Length: ", nchar(seq), " amino acids\n\n", formatted_seq)
      } else {
        "No sequence available or protein not found."
      }
    })
    
    # Nucleotide information display
    output$nucleotide_info <- renderText({
      req(input$results_table_rows_selected)
      
      selected_id <- search_results()[input$results_table_rows_selected, "UniProt_ID"]
      result <- get_uniprot_info(selected_id)
      
      if (!"error" %in% names(result)) {
        result$nucleotide_sequence
      } else {
        "No nucleotide information available."
      }
    })
    
    # Return search results for use in parent app
    return(reactive({ search_results() }))
  })
}

# Example usage in your main Shiny app:
# 
# ui <- fluidPage(
#   uniprotSearchUI("uniprot_search")
# )
# 
# server <- function(input, output, session) {
#   uniprot_results <- uniprotSearchServer("uniprot_search")
#   
#   # You can use uniprot_results() to access the search results
#   # in other parts of your app
# }
# 
# shinyApp(ui = ui, server = server)
