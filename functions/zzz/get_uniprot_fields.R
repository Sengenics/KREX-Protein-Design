# UniProt Fields Discovery and Management System
library(shiny)
library(DT)
library(jsonlite)

# Function to get all available UniProt fields from the API
get_uniprot_fields_from_api <- function() {
  tryCatch({
    # UniProt provides a list of available return fields
    url <- "https://rest.uniprot.org/configure/uniprotkb/result-fields"
    
    cat("Fetching UniProt fields from API...\n")
    
    response <- url(url)
    on.exit(close(response))
    
    json_text <- readLines(response, warn = FALSE)
    
    if (length(json_text) == 0) {
      stop("No response from UniProt fields API")
    }
    
    # Parse the fields configuration
    fields_data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
    
    # Convert to a nice data frame
    if (is.list(fields_data) && length(fields_data) > 0) {
      fields_df <- data.frame(
        field_name = character(),
        label = character(),
        description = character(),
        data_type = character(),
        category = character(),
        stringsAsFactors = FALSE
      )
      
      # Parse the nested structure
      for (i in seq_along(fields_data)) {
        field <- fields_data[[i]]
        
        field_name <- if (!is.null(field$name)) field$name else paste0("field_", i)
        label <- if (!is.null(field$label)) field$label else field_name
        description <- if (!is.null(field$description)) field$description else "No description available"
        data_type <- if (!is.null(field$dataType)) field$dataType else "unknown"
        category <- if (!is.null(field$groupName)) field$groupName else "General"
        
        fields_df <- rbind(fields_df, data.frame(
          field_name = field_name,
          label = label,
          description = description,
          data_type = data_type,
          category = category,
          stringsAsFactors = FALSE
        ))
      }
      
      # Sort by category then field name
      fields_df <- fields_df[order(fields_df$category, fields_df$field_name), ]
      
      # Add timestamp
      fields_df$last_updated <- Sys.time()
      
      cat("Successfully retrieved", nrow(fields_df), "UniProt fields\n")
      return(fields_df)
    } else {
      stop("Unexpected API response format")
    }
    
  }, error = function(e) {
    cat("Error fetching UniProt fields:", e$message, "\n")
    return(create_fallback_fields_table())
  })
}

# Fallback fields table in case API fails
create_fallback_fields_table <- function() {
  fallback_fields <- data.frame(
    field_name = c(
      "accession", "id", "protein_name", "gene_names", "organism_name", 
      "organism_id", "reviewed", "sequence", "length", "mass",
      "cc_function", "cc_subcellular_location", "cc_subunit", 
      "ft_signal", "ft_transmem", "ft_domain", "ft_act_site",
      "xref_embl", "xref_pdb", "keywords"
    ),
    label = c(
      "Accession", "Entry ID", "Protein Name", "Gene Names", "Organism Name",
      "Organism ID", "Reviewed Status", "Sequence", "Length", "Mass",
      "Function", "Subcellular Location", "Subunit Structure",
      "Signal Peptide", "Transmembrane", "Domain", "Active Site",
      "EMBL References", "PDB References", "Keywords"
    ),
    description = c(
      "Primary accession number",
      "UniProtKB entry identifier", 
      "Recommended protein name",
      "Gene name and synonyms",
      "Organism scientific name",
      "NCBI taxonomy identifier",
      "Entry review status (Swiss-Prot/TrEMBL)",
      "Amino acid sequence",
      "Sequence length in amino acids",
      "Molecular mass in daltons",
      "Protein function description",
      "Subcellular location information",
      "Subunit structure and interactions",
      "Signal peptide annotation",
      "Transmembrane regions",
      "Protein domains",
      "Active site residues",
      "Cross-references to EMBL/GenBank",
      "Cross-references to PDB structures",
      "Functional keywords"
    ),
    data_type = rep("string", 20),
    category = c(
      rep("Names & Taxonomy", 6),
      "Entry Information", 
      rep("Sequence", 3),
      rep("Function", 3),
      rep("Features", 4),
      rep("Cross-references", 2),
      "Miscellaneous"
    ),
    last_updated = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  return(fallback_fields)
}

# Function to save fields table to file
save_uniprot_fields <- function(fields_df, filename = "uniprot_fields_cache.rds") {
  tryCatch({
    saveRDS(fields_df, filename)
    cat("UniProt fields saved to", filename, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error saving fields:", e$message, "\n")
    return(FALSE)
  })
}

# Function to load fields table from file
load_uniprot_fields <- function(filename = "uniprot_fields_cache.rds") {
  if (file.exists(filename)) {
    tryCatch({
      fields_df <- readRDS(filename)
      cat("Loaded", nrow(fields_df), "UniProt fields from cache\n")
      return(fields_df)
    }, error = function(e) {
      cat("Error loading cached fields:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("No cached fields file found\n")
    return(NULL)
  }
}

# Function to get fields (from cache or API)
get_uniprot_fields <- function(force_refresh = FALSE) {
  if (!force_refresh) {
    # Try to load from cache first
    cached_fields <- load_uniprot_fields()
    if (!is.null(cached_fields)) {
      return(cached_fields)
    }
  }
  
  # Get from API and save to cache
  fields_df <- get_uniprot_fields_from_api()
  save_uniprot_fields(fields_df)
  return(fields_df)
}

# UI Module for UniProt Fields Manager
uniprotFieldsManagerUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("UniProt Fields Discovery & Management"),
    
    fluidRow(
      column(12,
             div(
               style = "margin-bottom: 20px;",
               h4("Field Management"),
               actionButton(ns("refresh_fields"), "Refresh Fields from API", 
                            class = "btn-primary"),
               actionButton(ns("load_cached"), "Load Cached Fields", 
                            class = "btn-secondary"),
               br(), br(),
               textOutput(ns("fields_status"))
             )
      )
    ),
    
    fluidRow(
      column(12,
             h4("Available UniProt Fields"),
             div(
               style = "margin-bottom: 15px;",
               selectInput(ns("category_filter"), "Filter by Category:",
                           choices = c("All Categories" = "all"),
                           selected = "all")
             ),
             
             withSpinner(DT::dataTableOutput(ns("fields_table")))
      )
    ),
    
    fluidRow(
      column(12,
             br(),
             h4("Export Options"),
             downloadButton(ns("download_csv"), "Download as CSV", 
                            class = "btn-outline-primary"),
             downloadButton(ns("download_rds"), "Download as RDS", 
                            class = "btn-outline-secondary")
      )
    )
  )
}

# Server Module for UniProt Fields Manager  
uniprotFieldsManagerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store fields data
    fields_data <- reactiveVal(NULL)
    
    # Load fields on startup
    observe({
      fields_df <- get_uniprot_fields(force_refresh = FALSE)
      fields_data(fields_df)
      
      # Update category filter choices
      if (!is.null(fields_df)) {
        categories <- c("All Categories" = "all", 
                        sort(unique(fields_df$category)))
        updateSelectInput(session, "category_filter", choices = categories)
      }
    })
    
    # Refresh fields from API
    observeEvent(input$refresh_fields, {
      showModal(modalDialog(
        "Fetching latest fields from UniProt API...",
        footer = NULL, easyClose = FALSE
      ))
      
      fields_df <- get_uniprot_fields(force_refresh = TRUE)
      fields_data(fields_df)
      
      # Update category filter
      if (!is.null(fields_df)) {
        categories <- c("All Categories" = "all", 
                        sort(unique(fields_df$category)))
        updateSelectInput(session, "category_filter", choices = categories)
      }
      
      removeModal()
      showNotification("Fields refreshed from API!", type = "success")
    })
    
    # Load cached fields
    observeEvent(input$load_cached, {
      fields_df <- load_uniprot_fields()
      if (!is.null(fields_df)) {
        fields_data(fields_df)
        showNotification("Cached fields loaded!", type = "success")
      } else {
        showNotification("No cached fields found!", type = "warning")
      }
    })
    
    # Fields status
    output$fields_status <- renderText({
      if (!is.null(fields_data())) {
        paste("Loaded", nrow(fields_data()), "fields. Last updated:", 
              format(fields_data()$last_updated[1], "%Y-%m-%d %H:%M:%S"))
      } else {
        "No fields data loaded"
      }
    })
    
    # Filtered fields data
    filtered_fields <- reactive({
      req(fields_data())
      
      df <- fields_data()
      
      if (input$category_filter != "all") {
        df <- df[df$category == input$category_filter, ]
      }
      
      return(df)
    })
    
    # Fields table
    output$fields_table <- DT::renderDataTable({
      req(filtered_fields())
      
      display_df <- filtered_fields()[, c("field_name", "label", "description", 
                                          "data_type", "category")]
      
      DT::datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv'),
          columnDefs = list(
            list(width = '150px', targets = c(0, 1)),
            list(width = '300px', targets = 2),
            list(width = '100px', targets = c(3, 4))
          )
        ),
        extensions = 'Buttons',
        colnames = c("Field Name", "Label", "Description", "Data Type", "Category"),
        caption = "UniProt API Fields - Click to sort, search to filter"
      )
    })
    
    # Download handlers
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("uniprot_fields_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(fields_data(), file, row.names = FALSE)
      }
    )
    
    output$download_rds <- downloadHandler(
      filename = function() {
        paste0("uniprot_fields_", Sys.Date(), ".rds")
      },
      content = function(file) {
        saveRDS(fields_data(), file)
      }
    )
    
    # Return fields data for use in other parts of the app
    return(reactive({ fields_data() }))
  })
}

# Example usage in your main app:
# 
# ui <- fluidPage(
#   uniprotFieldsManagerUI("fields_manager")
# )
# 
# server <- function(input, output, session) {
#   uniprot_fields <- uniprotFieldsManagerServer("fields_manager")
#   
#   # You can now use uniprot_fields() to get the current fields data
#   # in other parts of your app
# }
# 
# shinyApp(ui = ui, server = server)