# UniProt Search Module for R Shiny
# This module searches UniProt and retrieves basic protein information



# Function to query UniProt API (using base R only)
get_uniprot_info <- function(uniprot_id) {
  tryCatch({
    # Clean the input
    uniprot_id <- trimws(uniprot_id)
    
    # UniProt REST API endpoint
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    
    # Make the API request using base R
    response <- url(url)
    on.exit(close(response))
    
    # Read and parse JSON response
    json_text <- readLines(response, warn = FALSE)
    
    if (length(json_text) == 0) {
      return(list(error = paste("UniProt ID not found:", uniprot_id)))
    }
    
    # Parse JSON response
    data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
    
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

# Helper function to process multiple UniProt IDs and return combined data.frame
get_uniprot_batch <- function(uniprot_ids) {
  # Remove empty/NA values
  uniprot_ids <- uniprot_ids[!is.na(uniprot_ids) & uniprot_ids != ""]
  
  if (length(uniprot_ids) == 0) {
    return(data.frame())
  }
  
  # Process each ID and combine results
  results_list <- lapply(uniprot_ids, get_uniprot_info)
  
  # Combine all data.frames
  combined_df <- do.call(rbind, results_list)
  
  return(combined_df)
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


# UniProt Field Selector and Enhanced Search Function
library(shiny)
library(jsonlite)

# Complete list of UniProt fields available via API
uniprot_fields <- list(
  # Basic Information
  "accession" = "Accession ID (UniProt)",
  "id" = "Entry ID",
  "protein_name" = "Protein Name",
  "gene_names" = "Gene Symbol",
  "organism_name" = "Organism of Origin",
  "organism_id" = "Taxonomy ID",
  "reviewed" = "Reviewed Status (SwissProt/TrEMBL)",
  
  # Sequence Information
  "sequence" = "Protein Sequence",
  "length" = "Protein Length (aa)",
  "mass" = "Molecular Weight",
  "ft_signal" = "Signal Peptide",
  "ft_propep" = "Propeptide",
  "ft_transit" = "Transit Peptide",
  "ft_transmem" = "Transmembrane Domain",
  "ft_topo_dom" = "Topological Domain",
  "ft_intramem" = "Intramembrane Region",
  
  # Functional Information
  "cc_function" = "Function",
  "cc_activity_regulation" = "Activity Regulation",
  "cc_catalytic_activity" = "Catalytic Activity",
  "cc_cofactor" = "Cofactor",
  "cc_pathway" = "Pathway",
  "ft_act_site" = "Active Site",
  "ft_binding" = "Binding Site",
  "ft_site" = "Site",
  "ft_metal" = "Metal Binding",
  
  # Structural Information
  "cc_subunit" = "Subunit Structure/Multimerization",
  "ft_domain" = "Domain",
  "ft_region" = "Region",
  "ft_motif" = "Motif",
  "ft_repeat" = "Repeat",
  "ft_coiled" = "Coiled Coil",
  "ft_compbias" = "Compositional Bias",
  "structure_3d" = "3D Structure",
  
  # Subcellular Location
  "cc_subcellular_location" = "Subcellular Location",
  "cc_membrane" = "Membrane Information",
  "ft_membrane" = "Membrane Features",
  
  # Expression and Interactions
  "cc_tissue_specificity" = "Tissue Specificity",
  "cc_developmental_stage" = "Developmental Stage",
  "cc_induction" = "Induction",
  "cc_interaction" = "Protein Interactions",
  
  # Disease and Variants
  "cc_disease" = "Disease Association",
  "ft_variant" = "Natural Variants",
  "ft_mutagen" = "Mutagenesis",
  "ft_conflict" = "Sequence Conflicts",
  
  # Post-translational Modifications
  "ft_mod_res" = "Modified Residue",
  "ft_lipid" = "Lipidation",
  "ft_carbohyd" = "Glycosylation",
  "ft_disulfid" = "Disulfide Bond",
  "ft_crosslnk" = "Cross-link",
  
  # Immunological Information
  "ft_antigen" = "Antigenic Regions",
  "cc_allergen" = "Allergen Information",
  "cc_toxic_dose" = "Toxic Dose",
  
  # Cross-references
  "xref_embl" = "EMBL/GenBank References",
  "xref_pdb" = "PDB Structure References",
  "xref_go" = "Gene Ontology",
  "xref_interpro" = "InterPro Domains",
  "xref_pfam" = "Pfam Domains",
  
  # Additional Fields
  "annotation_score" = "Annotation Score",
  "protein_existence" = "Protein Evidence",
  "tools" = "Analysis Tools",
  "keywords" = "Keywords",
  "ec" = "EC Number",
  "absorption" = "Absorption Properties",
  "kinetics" = "Kinetic Parameters",
  "ph_dependence" = "pH Dependence",
  "redox_potential" = "Redox Potential",
  "temperature_dependence" = "Temperature Dependence"
)

# Your specific fields for protein design (pre-selected)
default_selected_fields <- c(
  "protein_name",           # Protein (Name/Symbol)
  "accession",              # Accession ID (e.g., UniProt)
  "organism_name",          # Organism of Origin
  "cc_subcellular_location", # For determining if secreted
  "cc_membrane",            # Membrane Protein information
  "ft_transmem",            # Transmembrane Domain Present
  "length",                 # Original Protein Length (aa)
  "ft_signal",              # Signal Peptide Present
  "ft_propep",              # Proprotein information
  "cc_subunit",             # Functional Multimer information
  "ft_domain",              # Known Active Sites/Functional Domains
  "ft_act_site",            # Active sites
  "ft_antigen",             # Known Immunodominant Epitope Locations
  "sequence",               # Protein sequence
  "xref_embl"               # For nucleotide sequence references
)

