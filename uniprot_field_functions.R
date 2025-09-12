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

# UI for field selector
uniprotFieldSelectorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Select UniProt Fields to Retrieve"),
    
    # Quick selection buttons
    div(
      style = "margin-bottom: 15px;",
      actionButton(ns("select_default"), "Select Protein Design Fields", 
                   class = "btn-primary btn-sm"),
      actionButton(ns("select_all"), "Select All Fields", 
                   class = "btn-secondary btn-sm"),
      actionButton(ns("clear_all"), "Clear All", 
                   class = "btn-outline-secondary btn-sm")
    ),
    
    # Field selector
    selectizeInput(
      ns("selected_fields"),
      "Available UniProt Fields:",
      choices = uniprot_fields,
      selected = default_selected_fields,
      multiple = TRUE,
      options = list(
        placeholder = "Type to search fields...",
        plugins = list("remove_button", "drag_drop"),
        maxItems = NULL
      )
    ),
    
    # Show selected count
    div(
      style = "color: #666; font-size: 0.9em; margin-top: 5px;",
      textOutput(ns("field_count"))
    )
  )
}

# Fixed Server for field selector
uniprotFieldSelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Quick selection handlers - FIXED: using updateSelectizeInput instead of updateSelectInput
    observeEvent(input$select_default, {
      updateSelectizeInput(session, "selected_fields", selected = default_selected_fields)
    })
    
    observeEvent(input$select_all, {
      updateSelectizeInput(session, "selected_fields", selected = names(uniprot_fields))
    })
    
    observeEvent(input$clear_all, {
      updateSelectizeInput(session, "selected_fields", selected = character(0))
    })
    
    # Show field count
    output$field_count <- renderText({
      count <- length(input$selected_fields)
      paste("Selected:", count, "fields")
    })
    
    # Return selected fields
    return(reactive({ input$selected_fields }))
  })
}

# Simplified and more reliable UniProt search function with better debugging
get_uniprot_info_enhanced <- function(uniprot_id, selected_fields = default_selected_fields) {
  tryCatch({
    # Clean the input
    uniprot_id <- trimws(uniprot_id)
    
    # UniProt REST API endpoint
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    
    # Make the API request
    response <- url(url)
    on.exit(close(response))
    
    json_text <- readLines(response, warn = FALSE)
    
    if (length(json_text) == 0) {
      return(data.frame(
        uniprot_id = uniprot_id,
        error = paste("UniProt ID not found:", uniprot_id),
        stringsAsFactors = FALSE
      ))
    }
    
    # Parse JSON response
    data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
    
    # Debug: print selected fields
    cat("Processing fields for", uniprot_id, ":\n")
    cat("Selected fields:", paste(selected_fields, collapse = ", "), "\n")
    
    # Initialize result data frame with uniprot_id
    result_df <- data.frame(uniprot_id = uniprot_id, stringsAsFactors = FALSE)
    
    # Extract data based on selected fields - with explicit debugging
    for (field in selected_fields) {
      cat("Processing field:", field, "\n")
      field_value <- extract_uniprot_field_debug(data, field)
      cat("Result for", field, ":", field_value, "\n")
      result_df[[field]] <- field_value
    }
    
    return(result_df)
    
  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    return(data.frame(
      uniprot_id = uniprot_id,
      error = paste("Error retrieving data:", e$message),
      stringsAsFactors = FALSE
    ))
  })
}

# Debug version of extraction function
extract_uniprot_field_debug <- function(data, field) {
  cat("  -> Extracting field:", field, "\n")
  
  # Direct test of basic fields first
  if (field == "accession") {
    result <- if (!is.null(data$primaryAccession)) data$primaryAccession else "Not available"
    cat("    Primary accession found:", result, "\n")
    return(result)
  }
  
  if (field == "id") {
    result <- if (!is.null(data$uniProtkbId)) data$uniProtkbId else "Not available"
    cat("    UniProt ID found:", result, "\n")
    return(result)
  }
  
  if (field == "protein_name") {
    if (!is.null(data$proteinDescription) && 
        !is.null(data$proteinDescription$recommendedName) &&
        !is.null(data$proteinDescription$recommendedName$fullName) &&
        !is.null(data$proteinDescription$recommendedName$fullName$value)) {
      result <- data$proteinDescription$recommendedName$fullName$value
      cat("    Protein name found:", result, "\n")
      return(result)
    } else {
      cat("    Protein name structure not found\n")
      return("Not available")
    }
  }
  
  if (field == "gene_names") {
    if (!is.null(data$genes) && length(data$genes) > 0 &&
        !is.null(data$genes[[1]]$geneName) &&
        !is.null(data$genes[[1]]$geneName$value)) {
      result <- data$genes[[1]]$geneName$value
      cat("    Gene name found:", result, "\n")
      return(result)
    } else {
      cat("    Gene name structure not found\n")
      return("Not available")
    }
  }
  
  if (field == "organism_name") {
    if (!is.null(data$organism) && !is.null(data$organism$scientificName)) {
      result <- data$organism$scientificName
      cat("    Organism name found:", result, "\n")
      return(result)
    } else {
      cat("    Organism name not found\n")
      return("Not available")
    }
  }
  
  if (field == "organism_id") {
    if (!is.null(data$organism) && !is.null(data$organism$taxonId)) {
      result <- as.character(data$organism$taxonId)
      cat("    Organism ID found:", result, "\n")
      return(result)
    } else {
      cat("    Organism ID not found\n")
      return("Not available")
    }
  }
  
  # For any other field
  cat("    Field not implemented:", field, "\n")
  return(paste("Field not implemented:", field))
}

# Fixed extraction function based on actual UniProt JSON structure
extract_uniprot_field_simple <- function(data, field) {
  result <- tryCatch({
    switch(field,
           "accession" = {
             if (!is.null(data$primaryAccession)) {
               data$primaryAccession
             } else "Not available"
           },
           "id" = {
             if (!is.null(data$uniProtkbId)) {
               data$uniProtkbId
             } else "Not available"
           },
           "protein_name" = {
             # Based on your debug output: proteinDescription$recommendedName$fullName$value
             if (!is.null(data$proteinDescription) && 
                 !is.null(data$proteinDescription$recommendedName) &&
                 !is.null(data$proteinDescription$recommendedName$fullName) &&
                 !is.null(data$proteinDescription$recommendedName$fullName$value)) {
               data$proteinDescription$recommendedName$fullName$value
             } else if (!is.null(data$proteinDescription$alternativeNames) && 
                        length(data$proteinDescription$alternativeNames) > 0 &&
                        !is.null(data$proteinDescription$alternativeNames[[1]]$fullName$value)) {
               data$proteinDescription$alternativeNames[[1]]$fullName$value
             } else "Not available"
           },
           "gene_names" = {
             if (!is.null(data$genes) && length(data$genes) > 0 &&
                 !is.null(data$genes[[1]]$geneName) &&
                 !is.null(data$genes[[1]]$geneName$value)) {
               data$genes[[1]]$geneName$value
             } else "Not available"
           },
           "organism_name" = {
             if (!is.null(data$organism) && !is.null(data$organism$scientificName)) {
               data$organism$scientificName
             } else "Not available"
           },
           "organism_id" = {
             if (!is.null(data$organism) && !is.null(data$organism$taxonId)) {
               as.character(data$organism$taxonId)
             } else "Not available"
           },
           "reviewed" = {
             if (!is.null(data$entryType)) {
               data$entryType
             } else "Not available"
           },
           "sequence" = {
             if (!is.null(data$sequence) && !is.null(data$sequence$value)) {
               data$sequence$value
             } else "Not available"
           },
           "length" = {
             if (!is.null(data$sequence) && !is.null(data$sequence$length)) {
               data$sequence$length
             } else NA_integer_
           },
           "mass" = {
             if (!is.null(data$sequence) && !is.null(data$sequence$molWeight)) {
               data$sequence$molWeight
             } else NA_real_
           },
           "cc_function" = {
             # Extract FUNCTION comment
             extract_comment_by_type(data, "FUNCTION")
           },
           "cc_subcellular_location" = {
             # Extract SUBCELLULAR LOCATION comment
             extract_comment_by_type(data, "SUBCELLULAR LOCATION")
           },
           "ft_signal" = {
             # Extract SIGNAL feature
             extract_feature_by_type(data, "SIGNAL")
           },
           "ft_transmem" = {
             # Extract TRANSMEM feature
             extract_feature_by_type(data, "TRANSMEM")
           },
           "xref_embl" = {
             # Extract EMBL cross-references
             extract_cross_ref_by_db(data, "EMBL")
           },
           # Default for unimplemented fields
           "Not implemented yet"
    )
  }, error = function(e) {
    paste("Error extracting", field, ":", e$message)
  })
  
  return(result)
}

# Helper function to extract comments by type
extract_comment_by_type <- function(data, comment_type) {
  if (!is.null(data$comments) && length(data$comments) > 0) {
    for (i in seq_along(data$comments)) {
      comment <- data$comments[[i]]
      if (!is.null(comment$commentType) && comment$commentType == comment_type) {
        if (!is.null(comment$texts) && length(comment$texts) > 0 &&
            !is.null(comment$texts[[1]]$value)) {
          return(comment$texts[[1]]$value)
        }
      }
    }
  }
  return("Not available")
}

# Helper function to extract features by type  
extract_feature_by_type <- function(data, feature_type) {
  if (!is.null(data$features) && length(data$features) > 0) {
    locations <- c()
    for (i in seq_along(data$features)) {
      feature <- data$features[[i]]
      if (!is.null(feature$type) && feature$type == feature_type) {
        start <- feature$location$start$value
        end <- feature$location$end$value
        if (!is.null(start) && !is.null(end)) {
          locations <- c(locations, paste0(start, "-", end))
        }
      }
    }
    if (length(locations) > 0) {
      return(paste(locations, collapse = "; "))
    }
  }
  return("Not available")
}

# Helper function to extract cross-references by database
extract_cross_ref_by_db <- function(data, database) {
  if (!is.null(data$uniProtKBCrossReferences) && length(data$uniProtKBCrossReferences) > 0) {
    ids <- c()
    for (i in seq_along(data$uniProtKBCrossReferences)) {
      ref <- data$uniProtKBCrossReferences[[i]]
      if (!is.null(ref$database) && ref$database == database) {
        ids <- c(ids, ref$id)
      }
    }
    if (length(ids) > 0) {
      return(paste(ids, collapse = "; "))
    }
  }
  return("Not available")
}

# Helper function to extract specific fields from UniProt data
extract_uniprot_field <- function(data, field) {
  switch(field,
         "accession" = ifelse(length(data$primaryAccession) > 0, data$primaryAccession, "Not available"),
         "id" = ifelse(length(data$uniProtkbId) > 0, data$uniProtkbId, "Not available"),
         "protein_name" = extract_protein_name(data),
         "gene_names" = extract_gene_names(data),
         "organism_name" = ifelse(length(data$organism$scientificName) > 0, data$organism$scientificName, "Not available"),
         "organism_id" = ifelse(length(data$organism$taxonId) > 0, data$organism$taxonId, "Not available"),
         "reviewed" = ifelse(length(data$entryType) > 0, data$entryType, "Not available"),
         "sequence" = ifelse(length(data$sequence$value) > 0, data$sequence$value, "Not available"),
         "length" = ifelse(length(data$sequence$length) > 0, data$sequence$length, NA),
         "mass" = ifelse(length(data$sequence$molWeight) > 0, data$sequence$molWeight, NA),
         "ft_signal" = extract_feature(data, "Signal peptide"),
         "ft_transmem" = extract_feature(data, "Transmembrane"),
         "ft_domain" = extract_feature(data, "Domain"),
         "ft_act_site" = extract_feature(data, "Active site"),
         "cc_function" = extract_comment(data, "FUNCTION"),
         "cc_subcellular_location" = extract_comment(data, "SUBCELLULAR LOCATION"),
         "cc_subunit" = extract_comment(data, "SUBUNIT"),
         "xref_embl" = extract_cross_references(data, "EMBL"),
         # Default case
         "Not available"
  )
}

# Helper functions for specific extractions
extract_protein_name <- function(data) {
  tryCatch({
    # Try recommended name first
    if (!is.null(data$proteinDescription$recommendedName$fullName$value)) {
      return(data$proteinDescription$recommendedName$fullName$value)
    }
    # Try submitted names
    if (!is.null(data$proteinDescription$submissionNames) && length(data$proteinDescription$submissionNames) > 0) {
      return(data$proteinDescription$submissionNames[[1]]$fullName$value)
    }
    # Try alternative names
    if (!is.null(data$proteinDescription$alternativeNames) && length(data$proteinDescription$alternativeNames) > 0) {
      return(data$proteinDescription$alternativeNames[[1]]$fullName$value)
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_gene_names <- function(data) {
  tryCatch({
    if (!is.null(data$genes) && length(data$genes) > 0) {
      # Get primary gene name
      if (!is.null(data$genes[[1]]$geneName$value)) {
        return(data$genes[[1]]$geneName$value)
      }
      # Try ordered locus names
      if (!is.null(data$genes[[1]]$orderedLocusNames) && length(data$genes[[1]]$orderedLocusNames) > 0) {
        return(data$genes[[1]]$orderedLocusNames[[1]]$value)
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_feature <- function(data, feature_type) {
  tryCatch({
    if (!is.null(data$features) && length(data$features) > 0) {
      # Filter features by type
      matching_features <- data$features[sapply(data$features, function(f) f$type == feature_type)]
      
      if (length(matching_features) > 0) {
        locations <- sapply(matching_features, function(feature) {
          start <- feature$location$start$value
          end <- feature$location$end$value
          if (!is.null(start) && !is.null(end)) {
            return(paste0(start, "-", end))
          } else if (!is.null(start)) {
            return(as.character(start))
          }
          return("unknown")
        })
        return(paste(locations, collapse = "; "))
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_comment <- function(data, comment_type) {
  tryCatch({
    if (!is.null(data$comments) && length(data$comments) > 0) {
      # Find matching comment type
      matching_comments <- data$comments[sapply(data$comments, function(c) c$commentType == comment_type)]
      
      if (length(matching_comments) > 0) {
        comment <- matching_comments[[1]]
        
        # Handle different comment structures
        if (!is.null(comment$texts) && length(comment$texts) > 0) {
          return(comment$texts[[1]]$value)
        } else if (!is.null(comment$subcellularLocations)) {
          # Special handling for subcellular location
          locations <- sapply(comment$subcellularLocations, function(loc) {
            if (!is.null(loc$location$value)) {
              return(loc$location$value)
            }
            return("unknown")
          })
          return(paste(locations, collapse = "; "))
        }
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

extract_cross_references <- function(data, database) {
  tryCatch({
    if (!is.null(data$uniProtKBCrossReferences) && length(data$uniProtKBCrossReferences) > 0) {
      # Filter by database
      matching_refs <- data$uniProtKBCrossReferences[sapply(data$uniProtKBCrossReferences, function(ref) ref$database == database)]
      
      if (length(matching_refs) > 0) {
        ids <- sapply(matching_refs, function(ref) ref$id)
        return(paste(ids, collapse = "; "))
      }
    }
    return("Not available")
  }, error = function(e) {
    return("Not available")
  })
}

determine_secreted_status <- function(subcellular_location) {
  if (is.na(subcellular_location) || subcellular_location == "Not available") {
    return("Unknown")
  }
  
  secreted_keywords <- c("Secreted", "Extracellular", "Cell surface", "Membrane; Single-pass")
  
  if (any(sapply(secreted_keywords, function(x) grepl(x, subcellular_location, ignore.case = TRUE)))) {
    return("Y")
  }
  
  return("N")
}

# Example usage in your Shiny app:
# 
# ui <- fluidPage(
#   uniprotFieldSelectorUI("field_selector"),
#   hr(),
#   textInput("uniprot_id", "UniProt ID:", value = "P04637"),
#   actionButton("search", "Search"),
#   hr(),
#   DT::dataTableOutput("results")
# )
# 
# server <- function(input, output, session) {
#   selected_fields <- uniprotFieldSelectorServer("field_selector")
#   
#   observeEvent(input$search, {
#     result <- get_uniprot_info_enhanced(input$uniprot_id, selected_fields())
#     output$results <- DT::renderDataTable({
#       DT::datatable(result, options = list(scrollX = TRUE))
#     })
#   })
# }