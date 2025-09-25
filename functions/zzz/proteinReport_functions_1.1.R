# Debug version to understand your data structure
debug_impact_df_structure <- function(impact_issues_list, uniprot_id) {
  
  cat("=== DEBUGGING IMPACT_DF STRUCTURE ===\n")
  
  # Check the raw impact_issues_list
  cat("impact_issues_list class:", class(impact_issues_list), "\n")
  cat("impact_issues_list length:", length(impact_issues_list), "\n")
  
  if (length(impact_issues_list) > 0) {
    cat("First element class:", class(impact_issues_list[[1]]), "\n")
    if (is.data.frame(impact_issues_list[[1]])) {
      cat("First element columns:", names(impact_issues_list[[1]]), "\n")
    }
  }
  
  # Try to create impact_df
  tryCatch({
    impact_df <- rbindlist(impact_issues_list) %>% 
      filter(uniprot_id == !!uniprot_id)
    
    cat("\nCombined impact_df:\n")
    cat("Class:", class(impact_df), "\n")
    cat("Dimensions:", dim(impact_df), "\n")
    cat("Column names:", names(impact_df), "\n")
    
    if (nrow(impact_df) > 0) {
      cat("First few rows:\n")
      print(head(impact_df))
    }
    
    return(impact_df)
    
  }, error = function(e) {
    cat("Error creating impact_df:", e$message, "\n")
    return(NULL)
  })
}

# Simplified report generation that skips problematic parts
generate_protein_report_debug <- function(uniprot_id, consolidate_data_func, 
                                          impact_issues_list, 
                                          c_term_buffer = 50, n_term_buffer = 50,
                                          output_dir = "protein_reports") {
  
  cat("Starting report generation for:", uniprot_id, "\n")
  
  # Debug the impact_df structure
  impact_df <- debug_impact_df_structure(impact_issues_list, uniprot_id)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get data
  feature_df <- consolidate_data_func()$feature_df %>% 
    filter(uniprot_id == !!uniprot_id)
  
  uniprot_df <- consolidate_data_func()$uniprot_df %>%
    filter(uniprot_id == !!uniprot_id)
  
  cat("Feature_df rows:", nrow(feature_df), "\n")
  cat("Uniprot_df rows:", nrow(uniprot_df), "\n")
  
  # Generate plots for this specific protein
  plots_for_protein <- NULL
  if (nrow(feature_df) > 0) {
    tryCatch({
      plots_for_protein <- visualize_protein(feature_df, uniprot_id, 
                                             c_term_buffer = c_term_buffer,
                                             n_term_buffer = n_term_buffer)
      cat("Plots generated successfully\n")
    }, error = function(e) {
      cat("Error generating plots:", e$message, "\n")
    })
  }
  
  # Create Word document
  doc <- read_docx()
  
  # Title page with protein-specific header
  protein_name <- uniprot_df$protein_name[1] %||% "Unknown Protein"
  
  doc <- doc %>%
    body_add_par("PROTEIN ANALYSIS REPORT", style = "heading 1") %>%
    body_add_par(paste("UniProt ID:", uniprot_id), style = "heading 2") %>%
    body_add_par(paste("Protein:", protein_name), style = "heading 2") %>%
    body_add_par(paste("Report Generated:", Sys.Date())) %>%
    body_add_break() %>%
    body_add_break()
  
  # Executive Summary (safe version)
  doc <- add_executive_summary_safe(doc, uniprot_df, impact_df)
  
  # Protein Summary Table (safe version)
  doc <- add_protein_summary_table_safe(doc, uniprot_df)
  
  # Skip terminal analysis tables for now to avoid the error
  doc <- doc %>%
    body_add_par("TERMINAL ANALYSIS", style = "heading 1") %>%
    body_add_par("Terminal analysis data structure being debugged - skipped in this version", style = "Normal") %>%
    body_add_break()
  
  # Feature Map Plot (generated fresh)
  doc <- add_feature_map_generated(doc, plots_for_protein, uniprot_id)
  
  # Feature Guide (properly formatted)
  doc <- add_feature_guide_formatted(doc)
  
  # Save the document
  output_file <- file.path(output_dir, paste0("Debug_Protein_Report_", uniprot_id, "_", Sys.Date(), ".docx"))
  print(doc, target = output_file)
  
  cat("Report saved to:", output_file, "\n")
  return(output_file)
}

# Safe versions of functions
add_executive_summary_safe <- function(doc, uniprot_df, impact_df) {
  
  if (nrow(uniprot_df) == 0) {
    return(doc %>% body_add_par("No protein data available"))
  }
  
  protein_name <- uniprot_df$protein_name[1] %||% "Unknown"
  gene_name <- uniprot_df$genes_geneName[1] %||% "Unknown"
  length <- uniprot_df$protein_length[1] %||% "Unknown"
  secreted <- uniprot_df$Secreted[1] %||% "Unknown"
  multimeric <- uniprot_df$Multimeric[1] %||% "Unknown"
  recommendation <- uniprot_df$recommendation[1] %||% "No recommendation available"
  
  summary_text <- paste0(
    "Protein: ", protein_name, "\n",
    "Gene: ", gene_name, "\n", 
    "Length: ", length, " amino acids\n",
    "Secreted: ", secreted, "\n",
    "Multimeric: ", multimeric, "\n\n",
    "TAGGING RECOMMENDATION:\n",
    recommendation
  )
  
  doc %>%
    body_add_par("Executive Summary", style = "heading 1") %>%
    body_add_par(summary_text, style = "Normal") %>%
    body_add_break()
}

add_protein_summary_table_safe <- function(doc, uniprot_df) {
  
  if (nrow(uniprot_df) == 0) {
    return(doc %>% body_add_par("No protein summary data available"))
  }
  
  # Create a transposed summary for better formatting
  summary_data <- data.frame(
    Property = c("UniProt ID", "Gene Name", "Protein Name", "Length (aa)", "Secreted", "Multimeric"),
    Value = c(
      uniprot_df$uniprot_id[1] %||% "Unknown",
      uniprot_df$genes_geneName[1] %||% "Unknown",
      substr(uniprot_df$protein_name[1] %||% "Unknown", 1, 60),
      as.character(uniprot_df$protein_length[1] %||% "Unknown"),
      uniprot_df$Secreted[1] %||% "Unknown",
      uniprot_df$Multimeric[1] %||% "Unknown"
    ),
    stringsAsFactors = FALSE
  )
  
  ft <- flextable(summary_data) %>%
    theme_vanilla() %>%
    set_table_properties(width = 1.0, layout = "fixed") %>%
    width(j = 1, width = 2.5) %>%
    width(j = 2, width = 4.0) %>%
    set_caption("Protein Summary Information")
  
  doc %>%
    body_add_par("PROTEIN SUMMARY", style = "heading 1") %>%
    body_add_flextable(ft) %>%
    body_add_break()
}

# Helper function
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x