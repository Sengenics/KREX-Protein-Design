# Protein Analysis Report Generator
# Creates Word documents with protein analysis results

library(officer)
library(flextable)
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(webshot)

# Main report generation function
generate_protein_report <- function(uniprot_id, consolidate_data_func, 
                                    impact_issues_list, 
                                    c_term_buffer = 50, n_term_buffer = 50,
                                    output_dir = "protein_reports") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get data
  feature_df <- consolidate_data_func()$feature_df %>% 
    filter(uniprot_id == !!uniprot_id)
  
  uniprot_df <- consolidate_data_func()$uniprot_df %>%
    filter(uniprot_id == !!uniprot_id)
  
  impact_df <- rbindlist(impact_issues_list) %>% 
    filter(uniprot_id == !!uniprot_id)
  
  # Generate plots for this specific protein
  plots_for_protein <- NULL
  if (nrow(feature_df) > 0) {
    tryCatch({
      plots_for_protein <- visualize_protein(feature_df, uniprot_id, 
                                             c_term_buffer = c_term_buffer,
                                             n_term_buffer = n_term_buffer)
    }, error = function(e) {
      cat("Error generating plots for", uniprot_id, ":", e$message, "\n")
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
  
  # Executive Summary
  doc <- add_executive_summary(doc, uniprot_df, impact_df)
  
  # Protein Summary Table (fixed width)
  doc <- add_protein_summary_table_fixed(doc, uniprot_df)
  
  # Terminal Analysis Tables (fixed width)
  doc <- add_terminal_analysis_tables_fixed(doc, uniprot_df, impact_df)
  
  # Feature Map Plot (generated fresh)
  doc <- add_feature_map_generated(doc, plots_for_protein, uniprot_id)
  
  # Feature Guide (properly formatted)
  doc <- add_feature_guide_formatted(doc)
  
  # Save the document
  output_file <- file.path(output_dir, paste0("Protein_Report_", uniprot_id, "_", Sys.Date(), ".docx"))
  print(doc, target = output_file)
  
  return(output_file)
}

# Add executive summary section
add_executive_summary <- function(doc, uniprot_df, impact_df) {
  
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
    "EXECUTIVE SUMMARY\n\n",
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

# Add protein summary table with fixed width
add_protein_summary_table_fixed <- function(doc, uniprot_df) {
  
  if (nrow(uniprot_df) == 0) {
    return(doc %>% body_add_par("No protein summary data available"))
  }
  
  # Create a transposed summary for better formatting
  summary_data <- data.frame(
    Property = c("UniProt ID", "Primary Accession", "Gene Name", 
                 "Protein Name", "Length (aa)", "Secreted", "Multimeric", "Preferred Terminus"),
    Value = c(
      uniprot_df$uniprot_id[1] %||% "Unknown",
      uniprot_df$primaryAccession[1] %||% "Unknown", 
      uniprot_df$genes_geneName[1] %||% "Unknown",
      substr(uniprot_df$protein_name[1] %||% "Unknown", 1, 60), # Truncate long names
      as.character(uniprot_df$protein_length[1] %||% "Unknown"),
      uniprot_df$Secreted[1] %||% "Unknown",
      uniprot_df$Multimeric[1] %||% "Unknown",
      uniprot_df$preferred_terminus[1] %||% "Unknown"
    ),
    stringsAsFactors = FALSE
  )
  
  # Create flextable with fixed width
  ft <- flextable(summary_data) %>%
    theme_vanilla() %>%
    set_table_properties(width = 1.0, layout = "fixed") %>%
    width(j = 1, width = 2.5) %>%  # Property column
    width(j = 2, width = 4.0) %>%  # Value column  
    set_caption("Protein Summary Information")
  
  doc %>%
    body_add_par("PROTEIN SUMMARY", style = "heading 1") %>%
    body_add_flextable(ft) %>%
    body_add_break()
}

# Add terminal analysis tables with fixed width
add_terminal_analysis_tables_fixed <- function(doc, uniprot_df, impact_df) {
  
  doc <- doc %>% body_add_par("TERMINAL ANALYSIS", style = "heading 1")
  
  # Terminal scores table
  if (nrow(uniprot_df) > 0) {
    scores_data <- data.frame(
      Terminus = c("N-terminal", "C-terminal"),
      Buffer = c(
        paste(uniprot_df$n_term_buffer[1] %||% "Unknown", "aa"),
        paste(uniprot_df$c_term_buffer[1] %||% "Unknown", "aa")
      ),
      Score = c(
        as.character(uniprot_df$n_terminal_score[1] %||% "Unknown"),
        as.character(uniprot_df$c_terminal_score[1] %||% "Unknown")
      ),
      stringsAsFactors = FALSE
    )
    
    ft_scores <- flextable(scores_data) %>%
      theme_vanilla() %>%
      set_table_properties(width = 0.8, layout = "fixed") %>%
      width(j = 1, width = 2.0) %>%
      width(j = 2, width = 1.5) %>%
      width(j = 3, width = 1.5) %>%
      set_caption("Terminal Tagging Scores")
    
    doc <- doc %>%
      body_add_par("Tagging Scores", style = "heading 2") %>%
      body_add_flextable(ft_scores)
    
    # Add recommendation
    recommendation <- uniprot_df$recommendation[1] %||% "No recommendation available"
    doc <- doc %>%
      body_add_par("Recommendation:", style = "heading 3") %>%
      body_add_par(recommendation, style = "Normal")
  }
  
  # Detailed impact analysis
  if (nrow(impact_df) > 0) {
    # First check what columns actually exist
    available_cols <- names(impact_df)
    
    # Select only columns that exist
    cols_to_select <- intersect(c("terminus", "feature_type", "position", "impact_level", "reason"), available_cols)
    
    if (length(cols_to_select) > 0) {
      impact_simple <- impact_df %>%
        select(all_of(cols_to_select))
      
      # Only truncate reason column if it exists
      if ("reason" %in% names(impact_simple)) {
        impact_simple <- impact_simple %>%
          mutate(reason = substr(reason, 1, 80))
      }
      
      if (nrow(impact_simple) > 0) {
        ft_impact <- flextable(impact_simple) %>%
          theme_vanilla() %>%
          set_table_properties(width = 1.0, layout = "fixed")
        
        # Set column widths based on what columns exist
        col_count <- ncol(impact_simple)
        if (col_count > 0) {
          col_width <- 6.5 / col_count  # Distribute width evenly
          for (j in 1:col_count) {
            ft_impact <- ft_impact %>% width(j = j, width = col_width)
          }
        }
        
        ft_impact <- ft_impact %>% set_caption("Terminal Impact Details")
        
        doc <- doc %>%
          body_add_par("Impact Details", style = "heading 2") %>%
          body_add_flextable(ft_impact)
      }
    } else {
      doc <- doc %>%
        body_add_par("Impact Details", style = "heading 2") %>%
        body_add_par("Impact data structure not recognized", style = "Normal")
    }
  } else {
    doc <- doc %>%
      body_add_par("Impact Details", style = "heading 2") %>%
      body_add_par("No terminal impact issues identified", style = "Normal")
  }
  
  doc %>% body_add_break()
}

# Add feature map with fresh plot generation
add_feature_map_generated <- function(doc, plots_object, uniprot_id) {
  
  doc <- doc %>% body_add_par("PROTEIN FEATURE MAP", style = "heading 1")
  
  tryCatch({
    # Check if plots were successfully generated
    if (!is.null(plots_object) && "feature_map" %in% names(plots_object)) {
      
      # Create temp file for image
      temp_png <- tempfile(fileext = ".png")
      
      # Try multiple methods to save the plot
      plot_saved <- FALSE
      
      # Method 1: Use plotly save_image (kaleido)
      if (!plot_saved && requireNamespace("plotly", quietly = TRUE)) {
        tryCatch({
          plotly::save_image(plots_object$feature_map, temp_png, 
                             width = 1000, height = 400, format = "png")
          plot_saved <- TRUE
        }, error = function(e) {
          cat("Method 1 failed:", e$message, "\n")
        })
      }
      
      # Method 2: Use htmlwidgets + webshot
      if (!plot_saved && requireNamespace("htmlwidgets", quietly = TRUE) && 
          requireNamespace("webshot", quietly = TRUE)) {
        tryCatch({
          # Save as HTML first
          temp_html <- tempfile(fileext = ".html")
          htmlwidgets::saveWidget(plots_object$feature_map, temp_html, selfcontained = TRUE)
          
          # Convert to PNG
          webshot::webshot(temp_html, temp_png, 
                           vwidth = 1000, vheight = 400,
                           delay = 2)
          
          # Clean up HTML
          if (file.exists(temp_html)) file.remove(temp_html)
          plot_saved <- TRUE
        }, error = function(e) {
          cat("Method 2 failed:", e$message, "\n")
        })
      }
      
      # Method 3: Try orca (legacy)
      if (!plot_saved && requireNamespace("plotly", quietly = TRUE)) {
        tryCatch({
          plotly::orca(plots_object$feature_map, temp_png, 
                       width = 1000, height = 400)
          plot_saved <- TRUE
        }, error = function(e) {
          cat("Method 3 failed:", e$message, "\n")
        })
      }
      
      # Add image to document if successful
      if (plot_saved && file.exists(temp_png)) {
        doc <- doc %>%
          body_add_img(src = temp_png, width = 6.5, height = 2.6) %>%
          body_add_par(paste("Feature visualization for protein", uniprot_id), 
                       style = "Normal")
        
        # Clean up
        file.remove(temp_png)
        
      } else {
        doc <- doc %>%
          body_add_par("Feature map could not be exported as image", style = "Normal") %>%
          body_add_par("The interactive plot is available in the web application", style = "Normal")
      }
      
    } else {
      doc <- doc %>%
        body_add_par("No feature data available for visualization", style = "Normal")
    }
    
  }, error = function(e) {
    doc <- doc %>%
      body_add_par(paste("Error generating feature map:", e$message), style = "Normal")
  })
  
  doc %>% body_add_break()
}

# Add properly formatted feature guide
add_feature_guide_formatted <- function(doc) {
  
  doc <- doc %>% 
    body_add_par("PROTEIN FEATURES GUIDE", style = "heading 1") %>%
    body_add_par("This guide explains the protein features shown in the analysis:", style = "Normal") %>%
    body_add_break()
  
  # Structural Features
  doc <- doc %>%
    body_add_par("STRUCTURAL FEATURES", style = "heading 2") %>%
    body_add_par("• Domain: Functional or structural protein domains", style = "Normal") %>%
    body_add_par("• Region: Regions of interest in the sequence", style = "Normal") %>%
    body_add_par("• Motif: Short functional sequence motifs", style = "Normal") %>%
    body_add_par("• Repeat: Repetitive sequence elements", style = "Normal") %>%
    body_add_par("• Coiled coil: Alpha-helical coiled-coil regions", style = "Normal") %>%
    body_add_break()
  
  # Functional Features  
  doc <- doc %>%
    body_add_par("FUNCTIONAL FEATURES", style = "heading 2") %>%
    body_add_par("• Active site: Catalytic or enzymatic sites", style = "Normal") %>%
    body_add_par("• Binding site: Ligand, DNA, RNA, or protein binding sites", style = "Normal") %>%
    body_add_par("• Metal binding: Metal ion coordination sites", style = "Normal") %>%
    body_add_par("• Site: Other functionally important locations", style = "Normal") %>%
    body_add_break()
  
  # Processing Features
  doc <- doc %>%
    body_add_par("PROCESSING FEATURES", style = "heading 2") %>%
    body_add_par("• Signal peptide: N-terminal localization sequences (removed)", style = "Normal") %>%
    body_add_par("• Transit peptide: Organelle targeting sequences (removed)", style = "Normal") %>%
    body_add_par("• Propeptide: Maturation sequences (removed during processing)", style = "Normal") %>%
    body_add_par("• Peptide: Peptide products or bioactive fragments", style = "Normal") %>%
    body_add_break()
  
  # Modifications
  doc <- doc %>%
    body_add_par("MODIFICATIONS", style = "heading 2") %>%
    body_add_par("• Modified residue: Post-translational modifications", style = "Normal") %>%
    body_add_par("• Glycosylation: Carbohydrate attachment sites", style = "Normal") %>%
    body_add_par("• Disulfide bond: Covalent bonds between cysteines", style = "Normal") %>%
    body_add_par("• Lipidation: Lipid modification sites", style = "Normal") %>%
    body_add_break()
  
  # Topological Features
  doc <- doc %>%
    body_add_par("MEMBRANE FEATURES", style = "heading 2") %>%
    body_add_par("• Transmembrane: Membrane-spanning regions", style = "Normal") %>%
    body_add_par("• Topological domain: Regions on one side of membrane", style = "Normal") %>%
    body_add_par("• Signal anchor: N-terminal membrane anchors", style = "Normal") %>%
    body_add_break()
  
  # Impact on Protein Design
  doc <- doc %>%
    body_add_par("IMPLICATIONS FOR PROTEIN TAGGING", style = "heading 2") %>%
    body_add_par("Features near protein termini can interfere with:", style = "Normal") %>%
    body_add_par("• Protein expression and folding", style = "Normal") %>%
    body_add_par("• Tag accessibility and functionality", style = "Normal") %>%
    body_add_par("• Protein purification efficiency", style = "Normal") %>%
    body_add_par("• Biological activity of the tagged protein", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_par("Processing features (signal peptides, propeptides) are particularly problematic as they are removed during protein maturation, taking any attached tags with them.", style = "Normal")
  
  return(doc)
}

# Simplified UI for report generation
proteinReportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Generate Protein Report"),
    div(
      style = "margin: 15px 0;",
      downloadButton(ns("download_report"), 
                     "Download Word Report", 
                     class = "btn-success"),
      br(), br(),
      helpText("Generates a comprehensive Word document with protein analysis results")
    )
  )
}

# Server function for report generation (updated)
proteinReportServer <- function(id, uniprot_id_reactive, consolidate_data_func, 
                                impact_issues_reactive, c_term_buffer_reactive = reactive(50), 
                                n_term_buffer_reactive = reactive(50)) {
  moduleServer(id, function(input, output, session) {
    
    output$download_report <- downloadHandler(
      filename = function() {
        uniprot_id <- uniprot_id_reactive()
        paste0("Protein_Report_", uniprot_id, "_", Sys.Date(), ".docx")
      },
      
      content = function(file) {
        # Generate the report with fresh plots
        uniprot_id <- uniprot_id_reactive()
        c_buffer <- c_term_buffer_reactive()
        n_buffer <- n_term_buffer_reactive()
        
        withProgress(message = 'Generating report...', value = 0, {
          
          incProgress(0.1, detail = "Collecting data...")
          
          # Generate report with plot generation built-in
          temp_file <- generate_protein_report(
            uniprot_id = uniprot_id,
            consolidate_data_func = consolidate_data_func,
            impact_issues_list = impact_issues_reactive(),
            c_term_buffer = c_buffer,
            n_term_buffer = n_buffer
          )
          
          incProgress(0.9, detail = "Finalizing document...")
          
          # Copy to download location
          file.copy(temp_file, file)
          
          incProgress(1, detail = "Complete!")
        })
      },
      
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
  })
}

# Usage in your main app:
# 
# # In UI (add to your individual protein analysis tab):
# proteinReportUI("protein_report")
# 
# # In Server:
# proteinReportServer("protein_report",
#                     reactive({ input$individual_uniprots }),
#                     consolidate_data,
#                     reactive({ values$impact_issues }),
#                     reactive({ list(feature_map = plots$feature_map) }))

# Helper function with null coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


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