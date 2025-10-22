# functions/plot_protein_features.R
# Enhanced protein features plot with epitopes overlay

library(plotly)
library(dplyr)

plot_protein_features_with_epitopes <- function(features_df, epitope_df = NULL, 
                                                uniprot_id = NULL, sequence_length = NULL, 
                                                c_term_buffer = 50, n_term_buffer = 50) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
    if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
      epitope_df <- epitope_df[epitope_df$uniprot_id == uniprot_id, ]
    }
  }
  
  # Get unique protein ID for title
  protein_id <- unique(features_df$uniprot_id)[1]
  
  # Estimate sequence length if not provided
  if (is.null(sequence_length)) {
    max_end <- max(features_df$end, na.rm = TRUE)
    sequence_length <- ifelse(is.finite(max_end), max_end, 1000)
  }
  
  if (nrow(features_df) == 0) {
    p <- plot_ly() %>%
      add_annotations(
        text = paste("No features found for", protein_id),
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16)
      ) %>%
      layout(title = paste("Protein Features:", protein_id))
    return(p)
  }
  
  # Define feature categories and their track positions
  feature_categories <- list(
    "Targeting_Secretion" = list(
      features = c("Signal", "Transit peptide", "Propeptide", "SIGNAL", "TRANSIT", "PROPEP"),
      track_start = 8,
      colors = c("#E74C3C", "#9B59B6", "#F39C12", "#27AE60", "#3498DB", "#E67E22")
    ),
    
    "Membrane_Topology" = list(
      features = c("Transmembrane", "Intramembrane", "Topological domain", "TRANSMEM", "INTRAMEM", "TOPO_DOM"),
      track_start = 7,
      colors = c("#1ABC9C", "#E91E63", "#8BC34A", "#FF5722", "#607D8B", "#795548")
    ),
    
    "Functional_Sites" = list(
      features = c("Domain", "Active site", "Binding site", "Site", "Metal", "DNA binding", 
                   "DOMAIN", "ACT_SITE", "BINDING", "SITE", "METAL", "DNA_BIND", "CA_BIND", "NP_BIND"),
      track_start = 6,
      colors = c("#2196F3", "#FF9800", "#4CAF50", "#9C27B0", "#F44336", "#00BCD4", "#FFEB3B", "#673AB7")
    ),
    
    "Structural_Elements" = list(
      features = c("Repeat", "Motif", "Coiled coil", "Zinc finger", "Region",
                   "REPEAT", "MOTIF", "COILED", "ZN_FING", "REGION"),
      track_start = 5,
      colors = c("#FF4081", "#00E676", "#FF6D00", "#7C4DFF", "#18FFFF")
    ),
    
    "Post_Translational_Modifications" = list(
      features = c("Modified residue", "Glycosylation", "Disulfide bond", "Lipidation", "Cross-link",
                   "MOD_RES", "CARBOHYD", "DISULFID", "LIPID", "CROSSLNK"),
      track_start = 4,
      colors = c("#CDDC39", "#FF1744", "#00C853", "#FF3D00", "#E040FB", "#1DE9B6")
    ),
    
    "Sequence_Variants" = list(
      features = c("Natural variant", "Mutagenesis", "Alternative sequence", "Sequence conflict",
                   "VARIANT", "MUTAGEN", "VAR_SEQ", "CONFLICT"),
      track_start = 3,
      colors = c("#536DFE", "#FFAB00", "#DD2C00", "#00B8D4", "#8E24AA", "#43A047", "#FB8C00", "#5E35B1")
    ),
    
    "Processing_Maturation" = list(
      features = c("Chain", "Peptide", "Initiator methionine", "Non-terminal residue",
                   "CHAIN", "PEPTIDE", "INIT_MET", "NON_TER"),
      track_start = 2,
      colors = c("#D4E157", "#26C6DA", "#EF5350", "#AB47BC", "#66BB6A", "#42A5F5", "#FFA726", "#26A69A")
    ),
    
    "Secondary_Structure" = list(
      features = c("Helix", "Beta strand", "Turn", "HELIX", "STRAND", "TURN"),
      track_start = 1,
      colors = c("#78909C", "#A1887F", "#90A4AE", "#BCAAA4", "#B0BEC5", "#D7CCC8")
    ),
    
    "Other_Features" = list(
      features = c(),
      track_start = 9,
      colors = c("#37474F")
    )
  )
  
  # Function to assign category, track position, and color
  assign_feature_category <- function(feature_type) {
    for (category_name in names(feature_categories)) {
      category <- feature_categories[[category_name]]
      feature_index <- which(category$features == feature_type)
      
      if (length(feature_index) > 0) {
        color_index <- feature_index[1]
        specific_color <- category$colors[color_index]
        
        return(list(
          category = category_name,
          track_start = category$track_start,
          color = specific_color
        ))
      }
    }
    return(list(
      category = "Other_Features",
      track_start = feature_categories$Other_Features$track_start,
      color = feature_categories$Other_Features$colors[1]
    ))
  }
  
  # Function to assign y-positions within tracks
  assign_y_positions_within_track <- function(features_subset) {
    if (nrow(features_subset) == 0) return(numeric(0))
    
    features_subset <- features_subset[!is.na(features_subset$start) & !is.na(features_subset$end), ]
    if (nrow(features_subset) == 0) return(numeric(0))
    
    features_subset <- features_subset[order(features_subset$start), ]
    features_subset$y_offset <- 0
    
    if (nrow(features_subset) > 1) {
      for (i in 2:nrow(features_subset)) {
        max_offset <- 0
        for (j in 1:(i-1)) {
          start_i <- features_subset$start[i]
          end_i <- features_subset$end[i]
          start_j <- features_subset$start[j]
          end_j <- features_subset$end[j]
          
          if (!is.na(start_i) && !is.na(end_i) && !is.na(start_j) && !is.na(end_j)) {
            if (end_j >= start_i && start_j <= end_i) {
              max_offset <- max(max_offset, features_subset$y_offset[j] + 1, na.rm = TRUE)
            }
          }
        }
        features_subset$y_offset[i] <- max_offset
      }
    }
    
    return(features_subset$y_offset)
  }
  
  # Add category information
  features_df$category <- NA
  features_df$track_start <- NA
  features_df$color <- NA
  features_df$y_offset <- 0
  
  for (i in 1:nrow(features_df)) {
    cat_info <- assign_feature_category(features_df$type[i])
    features_df$category[i] <- cat_info$category
    features_df$track_start[i] <- cat_info$track_start
    features_df$color[i] <- cat_info$color
  }
  
  # Assign y-positions
  for (category_name in unique(features_df$category)) {
    category_features <- features_df[features_df$category == category_name, ]
    if (nrow(category_features) > 0) {
      y_offsets <- assign_y_positions_within_track(category_features)
      if (length(y_offsets) > 0) {
        features_df[features_df$category == category_name, "y_offset"] <- y_offsets
      } else {
        features_df[features_df$category == category_name, "y_offset"] <- 0
      }
    }
  }
  
  features_df$y_pos <- features_df$track_start + (features_df$y_offset * 0.3)
  
  # Create hover text
  features_df$hover_text <- paste0(
    "Type: ", features_df$type, "<br>",
    "Category: ", gsub("_", " ", features_df$category), "<br>",
    "Position: ", features_df$start, "-", features_df$end, "<br>",
    "Length: ", features_df$end - features_df$start + 1, " aa"
  )
  
  # Calculate plot height
  max_y <- max(features_df$y_pos) + 1
  
  # Add space for epitope track at the top
  epitope_track_y <- max_y + 1
  if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
    max_y <- epitope_track_y + 1
  }
  
  # Category colors
  category_impact_colors <- list(
    "Targeting_Secretion" = "#dc3545",
    "Membrane_Topology" = "#dc3545",
    "Functional_Sites" = "#dc3545",
    "Structural_Elements" = "#fd7e14",
    "Post_Translational_Modifications" = "#fd7e14",
    "Sequence_Variants" = "#28a745",
    "Processing_Maturation" = "#fd7e14",
    "Secondary_Structure" = "#28a745",
    "Other_Features" = "#6c757d"
  )
  
  # Create track labels
  track_labels <- list()
  track_positions <- list()
  for (category_name in names(feature_categories)) {
    if (any(features_df$category == category_name)) {
      label_text <- gsub("_", " ", category_name)
      color <- category_impact_colors[[category_name]]
      colored_label <- paste0('<span style="color:', color, '; font-weight: bold;">', label_text, '</span>')
      track_labels <- c(track_labels, colored_label)
      track_positions <- c(track_positions, feature_categories[[category_name]]$track_start)
    }
  }
  
  # Add epitope track label if epitopes present
  if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
    track_labels <- c(track_labels, '<span style="color:#FF0000; font-weight: bold;">⭐ EPITOPES</span>')
    track_positions <- c(track_positions, epitope_track_y)
  }
  
  # Create plot
  p <- plot_ly() %>%
    layout(
      title = list(
        text = paste("Protein Features:", protein_id, "<br><sub>Length ~", sequence_length, "aa</sub>"),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Amino Acid Position",
        range = c(0, sequence_length),
        showgrid = TRUE,
        gridcolor = "#E5E5E5"
      ),
      yaxis = list(
        title = "Feature Categories",
        range = c(-0.5, max_y + 0.5),
        showticklabels = TRUE,
        ticktext = unlist(track_labels),
        tickvals = unlist(track_positions),
        showgrid = TRUE,
        gridcolor = "#F0F0F0"
      ),
      showlegend = TRUE,
      hovermode = "closest",
      height = 200 + max_y * 60,
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "#CCCCCC",
        borderwidth = 1
      )
    )
  
  # Add terminus shading
  n_term_end <- min(n_term_buffer, sequence_length)
  c_term_start <- max(1, sequence_length - c_term_buffer + 1)
  
  if (n_term_end > 0) {
    p <- p %>%
      add_trace(
        x = c(1, n_term_end, n_term_end, 1, 1),
        y = c(-0.5, -0.5, max_y + 0.5, max_y + 0.5, -0.5),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255, 200, 200, 0.3)",
        line = list(color = "rgba(255, 150, 150, 0.5)", width = 1),
        name = paste("N-terminus (", n_term_buffer, " aa)", sep = ""),
        hovertemplate = paste("N-terminus region: 1-", n_term_end, " aa<extra></extra>"),
        showlegend = TRUE
      )
  }
  
  if (c_term_start <= sequence_length) {
    p <- p %>%
      add_trace(
        x = c(c_term_start, sequence_length, sequence_length, c_term_start, c_term_start),
        y = c(-0.5, -0.5, max_y + 0.5, max_y + 0.5, -0.5),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255, 200, 200, 0.3)",
        line = list(color = "rgba(255, 150, 150, 0.5)", width = 1),
        name = paste("C-terminus (", c_term_buffer, " aa)", sep = ""),
        hovertemplate = paste("C-terminus region: ", c_term_start, "-", sequence_length, " aa<extra></extra>"),
        showlegend = TRUE
      )
  }
  
  # Add sequence backbone
  p <- p %>%
    add_trace(
      x = c(1, sequence_length),
      y = c(0, 0),
      type = "scatter",
      mode = "lines",
      line = list(color = "black", width = 4),
      name = "Protein Sequence",
      hovertemplate = paste("Sequence Length:", sequence_length, "aa<extra></extra>"),
      showlegend = FALSE
    )
  
  # Add category separators
  for (category_name in names(feature_categories)) {
    if (any(features_df$category == category_name)) {
      track_y <- feature_categories[[category_name]]$track_start
      p <- p %>%
        add_trace(
          x = c(0, sequence_length),
          y = c(track_y - 0.5, track_y - 0.5),
          type = "scatter",
          mode = "lines",
          line = list(color = "#CCCCCC", width = 1, dash = "dot"),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }
  
  # Add features
  for (category_name in unique(features_df$category)) {
    category_data <- features_df[features_df$category == category_name, ]
    category_display_name <- gsub("_", " ", category_name)
    
    p <- p %>%
      add_trace(
        x = c(NA), y = c(NA),
        type = "scatter", mode = "markers",
        marker = list(size = 0, color = "white"),
        name = paste("<b>", category_display_name, "</b>"),
        legendgroup = category_name,
        showlegend = TRUE,
        hoverinfo = "skip"
      )
    
    for (ft in unique(category_data$type)) {
      ft_data <- category_data[category_data$type == ft, ]
      
      for (i in 1:nrow(ft_data)) {
        feature <- ft_data[i, ]
        
        p <- p %>%
          add_trace(
            x = c(feature$start, feature$end, feature$end, feature$start, feature$start),
            y = c(feature$y_pos - 0.12, feature$y_pos - 0.12, feature$y_pos + 0.12, 
                  feature$y_pos + 0.12, feature$y_pos - 0.12),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = feature$color,
            line = list(width = 0),
            name = paste("  ", ft),
            legendgroup = category_name,
            showlegend = ifelse(i == 1, TRUE, FALSE),
            hovertemplate = paste0(feature$hover_text, "<extra></extra>")
          )
      }
    }
  }
  
  # ADD EPITOPES if available
  if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
    
    # Parse epitope positions
    epitope_df$start_pos <- NA
    epitope_df$end_pos <- NA
    epitope_df$is_linear <- FALSE
    
    for (i in 1:nrow(epitope_df)) {
      pos <- epitope_df$Position[i]
      epi_type <- epitope_df$Epitope_Type[i]
      
      if (!is.na(pos) && !is.na(epi_type)) {
        if (tolower(epi_type) == "linear" && grepl("-", pos)) {
          pos_parts <- strsplit(as.character(pos), "-")[[1]]
          if (length(pos_parts) == 2) {
            start <- suppressWarnings(as.numeric(pos_parts[1]))
            end <- suppressWarnings(as.numeric(pos_parts[2]))
            if (!is.na(start) && !is.na(end)) {
              epitope_df$start_pos[i] <- start
              epitope_df$end_pos[i] <- end
              epitope_df$is_linear[i] <- TRUE
            }
          }
        }
      }
    }
    
    # Plot linear epitopes
    linear_epitopes <- epitope_df[epitope_df$is_linear == TRUE, ]
    
    if (nrow(linear_epitopes) > 0) {
      # Add epitope separator line
      p <- p %>%
        add_trace(
          x = c(0, sequence_length),
          y = c(epitope_track_y - 0.5, epitope_track_y - 0.5),
          type = "scatter",
          mode = "lines",
          line = list(color = "#FF0000", width = 2, dash = "dot"),
          hoverinfo = "skip",
          showlegend = FALSE
        )
      
      # Add category header
      p <- p %>%
        add_trace(
          x = c(NA), y = c(NA),
          type = "scatter", mode = "markers",
          marker = list(size = 0, color = "white"),
          name = "<b>⭐ EPITOPES</b>",
          legendgroup = "epitopes",
          showlegend = TRUE,
          hoverinfo = "skip"
        )
      
      # Color by evidence level
      epitope_colors <- c("#FF6B6B", "#FFA500", "#FFD700", "#90EE90", "#00FF00")
      
      for (i in 1:nrow(linear_epitopes)) {
        epi <- linear_epitopes[i, ]
        
        evidence_level <- if(!is.na(epi$Evidence_Level)) epi$Evidence_Level else 1
        color_idx <- min(max(evidence_level, 1), 5)
        epi_color <- epitope_colors[color_idx]
        
        hover_text <- paste0(
          "<b>EPITOPE</b><br>",
          "Position: ", epi$start_pos, "-", epi$end_pos, "<br>",
          "Type: ", epi$Epitope_Type, "<br>",
          "Evidence: ", evidence_level, "/5<br>",
          "Context: ", epi$Antibody_Context, "<br>",
          "Citation: ", epi$Citation
        )
        
        p <- p %>%
          add_trace(
            x = c(epi$start_pos, epi$end_pos, epi$end_pos, epi$start_pos, epi$start_pos),
            y = c(epitope_track_y - 0.15, epitope_track_y - 0.15, epitope_track_y + 0.15, 
                  epitope_track_y + 0.15, epitope_track_y - 0.15),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = epi_color,
            line = list(color = "darkred", width = 2),
            name = paste0("  Epitope (", epi$start_pos, "-", epi$end_pos, ")"),
            legendgroup = "epitopes",
            showlegend = ifelse(i == 1, TRUE, FALSE),
            hovertemplate = paste0(hover_text, "<extra></extra>")
          )
        
        # Add star for high-value epitopes
        if (evidence_level >= 4) {
          p <- p %>%
            add_trace(
              x = (epi$start_pos + epi$end_pos) / 2,
              y = epitope_track_y + 0.3,
              type = "scatter",
              mode = "markers",
              marker = list(
                symbol = "star",
                size = 12,
                color = "#FFD700",
                line = list(color = "darkred", width = 1)
              ),
              name = "High-value epitope",
              legendgroup = "epitopes",
              showlegend = FALSE,
              hoverinfo = "skip"
            )
        }
      }
    }
    
    # Note for conformational epitopes
    conformational_count <- sum(!epitope_df$is_linear)
    if (conformational_count > 0) {
      p <- p %>%
        add_annotations(
          x = sequence_length * 0.98,
          y = epitope_track_y,
          text = paste0(conformational_count, " conformational epitope(s) not shown"),
          showarrow = FALSE,
          xanchor = "right",
          font = list(size = 10, color = "#666666"),
          bgcolor = "rgba(255,255,255,0.8)"
        )
    }
  }
  
  return(p)
}