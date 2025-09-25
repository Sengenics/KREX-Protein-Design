# Enhanced protein features plot with categorized tracks
# Copy this function into your functions file

plot_protein_features <- function(features_df, uniprot_id = NULL, sequence_length = NULL) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  # Get unique protein ID for title
  protein_id <- unique(features_df$uniprot_id)[1]
  
  # Estimate sequence length if not provided
  if (is.null(sequence_length)) {
    # Use max end position, but handle case where all ends are NA
    max_end <- max(features_df$end, na.rm = TRUE)
    sequence_length <- ifelse(is.finite(max_end), max_end + 50, 1000)  # Default to 1000 if no valid ends
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
    
    # Space for additional/unknown types
    "Other_Features" = list(
      features = c(),  # Will capture anything not in above categories
      track_start = 9,
      colors = c("#37474F")  # Single color for unknown features
    )
  )
  
  # Function to assign category, track position, and specific color
  assign_feature_category <- function(feature_type) {
    for (category_name in names(feature_categories)) {
      category <- feature_categories[[category_name]]
      feature_index <- which(category$features == feature_type)
      
      if (length(feature_index) > 0) {
        # Get the specific color for this feature
        color_index <- feature_index[1]  # Use first match if duplicates exist
        specific_color <- category$colors[color_index]
        
        return(list(
          category = category_name,
          track_start = category$track_start,
          color = specific_color
        ))
      }
    }
    # If not found in any category, assign to "Other_Features"
    return(list(
      category = "Other_Features",
      track_start = feature_categories$Other_Features$track_start,
      color = feature_categories$Other_Features$colors[1]
    ))
  }
  
  # Function to assign y-positions within tracks to avoid overlap
  assign_y_positions_within_track <- function(features_subset) {
    if (nrow(features_subset) == 0) return(numeric(0))
    
    # Remove rows with NA start or end positions
    features_subset <- features_subset[!is.na(features_subset$start) & !is.na(features_subset$end), ]
    
    if (nrow(features_subset) == 0) return(numeric(0))
    
    # Sort by start position
    features_subset <- features_subset[order(features_subset$start), ]
    
    # Initialize positions
    features_subset$y_offset <- 0
    
    # Assign non-overlapping positions within the track
    if (nrow(features_subset) > 1) {
      for (i in 2:nrow(features_subset)) {
        max_offset <- 0
        for (j in 1:(i-1)) {
          # Check if features overlap (with NA checks)
          start_i <- features_subset$start[i]
          end_i <- features_subset$end[i]
          start_j <- features_subset$start[j]
          end_j <- features_subset$end[j]
          
          # Only check overlap if all values are not NA
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
  
  # Add category information and positions to features
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
  
  # Assign y-positions within each category to avoid overlaps
  for (category_name in unique(features_df$category)) {
    category_features <- features_df[features_df$category == category_name, ]
    if (nrow(category_features) > 0) {
      y_offsets <- assign_y_positions_within_track(category_features)
      
      # Handle case where function returns empty vector due to all NA values
      if (length(y_offsets) > 0) {
        features_df[features_df$category == category_name, "y_offset"] <- y_offsets
      } else {
        # Assign default positions if all positions were NA
        features_df[features_df$category == category_name, "y_offset"] <- 0
      }
    }
  }
  
  # Calculate final y positions
  features_df$y_pos <- features_df$track_start + (features_df$y_offset * 0.3)
  
  # Create hover text
  features_df$hover_text <- paste0(
    "Type: ", features_df$type, "<br>",
    "Category: ", gsub("_", " ", features_df$category), "<br>",
    "Position: ", features_df$start, "-", features_df$end, "<br>",
    "Length: ", features_df$length, " aa"
  )
  
  # Calculate plot height based on max y position
  max_y <- max(features_df$y_pos) + 1
  
  # Create track labels
  track_labels <- list()
  track_positions <- list()
  for (category_name in names(feature_categories)) {
    if (any(features_df$category == category_name)) {
      track_labels <- c(track_labels, gsub("_", " ", category_name))
      track_positions <- c(track_positions, feature_categories[[category_name]]$track_start)
    }
  }
  
  # Create the plot
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
  
  # Add category separator lines
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
  
  # Add features grouped by category with legend grouping
  legend_counter <- 0
  
  for (category_name in unique(features_df$category)) {
    category_data <- features_df[features_df$category == category_name, ]
    category_display_name <- gsub("_", " ", category_name)
    
    # Add category header (invisible trace for legend grouping)
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
    legend_counter <- legend_counter + 1
    
    for (ft in unique(category_data$type)) {
      ft_data <- category_data[category_data$type == ft, ]
      first_feature <- ft_data[1, ]
      
      # Add rectangles for this feature type
      for (i in 1:nrow(ft_data)) {
        feature <- ft_data[i, ]
        
        p <- p %>%
          add_trace(
            x = c(feature$start, feature$end, feature$end, feature$start, feature$start),
            y = c(feature$y_pos - 0.12, feature$y_pos - 0.12, feature$y_pos + 0.12, 
                  feature$y_pos + 0.12, feature$y_pos - 0.12),
            type = "scatter",
            mode = "lines",
            fill = "toself",
            fillcolor = feature$color,
            line = list(color = darken_color(feature$color), width = 2),
            name = paste("  ", ft),  # Indent to show it's under category
            legendgroup = category_name,
            showlegend = ifelse(i == 1, TRUE, FALSE),
            hovertemplate = paste0(feature$hover_text, "<extra></extra>")
          )
      }
      legend_counter <- legend_counter + 1
    }
    
    # Add spacing between categories in legend
    legend_counter <- legend_counter + 0.5
  }
  
  return(p)
}

# Helper function to darken colors for borders
darken_color <- function(color, factor = 0.7) {
  col_rgb <- col2rgb(color)
  new_rgb <- col_rgb * factor
  rgb(new_rgb[1], new_rgb[2], new_rgb[3], maxColorValue = 255)
}

# Helper function to create a summary view for protein design
create_protein_design_summary <- function(features_df, uniprot_id = NULL) {
  
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  # Count features by design-relevant categories
  summary <- data.frame(
    Category = c(
      "Secretion Signals", "Membrane Features", "Functional Sites", 
      "Structural Elements", "Modifications", "Sequence Issues"
    ),
    Count = c(
      sum(features_df$type %in% c("Signal", "Transit peptide", "Propeptide", "SIGNAL", "TRANSIT", "PROPEP")),
      sum(features_df$type %in% c("Transmembrane", "Intramembrane", "Topological domain", "TRANSMEM", "INTRAMEM", "TOPO_DOM")),
      sum(features_df$type %in% c("Domain", "Active site", "Binding site", "Site", "DOMAIN", "ACT_SITE", "BINDING", "SITE")),
      sum(features_df$type %in% c("Repeat", "Motif", "Coiled coil", "Zinc finger", "REPEAT", "MOTIF", "COILED", "ZN_FING")),
      sum(features_df$type %in% c("Modified residue", "Glycosylation", "Disulfide bond", "MOD_RES", "CARBOHYD", "DISULFID")),
      sum(features_df$type %in% c("Natural variant", "Sequence conflict", "VARIANT", "CONFLICT"))
    ),
    Design_Impact = c(
      "HIGH - Affects targeting", "HIGH - Membrane integration", "HIGH - Function", 
      "MEDIUM - Stability", "MEDIUM - Processing", "LOW - Natural variation"
    ),
    stringsAsFactors = FALSE
  )
  
  return(summary)
}