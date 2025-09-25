# feature_visualization_direct.R
# Visualization functions for existing features_df dataframe

library(plotly)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)

# Function to create interactive feature plot from your features_df
# plot_protein_features <- function(features_df, uniprot_id = NULL, sequence_length = NULL) {
#   
#   # Filter for specific protein if provided
#   if (!is.null(uniprot_id)) {
#     features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
#   }
#   
#   # Get unique protein ID for title
#   protein_id <- unique(features_df$uniprot_id)[1]
#   
#   # Estimate sequence length if not provided
#   if (is.null(sequence_length)) {
#     sequence_length <- max(features_df$end, na.rm = TRUE) + 50
#   }
#   
#   if (nrow(features_df) == 0) {
#     p <- plot_ly() %>%
#       add_annotations(
#         text = paste("No features found for", protein_id),
#         x = 0.5, y = 0.5,
#         xref = "paper", yref = "paper",
#         showarrow = FALSE,
#         font = list(size = 16)
#       ) %>%
#       layout(title = paste("Protein Features:", protein_id))
#     return(p)
#   }
#   
#   # Define colors for different feature types
#   feature_types <- unique(features_df$type)
#   colors <- rainbow(length(feature_types), alpha = 0.7)
#   names(colors) <- feature_types
#   
#   # Add colors and y-positions
#   features_df <- features_df %>%
#     mutate(
#       color = colors[type],
#       y_pos = assign_y_positions_simple(start, end, type),
#       # hover_text = paste0(
#       #   "Type: ", type, "<br>",
#       #   "Position: ", start, "-", end, "<br>",
#       #   "Length: ", length, " aa<br>",
#       #   "UniProt ID: ", uniprot_id
#       # ),
#       hover_text = paste0(
#         "Type: ", type, "<br>",
#         "Length: ", length
#       )
#     )
#   
#   # Create the plot
#   p <- plot_ly() %>%
#     layout(
#       title = paste("Protein Features:", protein_id, "(Length ~", sequence_length, "aa)"),
#       xaxis = list(
#         title = "Amino Acid Position",
#         range = c(0, sequence_length),
#         showgrid = TRUE
#       ),
#       yaxis = list(
#         title = "Features",
#         showticklabels = TRUE,
#         ticktext = unique(features_df$type),
#         tickvals = 1:length(unique(features_df$type)),
#         showgrid = FALSE
#       ),
#       showlegend = TRUE,
#       hovermode = "closest",
#       height = 400 + length(feature_types) * 30
#     )
#   
#   # Add sequence backbone
#   p <- p %>%
#     add_trace(
#       x = c(1, sequence_length),
#       y = c(0, 0),
#       type = "scatter",
#       mode = "lines",
#       line = list(color = "black", width = 3),
#       name = "Sequence",
#       hoverinfo = "skip",
#       showlegend = FALSE
#     )
#   
#   # Add features grouped by type
#   for (ft in feature_types) {
#     ft_data <- features_df[features_df$type == ft, ]
#     
#     # Add rectangles for this feature type
#     for (i in 1:nrow(ft_data)) {
#       feature <- ft_data[i, ]
#       
#       p <- p %>%
#         add_trace(
#           x = c(feature$start, feature$end, feature$end, feature$start, feature$start),
#           y = c(feature$y_pos - 0.2, feature$y_pos - 0.2, feature$y_pos + 0.2, feature$y_pos + 0.2, feature$y_pos - 0.2),
#           type = "scatter",
#           mode = "lines",
#           fill = "toself",
#           fillcolor = feature$color,
#           line = list(color = feature$color, width = 2),
#           name = ft,
#           legendgroup = ft,
#           showlegend = ifelse(i == 1, TRUE, FALSE),
#           hoverinfo = "text",
#           text = feature$hover_text
#         )
#     }
#   }
#   
#   return(p)
# }

# Enhanced protein features plot with categorized tracks
# Copy this function into your functions file

# Enhanced protein features plot with categorized tracks
# Copy this function into your functions file

# plot_protein_features <- function(features_df, uniprot_id = NULL, sequence_length = NULL) {
#   
#   # Filter for specific protein if provided
#   if (!is.null(uniprot_id)) {
#     features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
#   }
#   
#   # Get unique protein ID for title
#   protein_id <- unique(features_df$uniprot_id)[1]
#   
#   # Estimate sequence length if not provided
#   if (is.null(sequence_length)) {
#     # Use max end position, but handle case where all ends are NA
#     max_end <- max(features_df$end, na.rm = TRUE)
#     sequence_length <- ifelse(is.finite(max_end), max_end + 50, 1000)  # Default to 1000 if no valid ends
#   }
#   
#   if (nrow(features_df) == 0) {
#     p <- plot_ly() %>%
#       add_annotations(
#         text = paste("No features found for", protein_id),
#         x = 0.5, y = 0.5,
#         xref = "paper", yref = "paper",
#         showarrow = FALSE,
#         font = list(size = 16)
#       ) %>%
#       layout(title = paste("Protein Features:", protein_id))
#     return(p)
#   }
#   
#   # Define feature categories and their track positions
#   feature_categories <- list(
#     "Targeting_Secretion" = list(
#       features = c("Signal", "Transit peptide", "Propeptide", "SIGNAL", "TRANSIT", "PROPEP"),
#       track_start = 8,
#       color_base = "#E74C3C"  # Red
#     ),
#     
#     "Membrane_Topology" = list(
#       features = c("Transmembrane", "Intramembrane", "Topological domain", "TRANSMEM", "INTRAMEM", "TOPO_DOM"),
#       track_start = 7,
#       color_base = "#3498DB"  # Blue
#     ),
#     
#     "Functional_Sites" = list(
#       features = c("Domain", "Active site", "Binding site", "Site", "Metal", "DNA binding", 
#                    "DOMAIN", "ACT_SITE", "BINDING", "SITE", "METAL", "DNA_BIND", "CA_BIND", "NP_BIND"),
#       track_start = 6,
#       color_base = "#27AE60"  # Green
#     ),
#     
#     "Structural_Elements" = list(
#       features = c("Repeat", "Motif", "Coiled coil", "Zinc finger", "Region",
#                    "REPEAT", "MOTIF", "COILED", "ZN_FING", "REGION"),
#       track_start = 5,
#       color_base = "#F39C12"  # Orange
#     ),
#     
#     "Post_Translational_Modifications" = list(
#       features = c("Modified residue", "Glycosylation", "Disulfide bond", "Lipidation", "Cross-link",
#                    "MOD_RES", "CARBOHYD", "DISULFID", "LIPID", "CROSSLNK"),
#       track_start = 4,
#       color_base = "#9B59B6"  # Purple
#     ),
#     
#     "Sequence_Variants" = list(
#       features = c("Natural variant", "Mutagenesis", "Alternative sequence", "Sequence conflict",
#                    "VARIANT", "MUTAGEN", "VAR_SEQ", "CONFLICT"),
#       track_start = 3,
#       color_base = "#E67E22"  # Dark Orange
#     ),
#     
#     "Processing_Maturation" = list(
#       features = c("Chain", "Peptide", "Initiator methionine", "Non-terminal residue",
#                    "CHAIN", "PEPTIDE", "INIT_MET", "NON_TER"),
#       track_start = 2,
#       color_base = "#1ABC9C"  # Teal
#     ),
#     
#     "Secondary_Structure" = list(
#       features = c("Helix", "Beta strand", "Turn", "HELIX", "STRAND", "TURN"),
#       track_start = 1,
#       color_base = "#95A5A6"  # Gray
#     ),
#     
#     # Space for additional/unknown types
#     "Other_Features" = list(
#       features = c(),  # Will capture anything not in above categories
#       track_start = 9,
#       color_base = "#34495E"  # Dark Gray
#     )
#   )
#   
#   # Function to assign category and track position
#   assign_feature_category <- function(feature_type) {
#     for (category_name in names(feature_categories)) {
#       category <- feature_categories[[category_name]]
#       if (feature_type %in% category$features) {
#         return(list(
#           category = category_name,
#           track_start = category$track_start,
#           color_base = category$color_base
#         ))
#       }
#     }
#     # If not found in any category, assign to "Other_Features"
#     return(list(
#       category = "Other_Features",
#       track_start = feature_categories$Other_Features$track_start,
#       color_base = feature_categories$Other_Features$color_base
#     ))
#   }
#   
#   # Function to assign y-positions within tracks to avoid overlap
#   assign_y_positions_within_track <- function(features_subset) {
#     if (nrow(features_subset) == 0) return(numeric(0))
#     
#     # Remove rows with NA start or end positions
#     features_subset <- features_subset[!is.na(features_subset$start) & !is.na(features_subset$end), ]
#     
#     if (nrow(features_subset) == 0) return(numeric(0))
#     
#     # Sort by start position
#     features_subset <- features_subset[order(features_subset$start), ]
#     
#     # Initialize positions
#     features_subset$y_offset <- 0
#     
#     # Assign non-overlapping positions within the track
#     if (nrow(features_subset) > 1) {
#       for (i in 2:nrow(features_subset)) {
#         max_offset <- 0
#         for (j in 1:(i-1)) {
#           # Check if features overlap (with NA checks)
#           start_i <- features_subset$start[i]
#           end_i <- features_subset$end[i]
#           start_j <- features_subset$start[j]
#           end_j <- features_subset$end[j]
#           
#           # Only check overlap if all values are not NA
#           if (!is.na(start_i) && !is.na(end_i) && !is.na(start_j) && !is.na(end_j)) {
#             if (end_j >= start_i && start_j <= end_i) {
#               max_offset <- max(max_offset, features_subset$y_offset[j] + 1, na.rm = TRUE)
#             }
#           }
#         }
#         features_subset$y_offset[i] <- max_offset
#       }
#     }
#     
#     return(features_subset$y_offset)
#   }
#   
#   # Add category information and positions to features
#   features_df$category <- NA
#   features_df$track_start <- NA
#   features_df$color_base <- NA
#   features_df$y_offset <- 0
#   
#   for (i in 1:nrow(features_df)) {
#     cat_info <- assign_feature_category(features_df$type[i])
#     features_df$category[i] <- cat_info$category
#     features_df$track_start[i] <- cat_info$track_start
#     features_df$color_base[i] <- cat_info$color_base
#   }
#   
#   # Assign y-positions within each category to avoid overlaps
#   for (category_name in unique(features_df$category)) {
#     category_features <- features_df[features_df$category == category_name, ]
#     if (nrow(category_features) > 0) {
#       y_offsets <- assign_y_positions_within_track(category_features)
#       
#       # Handle case where function returns empty vector due to all NA values
#       if (length(y_offsets) > 0) {
#         features_df[features_df$category == category_name, "y_offset"] <- y_offsets
#       } else {
#         # Assign default positions if all positions were NA
#         features_df[features_df$category == category_name, "y_offset"] <- 0
#       }
#     }
#   }
#   
#   # Calculate final y positions
#   features_df$y_pos <- features_df$track_start + (features_df$y_offset * 0.3)
#   
#   # Generate colors with variations for multiple features in same category
#   features_df$color <- mapply(function(base_color, offset) {
#     # Vary saturation/lightness based on offset
#     col_rgb <- col2rgb(base_color)
#     # Adjust brightness
#     factor <- 1 - (offset * 0.1)
#     factor <- max(0.6, min(1, factor))  # Keep within reasonable range
#     new_rgb <- col_rgb * factor
#     rgb(new_rgb[1], new_rgb[2], new_rgb[3], maxColorValue = 255, alpha = 180)
#   }, features_df$color_base, features_df$y_offset)
#   
#   # Create hover text
#   features_df$hover_text <- paste0(
#     "Type: ", features_df$type, "<br>",
#     "Category: ", gsub("_", " ", features_df$category), "<br>",
#     "Position: ", features_df$start, "-", features_df$end, "<br>",
#     "Length: ", features_df$length, " aa"
#   )
#   
#   # Calculate plot height based on max y position
#   max_y <- max(features_df$y_pos) + 1
#   
#   # Create track labels
#   track_labels <- list()
#   track_positions <- list()
#   for (category_name in names(feature_categories)) {
#     if (any(features_df$category == category_name)) {
#       track_labels <- c(track_labels, gsub("_", " ", category_name))
#       track_positions <- c(track_positions, feature_categories[[category_name]]$track_start)
#     }
#   }
#   
#   # Create the plot
#   p <- plot_ly() %>%
#     layout(
#       title = list(
#         text = paste("Protein Features:", protein_id, "<br><sub>Length ~", sequence_length, "aa</sub>"),
#         font = list(size = 16)
#       ),
#       xaxis = list(
#         title = "Amino Acid Position",
#         range = c(0, sequence_length),
#         showgrid = TRUE,
#         gridcolor = "#E5E5E5"
#       ),
#       yaxis = list(
#         title = "Feature Categories",
#         range = c(-0.5, max_y + 0.5),
#         showticklabels = TRUE,
#         ticktext = unlist(track_labels),
#         tickvals = unlist(track_positions),
#         showgrid = TRUE,
#         gridcolor = "#F0F0F0"
#       ),
#       showlegend = TRUE,
#       hovermode = "closest",
#       height = 200 + max_y * 60,
#       legend = list(
#         orientation = "h",
#         x = 0,
#         y = -0.2
#       )
#     )
#   
#   # Add sequence backbone
#   p <- p %>%
#     add_trace(
#       x = c(1, sequence_length),
#       y = c(0, 0),
#       type = "scatter",
#       mode = "lines",
#       line = list(color = "black", width = 4),
#       name = "Protein Sequence",
#       hovertemplate = paste("Sequence Length:", sequence_length, "aa<extra></extra>"),
#       showlegend = FALSE
#     )
#   
#   # Add category separator lines
#   for (category_name in names(feature_categories)) {
#     if (any(features_df$category == category_name)) {
#       track_y <- feature_categories[[category_name]]$track_start
#       p <- p %>%
#         add_trace(
#           x = c(0, sequence_length),
#           y = c(track_y - 0.5, track_y - 0.5),
#           type = "scatter",
#           mode = "lines",
#           line = list(color = "#CCCCCC", width = 1, dash = "dot"),
#           hoverinfo = "skip",
#           showlegend = FALSE
#         )
#     }
#   }
#   
#   # Add features grouped by category and type
#   for (category_name in unique(features_df$category)) {
#     category_data <- features_df[features_df$category == category_name, ]
#     
#     for (ft in unique(category_data$type)) {
#       ft_data <- category_data[category_data$type == ft, ]
#       
#       # Add rectangles for this feature type
#       for (i in 1:nrow(ft_data)) {
#         feature <- ft_data[i, ]
#         
#         p <- p %>%
#           add_trace(
#             x = c(feature$start, feature$end, feature$end, feature$start, feature$start),
#             y = c(feature$y_pos - 0.12, feature$y_pos - 0.12, feature$y_pos + 0.12, 
#                   feature$y_pos + 0.12, feature$y_pos - 0.12),
#             type = "scatter",
#             mode = "lines",
#             fill = "toself",
#             fillcolor = feature$color,
#             line = list(color = feature$color, width = 1),
#             name = paste0(gsub("_", " ", category_name), ": ", ft),
#             legendgroup = paste0(category_name, "_", ft),
#             showlegend = ifelse(i == 1, TRUE, FALSE),
#             hovertemplate = paste0(feature$hover_text, "<extra></extra>")
#           )
#       }
#     }
#   }
#   
#   return(p)
# }
# 
# # Helper function to create a summary view for protein design
# create_protein_design_summary <- function(features_df, uniprot_id = NULL) {
#   
#   if (!is.null(uniprot_id)) {
#     features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
#   }
#   
#   # Count features by design-relevant categories
#   summary <- data.frame(
#     Category = c(
#       "Secretion Signals", "Membrane Features", "Functional Sites", 
#       "Structural Elements", "Modifications", "Sequence Issues"
#     ),
#     Count = c(
#       sum(features_df$type %in% c("Signal", "Transit peptide", "Propeptide", "SIGNAL", "TRANSIT", "PROPEP")),
#       sum(features_df$type %in% c("Transmembrane", "Intramembrane", "Topological domain", "TRANSMEM", "INTRAMEM", "TOPO_DOM")),
#       sum(features_df$type %in% c("Domain", "Active site", "Binding site", "Site", "DOMAIN", "ACT_SITE", "BINDING", "SITE")),
#       sum(features_df$type %in% c("Repeat", "Motif", "Coiled coil", "Zinc finger", "REPEAT", "MOTIF", "COILED", "ZN_FING")),
#       sum(features_df$type %in% c("Modified residue", "Glycosylation", "Disulfide bond", "MOD_RES", "CARBOHYD", "DISULFID")),
#       sum(features_df$type %in% c("Natural variant", "Sequence conflict", "VARIANT", "CONFLICT"))
#     ),
#     Design_Impact = c(
#       "HIGH - Affects targeting", "HIGH - Membrane integration", "HIGH - Function", 
#       "MEDIUM - Stability", "MEDIUM - Processing", "LOW - Natural variation"
#     ),
#     stringsAsFactors = FALSE
#   )
#   
#   return(summary)
# }
# 
# # Helper function to assign y-positions to avoid overlap
# assign_y_positions_simple <- function(starts, ends, types) {
#   n <- length(starts)
#   if (n == 0) return(numeric(0))
#   
#   # Initialize positions
#   positions <- rep(1, n)
#   
#   # Simple approach: for each feature, find a non-overlapping y-level
#   for (i in 1:n) {
#     current_start <- starts[i]
#     current_end <- ends[i]
#     
#     # Find the lowest available y-position
#     y_level <- 1
#     overlap_found <- TRUE
#     
#     while (overlap_found && y_level <= 20) {  # Max 20 levels
#       overlap_found <- FALSE
#       
#       # Check if this y-level conflicts with any previous features
#       if (i > 1) {  # Only check if there are previous features
#         for (j in 1:(i-1)) {
#           if (positions[j] == y_level) {
#             # Check for overlap
#             prev_start <- starts[j]
#             prev_end <- ends[j]
#             
#             # Features overlap if they intersect
#             if (!(current_end < prev_start || current_start > prev_end)) {
#               overlap_found <- TRUE
#               break
#             }
#           }
#         }
#       }
#       
#       if (overlap_found) {
#         y_level <- y_level + 1
#       } else {
#         positions[i] <- y_level
#         overlap_found <- FALSE
#       }
#     }
#   }
#   
#   return(positions)
# }

# Create a cleaner domain architecture plot
plot_domain_architecture <- function(features_df, uniprot_id = NULL) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  protein_id <- unique(features_df$uniprot_id)[1]
  sequence_length <- max(features_df$end, na.rm = TRUE)
  
  # Filter for major structural features
  major_features <- c("Signal", "Chain", "Domain", "Topological domain", "Transmembrane", "Region")
  domain_data <- features_df[features_df$type %in% major_features, ]
  
  if (nrow(domain_data) == 0) {
    p <- ggplot() + 
      annotate("text", x = sequence_length/2, y = 0.5, 
               label = "No major structural features found", size = 5) +
      xlim(0, sequence_length) +
      ylim(0, 1) +
      theme_void() +
      ggtitle(paste("Domain Architecture:", protein_id))
    return(ggplotly(p))
  }
  
  # Create the ggplot
  p <- ggplot(domain_data) +
    # Sequence backbone
    geom_segment(aes(x = 1, xend = sequence_length, y = 0.5, yend = 0.5), 
                 size = 3, color = "gray30") +
    # Feature rectangles
    geom_rect(aes(xmin = start, xmax = end, ymin = 0.3, ymax = 0.7,
                  fill = type, color = type),
              alpha = 0.8, size = 1) +
    # Feature labels
    geom_text(aes(x = (start + end)/2, y = 0.5, label = type),
              size = 3, angle = 0, color = "white", fontface = "bold") +
    scale_fill_viridis_d(option = "plasma", alpha = 0.8) +
    scale_color_viridis_d(option = "plasma") +
    xlim(0, sequence_length + 50) +
    ylim(0.2, 0.8) +
    labs(
      title = paste("Domain Architecture:", protein_id),
      subtitle = paste("Sequence length:", sequence_length, "amino acids"),
      x = "Amino Acid Position",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.y = element_blank(),
      panel.grid.minor.x = element_line(color = "gray90", size = 0.3),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  return(ggplotly(p, tooltip = c("fill", "xmin", "xmax")))
}

# Create feature summary table
summarize_features <- function(features_df, uniprot_id = NULL) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  if (nrow(features_df) == 0) {
    return(data.frame(Message = "No features found"))
  }
  
  summary_df <- features_df %>%
    group_by(type) %>%
    summarise(
      Count = n(),
      `Avg Length` = round(mean(length, na.rm = TRUE), 1),
      `Total Coverage` = sum(length, na.rm = TRUE),
      `Position Range` = paste(min(start, na.rm = TRUE), "-", max(end, na.rm = TRUE)),
      `Example Positions` = paste(head(paste0(start, "-", end), 3), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(Count))
  
  return(summary_df)
}

# Create a stacked bar chart showing feature distribution
plot_feature_distribution <- function(features_df) {
  
  if (nrow(features_df) == 0) {
    return(plot_ly() %>% add_annotations(text = "No data", x = 0.5, y = 0.5))
  }
  
  # Count features by protein and type
  feature_counts <- features_df %>%
    group_by(uniprot_id, type) %>%
    summarise(count = n(), .groups = "drop")
  
  p <- ggplot(feature_counts, aes(x = uniprot_id, y = count, fill = type)) +
    geom_col(position = "stack", alpha = 0.8) +
    scale_fill_viridis_d(option = "turbo") +
    labs(
      title = "Feature Distribution Across Proteins",
      x = "UniProt ID",
      y = "Number of Features",
      fill = "Feature Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  return(ggplotly(p))
}

# Create an interactive length distribution plot
plot_feature_lengths <- function(features_df, uniprot_id = NULL) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  if (nrow(features_df) == 0) {
    return(plot_ly() %>% add_annotations(text = "No data", x = 0.5, y = 0.5))
  }
  
  p <- plot_ly(features_df, 
               x = ~type, 
               y = ~length,
               color = ~type,
               colors = "Set3",
               type = "box",
               hovertemplate = paste0(
                 "Type: %{x}<br>",
                 "Length: %{y} aa<br>",
                 "Position: %{customdata}<br>",
                 "<extra></extra>"
               ),
               customdata = ~paste0(start, "-", end)) %>%
    layout(
      title = list(
        text = ifelse(is.null(uniprot_id), 
                      "Feature Length Distribution", 
                      paste("Feature Lengths:", uniprot_id)),
        x = 0.05
      ),
      xaxis = list(title = "Feature Type"),
      yaxis = list(title = "Length (amino acids)"),
      showlegend = FALSE
    )
  
  return(p)
}

# Quick visualization function - all plots for one protein
visualize_protein <- function(features_df, uniprot_id, sequence_length = NULL, 
                              c_term_buffer = 30,
                              n_term_buffer = 30) {
  
  cat("Creating visualizations for", uniprot_id, "...\n")
  
  # Filter data
  protein_features <- features_df[features_df$uniprot_id == uniprot_id, ]
  
  if (nrow(protein_features) == 0) {
    cat("No features found for", uniprot_id, "\n")
    return(NULL)
  }
  
  cat("Found", nrow(protein_features), "features\n")
  
  # Create plots
  plots <- list(
    feature_map = plot_protein_features(protein_features, uniprot_id, sequence_length, 
                                        c_term_buffer = c_term_buffer,
                                        n_term_buffer = n_term_buffer),
    domain_arch = plot_domain_architecture(protein_features, uniprot_id),
    length_dist = plot_feature_lengths(protein_features, uniprot_id),
    summary_table = summarize_features(protein_features, uniprot_id)
  )
  
  return(plots)
}

# Example usage with your data:
# 
# # For one protein
# plots <- visualize_protein(features_df, "P08514")
# plots$feature_map  # Interactive feature map
# plots$domain_arch  # Domain architecture
# plots$summary_table  # Summary table
# 
# # For all proteins
# feature_distribution <- plot_feature_distribution(features_df)
# 
# # Just the feature map
# feature_plot <- plot_protein_features(features_df, "P08514")
# feature_plot