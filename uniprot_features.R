# feature_visualization_direct.R
# Visualization functions for existing features_df dataframe

library(plotly)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)

# Function to create interactive feature plot from your features_df
plot_protein_features <- function(features_df, uniprot_id = NULL, sequence_length = NULL) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  # Get unique protein ID for title
  protein_id <- unique(features_df$uniprot_id)[1]
  
  # Estimate sequence length if not provided
  if (is.null(sequence_length)) {
    sequence_length <- max(features_df$end, na.rm = TRUE) + 50
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
  
  # Define colors for different feature types
  feature_types <- unique(features_df$type)
  colors <- rainbow(length(feature_types), alpha = 0.7)
  names(colors) <- feature_types
  
  # Add colors and y-positions
  features_df <- features_df %>%
    mutate(
      color = colors[type],
      y_pos = assign_y_positions_simple(start, end, type),
      hover_text = paste0(
        "Type: ", type, "<br>",
        "Position: ", start, "-", end, "<br>",
        "Length: ", length, " aa<br>",
        "UniProt ID: ", uniprot_id
      )
    )
  
  # Create the plot
  p <- plot_ly() %>%
    layout(
      title = paste("Protein Features:", protein_id, "(Length ~", sequence_length, "aa)"),
      xaxis = list(
        title = "Amino Acid Position",
        range = c(0, sequence_length),
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Features",
        showticklabels = TRUE,
        ticktext = unique(features_df$type),
        tickvals = 1:length(unique(features_df$type)),
        showgrid = FALSE
      ),
      showlegend = TRUE,
      hovermode = "closest",
      height = 400 + length(feature_types) * 30
    )
  
  # Add sequence backbone
  p <- p %>%
    add_trace(
      x = c(1, sequence_length),
      y = c(0, 0),
      type = "scatter",
      mode = "lines",
      line = list(color = "black", width = 3),
      name = "Sequence",
      hoverinfo = "skip",
      showlegend = FALSE
    )
  
  # Add features grouped by type
  for (ft in feature_types) {
    ft_data <- features_df[features_df$type == ft, ]
    
    # Add rectangles for this feature type
    for (i in 1:nrow(ft_data)) {
      feature <- ft_data[i, ]
      
      p <- p %>%
        add_trace(
          x = c(feature$start, feature$end, feature$end, feature$start, feature$start),
          y = c(feature$y_pos - 0.2, feature$y_pos - 0.2, feature$y_pos + 0.2, feature$y_pos + 0.2, feature$y_pos - 0.2),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = feature$color,
          line = list(color = feature$color, width = 2),
          name = ft,
          legendgroup = ft,
          showlegend = ifelse(i == 1, TRUE, FALSE),
          hoverinfo = "text",
          text = feature$hover_text
        )
    }
  }
  
  return(p)
}

# Helper function to assign y-positions to avoid overlap
assign_y_positions_simple <- function(starts, ends, types) {
  n <- length(starts)
  if (n == 0) return(numeric(0))
  
  # Initialize positions
  positions <- rep(1, n)
  
  # Simple approach: for each feature, find a non-overlapping y-level
  for (i in 1:n) {
    current_start <- starts[i]
    current_end <- ends[i]
    
    # Find the lowest available y-position
    y_level <- 1
    overlap_found <- TRUE
    
    while (overlap_found && y_level <= 20) {  # Max 20 levels
      overlap_found <- FALSE
      
      # Check if this y-level conflicts with any previous features
      if (i > 1) {  # Only check if there are previous features
        for (j in 1:(i-1)) {
          if (positions[j] == y_level) {
            # Check for overlap
            prev_start <- starts[j]
            prev_end <- ends[j]
            
            # Features overlap if they intersect
            if (!(current_end < prev_start || current_start > prev_end)) {
              overlap_found <- TRUE
              break
            }
          }
        }
      }
      
      if (overlap_found) {
        y_level <- y_level + 1
      } else {
        positions[i] <- y_level
        overlap_found <- FALSE
      }
    }
  }
  
  return(positions)
}

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
visualize_protein <- function(features_df, uniprot_id, sequence_length = NULL) {
  
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
    feature_map = plot_protein_features(protein_features, uniprot_id, sequence_length),
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