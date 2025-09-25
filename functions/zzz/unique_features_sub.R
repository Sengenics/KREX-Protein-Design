# Simplified, clean protein features plot focused on design-relevant features
# Replace your messy plot with this clean version

plot_protein_features <- function(features_df, uniprot_id = NULL, sequence_length = NULL, 
                                        show_impact_zones = TRUE, filter_important_only = TRUE) {
  
  # Filter for specific protein if provided
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  # Get sequence length
  if (is.null(sequence_length)) {
    max_end <- max(features_df$end, na.rm = TRUE)
    sequence_length <- ifelse(is.finite(max_end), max_end + 50, 1000)
  }
  
  # Filter to only the most important features for protein design
  if (filter_important_only) {
    important_features <- c(
      # Critical for targeting/processing
      "Signal", "Transit peptide", "Propeptide", "SIGNAL", "TRANSIT", "PROPEP",
      # Critical for membrane proteins
      "Transmembrane", "Topological domain", "TRANSMEM", "TOPO_DOM", 
      # Critical for function
      "Domain", "Active site", "Binding site", "DOMAIN", "ACT_SITE", "BINDING", "SITE",
      # Important structural elements
      "Repeat", "Motif", "Coiled coil", "Zinc finger", "REPEAT", "MOTIF", "COILED", "ZN_FING",
      # Important modifications
      "Disulfide bond", "Glycosylation", "DISULFID", "CARBOHYD"
    )
    
    features_df <- features_df[features_df$type %in% important_features, ]
  }
  
  if (nrow(features_df) == 0) {
    p <- plot_ly() %>%
      add_annotations(
        text = "No important features found for protein design analysis",
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 16)
      ) %>%
      layout(title = paste("Protein Design Analysis:", uniprot_id))
    return(p)
  }
  
  # Simplified categories with distinct tracks
  design_categories <- list(
    "Targeting" = list(
      features = c("Signal", "Transit peptide", "Propeptide", "SIGNAL", "TRANSIT", "PROPEP"),
      track = 5, color = "#E74C3C", description = "Protein targeting signals"
    ),
    "Membrane" = list(
      features = c("Transmembrane", "Topological domain", "TRANSMEM", "TOPO_DOM"),
      track = 4, color = "#3498DB", description = "Membrane topology"
    ),
    "Function" = list(
      features = c("Domain", "Active site", "Binding site", "DOMAIN", "ACT_SITE", "BINDING", "SITE"),
      track = 3, color = "#27AE60", description = "Functional elements"
    ),
    "Structure" = list(
      features = c("Repeat", "Motif", "Coiled coil", "Zinc finger", "REPEAT", "MOTIF", "COILED", "ZN_FING"),
      track = 2, color = "#F39C12", description = "Structural elements"
    ),
    "Modifications" = list(
      features = c("Disulfide bond", "Glycosylation", "DISULFID", "CARBOHYD"),
      track = 1, color = "#9B59B6", description = "Critical modifications"
    )
  )
  
  # Assign categories and colors
  features_df$category <- "Other"
  features_df$track <- 0
  features_df$color <- "#CCCCCC"
  
  for (cat_name in names(design_categories)) {
    cat_info <- design_categories[[cat_name]]
    mask <- features_df$type %in% cat_info$features
    features_df$category[mask] <- cat_name
    features_df$track[mask] <- cat_info$track
    features_df$color[mask] <- cat_info$color
  }
  
  # Simple non-overlapping positioning within tracks
  features_df$y_pos <- features_df$track
  
  # For overlapping features in same track, stack them
  for (track_num in unique(features_df$track)) {
    track_features <- features_df[features_df$track == track_num, ]
    if (nrow(track_features) > 1) {
      track_features <- track_features[order(track_features$start), ]
      track_features$y_offset <- 0
      
      for (i in 2:nrow(track_features)) {
        max_offset <- 0
        for (j in 1:(i-1)) {
          if (!is.na(track_features$start[i]) && !is.na(track_features$end[j]) &&
              track_features$start[i] <= track_features$end[j] + 5) {  # 5 AA buffer
            max_offset <- max(max_offset, track_features$y_offset[j] + 0.3)
          }
        }
        track_features$y_offset[i] <- max_offset
      }
      
      features_df[features_df$track == track_num, "y_pos"] <- 
        track_features$track + track_features$y_offset
    }
  }
  
  # Create clean hover text
  features_df$hover_text <- paste0(
    "<b>", features_df$type, "</b><br>",
    "Category: ", features_df$category, "<br>",
    "Position: ", features_df$start, "-", features_df$end, "<br>",
    "Length: ", (features_df$end - features_df$start + 1), " aa"
  )
  
  # Create the plot
  protein_id <- unique(features_df$uniprot_id)[1]
  
  p <- plot_ly() %>%
    layout(
      title = list(
        text = paste0("<b>Protein Design Analysis: ", protein_id, "</b><br>",
                      "<span style='font-size:12px'>Length: ~", sequence_length, " aa | ",
                      "Showing design-critical features only</span>"),
        font = list(size = 18)
      ),
      xaxis = list(
        title = list(text = "<b>Amino Acid Position</b>", font = list(size = 14)),
        range = c(0, sequence_length),
        showgrid = TRUE,
        gridcolor = "#E8E8E8",
        showline = TRUE,
        linecolor = "black"
      ),
      yaxis = list(
        title = list(text = "<b>Feature Categories</b>", font = list(size = 14)),
        range = c(0.5, 6),
        ticktext = c("Modifications", "Structure", "Function", "Membrane", "Targeting"),
        tickvals = c(1, 2, 3, 4, 5),
        showgrid = TRUE,
        gridcolor = "#F0F0F0",
        showline = TRUE,
        linecolor = "black"
      ),
      showlegend = TRUE,
      hovermode = "closest",
      height = 500,
      plot_bgcolor = "white",
      legend = list(
        x = 1.02, y = 1,
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
    )
  
  # Add protein backbone
  p <- p %>%
    add_trace(
      x = c(1, sequence_length), y = c(0.2, 0.2),
      type = "scatter", mode = "lines",
      line = list(color = "black", width = 6),
      name = "Protein Backbone",
      showlegend = FALSE,
      hovertemplate = paste0("Full protein sequence<br>Length: ", sequence_length, " aa<extra></extra>")
    )
  
  # Add impact zones if requested
  if (show_impact_zones) {
    n_term_zone <- min(50, sequence_length * 0.1)
    c_term_zone <- min(50, sequence_length * 0.1)
    
    # N-terminal impact zone
    p <- p %>%
      add_trace(
        x = c(1, n_term_zone, n_term_zone, 1, 1),
        y = c(0.5, 0.5, 5.5, 5.5, 0.5),
        type = "scatter", mode = "lines", fill = "toself",
        fillcolor = "rgba(255, 100, 100, 0.2)",
        line = list(color = "rgba(255, 0, 0, 0.5)", width = 2, dash = "dash"),
        name = "N-term Impact Zone",
        hovertemplate = paste0("<b>N-terminal Impact Zone</b><br>",
                               "Positions 1-", round(n_term_zone), "<br>",
                               "⚠️ Adding tags here may disrupt:<br>",
                               "• Signal peptides<br>",
                               "• Protein targeting<br>",
                               "• N-terminal structure<extra></extra>")
      )
    
    # C-terminal impact zone  
    p <- p %>%
      add_trace(
        x = c(sequence_length - c_term_zone, sequence_length, sequence_length, 
              sequence_length - c_term_zone, sequence_length - c_term_zone),
        y = c(0.5, 0.5, 5.5, 5.5, 0.5),
        type = "scatter", mode = "lines", fill = "toself",
        fillcolor = "rgba(255, 100, 100, 0.2)",
        line = list(color = "rgba(255, 0, 0, 0.5)", width = 2, dash = "dash"),
        name = "C-term Impact Zone",
        hovertemplate = paste0("<b>C-terminal Impact Zone</b><br>",
                               "Positions ", round(sequence_length - c_term_zone), "-", sequence_length, "<br>",
                               "⚠️ Adding tags here may disrupt:<br>",
                               "• C-terminal domains<br>",
                               "• Membrane topology<br>",
                               "• Protein folding<extra></extra>")
      )
  }
  
  # Add features by category
  for (cat_name in unique(features_df$category)) {
    if (cat_name == "Other") next
    
    cat_features <- features_df[features_df$category == cat_name, ]
    first_added <- FALSE
    
    for (i in 1:nrow(cat_features)) {
      feature <- cat_features[i, ]
      
      p <- p %>%
        add_trace(
          x = c(feature$start, feature$end, feature$end, feature$start, feature$start),
          y = c(feature$y_pos - 0.15, feature$y_pos - 0.15, feature$y_pos + 0.15, 
                feature$y_pos + 0.15, feature$y_pos - 0.15),
          type = "scatter", mode = "lines", fill = "toself",
          fillcolor = feature$color,
          line = list(color = adjustcolor(feature$color, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
                      width = 2),
          name = cat_name,
          legendgroup = cat_name,
          showlegend = !first_added,
          hovertemplate = paste0(feature$hover_text, "<extra></extra>")
        )
      
      first_added <- TRUE
    }
  }
  
  return(p)
}

# Quick summary function for design decisions
create_design_summary <- function(features_df, uniprot_id = NULL, sequence_length = NULL) {
  
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  if (is.null(sequence_length)) {
    max_end <- max(features_df$end, na.rm = TRUE)
    sequence_length <- ifelse(is.finite(max_end), max_end + 50, 1000)
  }
  
  n_term_zone <- 50
  c_term_zone <- 50
  
  # Check for critical features in terminal regions
  n_term_features <- features_df[!is.na(features_df$start) & features_df$start <= n_term_zone, ]
  c_term_features <- features_df[!is.na(features_df$end) & features_df$end >= (sequence_length - c_term_zone), ]
  
  critical_n <- n_term_features$type[n_term_features$type %in% c("Signal", "SIGNAL", "Transit peptide", "TRANSIT")]
  critical_c <- c_term_features$type[c_term_features$type %in% c("Transmembrane", "TRANSMEM", "Domain", "DOMAIN")]
  
  summary <- list(
    protein_id = unique(features_df$uniprot_id)[1],
    length = sequence_length,
    n_terminal_critical = length(critical_n) > 0,
    c_terminal_critical = length(critical_c) > 0,
    n_critical_features = critical_n,
    c_critical_features = critical_c,
    recommendation = ""
  )
  
  # Generate recommendation
  if (summary$n_terminal_critical && summary$c_terminal_critical) {
    summary$recommendation <- "⚠️ CAUTION: Both termini have critical features. Consider internal tags or very careful linker design."
  } else if (summary$n_terminal_critical) {
    summary$recommendation <- "⚠️ N-terminus has critical features. Recommend C-terminal tagging with flexible linker."
  } else if (summary$c_terminal_critical) {
    summary$recommendation <- "⚠️ C-terminus has critical features. Recommend N-terminal tagging with flexible linker."
  } else {
    summary$recommendation <- "✅ Both termini appear suitable for tagging. Standard approaches should work."
  }
  
  return(summary)
}