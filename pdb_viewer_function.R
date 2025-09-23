# R Shiny integration for local AlphaFold PDB file visualization
# Add these functions to your functions file

library(plotly)
library(data.table)

# Function to extract UniProt ID from AlphaFold filename
extract_uniprot_from_filename <- function(filename) {
  # Pattern: AF-{UNIPROT_ID}-F1-model_v4.pdb
  pattern <- "AF-([A-Z0-9]+)-F1-model_v4\\.pdb"
  match <- regmatches(filename, regexpr(pattern, filename, perl = TRUE))
  if (length(match) > 0) {
    uniprot_id <- gsub("AF-([A-Z0-9]+)-F1-model_v4\\.pdb", "\\1", match)
    return(uniprot_id)
  }
  return(NULL)
}

# Function to find AlphaFold PDB file for a given UniProt ID
find_alphafold_file <- function(uniprot_id, alphafold_dir = "alphafold_structures") {
  expected_filename <- paste0("AF-", uniprot_id, "-F1-model_v4.pdb")
  file_path <- file.path(alphafold_dir, expected_filename)
  
  if (file.exists(file_path)) {
    return(file_path)
  }
  
  # Try to find any file containing the UniProt ID
  all_files <- list.files(alphafold_dir, pattern = "\\.pdb$", full.names = TRUE)
  matching_files <- all_files[grepl(uniprot_id, basename(all_files))]
  
  if (length(matching_files) > 0) {
    return(matching_files[1])
  }
  
  return(NULL)
}

# Function to parse PDB file and extract coordinates
parse_pdb_file <- function(pdb_file_path) {
  if (!file.exists(pdb_file_path)) {
    stop("PDB file not found: ", pdb_file_path)
  }
  
  # Read PDB file
  pdb_lines <- readLines(pdb_file_path)
  
  # Extract ATOM records (protein atoms)
  atom_lines <- pdb_lines[grepl("^ATOM", pdb_lines)]
  
  if (length(atom_lines) == 0) {
    stop("No ATOM records found in PDB file")
  }
  
  # Parse ATOM records using fixed-width format
  atom_data <- data.frame(
    record_type = substr(atom_lines, 1, 6),
    atom_number = as.numeric(substr(atom_lines, 7, 11)),
    atom_name = trimws(substr(atom_lines, 13, 16)),
    residue_name = trimws(substr(atom_lines, 18, 20)),
    chain_id = substr(atom_lines, 22, 22),
    residue_number = as.numeric(substr(atom_lines, 23, 26)),
    x = as.numeric(substr(atom_lines, 31, 38)),
    y = as.numeric(substr(atom_lines, 39, 46)),
    z = as.numeric(substr(atom_lines, 47, 54)),
    occupancy = as.numeric(substr(atom_lines, 55, 60)),
    b_factor = as.numeric(substr(atom_lines, 61, 66)),
    element = trimws(substr(atom_lines, 77, 78)),
    stringsAsFactors = FALSE
  )
  
  # Remove rows with missing coordinates
  atom_data <- atom_data[!is.na(atom_data$x) & !is.na(atom_data$y) & !is.na(atom_data$z), ]
  
  return(atom_data)
}

# Function to extract backbone (CA atoms) for simplified visualization
extract_backbone <- function(atom_data) {
  # Get alpha carbon (CA) atoms for backbone trace
  backbone <- atom_data[atom_data$atom_name == "CA", ]
  backbone <- backbone[order(backbone$residue_number), ]
  return(backbone)
}

# Function to map UniProt features to PDB residue numbers
map_features_to_structure <- function(features_df, uniprot_id, atom_data) {
  # Filter features for this protein
  protein_features <- features_df[features_df$uniprot_id == uniprot_id, ]
  
  if (nrow(protein_features) == 0) {
    cat("No features found for protein", uniprot_id, "\n")
    return(data.frame())
  }
  
  cat("Found", nrow(protein_features), "features for", uniprot_id, "\n")
  
  # Get residue range from structure
  min_res <- min(atom_data$residue_number, na.rm = TRUE)
  max_res <- max(atom_data$residue_number, na.rm = TRUE)
  
  cat("Structure residue range:", min_res, "-", max_res, "\n")
  
  # Map features to structure residues
  feature_mapping <- data.frame()
  
  for (i in 1:nrow(protein_features)) {
    feature <- protein_features[i, ]
    
    # Skip if start/end positions are NA
    if (is.na(feature$start) || is.na(feature$end)) {
      cat("Skipping feature", feature$type, "- missing start/end positions\n")
      next
    }
    
    # Map feature positions to structure (assuming 1:1 mapping for AlphaFold)
    feature_start <- max(min_res, feature$start)
    feature_end <- min(max_res, feature$end)
    
    cat("Processing feature", feature$type, ":", feature$start, "-", feature$end, 
        "-> mapped to", feature_start, "-", feature_end, "\n")
    
    if (feature_start <= feature_end && feature_start <= max_res && feature_end >= min_res) {
      # Get atoms for this feature region
      feature_atoms <- atom_data[
        atom_data$residue_number >= feature_start & 
          atom_data$residue_number <= feature_end &
          atom_data$atom_name == "CA", # Only backbone for visualization
      ]
      
      if (nrow(feature_atoms) > 0) {
        cat("Found", nrow(feature_atoms), "atoms for feature", feature$type, "\n")
        
        # Safely add feature information
        feature_atoms$feature_type <- as.character(feature$type)
        feature_atoms$feature_category <- as.character(feature$category %||% "Unknown")
        feature_atoms$feature_color <- assign_feature_color(feature$type)
        feature_atoms$feature_start <- feature_start
        feature_atoms$feature_end <- feature_end
        
        # Ensure all columns are present and correctly typed
        required_cols <- c("x", "y", "z", "residue_number", "residue_name", "b_factor")
        for (col in required_cols) {
          if (!col %in% colnames(feature_atoms)) {
            cat("Warning: Missing column", col, "in feature atoms\n")
            feature_atoms[[col]] <- NA
          }
        }
        
        feature_mapping <- rbind(feature_mapping, feature_atoms)
      } else {
        cat("No atoms found for feature", feature$type, "in range", feature_start, "-", feature_end, "\n")
      }
    } else {
      cat("Feature", feature$type, "out of structure range\n")
    }
  }
  
  cat("Total feature mapping:", nrow(feature_mapping), "atoms\n")
  return(feature_mapping)
}

# Enhanced color assignment for features
assign_feature_color <- function(feature_type) {
  color_map <- list(
    # Targeting/Secretion - Reds
    "Signal" = "#E53E3E",
    "SIGNAL" = "#E53E3E",
    "Transit peptide" = "#C53030",
    "TRANSIT" = "#C53030",
    "Propeptide" = "#9C2A2A",
    "PROPEP" = "#9C2A2A",
    
    # Membrane - Blues
    "Transmembrane" = "#3182CE",
    "TRANSMEM" = "#3182CE", 
    "Topological domain" = "#2C5AA0",
    "TOPO_DOM" = "#2C5AA0",
    
    # Functional - Greens
    "Domain" = "#38A169",
    "DOMAIN" = "#38A169",
    "Active site" = "#2F855A",
    "ACT_SITE" = "#2F855A",
    "Binding site" = "#276749",
    "BINDING" = "#276749",
    "Site" = "#22543D",
    "SITE" = "#22543D",
    
    # Structural - Oranges
    "Repeat" = "#DD6B20",
    "REPEAT" = "#DD6B20",
    "Motif" = "#C05621",
    "MOTIF" = "#C05621",
    "Coiled coil" = "#9C4221",
    "COILED" = "#9C4221",
    "Zinc finger" = "#7C2D12",
    "ZN_FING" = "#7C2D12",
    
    # Modifications - Purples
    "Disulfide bond" = "#805AD5",
    "DISULFID" = "#805AD5",
    "Glycosylation" = "#6B46C1",
    "CARBOHYD" = "#6B46C1",
    "Modified residue" = "#553C9A",
    "MOD_RES" = "#553C9A",
    
    # Secondary Structure - Grays
    "Helix" = "#4A5568",
    "HELIX" = "#4A5568",
    "Beta strand" = "#2D3748",
    "STRAND" = "#2D3748",
    "Turn" = "#1A202C",
    "TURN" = "#1A202C"
  )
  
  return(color_map[[feature_type]] %||% "#718096")
}

# Function to create proper protein structure visualization
create_alphafold_3d_plot <- function(features_df, uniprot_id, alphafold_dir = "alphafold_structures") {
  
  # Find the AlphaFold PDB file
  pdb_file <- find_alphafold_file(uniprot_id, alphafold_dir)
  
  if (is.null(pdb_file)) {
    # Return error plot
    p <- plot_ly() %>%
      add_annotations(
        text = paste("No AlphaFold structure found for", uniprot_id),
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, color = "red")
      ) %>%
      layout(title = paste("AlphaFold Structure:", uniprot_id))
    return(p)
  }
  
  tryCatch({
    # Parse PDB file
    cat("Parsing PDB file:", pdb_file, "\n")
    atom_data <- parse_pdb_file(pdb_file)
    
    # Extract backbone for main structure
    backbone <- extract_backbone(atom_data)
    
    if (nrow(backbone) == 0) {
      stop("No backbone atoms found")
    }
    
    cat("Found", nrow(backbone), "backbone atoms\n")
    
    # Clean backbone coordinates
    backbone_clean <- backbone[!is.na(backbone$x) & !is.na(backbone$y) & !is.na(backbone$z), ]
    
    if (nrow(backbone_clean) == 0) {
      stop("No valid backbone coordinates")
    }
    
    cat("Clean backbone atoms:", nrow(backbone_clean), "\n")
    
    # Create secondary structure visualization using B-factors (AlphaFold confidence)
    # High confidence (pLDDT > 90): Very confident - Dark blue
    # Confident (pLDDT 70-90): Confident - Light blue  
    # Low confidence (pLDDT 50-70): Low confidence - Yellow/Orange
    # Very low (pLDDT < 50): Very low confidence - Orange/Red
    
    backbone_clean$confidence_color <- sapply(backbone_clean$b_factor, function(b) {
      if (is.na(b)) return("#CCCCCC")
      if (b > 90) return("#1f4e79")      # Dark blue - very confident
      else if (b > 70) return("#4f81bd")  # Blue - confident  
      else if (b > 50) return("#f79646")  # Orange - low confidence
      else return("#c5504b")              # Red - very low confidence
    })
    
    backbone_clean$confidence_label <- sapply(backbone_clean$b_factor, function(b) {
      if (is.na(b)) return("Unknown")
      if (b > 90) return("Very High")
      else if (b > 70) return("Confident") 
      else if (b > 50) return("Low")
      else return("Very Low")
    })
    
    # Create the plot with cartoon-style representation
    p <- plot_ly()
    
    # Method 1: Cartoon-like ribbon using lines with varying thickness
    # Connect consecutive backbone atoms to create a continuous ribbon
    
    # Add backbone ribbon (thick lines connecting CA atoms)
    p <- p %>%
      add_trace(
        x = backbone_clean$x,
        y = backbone_clean$y, 
        z = backbone_clean$z,
        type = "scatter3d",
        mode = "lines",
        line = list(
          color = backbone_clean$confidence_color,
          width = 8,
          smoothing = 1.3,  # Smooth the line for better appearance
          shape = "spline"  # Use spline interpolation
        ),
        name = "Protein Backbone",
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          "Position: (%{x:.1f}, %{y:.1f}, %{z:.1f})<br>",
          "Confidence: %{customdata}<br>",
          "pLDDT: %{marker.color:.0f}",
          "<extra></extra>"
        ),
        text = paste(backbone_clean$residue_name, backbone_clean$residue_number),
        customdata = backbone_clean$confidence_label,
        marker = list(color = backbone_clean$b_factor),
        showlegend = TRUE
      )
    
    # Add key residues as spheres (every 10th residue for clarity)
    key_residues <- backbone_clean[seq(1, nrow(backbone_clean), by = 10), ]
    
    p <- p %>%
      add_trace(
        x = key_residues$x,
        y = key_residues$y,
        z = key_residues$z,
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 4,
          color = key_residues$confidence_color,
          opacity = 0.8,
          line = list(width = 1, color = "white")
        ),
        name = "Key Residues",
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          "Position: (%{x:.1f}, %{y:.1f}, %{z:.1f})<br>",
          "Confidence: %{customdata}",
          "<extra></extra>"
        ),
        text = paste(key_residues$residue_name, key_residues$residue_number),
        customdata = key_residues$confidence_label,
        showlegend = TRUE
      )
    
    # Add features if available
    feature_atoms <- map_features_to_structure(features_df, uniprot_id, atom_data)
    
    if (nrow(feature_atoms) > 0) {
      feature_atoms_clean <- feature_atoms[
        !is.na(feature_atoms$x) & !is.na(feature_atoms$y) & !is.na(feature_atoms$z), 
      ]
      
      if (nrow(feature_atoms_clean) > 0) {
        feature_types <- unique(feature_atoms_clean$feature_type)
        
        for (ft in feature_types) {
          ft_data <- feature_atoms_clean[feature_atoms_clean$feature_type == ft, ]
          
          if (nrow(ft_data) > 0) {
            # Create feature regions as highlighted spheres
            p <- p %>%
              add_trace(
                x = ft_data$x,
                y = ft_data$y,
                z = ft_data$z,
                type = "scatter3d", 
                mode = "markers",
                marker = list(
                  size = 10,
                  color = ft_data$feature_color[1],
                  opacity = 0.9,
                  line = list(width = 2, color = "white"),
                  symbol = "circle"
                ),
                name = paste("🔹", ft),
                hovertemplate = paste0(
                  "<b>", ft, "</b><br>",
                  "Residue: %{text}<br>",
                  "Position: (%{x:.1f}, %{y:.1f}, %{z:.1f})<br>",
                  "Range: ", ft_data$feature_start[1], "-", ft_data$feature_end[1],
                  "<extra></extra>"
                ),
                text = paste(ft_data$residue_name, ft_data$residue_number),
                showlegend = TRUE
              )
          }
        }
      }
    }
    
    # Professional-looking layout
    p <- p %>%
      layout(
        title = list(
          text = paste0("<b>", uniprot_id, " - AlphaFold Structure</b><br>",
                        "<span style='font-size:12px; color:#666;'>Colored by prediction confidence (pLDDT score)</span>"),
          font = list(size = 18, family = "Arial, sans-serif")
        ),
        scene = list(
          xaxis = list(
            title = "",
            showgrid = FALSE,
            showticklabels = FALSE,
            showspikes = FALSE,
            visible = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE, 
            showticklabels = FALSE,
            showspikes = FALSE,
            visible = FALSE
          ),
          zaxis = list(
            title = "",
            showgrid = FALSE,
            showticklabels = FALSE, 
            showspikes = FALSE,
            visible = FALSE
          ),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 1.5),
            center = list(x = 0, y = 0, z = 0)
          ),
          bgcolor = "#000000",  # Black background like professional viewers
          aspectmode = "cube"
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1,
          bgcolor = "rgba(255,255,255,0.9)",
          bordercolor = "#CCCCCC",
          borderwidth = 1,
          font = list(size = 12)
        ),
        paper_bgcolor = "#f8f9fa",
        plot_bgcolor = "#000000",
        margin = list(l = 0, r = 100, t = 80, b = 0)
      )
    
    # Add confidence scale annotation
    p <- p %>%
      add_annotations(
        text = paste0(
          "<b>Confidence Scale:</b><br>",
          "<span style='color:#1f4e79'>■</span> Very High (>90)<br>", 
          "<span style='color:#4f81bd'>■</span> Confident (70-90)<br>",
          "<span style='color:#f79646'>■</span> Low (50-70)<br>",
          "<span style='color:#c5504b'>■</span> Very Low (<50)"
        ),
        x = 1.02, y = 0.3,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "#CCCCCC",
        borderwidth = 1
      )
    
    cat("Successfully created professional 3D protein structure\n")
    return(p)
    
  }, error = function(e) {
    cat("Error in create_alphafold_3d_plot:", e$message, "\n")
    
    # Return error plot
    p <- plot_ly() %>%
      add_annotations(
        text = paste("Error loading structure:", e$message),
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "red")
      ) %>%
      layout(title = paste("AlphaFold Structure Error:", uniprot_id))
    return(p)
  })
}

# Function to create AlphaFold info card
create_alphafold_info_card <- function(uniprot_id, alphafold_dir = "alphafold_structures") {
  
  pdb_file <- find_alphafold_file(uniprot_id, alphafold_dir)
  
  if (is.null(pdb_file)) {
    return(div(
      class = "alert alert-warning",
      h4("No AlphaFold Structure Available"),
      p(paste("No AlphaFold structure found for", uniprot_id)),
      p("Expected file:", paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))
    ))
  }
  
  # Get file info
  file_info <- file.info(pdb_file)
  file_size <- round(file_info$size / 1024, 1)  # KB
  
  # Try to get some basic info from the file
  confidence_info <- "Available in structure (B-factors represent confidence)"
  
  return(div(
    class = "card border-primary",
    style = "margin: 10px 0;",
    div(
      class = "card-header bg-primary text-white",
      h4("AlphaFold Structure", style = "margin: 0;")
    ),
    div(
      class = "card-body",
      p(strong("UniProt ID: "), uniprot_id),
      p(strong("File: "), basename(pdb_file)),
      p(strong("Size: "), paste(file_size, "KB")),
      p(strong("Model: "), "Full-length structure prediction"),
      p(strong("Confidence: "), confidence_info),
      p(strong("Source: "), "AlphaFold Protein Structure Database"),
      hr(),
      a(
        href = paste0("https://alphafold.ebi.ac.uk/entry/", uniprot_id),
        target = "_blank",
        class = "btn btn-primary btn-sm",
        "View in AlphaFold DB →"
      ),
      " ",
      span(
        class = "badge badge-info",
        "Local File Available"
      )
    )
  ))
}

# Main function for AlphaFold Shiny integration
create_alphafold_viewer <- function(features_df, uniprot_id, alphafold_dir = "alphafold_structures") {
  
  # Create info card
  info_card <- create_alphafold_info_card(uniprot_id, alphafold_dir)
  
  # Create 3D plot
  plot_3d <- create_alphafold_3d_plot(features_df, uniprot_id, alphafold_dir)
  
  # Check if file exists
  pdb_file <- find_alphafold_file(uniprot_id, alphafold_dir)
  
  return(list(
    info_card = info_card,
    plot_3d = plot_3d,
    pdb_file = pdb_file,
    has_structure = !is.null(pdb_file)
  ))
}

# Example integration in your Shiny server:
# 
# output$alphafold_info <- renderUI({
#   req(input$uniprot_select)
#   viewer_data <- create_alphafold_viewer(search_section(), input$uniprot_select[1])
#   viewer_data$info_card
# })
# 
# output$alphafold_plot <- renderPlotly({
#   req(input$uniprot_select)
#   viewer_data <- create_alphafold_viewer(search_section(), input$uniprot_select[1])
#   viewer_data$plot_3d
# })
#
# # In your UI:
# tabPanel("AlphaFold Structure",
#          fluidRow(
#            column(4, uiOutput("alphafold_info")),
#            column(8, plotlyOutput("alphafold_plot", height = "700px"))
#          )
# )