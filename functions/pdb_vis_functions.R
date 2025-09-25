# shiny_r3dmol_fix.R
# Fixed version for Shiny r3dmol rendering

library(r3dmol)
library(shiny)

# Alternative approach using htmlwidgets directly
create_r3dmol_widget <- function(uniprot_id, pdb_folder = "alphafold_structures") {
  
  # Find PDB file
  pdb_file <- file.path(pdb_folder, paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))
  
  if (!file.exists(pdb_file)) {
    return(
      div(class = "alert alert-warning",
          paste("PDB file not found for", uniprot_id))
    )
  }
  
  tryCatch({
    # Read PDB content
    pdb_lines <- readLines(pdb_file, warn = FALSE)
    pdb_string <- paste(pdb_lines, collapse = "\n")
    
    # Create widget with explicit parameters
    widget <- r3dmol(
      width = "100%", 
      height = "500px",
      elementId = paste0("structure_", gsub("[^A-Za-z0-9]", "_", uniprot_id))
    ) %>%
      m_add_model(data = pdb_string, format = "pdb") %>%
      m_set_style(style = list(cartoon = list(color = "spectrum"))) %>%
      m_zoom_to()
    
    return(widget)
    
  }, error = function(e) {
    return(
      div(class = "alert alert-danger",
          "Error loading structure: ", e$message)
    )
  })
}

# Alternative: Create a custom HTML widget
create_custom_viewer <- function(uniprot_id, pdb_folder = "alphafold_structures") {
  
  pdb_file <- file.path(pdb_folder, paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))
  
  if (!file.exists(pdb_file)) {
    return(tags$p("Structure file not found"))
  }
  
  # Create a unique div ID
  viewer_id <- paste0("viewer_", gsub("[^A-Za-z0-9]", "_", uniprot_id))
  
  # Read PDB content and escape for JavaScript
  pdb_lines <- readLines(pdb_file, warn = FALSE)
  pdb_content <- paste(pdb_lines, collapse = "\\n")
  
  # Create HTML with embedded 3Dmol.js
  tags$div(
    # Container for the viewer
    tags$div(id = viewer_id, style = "height: 500px; width: 100%; position: relative;"),
    
    # Load 3Dmol.js from CDN
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/3Dmol/2.0.4/3Dmol-min.js"),
    
    # JavaScript to create the viewer
    tags$script(HTML(paste0("
      $(document).ready(function() {
        if (typeof $3Dmol !== 'undefined') {
          let viewer = $3Dmol.createViewer('", viewer_id, "', {
            defaultcolors: $3Dmol.rasmolElementColors
          });
          
          let pdbData = '", pdb_content, "';
          
          viewer.addModel(pdbData, 'pdb');
          viewer.setStyle({}, {cartoon: {color: 'spectrum'}});
          viewer.zoomTo();
          viewer.render();
          
          // Add some controls
          viewer.spin(true);
        } else {
          $('#", viewer_id, "').html('<div class=\"alert alert-warning\">3Dmol.js failed to load</div>');
        }
      });
    ")))
  )
}

# Simple fallback: NGL Viewer (alternative to 3Dmol)
create_ngl_viewer <- function(uniprot_id, pdb_folder = "alphafold_structures") {
  
  pdb_file <- file.path(pdb_folder, paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))
  
  if (!file.exists(pdb_file)) {
    return(tags$p("Structure file not found"))
  }
  
  viewer_id <- paste0("ngl_viewer_", gsub("[^A-Za-z0-9]", "_", uniprot_id))
  
  # Read and encode PDB content
  pdb_lines <- readLines(pdb_file, warn = FALSE)
  pdb_content <- paste(pdb_lines, collapse = "\\n")
  
  tags$div(
    tags$div(id = viewer_id, style = "height: 500px; width: 100%;"),
    tags$script(src = "https://unpkg.com/ngl@2.0.0-dev.37/dist/ngl.js"),
    tags$script(HTML(paste0("
      $(document).ready(function() {
        if (typeof NGL !== 'undefined') {
          var stage = new NGL.Stage('", viewer_id, "');
          var pdbBlob = new Blob(['", pdb_content, "'], { type: 'text/plain' });
          var pdbUrl = URL.createObjectURL(pdbBlob);
          
          stage.loadFile(pdbUrl, { ext: 'pdb' }).then(function(component) {
            component.addRepresentation('cartoon', { color: 'bfactor' });
            component.autoView();
          });
        } else {
          $('#", viewer_id, "').html('<div class=\"alert alert-warning\">NGL viewer failed to load</div>');
        }
      });
    ")))
  )
}

# Method to try multiple approaches
render_structure_robust <- function(uniprot_id, pdb_folder = "alphafold_structures", method = "auto") {
  
  if (method == "auto") {
    # Try r3dmol first, then fallback to custom viewer
    tryCatch({
      widget <- create_r3dmol_widget(uniprot_id, pdb_folder)
      return(widget)
    }, error = function(e) {
      return(create_custom_viewer(uniprot_id, pdb_folder))
    })
  } else if (method == "r3dmol") {
    return(create_r3dmol_widget(uniprot_id, pdb_folder))
  } else if (method == "custom") {
    return(create_custom_viewer(uniprot_id, pdb_folder))
  } else if (method == "ngl") {
    return(create_ngl_viewer(uniprot_id, pdb_folder))
  }
}

# Server function for testing different methods
test_structure_methods <- function(input, output, session, uniprot_id_reactive, pdb_folder = "alphafold_structures") {
  
  output$structure_r3dmol <- renderR3dmol({
    req(uniprot_id_reactive())
    create_r3dmol_widget(uniprot_id_reactive(), pdb_folder)
  })
  
  output$structure_custom <- renderUI({
    req(uniprot_id_reactive())
    create_custom_viewer(uniprot_id_reactive(), pdb_folder)
  })
  
  output$structure_ngl <- renderUI({
    req(uniprot_id_reactive())
    create_ngl_viewer(uniprot_id_reactive(), pdb_folder)
  })
}

# UI for testing different methods
test_structure_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Structure Viewer Testing"),
    
    tabsetPanel(
      tabPanel("r3dmol Method",
               h5("Using r3dmol package"),
               r3dmolOutput(ns("structure_r3dmol"), height = "500px")
      ),
      
      tabPanel("Custom 3Dmol.js",
               h5("Direct 3Dmol.js integration"),
               htmlOutput(ns("structure_custom"))
      ),
      
      tabPanel("NGL Viewer",
               h5("Alternative NGL viewer"),
               htmlOutput(ns("structure_ngl"))
      )
    )
  )
}


get_pdb_info <- function(uniprot_id, pdb_folder = "alphafold_structures") {
  pdb_file <- file.path(pdb_folder, paste0("AF-", uniprot_id, "-F1-model_v4.pdb"))
  
  if (!file.exists(pdb_file)) {
    return(list(status = "not_found", message = paste("No PDB file found for", uniprot_id)))
  }
  
  pdb_lines <- readLines(pdb_file, warn = FALSE)
  atom_lines <- pdb_lines[grepl("^ATOM", pdb_lines)]
  
  list(
    status = "found",
    file_name = basename(pdb_file),
    atom_count = length(atom_lines),
    file_size = file.size(pdb_file)
  )
}
