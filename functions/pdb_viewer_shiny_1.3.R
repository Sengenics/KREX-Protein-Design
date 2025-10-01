# pdb_viewer_module.R
# Module for displaying PDB structures in Shiny using r3dmol

library(shiny)
library(r3dmol)
library(stringr)

# UI Module for PDB Viewer
pdbViewerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        h4("3D Protein Structure Viewer"),
        
        # Controls panel
        wellPanel(
          fluidRow(
            column(
              width = 4,
              selectInput(
                ns("style_type"),
                "Display Style:",
                choices = c("Cartoon" = "cartoon",
                            "Stick" = "stick",
                            "Sphere" = "sphere",
                            "Line" = "line",
                            "Cross" = "cross"),
                selected = "cartoon"
              )
            ),
            column(
              width = 4,
              selectInput(
                ns("color_scheme"),
                "Color Scheme:",
                choices = c("Spectrum" = "spectrum",
                            "Chain" = "chain",
                            "Secondary Structure" = "ss",
                            "Residue" = "residue",
                            "B-factor" = "b"),
                selected = "spectrum"
              )
            ),
            column(
              width = 4,
              checkboxInput(
                ns("show_surface"),
                "Show Surface",
                value = FALSE
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              checkboxInput(
                ns("highlight_termini"),
                "Highlight N/C Termini",
                value = TRUE
              )
            ),
            column(
              width = 4,
              sliderInput(
                ns("termini_residues"),
                "Termini Residues:",
                min = 1,
                max = 20,
                value = 5,
                step = 1
              )
            ),
            column(
              width = 4,
              sliderInput(
                ns("termini_size"),
                "Termini Size:",
                min = 0.5,
                max = 3.0,
                value = 1.0,
                step = 0.1
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              actionButton(ns("reset_view"), "Reset View", class = "btn-sm"),
              actionButton(ns("spin"), "Toggle Spin", class = "btn-sm"),
              downloadButton(ns("download_pdb"), "Download PDB", class = "btn-sm")
            )
          )
        ),
        
        # Structure viewer
        r3dmolOutput(ns("structure"), height = "600px"),
        
        # Structure information
        wellPanel(
          h5("Structure Information"),
          verbatimTextOutput(ns("pdb_info"))
        )
      )
    )
  )
}

# Server Module for PDB Viewer
pdbViewerServer <- function(id, uniprot_id, pdb_folder = "pdb_files") {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to store current PDB content and structure info
    pdb_content <- reactiveVal(NULL)
    pdb_path <- reactiveVal(NULL)
    pdb_info <- reactiveVal(NULL)  # Store parsed PDB info
    spinning <- reactiveVal(FALSE)
    
    # Function to find PDB file for given UniProt ID
    find_pdb_file <- function(uniprot_id) {
      if (is.null(uniprot_id) || uniprot_id == "") return(NULL)
      
      # Try different naming conventions
      possible_names <- c(
        paste0(uniprot_id, ".pdb"),
        paste0(tolower(uniprot_id), ".pdb"),
        paste0(toupper(uniprot_id), ".pdb")
      )
      
      for (name in possible_names) {
        path <- file.path(pdb_folder, name)
        if (file.exists(path)) {
          return(path)
        }
      }
      
      # Try to find any file containing the UniProt ID
      all_files <- list.files(pdb_folder, pattern = "\\.pdb$", full.names = TRUE)
      matching <- grep(uniprot_id, basename(all_files), ignore.case = TRUE, value = TRUE)
      
      if (length(matching) > 0) {
        return(file.path(pdb_folder, matching[1]))
      }
      
      return(NULL)
    }
    
    # Load PDB file when UniProt ID changes
    observe({
      req(uniprot_id())
      
      pdb_file <- find_pdb_file(uniprot_id())
      
      if (!is.null(pdb_file)) {
        pdb_text <- readLines(pdb_file, warn = FALSE)
        pdb_string <- paste(pdb_text, collapse = "\n")
        pdb_content(pdb_string)
        pdb_path(pdb_file)
        
        # Parse PDB to get residue range
        info <- parse_pdb_info(pdb_text)
        pdb_info(info)
      } else {
        pdb_content(NULL)
        pdb_path(NULL)
        pdb_info(NULL)
        showNotification(
          paste("No PDB file found for UniProt ID:", uniprot_id()),
          type = "warning"
        )
      }
    })
    
    # Helper function to parse PDB and get residue information
    parse_pdb_info <- function(pdb_lines) {
      atom_lines <- grep("^ATOM", pdb_lines, value = TRUE)
      
      if (length(atom_lines) == 0) {
        return(list(min_res = 1, max_res = 100, chains = "A"))
      }
      
      # Extract residue numbers and chain IDs
      # PDB format: columns 23-26 for residue number, column 22 for chain
      residue_nums <- as.numeric(substr(atom_lines, 23, 26))
      chains <- unique(substr(atom_lines, 22, 22))
      chains <- chains[chains != " "]  # Remove empty chains
      
      if (length(chains) == 0) chains <- "A"
      
      list(
        min_res = min(residue_nums, na.rm = TRUE),
        max_res = max(residue_nums, na.rm = TRUE),
        chains = chains
      )
    }
    
    # Render 3D structure
    output$structure <- renderR3dmol({
      req(pdb_content())
      
      viewer <- r3dmol() %>%
        m_add_model(data = pdb_content(), format = "pdb") %>%
        m_set_style(style = m_style_cartoon(color = "spectrum")) %>%
        m_zoom_to()
      
      # Add termini highlighting if enabled
      if (!is.null(pdb_info()) && input$highlight_termini) {
        info <- pdb_info()
        n_res <- input$termini_residues
        size_multiplier <- input$termini_size
        
        for (chain in info$chains) {
          # N-terminus (blue) with adjustable size
          n_style <- m_style_cartoon(color = "blue")
          n_style$radius <- size_multiplier  # Adjust thickness
          
          viewer <- viewer %>%
            m_set_style(
              sel = m_sel(chain = chain, resi = info$min_res:(info$min_res + n_res - 1)),
              style = n_style
            ) %>%
            m_add_label(
              text = "N",
              sel = m_sel(chain = chain, resi = info$min_res),
              style = list(
                fontSize = 14 * size_multiplier,
                fontColor = "blue",
                backgroundColor = "white",
                backgroundOpacity = 0.7
              )
            )
          
          # C-terminus (red) with adjustable size
          c_style <- m_style_cartoon(color = "red")
          c_style$radius <- size_multiplier
          
          viewer <- viewer %>%
            m_set_style(
              sel = m_sel(chain = chain, resi = (info$max_res - n_res + 1):info$max_res),
              style = c_style
            ) %>%
            m_add_label(
              text = "C",
              sel = m_sel(chain = chain, resi = info$max_res),
              style = list(
                fontSize = 14 * size_multiplier,
                fontColor = "red",
                backgroundColor = "white",
                backgroundOpacity = 0.7
              )
            )
        }
      }
      
      viewer
    })
    
    # Update style when user changes settings
    observe({
      req(pdb_content())
      
      style_config <- switch(
        input$style_type,
        "cartoon" = m_style_cartoon(color = input$color_scheme),
        "stick" = m_style_stick(color = input$color_scheme),
        "sphere" = m_style_sphere(color = input$color_scheme),
        "line" = m_style_line(color = input$color_scheme),
        "cross" = m_style_cross(color = input$color_scheme)
      )
      
      viewer <- r3dmol() %>%
        m_add_model(data = pdb_content(), format = "pdb") %>%
        m_set_style(style = style_config)
      
      # Add termini highlighting if enabled
      if (!is.null(pdb_info()) && input$highlight_termini) {
        info <- pdb_info()
        n_res <- input$termini_residues
        size_multiplier <- input$termini_size
        
        for (chain in info$chains) {
          # Determine color based on current style
          n_term_color <- "blue"
          c_term_color <- "red"
          
          # N-terminus highlighting with size adjustment
          n_term_style <- switch(
            input$style_type,
            "cartoon" = {
              style <- m_style_cartoon(color = n_term_color)
              style$radius <- size_multiplier
              style
            },
            "stick" = {
              style <- m_style_stick(color = n_term_color)
              style$radius <- 0.2 * size_multiplier
              style
            },
            "sphere" = {
              style <- m_style_sphere(color = n_term_color)
              style$scale <- size_multiplier
              style
            },
            "line" = {
              style <- m_style_line(color = n_term_color)
              style$linewidth <- 2 * size_multiplier
              style
            },
            "cross" = {
              style <- m_style_cross(color = n_term_color)
              style$linewidth <- 2 * size_multiplier
              style
            }
          )
          
          # C-terminus highlighting with size adjustment
          c_term_style <- switch(
            input$style_type,
            "cartoon" = {
              style <- m_style_cartoon(color = c_term_color)
              style$radius <- size_multiplier
              style
            },
            "stick" = {
              style <- m_style_stick(color = c_term_color)
              style$radius <- 0.2 * size_multiplier
              style
            },
            "sphere" = {
              style <- m_style_sphere(color = c_term_color)
              style$scale <- size_multiplier
              style
            },
            "line" = {
              style <- m_style_line(color = c_term_color)
              style$linewidth <- 2 * size_multiplier
              style
            },
            "cross" = {
              style <- m_style_cross(color = c_term_color)
              style$linewidth <- 2 * size_multiplier
              style
            }
          )
          
          viewer <- viewer %>%
            m_set_style(
              sel = m_sel(chain = chain, resi = info$min_res:(info$min_res + n_res - 1)),
              style = n_term_style
            ) %>%
            m_add_label(
              text = "N",
              sel = m_sel(chain = chain, resi = info$min_res),
              style = list(
                fontSize = 14 * size_multiplier,
                fontColor = "blue",
                backgroundColor = "white",
                backgroundOpacity = 0.7
              )
            ) %>%
            m_set_style(
              sel = m_sel(chain = chain, resi = (info$max_res - n_res + 1):info$max_res),
              style = c_term_style
            ) %>%
            m_add_label(
              text = "C",
              sel = m_sel(chain = chain, resi = info$max_res),
              style = list(
                fontSize = 14 * size_multiplier,
                fontColor = "red",
                backgroundColor = "white",
                backgroundOpacity = 0.7
              )
            )
        }
      }
      
      if (input$show_surface) {
        viewer <- viewer %>%
          m_add_surface(style = m_style_surface(opacity = 0.7))
      }
      
      viewer <- viewer %>% m_zoom_to()
      
      if (spinning()) {
        viewer <- viewer %>% m_spin()
      }
      
      output$structure <- renderR3dmol(viewer)
    })
    
    # Reset view
    observeEvent(input$reset_view, {
      req(pdb_content())
      
      viewer <- r3dmol() %>%
        m_add_model(data = pdb_content(), format = "pdb") %>%
        m_set_style(style = m_style_cartoon(color = "spectrum")) %>%
        m_zoom_to()
      
      # Add termini highlighting if enabled
      if (!is.null(pdb_info()) && input$highlight_termini) {
        info <- pdb_info()
        n_res <- input$termini_residues
        size_multiplier <- input$termini_size
        
        for (chain in info$chains) {
          # N-terminus with size adjustment
          n_style <- m_style_cartoon(color = "blue")
          n_style$radius <- size_multiplier
          
          viewer <- viewer %>%
            m_set_style(
              sel = m_sel(chain = chain, resi = info$min_res:(info$min_res + n_res - 1)),
              style = n_style
            ) %>%
            m_add_label(
              text = "N",
              sel = m_sel(chain = chain, resi = info$min_res),
              style = list(
                fontSize = 14 * size_multiplier,
                fontColor = "blue",
                backgroundColor = "white",
                backgroundOpacity = 0.7
              )
            )
          
          # C-terminus with size adjustment
          c_style <- m_style_cartoon(color = "red")
          c_style$radius <- size_multiplier
          
          viewer <- viewer %>%
            m_set_style(
              sel = m_sel(chain = chain, resi = (info$max_res - n_res + 1):info$max_res),
              style = c_style
            ) %>%
            m_add_label(
              text = "C",
              sel = m_sel(chain = chain, resi = info$max_res),
              style = list(
                fontSize = 14 * size_multiplier,
                fontColor = "red",
                backgroundColor = "white",
                backgroundOpacity = 0.7
              )
            )
        }
      }
      
      output$structure <- renderR3dmol(viewer)
    })
    
    # Toggle spin
    observeEvent(input$spin, {
      spinning(!spinning())
    })
    
    # Display PDB information
    output$pdb_info <- renderText({
      req(pdb_content())
      
      lines <- strsplit(pdb_content(), "\n")[[1]]
      
      # Extract header information
      header <- grep("^HEADER", lines, value = TRUE)[1]
      title <- grep("^TITLE", lines, value = TRUE)
      compnd <- grep("^COMPND", lines, value = TRUE)
      source <- grep("^SOURCE", lines, value = TRUE)
      
      # Count atoms
      atom_lines <- grep("^ATOM", lines)
      n_atoms <- length(atom_lines)
      
      # Get resolution if available
      resolution <- grep("^REMARK   2 RESOLUTION", lines, value = TRUE)[1]
      
      info <- paste(
        "UniProt ID:", uniprot_id(),
        "\nPDB File:", basename(pdb_path()),
        "\nNumber of atoms:", n_atoms,
        "\n\n--- Header Information ---",
        if (!is.na(header)) paste0("\n", header) else "",
        if (length(title) > 0) paste0("\n", paste(title, collapse = "\n")) else "",
        if (!is.na(resolution)) paste0("\n", resolution) else "",
        sep = ""
      )
      
      return(info)
    })
    
    # Download PDB file
    output$download_pdb <- downloadHandler(
      filename = function() {
        if (!is.null(pdb_path())) {
          basename(pdb_path())
        } else {
          paste0(uniprot_id(), ".pdb")
        }
      },
      content = function(file) {
        req(pdb_content())
        writeLines(strsplit(pdb_content(), "\n")[[1]], file)
      }
    )
    
    # Return reactive indicating if PDB is loaded
    return(reactive({ !is.null(pdb_content()) }))
  })
}

# Integration instructions:
# 
# 1. Install required package:
#    install.packages("r3dmol")
#
# 2. Add to global.R:
#    library(r3dmol)
#    source('pdb_viewer_module.R')
#
# 3. In ui.R, add new tab to tabsetPanel:
#    tabPanel("3D Structure",
#             pdbViewerUI("pdb_viewer"))
#
# 4. In server.R, add the server call:
#    # Get selected UniProt ID from your selection
#    selected_uniprot <- reactive({
#      req(input$uniprot_select)
#      input$uniprot_select[1]  # Use first selected protein
#    })
#    
#    # Call PDB viewer server
#    pdbViewerServer("pdb_viewer", selected_uniprot, pdb_folder = "pdb_files")