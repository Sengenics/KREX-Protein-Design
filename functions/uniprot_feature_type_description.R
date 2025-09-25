# protein_features_help.R
# Help documentation for protein features - to be used as a tab in Shiny app



# Create the help content UI
proteinFeaturesHelpUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$style(HTML("
        .feature-category {
          background-color: #f8f9fa;
          border-left: 4px solid #007bff;
          padding: 15px;
          margin: 10px 0;
          border-radius: 5px;
        }
        .category-header {
          color: #007bff;
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .impact-high { color: #dc3545; font-weight: bold; }
        .impact-medium { color: #fd7e14; font-weight: bold; }
        .impact-low { color: #28a745; font-weight: bold; }
        .feature-item {
          margin: 5px 0;
          padding-left: 20px;
        }
        .color-legend {
          display: inline-block;
          width: 20px;
          height: 15px;
          margin-right: 10px;
          border: 1px solid #ccc;
          vertical-align: middle;
        }
        .overview-box {
          background-color: #e3f2fd;
          border: 1px solid #2196f3;
          border-radius: 8px;
          padding: 20px;
          margin: 20px 0;
        }
        .usage-tip {
          background-color: #fff3cd;
          border: 1px solid #ffc107;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
        }
      "))
    ),
    
    div(
      h1("🧬 Protein Feature Types Guide", style = "color: #2c3e50; text-align: center;"),
      
      # Overview section
      div(class = "overview-box",
          h3("📋 Overview"),
          p("This tool extracts and visualizes protein features from UniProt annotations to assist in protein design decisions. Features are organized into functional categories that impact different aspects of protein engineering."),
          
          h4("🎨 Visual Elements:"),
          tags$ul(
            tags$li(strong("Colored Tracks:"), "Each category has its own horizontal track with unique colors"),
            tags$li(strong("Light Red Shading:"), "N-terminal and C-terminal regions (default: 50 amino acids each)"),
            tags$li(strong("Feature Overlap:"), "Biologically accurate representation of overlapping functional elements")
          )
      ),
      
      hr(),
      
      # Feature Categories
      h2("📊 Feature Categories", style = "color: #34495e;"),
      
      # Targeting & Secretion
      div(class = "feature-category",
          div(class = "category-header", 
              span(class = "color-legend", style = "background-color: #E74C3C;"),
              "🎯 Targeting & Secretion"
          ),
          p("Critical for determining protein localization and processing:"),
          div(class = "feature-item", strong("Signal Peptide"), " - N-terminal sequence directing proteins to the secretory pathway (ER/Golgi)"),
          div(class = "feature-item", strong("Transit Peptide"), " - Targets proteins to organelles (mitochondria, chloroplasts)"),
          div(class = "feature-item", strong("Propeptide"), " - Temporary sequence removed during protein maturation"),
          p(span(class = "impact-high", "Design Impact: HIGH"), " - Essential for proper protein trafficking and expression")
      ),
      
      # Membrane Topology
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #1ABC9C;"),
              "🧱 Membrane Topology"
          ),
          p("Defines how proteins interact with cellular membranes:"),
          div(class = "feature-item", strong("Transmembrane"), " - Regions spanning lipid bilayers"),
          div(class = "feature-item", strong("Intramembrane"), " - Regions embedded within membrane but not crossing it"),
          div(class = "feature-item", strong("Topological Domain"), " - Regions on specific sides of membranes (cytoplasmic/extracellular)"),
          p(span(class = "impact-high", "Design Impact: HIGH"), " - Critical for membrane protein function and stability")
      ),
      
      # Functional Sites  
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #2196F3;"),
              "⚙️ Functional Sites"
          ),
          p("Regions essential for protein function:"),
          div(class = "feature-item", strong("Domain"), " - Structural/functional units with specific roles"),
          div(class = "feature-item", strong("Active Site"), " - Residues directly involved in catalysis"),
          div(class = "feature-item", strong("Binding Site"), " - Regions that bind ligands, substrates, or other proteins"),
          div(class = "feature-item", strong("Site"), " - Other functionally important positions"),
          div(class = "feature-item", strong("Metal Binding"), " - Coordination sites for metal ions (Fe, Zn, Mg, etc.)"),
          div(class = "feature-item", strong("DNA Binding"), " - Regions that interact with nucleic acids"),
          div(class = "feature-item", strong("Calcium Binding"), " - Specific Ca²⁺ coordination sites"),
          div(class = "feature-item", strong("Nucleotide Binding"), " - ATP/GTP binding regions"),
          p(span(class = "impact-high", "Design Impact: HIGH"), " - Modifications here directly affect protein function")
      ),
      
      # Structural Elements
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #FF4081;"),
              "🏗️ Structural Elements"
          ),
          p("Features that define protein architecture:"),
          div(class = "feature-item", strong("Repeat"), " - Repeated sequence motifs"),
          div(class = "feature-item", strong("Motif"), " - Short conserved sequence patterns"),
          div(class = "feature-item", strong("Coiled Coil"), " - α-helical structures that interact with other helices"),
          div(class = "feature-item", strong("Zinc Finger"), " - DNA-binding domains coordinated by zinc"),
          div(class = "feature-item", strong("Region"), " - Functionally important but structurally diverse areas"),
          p(span(class = "impact-medium", "Design Impact: MEDIUM"), " - Important for stability and protein-protein interactions")
      ),
      
      # Post-Translational Modifications
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #CDDC39;"),
              "🔧 Post-Translational Modifications"
          ),
          p("Chemical modifications that occur after translation:"),
          div(class = "feature-item", strong("Modified Residue"), " - Any chemically altered amino acid"),
          div(class = "feature-item", strong("Glycosylation"), " - Addition of carbohydrate groups (N-linked/O-linked)"),
          div(class = "feature-item", strong("Disulfide Bond"), " - Covalent bonds between cysteine residues"),
          div(class = "feature-item", strong("Lipidation"), " - Attachment of lipid groups (myristoylation, palmitoylation)"),
          div(class = "feature-item", strong("Cross-link"), " - Covalent bonds between different residues"),
          p(span(class = "impact-medium", "Design Impact: MEDIUM-HIGH"), " - Affects folding, stability, and function")
      ),
      
      # Sequence Variants
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #536DFE;"),
              "🧬 Sequence Variants"
          ),
          p("Natural and experimental sequence variations:"),
          div(class = "feature-item", strong("Natural Variant"), " - Polymorphisms found in populations"),
          div(class = "feature-item", strong("Mutagenesis"), " - Experimentally tested mutations"),
          div(class = "feature-item", strong("Alternative Sequence"), " - Alternative splicing or processing variants"),
          div(class = "feature-item", strong("Sequence Conflict"), " - Discrepancies between different sequence sources"),
          p(span(class = "impact-low", "Design Impact: LOW-MEDIUM"), " - Indicates tolerance for sequence changes")
      ),
      
      # Processing & Maturation
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #D4E157;"),
              "🔄 Processing & Maturation"
          ),
          p("Features related to protein processing:"),
          div(class = "feature-item", strong("Chain"), " - Mature protein after signal peptide removal"),
          div(class = "feature-item", strong("Peptide"), " - Released bioactive fragments"),
          div(class = "feature-item", strong("Initiator Methionine"), " - Starting methionine (often removed)"),
          div(class = "feature-item", strong("Non-terminal Residue"), " - Residues at unusual chain positions"),
          p(span(class = "impact-medium", "Design Impact: MEDIUM"), " - Important for proper protein maturation")
      ),
      
      # Secondary Structure
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #FF0080;"),
              "📐 Secondary Structure"
          ),
          p("Predicted or experimentally determined structural elements:"),
          div(class = "feature-item", strong("Helix"), " - α-helical regions"),
          div(class = "feature-item", strong("Beta Strand"), " - β-sheet forming regions"),
          div(class = "feature-item", strong("Turn"), " - Loop regions connecting structured elements"),
          p(span(class = "impact-low", "Design Impact: LOW-MEDIUM"), " - Guides structural predictions and modifications")
      ),
      
      # Other Features
      div(class = "feature-category",
          div(class = "category-header",
              span(class = "color-legend", style = "background-color: #37474F;"),
              "❓ Other Features"
          ),
          p("Catch-all for additional feature types:"),
          div(class = "feature-item", strong("Compositional Bias"), " - Regions enriched in specific amino acids"),
          div(class = "feature-item", "Any novel or unclassified feature types from newer UniProt releases"),
          p(span(class = "impact-medium", "Design Impact: VARIABLE"), " - Depends on specific feature type")
      ),
      
      hr(),
      
      # Usage Guidelines
      div(class = "usage-tip",
          h3("💡 Using This Information for Protein Design"),
          tags$ol(
            tags$li(strong("Signal Recognition"), " - Check N-terminus for targeting signals"),
            tags$li(strong("Membrane Integration"), " - Identify transmembrane regions for proper folding"),
            tags$li(strong("Functional Preservation"), " - Avoid modifying critical active sites and binding regions"),
            tags$li(strong("Structural Stability"), " - Consider disulfide bonds and structural motifs"),
            tags$li(strong("Expression Optimization"), " - Account for glycosylation sites and processing signals")
          ),
          p(strong("Note:"), "Feature overlap is biologically accurate - many functional elements span multiple categories and work together to determine protein behavior.")
      ),
      
      hr(),
      
      # Feature Summary Table
      h3("📋 Quick Reference Table"),
      DTOutput(ns("feature_summary_table"))
    )
  )
}

# Server function for the help tab
proteinFeaturesHelpServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create summary table
    feature_summary <- data.frame(
      Category = c(
        "Targeting & Secretion",
        "Membrane Topology", 
        "Functional Sites",
        "Structural Elements",
        "Post-Translational Modifications",
        "Sequence Variants",
        "Processing & Maturation",
        "Secondary Structure",
        "Other Features"
      ),
      `Key Features` = c(
        "Signal, Transit peptide, Propeptide",
        "Transmembrane, Intramembrane, Topological domain",
        "Domain, Active site, Binding site, Metal binding",
        "Repeat, Motif, Coiled coil, Zinc finger",
        "Glycosylation, Disulfide bond, Lipidation",
        "Natural variant, Mutagenesis, Alternative sequence",
        "Chain, Peptide, Initiator methionine",
        "Helix, Beta strand, Turn",
        "Compositional bias, Novel features"
      ),
      `Design Impact` = c(
        "HIGH", "HIGH", "HIGH", "MEDIUM", 
        "MEDIUM-HIGH", "LOW-MEDIUM", "MEDIUM", 
        "LOW-MEDIUM", "VARIABLE"
      ),
      `Color Code` = c(
        "#E74C3C", "#1ABC9C", "#2196F3", "#FF4081",
        "#CDDC39", "#536DFE", "#D4E157", "#FF0080", "#37474F"
      ),
      stringsAsFactors = FALSE
    )
    
    output$feature_summary_table <- renderDT({
      DT::datatable(
        feature_summary,
        options = list(
          pageLength = 15,
          dom = 't',
          columnDefs = list(
            list(
              targets = 3, # Color Code column
              render = JS(
                "function(data, type, row, meta) {",
                "  if(type === 'display') {",
                "    return '<div style=\"background-color: ' + data + '; width: 60px; height: 25px; border: 1px solid #ccc; display: inline-block; margin-right: 10px;\"></div>' + data;",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        ),
        rownames = FALSE,
        escape = FALSE
      )
    })
  })
}

# Usage in your main app:
# In ui.R, add this tab:
# tabPanel("Feature Guide", proteinFeaturesHelpUI("help"))
# 
# In server.R, add this server call:
# proteinFeaturesHelpServer("help")