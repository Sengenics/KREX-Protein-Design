# Protein HTML Report Generator Module
library(shiny)
library(htmltools)
library(plotly)

# UI Module
proteinReportGeneratorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    wellPanel(
      h4("Generate HTML Protein Report"),
      
      fluidRow(
        column(6,
               selectInput(ns("report_protein"), 
                           "Select Protein for Report:",
                           choices = NULL)
        ),
        column(6,
               br(),
               downloadButton(ns("download_html"), 
                              "Download HTML Report", 
                              class = "btn-success")
        )
      ),
      
      checkboxGroupInput(ns("report_sections"),
                         "Include in Report:",
                         choices = c(
                           "Protein Information" = "info",
                           "Recommendation Summary" = "recommendation",
                           "Feature Impact Analysis" = "impact",
                           "Feature Map Visualization" = "feature_map",
                           "Protein Sequence" = "sequence",
                           "PDB Structure (if available)" = "pdb"
                         ),
                         selected = c("info", "recommendation", "impact", "feature_map", "sequence")
      )
    )
  )
}

# Server Module
proteinReportGeneratorServer <- function(id, protein_data, feature_data, report_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update protein choices
    observe({
      req(protein_data())
      df <- protein_data()
      
      if ("uniprot_id" %in% names(df)) {
        choices <- df$uniprot_id
        names(choices) <- paste0(df$uniprot_id, 
                                 if ("protein_name" %in% names(df)) {
                                   paste0(" - ", df$protein_name)
                                 } else {
                                   ""
                                 })
        
        updateSelectInput(session, "report_protein", choices = choices)
      }
    })
    
    # Generate HTML report
    output$download_html <- downloadHandler(
      filename = function() {
        paste0("protein_report_", input$report_protein, "_", 
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        # Get data for selected protein
        selected_protein <- input$report_protein
        
        # Call the reactive expressions properly
        all_proteins <- protein_data()
        all_features <- feature_data()
        all_reports <- report_data()
        
        # Now filter the data frames
        protein_info <- all_proteins %>%
          filter(uniprot_id == selected_protein)
        
        features <- all_features %>%
          filter(uniprot_id == selected_protein)
        
        report <- all_reports %>%
          filter(uniprot_id == selected_protein)
        
        # Generate HTML content
        html_content <- generate_html_report(
          protein_id = selected_protein,
          protein_info = protein_info,
          features = features,
          report = report,
          sections = input$report_sections
        )
        
        # Write to file
        writeLines(html_content, file)
      }
    )
  })
}

# Helper function to generate complete HTML report
generate_html_report <- function(protein_id, protein_info, features, report, sections) {
  
  # CSS styling
  css <- "
  <style>
    body {
      font-family: Arial, sans-serif;
      max-width: 1200px;
      margin: 20px auto;
      padding: 20px;
      background-color: #f5f5f5;
    }
    .header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 30px;
      border-radius: 10px;
      margin-bottom: 30px;
    }
    .section {
      background: white;
      padding: 20px;
      margin-bottom: 20px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .section h2 {
      color: #667eea;
      border-bottom: 2px solid #667eea;
      padding-bottom: 10px;
      margin-bottom: 20px;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 15px 0;
    }
    th {
      background-color: #667eea;
      color: white;
      padding: 12px;
      text-align: left;
      font-weight: bold;
    }
    td {
      padding: 10px;
      border-bottom: 1px solid #ddd;
    }
    tr:hover {
      background-color: #f8f9fa;
    }
    .recommendation {
      padding: 15px;
      border-radius: 5px;
      margin: 15px 0;
      font-size: 16px;
    }
    .recommend-yes {
      background-color: #d4edda;
      border-left: 4px solid #28a745;
    }
    .recommend-no {
      background-color: #f8d7da;
      border-left: 4px solid #dc3545;
    }
    .impact-HIGH {
      color: #dc3545;
      font-weight: bold;
    }
    .impact-MODERATE {
      color: #ffc107;
      font-weight: bold;
    }
    .impact-LOW {
      color: #28a745;
    }
    .sequence {
      font-family: 'Courier New', monospace;
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 5px;
      line-height: 1.6;
      word-wrap: break-word;
      font-size: 12px;
    }
    .metadata {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 15px;
      margin: 15px 0;
    }
    .metadata-item {
      padding: 10px;
      background-color: #f8f9fa;
      border-radius: 5px;
    }
    .metadata-label {
      font-weight: bold;
      color: #667eea;
      margin-bottom: 5px;
    }
    .footer {
      text-align: center;
      color: #6c757d;
      margin-top: 40px;
      padding-top: 20px;
      border-top: 1px solid #ddd;
    }
  </style>
  "
  
  # Header
  header <- paste0(
    "<div class='header'>",
    "<h1>Protein Analysis Report</h1>",
    "<h2>", protein_id, "</h2>",
    "<p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
    "</div>"
  )
  
  # Sections
  content <- ""
  
  # 1. Protein Information
  if ("info" %in% sections && nrow(protein_info) > 0) {
    content <- paste0(content, "<div class='section'>",
                      "<h2>Protein Information</h2>",
                      "<div class='metadata'>")
    
    info_fields <- c("protein_name", "genes_geneName", "protein_length", 
                     "Secreted", "Multimeric", "preferred_terminus")
    
    for (field in info_fields) {
      if (field %in% names(protein_info)) {
        label <- gsub("_", " ", field)
        label <- tools::toTitleCase(label)
        value <- protein_info[[field]]
        
        content <- paste0(content,
                          "<div class='metadata-item'>",
                          "<div class='metadata-label'>", label, "</div>",
                          "<div>", ifelse(is.na(value), "N/A", value), "</div>",
                          "</div>")
      }
    }
    
    content <- paste0(content, "</div></div>")
  }
  
  # 2. Recommendation Summary
  if ("recommendation" %in% sections && nrow(report) > 0) {
    rec_class <- ifelse(grepl("RECOMMEND", report$recommendation[1]), 
                        "recommend-yes", "recommend-no")
    
    content <- paste0(content, "<div class='section'>",
                      "<h2>Design Recommendation</h2>",
                      "<div class='recommendation ", rec_class, "'>",
                      report$recommendation[1],
                      "</div>",
                      "<table>",
                      "<tr><th>Terminal</th><th>Buffer (aa)</th><th>Score</th></tr>")
    
    if ("c_term_buffer" %in% names(report)) {
      content <- paste0(content,
                        "<tr><td>C-terminal</td>",
                        "<td>", report$c_term_buffer[1], "</td>",
                        "<td>", round(report$c_terminal_score[1], 2), "</td></tr>")
    }
    
    if ("n_term_buffer" %in% names(report)) {
      content <- paste0(content,
                        "<tr><td>N-terminal</td>",
                        "<td>", report$n_term_buffer[1], "</td>",
                        "<td>", round(report$n_terminal_score[1], 2), "</td></tr>")
    }
    
    content <- paste0(content, "</table></div>")
  }
  
  # 3. Feature Impact Analysis
  if ("impact" %in% sections && nrow(features) > 0) {
    content <- paste0(content, "<div class='section'>",
                      "<h2>Feature Impact Analysis</h2>",
                      "<table>",
                      "<tr><th>Terminal</th><th>Feature Type</th><th>Position</th>",
                      "<th>Impact</th><th>Reason</th><th>Buffer</th></tr>")
    
    for (i in 1:nrow(features)) {
      feat <- features[i, ]
      impact_class <- paste0("impact-", feat$Impact_Level)
      
      content <- paste0(content,
                        "<tr>",
                        "<td>", feat$Terminal, "</td>",
                        "<td>", feat$Feature_Type, "</td>",
                        "<td>", feat$Position, "</td>",
                        "<td class='", impact_class, "'>", feat$Impact_Level, "</td>",
                        "<td>", feat$Reason, "</td>",
                        "<td>", feat$buffer, "</td>",
                        "</tr>")
    }
    
    content <- paste0(content, "</table></div>")
  }
  
  # 4. Feature Map (as embedded HTML if available)
  if ("feature_map" %in% sections) {
    content <- paste0(content, "<div class='section'>",
                      "<h2>Feature Map</h2>",
                      "<p><em>Note: Interactive feature map requires the full Shiny application. ",
                      "See protein features visualization in the analysis tool.</em></p>",
                      "</div>")
  }
  
  # 5. Protein Sequence
  if ("sequence" %in% sections && "sequence" %in% names(protein_info)) {
    seq <- protein_info$sequence[1]
    if (!is.na(seq) && nchar(seq) > 0) {
      # Format sequence with line breaks every 60 characters
      formatted_seq <- gsub("(.{60})", "\\1\n", seq)
      
      content <- paste0(content, "<div class='section'>",
                        "<h2>Protein Sequence</h2>",
                        "<p><strong>Length:</strong> ", nchar(seq), " amino acids</p>",
                        "<div class='sequence'>", formatted_seq, "</div>",
                        "</div>")
    }
  }
  
  # Footer
  footer <- paste0(
    "<div class='footer'>",
    "<p>Report generated by Protein Analysis Tool</p>",
    "<p>For questions or issues, contact your bioinformatics team</p>",
    "</div>"
  )
  
  # Combine everything
  html <- paste0(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<title>Protein Report - ", protein_id, "</title>",
    css,
    "</head>",
    "<body>",
    header,
    content,
    footer,
    "</body>",
    "</html>"
  )
  
  return(html)
}

# Example usage in your app:
# 
# UI:
# proteinReportGeneratorUI("report_gen")
# 
# Server:
# proteinReportGeneratorServer(
#   "report_gen",
#   protein_data = reactive({ filtered_proteins }),
#   feature_data = reactive({ feature_impacts }),
#   report_data = reactive({ recommendation_data })
# )