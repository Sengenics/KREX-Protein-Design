# app.R
# R Shiny: UniProt → ChatGPT Q&A → AlphaFold structure viewer

#source('requirements.R')
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)
library(DT)
library(NGLVieweR)

example_protein_data = read.xsls('Data/Protein Design Example.xlsx')

# -------- Helpers --------

# Build AlphaFold PDB URL from UniProt ID (most structures are model_v4)
alphafold_pdb_url <- function(uniprot_id) {
  # Basic sanitization
  uid <- gsub("\\s", "", toupper(uniprot_id))
  # Primary (v4) URL pattern; fallback (v3) tried lazily in loader
  paste0("https://alphafold.ebi.ac.uk/files/AF-", uid, "-F1-model_v4.pdb")
}

# Try URL; if 404, try v3; else return NA
resolve_alphafold_url <- function(uniprot_id) {
  v4 <- alphafold_pdb_url(uniprot_id)
  r <- try(HEAD(v4, timeout(10)), silent = TRUE)
  if (!inherits(r, "try-error") && identical(status_code(r), 200L)) return(v4)
  v3 <- paste0("https://alphafold.ebi.ac.uk/files/AF-", toupper(uniprot_id), "-F1-model_v3.pdb")
  r2 <- try(HEAD(v3, timeout(10)), silent = TRUE)
  if (!inherits(r2, "try-error") && identical(status_code(r2), 200L)) return(v3)
  return(NA_character_)
}

# Minimal OpenAI-compatible Chat Completions call
# - Works with OPENAI_API_KEY env var or provided api_key
# - model default can be changed in UI
ask_chatgpt <- function(prompt, api_key = Sys.getenv("OPENAI_API_KEY"), model = "gpt-4o-mini") {
  if (is.null(api_key) || api_key == "") {
    stop("No API key provided. Set OPENAI_API_KEY env var or enter a key in the UI.")
  }
  url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = model,
    messages = list(
      list(role = "system",
           content = "You are a concise protein knowledge assistant. Cite databases when appropriate (UniProt, InterPro, GO, Reactome)."),
      list(role = "user", content = prompt)
    ),
    temperature = 0.2
  )
  resp <- POST(
    url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    accept_json(),
    body = body,
    encode = "json",
    timeout(60)
  )
  stop_for_status(resp)
  out <- content(resp, as = "parsed", type = "application/json")
  txt <- try(out$choices[[1]]$message$content %||% "", silent = TRUE)
  if (inherits(txt, "try-error") || is.null(txt)) txt <- ""
  txt
}

# Build the per-protein prompt once per question
build_prompt <- function(uniprot_id, protein_name = NULL, question) {
  glue <- function(...) paste(..., collapse = "")
  glue(
    "Protein context:\n",
    "- UniProt ID: ", uniprot_id, if (!is.null(protein_name) && nzchar(protein_name)) paste0("\n- Name: ", protein_name) else "", "\n\n",
    "Question: ", question, "\n\n",
    "Instructions: Keep it accurate and succinct (5–8 bullet points if applicable). ",
    "Prefer primary resources (UniProt, GO, InterPro, Reactome, PDBe/AlphaFold). ",
    "If uncertain, say so briefly."
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# -------- UI --------
ui <- fluidPage(
  titlePanel("Protein Q&A + AlphaFold Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("excel", "Upload Excel (.xlsx) with UniProt IDs", accept = c(".xlsx")),
      uiOutput("sheetPicker"),
      uiOutput("idColumnPicker"),
      textInput("nameCol", "Optional: column for Protein Name (optional)", placeholder = "e.g. Protein, Gene, Symbol"),
      textAreaInput("questions", "Questions to ask (one per line)", rows = 7, placeholder =
                      "1) What is the primary function?
2) Known domains/motifs?
3) Subcellular localization?
4) Disease or phenotype associations?
5) Key pathways/interactions?"),
      passwordInput("apiKey", "OpenAI API Key (optional if OPENAI_API_KEY is set)"),
      textInput("model", "Model", value = "gpt-4o-mini"),
      checkboxInput("throttle", "Be gentle (1s delay between questions)", TRUE),
      actionButton("run", "Run Q&A", class = "btn-primary"),
      hr(),
      helpText("Tip: Your Excel must include a column with UniProt IDs (e.g. Q9Y261).")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview",
                 h5("Detected rows"),
                 DTOutput("previewTable")
        ),
        tabPanel("Q&A Results",
                 downloadButton("downloadCSV", "Download CSV"),
                 DTOutput("qaTable")
        ),
        tabPanel("AlphaFold Viewer",
                 fluidRow(
                   column(6,
                          selectInput("viewerProtein", "Pick a UniProt ID to view", choices = NULL)
                   ),
                   column(6,
                          verbatimTextOutput("viewerStatus")
                   )
                 ),
                 NGLVieweROutput("ngl", height = "600px")
        )
      )
    )
  )
)

# -------- Server --------
server <- function(input, output, session) {
  
  # Reactive: read workbook & list sheets
  sheets <- reactive({
    req(input$excel)
    excel_sheets(path = input$excel$datapath)
  })
  
  output$sheetPicker <- renderUI({
    req(input$excel)
    selectInput("sheet", "Sheet", choices = sheets())
  })
  
  # Sample data from chosen sheet
  df_raw <- reactive({
    req(input$excel, input$sheet)
    suppressWarnings(read_excel(path = input$excel$datapath, sheet = input$sheet))
  })
  
  # Let user pick UniProt ID column (auto-guess common names)
  output$idColumnPicker <- renderUI({
    req(df_raw())
    cols <- names(df_raw())
    guess <- cols[grepl("uniprot|protein.?id|accession|acc", cols, ignore.case = TRUE)]
    selectInput("idCol", "UniProt ID column", choices = cols,
                selected = if (length(guess)) guess[[1]] else NULL)
  })
  
  # Preview
  output$previewTable <- renderDT({
    req(df_raw())
    datatable(head(df_raw(), 20), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Q&A execution
  questions_vec <- reactive({
    qs <- strsplit(input$questions %||% "", "\n", fixed = TRUE)[[1]]
    qs <- trimws(qs)
    qs[nzchar(qs)]
  })
  
  # Store results
  qa_results <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    req(df_raw(), input$idCol)
    ids <- df_raw()[[input$idCol]] %>% as.character() %>% toupper() %>% trimws()
    nm_col <- input$nameCol
    names_vec <- if (!is.null(nm_col) && nzchar(nm_col) && nm_col %in% names(df_raw())) {
      df_raw()[[nm_col]] %>% as.character()
    } else rep(NA_character_, length(ids))
    
    ids <- ids[nzchar(ids)]
    validate(need(length(ids) > 0, "No UniProt IDs found in the selected column."))
    
    qs <- questions_vec()
    validate(need(length(qs) > 0, "Please provide at least one question."))
    
    api_key <- if (nzchar(input$apiKey)) input$apiKey else Sys.getenv("OPENAI_API_KEY")
    validate(need(nzchar(api_key), "Provide an OpenAI API key or set OPENAI_API_KEY environment variable."))
    
    model <- input$model %||% "gpt-4o-mini"
    
    showModal(modalDialog("Running Q&A… this can take a bit depending on rows × questions.", footer = NULL))
    on.exit(removeModal(), add = TRUE)
    
    prog <- shiny::Progress$new(min = 0, max = length(ids) * length(qs))
    on.exit(prog$close(), add = TRUE)
    prog$set(message = "Querying ChatGPT", value = 0)
    
    res <- list()
    k <- 0L
    
    for (i in seq_along(ids)) {
      uniprot <- ids[i]
      pname <- names_vec[i]
      for (q in qs) {
        k <- k + 1L
        prog$set(detail = paste0("Protein ", uniprot, " — Q", which(qs == q), "/", length(qs)),
                 value = k)
        prompt <- build_prompt(uniprot, pname, q)
        ans <- tryCatch(
          ask_chatgpt(prompt, api_key = api_key, model = model),
          error = function(e) paste("Error:", e$message)
        )
        res[[length(res)+1]] <- tibble::tibble(
          UniProt = uniprot,
          ProteinName = pname %||% NA_character_,
          Question = q,
          Answer = ans
        )
        if (isTRUE(input$throttle)) Sys.sleep(1)
      }
    }
    
    qa_results(dplyr::bind_rows(res))
    # update viewer list from found IDs
    updateSelectInput(session, "viewerProtein", choices = unique(ids), selected = ids[[1]])
  })
  
  output$qaTable <- renderDT({
    req(qa_results())
    datatable(
      qa_results(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() paste0("protein_qa_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content = function(file) {
      req(qa_results())
      readr::write_csv(qa_results(), file)
    }
  )
  
  # ---------- AlphaFold viewer ----------
  output$viewerStatus <- renderText({
    req(input$viewerProtein)
    paste0("Attempting to load AlphaFold model for UniProt ID: ", input$viewerProtein)
  })
  
  output$ngl <- renderNGLVieweR({
    req(input$viewerProtein)
    url <- resolve_alphafold_url(input$viewerProtein)
    if (is.na(url)) {
      # Render empty viewer with status note
      widget <- NGLVieweR()
      return(widget)
    }
    stage <- NGLVieweR(url)
    stage %>%
      addRepresentation("cartoon") %>%
      addRepresentation("ball+stick", param = list(sele = "hetero and not water")) %>%
      setQuality("medium") %>%
      stageParameters(backgroundColor = "white") %>%
      setFocus(0)
  })
}

shinyApp(ui, server)
