# functions/openai_functions.R
# ------------------------------------------------------------------------------
# OpenAI API Helper Functions 
# ------------------------------------------------------------------------------

library(httr2)
library(jsonlite)

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Determine if model is GPT-5
is_gpt5_model <- function(model) {
  grepl("^gpt-5", model, ignore.case = TRUE)
}

# Determine if model is an o-series reasoning model (o1/o3)
is_reasoning_model <- function(model) {
  grepl("^o1|^o3", model, ignore.case = TRUE)
}

# Extract text from API response
extract_response_text <- function(body) {
  tryCatch({
    if (!is.null(body$choices) && length(body$choices) > 0) {
      return(body$choices[[1]]$message$content)
    }
    return(NULL)
  }, error = function(e) NULL)
}

# UPDATE the openai_request function to check for empty responses

openai_request <- function(prompt, model = "gpt-5", max_tokens = 10000) {
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (is.null(api_key) || api_key == "" || api_key == "your_api_key_here") {
    return(list(
      success = FALSE,
      content = NULL,
      error = "No valid API key found."
    ))
  }
  
  endpoint <- "https://api.openai.com/v1/chat/completions"
  
  # Ensure prompt is simple string
  if (is.list(prompt)) {
    prompt <- paste(unlist(prompt), collapse = "\n")
  }
  prompt <- as.character(prompt)[1]
  
  # Build body - different for GPT-5
  if (is_gpt5_model(model)) {
    body <- list(
      model = model,
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      max_completion_tokens = max_tokens,
      reasoning_effort = "medium"
    )
    
    timeout_seconds <- 180  # 3 minutes
    
    cat("\nSending request to OpenAI...\n")
    cat("  Model:", model, " (reasoning limited to 'low')\n")
    cat("  Max completion tokens: 3000\n")
    cat("  Reasoning effort: LOW (faster, cheaper)\n")
    cat("  Timeout:", timeout_seconds, "seconds\n")
    
  } else {
    body <- list(
      model = model,
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      max_tokens = max_tokens
    )
  }
  
  cat("\nSending request to OpenAI...\n")
  cat("  Model:", model, "\n")
  cat("  Using parameter:", if(is_gpt5_model(model)) "max_completion_tokens" else "max_tokens", "\n")
  cat("  Prompt length:", nchar(prompt), "characters\n")
  
  result <- tryCatch({
    
    req <- httr2::request(endpoint) |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(300)
    
    cat("Making API call...\n")
    resp <- httr2::req_perform(req)
    
    # Get raw response for debugging
    cat("Response received, status:", httr2::resp_status(resp), "\n")
    
    # Parse response
    content <- httr2::resp_body_json(resp)
    
    # DEBUG: Print full response structure
    cat("\n=== FULL API RESPONSE ===\n")
    cat(jsonlite::toJSON(content, auto_unbox = TRUE, pretty = TRUE), "\n")
    cat("=========================\n\n")
    
    # Check for API error
    if (!is.null(content$error)) {
      cat("API Error:", content$error$message, "\n")
      return(list(
        success = FALSE,
        content = NULL,
        error = paste("OpenAI API Error:", content$error$message),
        usage = NULL
      ))
    }
    
    # Extract usage
    usage <- content$usage
    prompt_tokens <- usage$prompt_tokens %||% 0
    completion_tokens <- usage$completion_tokens %||% 0
    total_tokens <- usage$total_tokens %||% 0
    
    # Extract content
    response_content <- content$choices[[1]]$message$content
    
    # CHECK FOR EMPTY RESPONSE
    if (is.null(response_content) || nchar(response_content) == 0) {
      cat("⚠️  WARNING: GPT-5 returned EMPTY response but charged tokens!\n")
      cat("   Prompt tokens:", prompt_tokens, "\n")
      cat("   Completion tokens:", completion_tokens, "\n")
      cat("   This suggests the model hit a filter or safety mechanism.\n")
      
      # Check finish_reason
      finish_reason <- content$choices[[1]]$finish_reason %||% "unknown"
      cat("   Finish reason:", finish_reason, "\n")
      
      return(list(
        success = FALSE,
        content = NULL,
        error = paste0("Empty response from GPT-5 (finish_reason: ", finish_reason, ")"),
        usage = list(
          prompt_tokens = prompt_tokens,
          completion_tokens = completion_tokens,
          total_tokens = total_tokens,
          model = model
        )
      ))
    }
    
    # Log usage
    cat("\n=== OpenAI Token Usage ===\n")
    cat("Model:", model, "\n")
    cat("Prompt tokens:", prompt_tokens, "\n")
    cat("Completion tokens:", completion_tokens, "\n")
    cat("Total tokens:", total_tokens, "\n")
    if (exists("calculate_cost")) {
      cat("Estimated cost: $", round(calculate_cost(model, prompt_tokens, completion_tokens), 4), "\n")
    }
    cat("Finish reason:", content$choices[[1]]$finish_reason %||% "unknown", "\n")
    cat("==========================\n\n")
    
    return(list(
      success = TRUE,
      content = response_content,
      error = NULL,
      usage = list(
        prompt_tokens = prompt_tokens,
        completion_tokens = completion_tokens,
        total_tokens = total_tokens,
        model = model,
        finish_reason = content$choices[[1]]$finish_reason %||% "unknown"
      )
    ))
    
  }, error = function(e) {
    error_msg <- conditionMessage(e)
    cat("\n❌ ERROR ❌\n")
    cat("Error message:", error_msg, "\n")
    
    return(list(
      success = FALSE,
      content = NULL,
      error = paste("OpenAI API Error:", error_msg),
      usage = NULL
    ))
  })
  
  return(result)
}

# =============================================================================
# EPITOPE PROMPT BUILDER
# =============================================================================

build_epitope_prompt <- function(protein_info) {
  
  # Extract fields safely - ensure single values
  protein_name <- if (!is.null(protein_info$protein_name)) {
    as.character(protein_info$protein_name)[1]
  } else if (!is.null(protein_info$name)) {
    as.character(protein_info$name)[1]
  } else {
    as.character(protein_info$uniprot_id)[1]
  }
  
  organism <- if (!is.null(protein_info$organism)) {
    as.character(protein_info$organism)[1]
  } else if (!is.null(protein_info$Organism)) {
    as.character(protein_info$Organism)[1]
  } else {
    "Unknown"
  }
  
  uniprot_id <- if (!is.null(protein_info$uniprot_id)) {
    as.character(protein_info$uniprot_id)[1]
  } else if (!is.null(protein_info$id)) {
    as.character(protein_info$id)[1]
  } else {
    "Unknown"
  }
  
  cat("Building epitope prompt for:\n")
  cat("  protein_name:", protein_name, "\n")
  cat("  organism:", organism, "\n")
  cat("  uniprot_id:", uniprot_id, "\n")
  
  # Build prompt
  prompt <- paste0(
    "You are a scientific literature expert specializing in immunology and protein biochemistry.\n\n",
    
    "TASK: Search your knowledge for autoimmune epitopes and antibody binding sites for this protein:\n\n",
    
    "Protein: ", protein_name, "\n",
    "Organism: ", organism, "\n",
    "UniProt ID: ", uniprot_id, "\n\n",
    
    "Find epitopes that are:\n",
    "1. Recognized by autoantibodies in autoimmune diseases\n",
    "2. Linear epitopes (amino acid sequences) preferred\n",
    "3. Validated in experimental studies (ELISA, peptide arrays, etc.)\n",
    "4. Associated with specific autoimmune conditions\n\n",
    
    "Return results as a JSON array with this EXACT structure:\n",
    "[\n",
    "  {\n",
    '    "uniprot_id": "', uniprot_id, '",\n',
    '    "Epitope_Sequence": "amino acid sequence",\n',
    '    "Position": "start-end (e.g., 120-135)",\n',
    '    "Epitope_Type": "linear or conformational",\n',
    '    "Evidence_Level": 1-5,\n',
    '    "Citation": "PMID or DOI",\n',
    '    "Antibody_Context": "disease and assay method",\n',
    '    "Source": "OpenAI"\n',
    "  }\n",
    "]\n\n",
    
    "Evidence Level Scoring:\n",
    "5 = Multiple independent studies, strong validation\n",
    "4 = Well-validated, replicated findings\n",
    "3 = Single study with good validation\n",
    "2 = Preliminary or limited validation\n",
    "1 = Computational prediction or weak evidence\n\n",
    
    "Return ONLY the JSON array, no other text."
  )
  
  # Ensure single string
  prompt <- paste(prompt, collapse = "")
  
  cat("Final prompt length:", nchar(prompt), "characters\n")
  
  return(prompt)
}

# =============================================================================
# EXPRESSION PROMPT BUILDER
# =============================================================================

build_expression_prompt <- function(protein_info) {
  
  # Extract fields safely
  protein_name <- if (!is.null(protein_info$protein_name)) {
    as.character(protein_info$protein_name)[1]
  } else if (!is.null(protein_info$name)) {
    as.character(protein_info$name)[1]
  } else {
    as.character(protein_info$uniprot_id)[1]
  }
  
  organism <- if (!is.null(protein_info$organism)) {
    as.character(protein_info$organism)[1]
  } else if (!is.null(protein_info$Organism)) {
    as.character(protein_info$Organism)[1]
  } else {
    "Unknown"
  }
  
  uniprot_id <- if (!is.null(protein_info$uniprot_id)) {
    as.character(protein_info$uniprot_id)[1]
  } else if (!is.null(protein_info$id)) {
    as.character(protein_info$id)[1]
  } else {
    "Unknown"
  }
  
  cat("Building expression prompt for:\n")
  cat("  protein_name:", protein_name, "\n")
  cat("  organism:", organism, "\n")
  cat("  uniprot_id:", uniprot_id, "\n")
  
  # Build prompt
  prompt <- paste0(
    "You are a scientific literature expert specializing in recombinant protein expression.\n\n",
    
    "TASK: Search your knowledge for successful recombinant expression protocols for this protein:\n\n",
    
    "Protein: ", protein_name, "\n",
    "Organism: ", organism, "\n",
    "UniProt ID: ", uniprot_id, "\n\n",
    
    "Find published expression data including:\n",
    "1. Expression host/system (E.coli, yeast, insect, mammalian)\n",
    "2. Vector/plasmid used\n",
    "3. Affinity tags (6xHis, GST, MBP, etc.)\n",
    "4. Tag position (N-terminal, C-terminal)\n",
    "5. Yield information if available\n",
    "6. Citation (PMID or DOI)\n\n",
    
    "Return results as a JSON array with this EXACT structure:\n",
    "[\n",
    "  {\n",
    '    "uniprot_id": "', uniprot_id, '",\n',
    '    "Expression_Host": "E.coli, yeast, insect, mammalian, etc.",\n',
    '    "Vector_Plasmid": "vector name",\n',
    '    "Affinity_Tag": "6xHis, GST, etc.",\n',
    '    "Tag_Position": "N-terminal or C-terminal",\n',
    '    "Yield": "yield information if available",\n',
    '    "Citation": "PMID or DOI",\n',
    '    "Evidence_Score": 1-5\n',
    "  }\n",
    "]\n\n",
    
    "Evidence Score:\n",
    "5 = Detailed protocol with yield data\n",
    "4 = Clear protocol, successful expression confirmed\n",
    "3 = Expression reported but limited details\n",
    "2 = Mentioned but minimal information\n",
    "1 = Inferred or computational prediction\n\n",
    
    "Return ONLY the JSON array, no other text."
  )
  
  # Ensure single string
  prompt <- paste(prompt, collapse = "")
  
  cat("Final prompt length:", nchar(prompt), "characters\n")
  
  return(prompt)
}

# =============================================================================
# JSON PARSER
# =============================================================================

parse_json_safely <- function(json_text, expected_type = NULL) {
  
  # Remove markdown code blocks
  json_text <- gsub("```json\\s*", "", json_text)
  json_text <- gsub("```\\s*$", "", json_text)
  json_text <- trimws(json_text)
  
  tryCatch({
    parsed <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)
    
    # Return as data frame
    if (is.data.frame(parsed)) {
      return(parsed)
    } else if (is.list(parsed) && length(parsed) > 0) {
      return(as.data.frame(do.call(rbind, lapply(parsed, as.data.frame)), stringsAsFactors = FALSE))
    } else {
      # Empty result
      if (!is.null(expected_type)) {
        if (expected_type == "epitopes") {
          return(data.frame(
            uniprot_id = character(),
            Epitope_Sequence = character(),
            Position = character(),
            Epitope_Type = character(),
            Evidence_Level = numeric(),
            Citation = character(),
            Antibody_Context = character(),
            Source = character(),
            stringsAsFactors = FALSE
          ))
        } else if (expected_type == "expression") {
          return(data.frame(
            uniprot_id = character(),
            Expression_Host = character(),
            Vector_Plasmid = character(),
            Affinity_Tag = character(),
            Tag_Position = character(),
            Yield = character(),
            Citation = character(),
            Evidence_Score = numeric(),
            stringsAsFactors = FALSE
          ))
        }
      }
      return(data.frame())
    }
  }, error = function(e) {
    cat("JSON parse error:", conditionMessage(e), "\n")
    return(data.frame(Error = paste("Parse failed:", substr(json_text, 1, 100))))
  })
}

# =============================================================================
# COST CALCULATION
# =============================================================================

calculate_cost <- function(model, prompt_tokens, completion_tokens) {
  # OpenAI pricing (update as needed)
  pricing <- list(
    # GPT-5 models (your pricing)
    "gpt-5" = list(input = 10.00 / 1e6, output = 30.00 / 1e6),
    "gpt-5-mini" = list(input = 1.00 / 1e6, output = 4.00 / 1e6),
    
    # GPT-4o models
    "gpt-4o" = list(input = 2.50 / 1e6, output = 10.00 / 1e6),
    "gpt-4o-mini" = list(input = 0.150 / 1e6, output = 0.600 / 1e6),
    
    # GPT-4
    "gpt-4-turbo" = list(input = 10.00 / 1e6, output = 30.00 / 1e6),
    "gpt-4" = list(input = 30.00 / 1e6, output = 60.00 / 1e6),
    
    # GPT-3.5
    "gpt-3.5-turbo" = list(input = 0.50 / 1e6, output = 1.50 / 1e6)
  )
  
  if (!model %in% names(pricing)) {
    warning("Unknown model: ", model)
    return(0)
  }
  
  input_cost <- prompt_tokens * pricing[[model]]$input
  output_cost <- completion_tokens * pricing[[model]]$output
  
  return(input_cost + output_cost)
}

# Add this function to the end of openai_functions.R

# Helper function to return empty data frames with correct structure
get_empty_structure <- function(expected_type) {
  if (is.null(expected_type)) {
    return(data.frame())
  }
  
  if (expected_type == "epitopes") {
    return(data.frame(
      uniprot_id = character(),
      Epitope_Sequence = character(),
      Position = character(),
      Epitope_Type = character(),
      Evidence_Level = numeric(),
      Citation = character(),
      Antibody_Context = character(),
      Source = character(),
      stringsAsFactors = FALSE
    ))
  } else if (expected_type == "expression") {
    return(data.frame(
      uniprot_id = character(),
      Expression_Host = character(),
      Vector_Plasmid = character(),
      Affinity_Tag = character(),
      Tag_Position = character(),
      Yield = character(),
      Citation = character(),
      Evidence_Score = numeric(),
      stringsAsFactors = FALSE
    ))
  } else {
    return(data.frame())
  }
}