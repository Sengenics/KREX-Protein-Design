# Enhanced function to determine multimeric status from subunit text
# This version distinguishes between intrinsic subunit structure and external interactions
determine_multimeric_status_refined <- function(subunit_text) {
  if (is.na(subunit_text) || subunit_text == "Not available" || 
      subunit_text == "No subunit information available") {
    return("Unknown")
  }
  
  # Convert to lowercase for easier matching
  text_lower <- tolower(subunit_text)
  
  # Clear indicators of monomeric proteins
  monomer_patterns <- c(
    "monomer", "monomeric", "single subunit", "single polypeptide",
    "exists as a monomer", "functions as a monomer"
  )
  
  # Clear indicators of multimeric proteins (intrinsic quaternary structure)
  intrinsic_multimer_patterns <- c(
    "homodimer", "heterodimer", "forms.*dimer",
    "homotrimer", "heterotrimer", "forms.*trimer",
    "homotetramer", "heterotetramer", "forms.*tetramer", 
    "tetrameric", "trimeric", "dimeric",
    "composed of.*subunit", "consists of.*subunit",
    "quaternary structure", "\\d+\\s*subunit",
    "alpha.*beta.*subunit", "heavy.*light.*chain"
  )
  
  # Patterns that indicate external interactions (not intrinsic multimers)
  interaction_only_patterns <- c(
    "interacts with", "binds to", "forms.*complex with",
    "associates with.*upon", "interaction.*mediate",
    "through.*interact", "required for.*interaction",
    "binding.*partner", "ligand.*interaction"
  )
  
  # Check for definite monomers first
  for (pattern in monomer_patterns) {
    if (grepl(pattern, text_lower)) {
      return("Monomer")
    }
  }
  
  # Check for intrinsic multimers
  for (pattern in intrinsic_multimer_patterns) {
    if (grepl(pattern, text_lower)) {
      return("Multimer")
    }
  }
  
  # If the text only contains interaction descriptions, it's likely monomeric
  # but forms complexes with other proteins
  interaction_count <- sum(sapply(interaction_only_patterns, function(p) grepl(p, text_lower)))
  total_words <- length(strsplit(text_lower, "\\s+")[[1]])
  
  if (interaction_count > 0 && total_words > 50) {
    # Lots of interaction text but no clear subunit structure = likely monomer
    return("Likely monomer (forms external complexes)")
  }
  
  # Look for numeric indicators
  if (grepl("\\b(two|three|four|five|six|2|3|4|5|6)\\s+subunit", text_lower)) {
    return("Multimer")
  }
  
  return("Unknown")
}

# Function to analyze and categorize subunit information
analyze_subunit_information <- function(uniprot_id, subunit_text) {
  multimer_status <- determine_multimeric_status_refined(subunit_text)
  
  # Count interaction mentions
  interaction_indicators <- c("interacts with", "binds to", "forms complex", "associates with")
  interaction_count <- sum(sapply(interaction_indicators, function(x) 
    length(gregexpr(x, tolower(subunit_text), fixed = TRUE)[[1]]) - 
      (gregexpr(x, tolower(subunit_text), fixed = TRUE)[[1]][1] == -1)))
  
  # Determine if this is primarily interaction data vs structure data
  text_type <- if (interaction_count >= 3) {
    "Primarily interaction data"
  } else if (interaction_count >= 1) {
    "Mixed interaction and structure data"  
  } else {
    "Primarily structure data"
  }
  
  return(list(
    uniprot_id = uniprot_id,
    multimer_status = multimer_status,
    text_type = text_type,
    interaction_count = interaction_count,
    subunit_text_preview = substr(subunit_text, 1, 200),
    confidence = case_when(
      multimer_status %in% c("Monomer", "Multimer") ~ "High",
      grepl("Likely", multimer_status) ~ "Medium",
      TRUE ~ "Low"
    )
  ))
}

# Comprehensive protein analysis function
get_comprehensive_protein_analysis <- function(uniprot_id) {
  tryCatch({
    # Get UniProt data
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    response <- url(url)
    on.exit(close(response))
    json_text <- readLines(response, warn = FALSE)
    data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
    
    # Extract various information
    subunit_text <- extract_subunit_structure_corrected(data)
    subunit_analysis <- analyze_subunit_information(uniprot_id, subunit_text)
    
    # Also get protein name for context
    protein_name <- if (!is.null(data$proteinDescription) && 
                        !is.null(data$proteinDescription$recommendedName) &&
                        !is.null(data$proteinDescription$recommendedName$fullName) &&
                        !is.null(data$proteinDescription$recommendedName$fullName$value)) {
      data$proteinDescription$recommendedName$fullName$value
    } else "Not available"
    
    # Combine all information
    result <- list(
      uniprot_id = uniprot_id,
      protein_name = protein_name,
      multimer_status = subunit_analysis$multimer_status,
      text_type = subunit_analysis$text_type,
      confidence = subunit_analysis$confidence,
      subunit_text = subunit_text
    )
    
    return(result)
    
  }, error = function(e) {
    return(list(
      uniprot_id = uniprot_id,
      error = paste("Error in analysis:", e$message)
    ))
  })
}

# Test with the current protein and some others
test_multimer_analysis <- function() {
  # Test cases with known structures
  test_cases <- c(
    "P01308", # Insulin - known multimer (hexamers, dimers)
    "P04637", # p53 - known tetramer  
    "P69905", # Hemoglobin alpha - part of tetramer
    "P02768", # Albumin - monomeric
    "P0DTC2"  # SARS-CoV-2 spike - trimeric
  )
  
  results <- list()
  for (i in seq_along(test_cases)) {
    cat("Analyzing", test_cases[i], "...\n")
    result <- get_comprehensive_protein_analysis(test_cases[i])
    results[[i]] <- result
    Sys.sleep(0.5)
  }
  
  return(results)
}

# Print results function
print_multimer_results <- function(results) {
  for (result in results) {
    cat("=== ", result$uniprot_id, " ===\n")
    if ("error" %in% names(result)) {
      cat("Error:", result$error, "\n")
    } else {
      cat("Protein:", result$protein_name, "\n")
      cat("Multimer Status:", result$multimer_status, "\n") 
      cat("Confidence:", result$confidence, "\n")
      cat("Text Type:", result$text_type, "\n")
      cat("Preview:", substr(result$subunit_text, 1, 150), "...\n")
    }
    cat("\n")
  }
}

# For your current protein, let's analyze it:
analyze_current_protein <- function(data) {
  subunit_text <- extract_subunit_structure_corrected(data)
  analysis <- analyze_subunit_information("current_protein", subunit_text)
  
  cat("=== ANALYSIS OF CURRENT PROTEIN ===\n")
  cat("Multimer Status:", analysis$multimer_status, "\n")
  cat("Text Type:", analysis$text_type, "\n") 
  cat("Confidence:", analysis$confidence, "\n")
  cat("Interaction Count:", analysis$interaction_count, "\n")
  cat("Text Preview:", analysis$subunit_text_preview, "\n")
  
  return(analysis)
}

# Run this while in browser mode:
# analyze_current_protein(data)