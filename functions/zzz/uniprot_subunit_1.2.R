# Function to specifically detect homo-multimeric proteins
determine_homo_multimeric_status <- function(subunit_text) {
  if (is.na(subunit_text) || subunit_text == "Not available" || 
      subunit_text == "No subunit information available") {
    return("Unknown")
  }
  
  # Convert to lowercase for easier matching
  text_lower <- tolower(subunit_text)
  
  # Clear indicators of HOMO-multimeric proteins (same protein with itself)
  homo_multimer_patterns <- c(
    "homodimer", "homo-dimer", "homodimeric",
    "homotrimer", "homo-trimer", "homotrimeric", 
    "homotetramer", "homo-tetramer", "homotetrameric",
    "homopentamer", "homo-pentamer",
    "homohexamer", "homo-hexamer", "homohexameric",
    "homo-oligomer", "homooligomer", "homooligomeric",
    "identical subunit", "identical polypeptide",
    "same subunit", "copies of.*subunit",
    "self-associate", "self-association", "self associate",
    "forms dimer with itself", "dimerizes with itself",
    "composed of.*identical", "consists of.*identical"
  )
  
  # Patterns indicating the protein exists as multiple copies
  homo_copy_patterns <- c(
    "two identical", "three identical", "four identical",
    "\\d+\\s*identical.*subunit", "\\d+\\s*copies",
    "symmetrical.*subunit", "symmetric.*subunit"
  )
  
  # Clear indicators of monomeric proteins
  monomer_patterns <- c(
    "monomer", "monomeric", "single subunit", "single polypeptide",
    "exists as a monomer", "functions as a monomer", "single chain"
  )
  
  # Patterns that indicate HETERO-interactions (with different proteins)
  # These should NOT count as homo-multimers
  hetero_interaction_patterns <- c(
    "interacts with [A-Z][A-Z0-9]+", # Gene names like "interacts with THBS1"
    "binds to [A-Z][A-Z0-9]+",
    "forms.*complex with [A-Z]", # "forms complex with TLR4"
    "associates with [A-Z][A-Z0-9]+",
    "heterodimer", "hetero-dimer", "heterodimeric",
    "heterotrimer", "hetero-trimer", "heterotrimeric",
    "heterotetramer", "hetero-tetramer",
    "different subunit", "distinct subunit",
    "alpha.*beta", "heavy.*light" # Different chain types
  )
  
  # Check for definite monomers first
  for (pattern in monomer_patterns) {
    if (grepl(pattern, text_lower)) {
      return("Monomer")
    }
  }
  
  # Check for homo-multimers (highest priority)
  for (pattern in homo_multimer_patterns) {
    if (grepl(pattern, text_lower)) {
      return("Homo-multimer")
    }
  }
  
  # Check for homo-multimer copy patterns
  for (pattern in homo_copy_patterns) {
    if (grepl(pattern, text_lower)) {
      return("Homo-multimer")
    }
  }
  
  # If we find hetero-interaction patterns, it's likely a monomer
  # that forms external complexes
  hetero_matches <- sum(sapply(hetero_interaction_patterns, function(p) grepl(p, text_lower)))
  if (hetero_matches > 0) {
    return("Monomer (forms hetero-complexes)")
  }
  
  # Look for general multimer terms but be cautious
  general_multimer_patterns <- c("dimer", "trimer", "tetramer", "oligomer")
  for (pattern in general_multimer_patterns) {
    if (grepl(paste0("\\b", pattern, "\\b"), text_lower)) {
      # If it just says "dimer" without specifying homo/hetero, 
      # we need more context to decide
      return("Multimer (homo/hetero unclear)")
    }
  }
  
  # If text is primarily about interactions with named proteins, likely monomer
  # Count mentions of specific protein names (uppercase patterns)
  protein_mentions <- length(gregexpr("[A-Z][A-Z0-9]{2,}", subunit_text)[[1]]) - 
    (gregexpr("[A-Z][A-Z0-9]{2,}", subunit_text)[[1]][1] == -1)
  
  if (protein_mentions > 3) {
    return("Monomer (many hetero-interactions)")
  }
  
  return("Unknown")
}

# Enhanced analysis function that focuses on homo-multimerization
analyze_homo_multimer_status <- function(uniprot_id, subunit_text = NULL) {
  
  # If no text provided, extract it
  if (is.null(subunit_text)) {
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    response <- url(url)
    on.exit(close(response))
    json_text <- readLines(response, warn = FALSE)
    data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
    subunit_text <- extract_subunit_structure_corrected(data)
  }
  
  # Determine homo-multimer status
  homo_status <- determine_homo_multimeric_status(subunit_text)
  
  # Count specific indicators
  homo_indicators <- c("homo", "identical subunit", "self-associate")
  hetero_indicators <- c("interacts with", "binds to", "associates with")
  
  homo_count <- sum(sapply(homo_indicators, function(x) grepl(x, tolower(subunit_text))))
  hetero_count <- sum(sapply(hetero_indicators, function(x) grepl(x, tolower(subunit_text))))
  
  # Determine confidence
  confidence <- case_when(
    homo_status == "Homo-multimer" && homo_count > 0 ~ "High",
    homo_status == "Monomer" && grepl("monomer", tolower(subunit_text)) ~ "High", 
    grepl("unclear", homo_status) ~ "Low",
    hetero_count > homo_count ~ "Medium",
    TRUE ~ "Medium"
  )
  
  return(list(
    uniprot_id = uniprot_id,
    homo_multimer_status = homo_status,
    confidence = confidence,
    homo_indicators = homo_count,
    hetero_indicators = hetero_count,
    subunit_text_preview = substr(subunit_text, 1, 200),
    full_subunit_text = subunit_text
  ))
}

# Test with known homo-multimers vs hetero-interactors
test_homo_vs_hetero <- function() {
  test_cases <- list(
    # Known homo-multimers
    "P01308" = "Insulin - forms homodimers and homohexamers",
    "P04637" = "p53 - forms homotetramers", 
    "P69905" = "Hemoglobin alpha - but part of α2β2 heterotetramer",
    # Known monomers with interactions
    "P02768" = "Albumin - monomeric but binds many ligands",
    "P01133" = "EGF - monomeric growth factor"
  )
  
  results <- list()
  for (i in seq_along(test_cases)) {
    uniprot_id <- names(test_cases)[i]
    description <- test_cases[[i]]
    
    cat("Testing", uniprot_id, "-", description, "\n")
    result <- analyze_homo_multimer_status(uniprot_id)
    results[[uniprot_id]] <- result
    Sys.sleep(0.5)
  }
  
  return(results)
}

# Print results focusing on homo-multimerization
print_homo_multimer_results <- function(results) {
  for (result in results) {
    cat("=== ", result$uniprot_id, " ===\n")
    cat("Homo-multimer Status:", result$homo_multimer_status, "\n")
    cat("Confidence:", result$confidence, "\n")
    cat("Homo indicators:", result$homo_indicators, "\n")
    cat("Hetero indicators:", result$hetero_indicators, "\n")
    cat("Text preview:", result$subunit_text_preview, "...\n")
    cat("\n")
  }
}

# Quick function to analyze your current protein for homo-multimerization
analyze_current_for_homo_multimer <- function(data) {
  subunit_text <- extract_subunit_structure_corrected(data)
  homo_status <- determine_homo_multimeric_status(subunit_text)
  
  cat("=== HOMO-MULTIMER ANALYSIS ===\n")
  cat("Status:", homo_status, "\n")
  cat("Reasoning: Looking for 'homo-', 'identical subunits', 'self-associate'\n")
  cat("Text preview:", substr(subunit_text, 1, 300), "...\n")
  
  return(homo_status)
}

# Usage while in browser mode:
# analyze_current_for_homo_multimer(data)