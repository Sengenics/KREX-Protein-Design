# Simple function that returns single-word multimer classification
classify_multimer_simple <- function(subunit_text) {
  if (is.na(subunit_text) || subunit_text == "Not available" || 
      subunit_text == "No subunit information available") {
    return("Unknown")
  }
  
  text_lower <- tolower(subunit_text)
  
  # HOMO-MULTIMERS (highest priority)
  if (grepl("homodimer|homo-dimer", text_lower)) return("Homodimer")
  if (grepl("homotrimer|homo-trimer", text_lower)) return("Homotrimer") 
  if (grepl("homotetramer|homo-tetramer", text_lower)) return("Homotetramer")
  if (grepl("homopentamer|homo-pentamer", text_lower)) return("Homopentamer")
  if (grepl("homohexamer|homo-hexamer", text_lower)) return("Homohexamer")
  if (grepl("homo.*oligomer|homooligomer", text_lower)) return("Homo-oligomer")
  
  # HETERO-MULTIMERS
  if (grepl("heterodimer|hetero-dimer", text_lower)) return("Heterodimer")
  if (grepl("heterotrimer|hetero-trimer", text_lower)) return("Heterotrimer")
  if (grepl("heterotetramer|hetero-tetramer", text_lower)) return("Heterotetramer") 
  if (grepl("hetero.*oligomer|heterooligomer", text_lower)) return("Hetero-oligomer")
  
  # SPECIFIC HETERO PATTERNS (like α2β2)
  if (grepl("alpha.*beta.*tetramer|α.*β.*tetramer", text_lower)) return("Heterotetramer")
  if (grepl("heavy.*light.*chain", text_lower)) return("Heterodimer")
  
  # GENERAL PATTERNS (when homo/hetero not specified)
  if (grepl("\\btwo identical|2 identical", text_lower)) return("Homodimer")
  if (grepl("\\bthree identical|3 identical", text_lower)) return("Homotrimer")
  if (grepl("\\bfour identical|4 identical", text_lower)) return("Homotetramer")
  
  # SELF-ASSOCIATION patterns
  if (grepl("self.?associat|self.?dimeriz", text_lower)) return("Homodimer")
  
  # MONOMER indicators
  if (grepl("\\bmonomer|monomeric|single subunit|single chain", text_lower)) return("Monomer")
  
  # If lots of hetero-interactions mentioned, likely monomer
  hetero_mentions <- length(gregexpr("interacts with [A-Z]|binds to [A-Z]|associates with [A-Z]", subunit_text)[[1]]) - 
    (gregexpr("interacts with [A-Z]|binds to [A-Z]|associates with [A-Z]", subunit_text)[[1]][1] == -1)
  
  if (hetero_mentions >= 3) return("Monomer")
  
  # Last resort - general terms without homo/hetero specification
  if (grepl("\\bdimer\\b", text_lower)) return("Unknown") # Could be homo or hetero
  if (grepl("\\btrimer\\b", text_lower)) return("Unknown")
  if (grepl("\\btetramer\\b", text_lower)) return("Unknown")
  
  return("Unknown")
}

# Quick function for current data
get_simple_multimer_status <- function(data) {
  subunit_text <- extract_subunit_structure_corrected(data)
  status <- classify_multimer_simple(subunit_text)
  cat("Multimer Status:", status, "\n")
  return(status)
}

# Test function with known examples
test_simple_classification <- function() {
  # Test with some known cases
  test_texts <- list(
    "Forms homodimers in solution" = "Homodimer",
    "The protein exists as a monomer" = "Monomer", 
    "Composed of alpha and beta subunits forming a heterodimer" = "Heterodimer",
    "Functions as a homotetramer with four identical subunits" = "Homotetramer",
    "Interacts with THBS1 and TLR4 to form complexes" = "Monomer"
  )
  
  cat("=== TESTING SIMPLE CLASSIFICATION ===\n")
  for (text in names(test_texts)) {
    result <- classify_multimer_simple(text)
    expected <- test_texts[[text]]
    cat("Text:", substr(text, 1, 50), "...\n")
    cat("Result:", result, "| Expected:", expected, 
        if(result == expected) "✓" else "✗", "\n\n")
  }
}

# Usage:
# get_simple_multimer_status(data)  # For your current protein
# test_simple_classification()      # To test the logic