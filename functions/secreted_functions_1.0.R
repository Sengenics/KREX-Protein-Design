# Simple function to determine if a protein is secreted based on subcellular location
is_protein_secreted <- function(subcellular_location) {
  if (is.na(subcellular_location) || 
      subcellular_location == "Not available" || 
      subcellular_location == "No subcellular location information" ||
      subcellular_location == "Subcellular location comment found but data not accessible") {
    return("Unknown")
  }
  
  # Convert to lowercase for easier matching
  location_lower <- tolower(subcellular_location)
  
  # Definite secretion indicators
  secreted_patterns <- c(
    "secreted",
    "extracellular",
    "extracellular space", 
    "extracellular matrix",
    "cell surface",
    "membrane.*extracellular", # membrane proteins with extracellular domains
    "released",
    "shed"
  )
  
  # Definite non-secreted (intracellular) indicators
  intracellular_patterns <- c(
    "cytoplasm",
    "cytoplasmic", 
    "nucleus",
    "nuclear",
    "mitochondrion",
    "mitochondrial",
    "endoplasmic reticulum",
    "golgi",
    "peroxisome",
    "lysosome",
    "ribosome",
    "cytosol"
  )
  
  # Check for definite secretion
  for (pattern in secreted_patterns) {
    if (grepl(pattern, location_lower)) {
      return("Yes")
    }
  }
  
  # Check for definite intracellular location
  for (pattern in intracellular_patterns) {
    if (grepl(pattern, location_lower)) {
      return("No")
    }
  }
  
  # Special cases for membrane proteins
  if (grepl("membrane", location_lower)) {
    # Some membrane proteins are secreted (shed from membrane)
    if (grepl("single.?pass|multi.?pass", location_lower)) {
      return("Possibly") # Could be cleaved/shed
    } else {
      return("Possibly")
    }
  }
  
  return("Unknown")
}

# Alternative version that takes the data object directly
determine_secretion_from_data <- function(data) {
  subcellular_location <- extract_subcellular_location_df(data)
  return(is_protein_secreted(subcellular_location))
}

# Test the function with some example locations
test_secretion_classification <- function() {
  test_cases <- c(
    "Secreted" = "Yes",
    "Extracellular space" = "Yes", 
    "Cell surface" = "Yes",
    "Cytoplasm" = "No",
    "Nucleus" = "No",
    "Membrane; Single-pass" = "Possibly",
    "Not available" = "Unknown"
  )
  
  cat("=== TESTING SECRETION CLASSIFICATION ===\n")
  for (location in names(test_cases)) {
    result <- is_protein_secreted(location)
    expected <- test_cases[location]
    cat("Location:", location, "| Result:", result, "| Expected:", expected, 
        if(result == expected) "✓" else "✗", "\n")
  }
}