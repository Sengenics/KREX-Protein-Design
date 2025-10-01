# Simple Quaternary Structure Analysis Functions
# Works with the UniProt data object you already have
# Just requires: stringr

library(stringr)

# ============================================================================
# DEBUG FUNCTION - Run this in browser() to see the structure
# ============================================================================

debug_comments_structure <- function(data) {
  cat("=== DEBUGGING COMMENTS STRUCTURE ===\n\n")
  
  cat("1. Class of data$comments:\n")
  print(class(data$comments))
  
  cat("\n2. Length:\n")
  print(length(data$comments))
  
  cat("\n3. Names (if any):\n")
  print(names(data$comments))
  
  cat("\n4. Structure (first 2 levels):\n")
  print(str(data$comments, max.level = 2))
  
  cat("\n5. First element:\n")
  print(data$comments[[1]])
  
  cat("\n6. Class of first element:\n")
  print(class(data$comments[[1]]))
  
  cat("\n7. Looking for SUBUNIT:\n")
  if ("SUBUNIT" %in% data$comments) {
    cat("   Found 'SUBUNIT' in comments\n")
    idx <- which(data$comments == "SUBUNIT")
    cat("   At index:", idx, "\n")
  } else {
    cat("   'SUBUNIT' not found as a direct value\n")
  }
  
  cat("\n8. Trying to access commentType field:\n")
  for (i in 1:min(3, length(data$comments))) {
    cat("   Element", i, ":\n")
    elem <- data$comments[[i]]
    cat("     Class:", class(elem), "\n")
    if (is.list(elem)) {
      cat("     Has commentType:", !is.null(elem$commentType), "\n")
      if (!is.null(elem$commentType)) {
        cat("     commentType value:", elem$commentType, "\n")
      }
    }
  }
}

# ============================================================================
# STEP 1: Extract cc_subunit text from your existing UniProt data object
# ============================================================================

extract_subunit_text <- function(data) {
  # data is the UniProt JSON object you already have
  
  if (is.null(data$comments) || nrow(data$comments) == 0) {
    return("Not available")
  }
  
  # data$comments is a dataframe
  # Find the row where commentType == "SUBUNIT"
  subunit_idx <- which(data$comments$commentType == "SUBUNIT")
  
  if (length(subunit_idx) == 0) {
    return("Not available")
  }
  
  # Get the first SUBUNIT comment
  subunit_row <- data$comments[subunit_idx[1], ]
  
  # The text is in the texts column, which is a list
  if (!is.null(subunit_row$texts) && length(subunit_row$texts) > 0) {
    texts_data <- subunit_row$texts[[1]]
    
    # texts_data is a dataframe with a 'value' column
    if (is.data.frame(texts_data) && "value" %in% names(texts_data)) {
      if (nrow(texts_data) > 0) {
        return(texts_data$value[1])
      }
    }
  }
  
  return("Not available")
}

# ============================================================================
# STEP 2: Analyze the subunit text for quaternary structure
# ============================================================================

analyze_quaternary_structure <- function(subunit_text) {
  if (is.na(subunit_text) || subunit_text == "Not available" || subunit_text == "") {
    return(list(
      has_quaternary = "Unknown",
      oligomer_type = "Unknown",
      oligomer_state = "Unknown",
      stoichiometry = "Unknown",
      confidence = "No data"
    ))
  }
  
  # Convert to lowercase for easier pattern matching
  text_lower <- tolower(subunit_text)
  
  # Initialize result
  result <- list(
    has_quaternary = "No",
    oligomer_type = NA_character_,
    oligomer_state = "Monomer",
    stoichiometry = NA_character_,
    confidence = "Low"
  )
  
  # Keywords indicating quaternary structure
  multimer_keywords <- c("dimer", "trimer", "tetramer", "pentamer", "hexamer", 
                         "heptamer", "octamer", "nonamer", "decamer", "dodecamer",
                         "oligomer", "multimer", "complex", "subunit")
  
  # Check if protein has quaternary structure
  if (any(sapply(multimer_keywords, function(kw) grepl(kw, text_lower)))) {
    result$has_quaternary <- "Yes"
    result$confidence <- "High"
    
    # Determine homo- vs hetero-oligomer
    if (grepl("homo", text_lower)) {
      result$oligomer_type <- "Homomer"
    } else if (grepl("hetero", text_lower)) {
      result$oligomer_type <- "Heteromer"
    } else {
      # Try to infer from description
      result$oligomer_type <- infer_oligomer_type(subunit_text)
    }
    
    # Extract oligomeric state
    result$oligomer_state <- extract_oligomer_state(text_lower)
    
    # Extract stoichiometry if present
    result$stoichiometry <- extract_stoichiometry(subunit_text)
    
  } else if (grepl("monomer", text_lower)) {
    result$has_quaternary <- "No"
    result$oligomer_type <- NA_character_
    result$oligomer_state <- "Monomer"
    result$confidence <- "High"
  }
  
  return(result)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

infer_oligomer_type <- function(subunit_text) {
  text_lower <- tolower(subunit_text)
  
  # Strong indicators of homomers
  homomer_patterns <- c(
    "forms? (a |an )?dimer",
    "self-associates?",
    "identical subunits?",
    "composed of.*same",
    "^dimer$",
    "^trimer$",
    "^tetramer$"
  )
  
  # Strong indicators of heteromers
  heteromer_patterns <- c(
    "forms? (a |an )?complex with",
    "interacts? with",
    "associates? with",
    "composed of.*different",
    "alpha.*beta",
    "subunit.*and.*subunit",
    "chain.*and.*chain",
    "two.*and.*two"
  )
  
  homo_match <- any(sapply(homomer_patterns, function(p) grepl(p, text_lower)))
  hetero_match <- any(sapply(heteromer_patterns, function(p) grepl(p, text_lower)))
  
  if (homo_match && !hetero_match) {
    return("Homomer (inferred)")
  } else if (hetero_match && !homo_match) {
    return("Heteromer (inferred)")
  } else if (homo_match && hetero_match) {
    return("Both/Complex")
  } else {
    return("Unclear")
  }
}

extract_oligomer_state <- function(text_lower) {
  states <- c("monomer", "dimer", "trimer", "tetramer", "pentamer", 
              "hexamer", "heptamer", "octamer", "nonamer", "decamer", 
              "dodecamer", "oligomer")
  
  for (state in states) {
    if (grepl(state, text_lower)) {
      # Check for homo/hetero prefix
      if (grepl(paste0("homo", state), text_lower)) {
        return(paste0("Homo-", state))
      } else if (grepl(paste0("hetero", state), text_lower)) {
        return(paste0("Hetero-", state))
      } else {
        return(tools::toTitleCase(state))
      }
    }
  }
  
  return("Complex")
}

extract_stoichiometry <- function(subunit_text) {
  # Pattern 1: Alpha2Beta2 or A2B2 style
  stoich_pattern1 <- "([A-Za-z]+)(\\d+)([A-Za-z]+)(\\d+)"
  match1 <- str_match(subunit_text, stoich_pattern1)
  if (!is.na(match1[1])) {
    return(match1[1])
  }
  
  # Pattern 2: "two alpha and two beta" style
  stoich_pattern2 <- "(\\d+|two|three|four|five|six)\\s+([A-Za-z-]+)\\s+(subunits?|chains?)\\s+and\\s+(\\d+|two|three|four|five|six)\\s+([A-Za-z-]+)"
  match2 <- str_match(tolower(subunit_text), stoich_pattern2)
  if (!is.na(match2[1])) {
    num1 <- convert_word_to_number(match2[2])
    name1 <- match2[3]
    num2 <- convert_word_to_number(match2[5])
    name2 <- match2[6]
    return(paste0(name1, num1, name2, num2))
  }
  
  # Pattern 3: "composed of X subunits"
  stoich_pattern3 <- "(\\d+)\\s+subunits?"
  match3 <- str_match(tolower(subunit_text), stoich_pattern3)
  if (!is.na(match3[1])) {
    return(paste0(match3[2], "-mer"))
  }
  
  # Pattern 4: "dimer of dimers" = 4-mer
  if (grepl("dimer of dimers", tolower(subunit_text))) {
    return("4-mer")
  }
  if (grepl("trimer of dimers", tolower(subunit_text))) {
    return("6-mer")
  }
  
  return(NA_character_)
}

convert_word_to_number <- function(word) {
  word <- tolower(word)
  numbers <- c(
    "one" = "1", "two" = "2", "three" = "3", "four" = "4",
    "five" = "5", "six" = "6", "seven" = "7", "eight" = "8",
    "nine" = "9", "ten" = "10"
  )
  
  if (word %in% names(numbers)) {
    return(numbers[word])
  } else {
    return(word)
  }
}

# ============================================================================
# CONVENIENCE FUNCTION: Works with dataframes
# ============================================================================

add_quaternary_structure_info <- function(df, subunit_column = "cc_subunit") {
  if (!subunit_column %in% names(df)) {
    warning(paste("Column", subunit_column, "not found in dataframe"))
    return(df)
  }
  
  # Apply analysis to each row
  quat_info <- lapply(df[[subunit_column]], analyze_quaternary_structure)
  
  # Extract components into separate columns
  df$has_quaternary_structure <- sapply(quat_info, function(x) x$has_quaternary)
  df$oligomer_type <- sapply(quat_info, function(x) x$oligomer_type)
  df$oligomer_state <- sapply(quat_info, function(x) x$oligomer_state)
  df$stoichiometry <- sapply(quat_info, function(x) x$stoichiometry)
  df$quaternary_confidence <- sapply(quat_info, function(x) x$confidence)
  
  return(df)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# Example 1: You already have the UniProt data object
# In browser() or your code where you have 'data':
#
# subunit_text <- extract_subunit_text(data)
# quat_info <- analyze_quaternary_structure(subunit_text)
# print(quat_info)

# Example 2: You have a dataframe with cc_subunit column already
# 
# my_df <- data.frame(
#   protein_name = "p53",
#   cc_subunit = "Homotetramer. Binds DNA as a homotetramer."
# )
# my_df <- add_quaternary_structure_info(my_df)
# print(my_df)

# Example 3: Testing with text directly
#
# test_text <- "Homotetramer. Binds DNA as a homotetramer."
# result <- analyze_quaternary_structure(test_text)
# print(result)