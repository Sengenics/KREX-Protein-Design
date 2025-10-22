# functions/structure_functions.R
# Terminus disorder prediction for tag placement optimization

# Predict intrinsic disorder at protein termini
predict_terminus_disorder <- function(sequence, 
                                      signal_peptide_end = NULL,
                                      n_term_length = 30, 
                                      c_term_length = 30) {
  
  cat("\n=== Predicting Terminus Disorder ===\n")
  cat("Full sequence length:", nchar(sequence), "aa\n")
  
  # CRITICAL: Remove signal peptide if present
  mature_sequence <- sequence
  mature_start_position <- 1
  
  if (!is.null(signal_peptide_end) && signal_peptide_end > 0) {
    cat("⚠️  Signal peptide detected: aa 1-", signal_peptide_end, "\n")
    cat("   Removing signal peptide for analysis...\n")
    
    # Extract mature protein (everything after signal)
    mature_sequence <- substr(sequence, signal_peptide_end + 1, nchar(sequence))
    mature_start_position <- signal_peptide_end + 1
    
    cat("   Mature protein length:", nchar(mature_sequence), "aa\n")
    cat("   New N-terminus starts at original position", mature_start_position, "\n\n")
  } else {
    cat("No signal peptide detected - analyzing full sequence\n\n")
  }
  
  # Now analyze termini of MATURE protein
  n_term_seq <- substr(mature_sequence, 1, min(n_term_length, nchar(mature_sequence)))
  c_term_seq <- substr(mature_sequence, 
                       max(1, nchar(mature_sequence) - c_term_length + 1), 
                       nchar(mature_sequence))
  
  cat("Analyzing MATURE protein termini:\n")
  cat("  N-term:", nchar(n_term_seq), "aa (", substr(n_term_seq, 1, 10), "...)\n")
  cat("  C-term:", nchar(c_term_seq), "aa (...", substr(c_term_seq, nchar(c_term_seq)-9, nchar(c_term_seq)), ")\n\n")
  
  # Calculate disorder scores
  n_term_disorder <- estimate_disorder_simple(n_term_seq)
  c_term_disorder <- estimate_disorder_simple(c_term_seq)
  
  cat("Disorder Scores:\n")
  cat("  N-terminus (MATURE):", round(n_term_disorder, 2), 
      " - ", interpret_disorder(n_term_disorder), "\n")
  cat("  C-terminus:", round(c_term_disorder, 2), 
      " - ", interpret_disorder(c_term_disorder), "\n\n")
  
  # Generate recommendation
  recommendation_result <- generate_tag_recommendation(
    n_term_disorder, 
    c_term_disorder,
    has_signal_peptide = !is.null(signal_peptide_end) && signal_peptide_end > 0
  )
  
  cat("RECOMMENDATION:", recommendation_result$recommendation, "\n")
  cat("Reasoning:", recommendation_result$reasoning, "\n")
  cat("Confidence:", recommendation_result$confidence, "\n\n")
  
  return(list(
    mature_sequence = mature_sequence,
    mature_start_position = mature_start_position,
    signal_removed = !is.null(signal_peptide_end) && signal_peptide_end > 0,
    signal_peptide_end = signal_peptide_end,
    n_terminus = list(
      sequence = n_term_seq,
      disorder_score = n_term_disorder,
      interpretation = interpret_disorder(n_term_disorder),
      length = nchar(n_term_seq)
    ),
    c_terminus = list(
      sequence = c_term_seq,
      disorder_score = c_term_disorder,
      interpretation = interpret_disorder(c_term_disorder),
      length = nchar(c_term_seq)
    ),
    recommendation = recommendation_result$recommendation,
    reasoning = recommendation_result$reasoning,
    confidence = recommendation_result$confidence
  ))
}


# Generate tag position recommendation based on disorder scores
generate_tag_recommendation <- function(n_disorder, c_disorder, has_signal_peptide = FALSE) {
  
  diff <- abs(c_disorder - n_disorder)
  
  # Case 1: C-terminus CLEARLY better (significantly more flexible)
  if (c_disorder > n_disorder + 0.12) {
    return(list(
      recommendation = "C-terminal",
      reasoning = paste0("C-terminus is significantly more flexible (", round(c_disorder, 2), 
                         ") than N-terminus (", round(n_disorder, 2), 
                         "). C-terminal tag will not interfere with protein folding.",
                         if(has_signal_peptide) " Note: Signal peptide removed, analyzing mature protein." else ""),
      confidence = if(diff > 0.20) "High" else "Medium",
      preferred_tag_position = "C-terminal"
    ))
  }
  
  # Case 2: N-terminus CLEARLY better (significantly more flexible)
  if (n_disorder > c_disorder + 0.12) {
    warning_note <- if(has_signal_peptide) {
      " IMPORTANT: Construct must start with Methionine for N-terminal tagging. Use pPRO30A-SP vector."
    } else {
      " Use pPRO30A vector for N-terminal tagging."
    }
    
    return(list(
      recommendation = "N-terminal",
      reasoning = paste0("N-terminus (mature protein) is significantly more flexible (", 
                         round(n_disorder, 2), ") than C-terminus (", round(c_disorder, 2), 
                         "). N-terminal tag safer for maintaining proper folding.",
                         warning_note),
      confidence = if(diff > 0.20) "High" else "Medium",
      preferred_tag_position = "N-terminal"
    ))
  }
  
  # Case 3: Close scores (diff ≤ 0.12) - Need to decide
  # Instead of defaulting to C, pick the BETTER one unless truly identical
  
  # Sub-case 3a: Truly identical (within 0.03)
  if (diff <= 0.03) {
    return(list(
      recommendation = "C-terminal (default)",
      reasoning = paste0("Both termini show essentially identical flexibility (N: ", round(n_disorder, 2), 
                         ", C: ", round(c_disorder, 2), 
                         "). C-terminal preferred for standard cloning workflow (pPRO8/pPRO9 vectors).",
                         if(has_signal_peptide) " Signal peptide will be removed before tagging." else ""),
      confidence = "Medium",
      preferred_tag_position = "C-terminal"
    ))
  }
  
  # Sub-case 3b: Similar but one is better (0.03 < diff ≤ 0.12)
  # PICK THE BETTER ONE!
  if (n_disorder > c_disorder) {
    # N-terminus is better
    warning_note <- if(has_signal_peptide) {
      " Use pPRO30A-SP vector with signal peptide for N-terminal tagging."
    } else {
      " Use pPRO30A vector for N-terminal tagging."
    }
    
    return(list(
      recommendation = "N-terminal",
      reasoning = paste0("N-terminus is moderately more flexible (", round(n_disorder, 2), 
                         ") than C-terminus (", round(c_disorder, 2), 
                         "). Both termini are acceptable, but N-terminal has an advantage.",
                         warning_note),
      confidence = "Medium",
      preferred_tag_position = "N-terminal"
    ))
    
  } else {
    # C-terminus is better
    return(list(
      recommendation = "C-terminal",
      reasoning = paste0("C-terminus is moderately more flexible (", round(c_disorder, 2), 
                         ") than N-terminus (", round(n_disorder, 2), 
                         "). Both termini are acceptable, but C-terminal has an advantage.",
                         if(has_signal_peptide) " Signal peptide will be removed before tagging." else ""),
      confidence = "Medium",
      preferred_tag_position = "C-terminal"
    ))
  }
}


# Simple disorder estimator based on amino acid composition
estimate_disorder_simple <- function(sequence) {
  
  if (is.null(sequence) || nchar(sequence) == 0) {
    return(0.5)  # Neutral if no sequence
  }
  
  aa_counts <- table(strsplit(sequence, "")[[1]])
  
  # Disorder-promoting amino acids (charged, small, flexible)
  disorder_aa <- c("P", "E", "K", "S", "Q", "A", "G", "D", "R")
  
  # Order-promoting amino acids (hydrophobic, aromatic, large)
  order_aa <- c("W", "F", "Y", "I", "L", "V", "N", "C", "M")
  
  disorder_count <- sum(aa_counts[names(aa_counts) %in% disorder_aa], na.rm = TRUE)
  order_count <- sum(aa_counts[names(aa_counts) %in% order_aa], na.rm = TRUE)
  
  total <- nchar(sequence)
  
  if (total == 0) return(0.5)
  
  # Calculate raw score (-1 to +1)
  disorder_score <- (disorder_count - order_count) / total
  
  # Normalize to 0-1 scale
  # -1 (all order) → 0
  #  0 (neutral) → 0.5
  # +1 (all disorder) → 1
  normalized <- (disorder_score + 1.0) / 2.0
  normalized <- max(0, min(1, normalized))
  
  return(normalized)
}


# Interpret disorder score
interpret_disorder <- function(score) {
  if (score > 0.7) return("Highly disordered/flexible")
  if (score > 0.5) return("Likely disordered")
  if (score > 0.3) return("Mixed order/disorder")
  return("Likely structured")
}


# Get color for disorder score visualization
disorder_score_color <- function(score) {
  if (score > 0.7) return("#28a745")  # Green - good for tagging
  if (score > 0.5) return("#7cb342")  # Light green
  if (score > 0.3) return("#ffc107")  # Yellow - caution
  return("#dc3545")  # Red - avoid tagging
}