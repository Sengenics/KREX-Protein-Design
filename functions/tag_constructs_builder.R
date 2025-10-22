# Define tag components (NO M - but we'll add for predictions)
TAG_COMPONENTS <- list(
  cmyc = "EQKLISEEDL",
  bccp = "AAAEISGHIVRSPMVGTFYRTPSPDAKAFIEVGQKVNVGDTLCIVEAMKMMNQIEADKSGTVKAILVESGQPVEFDEPLVVIE",
  linker_short = "GGSGSG",
  linker_long = "GGGGSGGGGS",
  his_tag = "HHHHHHHHHH",
  insect_sp = "MKTIIALSYIFCLVFA"
)

# Build full vector tags
VECTOR_TAGS <- list()

# pPRO30A: cMyc - GGSGSG - BCCP - GGGGSGGGGS
VECTOR_TAGS$pPRO30A <- list(
  name = "pPRO30A",
  position = "N-terminal",
  tag_no_m = paste0(TAG_COMPONENTS$cmyc, TAG_COMPONENTS$linker_short, 
                    TAG_COMPONENTS$bccp, TAG_COMPONENTS$linker_long),
  description = "N-terminal: M-cMyc-GGSGSG-BCCP-GGGGSGGGGS"
)

# pPRO30A-SP: Insect SP (with M) - cMyc - GGSGSG - BCCP - GGGGSGGGGS
VECTOR_TAGS$`pPRO30A-SP` <- list(
  name = "pPRO30A-SP",
  position = "N-terminal",
  insect_sp = TAG_COMPONENTS$insect_sp,
  tag_no_sp = paste0(TAG_COMPONENTS$cmyc, TAG_COMPONENTS$linker_short, 
                     TAG_COMPONENTS$bccp, TAG_COMPONENTS$linker_long),
  tag_with_sp = paste0(TAG_COMPONENTS$insect_sp, TAG_COMPONENTS$cmyc, 
                       TAG_COMPONENTS$linker_short, TAG_COMPONENTS$bccp, 
                       TAG_COMPONENTS$linker_long),
  description = "N-terminal: Insect SP-cMyc-GGSGSG-BCCP-GGGGSGGGGS"
)

# pPRO8: GGGGSGGGGS - BCCP - GGSGSG - cMyc - 10xHis
VECTOR_TAGS$pPRO8 <- list(
  name = "pPRO8",
  position = "C-terminal",
  tag = paste0(TAG_COMPONENTS$linker_long, TAG_COMPONENTS$bccp, 
               TAG_COMPONENTS$linker_short, TAG_COMPONENTS$cmyc, 
               TAG_COMPONENTS$his_tag),
  insect_sp = TAG_COMPONENTS$insect_sp,
  description = "C-terminal: GGGGSGGGGS-BCCP-GGSGSG-cMyc-10xHis"
)


# Build all 3 constructs for structure prediction (KEEP M)
build_all_constructs <- function(protein_sequence, signal_peptide_end = NULL) {
  
  cat("\n======================================\n")
  cat("BUILDING ALL CONSTRUCTS FOR PREDICTION\n")
  cat("======================================\n")
  cat("Full protein length:", nchar(protein_sequence), "aa\n")
  cat("Starts with:", substr(protein_sequence, 1, 10), "...\n")
  
  has_signal <- !is.null(signal_peptide_end) && signal_peptide_end > 0
  
  constructs <- list()
  
  # Determine mature protein
  if (has_signal) {
    cat("Native signal peptide: 1-", signal_peptide_end, "\n")
    native_signal_seq <- substr(protein_sequence, 1, signal_peptide_end)
    mature_protein <- substr(protein_sequence, signal_peptide_end + 1, nchar(protein_sequence))
    cat("Native signal: ", native_signal_seq, "\n")
    cat("Mature protein: ", nchar(mature_protein), " aa, starts with: ", 
        substr(mature_protein, 1, 10), "...\n")
  } else {
    cat("No native signal peptide detected\n")
    native_signal_seq <- NULL
    mature_protein <- protein_sequence
  }
  
  cat("\n")
  
  # ===== CONSTRUCT 1: UNTAGGED (reference) =====
  # Always starts with M for prediction
  cat("--- Construct 1: UNTAGGED (reference) ---\n")
  
  if (has_signal) {
    # Mature protein might not start with M - add it for prediction
    if (substr(mature_protein, 1, 1) != "M") {
      untagged_seq <- paste0("M", mature_protein)
      m_added_for_prediction <- TRUE
      cat("Added M for prediction (mature protein didn't start with M)\n")
    } else {
      untagged_seq <- mature_protein
      m_added_for_prediction <- FALSE
    }
  } else {
    # Full protein - if no M, add it
    if (substr(protein_sequence, 1, 1) != "M") {
      untagged_seq <- paste0("M", protein_sequence)
      m_added_for_prediction <- TRUE
      cat("Added M for prediction (protein didn't start with M)\n")
    } else {
      untagged_seq <- protein_sequence
      m_added_for_prediction <- FALSE
    }
  }
  
  constructs$untagged <- list(
    name = "Untagged",
    sequence = untagged_seq,
    total_length = nchar(untagged_seq),
    regions = list(
      protein = list(start = 1, end = nchar(untagged_seq))
    ),
    description = if(has_signal) "Mature protein (native SP removed)" else "Full protein",
    has_native_signal = has_signal,
    native_signal_seq = native_signal_seq,
    native_signal_length = if(has_signal) signal_peptide_end else 0,
    m_added_for_prediction = m_added_for_prediction
  )
  
  cat("Prediction sequence: ", substr(constructs$untagged$sequence, 1, 60), "...\n")
  cat("Length:", constructs$untagged$total_length, "aa\n\n")
  
  # ===== CONSTRUCT 2: N-TERMINAL TAG =====
  if (has_signal) {
    # Use pPRO30A-SP (with insect signal - already has M)
    cat("--- Construct 2: pPRO30A-SP (N-terminal) ---\n")
    vector_info <- VECTOR_TAGS$`pPRO30A-SP`
    
    # Insect SP (has M) + tag + mature protein
    constructs$n_tagged <- list(
      name = "pPRO30A-SP",
      vector_name = "pPRO30A-SP",
      sequence = paste0(vector_info$tag_with_sp, mature_protein),
      vector_info = vector_info,
      regions = list(
        insect_signal = list(start = 1, end = nchar(vector_info$insect_sp)),
        tag = list(start = nchar(vector_info$insect_sp) + 1, 
                   end = nchar(vector_info$tag_with_sp)),
        protein = list(start = nchar(vector_info$tag_with_sp) + 1, 
                       end = nchar(vector_info$tag_with_sp) + nchar(mature_protein))
      ),
      description = "Insect SP + N-terminal tag + mature protein",
      has_native_signal = has_signal,
      native_signal_seq = native_signal_seq,
      native_signal_length = signal_peptide_end,
      # For export: remove insect SP (vector adds it)
      export_sequence = paste0(vector_info$tag_no_sp, mature_protein),
      export_note = "Insect SP removed (vector supplies it)"
    )
    
  } else {
    # Use pPRO30A - need to add M for prediction
    cat("--- Construct 2: pPRO30A (N-terminal) ---\n")
    vector_info <- VECTOR_TAGS$pPRO30A
    
    # Determine protein without M for tagging
    if (substr(protein_sequence, 1, 1) == "M") {
      protein_no_m <- substr(protein_sequence, 2, nchar(protein_sequence))
      cat("Removing M from protein for N-terminal construct\n")
    } else {
      protein_no_m <- protein_sequence
    }
    
    # For prediction: M + tag + protein_no_m
    constructs$n_tagged <- list(
      name = "pPRO30A",
      vector_name = "pPRO30A",
      sequence = paste0("M", vector_info$tag_no_m, protein_no_m),
      vector_info = vector_info,
      regions = list(
        m_start = list(start = 1, end = 1),
        tag = list(start = 2, end = 1 + nchar(vector_info$tag_no_m)),
        protein = list(start = 2 + nchar(vector_info$tag_no_m), 
                       end = 1 + nchar(vector_info$tag_no_m) + nchar(protein_no_m))
      ),
      description = "M + N-terminal tag + protein (M removed from protein)",
      has_native_signal = FALSE,
      # For export: no M at start (vector adds it)
      export_sequence = paste0(vector_info$tag_no_m, protein_no_m),
      export_note = "M removed (vector supplies it)"
    )
  }
  
  constructs$n_tagged$total_length <- nchar(constructs$n_tagged$sequence)
  cat("Prediction sequence: ", substr(constructs$n_tagged$sequence, 1, 60), "...\n")
  cat("Length:", constructs$n_tagged$total_length, "aa\n")
  cat("Export sequence: ", substr(constructs$n_tagged$export_sequence, 1, 60), "...\n")
  cat("Export length:", nchar(constructs$n_tagged$export_sequence), "aa\n")
  cat("Regions:\n")
  for (rn in names(constructs$n_tagged$regions)) {
    r <- constructs$n_tagged$regions[[rn]]
    cat("  ", rn, ": ", r$start, "-", r$end, " (", r$end - r$start + 1, " aa)\n", sep = "")
  }
  cat("\n")
  
  # ===== CONSTRUCT 3: C-TERMINAL TAG =====
  cat("--- Construct 3: pPRO8 (C-terminal) ---\n")
  vector_info <- VECTOR_TAGS$pPRO8
  
  if (has_signal) {
    # Insect SP (has M) + mature protein + C-tag
    constructs$c_tagged <- list(
      name = "pPRO8",
      vector_name = "pPRO8",
      sequence = paste0(vector_info$insect_sp, mature_protein, vector_info$tag),
      vector_info = vector_info,
      regions = list(
        insect_signal = list(start = 1, end = nchar(vector_info$insect_sp)),
        protein = list(start = nchar(vector_info$insect_sp) + 1, 
                       end = nchar(vector_info$insect_sp) + nchar(mature_protein)),
        tag = list(start = nchar(vector_info$insect_sp) + nchar(mature_protein) + 1, 
                   end = nchar(vector_info$insect_sp) + nchar(mature_protein) + nchar(vector_info$tag))
      ),
      description = "Insect SP + mature protein + C-terminal tag",
      has_native_signal = has_signal,
      native_signal_seq = native_signal_seq,
      native_signal_length = signal_peptide_end,
      # For export: insect SP + mature + tag (keep as-is)
      export_sequence = paste0(vector_info$insect_sp, mature_protein, vector_info$tag),
      export_note = "Full sequence (insect SP included)"
    )
    
  } else {
    # Full protein (keep M if present) + C-tag
    constructs$c_tagged <- list(
      name = "pPRO8",
      vector_name = "pPRO8",
      sequence = paste0(protein_sequence, vector_info$tag),
      vector_info = vector_info,
      regions = list(
        protein = list(start = 1, end = nchar(protein_sequence)),
        tag = list(start = nchar(protein_sequence) + 1, 
                   end = nchar(protein_sequence) + nchar(vector_info$tag))
      ),
      description = "Full protein + C-terminal tag",
      has_native_signal = FALSE,
      # For export: same as prediction (keep M if present)
      export_sequence = paste0(protein_sequence, vector_info$tag),
      export_note = "Full sequence (M retained if originally present)"
    )
  }
  
  constructs$c_tagged$total_length <- nchar(constructs$c_tagged$sequence)
  cat("Prediction sequence: ", substr(constructs$c_tagged$sequence, 1, 60), "...\n")
  cat("Length:", constructs$c_tagged$total_length, "aa\n")
  cat("Export sequence: ", substr(constructs$c_tagged$export_sequence, 1, 60), "...\n")
  cat("Export length:", nchar(constructs$c_tagged$export_sequence), "aa\n")
  cat("Regions:\n")
  for (rn in names(constructs$c_tagged$regions)) {
    r <- constructs$c_tagged$regions[[rn]]
    cat("  ", rn, ": ", r$start, "-", r$end, " (", r$end - r$start + 1, " aa)\n", sep = "")
  }
  cat("\n")
  
  cat("======================================\n")
  cat("ALL CONSTRUCTS BUILT SUCCESSFULLY\n")
  cat("Note: Prediction sequences include M\n")
  cat("      Export sequences follow cloning rules\n")
  cat("======================================\n\n")
  
  return(constructs)
}
