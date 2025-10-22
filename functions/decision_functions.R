# functions/decision_functions.R
# Complete decision-making functions for expression strategy

library(dplyr)
library(jsonlite)

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =============================================================================
# PART 1: EXPRESSION BLOCKERS DETECTION
# =============================================================================

detect_expression_blockers <- function(features_df, sequence_length) {
  
  signal_peptides <- features_df %>%
    filter(type %in% c("Signal", "SIGNAL", "Transit peptide", "TRANSIT"))
  
  transmembrane <- features_df %>%
    filter(type %in% c("Transmembrane", "TRANSMEM", "Intramembrane", "INTRAMEM"))
  
  # Check if protein is too large (>150 kDa ~ 1350 aa)
  too_large <- sequence_length > 1350
  
  list(
    has_signal_peptide = nrow(signal_peptides) > 0,
    signal_peptide_end = if(nrow(signal_peptides) > 0) max(signal_peptides$end) else 0,
    has_transmembrane = nrow(transmembrane) > 0,
    transmembrane_count = nrow(transmembrane),
    too_large = too_large,
    recommended_start = if(nrow(signal_peptides) > 0) max(signal_peptides$end) + 1 else 1
  )
}

# =============================================================================
# PART 2: ADVANCED TERMINUS ANALYSIS
# =============================================================================

analyze_terminus_features_advanced <- function(features_df, sequence_length,
                                               epitope_df = NULL,
                                               n_term_buffer = 50, 
                                               c_term_buffer = 50) {
  
  n_term_end <- min(n_term_buffer, sequence_length)
  c_term_start <- max(1, sequence_length - c_term_buffer + 1)
  
  # Feature categories
  critical_blockers <- c(
    
    "Transmembrane", "TRANSMEM",
    "Intramembrane", "INTRAMEM"
  )
  
  high_risk <- c(
    "Active site", "ACT_SITE",
    "Binding site", "BINDING",
    "Domain", "DOMAIN",
    "Zinc finger", "ZN_FING",
    "DNA binding", "DNA_BIND",
    "Metal", "METAL",
    "Disulfide bond", "DISULFID"
  )
  
  medium_risk <- c(
    "Glycosylation", "CARBOHYD",
    "Coiled coil", "COILED",
    "Modified residue", "MOD_RES",
    "Repeat", "REPEAT",
    "Motif", "MOTIF"
  )
  
  low_risk <- c(
    "Region", "REGION",
    "Compositional bias", "COMPBIAS",
    "Chain", "CHAIN",
    "Helix", "HELIX",
    "Turn", "TURN",
    "Beta strand", "STRAND",
    "Signal", "SIGNAL",
    "Transit peptide", "TRANSIT"
  )
  
  # Analyze N-terminus
  n_term_features <- features_df %>%
    filter(start <= n_term_end | end <= n_term_end)
  
  n_critical <- n_term_features %>% filter(type %in% critical_blockers)
  n_high <- n_term_features %>% filter(type %in% high_risk)
  n_medium <- n_term_features %>% filter(type %in% medium_risk)
  n_low <- n_term_features %>% filter(type %in% low_risk)
  n_flexible <- n_term_features %>% 
    filter(type %in% c("Region", "REGION", "Turn", "TURN", "Compositional bias", "COMPBIAS"))
  
  # Calculate N-term score
  n_score <- 0
  n_issues <- list()
  
  if (nrow(n_critical) > 0) {
    n_score <- n_score + 100
    for (i in 1:nrow(n_critical)) {
      n_issues[[length(n_issues) + 1]] <- paste0(
        "CRITICAL: ", n_critical$type[i], " (aa ", n_critical$start[i], "-", n_critical$end[i], ")"
      )
    }
  }
  
  if (nrow(n_high) > 0) {
    for (i in 1:nrow(n_high)) {
      if (n_high$end[i] <= n_term_end) {
        n_score <- n_score + 20
        n_issues[[length(n_issues) + 1]] <- paste0(
          "HIGH: ", n_high$type[i], " fully within N-term (aa ", 
          n_high$start[i], "-", n_high$end[i], ")"
        )
      } else {
        n_score <- n_score + 10
        n_issues[[length(n_issues) + 1]] <- paste0(
          "HIGH: ", n_high$type[i], " extends into N-term (aa ", 
          n_high$start[i], "-", n_high$end[i], ")"
        )
      }
    }
  }
  
  if (nrow(n_medium) > 0) {
    n_score <- n_score + (nrow(n_medium) * 5)
    if (nrow(n_medium) > 2) {
      n_issues[[length(n_issues) + 1]] <- paste0(
        "MEDIUM: ", nrow(n_medium), " modifications/structural elements in N-term"
      )
    }
  }
  
  if (nrow(n_flexible) > 0 && nrow(n_critical) == 0 && nrow(n_high) == 0) {
    n_score <- n_score - 10
    n_issues[[length(n_issues) + 1]] <- paste0(
      "GOOD: ", nrow(n_flexible), " flexible region(s) - ideal for tagging"
    )
  }
  
  # Check for epitopes in N-terminus
  n_epitopes <- 0
  n_high_value_epitopes <- 0
  if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
    for (i in 1:nrow(epitope_df)) {
      pos <- epitope_df$Position[i]
      if (!is.na(pos) && grepl("-", pos)) {
        pos_parts <- strsplit(as.character(pos), "-")[[1]]
        if (length(pos_parts) == 2) {
          epi_start <- suppressWarnings(as.numeric(pos_parts[1]))
          if (!is.na(epi_start) && epi_start <= n_term_end) {
            n_epitopes <- n_epitopes + 1
            evidence <- epitope_df$Evidence_Level[i]
            if (!is.na(evidence) && evidence >= 4) {
              n_high_value_epitopes <- n_high_value_epitopes + 1
              n_score <- n_score + 15
              n_issues[[length(n_issues) + 1]] <- paste0(
                "EPITOPE: High-value epitope at aa ", pos, " - tag may block antibody binding"
              )
            } else {
              n_score <- n_score + 5
            }
          }
        }
      }
    }
  }
  
  # N-terminus recommendation
  n_recommendation <- "OK"
  n_recommendation_detail <- "N-terminus suitable for tagging"
  
  if (n_score >= 100) {
    n_recommendation <- "BLOCKED"
    n_recommendation_detail <- "N-terminus contains critical blockers (TM domain)"
  } else if (n_score >= 40) {
    n_recommendation <- "AVOID"
    n_recommendation_detail <- "N-terminus has multiple critical features - strongly avoid tagging"
  } else if (n_score >= 20) {
    n_recommendation <- "CAUTION"
    n_recommendation_detail <- "N-terminus has important features - tag with caution"
  } else if (n_score < 0) {
    n_recommendation <- "EXCELLENT"
    n_recommendation_detail <- "N-terminus has flexible regions - ideal for tagging"
  }
  
  # Analyze C-terminus (same logic)
  c_term_features <- features_df %>%
    filter(start >= c_term_start | end >= c_term_start)
  
  c_critical <- c_term_features %>% filter(type %in% critical_blockers)
  c_high <- c_term_features %>% filter(type %in% high_risk)
  c_medium <- c_term_features %>% filter(type %in% medium_risk)
  c_low <- c_term_features %>% filter(type %in% low_risk)
  c_flexible <- c_term_features %>% 
    filter(type %in% c("Region", "REGION", "Turn", "TURN", "Compositional bias", "COMPBIAS"))
  
  c_score <- 0
  c_issues <- list()
  
  if (nrow(c_critical) > 0) {
    c_score <- c_score + 100
    for (i in 1:nrow(c_critical)) {
      c_issues[[length(c_issues) + 1]] <- paste0(
        "CRITICAL: ", c_critical$type[i], " (aa ", c_critical$start[i], "-", c_critical$end[i], ")"
      )
    }
  }
  
  if (nrow(c_high) > 0) {
    for (i in 1:nrow(c_high)) {
      if (c_high$start[i] >= c_term_start) {
        c_score <- c_score + 20
        c_issues[[length(c_issues) + 1]] <- paste0(
          "HIGH: ", c_high$type[i], " fully within C-term (aa ", 
          c_high$start[i], "-", c_high$end[i], ")"
        )
      } else {
        c_score <- c_score + 10
        c_issues[[length(c_issues) + 1]] <- paste0(
          "HIGH: ", c_high$type[i], " extends into C-term (aa ", 
          c_high$start[i], "-", c_high$end[i], ")"
        )
      }
    }
  }
  
  if (nrow(c_medium) > 0) {
    c_score <- c_score + (nrow(c_medium) * 5)
    if (nrow(c_medium) > 2) {
      c_issues[[length(c_issues) + 1]] <- paste0(
        "MEDIUM: ", nrow(c_medium), " modifications/structural elements in C-term"
      )
    }
  }
  
  if (nrow(c_flexible) > 0 && nrow(c_critical) == 0 && nrow(c_high) == 0) {
    c_score <- c_score - 10
    c_issues[[length(c_issues) + 1]] <- paste0(
      "GOOD: ", nrow(c_flexible), " flexible region(s) - ideal for tagging"
    )
  }
  
  # Check for epitopes in C-terminus
  c_epitopes <- 0
  c_high_value_epitopes <- 0
  if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
    for (i in 1:nrow(epitope_df)) {
      pos <- epitope_df$Position[i]
      if (!is.na(pos) && grepl("-", pos)) {
        pos_parts <- strsplit(as.character(pos), "-")[[1]]
        if (length(pos_parts) == 2) {
          epi_start <- suppressWarnings(as.numeric(pos_parts[1]))
          if (!is.na(epi_start) && epi_start >= c_term_start) {
            c_epitopes <- c_epitopes + 1
            evidence <- epitope_df$Evidence_Level[i]
            if (!is.na(evidence) && evidence >= 4) {
              c_high_value_epitopes <- c_high_value_epitopes + 1
              c_score <- c_score + 15
              c_issues[[length(c_issues) + 1]] <- paste0(
                "EPITOPE: High-value epitope at aa ", pos, " - tag may block antibody binding"
              )
            } else {
              c_score <- c_score + 5
            }
          }
        }
      }
    }
  }
  
  # C-terminus recommendation
  c_recommendation <- "OK"
  c_recommendation_detail <- "C-terminus suitable for tagging"
  
  if (c_score >= 100) {
    c_recommendation <- "BLOCKED"
    c_recommendation_detail <- "C-terminus contains critical blockers (TM domain)"
  } else if (c_score >= 40) {
    c_recommendation <- "AVOID"
    c_recommendation_detail <- "C-terminus has multiple critical features - strongly avoid tagging"
  } else if (c_score >= 20) {
    c_recommendation <- "CAUTION"
    c_recommendation_detail <- "C-terminus has important features - tag with caution"
  } else if (c_score < 0) {
    c_recommendation <- "EXCELLENT"
    c_recommendation_detail <- "C-terminus has flexible regions - ideal for tagging"
  }
  
  # Determine preferred position
  has_n_signal <- any(n_critical$type %in% c("Signal", "SIGNAL", "Transit peptide", "TRANSIT"))
  
  preferred_tag <- "N-terminal"
  preferred_reasoning <- ""
  
  if (n_score < c_score) {
    preferred_tag <- "N-terminal"
    preferred_reasoning <- paste0("N-terminus clearer (score: ", n_score, " vs ", c_score, ")")
  } else if (c_score < n_score) {
    preferred_tag <- "C-terminal"
    preferred_reasoning <- paste0("C-terminus clearer (score: ", c_score, " vs ", n_score, ")")
  } else {
    if (n_high_value_epitopes > c_high_value_epitopes) {
      preferred_tag <- "C-terminal"
      preferred_reasoning <- "More high-value epitopes at N-terminus - preserve antibody access"
    } else if (nrow(n_flexible) > nrow(c_flexible)) {
      preferred_tag <- "N-terminal"
      preferred_reasoning <- "N-terminus more flexible - better for tag accommodation"
    } else {
      preferred_tag <- "C-terminal"
      preferred_reasoning <- "Scores equal - default to C-terminal for insect cell expression"
    }
  }
  
  list(
    n_terminus = list(
      total_features = nrow(n_term_features),
      critical_blockers = nrow(n_critical),
      high_risk = nrow(n_high),
      medium_risk = nrow(n_medium),
      flexible_regions = nrow(n_flexible),
      epitopes = n_epitopes,
      high_value_epitopes = n_high_value_epitopes,
      score = n_score,
      recommendation = n_recommendation,
      recommendation_detail = n_recommendation_detail,
      issues = n_issues,
      features = n_term_features
    ),
    c_terminus = list(
      total_features = nrow(c_term_features),
      critical_blockers = nrow(c_critical),
      high_risk = nrow(c_high),
      medium_risk = nrow(c_medium),
      flexible_regions = nrow(c_flexible),
      epitopes = c_epitopes,
      high_value_epitopes = c_high_value_epitopes,
      score = c_score,
      recommendation = c_recommendation,
      recommendation_detail = c_recommendation_detail,
      issues = c_issues,
      features = c_term_features
    ),
    preferred_tag_position = preferred_tag,
    preferred_reasoning = preferred_reasoning,
    overall_assessment = paste0(
      "N-term: ", n_recommendation, " (score: ", n_score, "), ",
      "C-term: ", c_recommendation, " (score: ", c_score, "). ",
      "Recommendation: ", preferred_tag, " - ", preferred_reasoning
    )
  )
}

# =============================================================================
# PART 3: EPITOPE ANALYSIS
# =============================================================================


analyze_epitope_impact <- function(epitope_df, sequence_length,
                                   n_term_buffer = 50, c_term_buffer = 50) {
  
  if (is.null(epitope_df) || nrow(epitope_df) == 0) {
    cat("  No epitope data provided\n")
    return(list(
      n_terminus_epitopes = 0,
      c_terminus_epitopes = 0,
      high_value_epitopes = 0,
      total_epitopes = 0,
      recommendation = "No epitope data - consider full length"
    ))
  }
  
  cat("  Analyzing", nrow(epitope_df), "epitopes\n")
  
  # Parse positions - handle both formats
  epitope_df$start_pos <- NA
  epitope_df$end_pos <- NA
  
  for (i in 1:nrow(epitope_df)) {
    pos <- epitope_df$Position[i]
    
    # Skip if position is missing or conformational
    if (is.na(pos) || pos == "" || tolower(pos) == "conformational" || tolower(pos) == "unknown") {
      next
    }
    
    # Try to parse position range (e.g., "10-25")
    if (grepl("-", pos)) {
      pos_parts <- strsplit(as.character(pos), "-")[[1]]
      if (length(pos_parts) == 2) {
        start <- suppressWarnings(as.numeric(trimws(pos_parts[1])))
        end <- suppressWarnings(as.numeric(trimws(pos_parts[2])))
        
        if (!is.na(start) && !is.na(end)) {
          epitope_df$start_pos[i] <- start
          epitope_df$end_pos[i] <- end
          cat("    Epitope", i, ": position", start, "-", end, "\n")
        }
      }
    } 
    # Try single position (e.g., "10")
    else {
      single_pos <- suppressWarnings(as.numeric(trimws(pos)))
      if (!is.na(single_pos)) {
        epitope_df$start_pos[i] <- single_pos
        epitope_df$end_pos[i] <- single_pos
        cat("    Epitope", i, ": position", single_pos, "\n")
      }
    }
  }
  
  # Remove epitopes without valid positions
  valid_epitopes <- epitope_df[!is.na(epitope_df$start_pos), ]
  
  if (nrow(valid_epitopes) == 0) {
    cat("  No epitopes with valid positions found\n")
    return(list(
      n_terminus_epitopes = 0,
      c_terminus_epitopes = 0,
      high_value_epitopes = 0,
      total_epitopes = nrow(epitope_df),
      recommendation = "Epitopes found but positions not parseable - consider full length"
    ))
  }
  
  cat("  Successfully parsed", nrow(valid_epitopes), "epitope positions\n")
  
  n_term_end <- min(n_term_buffer, sequence_length)
  c_term_start <- max(1, sequence_length - c_term_buffer + 1)
  
  # Count epitopes in termini
  n_term_epitopes <- sum(valid_epitopes$start_pos <= n_term_end, na.rm = TRUE)
  c_term_epitopes <- sum(valid_epitopes$start_pos >= c_term_start, na.rm = TRUE)
  
  cat("  N-terminus epitopes (1-", n_term_end, "):", n_term_epitopes, "\n")
  cat("  C-terminus epitopes (", c_term_start, "-", sequence_length, "):", c_term_epitopes, "\n")
  
  # Count high-value epitopes (evidence >= 4)
  high_value <- 0
  if ("Evidence_Level" %in% names(valid_epitopes)) {
    high_value <- sum(valid_epitopes$Evidence_Level >= 4, na.rm = TRUE)
    cat("  High-value epitopes (evidence ≥4):", high_value, "\n")
  }
  
  result <- list(
    n_terminus_epitopes = n_term_epitopes,
    c_terminus_epitopes = c_term_epitopes,
    high_value_epitopes = high_value,
    total_epitopes = nrow(epitope_df),
    total_with_positions = nrow(valid_epitopes),
    recommendation = if(high_value > 0) {
      "Include all high-value epitopes in construct"
    } else if(nrow(valid_epitopes) > 0) {
      "Epitopes detected - consider preserving epitope regions"
    } else {
      "Consider full length to maximize epitope coverage"
    }
  )
  
  cat("  Analysis complete\n")
  return(result)
}

# =============================================================================
# PART 4: EXPRESSION HISTORY ANALYSIS
# =============================================================================

analyze_expression_history <- function(expression_df) {
  
  if (is.null(expression_df) || nrow(expression_df) == 0) {
    return(list(
      successful_expression = FALSE,
      best_system = "Unknown",
      recommended_tag = "6xHis",
      tag_position = "N-terminal",
      summary = "No prior expression data found",
      evidence_score = 0
    ))
  }
  
  successful <- expression_df %>%
    filter(Evidence_Score >= 3)
  
  if (nrow(successful) == 0) {
    return(list(
      successful_expression = FALSE,
      best_system = "None validated",
      recommended_tag = "6xHis",
      tag_position = "N-terminal",
      summary = "Prior expression attempts found but not well validated",
      evidence_score = 0
    ))
  }
  
  best <- successful %>%
    arrange(desc(Evidence_Score)) %>%
    slice(1)
  
  tag_positions <- table(successful$Tag_Position)
  preferred_position <- names(which.max(tag_positions))
  
  common_tags <- table(successful$Affinity_Tag)
  preferred_tag <- names(which.max(common_tags))
  
  list(
    successful_expression = TRUE,
    best_system = best$Expression_Host,
    best_vector = best$Vector_Plasmid %||% "Not specified",
    recommended_tag = preferred_tag,
    tag_position = preferred_position,
    evidence_score = best$Evidence_Score,
    summary = paste0("Successfully expressed in ", best$Expression_Host, 
                     " with ", preferred_tag, " tag (", preferred_position, ")")
  )
}

# =============================================================================
# PART 5: BUILD AI PROMPT
# =============================================================================

# REPLACE the build_decision_prompt function with this improved version

build_decision_prompt <- function(protein_info, terminus_analysis, 
                                  epitope_analysis, expression_history,
                                  expression_blockers, epitope_df = NULL) {  # ADD epitope_df
  
  prompt <- paste0(
    "You are a protein expression design expert for INSECT CELL expression (Sf9/Sf21 baculovirus system) of HUMAN proteins.\n",
    "Your goal: Design an expression construct that MAXIMIZES antibody epitope accessibility while ensuring successful expression.\n\n",
    
    "=== STRICT LABORATORY CONSTRAINTS (MUST FOLLOW) ===\n",
    "1. Expression System: ALWAYS Sf9/Sf21 insect cells with baculovirus\n",
    "   - These cells can perform simple glycosylation (simpler than mammalian)\n",
    "   - Native signal peptides must be removed and replaced with insect signal (HA)\n",
    "   - Good for proteins 10-150 kDa\n\n",
    
    "2. Affinity Tags: ONLY 6xHis-tag OR cMyc-tag\n",
    "   - 6xHis\n",
    "   - cMyc\n",
    "   - Tag adds ~10 amino acids\n\n",
    
    "3. Signal Peptide Handling:\n",
    "   - If native signal peptide present: REMOVE completely (do not include in construct)\n",
    "   - Insect cell signal will be added by expression vector (not part of construct design)\n",
    "   - Construct starts from mature protein (after signal cleavage site)\n",
    "   - N-terminal tag goes on mature protein N-terminus (after signal processing)\n\n",
    
    "4. Construct Design Philosophy:\n",
    "   - DEFAULT: Full-length mature protein (after signal removal)\n",
    "   - ONLY truncate if there is a STRONG reason:\n",
    "     a) Protein >150 kDa AND has clear domain boundaries\n",
    "     b) Contains transmembrane domain(s) that cannot be expressed\n",
    "     c) Large unstructured regions (>100 aa) with no epitopes\n",
    "     d) Aggregation-prone regions that don't contain epitopes\n",
    "   - PRESERVE: All regions containing high-value epitopes (evidence ≥4)\n\n",
    
    "5. Tag Position Decision Tree:\n",
    "   Step 1: Check if C-terminus is clear (score <20) → Use C-terminal tag\n",
    "   Step 2: If C-term blocked, check N-terminus (after signal) → Use N-terminal tag\n",
    "   Step 3: If BOTH termini problematic → Choose lesser evil based on:\n",
    "           - Fewer high-value epitopes nearby\n",
    "           - More flexible regions nearby\n",
    "           - Lower functional impact\n\n",
    
    "=== PROTEIN INFORMATION ===\n",
    "Protein: ", protein_info$protein_name, "\n",
    "Organism: ", protein_info$organism %||% "Homo sapiens", "\n",
    "UniProt ID: ", protein_info$uniprot_id, "\n",
    "Full Sequence Length: ", protein_info$sequence_length, " amino acids\n",
    "Estimated MW: ~", round(protein_info$sequence_length * 0.11, 1), " kDa\n",
    "Size Assessment: ", 
    if(protein_info$sequence_length < 100) "Small - easy to express" 
    else if(protein_info$sequence_length < 500) "Medium - standard size"
    else if(protein_info$sequence_length < 1000) "Large - may be challenging"
    else "Very large - consider domain expression", "\n\n",
    
    "=== EXPRESSION FEASIBILITY ===\n",
    "Signal Peptide: ", 
    if(expression_blockers$has_signal_peptide) {
      paste0("✓ YES - detected at aa 1-", expression_blockers$signal_peptide_end, 
             "\n   ACTION: Remove signal peptide. Construct starts at aa ", 
             expression_blockers$recommended_start, " (mature protein).\n",
             "   Insect signal (gp67/melittin) will be added by vector.\n")
    } else {
      "✗ NO - no native signal detected\n   ACTION: Start construct at aa 1.\n"
    }, "\n",
    
    "Transmembrane Domains: ",
    if(expression_blockers$has_transmembrane) {
      paste0("⚠ WARNING - ", expression_blockers$transmembrane_count, " TM domain(s) detected\n",
             "   PROBLEM: TM domains cannot be expressed in soluble form\n",
             "   ACTION: Must truncate to exclude TM regions OR express only extracellular domains\n")
    } else {
      "✓ OK - no TM domains detected\n   ACTION: Can express full-length protein\n"
    }, "\n",
    
    "Size for Insect Cells: ",
    if(expression_blockers$too_large) {
      paste0("⚠ LARGE - >150 kDa (~", round(protein_info$sequence_length * 0.11), " kDa)\n",
             "   CONCERN: Large proteins may have lower yields\n",
             "   ACTION: Consider expressing individual domains if epitopes are localized\n")
    } else {
      "✓ GOOD - within optimal range for insect cells\n"
    }, "\n\n"
  )
  
  # ADD DETAILED EPITOPE INFORMATION
  if (!is.null(epitope_df) && nrow(epitope_df) > 0) {
    
    # Parse epitope positions
    epitope_df$start_pos <- NA
    epitope_df$end_pos <- NA
    
    for (i in 1:nrow(epitope_df)) {
      pos <- epitope_df$Position[i]
      if (!is.na(pos) && grepl("-", pos)) {
        pos_parts <- strsplit(as.character(pos), "-")[[1]]
        if (length(pos_parts) == 2) {
          epitope_df$start_pos[i] <- suppressWarnings(as.numeric(pos_parts[1]))
          epitope_df$end_pos[i] <- suppressWarnings(as.numeric(pos_parts[2]))
        }
      }
    }
    
    valid_epitopes <- epitope_df[!is.na(epitope_df$start_pos), ]
    
    if (nrow(valid_epitopes) > 0) {
      # Sort by position
      valid_epitopes <- valid_epitopes[order(valid_epitopes$start_pos), ]
      
      prompt <- paste0(prompt,
                       "=== AUTOIMMUNE EPITOPE MAP (CRITICAL FOR ANTIBODY BINDING) ===\n",
                       "Total epitopes found: ", nrow(valid_epitopes), "\n",
                       "Source: ", if("Source" %in% names(valid_epitopes)) valid_epitopes$Source[1] else "Unknown", "\n\n",
                       
                       "IMPORTANT: These epitopes must be preserved and accessible for antibody binding!\n",
                       "Do NOT place tags directly adjacent to high-value epitopes.\n",
                       "Do NOT truncate protein in a way that removes high-value epitopes.\n\n"
      )
      
      # List high-value epitopes first
      high_value <- valid_epitopes[valid_epitopes$Evidence_Level >= 4, ]
      if (nrow(high_value) > 0) {
        prompt <- paste0(prompt, "⭐ HIGH-VALUE EPITOPES (Evidence ≥4) - MUST PRESERVE:\n")
        for (i in 1:nrow(high_value)) {
          epi <- high_value[i, ]
          prompt <- paste0(prompt,
                           "   ", i, ". Position ", epi$start_pos, "-", epi$end_pos, 
                           " (", epi$end_pos - epi$start_pos + 1, " aa) - Evidence: ", epi$Evidence_Level, "/5\n",
                           "      Type: ", epi$Epitope_Type, "\n",
                           "      Context: ", substr(epi$Antibody_Context, 1, 60), "...\n"
          )
        }
        prompt <- paste0(prompt, "\n")
      }
      
      # List medium-value epitopes
      medium_value <- valid_epitopes[valid_epitopes$Evidence_Level >= 2 & valid_epitopes$Evidence_Level < 4, ]
      if (nrow(medium_value) > 0) {
        prompt <- paste0(prompt, "MEDIUM-VALUE EPITOPES (Evidence 2-3) - PRESERVE IF POSSIBLE:\n")
        for (i in 1:min(5, nrow(medium_value))) {  # Show up to 5
          epi <- medium_value[i, ]
          prompt <- paste0(prompt,
                           "   ", i, ". Position ", epi$start_pos, "-", epi$end_pos, 
                           " - Evidence: ", epi$Evidence_Level, "/5\n"
          )
        }
        if (nrow(medium_value) > 5) {
          prompt <- paste0(prompt, "   ... and ", nrow(medium_value) - 5, " more\n")
        }
        prompt <- paste0(prompt, "\n")
      }
      
      # Epitope distribution analysis
      n_term_end <- min(protein_info$n_term_buffer, protein_info$sequence_length)
      c_term_start <- max(1, protein_info$sequence_length - protein_info$c_term_buffer + 1)
      
      n_term_epi <- valid_epitopes[valid_epitopes$start_pos <= n_term_end, ]
      c_term_epi <- valid_epitopes[valid_epitopes$start_pos >= c_term_start, ]
      core_epi <- valid_epitopes[valid_epitopes$start_pos > n_term_end & 
                                   valid_epitopes$start_pos < c_term_start, ]
      
      prompt <- paste0(prompt,
                       "EPITOPE DISTRIBUTION:\n",
                       "   N-terminus (aa 1-", n_term_end, "): ", nrow(n_term_epi), " epitopes\n"
      )
      if (nrow(n_term_epi) > 0) {
        high_n <- sum(n_term_epi$Evidence_Level >= 4)
        if (high_n > 0) {
          prompt <- paste0(prompt, "      ⚠ WARNING: ", high_n, " high-value epitope(s) in N-terminus region!\n")
          prompt <- paste0(prompt, "      RECOMMENDATION: Avoid N-terminal tag or use minimal tag (6xHis)\n")
        }
      }
      
      prompt <- paste0(prompt,
                       "   Core region (aa ", n_term_end + 1, "-", c_term_start - 1, "): ", nrow(core_epi), " epitopes\n"
      )
      
      prompt <- paste0(prompt,
                       "   C-terminus (aa ", c_term_start, "-", protein_info$sequence_length, "): ", 
                       nrow(c_term_epi), " epitopes\n"
      )
      if (nrow(c_term_epi) > 0) {
        high_c <- sum(c_term_epi$Evidence_Level >= 4)
        if (high_c > 0) {
          prompt <- paste0(prompt, "      ⚠ WARNING: ", high_c, " high-value epitope(s) in C-terminus region!\n")
          prompt <- paste0(prompt, "      RECOMMENDATION: Avoid C-terminal tag or use minimal tag (6xHis)\n")
        }
      }
      
      prompt <- paste0(prompt, "\n")
    }
  } else {
    prompt <- paste0(prompt,
                     "=== AUTOIMMUNE EPITOPE MAP ===\n",
                     "No epitope data available.\n",
                     "RECOMMENDATION: Express full-length to maximize potential epitope coverage.\n\n"
    )
  }
  
  # Continue with terminus analysis
  prompt <- paste0(prompt,
                   "=== TERMINUS STRUCTURAL ANALYSIS ===\n",
                   "Analysis window: N-term = first ", protein_info$n_term_buffer, " aa, ",
                   "C-term = last ", protein_info$c_term_buffer, " aa\n",
                   "Scoring: Lower is better. <0=Excellent, 0-20=OK, 20-40=Caution, 40-100=Avoid, ≥100=Blocked\n\n",
                   
                   "N-TERMINUS:\n",
                   "   Score: ", terminus_analysis$n_terminus$score, " (", 
                   terminus_analysis$n_terminus$recommendation, ")\n",
                   "   Features: ", terminus_analysis$n_terminus$total_features, 
                   " total (", terminus_analysis$n_terminus$critical_blockers, " critical, ",
                   terminus_analysis$n_terminus$high_impact, " high-risk)\n",
                   "   Flexible regions: ", terminus_analysis$n_terminus$flexible_regions, 
                   if(terminus_analysis$n_terminus$flexible_regions > 0) " ✓ GOOD for tagging" else "", "\n"
  )
  
  if (length(terminus_analysis$n_terminus$issues) > 0) {
    prompt <- paste0(prompt, "   Issues:\n")
    for (issue in head(terminus_analysis$n_terminus$issues, 5)) {
      prompt <- paste0(prompt, "      • ", issue, "\n")
    }
  }
  
  prompt <- paste0(prompt, "\n",
                   "C-TERMINUS:\n",
                   "   Score: ", terminus_analysis$c_terminus$score, " (", 
                   terminus_analysis$c_terminus$recommendation, ")\n",
                   "   Features: ", terminus_analysis$c_terminus$total_features, 
                   " total (", terminus_analysis$c_terminus$critical_blockers, " critical, ",
                   terminus_analysis$c_terminus$high_impact, " high-risk)\n",
                   "   Flexible regions: ", terminus_analysis$c_terminus$flexible_regions,
                   if(terminus_analysis$c_terminus$flexible_regions > 0) " ✓ GOOD for tagging" else "", "\n"
  )
  
  if (length(terminus_analysis$c_terminus$issues) > 0) {
    prompt <- paste0(prompt, "   Issues:\n")
    for (issue in head(terminus_analysis$c_terminus$issues, 5)) {
      prompt <- paste0(prompt, "      • ", issue, "\n")
    }
  }
  
  prompt <- paste0(prompt, "\n",
                   "RULE-BASED RECOMMENDATION: ", terminus_analysis$preferred_tag_position, "\n",
                   "Reasoning: ", terminus_analysis$preferred_reasoning, "\n\n"
  )
  
  # Expression history
  if (expression_history$successful_expression) {
    prompt <- paste0(prompt,
                     "=== PRIOR EXPRESSION SUCCESS ===\n",
                     "✓ This protein HAS been successfully expressed!\n",
                     "   System: ", expression_history$best_system, "\n",
                     "   Tag: ", expression_history$recommended_tag, " (", 
                     expression_history$tag_position, ")\n",
                     "   Evidence: ", expression_history$evidence_score, "/5\n",
                     "   Summary: ", expression_history$summary, "\n\n",
                     
                     "IMPORTANT: Prior success is STRONG evidence. Consider following this approach unless:\n",
                     "   - It conflicts with epitope preservation\n",
                     "   - Insect cells offer advantages over their system\n",
                     "   - Their construct was truncated and you can do full-length\n\n"
    )
  } else {
    prompt <- paste0(prompt,
                     "=== PRIOR EXPRESSION HISTORY ===\n",
                     "No validated expression data found in literature.\n",
                     "Base recommendation on structural features and epitope preservation.\n\n"
    )
  }
  
  # Decision task
  prompt <- paste0(prompt,
                   "=== YOUR DECISION TASK ===\n\n",
                   
                   "STEP 1: CONSTRUCT DESIGN\n",
                   "   - Start position: aa ", expression_blockers$recommended_start, 
                   " (after signal removal)\n",
                   "   - End position: aa ", protein_info$sequence_length, " OR earlier if truncation needed\n",
                   "   - Check: Does construct include ALL high-value epitopes?\n",
                   "   - Check: Are there TM domains to avoid?\n\n",
                   
                   "STEP 2: TAG SELECTION\n",
                   "   - Choose 6xHis OR cMyc (6xHis preferred unless specific reason)\n",
                   "   - 6xHis: Small, minimal interference, nickel purification\n",
                   "   - cMyc: Slightly larger, use if His-tag problematic\n\n",
                   
                   "STEP 3: TAG POSITION\n",
                   "   - Compare N-term score (", terminus_analysis$n_terminus$score, 
                   ") vs C-term score (", terminus_analysis$c_terminus$score, ")\n",
                   "   - Check: Are there high-value epitopes near either terminus?\n",
                   "   - Choose terminus with: Lower score + Fewer epitopes + More flexibility\n",
                   "   - If both bad: Choose lesser evil, document trade-offs\n\n",
                   
                   "STEP 4: BACKUP PLAN\n",
                   "   - What to try if primary approach fails?\n",
                   "   - Alternative tag position\n",
                   "   - Alternative tag type\n",
                   "   - Truncation options\n\n",
                   
                   "OUTPUT FORMAT (JSON):\n",
                   "{\n",
                   '  "Recommended_Construct": "full-length mature protein OR aa X-Y with justification",\n',
                   '  "Construct_Range": "X-Y amino acids",\n',
                   '  "Signal_Peptide_Handling": "removed aa 1-X, construct starts at mature protein" OR "none detected",\n',
                   '  "Preferred_Tag": "6xHis OR cMyc",\n',
                   '  "Preferred_Tag_Position": "N-terminal OR C-terminal OR Neither",\n',
                   '  "Tag_Position_Justification": "Why this position? Address epitopes and features",\n',
                   '  "Alternative_Tag_Position": "opposite terminus",\n',
                   '  "Expression_System": "Sf9/Sf21 insect cells - baculovirus",\n',
                   '  "Justification_For_Truncation": "explain IF truncated, null if full-length",\n',
                   '  "Epitope_Preservation_Strategy": "how are high-value epitopes protected?",\n',
                   '  "Key_Considerations": [\n',
                   '    "3-5 critical factors, MUST mention epitopes if present",\n',
                   '    "Mention specific epitope positions if relevant"\n',
                   '  ],\n',
                   '  "Potential_Challenges": [\n',
                   '    "2-4 specific issues that could cause expression failure",\n',
                   '    "Include epitope accessibility concerns if relevant"\n',
                   '  ],\n',
                   '  "Backup_Plan": {\n',
                   '    "if_primary_fails": "what to try next",\n',
                   '    "alternative_approach": "specific alternative recommendation"\n',
                   '  },\n',
                   '  "Reasoning": "2-3 paragraphs explaining decision, integrating ALL data",\n',
                   '  "Confidence_Score": 1-5 (5=very confident, 1=uncertain)\n',
                   "}\n\n",
                   
                   "CRITICAL REMINDERS:\n",
                   "✓ MUST use only Sf9/Sf21 insect cells\n",
                   "✓ MUST use only 6xHis or cMyc tags\n",
                   "✓ MUST preserve high-value epitopes (evidence ≥4)\n",
                   "✓ MUST remove native signal peptides\n",
                   "✓ Prefer full-length unless strong reason to truncate\n",
                   "✓ Provide specific, actionable recommendations\n\n",
                   
                   "Return ONLY valid JSON, no markdown formatting, no other text."
  )
  
  return(prompt)
}

# =============================================================================
# PART 6: PARSE AI RESPONSE
# =============================================================================

parse_decision_response <- function(json_text) {
  json_text <- gsub("```json\\s*", "", json_text)
  json_text <- gsub("```\\s*$", "", json_text)
  json_text <- trimws(json_text)
  
  tryCatch({
    jsonlite::fromJSON(json_text, simplifyVector = TRUE)
  }, error = function(e) {
    list(Error = paste("Failed to parse decision:", substr(json_text, 1, 200)))
  })
}