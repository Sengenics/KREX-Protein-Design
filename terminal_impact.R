# Smart impact analysis based on actual protein biology
# Replace the messy plotting with this focused analysis

analyze_terminal_tag_impact <- function(features_df, uniprot_id = NULL, sequence_length = NULL, 
                                        n_term_buffer = 30, c_term_buffer = 30) {
  
  if (!is.null(uniprot_id)) {
    features_df <- features_df[features_df$uniprot_id == uniprot_id, ]
  }
  
  if (is.null(sequence_length)) {
    max_end <- max(features_df$end, na.rm = TRUE)
    sequence_length <- ifelse(is.finite(max_end), max_end + 50, 1000)
  }
  
  protein_id <- unique(features_df$uniprot_id)[1]
  
  # Biological rules for tag impact
  n_terminal_critical_rules <- list(
    # These MUST be at the N-terminus to work
    "signal_peptides" = list(
      types = c("Signal", "SIGNAL"),
      rule = "Any signal peptide will be blocked by N-terminal tags",
      impact = "CRITICAL",
      affected_positions = function(feat) feat$start <= 50  # Signals are usually early
    ),
    
    "transit_peptides" = list(
      types = c("Transit peptide", "TRANSIT"),
      rule = "Transit peptides need free N-terminus for organelle targeting", 
      impact = "CRITICAL",
      affected_positions = function(feat) feat$start <= 50
    ),
    
    "n_terminal_processing" = list(
      types = c("Initiator methionine", "INIT_MET", "Propeptide", "PROPEP"),
      rule = "N-terminal processing sites will be disrupted",
      impact = "CRITICAL", 
      affected_positions = function(feat) feat$start <= 20
    ),
    
    # These need proper N-terminal structure
    "n_terminal_domains" = list(
      types = c("Domain", "DOMAIN", "Active site", "ACT_SITE", "Binding site", "BINDING"),
      rule = "N-terminal functional elements may be disrupted by steric hindrance",
      impact = "MODERATE",
      affected_positions = function(feat) feat$start <= n_term_buffer
    ),
    
    "n_terminal_structure" = list(
      types = c("Helix", "HELIX", "Beta strand", "STRAND", "Disulfide bond", "DISULFID"),
      rule = "N-terminal secondary structure may be destabilized", 
      impact = "LOW",
      affected_positions = function(feat) feat$start <= n_term_buffer
    )
  )
  
  c_terminal_critical_rules <- list(
    # These need free C-terminus
    "c_terminal_targeting" = list(
      types = c("Signal", "SIGNAL"),  # Some proteins have C-terminal signals (rare)
      rule = "C-terminal targeting signals will be blocked",
      impact = "CRITICAL",
      affected_positions = function(feat) feat$end >= (sequence_length - 20)
    ),
    
    "membrane_topology" = list(
      types = c("Transmembrane", "TRANSMEM", "Topological domain", "TOPO_DOM"),
      rule = "C-terminal membrane topology critical for proper insertion",
      impact = "HIGH", 
      affected_positions = function(feat) feat$end >= (sequence_length - c_term_buffer)
    ),
    
    "c_terminal_domains" = list(
      types = c("Domain", "DOMAIN", "Active site", "ACT_SITE", "Binding site", "BINDING"),
      rule = "C-terminal functional elements may be disrupted",
      impact = "MODERATE",
      affected_positions = function(feat) feat$end >= (sequence_length - c_term_buffer)
    ),
    
    "c_terminal_structure" = list(
      types = c("Helix", "HELIX", "Beta strand", "STRAND", "Disulfide bond", "DISULFID"),
      rule = "C-terminal secondary structure may be affected",
      impact = "LOW", 
      affected_positions = function(feat) feat$end >= (sequence_length - c_term_buffer)
    )
  )
  
  # Analyze N-terminal impact
  n_terminal_issues <- data.frame(
    Feature_Type = character(),
    Position = character(), 
    Impact_Level = character(),
    Reason = character(),
    stringsAsFactors = FALSE
  )
  
  for (rule_name in names(n_terminal_critical_rules)) {
    rule <- n_terminal_critical_rules[[rule_name]]
    matching_features <- features_df[features_df$type %in% rule$types, ]
    
    if (nrow(matching_features) > 0) {
      for (i in 1:nrow(matching_features)) {
        feat <- matching_features[i, ]
        if (rule$affected_positions(feat)) {
          n_terminal_issues <- rbind(n_terminal_issues, data.frame(
            Feature_Type = feat$type,
            Position = paste0(feat$start, "-", feat$end),
            Impact_Level = rule$impact,
            Reason = rule$rule,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Analyze C-terminal impact
  c_terminal_issues <- data.frame(
    Feature_Type = character(),
    Position = character(),
    Impact_Level = character(), 
    Reason = character(),
    stringsAsFactors = FALSE
  )
  
  for (rule_name in names(c_terminal_critical_rules)) {
    rule <- c_terminal_critical_rules[[rule_name]]
    matching_features <- features_df[features_df$type %in% rule$types, ]
    
    if (nrow(matching_features) > 0) {
      for (i in 1:nrow(matching_features)) {
        feat <- matching_features[i, ]
        if (rule$affected_positions(feat)) {
          c_terminal_issues <- rbind(c_terminal_issues, data.frame(
            Feature_Type = feat$type,
            Position = paste0(feat$start, "-", feat$end),
            Impact_Level = rule$impact,
            Reason = rule$rule,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Calculate impact scores (handle empty dataframes)
  impact_weights <- list("CRITICAL" = 10, "HIGH" = 5, "MODERATE" = 2, "LOW" = 1)
  
  n_term_score <- if (nrow(n_terminal_issues) > 0) {
    sum(unlist(lapply(n_terminal_issues$Impact_Level, function(x) {
      if (x %in% names(impact_weights)) impact_weights[[x]] else 0
    })))
  } else {
    0
  }
  
  c_term_score <- if (nrow(c_terminal_issues) > 0) {
    sum(unlist(lapply(c_terminal_issues$Impact_Level, function(x) {
      if (x %in% names(impact_weights)) impact_weights[[x]] else 0
    })))
  } else {
    0
  }
  
  # Determine recommendation
  if (n_term_score == 0 && c_term_score == 0) {
    recommendation <- "✅ EXCELLENT: Both termini suitable for tagging. Choose based on other factors."
    preferred_terminus <- "Either"
  } else if (n_term_score < c_term_score) {
    recommendation <- "👍 RECOMMEND N-TERMINAL tagging with flexible linker"
    preferred_terminus <- "N-terminal"
  } else if (c_term_score < n_term_score) {
    recommendation <- "👍 RECOMMEND C-TERMINAL tagging with flexible linker" 
    preferred_terminus <- "C-terminal"
  } else {
    recommendation <- "⚠️ CAUTION: Both termini have issues. Consider internal tagging or protein engineering."
    preferred_terminus <- "Internal/Engineering required"
  }
  
  # Special overrides for critical features
  has_n_critical <- any(n_terminal_issues$Impact_Level == "CRITICAL")
  has_c_critical <- any(c_terminal_issues$Impact_Level == "CRITICAL")
  
  if (has_n_critical && !has_c_critical) {
    recommendation <- "🚫 AVOID N-TERMINAL tagging - Critical features present. Use C-terminal."
    preferred_terminus <- "C-terminal (N-term blocked)"
  } else if (has_c_critical && !has_n_critical) {
    recommendation <- "🚫 AVOID C-TERMINAL tagging - Critical features present. Use N-terminal."
    preferred_terminus <- "N-terminal (C-term blocked)"
  } else if (has_n_critical && has_c_critical) {
    recommendation <- "🚫 AVOID BOTH TERMINI - Critical features at both ends. Internal tagging required."
    preferred_terminus <- "Internal tagging only"
  }
  
  return(list(
    protein_id = protein_id,
    sequence_length = sequence_length,
    n_terminal_issues = n_terminal_issues,
    c_terminal_issues = c_terminal_issues, 
    n_terminal_score = n_term_score,
    c_terminal_score = c_term_score,
    preferred_terminus = preferred_terminus,
    recommendation = recommendation,
    summary = data.frame(
      Terminus = c("N-terminal", "C-terminal"),
      Issues_Found = c(nrow(n_terminal_issues), nrow(c_terminal_issues)),
      Risk_Score = c(n_term_score, c_term_score),
      Suitability = c(
        ifelse(n_term_score == 0, "Excellent", 
               ifelse(n_term_score <= 2, "Good", 
                      ifelse(n_term_score <= 5, "Moderate", "Poor"))),
        ifelse(c_term_score == 0, "Excellent",
               ifelse(c_term_score <= 2, "Good",
                      ifelse(c_term_score <= 5, "Moderate", "Poor")))
      ),
      stringsAsFactors = FALSE
    )
  ))
}

# Function to create a clean summary report that RETURNS text (no printing)
create_tagging_report <- function(impact_analysis) {
  
  # Error handling - check if impact_analysis is valid
  if (is.null(impact_analysis)) {
    return("ERROR: No impact analysis data provided")
  }
  
  if (!is.list(impact_analysis)) {
    return("ERROR: Invalid impact analysis format")
  }
  
  # Check for required components
  required_fields <- c("protein_id", "sequence_length", "recommendation", 
                       "n_terminal_issues", "c_terminal_issues", "preferred_terminus", "summary")
  missing_fields <- setdiff(required_fields, names(impact_analysis))
  if (length(missing_fields) > 0) {
    return(paste("ERROR: Missing required fields:", paste(missing_fields, collapse = ", ")))
  }
  
  # Start building report
  report_lines <- c()
  
  report_lines <- c(report_lines, "═══════════════════════════════════════════════════════════════")
  report_lines <- c(report_lines, "                    PROTEIN TAGGING ANALYSIS REPORT")
  report_lines <- c(report_lines, "═══════════════════════════════════════════════════════════════")
  report_lines <- c(report_lines, "")
  
  # Safe access to fields
  protein_id <- if (!is.null(impact_analysis$protein_id)) impact_analysis$protein_id else "Unknown"
  seq_length <- if (!is.null(impact_analysis$sequence_length)) impact_analysis$sequence_length else "Unknown"
  recommendation <- if (!is.null(impact_analysis$recommendation)) impact_analysis$recommendation else "No recommendation available"
  
  report_lines <- c(report_lines, paste("🔬 PROTEIN:", protein_id))
  report_lines <- c(report_lines, paste("📏 LENGTH:", seq_length, "amino acids"))
  report_lines <- c(report_lines, "")
  
  report_lines <- c(report_lines, "🎯 RECOMMENDATION:")
  report_lines <- c(report_lines, paste("   ", recommendation))
  report_lines <- c(report_lines, "")
  
  # Summary table
  if (!is.null(impact_analysis$summary) && is.data.frame(impact_analysis$summary) && nrow(impact_analysis$summary) > 0) {
    report_lines <- c(report_lines, "📊 TERMINUS COMPARISON:")
    summary_table <- impact_analysis$summary
    for (i in 1:nrow(summary_table)) {
      row <- summary_table[i, ]
      report_lines <- c(report_lines, sprintf("   %-12s | Issues: %d | Risk: %d | %s", 
                                              row$Terminus, row$Issues_Found, 
                                              row$Risk_Score, row$Suitability))
    }
    report_lines <- c(report_lines, "")
  }
  
  # N-terminal issues
  n_issues <- impact_analysis$n_terminal_issues
  if (!is.null(n_issues) && is.data.frame(n_issues) && nrow(n_issues) > 0) {
    report_lines <- c(report_lines, "🅽 N-TERMINAL ISSUES:")
    for (i in 1:nrow(n_issues)) {
      issue <- n_issues[i, ]
      report_lines <- c(report_lines, sprintf("   %s: %s (%s) - %s", 
                                              issue$Impact_Level, issue$Feature_Type, 
                                              issue$Position, issue$Reason))
    }
    report_lines <- c(report_lines, "")
  } else {
    report_lines <- c(report_lines, "✅ N-TERMINAL: No issues detected")
    report_lines <- c(report_lines, "")
  }
  
  # C-terminal issues  
  c_issues <- impact_analysis$c_terminal_issues
  if (!is.null(c_issues) && is.data.frame(c_issues) && nrow(c_issues) > 0) {
    report_lines <- c(report_lines, "🅲 C-TERMINAL ISSUES:")
    for (i in 1:nrow(c_issues)) {
      issue <- c_issues[i, ]
      report_lines <- c(report_lines, sprintf("   %s: %s (%s) - %s",
                                              issue$Impact_Level, issue$Feature_Type,
                                              issue$Position, issue$Reason))
    }
    report_lines <- c(report_lines, "")
  } else {
    report_lines <- c(report_lines, "✅ C-TERMINAL: No issues detected")
    report_lines <- c(report_lines, "")
  }
  
  # Design suggestions
  preferred_terminus <- if (!is.null(impact_analysis$preferred_terminus)) impact_analysis$preferred_terminus else "Unknown"
  
  report_lines <- c(report_lines, "💡 DESIGN SUGGESTIONS:")
  if (preferred_terminus == "N-terminal") {
    report_lines <- c(report_lines, "   • Use N-terminal tagging with 5-15 AA flexible linker (GGGGS repeats)")
    report_lines <- c(report_lines, "   • Consider TEV cleavage site if tag removal needed")
  } else if (preferred_terminus == "C-terminal") {
    report_lines <- c(report_lines, "   • Use C-terminal tagging with 5-15 AA flexible linker")
    report_lines <- c(report_lines, "   • Monitor for effects on membrane insertion if applicable")
  } else if (preferred_terminus == "Either") {
    report_lines <- c(report_lines, "   • Both termini suitable - choose based on downstream applications")
    report_lines <- c(report_lines, "   • N-terminal often better for purification, C-terminal for detection")
  } else {
    report_lines <- c(report_lines, "   • Consider internal tagging approaches (loop insertion)")
    report_lines <- c(report_lines, "   • Evaluate protein engineering to create suitable sites")
    report_lines <- c(report_lines, "   • Test multiple constructs with different linker lengths")
  }
  
  report_lines <- c(report_lines, "")
  report_lines <- c(report_lines, "═══════════════════════════════════════════════════════════════")
  
  # Return as single string with line breaks - THIS IS THE KEY FIX
  final_report <- paste(report_lines, collapse = "\n")
  return(final_report)
}