# functions/structure_prediction_forge.R
# ===========================================================================
# ESM3 Forge API Structure Prediction
# ===========================================================================

library(httr)
library(jsonlite)

predict_structure_esm3 <- function(sequence, construct_name = "protein", forge_token = NULL) {
  
  cat("\n=== ESM3 Structure Prediction ===\n")
  cat("Construct:", construct_name, "\n")
  cat("Sequence length:", nchar(sequence), "aa\n")
  
  # Check for token
  if (is.null(forge_token)) {
    forge_token <- Sys.getenv("ESM_FORGE_TOKEN")
  }
  
  if (is.null(forge_token) || length(forge_token) == 0 || forge_token == "") {
    return(list(
      success = FALSE,
      error = paste0(
        "ESM Forge API token required.\n",
        "Get token at: https://forge.evolutionaryscale.ai"
      )
    ))
  }
  
  cat("Using token:", substr(forge_token, 1, 10), "...\n")
  
  # Validate sequence
  if (is.null(sequence) || nchar(sequence) == 0) {
    return(list(success = FALSE, error = "Empty sequence"))
  }
  
  valid_aa <- "ACDEFGHIKLMNPQRSTVWY"
  if (grepl(paste0("[^", valid_aa, "]"), sequence)) {
    return(list(success = FALSE, error = "Invalid amino acids in sequence"))
  }
  
  if (nchar(sequence) > 2000) {
    return(list(
      success = FALSE,
      error = paste0("Sequence too long (", nchar(sequence), " aa). Limit is ~2000 aa.")
    ))
  }
  
  tryCatch({
    
    cat("Submitting to ESM3 Forge API...\n")
    
    # ESM3 endpoint
    api_url <- "https://forge.evolutionaryscale.ai/api/v1/fold"
    
    # Request body
    request_body <- list(
      sequence = sequence,
      name = construct_name
    )
    
    # Submit
    response <- httr::POST(
      api_url,
      httr::add_headers(
        "Authorization" = paste("Bearer", forge_token),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "json",
      httr::timeout(300)
    )
    
    status <- httr::status_code(response)
    cat("Response status:", status, "\n")
    
    if (status == 401 || status == 403) {
      return(list(
        success = FALSE,
        error = "Invalid token. Check https://forge.evolutionaryscale.ai"
      ))
    }
    
    if (status == 429) {
      return(list(success = FALSE, error = "Rate limit exceeded"))
    }
    
    if (status != 200 && status != 201) {
      error_text <- httr::content(response, as = "text", encoding = "UTF-8")
      return(list(
        success = FALSE,
        error = paste0("API error (", status, "): ", error_text)
      ))
    }
    
    # Parse response
    result <- httr::content(response, as = "parsed")
    
    cat("✓ Structure prediction complete!\n")
    
    # Extract coordinates and confidence
    if (is.null(result$coordinates)) {
      return(list(success = FALSE, error = "No coordinates in response"))
    }
    
    # Extract pLDDT scores
    plddt_scores <- if (!is.null(result$plddt)) unlist(result$plddt) else NULL
    
    if (!is.null(plddt_scores) && length(plddt_scores) > 0) {
      mean_plddt <- mean(plddt_scores, na.rm = TRUE)
      confidence_level <- if(mean_plddt > 90) "Very High"
      else if(mean_plddt > 70) "High"
      else if(mean_plddt > 50) "Moderate"
      else "Low"
      cat("Mean pLDDT:", round(mean_plddt, 1), "(", confidence_level, ")\n")
    } else {
      mean_plddt <- NA
      confidence_level <- "Unknown"
      plddt_scores <- rep(50, nchar(sequence))  # Default values
    }
    
    # Convert coordinates to PDB format
    cat("Converting to PDB format...\n")
    pdb_text <- convert_esm3_coords_to_pdb(
      coordinates = result$coordinates,
      sequence = sequence,
      plddt_scores = plddt_scores,
      construct_name = construct_name  # FIX: Pass construct_name here
    )
    
    # Save PDB
    pdb_dir <- "structure_predictions"
    if (!dir.exists(pdb_dir)) {
      dir.create(pdb_dir, recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pdb_file <- file.path(pdb_dir, paste0(construct_name, "_esm3_", timestamp, ".pdb"))
    writeLines(pdb_text, pdb_file)
    
    cat("✓ PDB saved:", pdb_file, "\n")
    
    # Parse saved PDB to extract final scores
    pdb_lines <- strsplit(pdb_text, "\n")[[1]]
    atom_lines <- pdb_lines[grepl("^ATOM", pdb_lines)]
    ca_lines <- atom_lines[grepl(" CA ", atom_lines)]
    
    if (length(ca_lines) > 0) {
      ca_plddt <- as.numeric(substr(ca_lines, 61, 66))
      final_mean_plddt <- mean(ca_plddt, na.rm = TRUE)
      
      n <- length(ca_plddt)
      n_term_plddt <- mean(ca_plddt[1:min(50, n)], na.rm = TRUE)
      c_term_plddt <- mean(ca_plddt[max(1, n-49):n], na.rm = TRUE)
      
      return(list(
        success = TRUE,
        pdb_text = pdb_text,
        pdb_file = pdb_file,
        plddt_scores = ca_plddt,
        mean_plddt = final_mean_plddt,
        n_term_plddt = n_term_plddt,
        c_term_plddt = c_term_plddt,
        confidence_level = if(final_mean_plddt > 90) "Very High"
        else if(final_mean_plddt > 70) "High"
        else if(final_mean_plddt > 50) "Moderate"
        else "Low",
        construct_name = construct_name,
        sequence_length = nchar(sequence),
        method = "ESM3 Forge"
      ))
    } else {
      return(list(
        success = TRUE,
        pdb_text = pdb_text,
        pdb_file = pdb_file,
        construct_name = construct_name,
        sequence_length = nchar(sequence),
        method = "ESM3 Forge",
        note = "No CA atoms found"
      ))
    }
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    return(list(
      success = FALSE,
      error = paste0("ESM3 error: ", conditionMessage(e))
    ))
  })
}

# ===========================================================================
# HELPER: Convert ESM3 coordinates to PDB format
# ===========================================================================

convert_esm3_coords_to_pdb <- function(coordinates, sequence, plddt_scores = NULL, construct_name = "protein") {
  
  # ESM3 coordinates format: list or array
  # Convert to R array if needed
  if (is.list(coordinates)) {
    # Flatten nested list structure
    coords_array <- array(unlist(coordinates), 
                          dim = c(length(coordinates), 
                                  length(coordinates[[1]]), 
                                  3))
  } else {
    coords_array <- coordinates
  }
  
  n_residues <- dim(coords_array)[1]
  n_atoms_per_res <- dim(coords_array)[2]
  
  # Standard backbone atoms
  atom_names <- c("N", "CA", "C", "O")
  if (n_atoms_per_res > 4) {
    # Add placeholders for side-chain atoms
    atom_names <- c(atom_names, paste0("CB", seq_len(n_atoms_per_res - 4)))
  } else {
    atom_names <- atom_names[1:n_atoms_per_res]
  }
  
  # Amino acid mapping
  aa_3letter <- c(
    A="ALA", C="CYS", D="ASP", E="GLU", F="PHE",
    G="GLY", H="HIS", I="ILE", K="LYS", L="LEU",
    M="MET", N="ASN", P="PRO", Q="GLN", R="ARG",
    S="SER", T="THR", V="VAL", W="TRP", Y="TYR"
  )
  
  # Parse sequence
  if (length(sequence) == 1) {
    sequence <- strsplit(sequence, "")[[1]]
  }
  
  if (length(sequence) != n_residues) {
    warning("Sequence length doesn't match coordinates")
    sequence <- rep("X", n_residues)
  }
  
  # Map to 3-letter codes
  resnames <- ifelse(toupper(sequence) %in% names(aa_3letter),
                     unname(aa_3letter[toupper(sequence)]),
                     "UNK")
  
  # Prepare B-factors (pLDDT scores)
  if (is.null(plddt_scores) || length(plddt_scores) != n_residues) {
    b_factors <- matrix(50, nrow = n_residues, ncol = n_atoms_per_res)
  } else {
    b_factors <- matrix(plddt_scores, nrow = n_residues, ncol = n_atoms_per_res)
  }
  
  # Generate PDB lines
  pdb_lines <- character()
  atom_number <- 1
  
  # PDB ATOM format
  fmt <- "ATOM  %5d %-4s %3s A%4d    %8.3f%8.3f%8.3f%6.2f%6.2f          %2s"
  
  for (i in 1:n_residues) {
    for (j in 1:n_atoms_per_res) {
      xyz <- coords_array[i, j, ]
      
      # Skip if coordinates are NA
      if (any(is.na(xyz))) next
      
      atom_name <- atom_names[j]
      element <- substr(atom_name, 1, 1)
      
      line <- sprintf(fmt,
                      atom_number,
                      sprintf("%-4s", atom_name),
                      resnames[i],
                      i,
                      xyz[1], xyz[2], xyz[3],
                      1.00,  # occupancy
                      b_factors[i, j],
                      sprintf("%2s", element))
      
      pdb_lines <- c(pdb_lines, line)
      atom_number <- atom_number + 1
    }
  }
  
  # Add TER and END
  pdb_lines <- c(
    paste0("HEADER    ", construct_name, "    ", format(Sys.Date(), "%d-%b-%y")),
    pdb_lines,
    sprintf("TER   %5d      %3s A%4d", atom_number, tail(resnames, 1), n_residues),
    "END"
  )
  
  return(paste(pdb_lines, collapse = "\n"))
}

# ===========================================================================
# EXAMPLE USAGE
# ===========================================================================

# Set your ESM3 token
Sys.setenv(ESM_FORGE_TOKEN = "your_token_here")

# Test with insulin
sequence <- "MALWMRLLPLLALLALWGPDPAAAFVNQHLCGSHLVEALYLVCGERGFFYTPKTRREAEDLQVGQVELGGGPGAGSLQPLALEGSLQKRGIVEQCCTSICSLYQLENYCN"

result <- predict_structure_esm3(
  sequence = sequence,
  construct_name = "insulin_test"
)

if (result$success) {
  print(paste("✓ Success! File:", result$pdb_file))
  print(paste("Mean pLDDT:", round(result$mean_plddt, 1)))
  print(paste("N-term pLDDT:", round(result$n_term_plddt, 1)))
  print(paste("C-term pLDDT:", round(result$c_term_plddt, 1)))
}
