# Corrected function to extract subunit structure from nested data.frame structure
extract_subunit_structure_corrected <- function(data) {
  tryCatch({
    # Check if data and comments exist
    if (is.null(data) || !"comments" %in% names(data)) {
      return("Not available")
    }
    
    comments_df <- data$comments
    
    # Check if comments is a data.frame
    if (!is.data.frame(comments_df)) {
      return("Comments not in expected format")
    }
    
    # Find subunit structure rows
    subunit_rows <- comments_df[comments_df$commentType == "SUBUNIT", ]
    
    if (nrow(subunit_rows) == 0) {
      return("No subunit information available")
    }
    
    # Extract text from subunit comments - handle nested data.frame structure
    subunit_texts <- c()
    
    for (i in 1:nrow(subunit_rows)) {
      # The texts column contains data.frames, not lists
      if ("texts" %in% names(subunit_rows)) {
        texts_data <- subunit_rows$texts[[i]]
        
        # Check if texts_data is a data.frame (as shown in your output)
        if (!is.null(texts_data) && is.data.frame(texts_data)) {
          # Look for a 'value' column in the nested data.frame
          if ("value" %in% names(texts_data)) {
            values <- texts_data$value
            # Remove any NULL or empty values
            values <- values[!is.na(values) & values != "" & values != "NULL"]
            if (length(values) > 0) {
              subunit_texts <- c(subunit_texts, values)
            }
          } else {
            # If no 'value' column, check what columns exist
            cat("Nested data.frame columns:", paste(names(texts_data), collapse = ", "), "\n")
            # Try the first column if it exists
            if (ncol(texts_data) > 0) {
              first_col_values <- texts_data[, 1]
              first_col_values <- first_col_values[!is.na(first_col_values) & 
                                                     first_col_values != "" & 
                                                     first_col_values != "NULL"]
              if (length(first_col_values) > 0) {
                subunit_texts <- c(subunit_texts, first_col_values)
              }
            }
          }
        }
        # Also handle the case where it might still be a list (other proteins)
        else if (!is.null(texts_data) && is.list(texts_data) && length(texts_data) > 0) {
          for (j in seq_along(texts_data)) {
            if (is.list(texts_data[[j]]) && "value" %in% names(texts_data[[j]])) {
              subunit_texts <- c(subunit_texts, texts_data[[j]]$value)
            }
          }
        }
      }
    }
    
    if (length(subunit_texts) > 0) {
      return(paste(subunit_texts, collapse = "; "))
    } else {
      return("Subunit information found but text not accessible")
    }
    
  }, error = function(e) {
    return(paste("Error extracting subunit structure:", e$message))
  })
}

# Function to examine the exact structure of the texts column
examine_subunit_texts <- function(data) {
  tryCatch({
    comments_df <- data$comments
    subunit_rows <- comments_df[comments_df$commentType == "SUBUNIT", ]
    
    cat("=== EXAMINING SUBUNIT TEXTS STRUCTURE ===\n")
    cat("Number of subunit rows:", nrow(subunit_rows), "\n")
    
    if (nrow(subunit_rows) > 0) {
      for (i in 1:nrow(subunit_rows)) {
        cat("\n--- Subunit Row", i, "---\n")
        texts_data <- subunit_rows$texts[[i]]
        
        cat("texts class:", class(texts_data), "\n")
        cat("texts length/dim:", 
            if(is.data.frame(texts_data)) paste("rows:", nrow(texts_data), "cols:", ncol(texts_data))
            else length(texts_data), "\n")
        
        if (is.data.frame(texts_data)) {
          cat("Column names:", paste(names(texts_data), collapse = ", "), "\n")
          cat("Data preview:\n")
          print(texts_data)
        } else if (is.list(texts_data)) {
          cat("List structure:\n")
          print(str(texts_data, max.level = 2))
        } else {
          cat("Direct value:", texts_data, "\n")
        }
      }
    }
    
    return(subunit_rows)
    
  }, error = function(e) {
    cat("Error examining structure:", e$message, "\n")
    return(NULL)
  })
}

# Test the corrected extraction while in browser mode
test_corrected_extraction <- function(data) {
  cat("Testing corrected subunit extraction...\n")
  result <- extract_subunit_structure_corrected(data)
  cat("Extraction result:", result, "\n")
  return(result)
}

# While you're in browser mode, you can run:
# examine_subunit_texts(data)  # This will show us the exact structure
# test_corrected_extraction(data)  # This will test the extraction