# Function to extract subcellular location from data.frame comments structure
extract_subcellular_location_df <- function(data) {
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
    
    # Find subcellular location rows
    subcell_rows <- comments_df[comments_df$commentType == "SUBCELLULAR LOCATION", ]
    
    if (nrow(subcell_rows) == 0) {
      return("No subcellular location information")
    }
    
    # Method 1: Try to extract from texts column (if not NULL)
    if ("texts" %in% names(subcell_rows)) {
      for (i in 1:nrow(subcell_rows)) {
        texts_data <- subcell_rows$texts[[i]]
        if (!is.null(texts_data) && is.list(texts_data) && length(texts_data) > 0) {
          text_values <- c()
          for (j in seq_along(texts_data)) {
            if (is.list(texts_data[[j]]) && "value" %in% names(texts_data[[j]])) {
              text_values <- c(text_values, texts_data[[j]]$value)
            }
          }
          if (length(text_values) > 0) {
            return(paste(text_values, collapse = "; "))
          }
        }
      }
    }
    
    # Method 2: Check if there are other columns that might contain location info
    # Look for columns that might contain subcellular location data
    potential_cols <- c("subcellularLocations", "locations", "location")
    
    for (col in potential_cols) {
      if (col %in% names(subcell_rows)) {
        for (i in 1:nrow(subcell_rows)) {
          col_data <- subcell_rows[[col]][[i]]
          if (!is.null(col_data)) {
            # Try to extract meaningful info from this column
            if (is.list(col_data) && length(col_data) > 0) {
              location_values <- c()
              for (j in seq_along(col_data)) {
                item <- col_data[[j]]
                if (is.list(item)) {
                  # Look for location/value pairs
                  if ("location" %in% names(item) && is.list(item$location) && "value" %in% names(item$location)) {
                    location_values <- c(location_values, item$location$value)
                  }
                  # Look for direct value
                  if ("value" %in% names(item)) {
                    location_values <- c(location_values, item$value)
                  }
                }
              }
              if (length(location_values) > 0) {
                return(paste(location_values, collapse = "; "))
              }
            }
          }
        }
      }
    }
    
    # Method 3: Check the note column for ECO codes or additional info
    if ("note" %in% names(subcell_rows)) {
      notes <- subcell_rows$note[!is.na(subcell_rows$note) & subcell_rows$note != "NULL"]
      if (length(notes) > 0) {
        # The note might contain ECO evidence codes, but not the actual location
        return(paste("Evidence available:", paste(notes, collapse = "; ")))
      }
    }
    
    # If we get here, we found a subcellular location comment but couldn't extract the data
    return("Subcellular location comment found but data not accessible")
    
  }, error = function(e) {
    return(paste("Error extracting subcellular location:", e$message))
  })
}

# Enhanced debugging function to explore the subcellular location row in detail
debug_subcellular_row <- function(uniprot_id) {
  tryCatch({
    # Get data
    url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
    response <- url(url)
    on.exit(close(response))
    json_text <- readLines(response, warn = FALSE)
    data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
    
    comments_df <- data$comments
    subcell_rows <- comments_df[comments_df$commentType == "SUBCELLULAR LOCATION", ]
    
    cat("=== SUBCELLULAR LOCATION ROW DETAILS ===\n")
    cat("Number of subcellular location rows:", nrow(subcell_rows), "\n")
    
    if (nrow(subcell_rows) > 0) {
      for (i in 1:nrow(subcell_rows)) {
        cat("\n--- Row", i, "---\n")
        cat("Column names:", names(subcell_rows), "\n")
        
        # Check each column
        for (col_name in names(subcell_rows)) {
          col_value <- subcell_rows[[col_name]][[i]]
          cat(col_name, ":", class(col_value), "\n")
          
          if (col_name == "texts" && !is.null(col_value)) {
            cat("  texts structure:\n")
            print(str(col_value))
          }
          
          if (!is.null(col_value) && col_value != "NULL" && length(col_value) > 0) {
            cat("  Value preview:", paste(head(as.character(col_value), 3), collapse = ", "), "\n")
          }
        }
      }
    }
    
    return(subcell_rows)
    
  }, error = function(e) {
    cat("Error in debug:", e$message, "\n")
    return(NULL)
  })
}

# Test the extraction
test_subcellular_extraction <- function(uniprot_id = "P01308") {
  cat("Testing subcellular location extraction for:", uniprot_id, "\n")
  
  # Get data
  url <- paste0("https://rest.uniprot.org/uniprotkb/", uniprot_id, ".json")
  response <- url(url)
  on.exit(close(response))
  json_text <- readLines(response, warn = FALSE)
  data <- jsonlite::fromJSON(paste(json_text, collapse = ""))
  
  # Extract subcellular location
  location <- extract_subcellular_location_df(data)
  cat("Result:", location, "\n")
  
  return(location)
}

# Run detailed debugging
# debug_subcellular_row("P01308")