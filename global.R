# global.R
# Shared setup & helpers for Protein Design Example app

library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(stringr) 
library(httr)
library(jsonlite)
library(openxlsx)

source('uniprot functions.R')
source('uniprot_field_functions.R')
source('get_uniprot_fields.R')
source('AlphaFold_functions.R')

# Default example file bundled with the project
example_file <- "Data/Protein Design Example.xlsx"

# Helper: safely read an Excel sheet, returning a tibble or NULL on error
read_excel_safe <- function(path, sheet) {
  tryCatch({
    read_excel(path, sheet = sheet)
  }, error = function(e) NULL)
}

# Helper: list sheets in a workbook (character vector) or NULL
excel_sheets_safe <- function(path) {
  tryCatch({
    excel_sheets(path)
  }, error = function(e) NULL)
}

# Helper: pick a default sheet
# Priority: a sheet named "Proteins" (case-insensitive), else index 3, else 1
pick_default_sheet <- function(sheets_chr) {
  if (is.null(sheets_chr) || length(sheets_chr) == 0) return(NULL)
  i_proteins <- which(tolower(sheets_chr) == "proteins")
  if (length(i_proteins)) return(sheets_chr[i_proteins[1]])
  if (length(sheets_chr) >= 3) return(sheets_chr[3])
  return(sheets_chr[1])
}

# Helper: auto-detect a UniProt-like column (returns name or NA_character_)
detect_uniprot_col <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(NA_character_)
  cols <- names(df)
  hit <- cols[grepl("^(uniprot|accession|acc)$|uniprot|accession|acc|protein.?id",
                    cols, ignore.case = TRUE)]
  if (length(hit)) hit[[1]] else NA_character_
}
