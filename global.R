# global.R
# Shared setup & helpers for Protein Design Example app
first = F
if(first == T){
  # Please run this line before running the app the setup the package environment
  #renv::restore()
  renv::init()
}
source('renv/activate.R')

source('functions/packages.R')

source('functions/uniprot functions.R')
source('functions/uniprot_field_functions.R')
#source('functions/get_uniprot_fields.R')
source('functions/AlphaFold_functions.R')
source('functions/uniprot_features.R')
source('functions/unique_features_sub_1.6.R')
source('functions/pdb_vis_functions.R')
source('functions/terminal_impact.R')
source('functions/pdb_viewer_function.R')
source('functions/uniprot_feature_type_description.R')
source('functions/uniprot_subcellular_location_1.0.R')
source('functions/uniprot_subunit_1.0.R')
#source('uniprot_subunit_1.1.R')
#source('uniprot_subunit_1.3.R')
source('functions/secreted_functions_1.0.R')
source('functions/dynamic_filter_function_1.5.R')
source('functions/dynamic_feature_filter_1.2.R')
source('functions/proteinReport_functions_1.0.R')

# Default example file bundled with the project
#example_file <- "Data/Protein Design Example.xlsx"
example_file <- "../InputData/add_uniprot_data_2025_1018_Mismatches.xlsx"

#vector = read_excel("Data/Vectors.xlsx")
vector = read_excel("../InputData/Vectors.xlsx")

colnames(vector)
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
