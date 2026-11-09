#' Function to download and Import the Latest SDWA Dataset
#'
#' Downloads the latest Safe Drinking Water Act (SDWA) dataset from the EPA's
#' ECHO system, unzips it to a local directory, and returns a specified CSV
#' file as a dataframe. Subsequent calls skip the download and unzip steps
#' unless `overwrite = TRUE` is set.
#'
#' @param dest_dir Character. Path to the directory where the zip file and
#'   unzipped contents will be saved. Created recursively if it does not exist.
#'   Defaults to `"data/all/Water_Supply/"`.
#' @param file Character. Name of the CSV file to read from the unzipped
#'   directory. If the file does not exist, an error is thrown listing all
#'   available CSVs. Defaults to `"SDWA_FACILITIES.csv"`. See Details for
#'   all available files.
#' @param overwrite Logical. If `TRUE`, re-downloads the zip and re-unzips
#'   even if they already exist locally. Useful for refreshing to the latest
#'   EPA release. Defaults to `FALSE`.
#' @param skip_if_extracted Logical. If `TRUE`, skips downloading and unzipping
#'   if the requested CSV file already exists in the unzipped directory. Takes
#'   precedence over `overwrite`. Defaults to `TRUE`.
#'
#' @return A tibble containing the contents of the requested CSV file.
#'
#' @details
#' The zip contains 11 CSVs covering different aspects of the SDWA program:
#' \itemize{
#'   \item `SDWA_FACILITIES.csv` — Physical facility locations associated with public water systems
#'   \item `SDWA_PUB_WATER_SYSTEMS.csv` — Core public water system attributes and contact information
#'   \item `SDWA_VIOLATIONS_ENFORCEMENT.csv` — Drinking water violations and associated enforcement actions
#'   \item `SDWA_EVENTS_MILESTONES.csv` — Compliance schedule events and milestone tracking
#'   \item `SDWA_SITE_VISITS.csv` — Inspection and site visit records
#'   \item `SDWA_LCR_SAMPLES.csv` — Lead and copper rule sample results
#'   \item `SDWA_PN_VIOLATION_ASSOC.csv` — Public notice to violation associations
#'   \item `SDWA_SERVICE_AREAS.csv` — Geographic service area definitions for water systems
#'   \item `SDWA_GEOGRAPHIC_AREAS.csv` — Geographic area classifications for water systems
#'   \item `SDWA_REF_ANSI_AREAS.csv` — ANSI geographic area reference codes
#'   \item `SDWA_REF_CODE_VALUES.csv` — Reference table for coded values used across other tables
#' }
#'
#' @examples
#' \dontrun{
#' # Default — returns SDWA_FACILITIES.csv
#' facilities <- get_sdwa()
#'
#' # Return a different file
#' violations <- get_sdwa(file = "SDWA_VIOLATIONS_ENFORCEMENT.csv")
#'
#' # Force re-download of the latest release
#' facilities <- get_sdwa(overwrite = TRUE)
#'
#' # Save to a custom directory
#' facilities <- get_sdwa(dest_dir = "data/epa/sdwa/")
#' }
#'
#' @seealso \url{https://echo.epa.gov/tools/data-downloads/sdwa-download-summary}
#'   for full documentation of the SDWA dataset and field definitions.
#'
#' @importFrom httr GET write_disk progress
#' @importFrom readr read_csv
#'
#' @export
get_sdwa <- function(
    dest_dir          = "data/all/Water_Supply/",
    file              = "SDWA_FACILITIES.csv",
    overwrite         = FALSE,
    skip_if_extracted = TRUE
) {
  
  # --- Input validation ---
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("Created directory: ", dest_dir)
  }
  
  zip_path    <- file.path(dest_dir, "SDWA_latest_downloads.zip")
  unzip_dir   <- file.path(dest_dir, "SDWA_latest_downloads")
  target_file <- file.path(unzip_dir, file)
  
  # --- Skip everything if target CSV already exists ---
  if (skip_if_extracted && file.exists(target_file) && !overwrite) {
    message("'", file, "' already exists locally, skipping download and unzip.",
            "\nUse `overwrite = TRUE` to re-download or `skip_if_extracted = FALSE` to re-unzip only.")
  } else {
    
    # --- Download (skip if already exists and overwrite = FALSE) ---
    if (!file.exists(zip_path) || overwrite) {
      message("Downloading SDWA dataset (this may take a while)...")
      httr::GET(
        "https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip",
        httr::write_disk(zip_path, overwrite = TRUE),
        httr::progress()
      )
      message("Download complete.")
    } else {
      message("Zip already exists, skipping download. Use `overwrite = TRUE` to re-download.")
    }
    
    # --- Unzip ---
    if (!dir.exists(unzip_dir) || overwrite) {
      message("Unzipping...")
      unzip(zip_path, exdir = unzip_dir)
      message("Unzipped to: ", unzip_dir)
    } else {
      message("Unzip directory already exists, skipping. Use `overwrite = TRUE` to re-unzip.")
    }
  }
  
  # --- Check requested file exists ---
  if (!file.exists(target_file)) {
    available <- list.files(unzip_dir, pattern = "\\.csv$")
    stop(
      "File '", file, "' not found in unzipped directory.\n",
      "Available CSVs:\n", paste(" -", available, collapse = "\n")
    )
  }
  
  # --- Read and return ---
  message("Reading: ", target_file)
  readr::read_csv(target_file, show_col_types = FALSE)
}
