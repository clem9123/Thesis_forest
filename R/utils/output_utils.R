# ===============================================================================
# FOREST SIMULATION OUTPUT PROCESSING UTILITIES
# ===============================================================================
#
# Purpose: Process and aggregate forest simulation output files from FORCEEPS model
# Author: Clementine de Montgolfier
# Last Modified: 19 août 2025
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
# Description:
# This file contains utilities for processing forest simulation output files.
# The main function `import_output_scene()` reads multiple simulation files,
# extracts metadata (plot IDs, simulation scenarios), and combines all data
# into a single aggregated dataset.
#
# Key Features:
# - Automatically discovers simulation files in directory structure
# - Extracts plot IDs and simulation scenarios from filenames
# - Handles different output types ("mean" vs "productivityScene")
# - Cleans and standardizes column names
# - Combines all data into a single data frame
#
# File Structure Expected:
# base_path/
#   ├── output-cmd_1.txt/
#   │   ├── retz_act.climate_inventories_RETZ_XX_XX.inv_simulation_1productivityScene.txt
#   │   └── [more simulation files...]
#   ├── output-cmd_2.txt/
#   │   └── [simulation files...]
#   └── [more output directories...]
#
# ===============================================================================

#' Process Forest Simulation Output Files (With Plot ID and Scenario Extraction)
#'
#' This function loads and aggregates simulation outputs from forest productivity scene files
#' with repeated simulations. It processes multiple simulation files, extracts plot IDs and
#' simulation scenarios from filenames, and combines all data into a single dataset.
#' Each simulation file is processed individually and added to the final aggregated dataset.
#'
#' @param output_name Character. Type of output to process, either "mean" or "productivityScene".
#'                    Determines how many lines to skip when reading files (1 for "mean", 14 for others).
#' @param base_path Character. Path to the base directory containing simulation output folders.
#'                  Should contain subdirectories named like "output-cmd_XX.txt".
#' @param output_file Character. File path to save the final aggregated dataset (as .RData).
#'                    Currently not used in the function but kept for future implementation.
#'
#' @return Returns a `data.frame` containing all processed simulation data with added plot_id and scenario columns.
#'         The function combines data from all found simulation files into a single dataset.
#' @export
import_output_scene <- function(
  output_name = "productivityScene",  # Default output type to process
  base_path,                          # Required: path to directory containing output folders
  output_file                        # File path for saving results (currently not implemented)
) {

  # ===== STEP 1: FILE DISCOVERY =====
  # Find all output directories that match the pattern "output-cmd_XX.txt" 
  # These directories contain the simulation result files
  output_dirs <- dir_ls(base_path, regexp = "output-cmd_\\d+\\.txt$", type = "directory")

  # ===== STEP 2: BUILD FILE INVENTORY =====
  # For each output directory, find all simulation files and extract metadata
  all_sim_files <- map_dfr(output_dirs, function(dir_path) {
    # Find simulation files matching the specific naming pattern:
    # Format: Retz_act.climate_inventories_RETZ_XX_XX.inv_simulation_XX[output_name].txt
    sim_files <- dir_ls(
      dir_path,
      regexp = paste0("retz_act\\.climate_inventories_RETZ_\\d+_\\d+\\.inv_simulation_\\d+", output_name, "\\.txt$"),
      type = "file"
    )
    
    # Create a tibble with file information and extracted metadata
    tibble(
      file_path = sim_files,                                    # Full path to each file
      folder = basename(dir_path),                              # Name of containing folder
      plot_id = str_extract(sim_files, "RETZ_[0-9_]+"),       # Extract plot ID (e.g., "RETZ_12_34")
      simul = str_extract(sim_files, "simulation_\\d+")        # Extract simulation number (e.g., "simulation_1")
    )
  }) %>%
    # Add a unique identifier for each simulation file
    mutate(simulation_id = row_number())

  # ===== STEP 3: INITIALIZE DATA STORAGE =====
  # Create empty data frame to store all processed simulation data
  aggregated_data <- data.frame()

  # ===== STEP 4: PROCESS EACH SIMULATION FILE =====
  # Loop through each simulation file and process it individually
  for (i in seq_len(nrow(all_sim_files))) {

    # Extract file information for current iteration
    file_path <- all_sim_files$file_path[i]  # Path to current simulation file
    plot_id <- all_sim_files$plot_id[i]      # Plot identifier (extracted from filename)
    scenario <- all_sim_files$simul[i]       # Simulation scenario (extracted from filename)

    # ===== READ AND PROCESS CURRENT FILE =====
    # Read the simulation file, skipping header lines as determined earlier
    header_lines <- readLines(file_path, warn = FALSE)
    skip <- which(grepl("^#date", header_lines))[1] - 1
    
    df <- read_table(file_path, show_col_types = FALSE, skip = skip)
    
    # Add metadata columns to identify the source of this data
    df$plot_id = plot_id      # Add plot identifier
    df$scenario = scenario    # Add simulation scenario

    # Safety check: if file reading failed, skip this iteration
    if (is.null(df)) return(NULL)

    # ===== CLEAN COLUMN NAMES =====
    # Standardize column names for consistency across all files
    colnames(df)[colnames(df) == "#date"] <- "date"  # Remove # from date column
    colnames(df) <- gsub("/", ".", colnames(df))     # Replace / with . in column names
    colnames(df) <- gsub("[()]", ".", colnames(df))   # Remove parentheses from column names
    colnames(df) <- gsub("\\.$", "", colnames(df))     # remove last "." of colnames if there is

    # ===== FORMAT FOR COMPLETE FILE =====
    # only usable if nrep == 1
    if(output_name == "complete") {
      # ===== FORMAT FOR COMPLETE FILE =====
      # Apply transformations to add diameter class and adjust date for cut events
      df <- df %>%
        mutate(cut = row_number() == 2) %>%
        mutate(date_init = date) %>%
        mutate(date = ifelse(cut, date + 0.1, date)) %>%
        mutate(cut = as.logical(cut)) %>%
        mutate(
          classe_diam = case_when(
        dbh.cm < 7.5  ~ "R",
        dbh.cm < 17.5 ~ "I",
        dbh.cm < 27.5 ~ "P",
        dbh.cm < 47.5 ~ "M",
        dbh.cm < 67.5 ~ "G",
        dbh.cm >= 67.5 ~ "T",
        TRUE ~ NA_character_
          )
        )
      df$classe_diam <- factor(df$classe_diam,
              levels = c("R", "I", "P", "M", "G", "T"),
              ordered = TRUE)
    }

    # ===== COMBINE DATA =====
    # Add current file's data to the growing aggregated dataset
    aggregated_data <- bind_rows(aggregated_data, df)
  }

  # ===== STEP 5: RETURN RESULTS =====
  # Return the complete aggregated dataset containing all processed simulation files
  return(aggregated_data)
}
