#    FORCEEPS output import for inventory distribution and species proportion analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective: Import and process FORCEEPS output files for inventory testing analysis.
# This script processes simulation results from different distribution types and species proportions
# to create comprehensive datasets for analyzing the impact of inventory methods on simulation outputs.
#-------------------------------------------------------------------------------

## File cleaning and preprocessing ---------------------------------------------
#------------------------------------------------------------------------------#

# Clean and rename FORCEEPS output files for easier analysis
# Process files from multiple simulation seeds for robust statistical results

for (i in seeds){
    # Define folder containing the raw ForCEEPS output files
    folder <- 
        paste0(base_path, "output-cmd_",
        as.character(i), ".txt/")  # Path to simulation output directory

    # List all files in the output folder
    files <- list.files(folder, full.names = TRUE)

    # Define filename cleaning patterns to standardize file names
    prefix_pattern <- "^retz_act\\.climate_inventaires_"  # Remove climate prefix
    simul_pattern <- "_simulation_([1-9]|10|11)"          # Remove simulation numbers  
    inv_pattern <- "\\.inv"                               # Clean inventory extension

    # Process and rename files for standardized analysis workflow
    for (file in files) {
      filename <- basename(file)
    
      # Apply sequential filename cleaning operations
      new_filename <- filename
      new_filename <- sub(prefix_pattern, "", new_filename)
      new_filename <- sub(simul_pattern, "", new_filename)
      new_filename <- sub(inv_pattern, "_", new_filename)  # Replace .inv with underscore

      # Rename file only if modifications were applied
      if (new_filename != filename) {
        old_path <- file.path(folder, filename)
        new_path <- file.path(folder, new_filename)

        file.rename(old_path, new_path)
      }
    }
}

## Data import and processing functions ----------------------------------------
#------------------------------------------------------------------------------#

# Function to read and parse ForCEEPS simulation output files
# Extracts inventory testing parameters from filename and combines with simulation data
read_forceps_file <- function(seed) {
    # Define path to simulation output folder for this seed
    folder <- paste0(base_path, "output-cmd_", seed, ".txt/")
    # Find productivity scene output files (main ForCEEPS output type for this analysis)
    files <- list.files(folder, pattern = "_productivityScene\\.txt$", full.names = TRUE)
    
    # Return NULL if no files found for this seed
    if (length(files) == 0) return(NULL)
    
    # Process each output file and extract metadata from filename
    map_dfr(files, function(f) {
        # Extract inventory testing parameters from filename
        fname <- basename(f)
        # Expected filename pattern: {distribution}_{species_proportion}_{repetition}_productivityScene.txt
        m <- str_match(fname, "^(.+?)_([a-zA-Z0-9]+)_([0-9]+)_productivityScene\\.txt$")
        if (is.na(m[1,1])) return(NULL)
        
        # Parse filename components
        distribution <- m[1,2]        # Distribution type (unif, normal_4, etc.)
        species_proportion <- m[1,3]  # Species proportion method (fixe, random)
        repetition <- as.integer(m[1,4]) + 1 # +1 because file numbering starts from 0
        
        # Read ForCEEPS simulation data (skip metadata header lines)
        df <- read_delim(f, delim = "\t", skip = 11, show_col_types = FALSE)
        
        # Add inventory testing metadata to simulation results
        df <- df %>%
            mutate(
                seed = seed,                           # Random seed for replication tracking
                distribution = distribution,           # Tree diameter distribution type
                species_proportion = species_proportion, # Species proportion sampling method
                repetition = repetition                # Repetition number for random methods
            )
        df
    })
}

## Final data compilation and export -------------------------------------------
#------------------------------------------------------------------------------#

# Load and combine data from all simulation seeds using the parsing function
# This creates a comprehensive dataset across all inventory testing combinations
forceps_data <- map_dfr(seeds, read_forceps_file)

# Clean column names by removing ForCEEPS header symbols (# characters)
colnames(forceps_data) <- gsub("#", "", colnames(forceps_data))

# Save the complete consolidated dataset for statistical analysis
# This file contains all combinations of:
# - Distribution types (uniform, normal distributions, mean, min, max)
# - Species proportion methods (fixed from data vs random sampling)  
# - Multiple repetitions for robust testing
# - All random seeds for replication
save(forceps_data, file = "data/forceeps_output/inventory_productivityScene.RData")