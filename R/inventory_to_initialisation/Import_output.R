#    FORCEEPS output import for productivity scene analysis
#         Author: Clementine de Montgolfier (June 2024)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective: Import and process FORCEEPS output files for productivity scene analysis
# and create a comprehensive dataset (repetition_productivityScene.RData) for further analyses
#-------------------------------------------------------------------------------

## Import mean output ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Clean and rename FORCEEPS output files for analysis
# Process files from multiple simulation seeds for robust results

for (i in seeds){
    # Define folder containing the output files
    folder <- 
        paste0(base_path, "output-cmd_",
        as.character(i), ".txt/")  # Replace with your path

    # List all files in the folder
    files <- list.files(folder, full.names = TRUE)

    # Define patterns to clean from filenames
    prefix_pattern <- "^retz_act\\.climate_inventaires_"
    simul_pattern <- "_simulation_([1-9]|10|11)"
    inv_pattern <- "\\.inv"

    # Process and rename files for easier analysis
    for (file in files) {
      filename <- basename(file)
    
      # Apply filename cleaning
      new_filename <- filename
      new_filename <- sub(prefix_pattern, "", new_filename)
      new_filename <- sub(simul_pattern, "", new_filename)
      new_filename <- sub(inv_pattern, "_", new_filename)  # Replace .inv with _

      # Rename file if changes were made
      if (new_filename != filename) {
        old_path <- file.path(folder, filename)
        new_path <- file.path(folder, new_filename)

        file.rename(old_path, new_path)
      }
    }
}


# Function to read and parse simulation output files
read_forceps_file <- function(seed) {
    folder <- paste0(base_path, "output-cmd_", seed, ".txt/")
    files <- list.files(folder, pattern = "_productivityScene\\.txt$", full.names = TRUE)
    
    if (length(files) == 0) return(NULL)
    
    # Process each file and extract metadata from filename
    map_dfr(files, function(f) {
        # Extract information from filename
        fname <- basename(f)
        # Expected pattern: {distribution}_{species_proportion}_{repetition}_productivityScene.txt
        m <- str_match(fname, "^(.+?)_([a-zA-Z0-9]+)_([0-9]+)_productivityScene\\.txt$")
        if (is.na(m[1,1])) return(NULL)
        distribution <- m[1,2]
        species_proportion <- m[1,3]
        repetition <- as.integer(m[1,4]) + 1 # +1 because files start from 0
        
        # Read simulation data (skip header lines)
        df <- read_delim(f, delim = "\t", skip = 11, show_col_types = FALSE)
        df <- df %>%
            mutate(
                seed = seed,
                distribution = distribution,
                species_proportion = species_proportion,
                repetition = repetition
            )
        df
    })
}



# Load data from all simulation seeds
forceps_data <- map_dfr(seeds, read_forceps_file)
# Clean column names (remove # symbols)
colnames(forceps_data) <- gsub("#", "", colnames(forceps_data))

# Save the complete dataset for further analysis
save(forceps_data, file = "data/forceeps_output/inventory_productivityScene.RData")