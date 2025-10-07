#    FORCEEPS simulation itineraries generation for replicates analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective:
# Generate FORCEEPS inventory from real forest data
# and command files with multiple seeds for replicates analysis
# in order to run ForCEEPS simulations and analyse variability.
#-------------------------------------------------------------------------------

# Correspondence table between common species names and ForCEEPS codes
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

# Load forest data
load("data/forest_data.RData")
Retz <- forest_data %>% as.data.frame() %>% select(-geometry)

# Parameters
patch_id = "RETZ_00137_02"
patcharea = 1000
patchnumber = 1
# Climate data file for current conditions in Retz forest
climate_file = "retz_act.climate"
scenario = "80_3_1_25_FSyl-80"
# Get potential species list in forceps format
potential_species <- "17 21 23 14 18 13 33 31 5"

## Set up ForCEEPS directory ---------------------------------------------------
#------------------------------------------------------------------------------#

initialise_forceeps_folder(forceeps_path, analyse_name, overwrite = TRUE)

# add site file in the data/site folder
file.copy("data/forceeps_init_files/forceps.site",
          file.path(base_path, "data/sites/RETZ_00137_02.site"),
          overwrite = TRUE)

## Inventory -------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Generate inventory from real forest data for the specified patch
results <- simulate_inventory_for_patch(
    patch_id, distribution = "mean", species_proportion = "fixed")

# Format the inventory for Forceps (uses function from inventory_utils.R)
forceps_inv <- format_to_forceps(results$inventory, patcharea, patchnumber)

# Save the inventory (uses function from inventory_utils.R)
output_file <- paste0(base_path, "data/inventories/", patch_id, ".inv")
write_forceps_inventory(forceps_inv, output_file, patcharea, patchnumber)

# Generate cmd -----------------------------------------------------------------
#------------------------------------------------------------------------------#

# Number of command files to generate
n_files <- 30
seeds <- 1612:5000
seeds_split <- split(seeds, cut(seq_along(seeds), n_files, labels = FALSE))

for (file_idx in seq_along(seeds_split)) {
  # Command file name
  cmd_file <- paste0(base_path, "cmd_", file_idx, ".txt")
  # Setup file
  setup_file <- "data/forceps.setup"
  
  # Write the command file (uses function from output_utils.R)
  write_command_file(
    output_file = cmd_file,
    file_setup = setup_file
  )
  
  # Add lines for each seed
  for (i in seeds_split[[file_idx]]) {
    # write a simulation line
    write(paste0(
      i, "\t",
      "sites/RETZ_00137_02.site", "\t",
      climate_file, "\t",
      paste0("inventories/", patch_id, ".inv"), "\t",
      potential_species, "\t",
      scenario), 
      file = cmd_file, append = TRUE)
  }
}
