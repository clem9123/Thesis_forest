#    FORCEEPS simulation itineraries generation for paramType parameter impact analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective: Generate FORCEEPS inventory, and command file with different paramType values in order to run ForCEEPS simulations
#-------------------------------------------------------------------------------

# Base path to FORCEEPS working directory
base_path = "C:/Capsis4/data/forceps/clementine/Test_itinerary/"

## Library and data ------------------------------------------------------------
#------------------------------------------------------------------------------#

# Load required libraries
library(tidyverse)  # Data manipulation packages
source("R/utils/inventory_utils.R")  # Utility functions for inventories

# Correspondence table between common species names and ForCEEPS codes
corresponding_species = read.csv("data/corresponding_species.csv")

# Name of the inventory file to generate (name convention RETZ_#_#), even if not extracted from Retz data here
inventory_file <- "inventaires/RETZ_0_0.inv"
# Climate data file for current conditions in Retz forest
climate_file <- "retz_act.climate"
# Forest site parameter file
site_file <- "RETZ_0_0.site"
# potential species (see ForCEEPS documentation)
potential_species <- "17"


## Inventory -------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Generate a Forceps inventory with uniform diameters and all stems as FSyl

# Generate the uniform inventory with the new function
inventory <- simulate_inventory_uniforme(
  n_trees = 20,
  diam_min = 0,
  diam_max = 80,
  species = "FSyl",
  age = 10
)

# Format the inventory for Forceps (uses function from inventory_utils.R)
forceps_inventory <- format_to_forceps(
  inventory, 
  patch_area = 1000, 
  patchId = 1
)

# Save the inventory (uses function from inventory_utils.R)
write_forceps_inventory(
  inventory = forceps_inventory, 
  output_file = paste0(base_path,"data/", inventory_file)
)

## Scenario --------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Create a scenario
generate_scenario <- function(
    rotation = 10,
    basal_area = 25,
    paramType = 0.5,
    species_share = c("FSyl_80"),
    first_rotation = NULL,
    total_years = 80,
    cycles = 3) {
  
  # --- Helper function to format species string ---
  format_species <- function(species) {
    sapply(species, function(sp) {
      parts <- strsplit(sp, "_")[[1]]
      paste0(parts[1], "-", parts[2])
    }) %>%
      paste(collapse = ",")
  }
  
  # --- Handle first rotation ---
  if (is.null(first_rotation)) {
    first_rotation <- rotation
  }
  
  # --- Number of rotations ---
  n_rotations <- ceiling((total_years - first_rotation) / rotation) + 1
  
  # --- Build first rotation string ---
  scenario_first <- paste0(
    paste(first_rotation, cycles, paramType, basal_area, sep = "_"), "_",
    format_species(species_share)
  )
  
  # --- Build subsequent rotation string ---
  scenario_other <- paste0(
    paste(rotation, cycles, paramType, basal_area, sep = "_"), "_",
    format_species(species_share)
  )
  
  # --- Final scenario string ---
  scenario <- paste(
    c(scenario_first, rep(scenario_other, n_rotations - 1)),
    collapse = ";"
  )
  
  # --- Return ---
  return(scenario)
}


## Command file ----------------------------------------------------------------
#------------------------------------------------------------------------------#

write_command_file(
  output_file = paste0(base_path,"cmd_1.txt"),
  file_setup = "data/forceps.setup"
)

seeds <- c(332, 124, 102, 895, 869, 777, 969, 449, 131, 704)
types <- seq(0, 1, by = 0.2)

for (t in types) { # create simulations for each paramType
  for (seed in seeds) { # create 10 repetitions for each paramType
    # write a simulation line
    write(
      paste0(
        seed, "\t",
        site_file, "\t",
        climate_file, "\t",
        inventory_file, "\t",
        potential_species, "\t",
        generate_scenario(
          rotation = 12,
          basal_area = 15,
          paramType = t,
          species = c("FSyl_100"),
          first_rotation = 1
        )
      ),
      file = paste0(base_path,"cmd_1.txt"),
      append = TRUE
    )
  }
}