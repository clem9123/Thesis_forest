#    FORCEEPS simulation itineraries generation for paramType parameter impact analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective:
# Generate one (uniform diameter distribution) FORCEEPS inventory
# and command file with varying paramType values in the itinerary
# in order to run ForCEEPS simulations and analyse paramType effects.
#-------------------------------------------------------------------------------

# Base path to FORCEEPS working directory
forceeps_path = "C:/Capsis4/data/forceps/clementine/"
analyse_name = "ParamType"
base_path = paste0(forceeps_path, analyse_name, "/")

## Library and data ------------------------------------------------------------
#------------------------------------------------------------------------------#

# Load required libraries
library(tidyverse)  # Data manipulation packages
source("R/utils/inventory_utils.R")  # Utility functions for inventories
source("R/utils/itinerary_utils.R")  # Utility functions for itineraries

# Correspondence table between common species names and ForCEEPS codes
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")


# Name of the inventory file to generate (name convention RETZ_#_#), even if not extracted from Retz data here
inventory_file <- "inventories/RETZ_0_0.inv"
# Climate data file for current conditions in Retz forest
climate_file <- "retz_act.climate"
# Forest site parameter file
site_file <- "sites/RETZ_0_0.site"
# potential species (see ForCEEPS documentation)
potential_species <- "17"

## Set up ForCEEPS directory ---------------------------------------------------
#------------------------------------------------------------------------------#

initialise_forceeps_folder(forceeps_path, analyse_name, overwrite = TRUE)

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
        generate_itinerary_paramType(
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