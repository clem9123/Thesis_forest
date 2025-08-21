#    FORCEEPS simulation itineraries generation for exploration of multiscale forest functions
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective:
# Generate 10 FORCEEPS inventories (#10 chosen to speed up exploration analyses)
# and command file with varying itineraries (clearcut, irregular, no intervention)
# in order to run ForCEEPS simulations and analyse multiscale dynamics.
#-------------------------------------------------------------------------------

# Base path to FORCEEPS working directory
forceeps_path = "C:/Capsis4/data/forceps/clementine/"
analyse_name = "Study_protocol"
base_path = paste0(forceeps_path, analyse_name, "/")

## Library and data ------------------------------------------------------------
#------------------------------------------------------------------------------#

# Load required libraries
library(tidyverse)  # Data manipulation packages
library(sf)         # Spatial data manipulation
source("R/utils/inventory_utils.R")  # Utility functions for inventory generation
source("R/utils/itinerary_utils.R")  # Utility functions for itineraries

# Load data
corresponding.species <- # Correspondence table between common species names and ForCEEPS codes
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")
load("data/forest_data.RData") # Retz forest inventory data
Retz <- forest_data  %>%
  filter(Structure.et.occupation.du.sol %in% c("F", "I")) %>%
  slice_sample(n = 10) # 10 patches are chosen for this test

# parameters
set.seed(3400)
seed <- 3400
tot_simul_time = 150 # Total simulation time (years) to visualise trajectory and analyse dynamics on
potential_species <- "17 21 31 5 23 14 18 13" # ForCEEPS parameter (list pf species that can appear on th patch)
params <- tibble::tibble(
  essence = c("CHS", "CHP", "HET", "CHA", "P.L"),
  rotation_sp = c(150, 150, 100, 80, 60), # Rotation age for each species
  ba_irregulier = c(12, 12, 16, 20, 25) # After cut basal area for irregular management
)
n_rep = 2 # Number of repetitions of ForCEEPS simulations for each itinerary

## Set up ForCEEPS directory ---------------------------------------------------
#------------------------------------------------------------------------------#

initialise_forceeps_folder(forceeps_path, analyse_name, overwrite = TRUE)

## Generate all inventory ------------------------------------------------------
#------------------------------------------------------------------------------#

# Create a RetzId.inv and RetsId.site for every retz_id

# inventories and sites
generate_all_inventories(
  Retz,
  distribution_types = "mean",
  species_proportion = "fixe",
  patcharea = 1000,
  patchnumber = 1,
  path = base_path
)

## Generate all command files --------------------------------------------------
#------------------------------------------------------------------------------#

for (i in seq_along(Retz$Identifiant.peuplement.élémentaire)) {
  retz_id <- Retz$Identifiant.peuplement.élémentaire[i]
  cmd_file <- file.path(base_path, paste0("cmd_", i, ".txt"))
  write_command_file(
    output_file = cmd_file,
    file_setup = "data/forceps.setup"
  )
  inventory_file <- paste0("inventories/", retz_id, ".inv")
  site_file <- paste0("sites/", retz_id, ".site")
  scenario <- generate_scenario(
    Retz$Essence.déterminant.la.sylviculture[i],
    Retz$median_age[i],
    species_proportion(Retz$Identifiant.peuplement.élémentaire[i])
  )


  for (scen in scenario) {
    for (seed_offset in 0:n_rep) {
      current_seed <- seed + seed_offset
      write(
        paste0(
          current_seed, "\t",
          site_file, "\t",
          "retz_act.climate", "\t", # climate file
          inventory_file, "\t",
          potential_species, "\t",
          scen
        ),
        file = cmd_file, append = TRUE
      )
    }
  }
}
