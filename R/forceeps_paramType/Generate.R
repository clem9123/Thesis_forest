# Prepare scenario and files to study different itineraries
# 12/06/2025
# Clémentine de Montgolfier
#-------------------------------------------------------------------------------

base_path = "C:/Capsis4/data/forceps/clementine/Test_itinerary/"

## Library and data ------------------------------------------------------------
#------------------------------------------------------------------------------#

library(tidyverse)
source("R/utils/inventory_utils.R")

load("data/forest_data.RData")
corresponding_species = read.csv("data/corresponding_species.csv")

inventory_file <- "inventaires/RETZ_0_0.inv" # explain name
climate_file <- "retz_act.climate"
site_file <- "RETZ_00102_02.site"
potential_species <- "17" # Fagus Sylvatica, Hêtre


## Inventory -------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Génère un inventaire Forceps avec diamètres uniformes et toutes les tiges en FSyl

# Générer l'inventaire uniforme avec la nouvelle fonction
inventory <- simulate_inventory_uniforme(
  n_trees = 20,
  diam_min = 0,
  diam_max = 80,
  species = "FSyl",
  age = 10
)

# Formater l'inventaire pour Forceps (utilise la fonction de inventory_utils.R)
forceps_inventory <- format_to_forceps(
  inventory, 
  patch_area = 1000, 
  patchId = 1
)

# Sauvegarder l'inventaire (utilise la fonction de inventory_utils.R)
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