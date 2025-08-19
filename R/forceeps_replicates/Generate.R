## Library ---------------------------------------------------------------------
#------------------------------------------------------------------------------#
library(tidyverse)
source("R/utils/inventory_utils.R")

## Parameters and data ---------------------------------------------------------
#------------------------------------------------------------------------------#

# paramètres
patch_id = "RETZ_00137_02"
patcharea = 1000
patchnumber = 1
climate_file = "retz_act.climate"
scenario = "80_3_1_25_FSyl-80"
path = "C:/Capsis4/data/forceps/clementine/Test_repetition"

# Get potential species list in forceps format
potential_species <- "17 21 23 14 18 13 33 31 5"

load("data/forest_data.RData")
Retz <- forest_data %>% as.data.frame() %>% select(-geometry)
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

## Generate inventory ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Générer l'inventaire
results <- simulate_inventory_for_patch(
    patch_id, distribution = "mean", species_proportion = "fixed")

# Formatter l'inventaire pour forceps
forceps_inv <- format_to_forceps(results$inventory, patcharea, 1)

# Sauvegarder l'inventaire individuel
output_file <- paste0(path, "/data/forceps.inv")

write_forceps_inventory(forceps_inv, output_file, patcharea, patchnumber)

# Generate cmd -----------------------------------------------------------------
#------------------------------------------------------------------------------#

# Nombre de fichiers à générer
n_files <- 30
seeds <- 1612:5000
seeds_split <- split(seeds, cut(seq_along(seeds), n_files, labels = FALSE))

for (file_idx in seq_along(seeds_split)) {
  # Nom du fichier cmd
  cmd_file <- paste0(path, "/cmd_", file_idx, ".txt")
  # Nom du fichier setup
  setup_file <- paste0("data/forceps.setup")
  
  # Écrire le fichier setup dans le cmd
  write_command_file(
    output_file = cmd_file,
    file_setup = setup_file
  )
  
  # Ajouter les lignes pour chaque seed
  for (i in seeds_split[[file_idx]]) {
    write(paste0(
      paste0(i, "\t"),
      "RETZ_00102_04.site\t",
      climate_file, "\t",
      "forceps.inv", "\t",
      potential_species, "\t",
      scenario), 
      file = cmd_file, append = TRUE)
  }
}
