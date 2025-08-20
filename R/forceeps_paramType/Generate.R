# Prepare scenario and files to study different itineraries
# 12/06/2025
# Clémentine de Montgolfier
#-------------------------------------------------------------------------------

## Library and data ------------------------------------------------------------
#------------------------------------------------------------------------------#

library(tidyverse)
load("data/forest_data.RData")
inventory_file <- "unif.inv" # "RETZ_00102_02.inv"
climate_file <- "retz_act.climate"
site_file <- "RETZ_00102_02.site"
potential_species <- "17" # 21 23 14 18 13" # 33 31 5
source("R/utils/inventory_utils.R")

## Inventory -------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Génère un inventaire Forceps avec diamètres uniformes et toutes les tiges en FSyl

library(dplyr)
source("R/utils/inventory_utils.R")

# Paramètres de l'inventaire
n_trees <- 20
diam_min <- 0
diam_max <- 80
species <- "FSyl"
age <- 10

# Générer l'inventaire uniforme avec la nouvelle fonction
inventory <- simulate_inventory_uniforme(n_trees, diam_min, diam_max, species, age)

# surface terrière
# Surface terrière totale en m²/ha (π * (dbh/200)^2 pour chaque arbre, dbh en cm)
surface_terriere <- sum(pi * (inventory$diamètre / 200)^2) * 10
print(paste("Surface terrière totale:", surface_terriere, "m²"))

# Charger la table de correspondance des codes espèces
# (à adapter selon votre environnement)
corresponding.species <- data.frame(
  Retz_Code = "FSyl",
  Forceps_Code = 17
)

# Formater l'inventaire pour Forceps (utilise la fonction de inventory_utils.R)
forceps_inventory <- format_to_forceps(inventory, patch_area = 1000, patchId = 1)

# Sauvegarder l'inventaire (utilise la fonction de inventory_utils.R)
write_forceps_inventory(forceps_inventory, "C:/Capsis4/data/forceps/clementine/Test_itinerary/data/unif.inv")

## Scenario --------------------------------------------------------------------
#------------------------------------------------------------------------------#

# Create a scenario
generate_scenario <- function(
    rotation = 10,
    basal_area = 25,
    type = 0.5,
    species = c("FSyl_80"),
    first_rotation = NULL) {
  total_years <- 80
  # Si first_rotation n'est pas spécifié, utiliser rotation pour la première rotation
  if (is.null(first_rotation)) {
    first_rotation <- rotation
  }
  # Calculer le nombre de rotations
  n_rotations <- ceiling((total_years - first_rotation) / rotation) + 1
  # Créer la chaîne pour la première rotation
  scenario_first <- paste0(
    paste(first_rotation, 3, type, basal_area, sep = "_"), "_",
    paste0(
      sapply(species, function(sp) {
        parts <- strsplit(sp, "_")[[1]]
        paste0(parts[1], "-", parts[2])
      }),
      collapse = ","
    )
  )
  # Chaîne pour les rotations suivantes
  scenario_other <- paste0(
    paste(rotation, 3, type, basal_area, sep = "_"), "_",
    paste0(
      sapply(species, function(sp) {
        parts <- strsplit(sp, "_")[[1]]
        paste0(parts[1], "-", parts[2])
      }),
      collapse = ","
    )
  )
  # Chaîne pour les rotations suivantes
  scenario_other <- paste0(
    paste(rotation, 3, type, basal_area, sep = "_"), "_",
    paste0(
      sapply(species, function(sp) {
        parts <- strsplit(sp, "_")[[1]]
        paste0(parts[1], "-", parts[2])
      }),
      collapse = ","
    )
  )
  # Construire le scénario complet
  scenario <- paste(
    c(scenario_first, rep(scenario_other, n_rotations - 1)),
    collapse = ";"
  )
  return(scenario)
}

## Command file ----------------------------------------------------------------
#------------------------------------------------------------------------------#

write_command_file(
  output_file = "C:/Capsis4/data/forceps/clementine/Test_itinerary/cmd.txt",
  file_setup = "data/forceps.setup"
)

seeds <- c(332, 124, 102, 895, 869, 777, 969, 449, 131, 704)
types <- seq(0, 1, by = 0.2)

for (t in types) {
  for (seed in seeds) {
    # write a simulation line
    write(
      paste0(
        seed, "\t",
        site_file, "\t",
        climate_file, "\t",
        inventory_file, "\t",
        potential_species, "\t",
        # Vous pouvez passer first_rotation ici si besoin, ex: first_rotation = 20
        generate_scenario(
          rotation = 12,
          basal_area = 15,
          type = t,
          species = c("FSyl_100"),
          first_rotation = 1
        )
      ),
      file = "C:/Capsis4/data/forceps/clementine/Test_itinerary/cmd.txt",
      append = TRUE
    )
  }
}
