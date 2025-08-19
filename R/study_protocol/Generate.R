# Library ---------------------------------------------------------------------
#------------------------------------------------------------------------------#
library(tidyverse)
library(sf)
source("R/inventory_utils.R")

## Parameters and data ---------------------------------------------------------
#------------------------------------------------------------------------------#

# paramètres

set.seed(3400)

load("data/forest_data.RData")
forest_data %>%
  as.data.frame() %>%
  filter(Structure.et.occupation.du.sol %in% c("F", "I")) %>%
  filter(Classe.de.catégorie.de.diamètre.dominant == "S") %>%
  nrow()
forest_data %>%
  as.data.frame() %>%
  filter(Structure.et.occupation.du.sol %in% c("F", "I")) %>%
  nrow()

# ajouter l'age median quand il n'existe pas avec la formule (NA)
forest_data$median_age[is.na(forest_data$median_age)] <- 
  forest_data$diamètre.mean[is.na(forest_data$median_age)] * 2

Retz <- forest_data %>%
  as.data.frame() %>%
  filter(Structure.et.occupation.du.sol %in% c("F", "I")) %>%
  slice_sample(n = 10) # pour le test
corresponding.species <-
  read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

tot_simul_time = 150

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
  path = "C:/Capsis4/data/forceps/clementine/Test_protocole"
)

# write the command files for all
# generate_command_files(
Retz <- Retz
seed <- 3400
path <- "C:/Capsis4/data/forceps/clementine/Test_protocole"
setup_file <- "data/forceps.setup"

climate_file <- "Retz_act.climate"
potential_species <- "17 21 31 5 23 14 18 13" # 33

round_to_nearest_10 <- function(x) {
  return(round(x / 10) * 10)
}

params <- tibble::tibble(
  essence = c("CHS", "CHP", "HET", "CHA", "P.L"),
  rotation_sp = c(150, 150, 100, 80, 60),
  ba_irregulier = c(12, 12, 16, 20, 25)
)

species_proportion <- function(retz_id) {
species_columns <- colnames(Retz)[9:23]
  # récupérer les espèces présentes et leur proportion
  proportions <- Retz %>%
	filter(Identifiant.peuplement.élémentaire == retz_id) %>%
	select(all_of(species_columns))

	proportions[is.na(proportions)] <- 0

  # récupérer les 4 plus importantes
  top_species <- proportions %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "species", values_to = "proportion") %>%
    arrange(desc(proportion)) %>%
    slice_head(n = 3) %>%
    # proportion > 0
    filter(proportion > 0)

  # formater en forceps cut proportion
  sp_reg <- paste0(
    corresponding.species$speciesShortName[match(top_species$species, corresponding.species$Retz_Code)], "-", 
    top_species$proportion
  ) %>%
    paste(collapse = ",")

    top_species <- proportions %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "species", values_to = "proportion") %>%
    arrange(desc(proportion)) %>%
    slice_head(n = 4) %>%
    # proportion > 0
    filter(proportion > 0)

  sp_irreg <- paste0(
    corresponding.species$speciesShortName[match(top_species$species, corresponding.species$Retz_Code)], "-", 
    25 # proportion fixe pour les irréguliers
  ) %>%
    paste(collapse = ",")
  
  return(list(sp_reg, sp_irreg))
}

generate_scenario <- function(structure, essence, median_age, species_proportion) {
  sp_irreg <- species_proportion[[2]]
  sp_reg <- species_proportion[[1]]

  # Valeurs par défaut si essence non trouvée
  default_rotation_sp <- 100
  default_ba_irregulier <- 20

  # Récupérer les valeurs pour l'essence
  param_row <- params %>% filter(essence == !!essence)
  rotation_sp <- 
    ifelse(nrow(param_row) > 0, param_row$rotation_sp, default_rotation_sp)
  ba_irregulier <- 
    ifelse(nrow(param_row) > 0, param_row$ba_irregulier, default_ba_irregulier)

  # ClearCut scenario
  ####################

  scenario_clearCut <- ""
  simul_time <- 0

  while (simul_time < tot_simul_time) {
    ClearcutTime <- (rotation_sp - median_age) %>%
      round_to_nearest_10() %>%
      max(1, .) %>%
      min(tot_simul_time - simul_time, .)

    if (ClearcutTime > 10) {
      n <- ClearcutTime / 10 - 1
      for (i in 1:n) {
        scenario_clearCut <- paste0(
          scenario_clearCut, "10_3_0.5_70%_",
          sp_reg,
          ";"
        )
      }
    }

    scenario_clearCut <- paste0(
      scenario_clearCut,
      "10_3_0.5_0%_FSyl-80", ";"
    )

    median_age <- 0
    simul_time <- simul_time + ClearcutTime
  }

  # Irregular scenario
  ####################

  scenario_irregular <- ""
  simul_time <- 0

  while (simul_time < tot_simul_time) {
    cut_time <- 5 %>% min(tot_simul_time - simul_time, .)
    scenario_irregular <- paste0(
      scenario_irregular,
      cut_time, "_3_0.5_90%_",
      sp_irreg, ";"
    )
    simul_time <- simul_time + cut_time
  }

  # NoCut scenario
  ################

  scenario_noCut <- paste0(tot_simul_time, "_3_0.5_0%_FSyl-80")
  
  return(c(scenario_clearCut, scenario_irregular, scenario_noCut))
}

for (i in seq_along(Retz$Identifiant.peuplement.élémentaire)) {
  retz_id <- Retz$Identifiant.peuplement.élémentaire[i]
  cmd_file <- file.path(path, paste0("cmd_", i, ".txt"))
  write_command_file(
    output_file = cmd_file,
    file_setup = setup_file
  )
  inventory_file <- paste0("inventaires/", retz_id, ".inv")
  site_file <- paste0("sites/", retz_id, ".site")
  scenario <- generate_scenario(
    Retz$Structure.et.occupation.du.sol[i],
    Retz$Essence.déterminant.la.sylviculture[i],
    Retz$median_age[i],
    species_proportion(Retz$Identifiant.peuplement.élémentaire[i])
  )


  for (scen in scenario) {
    for (seed_offset in 0:9) {
      current_seed <- seed + seed_offset
      write(
        paste0(
          current_seed, "\t",
          site_file, "\t",
          climate_file, "\t",
          inventory_file, "\t",
          potential_species, "\t",
          scen
        ),
        file = cmd_file, append = TRUE
      )
    }
  }
}