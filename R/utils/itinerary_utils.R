## Libraries -------------------------------------------------------------------
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)

## Utility Functions -----------------------------------------------------------
#------------------------------------------------------------------------------#

#' Round Value to Nearest 10
#'
#' @param x Numeric value to round.
#' @return Numeric value rounded to nearest 10.
round_to_nearest_10 <- function(x) {
  return(round(x / 10) * 10)
}

## Species Proportions for Scenarios ------------------------------------------
#------------------------------------------------------------------------------#

#' Generate Species Proportions for Regular and Irregular Management
#'
#' @param retz_id Character. Patch identifier from Retz dataset.
#' @return List with two elements: sp_reg (regular management) and sp_irreg (irregular management).
species_proportion <- function(retz_id) {
  species_columns <- colnames(Retz)[9:23]
  
  # Récupérer les espèces présentes et leur proportion
  proportions <- Retz %>%
    filter(Identifiant.peuplement.élémentaire == retz_id) %>%
    select(all_of(species_columns))

  proportions[is.na(proportions)] <- 0

  # Récupérer les 3 plus importantes pour gestion régulière
  top_species_reg <- proportions %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "species", values_to = "proportion") %>%
    arrange(desc(proportion)) %>%
    slice_head(n = 3) %>%
    filter(proportion > 0)

  # Formater en forceps cut proportion pour gestion régulière
  reg_species_short <- corresponding.species$speciesShortName[match(top_species_reg$species, corresponding.species$Retz_Code)]
  valid_idx <- !is.na(reg_species_short)
  reg_species_short <- reg_species_short[valid_idx]
  reg_proportion <- top_species_reg$proportion[valid_idx]

  sp_reg <- paste0(
    reg_species_short, "-", 
    reg_proportion
  ) %>%
    paste(collapse = ",")

  # Récupérer les 4 plus importantes pour gestion irrégulière
  top_species_irreg <- proportions %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "species", values_to = "proportion") %>%
    arrange(desc(proportion)) %>%
    slice_head(n = 4) %>%
    filter(proportion > 0)

  # Formater en forceps cut proportion pour gestion irrégulière (proportion fixe)
  # Supprimer les espèces NA dans le mapping
  irreg_species_short <- corresponding.species$speciesShortName[match(top_species_irreg$species, corresponding.species$Retz_Code)]
  valid_idx <- !is.na(irreg_species_short)
  
  irreg_species_short <- irreg_species_short[valid_idx]

  # Formater en forceps cut proportion pour gestion irrégulière (proportion fixe)
  sp_irreg <- paste0(
    irreg_species_short, "-", 
    25 # proportion fixe pour les irréguliers
  ) %>%
    paste(collapse = ",")
  print(sp_reg)
  print(sp_irreg)
  return(list(sp_reg, sp_irreg))
}

## Itinerary Generation --------------------------------------------------------
#------------------------------------------------------------------------------#

#' Generate ForCEEPS Itinerary for Parameter Type Studies
#'
#' @param rotation Integer. Rotation period in years (default = 10).
#' @param basal_area Numeric. Target basal area (default = 25).
#' @param paramType Numeric. Parameter type value (default = 0.5).
#' @param species_share Character vector. Species shares (default = c("FSyl_80")).
#' @param first_rotation Integer. First rotation period, if different from regular rotation.
#' @param total_years Integer. Total simulation years (default = 80).
#' @param cycles Integer. Number of cycles (default = 3).
#' @return Character string representing the complete itinerary.
generate_itinerary_paramType <- function(
    rotation = 10,
    basal_area = 25,
    paramType = 0.5,
    species_share = c("FSyl_80"),
    first_rotation = NULL,
    total_years = 80,
    cycles = 3) {
  
  # Helper function to format species string
  format_species <- function(species) {
    sapply(species, function(sp) {
      parts <- strsplit(sp, "_")[[1]]
      paste0(parts[1], "-", parts[2])
    }) %>%
      paste(collapse = ",")
  }
  
  # Handle first rotation
  if (is.null(first_rotation)) {
    first_rotation <- rotation
  }
  
  # Calculate number of rotations
  n_rotations <- ceiling((total_years - first_rotation) / rotation) + 1
  
  # Build first rotation string
  itinerary_first <- paste0(
    paste(first_rotation, cycles, paramType, basal_area, sep = "_"), "_",
    format_species(species_share)
  )
  
  # Build subsequent rotation string
  itinerary_other <- paste0(
    paste(rotation, cycles, paramType, basal_area, sep = "_"), "_",
    format_species(species_share)
  )
  
  # Combine into final itinerary string
  itinerary <- paste(
    c(itinerary_first, rep(itinerary_other, n_rotations - 1)),
    collapse = ";"
  )
  
  return(itinerary)
}

## Scenario Generation ---------------------------------------------------------
#------------------------------------------------------------------------------#

#' Generate Management Scenarios for ForCEEPS Simulation
#'
#' @param essence Character. Dominant species code.
#' @param median_age Numeric. Median age of the stand.
#' @param species_proportion List. Species proportions for regular and irregular management.
#' @return Character vector with three scenarios: clearcut, irregular, and no-cut.
generate_scenario <- function(essence, median_age, species_proportion) {
  sp_irreg <- species_proportion[[2]]
  sp_reg <- species_proportion[[1]]

  # Valeurs par défaut si essence non trouvée
  default_rotation_sp <- 100
  default_ba_irregulier <- 20

  # Récupérer les valeurs spécifiques pour l'essence
  param_row <- params %>% filter(essence == !!essence)
  rotation_sp <- 
    ifelse(nrow(param_row) > 0, param_row$rotation_sp, default_rotation_sp)
  ba_irregulier <- 
    ifelse(nrow(param_row) > 0, param_row$ba_irregulier, default_ba_irregulier)

  # ClearCut scenario - Coupe rase avec régénération
  scenario_clearCut <- ""
  simul_time <- 0

  while (simul_time < tot_simul_time) {
    ClearcutTime <- (rotation_sp - median_age) %>%
      round_to_nearest_10() %>%
      max(1, .) %>%
      min(tot_simul_time - simul_time, .)

    # Éclaircies avant coupe finale
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

    # Coupe finale et régénération
    scenario_clearCut <- paste0(
      scenario_clearCut,
      "10_3_0.5_0%_FSyl-80", ";"
    )

    median_age <- 0
    simul_time <- simul_time + ClearcutTime
  }

  # Irregular scenario - Gestion irrégulière
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

  # NoCut scenario - Aucune intervention
  scenario_noCut <- paste0(tot_simul_time, "_3_0.5_0%_FSyl-80")
  return(c(scenario_clearCut, scenario_irregular, scenario_noCut))
}
