## Library ---------------------------------------------------------------------
#------------------------------------------------------------------------------#

library(tidyr)
library(truncnorm)

## Inventory variable functions ------------------------------------------------
#------------------------------------------------------------------------------#

generate_diameters <- function(patch, type, n) {
  switch(type,
         unif = runif(n, min = patch$diamètre.min, max = patch$diamètre.max),
         normal_6 = rnorm(n, mean = patch$diamètre.mean, sd = (patch$diamètre.max - patch$diamètre.min) / 6),
         normal_4 = rnorm(n, mean = patch$diamètre.mean, sd = (patch$diamètre.max - patch$diamètre.min) / 4),
         normal_IR = rtruncnorm(n, a = patch$diamètre.min, b = patch$diamètre.max, mean = patch$diamètre.mean, sd = (patch$diamètre.max - patch$diamètre.min) / 10),
         mean = rep(patch$diamètre.mean, n),
         min = rep(patch$diamètre.min, n),
         max = rep(patch$diamètre.max, n),
         stop("Invalid type"))
}

generate_species_proportion <- function(patch_id, species_cols, variation = 10) {
  # Extraire les proportions d'origine
  proportions <- Retz %>%
    filter(Identifiant.peuplement.élémentaire == "RETZ_00102_04") %>%
    select(all_of(species_cols)) %>%
    unlist() %>%
    as.numeric()
  
  proportions[is.na(proportions)] <- 0
  
  if (sum(proportions) == 0) {
    stop("No species proportions found for this patch.")
  }

  # Séparer les indices valides (proportions > 0)
  active_idx <- which(proportions > 0)
  fixed_idx <- which(proportions == 0)

  active_props <- proportions[active_idx]

  # Normaliser si besoin (pas besoin)
  active_props <- active_props / sum(active_props) * 100

  # Générer du bruit aléatoire dans la plage [-variation, +variation]
  noise <- runif(length(active_props), -variation, variation)
  modified <- active_props + noise

  # Mettre à 0 les négatifs
  modified[modified < 0] <- 0

  # Re-normaliser
  modified <- modified / sum(modified) * 100

  # Reconstruire le vecteur complet
  final_props <- numeric(length(proportions))
  final_props[active_idx] <- modified
  final_props[fixed_idx] <- 0

  return(final_props)
}

generate_mean_diameter <- function(patch, distribution) {
  # Calculer la moyenne des diamètres
  mean_diameter <- switch(distribution,
    "unif" = (patch$diamètre.min + patch$diamètre.max) / 2,
    "normal_IR" = patch$diamètre.mean,
    "normal_6" = patch$diamètre.mean,
    "normal_4" = patch$diamètre.mean,
    "mean" = patch$diamètre.mean,
    "min" = patch$diamètre.min,
    "max" = patch$diamètre.max,
    stop("Invalid distribution")
  )
  return(mean_diameter)
}

## Inventory general -----------------------------------------------------------
#------------------------------------------------------------------------------#

# fonction surface d'un disque à partir du diamètre
disk_area <- function(d) {
  pi * (d / 2)^2
}

get_species_distribution <- function(n_trees, patch_id, species_cols, species_proportion) {
  if (species_proportion == "random") {
    proportions <- generate_species_proportion(patch_id, species_cols)
  } else {
    proportions <- Retz %>%
      filter(Identifiant.peuplement.élémentaire == patch_id) %>%
      select(all_of(species_cols)) %>%
      unlist() %>%
      as.numeric()
  }

  proportions[is.na(proportions)] <- 0
  species_names <- species_cols

  sampled_species <- rep(species_names, round(proportions * n_trees / 100, 0))
  # si pas de longueur n_trees ajoiuter un arbre de l'espèce la plus commune
  if (length(sampled_species) != n_trees) {
    if (length(sampled_species) < n_trees) {
      sampled_species <- c(sampled_species, rep(species_names[which.max(proportions)], n_trees - length(sampled_species)))
    } else {
      sampled_species <- sampled_species[1:n_trees]
    }
  }
  return(sampled_species)
}

generate_inventory <- function(patch_id, distribution = "normal_6", species_proportion) {
  patch <- Retz %>% filter(Identifiant.peuplement.élémentaire == patch_id)
  species_cols <- colnames(Retz)[9:36]

  # If surf.terrière.moy == 0, return empty inventory with correct colnames
  if (patch$surf.terrière.moy == 0) {
    inventory_df <- data.frame(
      species = character(0),
      diamètre = numeric(0),
      age = numeric(0)
    )
    patch <- patch %>% select(where(~ !all(is.na(.))))
    return(list(
      inventory = inventory_df,
      # potential_species = intersect(species_cols, colnames(patch)),
      RUM = patch$RUM
    ))
  }

  n_trees <- round((patch$surf.terrière.moy / disk_area(generate_mean_diameter(patch, distribution)/100)) / 10)

  if(patch$Structure.et.occupation.du.sol == "I"){distribution <- "normal_IR"}

  diameters <- generate_diameters(patch, distribution, n_trees)
  species <- get_species_distribution(n_trees, patch_id, species_cols, species_proportion)

  age_value <- if (!is.na(patch$Age.réel.en.2010)) patch$Age.réel.en.2010 else round(patch$median_age, 0)

  inventory_df <- data.frame(
    species = species,
    diamètre = diameters,
    age = age_value
  )
  # select les colonnes de patch qui n'ont pas comme valeur NA
  patch <- patch %>% select(where(~ !all(is.na(.))))
  list(
    inventory = inventory_df,
    # potential_species = intersect(species_cols, colnames(patch)),
    RUM = patch$RUM
  )
}

forceps_format <- function(inventory, patch_area, patchId) {
  inventory %>%
    left_join(corresponding.species %>% select(Retz_Code, Forceps_Code),
              by = c("species" = "Retz_Code")) %>%
    mutate(
      patchId = patchId,
      speciesId = Forceps_Code,
      dbh = round(diamètre, 1),  # Convert to cm
      crownA1 = 0.1,
      dbhIncrement = 0.1,
      slowGrowthIndex = 0
    ) %>%
    mutate(treeId = row_number()) %>%
    select(patchId, treeId, speciesId, age, dbh, crownA1, dbhIncrement, slowGrowthIndex)
}

## File management -------------------------------------------------------------
#------------------------------------------------------------------------------#

file_inventory <- function(forceps_inv, patcharea, patchnumber = 1, output_file){
  writeLines(c(
    paste0("# Forceps inventory"),
    "",
    paste0("inventoryPatchN = ", patchnumber),
    paste0("inventoryPatchArea = ", patcharea),
    "",
    "#seed\tpatchId\ttreeId\tspeciesId\tage(years)\tdbh(cm)\tcrownA1[0,1]\tdbhIncrement(cm)\tslowGrowthIndex(0,1)"), 
    con = output_file)

  write.table(
    forceps_inv ,
    file = output_file,
    append = TRUE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
}

write_cmd_file <- function(output_file, file_setup) {
  cmd_content <- c(
    "# Forceps script command file, format SimulationCommandReader2 (Xavier Morin)",
    "",
    paste0("setupFileName = ", file_setup),
    "numberOfYearsToBeJumped = 1",
    "exportTimeStep = 1",
    "",
    "#siteFileName\tclimateFileName\tinventoryFileName\tpotentialSpeciesList\tScenario"
  )
  
  writeLines(cmd_content, con = output_file)
}

update_forceps_file <- function(filepath, updates = list(), output_path = filepath) {
  # Lire les lignes du fichier
  lines <- readLines(filepath)
  
  # Fonction pour modifier une ligne si elle correspond à un paramètre donné
  modify_line <- function(lines, param, new_value) {
    sapply(lines, function(line) {
      if (grepl(paste0("^", param, "\\s*="), line)) {
        return(paste0(param, " = ", new_value))
      } else {
        return(line)
      }
    })
  }

  # Appliquer toutes les mises à jour
  for (param in names(updates)) {
    lines <- modify_line(lines, param, updates[[param]])
  }

  # Si output_path est identique à filepath, on écrase
  # Sinon, on écrit un nouveau fichier
  writeLines(lines, con = output_path)
}


add_scenario_cmd <- function(output_file, scenario_name, inventory_file, site_file, climate_file, potential_species) {
  cmd_content <- paste0(site_file, "\t", climate_file, "\t", inventory_file, "\t", potential_species, "\t", scenario_name)
  
  con <- file(output_file, open = "a")  # open in append mode
  writeLines(cmd_content, con)
  close(con)
}