library(tidyr)
library(dplyr)

import_data <- function(){
  # import data, attention remplacer les , par des . dans les colonnes
  UEP <- read.csv("data/BDD_UEP_2012.csv", header = TRUE, sep = ",")
  # remplacer les , par des . dans les colonnes
  UEP <- UEP %>% mutate(across(where(is.character), ~ gsub(",", ".", .x)))

  # supprimer  (% du couvert boisé) avec des pionts du coup ....du.couvert.boisé. des noms de colonnes
  UEP <- UEP %>% rename_with(~ gsub("\\....du.couvert.boisé.", "", .x))

  table_ccd <- data.frame(
    Classe.de.catégorie.de.diamètre.dominant = c("S", "E", "IR", "1", "P", "M", "G", "G-", "G+", "T"),
    diamètre.min = c(0, 3, 3, 7.5, 17.5, 27.5, 47.5, 47.5, 55, 67.5),
    diamètre.max = c(3, 10, 10, 17.5, 27.5, 47.5, 67.5, 55, 67.5, 100))
  table_ccd <- table_ccd %>% mutate(diamètre.mean = (diamètre.min + diamètre.max) / 2)

  # passer des cm en metre
  table_ccd <- table_ccd %>% mutate(diamètre.min = diamètre.min / 100, diamètre.max = diamètre.max / 100, diamètre.mean = diamètre.mean / 100)

  UEP %>% left_join(table_ccd, by = "Classe.de.catégorie.de.diamètre.dominant") -> UEP
  UEP[,"Âge.réel.en.2010"] <- as.numeric(UEP$Âge.réel.en.2010)
  UEP[,"surf.terrière.moy"] <- as.numeric(UEP$surf.terrière.moy)
  UEP <- UEP %>%
    mutate(Classe.d.âge.en.2010 = ifelse(Classe.d.âge.en.2010 == "NC", NA, Classe.d.âge.en.2010)) %>%
    separate(Classe.d.âge.en.2010, into = c("age_min", "age_max"), sep = "-", convert = TRUE) %>%
    mutate(median_age = (as.numeric(age_min)+as.numeric(age_max))/2, na.rm = TRUE)
  return(UEP)
}

# fonction surface d'un disque à partir du diamètre
G <- function(d) {
  return(pi * (d / 2)^2)
}

get_species_prop <- function(Nb_tree,n) {
  species_list <- Retz %>% select(c(9:36)) %>% colnames()
  proportions <- Retz %>% filter(Identifiant.peuplement.élémentaire == n) %>% select(c(9:36)) %>% as.numeric()
  proportions[is.na(proportions)] <- 0
  return(rep(species_list, round(proportions*Nb_tree/100,0)) %>% sample(size = Nb_tree, replace = TRUE))
}


random_d <- function(x, type,n){
  # type = "unif", "normal", "mean", "min", "max"
  if (type == "unif") {
    return(runif(n, min = x$diamètre.min, max = x$diamètre.max))
  } else if (type == "normal_6") {
    return(rnorm(n, mean = x$diamètre.mean, sd = (x$diamètre.max - x$diamètre.min) / 6))
  } else if (type == "normal_4") {
  return(rnorm(n, mean = x$diamètre.mean, sd = (x$diamètre.max - x$diamètre.min) / 6))
  } else if (type == "mean") {
    return(rep(x$diamètre.mean, n))
  } else if (type == "min") {
    return(rep(x$diamètre.min, n))
  } else if (type == "max") {
    return(rep(x$diamètre.max, n))
  } else {
    stop("Invalid type")
  }
}

# graph de rnorm(n, mean = x$diamètre.mean, sd = (x$diamètre.max - x$diamètre.min) / 6) :
hist(rnorm(10000, mean = Patch$diamètre.mean, sd = (Patch$diamètre.max - Patch$diamètre.min) / 6))

full_inventory <- function(n, inv_type) {
  Patch <- Retz %>% filter(Identifiant.peuplement.élémentaire == n)
  Nb_tree <- (Patch$surf.terrière.moy/G(Patch$diamètre.mean)) %>% round(0)

  d <- random_d(Patch, inv_type, Nb_tree)

  species <- get_species_prop(Nb_tree,n)

  age <- (if(!is.na(Patch$Âge.réel.en.2010)){Patch$Âge.réel.en.2010} else {round(Patch$median_age,0)})

  # Créer un data frame avec les diamètres et les espèces
  return(data.frame(
    species = species,
    diamètre = d,
    age = age
  ))
}

forceps_inv <- function(inventory, patcharea){
    # add species code from forceps
  forceps_inv <- inventory %>%
    left_join(corresponding.species %>% select(Retz_Code,Forceps_Code), by = c("species" = "Retz_Code"))
  
  forceps_inv <- forceps_inv %>% 
    mutate(
      patchId = 1,
      speciesId = Forceps_Code,
      age = age,
      dbh = round(diamètre * 100, 1), # convert to cm
      crownA1 = 0.1,
      dbhIncrement = 0.1,
      slowGrowthIndex = 0) %>% 
    sample_n(size = nrow(forceps_inv) * patcharea / 10000) %>%
    mutate(treeId = row_number()) %>%
    select(patchId, treeId, speciesId, age, dbh, crownA1, dbhIncrement, slowGrowthIndex)
  
  return(forceps_inv)
}

file_inventory <- function(forceps_inv, patcharea, patchnumber = 1, output_file){
  writeLines(c(
    paste0("# Forceps inventory"),
    "",
    paste0("inventoryPatchN = ", patchnumber),
    paste0("inventoryPatchArea = ", patcharea),
    "",
    "#patchId\ttreeId\tspeciesId\tage(years)\tdbh(cm)\tcrownA1[0,1]\tdbhIncrement(cm)\tslowGrowthIndex(0,1)"), 
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

write_cmd_file <- function(output_file, inventory_file, site_file, climate_file, potential_species, scenarios) {
  cmd_content <- c(
    "# Forceps script command file, format SimulationCommandReader2 (Xavier Morin)",
    "",
    "setupFileName = forcepsBasic_new.setup",
    "numberOfYearsToBeJumped = 1",
    "exportTimeStep = 1",
    "",
    "#siteFileName\tclimateFileName\tinventoryFileName\tpotentialSpeciesList\tScenario"
  )
  
  writeLines(cmd_content, con = output_file)
}

add_scenario_cmd <- function(output_file, scenario_name, inventory_file, site_file, climate_file, potential_species) {
  cmd_content <- paste0(site_file, "\t", climate_file, "\t", inventory_file, "\t", potential_species, "\t", scenario_name)
  
  con <- file(output_file, open = "a")  # open in append mode
  writeLines(cmd_content, con)
  close(con)
}