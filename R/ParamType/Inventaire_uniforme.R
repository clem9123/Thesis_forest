# Génère un inventaire Forceps avec diamètres uniformes et toutes les tiges en FSyl

library(dplyr)

# Paramètres de l'inventaire
n_trees <- 20
diam_min <- 0
diam_max <- 80
species <- "FSyl"
age <- 10

# Générer des diamètres uniformément répartis
diameters <- seq(diam_min, diam_max, length.out = n_trees)

# Créer le data.frame d'inventaire
inventory <- data.frame(
  species = species,
  diamètre = diameters,
  age = age
)

# surface terrière
# Surface terrière totale en m² (π * (dbh/200)^2 pour chaque arbre, dbh en cm)
surface_terriere <- sum(pi * (inventory$diamètre / 200)^2)
print(paste("Surface terrière totale:", surface_terriere, "m²"))

# Charger la table de correspondance des codes espèces
# (à adapter selon votre environnement)
corresponding.species <- data.frame(
  Retz_Code = "FSyl",
  Forceps_Code = 17
)

# Fonction de formatage Forceps (issue de inventory_utils.R)
format_to_forceps <- function(inventory, patch_area, patchId = 1) {
  inventory %>%
    left_join(corresponding.species %>% select(Retz_Code, Forceps_Code),
              by = c("species" = "Retz_Code")) %>%
    mutate(
      patchId = patchId,
      speciesId = Forceps_Code,
      dbh = round(diamètre, 1),
      crownA1 = 0.1,
      dbhIncrement = 0.1,
      slowGrowthIndex = 0,
      treeId = row_number()
    ) %>%
    select(patchId, treeId, speciesId, age, dbh, crownA1, dbhIncrement, slowGrowthIndex)
}

# Formater l'inventaire pour Forceps
forceps_inventory <- format_to_forceps(inventory, patch_area = 1000, patchId = 1)

# Écrire le fichier inventaire Forceps
write_forceps_inventory <- function(inventory, output_file, patch_area = 1000, patch_number = 1) {
  writeLines(c(
    "# Forceps inventory",
    "",
    paste0("inventoryPatchN = ", patch_number),
    paste0("inventoryPatchArea = ", patch_area),
    "",
    "#seed\tretz_id\ttreeId\tspeciesId\tage(years)\tdbh(cm)\tcrownA1[0,1]\tdbhIncrement(cm)\tslowGrowthIndex(0,1)"
  ), con = output_file)

  write.table(
    inventory,
    file = output_file,
    append = TRUE,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
}

# Sauvegarder l'inventaire
write_forceps_inventory(forceps_inventory, "C:/Capsis4/data/forceps/clementine/Test_itinerary/data/unif.inv")
