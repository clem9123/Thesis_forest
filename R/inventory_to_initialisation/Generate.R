## Library ---------------------------------------------------------------------
#------------------------------------------------------------------------------#

source("R/inventory_utils.R")

## Parameters and data ---------------------------------------------------------
#------------------------------------------------------------------------------#

# paramètres
patch_id = "RETZ_00102_04"
patcharea = 1000
patchnumber = 1
climate_file = "retz_act.climate"
scenario = "80_3_1_25_FSyl-80"
path = "C:/Capsis4/data/forceps/clementine/Test_initialisation"

load("data/forest_data.RData")
Retz <- forest_data %>% as.data.frame() %>% select(-geometry)
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

## Inventory variables ---------------------------------------------------------
#------------------------------------------------------------------------------#

distribution_types = c("unif", "normal_4", "normal_6", "mean", "min", "max")
species_proportion = c("fixe", "random")

combinations <- 
  expand.grid(
    distribution = distribution_types,
    species_proportion = "fixe") %>% 
  cbind(repetition = 0) %>%
  bind_rows(data.frame(
    distribution = rep("mean", 5),
    species_proportion = rep("random", 5),
    repetition = 1:5))

nrow(combinations)

## Generate inventory ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Initialiser une liste pour stocker tous les inventaires
all_inventories <- list()

for (i in 1:nrow(combinations)) {
  distribution <- combinations$distribution[i]
  species_proportion <- combinations$species_proportion[i]
  repetition <- combinations$repetition[i]
  
  # Générer l'inventaire
  results <- simulate_inventory_for_patch(patch_id, distribution, species_proportion)
  
  # Formatter l'inventaire pour forceps
  forceps_inv <- format_to_forceps(results$inventory, patcharea)
  
  # Ajouter les colonnes d'identification
  forceps_inv$distribution <- distribution
  forceps_inv$species_proportion <- species_proportion
  forceps_inv$repetition <- repetition
  
  # Stocker dans la liste
  all_inventories[[length(all_inventories) + 1]] <- forceps_inv
  
  # Sauvegarder l'inventaire individuel
  output_file <- paste0(path, "/data/inventaires/", distribution, "_", species_proportion, "_", repetition, ".inv")
  write_forceps_inventory(forceps_inv %>% select(-c(distribution, species_proportion, repetition)), output_file, patcharea, patchnumber)
}

# Fusionner tous les inventaires en un seul data.frame
Inventories <- do.call(rbind, all_inventories)

# Sauvegarder le tableau complet en .RData
save(Inventories, file = "output/inventory.RData")


# Get potential species list in forceps format
potential_species <- potential_species <- "17 21 23 14 18 13 33 31 5"

# Generate site file -----------------------------------------------------------
#------------------------------------------------------------------------------#

update_forceps_parameters(
  filepath = paste0(path, "/data/RETZ_00102_04.site"),
  updates = list(
    siteBucketSize = round(results$RUM,0),
    siteLatitude = 48.5,
    siteLongitude = 2.2
  )
)

# Generate basic setup & cmd ---------------------------------------------------
#------------------------------------------------------------------------------#

# faire une liste de 10 random seed puis une boucle sur cette liste

seeds = c(332, 124, 102, 895, 869, 777, 969, 449, 131, 704)

for (i in seeds){
  update_forceps_parameters(
    filepath = "data/forceps.setup",
    updates = list(randomSeed = i),
    output_path = paste0(path, "/data/forceps_", i, ".setup")
  )

  write_command_file(
    output_file = paste0(path, "/cmd_", i, ".txt"),
    file_setup = paste0("data/forceps_", i, ".setup")
  )

  # add a line for each inventory
  for(j in 1:nrow(combinations)) {
    distribution <- combinations$distribution[j]
    species_proportion <- combinations$species_proportion[j]
    repetition <- combinations$repetition[j]
    
    inventory_file <- 
      paste0( "inventaires/", distribution, "_", species_proportion, "_",
        repetition, ".inv")
    
    write(paste0(
      "RETZ_00102_04.site\t",
      climate_file, "\t",
      inventory_file, "\t",
      potential_species, "\t",
      scenario), 
      file = paste0(path, "/cmd_", i, ".txt"), append = TRUE)
  }
}
