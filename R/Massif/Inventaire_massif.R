## Library ---------------------------------------------------------------------
#------------------------------------------------------------------------------#
library(tidyverse)
library(sf)
source("R/inventory_utils.R")

## Parameters and data ---------------------------------------------------------
#------------------------------------------------------------------------------#

# paramètres
patcharea = 1000
patchnumber = 1
path = "C:/Capsis4/data/forceps/clementine/Test_Retz"

load("data/forest_data.RData")
Retz <- forest_data %>% as.data.frame()
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

## Inventory variables ---------------------------------------------------------
#------------------------------------------------------------------------------#

distribution_types = "mean"
species_proportion = "fixe"

## Inventory status study ------------------------------------------------------
#------------------------------------------------------------------------------#

process_lignes <- function(Retz, distribution, species_proportion, patcharea) {
    Retz$inventory_status <- NA
    all_lignes <- unique(Retz$Identifiant.peuplement.élémentaire)
    
    for (ligne in all_lignes) {
        results <- tryCatch(
            simulate_inventory_for_patch(ligne, distribution, species_proportion),
            error = function(e) e
        )
        if (inherits(results, "error")) {
            Retz$inventory_status[Retz$Identifiant.peuplement.élémentaire == ligne] <- results$message
            next
        }
        forceps_inv <- tryCatch(
            format_to_forceps(results$inventory, patcharea),
            error = function(e) e
        )
        if (inherits(forceps_inv, "error")) {
            Retz$inventory_status[Retz$Identifiant.peuplement.élémentaire == ligne] <- forceps_inv$message
            next
        }
        Retz$inventory_status[Retz$Identifiant.peuplement.élémentaire == ligne] <- nrow(results$inventory)
    }
    return(Retz)
}

Retz <- process_lignes(Retz, distribution_types, species_proportion, patcharea)

table(Retz$inventory_status)

# puit geometry back as coord
ggplot(Retz %>%
    st_as_sf(), aes(fill = inventory_status)) +
  geom_sf() +
  labs(title = "Status of inventories in Retz forest",
       fill = "Inventory Status") +
  theme_minimal()

## Generate all inventory ------------------------------------------------------
#------------------------------------------------------------------------------#

# Create a RetzId.inv and RetsId.site for every ligne
# use the function simulate_inventory_for_patch, format_to_forceps and
# write_forceps_inventory from inventory_utils.R
# put them in C:\Capsis4\data\forceps\clementine\Test_Retz\data\inventaires
# and C:\Capsis4\data\forceps\clementine\Test_Retz\data\sites respectively

path = "C:/Capsis4/data/forceps/clementine/Test_Retz/data"

for (i in 1:nrow(Retz)){
    ligne <- Retz$Identifiant.peuplement.élémentaire[i]
    
    # Simulate inventory
    results <- simulate_inventory_for_patch(ligne, distribution_types, species_proportion)
    
    # Format to Forceps inventory
    forceps_inv <- format_to_forceps(results$inventory, patcharea)
    
    # Write inventory and site files
    write_forceps_inventory(
        inventory = forceps_inv, 
        output_file = paste0(path, "/inventaires/", ligne, ".inv")
    )

    # write the site file (using updated parameters)
    update_forceps_parameters(
        "data/forceps.site",
        updates = list(
            "siteName" = ligne,
            "siteBucketSize" = results$RUM
        ),
        output_path = paste0(path, "/sites/", ligne, ".site")
    )

}

write_command_file <- function(output_file, file_setup) {
	writeLines(c(
		"# Forceps script command file, format SimulationCommandReader2 (Xavier Morin)",
		"",
		paste0("setupFileName = ", file_setup),
		"numberOfYearsToBeJumped = 1",
		"exportTimeStep = 1",
		"",
		"#siteFileName\tclimateFileName\tinventoryFileName\tpotentialSpeciesList\tScenario"
	), con = output_file)
}

add_scenario_to_cmd <- function(output_file, scenario_name, inventory_file, site_file, climate_file, potential_species) {
	cmd_line <- paste(site_file, climate_file, inventory_file, potential_species, scenario_name, sep = "\t")
	write(cmd_line, file = output_file, append = TRUE)
}

# write the command files for all (in 20 different files, so that I can run them in parallel)

n_files <- 20
seed = 3400
path = "C:/Capsis4/data/forceps/clementine/Test_Retz"
setup_file <- "data/forceps.setup"
climate_file <- "retz_act.climate"
potential_species <- "17 21 31 5 23 33 14 18 13"
scenario_name <- "80_3_1_25_FSyl-80"

# Découper les lignes de Retz en n fichiers
retz_split <- split(Retz$Identifiant.peuplement.élémentaire, 
                    cut(seq_along(Retz$Identifiant.peuplement.élémentaire),
                    n_files, labels = FALSE))

# Générer les fichiers
for (file_idx in seq_along(retz_split)) {
    
    cmd_file <- file.path(path, paste0("cmd_", file_idx, ".txt"))

    # Écrire le header + setup
    write_command_file(
        output_file = cmd_file,
        file_setup = setup_file
    )   
    for (ligne in retz_split[[file_idx]]) {
        
        # Définir les chemins relatifs attendus par Forceps
        inventory_file <- paste0("/inventaires/", ligne, ".inv")
        site_file <- paste0("/sites/", ligne, ".site")
        
        # Ajouter la ligne de scénario
        write(paste0(
            seed, "\t",
            site_file, "\t",
            climate_file, "\t",
            inventory_file, "\t",
            potential_species, "\t",
            scenario_name),
            file = cmd_file, append = TRUE
        )
    }
}
