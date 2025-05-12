# run Inventaire_functions.R
source("R/Inventaire_functions.R")

# paramètres
ligne = "RETZ_00102_04"
patcharea = 1000
patchnumber = 1
site_file = "forceps.site"
climate_file = "forceps.climate.bern"
scenario = "80_3_1_25_FSyl-80"

# importation des données
Retz <- import_data()
corresponding.species <- read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

# Inventory and foprceps init file

# For different inventory technique {
inv_types = c("unif", "normal_4", "normal_6", "mean", "min", "max")
for (inv_type in inv_types) {
    cmd_path = paste0("output/Retz_cmd_", inv_type, ".txt")
    write_cmd_file(cmd_path)
    output_file = paste0("output/forceps_", inv_type, ".inv")
    inventory <- full_inventory(ligne, inv_type)
    f_inv <- forceps_inv(inventory, patcharea)
    potential_species <- "17 14 23 13"#forceps_inv %>% select(speciesId) %>% unique() %>% pull() %>% paste(collapse = " ")
    file_inventory(f_inv, patcharea, patchnumber, output_file)
    for(i in 1:10){
        add_scenario_cmd(cmd_path, scenario, paste0("forceps_", inv_type, ".inv"), site_file, climate_file, potential_species)
    }
}
