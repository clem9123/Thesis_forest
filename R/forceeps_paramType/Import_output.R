# Prepare scenario and files to study different itineraries
# 12/06/2025
# Clémentine de Montgolfier
#-------------------------------------------------------------------------------

base_path <- "C:/Capsis4/data/forceps/clementine/Test_itinerary/"

source("R/utils/output_utils.R")

# Utiliser la fonction d'output_utils.R pour lire les fichiers "complete"
data_new <- import_output_scene(
  output_name = "complete",
  base_path = base_path,
  output_file = "data/forceeps_output/paramType_complete.RData"
)
  
# Créer les colonnes type et rep à partir de la colonne scenario
# Le scenario contient "simulation_X" où X est le numéro de simulation
data_new <- data_new %>%
  mutate(
    # Extraire le numéro de simulation
    sim_number = as.numeric(str_extract(scenario, "\\d+"))
  ) %>%
  mutate(
    # Calculer type et rep (see how it was generated in generate)
    # Logique: sim 1-10 = type 0.0, rep 1-10
    #         sim 11-20 = type 0.2, rep 1-10
    #         etc.
    type = ((sim_number - 1) %/% 10) * 0.2,
    rep = ((sim_number - 1) %% 10) + 1
  ) %>%
  select(-sim_number)  # Supprimer la colonne temporaire

itinerary_data <- data_new
save(itinerary_data, file = "data/forceeps_output/paramType_complete.RData")

# Note: Vous devrez peut-être d'abord adapter output_utils.R pour qu'il reconnaisse 
# vos fichiers "complete.txt" au lieu de "productivityScene.txt"