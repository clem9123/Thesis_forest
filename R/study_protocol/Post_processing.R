# Post-processing des résultats Forceps
# Objectif : Regrouper les simulations par scénario (10 simulations par scénario)
# et calculer les moyennes pour réduire le nombre de fichiers

# Libraries ----------------------------------------------------------------
library(tidyverse)
library(fs)
source("R/output_utils.R")

# Configuration -------------------------------------------------------------
base_path <- "C:/Capsis4/data/forceps/clementine/Test_protocole/"
output_dir <- "output/"

# Créer le dossier de sortie s'il n'existe pas
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Fonction simplifiée utilisant import_output_scene ----------------------
process_with_import_function <- function(output_type, base_path, output_dir) {
  cat("Traitement des fichiers", output_type, "avec import_output_scene\n")
  
  # Utiliser la fonction existante pour charger toutes les données
  all_data <- import_output_scene(
    output_name = output_type,
    base_path = base_path,
    output_file = NULL  # On ne sauvegarde pas encore
  )
  
  if (is.null(all_data) || nrow(all_data) == 0) {
    cat("Aucune donnée trouvée pour", output_type, "\n")
    return(FALSE)
  }
  
  # Extraire le numéro de scénario depuis la colonne scenario
  all_data$scenario_num <- as.numeric(gsub("simulation_(\\d+)", "\\1", all_data$scenario))
  
  # Déterminer quel scénario (itinéraire) chaque simulation appartient
  # Scénario 1: simulations 1-10, Scénario 2: simulations 11-20, Scénario 3: simulations 21-30
  all_data$itinerary <- ceiling(all_data$scenario_num / 10)
  
  # Traiter chaque itinéraire séparément
  for (itinerary_num in 1:3) {
    cat("  Traitement itinéraire", itinerary_num, "\n")
    
    # Filtrer les données pour cet itinéraire
    itinerary_data <- all_data %>%
      filter(itinerary == itinerary_num)
    
    if (nrow(itinerary_data) == 0) {
      cat("    Aucune donnée pour l'itinéraire", itinerary_num, "\n")
      next
    }
    
    # Calculer les moyennes selon le type de fichier
    if (output_type == "productivityScene" && "speciesShortName" %in% colnames(itinerary_data)) {
      grouped_data <- itinerary_data %>%
        group_by(plot_id, date, speciesShortName) %>%
        summarise(
          across(where(is.numeric) & !matches("scenario_num|itinerary"), mean, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(scenario = paste0("itinerary_", itinerary_num))
    } else {
      # Pour mean, complete ou productivityScene sans speciesShortName
      group_cols <- intersect(c("plot_id", "date"), colnames(itinerary_data))
      
      if (length(group_cols) > 0) {
        grouped_data <- itinerary_data %>%
          group_by(across(all_of(group_cols))) %>%
          summarise(
            across(where(is.numeric) & !matches("scenario_num|itinerary"), mean, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(scenario = paste0("itinerary_", itinerary_num))
      } else {
        grouped_data <- itinerary_data %>%
          summarise(
            across(where(is.numeric) & !matches("scenario_num|itinerary"), mean, na.rm = TRUE)
          ) %>%
          mutate(scenario = paste0("itinerary_", itinerary_num))
      }
    }
    
    # Créer le nom de fichier
    plot_ids <- unique(grouped_data$plot_id)
    plot_id_str <- paste(plot_ids, collapse = "_")
    
    output_file <- file.path(output_dir, paste0(plot_id_str, "_itinerary", itinerary_num, "_", output_type, ".RData"))
    save(grouped_data, file = output_file)
    cat("    Sauvegardé:", output_file, "\n")
    
    rm(grouped_data)
    gc()
  }
  
  return(TRUE)
}

# Exécution du post-traitement ---------------------------------------------
cat("=== Début du post-traitement ===\n")

# Traiter chaque type de sortie
output_types <- c("productivityScene", "mean", "complete")

for (output_type in output_types) {
  cat("\n--- Traitement des fichiers", output_type, "---\n")
  
  success <- process_with_import_function(output_type, base_path, output_dir)
  
  if (!success) {
    cat("Échec du traitement pour", output_type, "\n")
  }
  
  # Pause pour libérer la mémoire
  Sys.sleep(1)
}

# Créer un fichier final combiné (optionnel) -------------------------------
cat("\n--- Création des fichiers combinés finaux ---\n")

for (output_type in output_types) {
  cat("Combinaison des scénarios pour", output_type, "\n")
  
  all_scenarios <- list()
  
  for (scenario_num in 1:3) {
    # Construire le nom de fichier avec le nouveau format
    temp_file <- list.files(output_dir, pattern = paste0(".*_itinerary", scenario_num, "_", output_type, "\\.RData$"), full.names = TRUE)
    
    if (length(temp_file) > 0) {
      load(temp_file[1])  # Charge la variable 'grouped_data'
      all_scenarios[[scenario_num]] <- grouped_data
      rm(grouped_data)
    }
  }
  
  if (length(all_scenarios) > 0) {
    final_data <- bind_rows(all_scenarios)
    
    # Créer le nom final avec tous les plot_ids
    all_plot_ids <- unique(final_data$plot_id)
    final_plot_id_str <- paste(all_plot_ids, collapse = "_")
    
    # Sauvegarder le fichier final avec le nouveau format
    final_file <- file.path(output_dir, paste0(final_plot_id_str, "_all_itineraries_", output_type, ".RData"))
    save(final_data, file = final_file)
    cat("Fichier final sauvegardé:", final_file, "\n")
    
    rm(final_data, all_scenarios)
    gc()
  }
}

cat("\n=== Post-traitement terminé ===\n")