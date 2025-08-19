library(tidyverse)
library(fs)

################################################################################
## Import mean output --------------------------------------------------------##
################################################################################

base_path <- "C:/Capsis4/data/forceps/clementine/Test_repetition/"

output_dirs <- dir_ls(base_path, regexp = "output-cmd_\\d+\\.txt$", type = "directory")

file_id <- 0  # compteur global

all_data <- map_dfr(
  output_dirs,
  function(dir_path) {
    sim_files <- dir_ls(
      dir_path,
      regexp = "retz_act\\.climate_forceps\\.inv_simulation_\\d+mean\\.txt$",
      type = "file"
    )

    map_dfr(
      sim_files,
      function(file_path) {
        file_id <<- file_id + 1  # incrémente le compteur global
        read_table(file_path, show_col_types = FALSE) %>%
          mutate(simulation = file_id, folder = basename(dir_path), file = basename(file_path))
      }
    )
  }
)

# rename the #date by date in the colnames
colnames(all_data)[colnames(all_data) == "#date"] <- "date"
colnames(all_data)

# transformer les / et () en . dans les noms de colonnes
all_data <- all_data %>%
  rename_with(~ gsub("/", ".", .), everything()) %>%
  rename_with(~ gsub("\\(", ".", .), everything()) %>%
  rename_with(~ gsub("\\)", "", .), everything())

print(head(all_data))
# save as RData
save(all_data, file = "data/forceeps_output/repetition_mean.RData")

################################################################################
## Import productivityScene output -------------------------------------------##
################################################################################

# Paramètres
base_path <- "C:/Capsis4/data/forceps/clementine/Test_repetition/"
groupes <- c(1, 5, 10, 20, 30, 50, 75, 100, 150)
nb_rep <- 10
variable_name <- "adultProdBasalArea_m2_ha"

# Étape 1 : construire la liste complète des fichiers simulation
output_dirs <- dir_ls(base_path, regexp = "output-cmd_\\d+\\.txt$", type = "directory")

# Liste de tous les fichiers de simulation
all_sim_files <- map_dfr(output_dirs, function(dir_path) {
  sim_files <- dir_ls(
    dir_path,
    regexp = "retz_act\\.climate_forceps\\.inv_simulation_\\d+productivityScene\\.txt$",
    type = "file"
  )
  tibble(file_path = sim_files, folder = basename(dir_path))
}) %>%
  mutate(simulation_id = row_number())  # identifiant unique par simulation

# Vérification du total
total_sims <- nrow(all_sim_files)
message("Nombre total de simulations disponibles : ", total_sims)

# Étape 2 : boucle d’échantillonnage et agrégation immédiate
set.seed(123)
global_df <- data.frame()

for (m in 1:nb_rep) {
  one_rep <- data.frame()
  remaining_ids <- all_sim_files$simulation_id

  for (n in groupes) {
    if (length(remaining_ids) < n) {
      warning("Pas assez de simulations restantes pour n = ", n)
      break
    }

    selected_ids <- sample(remaining_ids, n, replace = FALSE)
    selected_files <- all_sim_files %>% filter(simulation_id %in% selected_ids)
    remaining_ids <- setdiff(remaining_ids, selected_ids)

    temp_df <- map_dfr(selected_files$file_path, function(file_path) {
      df <- tryCatch(read_table(file_path, show_col_types = FALSE, skip = 11), error = function(e) return(NULL))
      if (is.null(df)) return(NULL)

      # Nettoyage
      colnames(df)[colnames(df) == "#date"] <- "date"
      colnames(df) <- gsub("/", ".", colnames(df))
      colnames(df) <- gsub("\\(", ".", colnames(df))
      colnames(df) <- gsub("\\)", "", colnames(df))

      if (!all(c("date", "speciesShortName", variable_name) %in% colnames(df))) return(NULL)

      # Agrégation immédiate
      df %>%
        group_by(date, speciesShortName) %>%
        summarise(
          mean_val = mean(.data[[variable_name]], na.rm = TRUE),
          dead_total = sum(deadNumber_ha, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(n = n, m = m) %>%
        bind_rows(
          df %>%
            group_by(date) %>%
            summarise(
              mean_val = sum(.data[[variable_name]], na.rm = TRUE),
              dead_total = sum(deadNumber_ha, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(speciesShortName = "Tot", n = n, m = m)
        )
    })

    one_rep <- bind_rows(one_rep, temp_df)
  }

  global_df <- bind_rows(global_df, one_rep)
}

# Sauvegarde
save(global_df, file = "data/forceeps_output/repetition_productivityScene.RData")