#    FORCEEPS output import for replicates analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective: Import and process FORCEEPS output files for replicates analysis
# and create comprehensive datasets (repetition_mean.RData and repetition_productivityScene.RData) for analyses
#-------------------------------------------------------------------------------

## Import mean output ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Get output directories from the ForCEEPS simulation results
output_dirs <- dir_ls(base_path, regexp = "output-cmd_\\d+\\.txt$", type = "directory")

file_id <- 0  # Global counter for simulations

# Import all mean output files from multiple command file runs
all_data <- map_dfr(
  output_dirs,
  function(dir_path) {
    # Get simulation files with mean output
    sim_files <- dir_ls(
      dir_path,
      regexp = "retz_act\\.climate_forceps\\.inv_simulation_\\d+mean\\.txt$",
      type = "file"
    )

    # Read each simulation file and add metadata
    map_dfr(
      sim_files,
      function(file_path) {
        file_id <<- file_id + 1  # Increment global counter
        read_table(file_path, show_col_types = FALSE) %>%
          mutate(simulation = file_id, folder = basename(dir_path), file = basename(file_path))
      }
    )
  }
)

# Clean column names: rename #date to date and replace special characters
colnames(all_data)[colnames(all_data) == "#date"] <- "date"

# Transform / and () to . in column names for R compatibility
all_data <- all_data %>%
  rename_with(~ gsub("/", ".", .), everything()) %>%
  rename_with(~ gsub("\\(", ".", .), everything()) %>%
  rename_with(~ gsub("\\)", "", .), everything())

print(head(all_data))
# Save as RData file for further analysis
save(all_data, file = "data/forceeps_output/repetition_mean.RData")

## Import productivityScene output ---------------------------------------------
#------------------------------------------------------------------------------#

# Parameters for productivity scene analysis
nb_rep <- 10
groupes <- c(1, 5, 10, 20, 30, 50, 75, 100, 150)
variable_name <- "adultProdBasalArea_m2_ha"

# Step 1: Build complete list of simulation files
output_dirs <- dir_ls(base_path, regexp = "output-cmd_\\d+\\.txt$", type = "directory")

# List of all simulation files
all_sim_files <- map_dfr(output_dirs, function(dir_path) {
  sim_files <- dir_ls(
    dir_path,
    regexp = "retz_act\\.climate_forceps\\.inv_simulation_\\d+productivityScene\\.txt$",
    type = "file"
  )
  tibble(file_path = sim_files, folder = basename(dir_path))
}) %>%
  mutate(simulation_id = row_number())  # Unique identifier per simulation

# Verification of total simulations available
total_sims <- nrow(all_sim_files)
message("Total number of available simulations: ", total_sims)

# Étape 2 : boucle d’échantillonnage et agrégation immédiate
set.seed(123)
global_df <- data.frame()

for (m in 1:nb_rep) {
  one_rep <- data.frame()
  remaining_ids <- all_sim_files$simulation_id

  for (n in groupes) {
    if (length(remaining_ids) < n) {
      warning("Not enough remaining simulations for n = ", n)
      break
    }

    # Sample simulations without replacement
    selected_ids <- sample(remaining_ids, n, replace = FALSE)
    selected_files <- all_sim_files %>% filter(simulation_id %in% selected_ids)
    remaining_ids <- setdiff(remaining_ids, selected_ids)

    # Process selected files with immediate aggregation
    temp_df <- map_dfr(selected_files$file_path, function(file_path) {
      df <- tryCatch(read_table(file_path, show_col_types = FALSE, skip = 11), error = function(e) return(NULL))
      if (is.null(df)) return(NULL)

      # Clean column names
      colnames(df)[colnames(df) == "#date"] <- "date"
      colnames(df) <- gsub("/", ".", colnames(df))
      colnames(df) <- gsub("\\(", ".", colnames(df))
      colnames(df) <- gsub("\\)", "", colnames(df))

      if (!all(c("date", "speciesShortName", variable_name) %in% colnames(df))) return(NULL)

      # Immediate aggregation by species and date
      df %>%
        group_by(date, speciesShortName) %>%
        summarise(
          mean_val = mean(.data[[variable_name]], na.rm = TRUE),
          dead_total = sum(deadNumber_ha, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(n = n, m = m) %>%
        bind_rows(
          # Add total across all species
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

# Save productivity scene analysis results
save(global_df, file = "data/forceeps_output/repetition_productivityScene.RData")