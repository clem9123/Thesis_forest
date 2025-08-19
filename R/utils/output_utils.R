#' Process Productivity Scene Output Files (With Plot ID Extraction)
#'
#' This function loads, samples, and aggregates simulation outputs from
#' productivity scene files with repeated simulations. Each simulation is used
#' independently in each repetition without grouping. It also extracts the plot
#' ID from the filename.
#'
#' @param base_path Character. Path to the base directory containing simulation output folders.
#' @param variable_to_aggregate Character. The variable name to aggregate (e.g., "adultProdBasalArea_m2_ha").
#' @param output_file Character. File path to save the final aggregated dataset (as .RData).
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return Saves an `.RData` file containing a `data.frame` named `aggregated_data`.
#' @export
import_output_scene <- function(
  output_name = "productivityScene",
  base_path,
  output_file) {
  
  
  
  library(fs)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)

  # Set skip option
  skip <- ifelse(output_name == "mean", 1, 14)


  # Step 1: Locate files
  output_dirs <- dir_ls(base_path, regexp = "output-cmd_\\d+\\.txt$", type = "directory")

  all_sim_files <- map_dfr(output_dirs, function(dir_path) {
    sim_files <- dir_ls(
      dir_path,
      regexp = paste0("Retz_act\\.climate_inventaires_RETZ_\\d+_\\d+\\.inv_simulation_\\d+", output_name, "\\.txt$"),
      type = "file"
    )
    tibble(
      file_path = sim_files,
      folder = basename(dir_path),
      plot_id = str_extract(sim_files, "RETZ_[0-9_]+"),
      simul = str_extract(sim_files, "simulation_\\d+")
    )
  }) %>%
    mutate(simulation_id = row_number())

  message("Total simulation files found: ", nrow(all_sim_files))

  aggregated_data <- data.frame()

  print(all_sim_files %>% select(-file_path))

  for (i in 1:nrow(all_sim_files)) {
    message("File number: ", i)

    file_path <- all_sim_files$file_path[i]
    plot_id <- all_sim_files$plot_id[i]
    scenario <- all_sim_files$simul[i]

    df <- read_table(file_path, show_col_types = FALSE, skip = skip)
    df$plot_id = plot_id
    df$scenario = scenario

    if (is.null(df)) return(NULL)

    colnames(df)[colnames(df) == "#date"] <- "date"
    colnames(df) <- gsub("/", ".", colnames(df))
    colnames(df) <- gsub("[()]", "", colnames(df))

#      required_vars <- c("date", "speciesShortName")
#      if (!all(required_vars %in% colnames(df))) return(NULL)
#
#      df <- df %>% select(all_of(required_vars))
#
#      species_summary <- df %>%
#        group_by(date, speciesShortName) %>%
#        summarise(
#          mean_val = mean(.data[[variable_to_aggregate]], na.rm = TRUE),
#          dead_total = sum(deadNumber_ha, na.rm = TRUE),
#          .groups = "drop"
#        ) %>%
#        mutate(repetition = rep_idx, plot_id = plot_id)
#
#      total_summary <- df %>%
#        group_by(date) %>%
#        summarise(
#          mean_val = sum(.data[[variable_to_aggregate]], na.rm = TRUE),
#          dead_total = sum(deadNumber_ha, na.rm = TRUE),
#          .groups = "drop"
#        ) %>%
#        mutate(speciesShortName = "Tot", repetition = rep_idx, plot_id = plot_id)
#
#      bind_rows(species_summary, total_summary)
#    })

    aggregated_data <- bind_rows(aggregated_data, df)
  }

  return(aggregated_data)#, file = output_file)
  message("Aggregated data saved to: ", output_file)
}
