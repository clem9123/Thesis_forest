#    FORCEEPS output import for study protocol multiscale forest functions analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective: Import and process FORCEEPS output files for study protocol analysis
# and create comprehensive datasets for different management scenarios (clearcut, irregular, no intervention)
#-------------------------------------------------------------------------------

## Import output data ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Function to process different output types using import_output_scene utility
process_output_by_itinerary <- function(output_type, base_path, output_dir) {
  cat("Processing", output_type, "files with import_output_scene\n")
  
  # Use the existing function to load all data (uses function from output_utils.R)
  all_data <- import_output_scene(
    output_name = output_type,
    base_path = base_path,
    output_file = NULL  # Don't save yet
  )
  
  if (is.null(all_data) || nrow(all_data) == 0) {
    cat("No data found for", output_type, "\n")
    return(FALSE)
  }
  
  # Extract scenario number from the scenario column
  all_data$scenario_num <- as.numeric(gsub("simulation_(\\d+)", "\\1", all_data$scenario))
  
  # Determine which management scenario (itinerary) each simulation belongs to
  # Scenario 1: simulations 1-10, Scenario 2: simulations 11-20, Scenario 3: simulations 21-30
  all_data$itinerary <- ceiling(all_data$scenario_num / 2)
  
  # Process each itinerary separately
  for (itinerary_num in 1:3) {
    cat("  Processing itinerary", itinerary_num, "\n")
    
    # Filter data for this itinerary
    itinerary_data <- all_data %>%
      filter(itinerary == itinerary_num)
    
    if (nrow(itinerary_data) == 0) {
      cat("    No data for itinerary", itinerary_num, "\n")
      next
    }
    
    # Calculate means according to file type
    if (output_type == "productivityScene" && "speciesShortName" %in% colnames(itinerary_data)) {
      # Group by plot, date, and species for productivity scene data
      grouped_data <- itinerary_data %>%
        group_by(plot_id, date, speciesShortName) %>%
        summarise(
          across(where(is.numeric) & !matches("scenario_num|itinerary"), mean, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(scenario = paste0("itinerary_", itinerary_num))
    } else {
      # For mean, complete or productivityScene without speciesShortName
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
    
    # Create filename with plot IDs
    plot_ids <- unique(grouped_data$plot_id)
    plot_id_str <- paste(plot_ids, collapse = "_")
    
    output_file <- file.path(output_dir, paste0(plot_id_str, "_itinerary", itinerary_num, "_", output_type, ".RData"))
    save(grouped_data, file = output_file)
    cat("    Saved:", output_file, "\n")
    
    rm(grouped_data)
    gc()
  }
  
  return(TRUE)
}

## Post-processing by itinerary -----------------------------------------------
#------------------------------------------------------------------------------#
#
## Process output files and separate by management itinerary
#cat("=== Starting post-processing ===\n")
#
## Define output types to process
#output_types <- c("productivityScene", "mean")
#
#for (output_type in output_types) {
#  cat("\n--- Processing", output_type, "files ---\n")
#  
#  success <- process_output_by_itinerary(output_type, base_path, "data/forceeps_output/")
#  
#  if (!success) {
#    cat("Failed processing for", output_type, "\n")
#  }
#  
#  # Pause to free memory
#  Sys.sleep(1)
#}
#
### Create combined final files ------------------------------------------------
##------------------------------------------------------------------------------#
#
## Create final combined files for all scenarios (optional)
#cat("\n--- Creating combined final files ---\n")
#
#for (output_type in output_types) {
#  cat("Combining scenarios for", output_type, "\n")
#  
#  all_scenarios <- list()
#  
#  for (scenario_num in 1:3) {
#    # Build filename with the new format
#    temp_file <- list.files("data/forceeps_output/", 
#                           pattern = paste0(".*_itinerary", scenario_num, "_", output_type, "\\.RData$"), 
#                           full.names = TRUE)
#    
#    if (length(temp_file) > 0) {
#      load(temp_file[1])  # Load the 'grouped_data' variable
#      all_scenarios[[scenario_num]] <- grouped_data
#      rm(grouped_data)
#    }
#  }
#  
#  if (length(all_scenarios) > 0) {
#    final_data <- bind_rows(all_scenarios)
#    
#    # Create final name with all plot_ids
#    all_plot_ids <- unique(final_data$plot_id)
#    final_plot_id_str <- paste(all_plot_ids, collapse = "_")
#    
#    # Save final file with the new format
#    final_file <- file.path("data/forceeps_output/", 
#                           paste0(final_plot_id_str, "_all_itineraries_", output_type, ".RData"))
#    save(final_data, file = final_file)
#    cat("Final file saved:", final_file, "\n")
#    
#    rm(final_data, all_scenarios)
#    gc()
#  }
#}
#
#cat("\n=== Post-processing completed ===\n")
#