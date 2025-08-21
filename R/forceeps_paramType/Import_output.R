#    FORCEEPS output import for paramType parameter impact analysis
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective: Import and process FORCEEPS output files for paramType parameter impact analysis and create a comprehensive dataset (paramType_complete.RData) for analyses
#-------------------------------------------------------------------------------

# Base path to FORCEEPS working directory
forceeps_path = "C:/Capsis4/data/forceps/clementine/"
analyse_name = "ParamType"
base_path = paste0(forceeps_path, analyse_name, "/")

source("R/utils/output_utils.R")

# Use the function from output_utils.R to read "complete" ForCEEPS output files
data_new <- import_output_scene(
  output_name = "complete",
  base_path = base_path,
  output_file = "data/forceeps_output/paramType_complete.RData"
)
  
# Create type and rep columns from the scenario column
# The scenario contains "simulation_X" where X is the simulation number
data_new <- data_new %>%
  mutate(
    # Extract simulation number
    sim_number = as.numeric(str_extract(scenario, "\\d+"))
  ) %>%
  mutate(
    # Calculate type and rep (see how it was generated in Generate.R)
    # Logic: sim 1-10 = type 0.0, rep 1-10
    #        sim 11-20 = type 0.2, rep 1-10
    #        etc.
    type = ((sim_number - 1) %/% 10) * 0.2,
    rep = ((sim_number - 1) %% 10) + 1
  ) %>%
  select(-sim_number)  # Remove temporary column

itinerary_data <- data_new
save(itinerary_data, file = "data/forceeps_output/paramType_complete.RData")