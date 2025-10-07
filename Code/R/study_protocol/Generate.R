#    FORCEEPS simulation itineraries generation for exploration of multiscale forest functions
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective:
# Generate 10 FORCEEPS inventories (#10 chosen to speed up exploration analyses)
# and command file with varying itineraries (clearcut, irregular, no intervention)
# in order to run ForCEEPS simulations and analyse multiscale dynamics.
#-------------------------------------------------------------------------------

# Correspondence table between common species names and ForCEEPS codes
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

# Load Retz forest inventory data
load("data/forest_data.RData")
Retz <- forest_data  %>%
  filter(Structure.et.occupation.du.sol %in% c("F", "I")) %>%
  slice_sample(n = 10) # 10 patches are chosen for this test

# Simulation parameters
set.seed(3400)
seed <- 3400
tot_simul_time = 150 # Total simulation time (years) to visualise trajectory and analyse dynamics
# Potential species list in ForCEEPS format (see ForCEEPS documentation)
potential_species <- "17 21 31 5 23 14 18 13"
# Species-specific parameters for silvicultural management
params <- tibble::tibble(
  essence = c("CHS", "CHP", "HET", "CHA", "P.L"),
  rotation_sp = c(150, 150, 100, 80, 60), # Rotation age for each species
  ba_irregulier = c(12, 12, 16, 20, 25) # After cut basal area for irregular management
)
n_rep = 1 # Number of repetitions of ForCEEPS simulations for each itinerary

## Set up ForCEEPS directory ---------------------------------------------------
#------------------------------------------------------------------------------#

initialise_forceeps_folder(forceeps_path, analyse_name, overwrite = TRUE)

## Generate all inventory ------------------------------------------------------
#------------------------------------------------------------------------------#

# Create inventory and site files for every Retz patch
# Generate inventories and sites using function from inventory_utils.R
generate_all_inventories(
  Retz,
  distribution_types = "mean",
  species_proportion = "fixe",
  patcharea = 1000,
  patchnumber = 1,
  path = base_path
)

## Generate all command files --------------------------------------------------
#------------------------------------------------------------------------------#

# Generate command files for each Retz patch with different management scenarios
for (i in seq_along(Retz$Identifiant.peuplement.elementaire)) {
  retz_id <- Retz$Identifiant.peuplement.elementaire[i]
  cmd_file <- file.path(base_path, paste0("cmd_", i, ".txt"))
  
  # Write command file header (uses function from output_utils.R)
  write_command_file(
    output_file = cmd_file,
    file_setup = "data/forceps.setup"
  )
  
  # Define file paths for this patch
  inventory_file <- paste0("inventories/", retz_id, ".inv")
  site_file <- paste0("sites/", retz_id, ".site")
  
  # Generate management scenarios for this patch (uses function from itinerary_utils.R)
  scenario <- generate_scenario(
    Retz$Essence.determinant.la.sylviculture[i],
    Retz$median_age[i],
    species_proportion(Retz$Identifiant.peuplement.elementaire[i])
  )

  # Write simulation lines for each scenario and repetition
  for (scen in scenario) {
    for (seed_offset in 1:n_rep) {
      current_seed <- seed + seed_offset
      # Write a simulation line
      write(
        paste0(
          current_seed, "\t",
          site_file, "\t",
          "retz_act.climate", "\t", # Climate data file for current conditions in Retz forest
          inventory_file, "\t",
          potential_species, "\t",
          scen
        ),
        file = cmd_file, append = TRUE
      )
    }
  }
}