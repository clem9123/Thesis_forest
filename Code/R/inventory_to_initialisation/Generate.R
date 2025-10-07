#    FORCEEPS inventory generation for distribution and species proportion testing
#         Author: Clementine de Montgolfier (August 2025)
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
#
# Script objective:
# Generate FORCEEPS inventories with different distribution types and species proportions
# for testing the impact of inventory sampling methods on simulation results.
# This includes testing uniform, normal distributions and fixed vs random species proportions.
#-------------------------------------------------------------------------------

## Parameters and data ---------------------------------------------------------
#------------------------------------------------------------------------------#

# Single patch simulation parameters
patch_id = "RETZ_00102_04" # Specific Retz forest patch identifier
patcharea = 1000           # Patch area in m²
patchnumber = 1            # Patch numbering for ForCEEPS
climate_file = "retz_act.climate" # Climate data file for current conditions in Retz forest
scenario = "80_3_1_25_FSyl-80"   # Management scenario code

# Load Retz forest inventory data
load("data/forest_data.RData")
Retz <- forest_data %>% as.data.frame() %>% select(-geometry)

# Correspondence table between common species names and ForCEEPS codes
corresponding.species <- 
read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

# Random seeds for replication (10 different seeds for robust testing)
seeds = c(332, 124, 102, 895, 869, 777, 969, 449, 131, 704)

## Inventory testing variables -------------------------------------------------
#------------------------------------------------------------------------------#

# Different distribution types to test for tree diameter sampling
distribution_types = c("unif", "normal_4", "normal_6", "mean", "min", "max")
# Species proportion methods: fixed (based on real data) vs random sampling
species_proportion = c("fixe", "random")

# Create all combinations of distribution types and species proportion methods
combinations <- 
  expand.grid(
    distribution = distribution_types,
    species_proportion = "fixe") %>%    # Test all distributions with fixed proportions
  cbind(repetition = 0) %>%
  bind_rows(data.frame(                 # Add random proportion tests with mean distribution
    distribution = rep("mean", 5),
    species_proportion = rep("random", 5),
    repetition = 1:5))                  # 5 repetitions for random sampling

# Total number of inventory combinations to generate
nrow(combinations)

## Generate inventory combinations ---------------------------------------------
#------------------------------------------------------------------------------#

# Initialize a list to store all generated inventories
all_inventories <- list()

# Loop through each combination of distribution type and species proportion method
for (i in seq_len(nrow(combinations))) {
  distribution <- combinations$distribution[i]
  species_proportion <- combinations$species_proportion[i]
  repetition <- combinations$repetition[i]
  
  # Generate inventory using simulation function from inventory_utils.R
  results <- simulate_inventory_for_patch(patch_id, distribution, species_proportion)
  
  # Format inventory data for ForCEEPS input format
  forceps_inv <- format_to_forceps(results$inventory, patcharea)
  
  # Add identification columns to track combination parameters
  forceps_inv$distribution <- distribution
  forceps_inv$species_proportion <- species_proportion
  forceps_inv$repetition <- repetition
  
  # Store in the master list
  all_inventories[[length(all_inventories) + 1]] <- forceps_inv
  
  # Save individual inventory file for ForCEEPS simulation
  output_file <- paste0(base_path, "/data/inventaires/", distribution, "_", species_proportion, "_", repetition, ".inv")
  write_forceps_inventory(forceps_inv %>% select(-c(distribution, species_proportion, repetition)), output_file, patcharea, patchnumber)
}

# Merge all inventories into a single data.frame for analysis
Inventories <- do.call(rbind, all_inventories)

# Save complete inventory table as .RData for further analysis
save(Inventories, file = "data/forceeps_output/inventory.RData")


# Define potential species list in ForCEEPS format (see ForCEEPS documentation)
potential_species <- "17 21 23 14 18 13 33 31 5"

## Generate site file ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Update ForCEEPS site parameters using patch-specific data
update_forceps_parameters(
  filebase_path = paste0(base_path, "/data/RETZ_00102_04.site"),
  updates = list(
    siteBucketSize = round(results$RUM,0), # Soil water holding capacity
    siteLatitude = 48.5,                   # Retz forest latitude
    siteLongitude = 2.2                    # Retz forest longitude
  )
)

## Generate setup files and command files -------------------------------------
#------------------------------------------------------------------------------#

# Generate setup and command files for each random seed
for (i in seeds){
  # Create ForCEEPS setup file with specific random seed
  update_forceps_parameters(
    filebase_path = "data/forceps.setup",
    updates = list(randomSeed = i),
    output_base_path = paste0(base_path, "/data/forceps_", i, ".setup")
  )

  # Initialize command file with setup parameters (uses function from output_utils.R)
  write_command_file(
    output_file = paste0(base_path, "/cmd_", i, ".txt"),
    file_setup = paste0("data/forceps_", i, ".setup")
  )

  # Add simulation lines for each inventory combination
  for(j in seq_len(nrow(combinations))) {
    distribution <- combinations$distribution[j]
    species_proportion <- combinations$species_proportion[j]
    repetition <- combinations$repetition[j]
    
    # Define inventory file path
    inventory_file <- 
      paste0( "inventaires/", distribution, "_", species_proportion, "_",
        repetition, ".inv")
    
    # Write simulation command line with all required parameters
    write(paste0(
      "RETZ_00102_04.site\t",    # Site file
      climate_file, "\t",         # Climate data
      inventory_file, "\t",       # Inventory file
      potential_species, "\t",    # Potential species list
      scenario),                  # Management scenario
      file = paste0(base_path, "/cmd_", i, ".txt"), append = TRUE)
  }
}
