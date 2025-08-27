# ==============================================================================
# FORCEPS SIMULATION RUNNER - Forest Growth Modeling Utilities
# ==============================================================================
#
# Purpose:
# This script provides utilities for running FORCEPS forest growth simulations
# in parallel using the Capsis modeling platform. FORCEPS (Forest Growth 
# Simulator) is used to model forest dynamics and management scenarios.
#
# Key Features:
# - Parallel execution of multiple FORCEPS simulations
# - Automatic cleanup of previous simulation outputs
# - Dynamic core allocation for optimal performance
# - Integration with Capsis4 platform
#
# Dependencies:
# - Capsis4 installed at C:/Capsis4
# - FORCEPS module available in Capsis
# - R packages: parallel
#
# Required Variables:
# - analyse_name: String identifying the analysis/experiment name
# - n_commandFiles: Integer specifying the number of command files to process
#
# Author: Forest Thesis Project
# Last modified: August 2025
# ==============================================================================

# ==============================================================================
# CONFIGURATION AND SETUP
# ==============================================================================

# Define output directory structure based on analysis name
# This creates a hierarchical directory structure for organizing simulation results
output_base_path <- paste0("data/forceps/clementine/", analyse_name, "/")

# Set working directory to Capsis installation
# Capsis must be run from its installation directory to access all modules
setwd("C:/Capsis4")

# ==============================================================================
# PARALLEL PROCESSING CONFIGURATION
# ==============================================================================

# Determine optimal number of CPU cores for parallel processing
# Reserve 2 cores for system operations to maintain system responsiveness
n_cores <- max(1, detectCores() - 2)
cat("Using", n_cores, "cores for parallel processing\n")

# ==============================================================================
# OUTPUT CLEANUP
# ==============================================================================

# Clean up previous simulation outputs to avoid conflicts
# This ensures each run starts with a clean slate
cat("Cleaning up previous simulation outputs...\n")
for (i in 1:10) {
  # Construct path to potential old output directory
  dir_to_delete <- file.path(output_base_path, sprintf("output-cmd_%d.txt", i))
  
  # Check if directory exists and remove it
  if (dir.exists(dir_to_delete)) {
    cat("  Deleting existing output:", dir_to_delete, "\n")
    unlink(dir_to_delete, recursive = TRUE, force = TRUE)
  }
}
cat("Cleanup completed.\n")

# ==============================================================================
# SIMULATION COMMAND PREPARATION
# ==============================================================================

# Build list of Capsis commands for parallel execution
# Each command runs a FORCEPS simulation with a specific parameter file
cat("Preparing", n_commandFiles, "simulation commands...\n")
cmds <- sprintf(
  'capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement %s',
  file.path(output_base_path, sprintf("cmd_%d.txt", 1:n_commandFiles))
)

# ==============================================================================
# PARALLEL EXECUTION
# ==============================================================================

cat("Starting parallel execution of FORCEPS simulations...\n")
start_time <- Sys.time()

# Initialize parallel computing cluster
# This creates worker processes for parallel execution
cl <- makeCluster(n_cores)

# Execute all simulation commands in parallel
# Each worker process runs one or more simulations
parLapply(cl, cmds, function(cmd) {
  cat("Executing simulation command:", cmd, "\n")
  
  # Run the Capsis command and wait for completion
  # system() executes the command in the system shell
  result <- system(cmd, wait = TRUE)
  
  # Log completion status
  if (result == 0) {
    cat("Successfully completed:", cmd, "\n")
  } else {
    cat("Error in command:", cmd, "- Exit code:", result, "\n")
  }
  
  return(result)
})

# Clean up parallel cluster resources
stopCluster(cl)

# ==============================================================================
# EXECUTION SUMMARY
# ==============================================================================

end_time <- Sys.time()
execution_time <- difftime(end_time, start_time, units = "mins")

cat("\n==============================================================================\n")
cat("FORCEPS SIMULATION EXECUTION COMPLETED\n")
cat("==============================================================================\n")
cat("Total simulations:", n_commandFiles, "\n")
cat("Cores used:", n_cores, "\n")
cat("Execution time:", round(execution_time, 2), "minutes\n")
cat("Output directory:", output_base_path, "\n")
cat("==============================================================================\n")

