# ===============================================================================
# FORCEEPS FOREST SIMULATION WORKFLOW - MAIN EXECUTION SCRIPT
# ===============================================================================
#
# Purpose: Main workflow script for running complete FORCEEPS forest simulation analyses
# Author: Clementine de Montgolfier
# Last Modified: 26 août 2025
#
# Description:
# This is the main execution script that orchestrates a complete FORCEEPS forest simulation
# workflow. It coordinates inventory generation, simulation execution, output processing,
# and report generation for different types of analyses (replicates, paramType, study protocol).
#
# Workflow Steps:
# 1. Set up analysis parameters and working directories, change to your FORCEEPS base path
# 2. Load utility functions for inventory, itinerary, and output processing
# 3. Generate forest inventories and command files
# 4. Execute FORCEEPS simulations (external process
# 5. Import and process simulation outputs
# 6. Generate analysis reports (PDF format)
#
# Key Features:
# - Configurable for different analysis types (change analyse_name and n_commandFiles --> AUTOMATISE)
# - Maintains working directory consistency
# - Handles batch simulation execution
# - Automated report generation
# - Modular design with utility functions
#
# Usage:
# Simply change the 'analyse_name' variable to switch between analyses:
# - "forceeps_replicates": Variability analysis with multiple seeds n_commandFiles = 30
# - "forceeps_paramType": Parameter sensitivity analysis n_commandFiles = 1
# - "study_protocol": Multi-scenario management comparison n_commandFiles = 10
#
# Dependencies:
# - FORCEEPS model installed and accessible
# - Utility scripts in R/utils/
# - Analysis-specific scripts in R/{analyse_name}/
# - Base forest data in data/ directory
#
# ===============================================================================

# Library requirement
source("R/utils/requirement.R")

# Base path to FORCEEPS working directory and analysis configuration
forceeps_path = "C:/Capsis4/data/forceps/clementine/" # Change this to your FORCEEPS base path
analyse_name = "study_protocol"  # Change this to switch analysis type
base_path = paste0(forceeps_path, analyse_name, "/")

# Store current working directory to return to it later
working_dir <- getwd()

# Load utility functions for the analysis workflow
source("R/utils/inventory_utils.R")    # Forest inventory generation utilities
source("R/utils/itinerary_utils.R")    # Management scenario utilities
source("R/utils/output_utils.R")       # Simulation output processing utilities

# Step 1: Generate forest inventories and simulation command files
# This creates the necessary input files for FORCEEPS simulations
source(paste0("R/", analyse_name, "/Generate.R"))

# Step 2: Execute FORCEEPS simulations
# Run the external FORCEEPS model with the generated command files
n_commandFiles = 10  # Number of command files to process for batch execution
source("R/utils/runForceeps_utils.R")

# Return to the original R working directory after simulation
setwd(working_dir)

# Step 3: Import and process simulation outputs
# Read FORCEEPS output files and create analysis-ready datasets
source(paste0("R/", analyse_name, "/Import_output.R"))

# Step 4: Generate analysis reports
# Create PDF reports with results, figures, and interpretations
rmarkdown::render(
  paste0("R/", analyse_name, "/Results.Rmd"),
  output_file = paste0(analyse_name, ".pdf"),
  output_dir = "Reports"
)
