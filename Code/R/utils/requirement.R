# ==============================================================================
# PROJECT DEPENDENCIES MANAGEMENT - Forest Thesis Project
# ==============================================================================
#
# Purpose:
# This script manages all package dependencies required for the Forest Thesis 
# project. It provides a centralized location for:
# - Defining all required R packages
# - Automatically installing missing packages
# - Loading all necessary libraries with proper organization
#
# Usage:
# Source this file at the beginning of any script in the project:
# source("R/utils/requirement.R")
#
# Author: Forest Thesis Project
# Last modified: August 2025
# ==============================================================================

# ==============================================================================
# PACKAGE DEFINITIONS
# ==============================================================================

# Complete list of packages required for the Forest Thesis project
packages <- c(
  # Geospatial analysis and mapping
  "sf",                  # Simple Features for spatial vector data
  
  # Data manipulation and tidying
  "tidyverse",           # Collection of data science packages (dplyr, ggplot2, etc.)
  "tidyr",               # Data tidying and reshaping
  "dplyr",               # Data manipulation and transformation
  "purrr",               # Functional programming tools
  "readr",               # Fast and friendly file reading
  "stringr",             # String manipulation
  "rlang",               # Low-level R language tools
  
  # File and system operations
  "fs",                  # Cross-platform file system operations
  "readxl",              # Read Excel files
  
  # Statistical analysis and modeling
  "truncnorm",           # Truncated normal distribution functions
  "parallel",            # Parallel computing support
  
  # Data visualization and plotting
  "ggplot2",             # Grammar of graphics (included in tidyverse)
  "patchwork",           # Combine and arrange multiple plots
  "gridExtra",           # Arrange multiple grid-based plots
  "grid",                # Grid graphics system
  "plotly",              # Interactive web-based plots
  "RColorBrewer",        # Color palettes for plots
  
  # Report generation and documentation
  "knitr",               # Dynamic report generation
  "kableExtra"           # Enhanced table formatting for knitr
)

# ==============================================================================
# PACKAGE INSTALLATION
# ==============================================================================

# Check for missing packages and install them automatically
cat("Checking for missing packages...\n")
installed <- packages %in% rownames(installed.packages())

if(any(!installed)) {
  missing_packages <- packages[!installed]
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
  cat("Package installation completed.\n")
} else {
  cat("All required packages are already installed.\n")
}

# ==============================================================================
# LIBRARY LOADING
# ==============================================================================

cat("Loading required libraries...\n")

# Core data manipulation and analysis
suppressPackageStartupMessages({
  library(tidyverse)     # Data science workflow
  library(sf)            # Spatial data handling
  library(fs)            # File system operations
  library(readxl)        # Excel file reading
  library(truncnorm)     # Statistical distributions
  library(parallel)      # Parallel processing
})

# Visualization and plotting
suppressPackageStartupMessages({
  library(ggplot2)       # Already loaded with tidyverse, but explicit for clarity
  library(patchwork)     # Plot composition
  library(gridExtra)     # Grid arrangements
  library(grid)          # Grid graphics
  library(plotly)        # Interactive plots
  library(RColorBrewer)  # Color schemes
})

# Documentation and reporting
suppressPackageStartupMessages({
  library(knitr)         # Dynamic documents
  library(kableExtra)    # Enhanced tables
})

cat("All libraries loaded successfully.\n")
cat("==============================================================================\n")
