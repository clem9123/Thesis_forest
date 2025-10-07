# ===============================================================================
# FOREST INVENTORY SIMULATION AND FORCEEPS INTEGRATION UTILITIES
# ===============================================================================
#
# Purpose: Generate and manage forest inventories for FORCEEPS forest simulation model
# Author: Clementine de Montgolfier
# Last Modified: 26 août 2025
#        R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#
# Description:
# This file contains comprehensive utilities for generating forest inventories from
# real forest data and formatting them for use with the FORCEEPS forest simulation model.
# The utilities handle diameter distributions, species proportions, and file formatting
# to create realistic forest stands for simulation.
#
# Key Features:
# - Generate tree inventories from real forest data (Retz forest)
# - Support multiple diameter distribution types (uniform, normal, truncated normal)
# - Handle species proportion calculations with variation
# - Format inventories for FORCEEPS input requirements
# - Create command files for batch simulation runs
# - Manage folder structure and file organization
# - Generate uniform inventories for controlled experiments
#
# Main Functions:
# - simulate_inventory_for_patch(): Generate inventory from real forest data
# - simulate_inventory_uniforme(): Generate uniform diameter distributions
# - format_to_forceps(): Convert inventory to FORCEEPS format
# - generate_all_inventories(): Batch process multiple forest patches
# - initialise_forceeps_folder(): Set up directory structure
#
# File Structure Created:
# base_path/analyse_name/
#   ├── data/
#   │   ├── inventories/
#   │   │   ├── RETZ_XX_XX.inv
#   │   │   └── [more inventory files...]
#   │   ├── sites/
#   │   │   ├── RETZ_XX_XX.site
#   │   │   └── [more site files...]
#   │   ├── forceps.setup
#   │   ├── forceps.species
#   │   └── retz_act.climate
#   ├── cmd_1.txt
#   └── [more command files...]
#
# ===============================================================================

## Utility Functions -----------------------------------------------------------
#------------------------------------------------------------------------------#

#' Calculate Disk Area from Diameter
#'
#' @param diameter Diameter in centimeters.
#' @return Area in square centimeters.
calculate_disk_area <- function(diameter) {
	pi * (diameter / 2)^2
}

#' Generate Mean Diameter from Patch and Distribution Type
#'
#' @param patch Data frame row with diameter parameters.
#' @param distribution Character. Distribution type ("unif", "normal_IR", etc.).
#' @return Numeric value of mean diameter.
generate_mean_diameter <- function(patch, distribution) {
	switch(distribution,
		"unif" = (patch$diametre.min + patch$diametre.max) / 2,
		"normal_IR" = patch$diametre.mean,
		"normal_6" = patch$diametre.mean,
		"normal_4" = patch$diametre.mean,
		"mean" = patch$diametre.mean,
		"min" = patch$diametre.min,
		"max" = patch$diametre.max,
		stop("Invalid distribution")
	)
}

## Diameter Generation ---------------------------------------------------------
#------------------------------------------------------------------------------#

#' Generate Tree Diameters Based on Distribution Type
#'
#' @param patch Data frame row with diameter parameters.
#' @param distribution Character. Type of distribution.
#' @param n Integer. Number of values to generate.
#' @return Numeric vector of diameters.
generate_diameters <- function(patch, distribution, n) {
	switch(distribution,
		"unif" = runif(n, min = patch$diametre.min, max = patch$diametre.max),
		"normal_6" = rnorm(
            n, mean = patch$diametre.mean,
            sd = (patch$diametre.max - patch$diametre.min) / 6),
		"normal_4" = rnorm(
            n, mean = patch$diametre.mean,
            sd = (patch$diametre.max - patch$diametre.min) / 4),
		"normal_IR" = rtruncnorm(
            n, a = patch$diametre.min, b = patch$diametre.max,
            mean = patch$diametre.mean,
            sd = (patch$diametre.max - patch$diametre.min) / 10),
		"mean" = rep(patch$diametre.mean, n),
		"min" = rep(patch$diametre.min, n),
		"max" = rep(patch$diametre.max, n),
		stop("Invalid distribution type")
	)
}

## Species Proportions ---------------------------------------------------------
#------------------------------------------------------------------------------#

#' Generate Species Proportions with Variation
#'
#' @param retz_id Character. Patch identifier.
#' @param species_columns Character vector. Species column names.
#' @param variation Numeric. Percentage noise (default = 10).
#' @return Numeric vector summing to 100 with varied proportions.
generate_species_proportion <- function(retz_id, species_columns, variation = 10) {
	proportions <- Retz %>%
		filter(Identifiant.peuplement.elementaire == retz_id) %>%
		select(all_of(species_columns)) %>%
		unlist(use.names = FALSE) %>%
		as.numeric()

	proportions[is.na(proportions)] <- 0
	if (sum(proportions) == 0) stop("No species proportions found for this patch.")

	active_indices <- which(proportions > 0)
	inactive_indices <- which(proportions == 0)

	active_props <- proportions[active_indices] / sum(proportions[active_indices]) * 100
	noise <- runif(length(active_props), -variation, variation)
	modified <- pmax(active_props + noise, 0)
	modified <- modified / sum(modified) * 100

	final_props <- numeric(length(proportions))
	final_props[active_indices] <- modified
	final_props[inactive_indices] <- 0

	return(final_props)
}

#' Get Species Distribution for Trees
#'
#' @param n_trees Integer. Number of trees to assign species to.
#' @param retz_id Character. Patch ID.
#' @param species_columns Character vector. Species column names.
#' @param method Character. Either "random" or "static".
#' @return Character vector of species.
get_species_distribution <- function(n_trees, retz_id, species_columns, method = "random") {
	if (method == "random") {
		proportions <- generate_species_proportion(retz_id, species_columns)
	} else {
		proportions <- Retz %>%
			filter(Identifiant.peuplement.elementaire == retz_id) %>%
			select(all_of(species_columns)) %>%
			unlist() %>%
			as.numeric()
	}

	proportions[is.na(proportions)] <- 0
	species_names <- species_columns
	sampled <- rep(species_names, round(proportions * n_trees / 100))

	if (length(sampled) < n_trees) {
		diff <- n_trees - length(sampled)
		most_common <- species_names[which.max(proportions)]
		sampled <- c(sampled, rep(most_common, diff))
	} else {
		sampled <- sampled[1:n_trees]
	}

	return(sampled)
}

## Inventory Generation from a patch -------------------------------------------
#------------------------------------------------------------------------------#

#' Simulate Inventory for a Patch
#'
#' @param retz_id Character. ID of the patch.
#' @param distribution Character. Diameter distribution type.
#' @param species_proportion Character. Either "random" or "static".
#' @return A list with inventory data frame and RUM info.
simulate_inventory_for_patch <- function(
    retz_id, distribution = "normal_6", species_proportion = "random") {

	patch <- Retz %>% filter(Identifiant.peuplement.elementaire == retz_id)
	species_cols <- colnames(Retz)[9:23]

	if (patch$surf.terriere.moy == 0) {
		return(list(
			inventory = data.frame(
                species = character(), diametre = numeric(), age = numeric()),
			RUM = patch$RUM
		))
	}

	if (patch$Structure.et.occupation.du.sol == "I") {
		distribution <- "normal_IR"
	}

	mean_diam <- generate_mean_diameter(patch, distribution) / 100
	basal_area_per_tree <- calculate_disk_area(mean_diam)
	n_trees <- round((patch$surf.terriere.moy / basal_area_per_tree) / 10)

	diameters <- generate_diameters(patch, distribution, n_trees)
	species <- get_species_distribution(n_trees, retz_id, species_cols, species_proportion)

    age <- if (!is.na(patch$Age.reel.en.2010)) {
        patch$Age.reel.en.2010
    } else if (!is.na(patch$median_age)) {
        round(patch$median_age)
    } else {
        0
    }

	inventory <- data.frame(
		species = species,
		diametre = diameters,
		age = age
	)


	return(list(
		inventory = inventory,
		RUM = patch$RUM
	))
}

## Uniform Inventory Generation ------------------------------------------------
#------------------------------------------------------------------------------#

#' Generate Uniform Inventory 
#'
#' @param n_trees Integer. Number of trees to generate.
#' @param diam_min Numeric. Minimum diameter in cm.
#' @param diam_max Numeric. Maximum diameter in cm.
#' @param species Character. Species code.
#' @param age Numeric. Age of trees.
#' @return Data frame with inventory (species, diametre, age).
simulate_inventory_uniforme <- function(n_trees = 20, diam_min = 0, diam_max = 80, species = "FSyl", age = 10) {
	# Generer des diametres uniformement repartis
	diameters <- seq(diam_min, diam_max, length.out = n_trees)
	
	# Creer le data.frame d'inventaire
	inventory <- data.frame(
		species = species,
		diametre = diameters,
		age = age
	)
	
	return(inventory)
}

## Formatting for Forceps ------------------------------------------------------
#------------------------------------------------------------------------------#

#' Format Inventory to Forceps Format
#'
#' @param inventory Data frame of trees.
#' @param patch_area Numeric. Area of the patch.
#' @param patchId Character. Patch ID.
#' @return Data frame formatted for Forceps.
format_to_forceps <- function(inventory, patch_area, patchId = 1) {
	inventory %>%
		left_join(corresponding.species %>% select(Retz_Code, Forceps_Code),
		          by = c("species" = "Retz_Code")) %>%
		mutate(
			patchId = patchId,
			speciesId = Forceps_Code,
			dbh = round(diametre, 1),
			crownA1 = 0.1,
			dbhIncrement = 0.1,
			slowGrowthIndex = 0,
			treeId = row_number()
		) %>%
		select(patchId, treeId, speciesId, age, dbh, crownA1, dbhIncrement, slowGrowthIndex)
}

## File Writing ----------------------------------------------------------------
#------------------------------------------------------------------------------#

#' Write Forceps Inventory File
#'
#' @param inventory Data frame. Forceps-formatted inventory.
#' @param patch_area Numeric.
#' @param patch_number Integer.
#' @param output_file Character. Output file path.
write_forceps_inventory <- function(inventory, output_file, patch_area = 1000, patch_number = 1) {
	writeLines(c(
		"# Forceps inventory",
		"",
		paste0("inventoryPatchN = ", patch_number),
		paste0("inventoryPatchArea = ", patch_area),
		"",
		"#patch_id\ttreeId\tspeciesId\tage(years)\tdbh(cm)\tcrownA1[0,1]\tdbhIncrement(cm)\tslowGrowthIndex(0,1)"
	), con = output_file)

	write.table(
		inventory,
		file = output_file,
		append = TRUE,
		sep = "\t",
		row.names = FALSE,
		col.names = FALSE,
		quote = FALSE
	)
}

#' Write Forceps Command File
#'
#' @param output_file Character.
#' @param file_setup Character.
write_command_file <- function(output_file, file_setup) {
	writeLines(c(
		"# Forceps script command file, format SimulationCommandReader2 (Xavier Morin)",
		"",
		paste0("setupFileName = ", file_setup),
		"numberOfYearsToBeJumped = 1",
		"exportTimeStep = 1",
		"",
		"#seed\tsiteFileName\tclimateFileName\tinventoryFileName\tpotentialSpeciesList\tScenario"
	), con = output_file)
}

#' Update Parameters in a Forceps Command File
#'
#' @param filepath Character. Input file path.
#' @param updates Named list. Keys are parameters to replace.
#' @param output_path Character. Output file path (default is same as input).
update_forceps_parameters <- function(filepath, updates = list(), output_path = filepath) {
	lines <- readLines(filepath)

	modify_line <- function(lines, param, new_value) {
		sapply(lines, function(line) {
			if (grepl(paste0("^", param, "\\s*="), line)) {
				return(paste0(param, " = ", new_value))
			}
			line
		})
	}

	for (param in names(updates)) {
		lines <- modify_line(lines, param, updates[[param]])
	}

	writeLines(lines, con = output_path)
}

## Massif Inventory Generation -------------------------------------------------
#------------------------------------------------------------------------------#

#' Generate All Inventories for Retz
#' @param Retz Data frame. Retz data with patches.
#' @param distribution_types Character vector. Types of diameter distributions.
#' @param species_proportion Character. Either "random" or "static".
#' @param patcharea Numeric. Area of the patch.
#' @param path Character. Path to save inventory files.
#' @return NULL. Writes files to specified path.
generate_all_inventories <- function(Retz, distribution_types, species_proportion, patcharea, patchnumber, path) {
    for (i in 1:nrow(Retz)) {
        ligne <- Retz$Identifiant.peuplement.elementaire[i]
        
        # Simulate inventory
        results <- simulate_inventory_for_patch(ligne, distribution_types, species_proportion)
        
        # Format to Forceps inventory
        forceps_inv <- format_to_forceps(results$inventory, patcharea)
        
        # Write inventory and site files
        write_forceps_inventory(
            inventory = forceps_inv, 
            output_file = paste0(path, "/data/inventories/", ligne, ".inv"),
			patch_area = patcharea,
			patch_number = patchnumber
        )

        # write the site file (using updated parameters)
        update_forceps_parameters(
            "data/forceeps_init_files/forceps.site",
            updates = list(
                "siteName" = ligne,
                "siteBucketSize" = results$RUM
            ),
            output_path = paste0(path, "/data/sites/", ligne, ".site")
        )
    }
}

#' Generate All Command Files for Retz
#' @param Retz Data frame. Retz data with patches.
#' @param n_files Integer. Number of command files to generate for parallelisation
#' @param seed Integer. Random seed for simulations.
#' @param path Character. Path to save command files.
#' @param setup_file Character. Path to setup file.
#' @param climate_file Character. Path to climate file.
#' @param potential_species Character. List of potential species.
#' @param scenario_name Character. Name of the scenario.

# old function with separation by file
generate_command_files <- function(Retz, n_files, seed, path, setup_file, climate_file, potential_species, scenario) {
	retz_split <- split(Retz$Identifiant.peuplement.elementaire, 
						cut(seq_along(Retz$Identifiant.peuplement.elementaire),
						n_files, labels = FALSE))
	for (file_idx in seq_along(retz_split)) {
		cmd_file <- file.path(path, paste0("cmd_", file_idx, ".txt"))
		write_command_file(
			output_file = cmd_file,
			file_setup = setup_file
		)   
		for (ligne in retz_split[[file_idx]]) {
			inventory_file <- paste0("/inventaires/", ligne, ".inv")
			site_file <- paste0("/sites/", ligne, ".site")
			# scenario can be a vector/list; write one line for each scenario
			for (scen in scenario) {
				write(paste0(
					seed, "\t",
					site_file, "\t",
					climate_file, "\t",
					inventory_file, "\t",
					potential_species, "\t",
					scen),
					file = cmd_file, append = TRUE
				)
			}
		}
	}
}

# new function with no separation by inventory
generate_command_files <- function(Retz, seed, path, setup_file, climate_file, potential_species, scenario) {
	for (i in seq_along(Retz$Identifiant.peuplement.elementaire)) {
		ligne <- Retz$Identifiant.peuplement.elementaire[i]
		cmd_file <- file.path(path, paste0("cmd_", i, ".txt"))
		write_command_file(
			output_file = cmd_file,
			file_setup = setup_file
		)
		inventory_file <- paste0("/inventaires/", ligne, ".inv")
		site_file <- paste0("/sites/", ligne, ".site")
		for (scen in scenario) {
			write(paste0(
				seed, "\t",
				site_file, "\t",
				climate_file, "\t",
				inventory_file, "\t",
				potential_species, "\t",
				scen),
				file = cmd_file, append = TRUE
			)
		}
	}
}

## Folder Structure for ForCEEPS initialisation --------------------------------
#------------------------------------------------------------------------------#

#' Initialise Folder Structure for ForCEEPS Analysis
#'
#' @param base_path Character. Base directory path.
#' @param analyse_name Character. Name of the analysis.
#' @param overwrite Logical. If TRUE, overwrite existing analysis folder. Default FALSE.
#' @return Character. Path to the created analysis folder.
initialise_forceeps_folder <- function(base_path, analyse_name, overwrite = FALSE) {
  analysis_path <- file.path(base_path, analyse_name)
  data_path <- file.path(analysis_path, "data")
  inventories_path <- file.path(data_path, "inventories")
  sites_path <- file.path(data_path, "sites")

  if (dir.exists(analysis_path)) {
    message(paste("The analysis folder", analysis_path, "already exists."))
    if (!overwrite) {
      warning("Set overwrite = TRUE to delete and recreate the folder, or choose a different analyse_name.")
      return(invisible(NULL))
    } else {
      unlink(analysis_path, recursive = TRUE)
      warning(paste("Existing folder", analysis_path, "deleted."))
    }
  }

  # Create all directories inside data
  dir.create(data_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(inventories_path, showWarnings = FALSE)
  dir.create(sites_path, showWarnings = FALSE)

  # Copy required files into data folder
  files_to_copy <- c(
    "forceps.setup" = "data/forceeps_init_files/forceps.setup",
    "forceps.species" = "data/forceeps_init_files/forceps.species",
    "retz_act.climate" = "data/forceeps_init_files/retz_act.climate"
  )
  for (fname in names(files_to_copy)) {
	file.copy(
	  from = files_to_copy[[fname]],
	  to = file.path(analysis_path, "data", fname),
	  overwrite = TRUE
	)
  }

  return(data_path)
}
