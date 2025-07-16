## Libraries -------------------------------------------------------------------
#------------------------------------------------------------------------------#

library(tidyr)
library(truncnorm)
library(dplyr)

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
		"unif" = (patch$diamètre.min + patch$diamètre.max) / 2,
		"normal_IR" = patch$diamètre.mean,
		"normal_6" = patch$diamètre.mean,
		"normal_4" = patch$diamètre.mean,
		"mean" = patch$diamètre.mean,
		"min" = patch$diamètre.min,
		"max" = patch$diamètre.max,
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
		"unif" = runif(n, min = patch$diamètre.min, max = patch$diamètre.max),
		"normal_6" = rnorm(
            n, mean = patch$diamètre.mean,
            sd = (patch$diamètre.max - patch$diamètre.min) / 6),
		"normal_4" = rnorm(
            n, mean = patch$diamètre.mean,
            sd = (patch$diamètre.max - patch$diamètre.min) / 4),
		"normal_IR" = rtruncnorm(
            n, a = patch$diamètre.min, b = patch$diamètre.max,
            mean = patch$diamètre.mean,
            sd = (patch$diamètre.max - patch$diamètre.min) / 10),
		"mean" = rep(patch$diamètre.mean, n),
		"min" = rep(patch$diamètre.min, n),
		"max" = rep(patch$diamètre.max, n),
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
		filter(Identifiant.peuplement.élémentaire == retz_id) %>%
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
			filter(Identifiant.peuplement.élémentaire == retz_id) %>%
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

## Inventory Generation --------------------------------------------------------
#------------------------------------------------------------------------------#

#' Simulate Inventory for a Patch
#'
#' @param retz_id Character. ID of the patch.
#' @param distribution Character. Diameter distribution type.
#' @param species_proportion Character. Either "random" or "static".
#' @return A list with inventory data frame and RUM info.
simulate_inventory_for_patch <- function(
    retz_id, distribution = "normal_6", species_proportion = "random") {

	patch <- Retz %>% filter(Identifiant.peuplement.élémentaire == retz_id)
	species_cols <- colnames(Retz)[9:23]

	if (patch$surf.terrière.moy == 0) {
		return(list(
			inventory = data.frame(
                species = character(), diamètre = numeric(), age = numeric()),
			RUM = patch$RUM
		))
	}

	if (patch$Structure.et.occupation.du.sol == "I") {
		distribution <- "normal_IR"
	}

	mean_diam <- generate_mean_diameter(patch, distribution) / 100
	basal_area_per_tree <- calculate_disk_area(mean_diam)
	n_trees <- round((patch$surf.terrière.moy / basal_area_per_tree) / 10)

	diameters <- generate_diameters(patch, distribution, n_trees)
	species <- get_species_distribution(n_trees, retz_id, species_cols, species_proportion)

    age <- if (!is.na(patch$Age.réel.en.2010)) {
        patch$Age.réel.en.2010
    } else if (!is.na(patch$median_age)) {
        round(patch$median_age)
    } else {
        0
    }

	inventory <- data.frame(
		species = species,
		diamètre = diameters,
		age = age
	)


	return(list(
		inventory = inventory,
		RUM = patch$RUM
	))
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
			dbh = round(diamètre, 1),
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
		"#seed\tretz_id\ttreeId\tspeciesId\tage(years)\tdbh(cm)\tcrownA1[0,1]\tdbhIncrement(cm)\tslowGrowthIndex(0,1)"
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
