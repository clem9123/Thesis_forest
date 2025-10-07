# Import and prepare forest data for Retz forest analyses
# This script imports, cleans and merges various forest datasets including:
# - Forest inventory data (BDD_UEP_2012.csv)
# - Spatial coordinates (uep.shp)
# - Pedological data (retz_pedo.shp)
#
# Author: Clementine de Montgolfier
# Date: 15.05.2025
# Last modified: 19.08.2025
#-------------------------------------------------------------------------------

## Load required libraries -----------------------------------------------------
#------------------------------------------------------------------------------#

library(sf)        # For spatial data manipulation
library(tidyverse) # For data manipulation and visualization


## Import raw data files ------------------------------------------------------
#------------------------------------------------------------------------------#

# Load forest inventory data from CSV file
# Contains information about forest stands including species composition, 
# diameter classes, age classes, and basal area
forest_data <- 
    read.csv("data/retz/BDD_UEP_2012.csv")

# Change all colnames é and è to e
colnames(forest_data) <- gsub("é", "e", colnames(forest_data))
colnames(forest_data) <- gsub("è", "e", colnames(forest_data))

# Example of diameter classes in the data:
# forest_data %>% pull(Classe.de.categorie.de.diametre.dominant) %>% table()

# Load spatial coordinates from shapefile
# Contains geometry information for each forest stand (UEP = Unite elementaire de Peuplement)
spatial_coord <- 
    st_read("data/retz/uep.shp") %>%
    select(IIDT_UEP, geometry)  # Keep only ID and geometry columns

# Create lookup table for diameter classes
# Convert diameter class codes to actual diameter ranges (cm -> m conversion)
# Classes: S=Semis, E=etale, I=Irregulier, 1-9=diameter classes, P=Petit, M=Moyen, G=Gros, T=Tres gros
table_ccd <- data.frame(
  Classe.de.categorie.de.diametre.dominant = 
    c("S", "E", "I", "1", "P", "M", "G", "G-", "G+", "T"),
  diametre.min = c(2, 5, 7.5, 7.5, 17.5, 27.5, 47.5, 47.5, 55, 67.5),     # Minimum diameter (cm)
  diametre.max = c(5, 10, 90, 17.5, 27.5, 47.5, 67.5, 55, 67.5, 100),      # Maximum diameter (cm)
  diametre.mean = c(3.5, 7.5, 45, 12.5, 22.5, 37.5, 57.5, 52.5, 62.5, 85)) # Mean diameter (cm)

# Load pedological (soil) data from shapefile
# RUM = Reserve Utile Maximale (Maximum Available Water Capacity)
pedo_data <- 
    st_read("data/retz/retz_pedo.shp") %>%
    select(RUM, geometry) %>%                           # Keep only soil water capacity and geometry
    st_transform(st_crs(spatial_coord)) %>%            # Transform CRS to match forest stands data
    st_intersection(spatial_coord) %>%                 # Intersect with forest stands boundaries
    group_by(IIDT_UEP) %>%                            # Group by forest stand ID
    summarize(RUM = mean(RUM, na.rm = TRUE))          # Calculate mean RUM per forest stand

## Data cleaning and preprocessing ---------------------------------------------
#------------------------------------------------------------------------------#

# Convert French decimal notation (comma) to standard notation (period)
# This is necessary because French CSV files often use comma as decimal separator
forest_data <- forest_data %>%
  mutate(across(where(is.character), ~ gsub(",", ".", .)))

# Clean and simplify column names by removing redundant text
# Remove "....du.couvert.boise." suffix from species column names
colnames(forest_data) <- 
    gsub("....du.couvert.boise.", "", colnames(forest_data))
especes <- colnames(forest_data)[9:36]  # Extract species columns (columns 9-36)

# Convert character columns to numeric where appropriate
forest_data <- forest_data %>%
    mutate(
        surface.retenue = as.numeric(surface.retenue),      # Stand area
        surf.terriere.moy = as.numeric(surf.terriere.moy),  # Mean basal area
        Age.reel.en.2010 = as.numeric(Âge.reel.en.2010),   # Real age in 2010
        across(all_of(especes), as.numeric)                # All species percentage columns
    ) %>%
    select(-Âge.reel.en.2010)  # Remove original age column (will be replaced by processed version)
         
# Process age class information
# Convert age class ranges (e.g., "20-40") to separate min/max columns and calculate median
forest_data <- forest_data %>% 
    mutate(
        # Handle missing values coded as "NC" (Non Classe = Not Classified)
        Classe.d.âge.en.2010 = ifelse(Classe.d.âge.en.2010 == "NC", NA, Classe.d.âge.en.2010)
    ) %>%
    # Split age class ranges into minimum and maximum values
    separate(Classe.d.âge.en.2010,
        into = c("age_min", "age_max"), sep = "-", convert = TRUE) %>%
    # Calculate median age for each stand
    mutate(median_age = (as.numeric(age_min) + as.numeric(age_max)) / 2)

## Merge datasets and finalize data preparation -------------------------------
#------------------------------------------------------------------------------#

# Merge forest data with pedological information
# Join by forest stand ID (UEP = Unite elementaire de Peuplement)
forest_data <- forest_data %>%
  left_join(pedo_data,
    by = c("Identifiant.peuplement.elementaire" = "IIDT_UEP"))

# Add diameter class information to forest data
forest_data <- forest_data %>%
  left_join(table_ccd,
    by = "Classe.de.categorie.de.diametre.dominant") %>%
  # Convert diameter class to ordered factor for proper sorting
  mutate(Classe.de.categorie.de.diametre.dominant = 
    factor(Classe.de.categorie.de.diametre.dominant,
        levels = c("I", "S", "E", "1", "P", "M", "G", "G-", "G+", "T"),
        ordered = TRUE))

save(forest_data, file = "data/retz/forest_data.RData")

## Forceeps filtering ----------------------------------------------------------
#------------------------------------------------------------------------------#

# Filter out non-forest areas and invalid records
# Keep only valid forest stands by removing:
# - Empty or missing stand IDs
# - Non-forest land use types: R (non-forested forestable), V (non-forestable), M (service areas)
forest_data <- forest_data %>%
    filter(
        !is.na(Identifiant.peuplement.elementaire) & 
        Identifiant.peuplement.elementaire != "" &
        !Structure.et.occupation.du.sol %in% c("R", "V", "M")
    )

# Remove stands with high proportion of species not compatible with FORCEPS model
# Load correspondence table between Retz species codes and FORCEPS model codes
corresponding.species <- 
  read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")

species_cols <- colnames(forest_data)[9:36]  # Species columns in forest data

# Identify species that don't have corresponding FORCEPS codes
non_forceps_species <- corresponding.species %>%
    filter(Retz_Code %in% species_cols, is.na(Forceps_Code)) %>%
    pull(Retz_Code) %>%
    # Add additional non-FORCEPS species categories
    append(c("A.F", "A.F...preciser.essence", "A.R", "A.R...preciser.essence"))

# Optional: Check number of patches with >20% non-FORCEPS species (for information)
# forest_data %>%
#   mutate(sum_species = rowSums(select(., all_of(non_forceps_species)), na.rm = TRUE)) %>%
#   filter(sum_species > 20) %>%
#   summarise(surf.tot = sum(surface.retenue, na.rm = TRUE), n_patches = n())

# Filter out stands with >20% non-FORCEPS species
forest_data <- forest_data %>%
    mutate(sum_species = rowSums(select(., all_of(non_forceps_species)), na.rm = TRUE)) %>%
    filter(sum_species <= 20) %>%
    select(-sum_species)

# Remove columns for non-FORCEPS species (they won't be used in modeling)
forest_data <- forest_data %>%
    select(-all_of(non_forceps_species))

# Add median age when not available, use diameter mean as proxy
forest_data <- forest_data %>%
  mutate(
    median_age = ifelse(is.na(median_age), diametre.mean * 2, median_age)
  )

# Clean workspace and save processed data
rm(list = ls()[!ls() %in% c("forest_data")])  # Keep only forest_data object

# To save processed forest data if needed
#write.csv(forest_data, file = "data/forest_data.csv", row.names = FALSE)
save(forest_data, file = "data/forest_data.RData")
