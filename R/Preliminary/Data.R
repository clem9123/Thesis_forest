# Prepare data for analyses
# 15.05.2025
# Clémentine de Montgolfier
#-------------------------------------------------------------------------------

## Library ---------------------------------------------------------------------
#------------------------------------------------------------------------------#

library(sf)
library(tidyverse)


## Load all data ---------------------------------------------------------------
#------------------------------------------------------------------------------#

# Forest data
forest_data <- 
    read.csv("C:/Users/cdemontgolf/Desktop/RETZ_FIF/BDD_UEP_2012.csv")

# forest_data %>% pull(Classe.de.catégorie.de.diamètre.dominant) %>% table()

# Load spatial data
spatial_coord <- 
    st_read("C:/Users/cdemontgolf/Desktop/RETZ_FIF/uep.shp")%>%
    select(IIDT_UEP, geometry)

# Diamètre, détails sur les classes (cm -> m)
table_ccd <- data.frame(
  Classe.de.catégorie.de.diamètre.dominant = 
    c("S", "E", "I", "1", "P", "M", "G", "G-", "G+", "T"),
  diamètre.min = c(2, 5, 7.5, 7.5, 17.5, 27.5, 47.5, 47.5, 55, 67.5),
  diamètre.max = c(5, 10, 90, 17.5, 27.5, 47.5, 67.5, 55, 67.5, 100),
  diamètre.mean = c(3.5, 7.5, 45, 12.5, 22.5, 37.5, 57.5, 52.5, 62.5, 85))

# Pedologie data
pedo_data <- 
    st_read("C:/Users/cdemontgolf/Desktop/RETZ_FIF/ZOOM50_Pedo/retz_pedo.shp") %>%
    select(RUM, geometry) %>%
    st_transform(st_crs(spatial_coord)) %>%
    # uniformise coordonate with spatial_coord
    st_intersection(spatial_coord) %>%
    group_by(IIDT_UEP) %>%
    summarize(RUM = mean(RUM, na.rm = TRUE))

## Clean forest data -----------------------------------------------------------
#------------------------------------------------------------------------------#

# transformer les virgules en points
forest_data <- forest_data %>%
  mutate(across(where(is.character), ~ gsub(",", ".", .)))

# Clean column names
colnames(forest_data) <- 
    gsub("....du.couvert.boisé.", "", colnames(forest_data))
especes <- colnames(forest_data)[9:36] # species columns

# as numeric
forest_data <- forest_data %>%
    mutate(surface.retenue = as.numeric(surface.retenue),
        surf.terrière.moy = as.numeric(surf.terrière.moy),
        Age.réel.en.2010 = as.numeric(Âge.réel.en.2010),
        across(all_of(especes), as.numeric)) %>%
        select(- Âge.réel.en.2010)
         
# Classe d'âge détails sur les classes
forest_data <- forest_data %>% 
    mutate(Classe.d.âge.en.2010 = 
            ifelse(Classe.d.âge.en.2010 == "NC", NA, Classe.d.âge.en.2010)) %>%
    separate(Classe.d.âge.en.2010,
        into = c("age_min", "age_max"), sep = "-", convert = TRUE) %>%
    mutate(median_age = 
        (as.numeric(age_min)+as.numeric(age_max))/2, na.rm = TRUE)

# supprimer ce qui n'est pas un peuplement
forest_data <- forest_data %>%
    filter(
        !is.na(Identifiant.peuplement.élémentaire) & 
        Identifiant.peuplement.élémentaire != "" &
        ! Structure.et.occupation.du.sol %in% c("R", "V", "M")) 
    # R non boisé boisable, V non boisable, M terrain de service

# supprimer les patchs qui sont >20% des espèces qui n'ont pas de corresponding species
corresponding.species <- 
  read.csv("data/corresponding_species.csv", header = TRUE, sep = ",")
species_cols <- colnames(forest_data)[9:36] # species columns
non_forceps_species <- corresponding.species %>%
    filter(Retz_Code %in% species_cols, is.na(Forceps_Code)) %>%
    pull(Retz_Code) %>%
    append (c("A.F", "A.F...préciser.essence", "A.R", "A.R...préciser.essence"))
# Number of patches and surf with more than 20% of non-forceps species
#forest_data %>%
#  mutate(sum_species = rowSums(select(., all_of(non_forceps_species)), na.rm = TRUE)) %>%
#  filter(sum_species > 20) %>%
#  summarise(surf.tot = sum(surface.retenue, na.rm = TRUE)) #   nrow()
forest_data <- forest_data %>%
    mutate(sum_species = rowSums(select(., all_of(non_forceps_species)), na.rm = TRUE)) %>%
    filter(sum_species <= 20) %>%
    select(-sum_species)
# remove colums non corresponding
forest_data <- forest_data %>%
    select(-all_of(non_forceps_species))

## Merge with all data ---------------------------------------------------------
#------------------------------------------------------------------------------#

# Merge with spatial and pedo data
forest_data <- forest_data %>%
  left_join(pedo_data,
    by = c("Identifiant.peuplement.élémentaire" = "IIDT_UEP"))

# merge diameter information
forest_data <- forest_data %>%
  left_join(table_ccd,
    by = "Classe.de.catégorie.de.diamètre.dominant") %>%
  mutate(Classe.de.catégorie.de.diamètre.dominant = 
    factor(Classe.de.catégorie.de.diamètre.dominant,
        levels = c("I", "S", "E", "1", "P", "M", "G", "G-", "G+", "T"),
        ordered = TRUE))

# save as RData and csv
rm(list = ls()[!ls() %in% c("forest_data")])
write.csv(forest_data, file = "data/forest_data.csv", row.names = FALSE)
save(forest_data, file = "data/forest_data.RData")


################################################################################
#------------------------------------------------------------------------------#


## Correct climate data --------------------------------------------------------
#------------------------------------------------------------------------------#

climate_act <- read.csv("data/data_clim_act_compiegne.bern", sep = "\t")
colnames(climate_act) <- gsub("X..", "", colnames(climate_act))
# Add missing columns with 0 values if they do not exist
for (col in c("tmean", "tmin", "tmax", "sumPrec", "nbRainyDays")) {
  if (!col %in% colnames(climate_act)) {
    climate_act[[col]] <- 0
  }
}
# arrange and save
climate_act <- climate_act %>%
  select(year, month, tmean, tmin, tmax, sumPrec, nbRainyDays)
write.table(climate_act, file = "data/retz_act.climate", row.names = FALSE, sep = "\t")