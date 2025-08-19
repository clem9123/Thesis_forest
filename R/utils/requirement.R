# Liste des packages utilisés dans le projet Retz
packages <- c(
  # Packages déjà listés
  "sf",
  "tidyr",
  "readxl",
  "patchwork",
  "knitr",
  "kableExtra",
  "RColorBrewer",
  "tidyverse",
  "rlang",
  "truncnorm",
  "fs",
  "gridExtra",
  "grid",
  "parallel",
  "plotly"
)

# Installer les packages manquants
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) {
  install.packages(packages[!installed])
}

# Charger tous les packages
#lapply(packages, library, character.only = TRUE)
