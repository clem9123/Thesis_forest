library(tidyverse)

# import csv forceps2.New_species.csv
# ignorer la première ligne
Forceps_species <- read.csv("forceps2.New_species", header = TRUE, sep = "\t", skip = 1)
Forceps_species <- Forceps_species %>% select(X.kID, kName)
colnames(Forceps_species) <- c("Forceps_Code", "Latin_Name")

# Create a data frame with 
Retz_species <- data.frame(
  Retz_Code = c("ALB", "ALI", "ALT", "AUL", "BOU", "CHA", "CHC", "CHE", "CHP", "CHS", "CHR", "CHY", "CHT",
           "ERA", "ERS", "ERP", "ERC", "FRE", "FRU", "HET", "MAR", "MER", "NOY", "ORM", "PEU", "PLA",
           "PPP", "ROB", "SAU", "SOR", "TIL", "TRE", "TUL", "CEA", "CYP", "DOU", "EPC", "EPI", "EPS",
           "GEN", "IFS", "MEL", "PIN", "P.L", "P.M", "P.N", "P.S", "P.W", "S.N", "S.P", "S.V", "TAX", 
           "THU", "TSU"),
  French_Name = c("Alisier blanc", "Alisier", "Alisier torminal", "Aulnes divers", "Bouleaux divers",
                  "Charme", "Chêne chevelu", "Chênes indigènes", "Chêne pédonculé", "Chêne sessile",
                  "Chêne rouge", "Chêne pubescent", "Châtaignier", "Grands Erables", "Erable sycomore",
                  "Erable plane", "Erable champêtre", "Frênes", "Fruitiers divers", "Hêtre", 
                  "Marronnier (d'Inde)", "Merisier", "Noyers", "Ormes divers", "Peupliers divers", 
                  "Platane", "Poirier, Pommier, Prunier", "Robinier", "Saules", "Sorbier des oiseleurs", 
                  "Tilleuls", "Tremble", "Tulipier de Virginie", "Cèdres de l’Atlas ou du Liban", 
                  "Cyprès", "Douglas", "Epicéa commun", "Epicéa autres", "Epicéa de Sitka", 
                  "Genévrier", "If commun", "Mélèzes divers", "Pin", "Pin laricio", "Pin maritime", 
                  "Pins noirs divers", "Pin sylvestre", "Pin Weymouth", "Sapin de Nordmann", 
                  "Sapin pectiné", "Sapin de Vancouver (grandis)", "Taxodiacées", "Thuya géant", 
                  "Tsuga hétérophylle"),
  Latin_Name = c("Sorbus aria", "Sorbus sp.", "Sorbus torminalis", "Alnus sp.", "Betula pendula",
                 "Carpinus betulus", "Quercus cerris", "Quercus sp.", "Quercus robur", "Quercus petraea",
                 "Quercus rubra", "Quercus pubescens", "Castanea sativa", "Acer sp.", "Acer pseudoplatanus",
                 "Acer platanoides", "Acer campestre", "Fraxinus excelsior", "Various fruit trees",
                 "Fagus sylvatica", "Aesculus hippocastanum", "Prunus avium", "Juglans sp.", 
                 "Ulmus sp.", "Populus sp.", "Platanus × acerifolia", "Pyrus, Malus, Prunus spp.", 
                 "Robinia pseudoacacia", "Salix sp.", "Sorbus aucuparia", "Tilia sp.", "Populus tremula", 
                 "Liriodendron tulipifera", "Cedrus atlantica or Cedrus libani", "Cupressus sp.", 
                 "Pseudotsuga menziesii", "Picea abies", "Other Picea species", "Picea sitchensis", 
                 "Juniperus sp.", "Taxus baccata", "Larix sp.", "Pinus sp.", "Pinus nigra",
                 "Pinus pinaster", "Pinus nigra", "Pinus sylvestris", "Pinus strobus", 
                 "Abies nordmanniana", "Abies alba", "Abies grandis", "Taxodiaceae family", 
                 "Thuja plicata", "Tsuga heterophylla")
)

#save at csv
write.csv(full_join(Forceps_species, Retz_species), "data/corresponding_species.csv")