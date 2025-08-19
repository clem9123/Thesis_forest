read_forceps_complete_output <- function(path) {
    # Lire les lignes du fichier
    lines <- readLines(path)

    # Trouver la ligne avec les noms de colonnes (celle qui commence par "#Trees")
    header_line_index <- grep("^#Trees", lines)
    if (length(header_line_index) == 0) {
        stop("Ligne d'en-tête '#Trees' non trouvée.")
    }

    # La ligne suivante contient les noms de colonnes
    column_line <- lines[header_line_index + 1]
    # Supprimer le caractère '#' s'il est encore présent
    column_line <- gsub("^#", "", column_line)
    # Convertir en noms de colonnes
    column_names <- unlist(strsplit(column_line, "\t"))

    # Nettoyage des noms de colonnes
    column_names <- gsub("\\)", "", column_names)
    column_names <- gsub("\\(", ".", column_names)
    column_names <- gsub(" ", "_", column_names)

    # Lire les données à partir de la ligne suivante
    data_start <- header_line_index + 2
    data <- read.table(text = lines[data_start:length(lines)],
                       header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    
    # Assigner les noms de colonnes nettoyés
    colnames(data) <- column_names

    return(data)
}

path = "C:/Capsis4/data/forceps/clementine/Test_itinerary/output-cmd.txt/"

data = data.frame()
t = 0

for(type in seq(0,1,0.2)){    
    for (i in 1:10) {
        file = paste0(path, "retz_act.climate_unif.inv_simulation_", t+i, "complete.txt")
        one_rep = read_forceps_complete_output(file)
        one_rep$type = type
        one_rep$rep = i
        data = rbind(data, one_rep)
    }
    t = t + 10
}

data <- data %>%
  group_by(rep, type, date, id) %>%
  mutate(cut = row_number() == 2) %>%
  ungroup()

# si cut == TRUE alors date == date +0.1
data <- data %>%
  mutate(date_init = date) %>%
  mutate(date = ifelse(cut, date + 0.1, date)) %>%
  mutate(cut = as.logical(cut))

# Ajouter la classe de diamètre à chaque arbre avec case_when
data <- data %>%
  mutate(
    classe_diam = case_when(
      dbh.cm < 7.5  ~ "R",
      dbh.cm < 17.5 ~ "I",
      dbh.cm < 27.5 ~ "P",
      dbh.cm < 47.5 ~ "M",
      dbh.cm < 67.5 ~ "G",
      dbh.cm >= 67.5 ~ "T",
      TRUE ~ NA_character_
    )
  )

# mettre les classes de diamètre dans l'ordre
data$classe_diam <- factor(data$classe_diam, 
                            levels = c("R", "I", "P", "M", "G", "T"),
                            ordered = TRUE)

itinerary_data <- data
save(itinerary_data, file = "output/paramType_complete.RData")