# paths and other parameters

path = "C:/Capsis4/data/forceps/clementine/Test_protocole"

# generate forceeps initial files

# run forceeps

# import output

# generate analyses reports
rmarkdown::render(
  "R/Retz_vizualisation/Visualisation.Rmd",
  output_file = "Retz_data_visualization.pdf",
  output_dir = "Reports" )

rmarkdown::render(
  "R/study_protocol/Results.Rmd",
  output_file = "Scale_study_analyses.html",
  output_dir = "Reports")
