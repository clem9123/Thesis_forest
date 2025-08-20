rmarkdown::render(
  "R/Retz_vizualisation/Visualisation.Rmd",
  output_file = "Retz_data_visualization.pdf",
  output_dir = "Reports" )

rmarkdown::render(
  "R/study_protocol/Results.Rmd",
  output_file = "Scale_study_analyses.html",
  output_dir = "Reports")
