
# Global To Do
- [ ] Change the path variable so that someone else can easily adapt it (for ForCEEPS output mainly but also relative path for libraries).
- [ ] Add a description of each created output and where they are created
- [ ] Write the main in order to run each study independently (with parameters, maybe rethink the organisation to facilitate this)
- [ ] A lot of code look alike : more function or how to make that clear

- [ ] Add the forceeps initialisation files in data (for reproducibility)
- [ ] Parfois Retz_act.climate est en maj parfois en min !!!

# Description
Summary of work done during the first year of the PhD, including literature review, preliminary analyses, and initial modeling work.

# Literature Review

- [ ] Add content to this file.

# Preliminary Work

## Retz Analyses

- [ ] Attention : error in some of the graph need to check where it is coming from (maybe species columns)

- In `Import_data.R`:
  - Created two data tables:
    - `data/retz/forest_data` — contains all Retz data.
    - `data/forest_data` — filtered according to choices determined in `visualisation.Rmd` with ForceEPS species constraints.
- In `visualisation.Rmd`:
  - Visualized the Retz data. Made choice of species and parameters for the ForCEEPS model.

## From Inventory to Model Initialization

- Work in progress: need to explain and clean the code.

- [x] Merge Create_unif and Generate so that there is one file and it uses inventory_utils 
- [ ] Import output is made in the `Results.Rmd` file maybe change it to the import output for knitting speed and clarity

## Repetition

- `Generate.R`: generate the necessary repetition data for ForceEPS.
- `Import_output.R`: create outputs:
  - `output/repetition_mean.RData`
  - `output/repetition_productivityScene.RData`

## Param_Type

- Results are okay.
- Would like to improve:
  - `Create_unif_inv.R`
  - `Generate.R`
  - `Import_output.R`  
  by using `utils` and rewriting the code more cleanly.

- [x] code reviewed to use inventory and output utils
- [x] Path to check but it is far better already (pour l'instant je l'ai mis en haut du fichier pour que ce soit facile à changer)
- [ ] Comment

# Study Beginning

Potential introduction for a Master internship:  
- `Generate.R`: generate ForceEPS initialization files.  
- `Run_forceeps.R`: run ForceEPS in parallel with the generated init files.  
- `Post_processing.R`: import and modify data to relink each simulation to each itinerary and create different scenarios (combinations of itineraries).  

A lot of writing is still needed to explain the questions and what was done.

# Reusable code (maybe for forceeps analyses)

- `utils.R` — mainly for inventory and output functions.
- SEE : `R/utils/inventory_utils.R` & `R/utils/output_utils.R`

# structure du code et de forceps :

- data inventaire, ...
- naming of files in order to work...