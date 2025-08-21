
# Global To Do
- [ ] Change the path variable so that someone else can easily adapt it (for ForCEEPS output mainly but also relative path for libraries).
- [ ] Add a description of each created output and where they are created
- [ ] Write THE or MANY main in order to run each study independently (with parameters, maybe rethink the organisation to facilitate this)
- [ ] A lot of code look alike : more function or how to make that clear

- [ ] Add the forceeps initialisation files in data (for reproducibility)
- [ ] Parfois Retz_act.climate est en maj parfois en min !!!
- [ ] Créer l'arborescence pour forceeps dans les fichiers generate

# Description
Summary of work done during the first year of the PhD, including literature review, preliminary analyses, and initial modeling work.

# Global link between forceeps and code, organisation

## ForCEEPS Files Structure

ForCEEPS requires specific initialization files located at: `C:/Capsis4/data/forceps/user/`

```
study name/
├── data/
│   ├── forceps.setup              # ForCEEPS configuration
│   ├── RETZ_0_0.site              # Site file (in a folder or not)
│   ├── retz_act.climate          # Climate data
│   └── inventaires               # Inventories
│       ├── RETZ_00102_02.inv      # Name needs to be RETS_#_#.inv
├── cmd_1.txt                      # Command file for batch runs needs to be named cmd_#.txt
```

To run it :
capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement data\forceps\[user]\[study name]\cmd_1.txt at C:/Capsis4/ in the terminal

## Study Code Organization

Each ForCEEPS study follows a standardized 4-step workflow:

### 1. **Generate.R**
- Creates ForCEEPS initialization files (inventories, scenarios, command files)
- Sets up experimental design (parameter combinations, repetitions)
- Uses functions from `R/utils/inventory_utils.R`
- Outputs: ForCEEPS input files in `C:/Capsis4/data/forceps/[user]/[study]/` at least :
  - site
  - inventories
  - command files

### 2. **Run** (manual step)
- Execute ForCEEPS simulations via command line:
  ```bash
  capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement data\forceps\[user]\[study]\cmd_1.txt
  ```
- Run from `C:/Capsis4/` directory

### 3. **Import_output.R**
- Imports ForCEEPS simulation results
- Links results to experimental conditions (scenarios, repetitions)
- Uses functions from `R/utils/output_utils.R`
- Outputs: Processed `.RData` files in `data/forceeps_output/`

### 4. **Results.Rmd**
- Statistical analysis and visualization
- Generates PDF reports
- Knits to `Reports/[study_name].pdf`
- Uses data from step 3

This standardized workflow ensures reproducibility and makes it easy to adapt studies for new research questions.

## How to Use

### Requirements
- **R** (≥ 4.0.0)
- **ForCEEPS** installed in `C:/Capsis4/`
- **LaTeX** for PDF reports
- **R packages:** `tidyverse`, `fs`, `gridExtra`, `patchwork`, `knitr`

### Setup
```r
# Install dependencies
install.packages(c("tidyverse", "fs", "gridExtra", "patchwork", "knitr"))

# Update base_path in Generate.R files to match your ForCEEPS installation
base_path <- "C:/Capsis4/data/forceps/[your_user]/[study_name]/"
```

### Run a study
1. Execute `Generate.R` 
2. Run ForCEEPS simulations manually
3. Execute `Import_output.R`
4. Knit `Results.Rmd`

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

**Objective:** Study the effect of the `param_type` parameter on cutting selectivity in ForCEEPS (from selective cutting of small trees at 0.0, to random cutting at 0.5, to selective cutting of large trees at 1.0).

**Method:** 
- Fagus sylvatica monospecific stand (20 trees, 0-80 cm diameter)
- 6 param_type values (0.0 to 1.0, step=0.2) × 10 repetitions = 60 simulations
- 80-year simulations, 12-year rotation, target basal area 15 m²/ha

**Code organization:**
- `Generate.R`: Creates uniform inventory and ForCEEPS command files for all param_type scenarios
- `Import_output.R`: Imports simulation results and links to param_type/repetition structure  
- `Results.Rmd`: Theoretical analysis + empirical results (basal area, density, diameter distributions)

**Outputs created:**
- `data/forceeps_output/paramType_complete.RData`: Complete dataset with individual tree data for all simulations (created in `Import_output.R`)
- `Reports/Forceeps_paramType.pdf`: Full analysis report

**Status:**
- [x] Code reviewed and utilizes inventory/output utilities
- [x] Path management improved for reproducibility  
- [x] Comprehensive documentation and analysis completed
- [ ] Integration with main workflow (pending main.R development)
- [ ] Review the report

# Study Beginning

Potential introduction for a Master internship:  
- `Generate.R`: generate ForceEPS initialization files.  
- `Run_forceeps.R`: run ForceEPS in parallel with the generated init files.  
- `Post_processing.R`: import and modify data to relink each simulation to each itinerary and create different scenarios (combinations of itineraries).  

A lot of writing is still needed to explain the questions and what was done.

- [x] Overall changes in code to be clearer Generate.R
- [ ] Attention some patch still don't work (maybe no proportion of species or inexistant species, I dont know)
- [ ] Comment parameters in generate

- [ ] Overall changes in code to be clearer Import.R

- [ ] Lot of changes in Results.R

# Reusable code (maybe for forceeps analyses)

- `utils.R` — mainly for inventory and output functions.
- SEE : `R/utils/inventory_utils.R` & `R/utils/output_utils.R`

# structure du code et de forceps :

- data inventaire, ...
- naming of files in order to work...