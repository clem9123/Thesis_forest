# Description
Summary of work done during the first year of the PhD, including literature review, preliminary analyses, and initial modeling work.

I have done 5 different studies that  will be presented :
1. Retz analyses
2. From inventory to model initialization
3. Repetition
4. Param_Type
5. Study beginning

Each of them is in a different folder (in R) with its own Generate.R, Import_output.R and Results.Rmd files.

## Table of Contents

- [ForCEEPS and code organisation](#forceeps-and-code-organisation)
  - [ForCEEPS Files Structure](#forceeps-files-structure)
  - [Study Code Organization](#study-code-organization)
  - [How to Use](#how-to-use)
- [Literature Review](#literature-review)
- [Preliminary Work](#preliminary-work)
  - [Retz Analyses](#retz-analyses)
  - [From Inventory to Model Initialization](#from-inventory-to-model-initialization)
  - [Repetition](#repetition)
  - [Param_Type](#param_type)
  - [Study Beginning](#study-beginning)
- [Data](#data)
  - [forceeps_init_files](#forceeps_init_files)
  - [retz](#retz)


# ForCEEPS and code organisation

## ForCEEPS Files Structure

ForCEEPS requires specific initialization files located at: `C:/Capsis4/data/forceps/[user]/`  
This structure should be followed for each study (it is created in the Generate.R scripts for each study):

```
study name/
├── data/
│   ├── forceps.setup             # ForCEEPS configuration
│   ├── sites                     # Site data
│   │   ├── RETZ_0_0.site         # Name needs to be RETS_#_#.site
│   ├── retz_act.climate          # Climate data
│   └── inventaires               # Inventories
│       ├── RETZ_00102_02.inv     # Name needs to be RETS_#_#.inv
├── cmd_1.txt                     # Command file for batch runs needs to be named cmd_#.txt
```

To run it :
capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement data\forceps\[user]\[study name]\cmd_1.txt at C:/Capsis4/ in the terminal

Or use the main directly from R/main.R

ForCEEPS (with brieuc running script) gives 3 different output files :
- complete : individual tree data
- mean : summary data
- productivity_scene : summary data for productivity scene (by species)

## Study Code Organization

Each ForCEEPS study follows a standardized 4-step workflow that can be found in `R/main.R`:

### 1. **Generate.R**
- Creates ForCEEPS initialization files (inventories, scenarios, command files)
- Sets up experimental design (parameter combinations, repetitions)
- Uses functions from `R/utils/inventory_utils.R`
- Outputs: ForCEEPS input files in `C:/Capsis4/data/forceps/[user]/[study]/` at least :
  - site
  - inventories
  - command files

### 2. **Run** (automatic or manual step)

### 2. **Run ForCEEPS Simulations**

You can run ForCEEPS simulations either automatically (from main) or manually (via command line):

#### **Automatic (local machine)**
- Use the provided utility script to run simulations directly from R (`R/utils/runForceeps.R`):
  This will execute all command files for the study using your local ForCEEPS installation.

#### **Manual (server or other machine)**
- Execute ForCEEPS simulations via command line and/or bash script:
  ```bash
  capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement data\forceps\[user]\[study]\cmd_[n].txt
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

## How to Use

### Requirements
- **R** (≥ 4.0.0)
- **ForCEEPS** installed in `C:/Capsis4/`
- **LaTeX** for PDF reports
- **R packages:** see `R/utils/requirements.R`

### Setup

1. Clone this repository.
2. Install required R packages by running `R/utils/requirements.R`.
3. Set your ForCEEPS path in `R/main.R` (variable `forceeps_path`) and your working directory to this repository (Thesis_forest).
4. Ensure ForCEEPS is correctly installed;
5. Run main script for a specific study.

# Literature Review

- [ ] Add content

# Preliminary Work

## Retz Analyses

Only study that does not work with main.R  

**Objective:** It is a simple visualisation of the Retz data BDD_UEP_2021.csv, as well as some choices made for future analyses concerning this data.

- In `Import_data.R`:
  - Created two data tables:
    - `data/retz/forest_data` — contains all Retz data.
    - `data/forest_data` — filtered according to choices determined in `visualisation.Rmd` with ForceEPS species constraints.
- In `visualisation.Rmd`:
  - Visualized the Retz data. Made choice of species and parameters for the ForCEEPS model.

## From Inventory to Model Initialization

**Objective:** Test the process of converting an inventory into ForCEEPS initialization files with different choices on data interpretation.

This is an unfinished work but gives the main lines of what could be done. The goal would be to undertand the sensitivity of the model to initialisation.

- In `Generate.R` create a data table `data/forceeps_output/inventory.RData` with different initial inventories (from the same Retz data) according to different choices of interpretation of the data (species proportion, diameter distribution, etc).
- In `Import_output.R` create a data table `data/forceeps_output/inventory_productivityScene.RData` with the output of the simulations linked to the different inventories.

## Repetition

**Objective:** Determine the minimum number of simulations needed for reliable forest metric estimates in ForCEEPS.

**Method:** Ran 5,000 ForCEEPS simulations for the Retz forest, analyzed convergence of coefficient of variation (CV) and standardized error across different numbers of dynamics (m) and simulations (n).

**Output Created:**
- `Import_output.R`: create outputs:
  - `output/repetition_mean.RData` : output of the mean forceeps output
  - `output/repetition_productivityScene.RData` : output of the productivity scene forceeps output

## Param_Type

**Objective:** Study the effect of the `param_type` parameter on cutting selectivity in ForCEEPS (from selective cutting of small trees at 0.0, to random cutting at 0.5, to selective cutting of large trees at 1.0).

**Method:** 
- Fagus sylvatica monospecific stand (20 trees, 0-80 cm diameter)
- 6 param_type values (0.0 to 1.0, step=0.2) × 10 repetitions = 60 simulations
- 80-year simulations, 12-year rotation, target basal area 15 m²/ha

**Outputs created:**
- `data/forceeps_output/paramType_complete.RData`: Complete dataset with individual tree data for all simulations (created in `Import_output.R`)

# Study Beginning

**Objective:**  
Analyze the effects of different forest management strategies on multi-scale forest dynamics using FORCEEPS simulations. Gives my vision for future work.

**Method:**  
Simulated three silvicultural scenarios (clear-cut, continuous cover, natural evolution) across multiple plots. Generated scenarios with repeated random plot assignments to assess variability. Calculated productivity, harvested volume, standing biomass, and diversity indices (Hill numbers) at both plot and landscape levels. Compared local and global diversity to evaluate beta diversity. Visualized results with temporal and relational plots.

**Output Created:**  
- `data/forceeps_output/protocole_mean.RData`: output of the mean forceeps output
- `data/forceeps_output/protocole_productivityScene.RData`: output of the productivity scene forceeps output

# Data

- **forest_data**: Retz data filtered to include only species compatible with ForCEEPS.
- **corresponding.species**: Table mapping Retz species names to their ForCEEPS equivalents (made manually).

## forceeps_init_files

see forceeps documentation ;)

| Output File                                   | Created In (Study)                      | Description                                                                                   |
|-----------------------------------------------|------------------------------------------------|-----------------------------------------------------------------------------------------------|
| `inventory.RData`                             | `inventory_to_initialization`        | Initial inventories generated from Retz data with different interpretation choices             |
| `inventory_productivityScene.RData`           | `inventory_to_initialization`        | Simulation outputs linked to different inventories                                            |
| `repetition_mean.RData`                       | `Repetition`                                   | Mean results from 5,000 ForCEEPS simulations for repetition analysis                          |
| `repetition_productivityScene.RData`          | `Repetition`                                   | Productivity scene outputs from repetition simulations                                        |
| `paramType_complete.RData`                    | `Param_Type`                                   | Complete dataset with individual tree data for all param_type simulations                     |
| `protocole_mean.RData`                        | `Study_beginning`                              | Mean results from management strategy simulations                                             |
| `protocole_productivityScene.RData`           | `Study_beginning`                              | Productivity scene outputs from management strategy simulations                               |

## retz

Data from the ONF (both csv and QGIS files)