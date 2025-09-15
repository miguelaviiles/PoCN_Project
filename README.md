# Physics of Complex Networks

This repository contains the code, data, and report for two projects completed as part of the course Physics of Complex Networks done by the student Miguel Avilés Moreno. The projects focus on computational modeling and network analysis in the context of sociophysics, using R programming in Jupyter Notebooks and standalone R scripts. Below are the details of the tasks completed, as required by the professor’s instructions.

## Repository Structure

- `report.pdf`: The compiled LaTeX report documenting the methodology, results, and analysis for both tasks.
- `latex/`: Contains the LaTeX source files (e.g., `.tex`) and figures (e.g., `.png` or `.pdf`) used to generate `report.pdf`. 
- `code/`: Contains subfolders for each task, with standalone R scripts, Jupyter Notebooks, and output files.
  - `code/task_30/`: Code and outputs for Task #30 (Axelrod model).
  - `code/task_44/`: Code and outputs for Task #44 (Social Connectedness Index II).
- `data/`: Contains post-processed output data for both tasks (e.g., node and edge files for Task #44). Original raw data is not included, as per instructions.

## Task Details

### Task #30: Axelrod Model for Dissemination of Culture (Score: 0.5)

**Objective**: Simulate the Axelrod model for cultural dissemination.

**Description**:

- Implemented the classical Axelrod model in R to simulate cultural dynamics, where nodes represent agents with cultural feature vectors, and edges represent interactions.
- Applied to a real case of United States National Elections.
- Studied the phase transition from a global consensus to a polarized state.
- Code is provided as both a Jupyter Notebook (`code/task_30/`Axelrod_Model_Project`.ipynb`) and a standalone R script (`code/task_30/`Axelrod_Model_Project.ipynb`.R`).

**Files**:

- `code/task_30/Axelrod_Model_Project.ipynb.R`: Standalone R script implementing the Axelrod model and generating outputs.
- `code/task_30/Axelrod_Model_Project.ypynb`: Original Jupyter Notebook with the development code and visualizations.
- `latex/images/`: Contains figures used in the report, with clear fonts and captions.

### Task #44: Social Connectedness Index II from Facebook (Score: 1.0)

**Objective**: Construct a global social network from Facebook’s Social Connectedness Index data for GADM NUTS3 areas in the top 100 countries (excluding the USA) and perform a simplified network analysis.

**Description**:

- Processed raw data from the Facebook Social Connectedness Index repository to build a network where nodes are GADM NUTS3 areas and edges represent friendship connections.
- Remapped node IDs to start from 1, ensuring compatibility with the dataset’s indexing.
- Generated two output files:
  - `my_nodes.csv`: Contains `nodeID`, `nodeLabel`, `latitude`, `longitude` for each GADM NUTS3 area.
  - `my_edges.csv`: Contains `nodeID_from`, `nodeID_to`, `country_name`, `country_ISO3` for friendship connections.
- Performed a simplified network analysis, including degree distribution and basic topological metrics (e.g., average path length, clustering coefficient).
- Outputs include network data files (`data/my_nodes.csv`, `data/my_edges.csv`) and visualizations of network properties (e.g., degree distribution histograms), included as figures in `latex/`.
- Code is provided as both a Jupyter Notebook (`code/task_44/`SCI_Facebook_Project`.ipynb`) and a standalone R script (`code/task_44/`SCI_Facebook_Project`.R`).

**Files**:

- `code/task_44/SCI_Facebook_Project.R`: Standalone R script for data processing, network construction, and analysis.
- `code/task_44/SCI_Facebook_Project.ipynb`: Original Jupyter Notebook with the development code and visualizations.
- `data/my_nodes.csv`: Node file with remapped IDs, labels, and geographic coordinates.
- `data/my_edges.csv`: Edge file with friendship connections and country metadata.
- `latex/images`: Contains figures used in the report.

## Dependencies

- **R**: Version 4.0 or higher.
- **R Packages**:
  - Task #30: `igraph`, `ggplot2` , `reshape2`, `RColorBrewer`, `dplyr` and `ggpubr`.
  - Task #44: `igraph`, `ggplot2`, `dplyr`, `stringr`, `tidyr`, `RColorBrewer`, `rgl`, `sf` and `readr`.

## How to Run

1. Clone this repository
2. Navigate to the task subfolder
3. Ensure R is installed and the required packages are available (see scripts for automatic installation).
4. Run the standalone scripts using:

   ```bash
   Rscript code/task_30/Axelrod_Model_Project.R
   Rscript code/task_44/SCI_Facebook_Project.R
   ```
5. Alternatively, open the `.ipynb` files in Jupyter Notebook with the R kernel (IRkernel) installed to view the development code.

## 
