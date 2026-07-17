------------------------------------------------------------------------

# seasonalEnvr: Modelling and Analysing Plant-Pollinator Networks Under Seasonal Environments

## Overview

`seasonalEnvr` is an R-based repository designed for simulating, modelling, and analyzing the dynamics of **plant-pollinator mutualistic networks** within seasonally variable environments. The project explores how seasonal shifts influence community structural change and interaction turnover. It incorporates statistical frameworks like Generalized Linear Models (GLM) to evaluate complex ecological pathways.

------------------------------------------------------------------------

## Repository Structure

The repository contains both processed data objects and core analysis scripts:

### Data Files (`.rds`)

- **`tover.rds`**: An example of turnover data documenting temporal changes and interaction rewirings across simulated seasons.

### Core Simulation & Modelling

- **`SeasonalNet.R`**: Scripts managing the foundational setup, simulation parameters, and execution of the seasonal network models.

### Statistical Analysis & Evaluation

- **`PostmodelAnalysis_SE.R`**: Post-simulation analysis functions to extract metrics, network topology, and performance characteristics under a seasonal environment.
- **`Seasonal Community Par.R`**: Implements parallel computation to simulate several networks, structures and turnover.

### Visualization Scripts

- **`Stackplot_SE.R`**: Generates stacked plots illustrating community dynamics under seasonal environments.
- **`Stackplot_Intrinc.R`**: Visualizes intrinsic growth rate of species.
- **`Stackplot_BC.R`**: Visualizes the Brey-Curtis turnovers

------------------------------------------------------------------------

## Getting Started

### Prerequisites

To run these scripts, you will need a working installation of **R** along with libraries typically used for network ecology and structural equation modeling.

``` r
# Recommended packages to install before running
install.packages(c("tidyverse",
"igraph", 
"bipartite",
"reshape2",
"car",
"zetadiv"))
```

### Usage

1.  **Clone the repository:**

``` bash
git clone https://github.com/mmyahaya/seasonalEnvr.git
cd seasonalEnvr
```

2.  **Run Simulations:** Start by executing `SeasonalNet.R` to run the environmental seasonality models.
3.  **Analyze & Plot:** Utilize the `PostmodelAnalysis_SE.R` script to process your results, and use any of the `Stackplot_*.R` scripts to generate publication-ready figures.

------------------------------------------------------------------------

## License

This project is open-source and available under the [MIT License](https://opensource.org/licenses/MIT).
