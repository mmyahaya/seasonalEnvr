
------------------------------------------------------------------------

# seasonalEnvr: Modelling and Analysing Plant-Pollinator Networks Under Seasonal Environments

## Overview

`seasonalEnvr` is an R-based repository designed for simulating, modelling, and analyzing the dynamics of **plant-pollinator mutualistic networks** within seasonally variable environments. The project explores how seasonal shifts influence community parameters, interaction rewiring, and species richness (including specific simulations for 50-species communities). It incorporates statistical frameworks like piecewise Structural Equation Modelling (SEM) to evaluate complex ecological pathways.

------------------------------------------------------------------------

## Repository Structure

The repository contains both processed data objects and core analysis scripts:

### Data Files (`.rds`)

- **`tover.rds`**: Turnover data documenting temporal changes and interaction rewirings across simulated seasons.

### Core Simulation & Modelling

- **`SeasonalNet.R`**: Scripts managing the foundational setup, simulation parameters, and execution of the seasonal network models.
- **`modified Tomas single (new2).R`**: Adapted baseline model (likely building on structural frameworks by Tomas et al.) optimizing single-network dynamics under varying conditions.
- **`Postmodel_50.R`**: Specific script scaled to simulate and track dynamics for a standardized species richness benchmark of 50 species.

### Statistical Analysis & Evaluation

- **`PostmodelAnalysis_SE.R`**: Post-simulation analysis functions to extract metrics, network topology, and performance characteristics under a seasonal environment.
- **`Seasonal Community Par.R`** & **`piecewiseSEM.R`**: Implements piecewise Structural Equation Modelling (SEM) to partition and evaluate direct/indirect effects of seasonality on community parameters and web structures.

### Visualization Scripts

- **`Stackplot_SE.R`**: Generates stacked plots illustrating community distribution profiles under seasonal environments.
- **`Stackplot_Intrinc.R`**: Visualizes intrinsic network parameters or metrics over time.
- **`Stackplot_BC.R`**: Generates improved visualization profiles, potentially focusing on centrality metrics or baseline community contrasts.

------------------------------------------------------------------------

## Getting Started

### Prerequisites

To run these scripts, you will need a working installation of **R** along with libraries typically used for network ecology and structural equation modeling.

``` r
# Recommended packages to install before running
install.packages(c("tidyverse",
"igraph", 
"bipartite",
"piecewiseSEM"))
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
