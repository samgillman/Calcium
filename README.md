# Calcium Imaging Analysis App

This Shiny application provides a comprehensive suite of tools for analyzing calcium imaging data. It is designed for both individual and group-level analysis, offering features for data preprocessing, metric calculation, statistical analysis, and visualization.

## Features

- **Group Comparison:** Upload and compare multiple experimental groups.
- **Individual Analysis:** Perform in-depth analysis on single datasets.
- **Data Preprocessing:** Includes baseline correction, normalization, smoothing, and detrending.
- **Advanced Metrics:** Calculate over a dozen key calcium imaging metrics.
- **Statistical Analysis:** Built-in tools for ANOVA, t-tests, and correlation analysis.
- **Rich Visualizations:** Generate interactive and publication-ready plots.

## How to Run the Application

To run the application locally, you will need to have R and the necessary packages installed.

### 1. Clone the Repository

Clone this repository to your local machine:

```bash
git clone https://github.com/samgillman/Calcium.git
cd Calcium
```

### 2. Install Dependencies

The application requires several R packages to run. You can install them by running the following command in your R console:

```R
install.packages(c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "DT", "ggplot2", "dplyr", "tidyr", "data.table", "readxl", "purrr", "cowplot", "corrplot", "rlang", "RColorBrewer", "scales", "colourpicker", "patchwork", "latex2exp", "zoo", "shinyvalidate", "plotly", "pheatmap", "openxlsx", "pryr", "viridis"))
```

### 3. Launch the App

Once the dependencies are installed, you can launch the app by running the following script in your R console from the project's root directory:

```R
source("run_app.R")
```

Alternatively, you can run the following command in your terminal:

```bash
Rscript run_app.R
```

The application should now be running and accessible in your web browser.

## Project Structure

The project is organized as follows:

- `run_app.R`: The main script to launch the application.
- `app_complete.R`: Defines the user interface (UI).
- `app_complete_server.R`: Contains the server-side logic.
- `R/`: A directory containing the core R code.
  - `R/global.R`: Loads packages and defines global functions.
  - `R/modules/`: Contains the modular components of the app.
  - `R/utils/`: Contains utility and calculation functions.
- `.gitignore`: Specifies files to be ignored by Git.
- `README.md`: This file.