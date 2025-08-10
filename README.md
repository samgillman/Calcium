# Calcium Imaging Analysis App - Version 2.4

A comprehensive R Shiny application for analyzing calcium imaging data with improved performance, modularity, and user experience.

## Key Features

- **Modular Architecture**: Organized into focused modules for data loading, preprocessing, metrics calculation, and visualization
- **Performance Optimized**: Vectorized calculations, memory management, and progress indicators
- **Enhanced Error Handling**: Comprehensive input validation and graceful failure recovery
- **Interactive Visualizations**: Plotly-based charts with customization options
- **Batch Processing**: Handle multiple files efficiently
- **Memory Management**: Built-in cache clearing and memory monitoring

## Project Structure

```
calcium-app/
├── app.R                    # Main application file
├── R/
│   ├── global.R            # Global configuration
│   ├── modules/            # Shiny modules
│   │   ├── mod_data_loading.R
│   │   ├── mod_preprocessing.R
│   │   ├── mod_metrics.R
│   │   └── mod_visualization.R
│   └── utils/              # Utility functions
│       ├── calculations.R
│       └── validation.R
├── .gitignore              # Git ignore file
└── README.md               # This file
```

## Installation

### Required R Packages

```r
# Core packages
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets",
  "shinycssloaders", "DT", "shinyvalidate"
))

# Data manipulation
install.packages(c(
  "dplyr", "tidyr", "data.table", "readxl", "purrr"
))

# Visualization
install.packages(c(
  "ggplot2", "plotly", "corrplot", "cowplot", "patchwork",
  "RColorBrewer", "scales", "colourpicker", "viridis", "pheatmap"
))

# Analysis
install.packages(c(
  "zoo", "latex2exp", "openxlsx", "pryr"
))
```

## Quick Start

1. **Clone or download** the repository
2. **Install dependencies** (see above)
3. **Run the app**:
   ```r
   shiny::runApp("app.R")
   ```

## Data Format

Your data should be structured as:
- **Column 1**: Time values (numeric)
- **Columns 2+**: Signal values for each cell/ROI
- **File formats**: CSV, Excel (.xlsx/.xls), or TXT

Example:
```
Time    Cell1   Cell2   Cell3
0.0     1.00    1.02    0.98
0.2     1.01    1.03    0.99
0.4     1.05    1.08    1.02
...
```

## Key Metrics Calculated

- **Peak ΔF/F₀**: Maximum fluorescence change
- **Time to Peak**: Time from stimulus to peak
- **Rise Time**: 10-90% rise time
- **Area Under Curve (AUC)**: Integrated response
- **Half Width (HWHM)**: Response duration
- **Signal-to-Noise Ratio (SNR)**: Signal quality
- **Ca²⁺ Entry Rate**: Maximum rate of change
- **Spike Detection**: Automated spike counting

## Usage

### Memory Management
- Monitor memory usage in the sidebar
- Click "Clear Cache" if memory usage exceeds 500MB
- Enable chunk processing for datasets with >1000 cells

### Performance Settings
- Enable parallel processing for large datasets
- Adjust chunk size based on available memory
- Use vectorized calculations (automatic for >50 cells)

## Troubleshooting

### High Memory Usage
- Clear cache regularly
- Process files in smaller batches
- Reduce the number of cells analyzed simultaneously

### Slow Performance
- Enable parallel processing in Settings
- Increase chunk size if memory allows
- Close other applications

### Import Errors
- Ensure data is properly formatted
- Check for non-numeric values in signal columns
- Verify time column is monotonically increasing

## Performance Benchmarks

Tested on MacBook Pro (16GB RAM):
- **100 cells, 1000 timepoints**: ~2 seconds
- **500 cells, 1000 timepoints**: ~8 seconds (vectorized)
- **1000 cells, 1000 timepoints**: ~15 seconds (parallel)

## Version History

### v2.4 (Current)
- Complete modularization
- Performance optimizations
- Memory management
- Bug fixes and improvements

## License

This software is provided as-is for research purposes.

## Contributing

Suggestions and improvements welcome! Please report issues or submit pull requests.