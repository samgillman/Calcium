# Calcium Imaging Analysis App - Version 2.4

A comprehensive R Shiny application for analyzing calcium imaging data with improved performance, modularity, and user experience.

## 🚀 Key Improvements

### 1. **Modular Architecture**
- Separated 1600+ line monolithic file into organized modules
- Clear separation of concerns with dedicated modules for:
  - Data loading (`mod_data_loading.R`)
  - Preprocessing (`mod_preprocessing.R`)
  - Metrics calculation (`mod_metrics.R`)
  - Visualization (`mod_visualization.R`)
- Shared utilities and validation functions

### 2. **Performance Optimizations**
- **Vectorized calculations** for faster processing of large datasets
- **Memory management** with cache clearing and garbage collection
- **Progress indicators** for all long-running operations
- **Chunk processing** option for very large datasets
- **Parallel processing** support (optional)

### 3. **Enhanced Error Handling**
- Comprehensive input validation with actionable error messages
- Edge case handling in metrics calculations
- Graceful failure recovery in batch processing

### 4. **Bug Fixes**
- Fixed FWHM calculation for signals that don't return to baseline
- Improved baseline detection with adaptive window sizing
- Better handling of non-responder cells
- Corrected spike detection algorithm

### 5. **New Features**
- **Batch processing** for multiple files
- **Interactive visualizations** with plotly
- **Spike detection** and analysis
- **Session management** with auto-save
- **Customizable settings** with persistence
- **Memory usage monitoring**
- **Help system** with documentation

## 📁 Project Structure

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
└── README.md
```

## 🔧 Installation

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

## 🚦 Quick Start

1. **Clone or download** the repository
2. **Install dependencies** (see above)
3. **Run the app**:
   ```r
   shiny::runApp("app.R")
   ```
4. **Load demo data** to explore features

## 📊 Data Format

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

## 🎯 Key Metrics Calculated

- **Peak ΔF/F₀**: Maximum fluorescence change
- **Time to Peak**: Time from stimulus to peak
- **Rise Time**: 10-90% rise time
- **Area Under Curve (AUC)**: Integrated response
- **Half Width (HWHM)**: Response duration
- **Signal-to-Noise Ratio (SNR)**: Signal quality
- **Ca²⁺ Entry Rate**: Maximum rate of change
- **Spike Detection**: Automated spike counting

## 💡 Usage Tips

### Memory Management
- Monitor memory usage in the sidebar
- Click "Clear Cache" if memory usage exceeds 500MB
- Enable chunk processing for datasets with >1000 cells

### Batch Processing
1. Navigate to "Batch Analysis" tab
2. Select multiple files
3. Choose to use current preprocessing settings
4. Optionally enable auto-export
5. Click "Run Batch Analysis"

### Performance Settings
- Enable parallel processing for large datasets
- Adjust chunk size based on available memory
- Use vectorized calculations (automatic for >50 cells)

## 🐛 Troubleshooting

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

## 📈 Performance Benchmarks

Tested on MacBook Pro (16GB RAM):
- **100 cells, 1000 timepoints**: ~2 seconds
- **500 cells, 1000 timepoints**: ~8 seconds (vectorized)
- **1000 cells, 1000 timepoints**: ~15 seconds (parallel)

## 🔄 Version History

### v2.4 (Current)
- Complete modularization
- Performance optimizations
- Memory management
- Bug fixes and improvements

### v2.3
- Original monolithic version
- Basic functionality

## 📝 License

This software is provided as-is for research purposes.

## 🤝 Contributing

Suggestions and improvements welcome! Please report issues or submit pull requests.

## 📧 Contact

For questions or support, please contact the development team.