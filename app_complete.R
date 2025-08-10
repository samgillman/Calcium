#!/usr/bin/env Rscript

# Universal Calcium Imaging Analysis App - COMPLETE Modularized Version 2.5
# All features from original app preserved and enhanced

# Load configuration and modules
source("R/global.R")
source("R/utils/validation.R")
source("R/utils/calculations.R")
source("R/modules/mod_data_loading.R")
source("R/modules/mod_preprocessing.R")
source("R/modules/mod_metrics.R")
source("R/modules/mod_visualization.R")

# Additional modules for complete functionality
source("R/modules/mod_group_comparison.R")
source("R/modules/mod_metric_guide.R")
source("R/modules/mod_tables.R")
source("R/modules/mod_export.R")
source("R/modules/mod_statistical_analysis.R")

# Enable all features
shinyjs::useShinyjs()
enableBookmarking(store = "url")

# Complete UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Calcium Imaging Analysis v2.5",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('show_help', Math.random())",
        icon("question-circle"),
        "Help"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('show_quickstart', Math.random())",
        icon("rocket"),
        "Quick Start"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      
      # GROUP COMPARISON SECTION
      menuItem("Group Comparison", icon = icon("layer-group"), startExpanded = TRUE,
        menuSubItem("Load Groups", tabName = "gc_load", icon = icon("upload")),
        menuSubItem("Time Course", tabName = "gc_time", icon = icon("chart-line")),
        menuSubItem("Metrics", tabName = "gc_metrics", icon = icon("chart-bar")),
        menuSubItem("Heatmap", tabName = "gc_heatmap", icon = icon("th")),
        menuSubItem("Tables", tabName = "gc_tables", icon = icon("table")),
        menuSubItem("Export", tabName = "gc_export", icon = icon("download"))
      ),
      
      # INDIVIDUAL ANALYSIS SECTION
      menuItem("Individual Analysis", icon = icon("microscope"),
        menuSubItem("Load Data", tabName = "load", icon = icon("file-upload")),
        menuSubItem("Pre-processing", tabName = "preproc", icon = icon("filter")),
        menuSubItem("Time Course", tabName = "time", icon = icon("chart-line")),
        menuSubItem("Metrics", tabName = "metrics", icon = icon("calculator")),
        menuSubItem("Heatmap", tabName = "heatmap", icon = icon("th")),
        menuSubItem("Tables", tabName = "tables", icon = icon("table")),
        menuSubItem("Metric Guide", tabName = "guide", icon = icon("book")),
        menuSubItem("Export", tabName = "export", icon = icon("download"))
      ),
      
      # ADVANCED FEATURES
      menuItem("Advanced", icon = icon("cogs"),
        menuSubItem("Batch Analysis", tabName = "batch", icon = icon("tasks")),
        menuSubItem("Statistical Tests", tabName = "stats", icon = icon("chart-pie")),
        menuSubItem("Correlation Analysis", tabName = "correlation", icon = icon("project-diagram")),
        menuSubItem("Classification", tabName = "classification", icon = icon("tags"))
      ),
      
      # SETTINGS
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      
      br(),
      
      # Memory and session info
      div(
        style = "padding: 10px;",
        h5("System Status"),
        verbatimTextOutput("memory_usage", placeholder = FALSE),
        actionButton(
          "clear_memory",
          "Clear Cache",
          icon = icon("trash"),
          class = "btn-warning btn-sm",
          style = "width: 100%;"
        ),
        br(), br(),
        downloadButton(
          "save_session",
          "Save Session",
          class = "btn-info btn-sm",
          style = "width: 100%;"
        )
      )
    )
  ),
  
  dashboardBody(
    # Custom CSS for complete styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .btn-primary {
          background-color: #3c8dbc;
          border-color: #367fa9;
        }
        .btn-primary:hover {
          background-color: #367fa9;
        }
        .small-help {
          font-size: 11px;
          color: #666;
          margin-top: 5px;
        }
        details summary {
          cursor: pointer;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .status-indicator {
          display: inline-block;
          width: 10px;
          height: 10px;
          border-radius: 50%;
          margin-right: 5px;
        }
        .status-ready { background-color: #4CAF50; }
        .status-processing { background-color: #FFC107; }
        .status-error { background-color: #F44336; }
      "))
    ),
    
    # Include MathJax for formula rendering
    withMathJax(),
    
    # Tab content
    tabItems(
      # ========== GROUP COMPARISON TABS ==========
      
      # GC Load Tab
      tabItem(
        tabName = "gc_load",
        fluidRow(
          box(
            title = "Group Comparison - Load Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                h4("Load Processed Data Files"),
                p("Upload ΔF/F₀ files (CSV/Excel) with Time in first column"),
                
                fileInput(
                  "gc_files",
                  "Choose Files",
                  multiple = TRUE,
                  accept = c(".csv", ".txt", ".xlsx", ".xls")
                ),
                
                textInput(
                  "gc_group_name",
                  "Group Name",
                  placeholder = "Enter group name"
                ),
                
                actionButton(
                  "gc_append_btn",
                  "Add Group",
                  icon = icon("plus"),
                  class = "btn-success"
                ),
                
                actionButton(
                  "gc_demo_btn",
                  "Load Demo Data",
                  icon = icon("database"),
                  class = "btn-info"
                ),
                
                hr(),
                
                h4("Current Groups"),
                DT::dataTableOutput("gc_groups_table")
              ),
              
              column(6,
                h4("Group Management"),
                
                uiOutput("gc_group_editors"),
                
                br(),
                
                actionButton(
                  "gc_clear_all",
                  "Clear All Groups",
                  icon = icon("trash"),
                  class = "btn-danger"
                ),
                
                hr(),
                
                h4("Data Preview"),
                verbatimTextOutput("gc_data_summary"),
                
                div(
                  id = "gc_status",
                  style = "margin-top: 20px;",
                  span(class = "status-indicator status-ready"),
                  textOutput("gc_status_text")
                )
              )
            )
          )
        )
      ),
      
      # GC Time Course Tab
      tabItem(
        tabName = "gc_time",
        fluidRow(
          box(
            title = "Controls",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            
            selectInput(
              "gc_time_plot_type",
              "Plot Type",
              choices = c(
                "Static (ggplot2)" = "static",
                "Interactive (plotly)" = "plotly"
              ),
              selected = "static"
            ),
            
            checkboxInput("gc_time_show_ribbon", "Show SEM ribbon", TRUE),
            checkboxInput("gc_time_show_points", "Show mean points", TRUE),
            checkboxInput("gc_time_show_individual", "Show individual traces", FALSE),
            
            sliderInput(
              "gc_time_alpha",
              "Individual trace transparency",
              min = 0.1,
              max = 1,
              value = 0.3,
              step = 0.1
            ),
            
            sliderInput(
              "gc_time_font_size",
              "Font size",
              min = 8,
              max = 20,
              value = 12,
              step = 1
            ),
            
            hr(),
            
            h4("Appearance"),
            
            selectInput(
              "gc_time_theme",
              "Theme",
              choices = c("Classic", "Minimal", "Dark", "Publication"),
              selected = "Classic"
            ),
            
            checkboxInput("gc_time_show_grid", "Show gridlines", TRUE),
            checkboxInput("gc_time_show_legend", "Show legend", TRUE),
            
            selectInput(
              "gc_time_legend_pos",
              "Legend position",
              choices = c("right", "bottom", "top", "left"),
              selected = "right"
            ),
            
            div(
              class = "small-help",
              "Interactive plot allows zooming, panning, and hover tooltips."
            )
          ),
          
          box(
            title = "Time Course Plot (Group Comparison)",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            
            conditionalPanel(
              condition = "input.gc_time_plot_type == 'static'",
              withSpinner(plotOutput("gc_timecourse_plot", height = "620px"), type = 4)
            ),
            
            conditionalPanel(
              condition = "input.gc_time_plot_type == 'plotly'",
              withSpinner(plotlyOutput("gc_timecourse_plotly", height = "620px"), type = 4)
            ),
            
            hr(),
            
            fluidRow(
              column(3,
                selectInput(
                  "gc_time_export_format",
                  "Format",
                  choices = c("PNG", "PDF", "SVG", "TIFF"),
                  selected = "PNG"
                )
              ),
              column(3,
                numericInput("gc_time_export_width", "Width (in)", 10, min = 4)
              ),
              column(3,
                numericInput("gc_time_export_height", "Height (in)", 6, min = 3)
              ),
              column(3,
                numericInput("gc_time_export_dpi", "DPI", 300, min = 72)
              )
            ),
            
            downloadButton(
              "gc_time_download",
              "Download Plot",
              class = "btn-primary"
            )
          )
        )
      ),
      
      # GC Metrics Tab
      tabItem(
        tabName = "gc_metrics",
        fluidRow(
          box(
            title = "Controls",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            
            selectInput(
              "gc_metric_name",
              "Metric",
              choices = c(
                "Peak ΔF/F₀" = "Peak_dFF0",
                "Time to Peak (s)" = "Time_to_Peak",
                "Time to 25% Peak (s)" = "Time_to_25_Peak",
                "Time to 50% Peak (s)" = "Time_to_50_Peak",
                "Time to 75% Peak (s)" = "Time_to_75_Peak",
                "Rise Time (s)" = "Rise_Time",
                "Half Width (HWHM)" = "Half_Width",
                "Ca²⁺ Entry Rate" = "Calcium_Entry_Rate",
                "AUC" = "AUC",
                "Response Amplitude" = "Response_Amplitude",
                "SNR" = "SNR",
                "Baseline SD" = "Baseline_SD"
              ),
              selected = "Peak_dFF0"
            ),
            
            selectInput(
              "gc_metric_geom",
              "Plot type",
              choices = c(
                "Bar" = "bar",
                "Box" = "box",
                "Violin" = "violin",
                "Dot" = "dot"
              ),
              selected = "bar"
            ),
            
            checkboxInput("gc_metric_points", "Show individual points", TRUE),
            checkboxInput("gc_metric_stats", "Show statistical tests", TRUE),
            
            selectInput(
              "gc_stat_test",
              "Statistical Test",
              choices = c(
                "One-way ANOVA" = "anova",
                "Kruskal-Wallis" = "kruskal",
                "Wilcoxon (2 groups)" = "wilcox",
                "t-test (2 groups)" = "ttest"
              ),
              selected = "anova"
            ),
            
            checkboxInput("gc_show_posthoc", "Show post-hoc comparisons", TRUE),
            
            radioButtons(
              "gc_inset_pos",
              "Statistics position",
              choices = c("Above bars" = "above", "Below axis" = "below"),
              selected = "above",
              inline = TRUE
            ),
            
            checkboxInput("gc_show_brackets", "Show comparison brackets", TRUE),
            checkboxInput("gc_show_insets", "Show mean ± SEM, n", TRUE),
            
            sliderInput(
              "gc_metric_size",
              "Base font size",
              min = 8,
              max = 22,
              value = 14,
              step = 1
            ),
            
            hr(),
            
            h4("Multiple Comparisons Correction"),
            
            selectInput(
              "gc_correction_method",
              "Correction Method",
              choices = c(
                "None" = "none",
                "Bonferroni" = "bonferroni",
                "Holm" = "holm",
                "FDR" = "fdr"
              ),
              selected = "none"
            ),
            
            div(
              class = "small-help",
              "ANOVA/Kruskal-Wallis for multiple groups; Wilcoxon/t-test for 2 groups only."
            )
          ),
          
          box(
            title = "Metrics Plot (Group Comparison)",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            
            withSpinner(plotOutput("gc_metrics_plot", height = "640px"), type = 4),
            
            hr(),
            
            h4("Statistical Results"),
            verbatimTextOutput("gc_stats_summary"),
            
            DT::dataTableOutput("gc_posthoc_table")
          )
        )
      ),
      
      # GC Heatmap Tab
      tabItem(
        tabName = "gc_heatmap",
        fluidRow(
          box(
            title = "Heatmap Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            selectInput(
              "gc_heatmap_scale",
              "Scale Method",
              choices = c(
                "None" = "none",
                "Row (per cell)" = "row",
                "Column (per time)" = "column",
                "Global" = "global"
              ),
              selected = "row"
            ),
            
            selectInput(
              "gc_heatmap_palette",
              "Color Palette",
              choices = c(
                "Viridis" = "viridis",
                "Plasma" = "plasma",
                "Magma" = "magma",
                "Inferno" = "inferno",
                "Cividis" = "cividis",
                "Blue-Red" = "bluered",
                "Heat" = "heat"
              ),
              selected = "viridis"
            ),
            
            checkboxInput("gc_heatmap_cluster_rows", "Cluster cells", TRUE),
            checkboxInput("gc_heatmap_cluster_cols", "Cluster time points", FALSE),
            checkboxInput("gc_heatmap_show_dendro", "Show dendrogram", TRUE),
            
            sliderInput(
              "gc_heatmap_font",
              "Font size",
              min = 6,
              max = 14,
              value = 10,
              step = 1
            ),
            
            hr(),
            
            h4("Annotations"),
            
            checkboxInput("gc_heatmap_annotate_groups", "Show group labels", TRUE),
            checkboxInput("gc_heatmap_annotate_metrics", "Show metric sidebar", FALSE),
            
            conditionalPanel(
              condition = "input.gc_heatmap_annotate_metrics",
              selectInput(
                "gc_heatmap_sidebar_metric",
                "Sidebar metric",
                choices = c("Peak_dFF0", "Time_to_Peak", "AUC"),
                selected = "Peak_dFF0"
              )
            )
          ),
          
          box(
            title = "Heatmap (Group Comparison)",
            status = "warning",
            solidHeader = TRUE,
            width = 9,
            
            withSpinner(plotOutput("gc_heatmap_plot", height = "760px"), type = 4),
            
            hr(),
            
            downloadButton(
              "gc_heatmap_download",
              "Download Heatmap",
              class = "btn-primary"
            )
          )
        )
      ),
      
      # GC Tables Tab
      tabItem(
        tabName = "gc_tables",
        fluidRow(
          box(
            title = "Per-cell Metrics (Group Comparison)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Summary Statistics by Group"),
            DT::dataTableOutput("gc_summary_table"),
            
            br(),
            
            h4("Individual Cell Metrics"),
            DT::dataTableOutput("gc_metrics_table"),
            
            br(),
            
            h4("Time Course Summary (Mean ± SEM)"),
            DT::dataTableOutput("gc_timecourse_summary_table"),
            
            hr(),
            
            downloadButton(
              "gc_tables_download_all",
              "Download All Tables (Excel)",
              class = "btn-success"
            )
          )
        )
      ),
      
      # GC Export Tab
      tabItem(
        tabName = "gc_export",
        fluidRow(
          box(
            title = "Export Group Comparison Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                h4("Data Export"),
                
                downloadButton(
                  "gc_dl_metrics_csv",
                  "Download Metrics (CSV)",
                  class = "btn-primary"
                ),
                
                br(), br(),
                
                downloadButton(
                  "gc_dl_summary_csv",
                  "Download Summary (CSV)",
                  class = "btn-primary"
                ),
                
                br(), br(),
                
                downloadButton(
                  "gc_dl_stats_csv",
                  "Download Statistics (CSV)",
                  class = "btn-primary"
                ),
                
                br(), br(),
                
                downloadButton(
                  "gc_dl_all_excel",
                  "Download All Data (Excel)",
                  class = "btn-success"
                )
              ),
              
              column(6,
                h4("Figure Export"),
                
                radioButtons(
                  "gc_export_format",
                  "Format",
                  choices = c("PNG", "PDF", "SVG", "TIFF"),
                  selected = "PNG",
                  inline = TRUE
                ),
                
                numericInput("gc_export_width", "Width (inches)", 12, min = 4),
                numericInput("gc_export_height", "Height (inches)", 8, min = 3),
                numericInput("gc_export_dpi", "DPI", 300, min = 72),
                
                hr(),
                
                h4("Composite Figure"),
                
                checkboxGroupInput(
                  "gc_compose_panels",
                  "Include panels",
                  choices = c(
                    "Time Course" = "tc",
                    "Metrics" = "mp",
                    "Heatmap" = "hm",
                    "Statistics" = "st"
                  ),
                  selected = c("tc", "mp")
                ),
                
                selectInput(
                  "gc_compose_layout",
                  "Layout",
                  choices = c("Columns" = "col", "Rows" = "row", "Grid" = "grid"),
                  selected = "col"
                ),
                
                downloadButton(
                  "gc_dl_composite",
                  "Download Composite Figure",
                  class = "btn-warning"
                )
              )
            )
          )
        )
      ),
      
      # ========== INDIVIDUAL ANALYSIS TABS ==========
      
      # Individual Load Tab
      tabItem(
        tabName = "load",
        mod_data_loading_ui("ind_data_module")
      ),
      
      # Individual Preprocessing Tab  
      tabItem(
        tabName = "preproc",
        fluidRow(
          column(5,
            box(
              title = "Data Processing Controls",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              
              switchInput(
                "pp_enable",
                "Enable pre-processing",
                onLabel = "Yes",
                offLabel = "No",
                value = TRUE
              ),
              
              checkboxInput("pp_compute_dff", "Compute ΔF/F₀ per cell", TRUE),
              
              selectInput(
                "pp_baseline_method",
                "Baseline (F₀) method",
                choices = c(
                  "First N frames" = "first_n",
                  "Rolling minimum" = "rolling_min",
                  "Percentile" = "percentile"
                ),
                selected = "first_n"
              ),
              
              conditionalPanel(
                "input.pp_baseline_method == 'first_n'",
                numericInput(
                  "pp_baseline_frames",
                  "N frames for baseline (F₀)",
                  value = 20,
                  min = 1,
                  step = 1
                )
              ),
              
              conditionalPanel(
                "input.pp_baseline_method == 'rolling_min'",
                numericInput(
                  "pp_window_size",
                  "Rolling window (frames)",
                  value = 50,
                  min = 5,
                  step = 1
                )
              ),
              
              conditionalPanel(
                "input.pp_baseline_method == 'percentile'",
                numericInput(
                  "pp_percentile",
                  "Baseline percentile",
                  value = 10,
                  min = 1,
                  max = 50,
                  step = 1
                )
              ),
              
              checkboxInput(
                "pp_minmax_enable",
                "Per-cell min-max normalization (0-1)",
                FALSE
              ),
              
              tags$hr(),
              
              checkboxInput(
                "pp_apply_bg",
                "Background subtraction (single column)",
                FALSE
              ),
              
              textInput(
                "pp_bg_col",
                "Background column name (exact)",
                value = ""
              ),
              
              tags$hr(),
              
              h4("Smoothing"),
              
              checkboxInput("pp_smooth", "Apply smoothing", FALSE),
              
              conditionalPanel(
                condition = "input.pp_smooth",
                
                selectInput(
                  "pp_smooth_method",
                  "Method",
                  choices = c(
                    "Moving Average" = "ma",
                    "Gaussian" = "gaussian",
                    "Savitzky-Golay" = "sg"
                  ),
                  selected = "ma"
                ),
                
                numericInput(
                  "pp_smooth_window",
                  "Window size",
                  value = 5,
                  min = 3,
                  max = 21,
                  step = 2
                )
              ),
              
              tags$hr(),
              
              h4("Detrending"),
              
              checkboxInput("pp_detrend", "Remove trend", FALSE),
              
              conditionalPanel(
                condition = "input.pp_detrend",
                
                selectInput(
                  "pp_detrend_method",
                  "Method",
                  choices = c(
                    "Linear" = "linear",
                    "Polynomial" = "poly",
                    "Exponential" = "exp"
                  ),
                  selected = "linear"
                ),
                
                conditionalPanel(
                  condition = "input.pp_detrend_method == 'poly'",
                  numericInput(
                    "pp_poly_order",
                    "Polynomial order",
                    value = 2,
                    min = 2,
                    max = 5
                  )
                )
              ),
              
              tags$hr(),
              
              numericInput(
                "pp_sampling_rate",
                "Sampling rate (Hz) if Time missing/invalid",
                value = 1,
                min = 0.0001,
                step = 0.1
              ),
              
              actionButton(
                "pp_apply",
                "Apply Processing",
                icon = icon("play"),
                class = "btn-primary",
                width = "100%"
              ),
              
              div(
                class = "small-help",
                "ΔF/F₀ = (F - F₀)/F₀. Operations apply per uploaded file."
              )
            )
          ),
          
          column(7,
            box(
              title = "Notes",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              
              tags$ul(
                tags$li("Baseline uses per-cell F₀ (method above)."),
                tags$li("Min-max normalization rescales each cell independently to [0,1]."),
                tags$li("Turn pre-processing off to analyze raw uploaded values."),
                tags$li("Smoothing reduces noise but may affect peak amplitude."),
                tags$li("Detrending removes slow drift in the signal.")
              )
            ),
            
            box(
              title = "Average Metrics (All Cells)",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              
              DT::dataTableOutput("preproc_avg_metrics")
            ),
            
            box(
              title = "Processing Preview",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              
              selectInput("pp_preview_cell", "Preview Cell", choices = NULL),
              plotOutput("pp_preview_plot", height = "300px")
            ),
            
            box(
              title = "Download Processed Data",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              tags$p("Download the processed data in the original wide format."),
              
              selectInput("pp_dl_group", "Select file", choices = NULL),
              
              downloadButton("dl_processed_wide", "Download Processed File (CSV)")
            )
          )
        )
      ),
      
      # Individual Time Course Tab
      tabItem(
        tabName = "time",
        fluidRow(
          box(
            title = "Controls",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            
            textInput("tc_title", "Title", ""),
            textInput("tc_subtitle", "Subtitle", "ΔF/F₀ over time"),
            
            checkboxInput("tc_show_traces", "Show individual cell traces", TRUE),
            
            sliderInput(
              "tc_trace_transparency",
              "Traces transparency (%)",
              0, 100, 65, 1
            ),
            
            checkboxInput("tc_show_ribbon", "Show SEM ribbon", TRUE),
            
            sliderInput(
              "tc_line_width",
              "Line width",
              0.5, 4, 1.6, 0.1
            ),
            
            colourpicker::colourInput(
              "tc_line_color",
              "Line color override",
              value = "#1b9e77"
            ),
            
            sliderInput("tc_title_size", "Title size", 10, 28, 18, 1),
            sliderInput("tc_axis_size", "Axis text size", 8, 28, 12, 1),
            sliderInput("tc_axis_title_size", "Axis title size", 8, 28, 14, 1),
            
            selectInput(
              "tc_legend_pos",
              "Legend position",
              choices = c("none", "bottom", "right", "top", "left"),
              selected = "none"
            ),
            
            tags$details(
              tags$summary("Advanced formatting"),
              
              textInput("tc_x", "X label", "Time (s)"),
              textInput("tc_y", "Y label", "ΔF/F₀"),
              
              selectInput(
                "tc_theme",
                "Theme",
                choices = c("classic", "minimal", "light", "dark"),
                selected = "classic"
              ),
              
              selectInput(
                "tc_font",
                "Font",
                choices = c("Arial", "Helvetica", "Times", "Courier"),
                selected = "Arial"
              ),
              
              checkboxInput("tc_grid_major", "Show major gridlines", TRUE),
              checkboxInput("tc_grid_minor", "Show minor gridlines", FALSE),
              checkboxInput("tc_log_y", "Log10 Y", FALSE),
              checkboxInput("tc_limits", "Custom axis limits", FALSE),
              
              conditionalPanel(
                "input.tc_limits == true",
                numericInput("tc_xmin", "X min", NA),
                numericInput("tc_xmax", "X max", NA),
                numericInput("tc_ymin", "Y min", NA),
                numericInput("tc_ymax", "Y max", NA)
              ),
              
              tags$hr(),
              
              h4("Axis ticks"),
              textInput("tc_x_breaks", "X breaks (comma)", ""),
              textInput("tc_y_breaks", "Y breaks (comma)", ""),
              
              selectInput(
                "tc_tick_format",
                "Tick format",
                choices = c("number", "scientific", "percent"),
                selected = "number"
              )
            )
          ),
          
          box(
            title = "Time Course",
            status = "primary",
            solidHeader = TRUE,
            width = 5,
            
            withSpinner(plotOutput("timecourse_plot", height = "620px"), type = 4),
            
            tags$hr(),
            
            fluidRow(
              column(3,
                selectInput(
                  "tc_dl_fmt",
                  "Format",
                  choices = c("PNG", "PDF", "TIFF", "SVG"),
                  selected = "PNG"
                )
              ),
              column(3,
                numericInput("tc_dl_w", "Width (in)", 12, min = 4, max = 30)
              ),
              column(3,
                numericInput("tc_dl_h", "Height (in)", 8, min = 4, max = 30)
              ),
              column(3,
                numericInput("tc_dl_dpi", "DPI", 300, min = 72, max = 600)
              )
            ),
            
            div(
              style = "margin-top:8px;",
              downloadButton("dl_timecourse_plot_local", "Download Time Course")
            )
          ),
          
          box(
            title = "Time Course Summary",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            htmlOutput("tc_summary_table")
          )
        )
      ),
      
      # Individual Metrics Tab
      tabItem(
        tabName = "metrics",
        mod_metrics_ui("ind_metrics_module")
      ),
      
      # Individual Heatmap Tab
      tabItem(
        tabName = "heatmap",
        fluidRow(
          box(
            title = "Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            selectInput(
              "hmap_normalize",
              "Normalization",
              choices = c(
                "None" = "none",
                "Per cell (row)" = "row",
                "Per time (column)" = "col",
                "Global 0-1" = "global"
              ),
              selected = "row"
            ),
            
            selectInput(
              "hmap_sort",
              "Sort cells by",
              choices = c(
                "Original order" = "original",
                "Peak amplitude" = "amplitude",
                "Time to peak" = "time",
                "Hierarchical clustering" = "cluster"
              ),
              selected = "original"
            ),
            
            selectInput(
              "hmap_palette",
              "Color palette",
              choices = c(
                "Viridis" = "viridis",
                "Plasma" = "plasma",
                "Magma" = "magma",
                "Inferno" = "inferno",
                "Cividis" = "cividis"
              ),
              selected = "viridis"
            ),
            
            checkboxInput("hmap_show_dendro", "Show dendrogram", FALSE),
            
            sliderInput("hmap_font", "Font size", 6, 16, 10, 1),
            
            textInput("hmap_title", "Title", "Calcium Activity Heatmap"),
            
            hr(),
            
            h4("Cell Selection"),
            
            selectInput(
              "hmap_cells",
              "Select cells to display",
              choices = NULL,
              multiple = TRUE
            ),
            
            actionButton(
              "hmap_select_all",
              "Select All",
              class = "btn-sm"
            ),
            
            actionButton(
              "hmap_clear_selection",
              "Clear",
              class = "btn-sm"
            )
          ),
          
          box(
            title = "Heatmap",
            status = "warning",
            solidHeader = TRUE,
            width = 9,
            
            withSpinner(plotOutput("heatmap_plot", height = "700px"), type = 4),
            
            hr(),
            
            downloadButton("dl_heatmap", "Download Heatmap")
          )
        )
      ),
      
      # Individual Tables Tab
      tabItem(
        tabName = "tables",
        mod_tables_ui("ind_tables_module")
      ),
      
      # Metric Guide Tab
      tabItem(
        tabName = "guide",
        mod_metric_guide_ui("guide_module")
      ),
      
      # Individual Export Tab
      tabItem(
        tabName = "export",
        mod_export_ui("ind_export_module")
      ),
      
      # ========== ADVANCED TABS ==========
      
      # Batch Analysis Tab
      tabItem(
        tabName = "batch",
        fluidRow(
          box(
            title = "Batch Processing",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Process Multiple Datasets"),
            p("Upload multiple files and apply the same analysis pipeline to all."),
            
            fileInput(
              "batch_files",
              "Select Multiple Files",
              multiple = TRUE,
              accept = c(".csv", ".txt", ".xlsx", ".xls")
            ),
            
            hr(),
            
            h4("Batch Settings"),
            
            checkboxInput(
              "batch_use_current",
              "Use Current Preprocessing Settings",
              value = TRUE
            ),
            
            checkboxInput(
              "batch_parallel",
              "Enable Parallel Processing",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.batch_parallel",
              numericInput(
                "batch_cores",
                "Number of cores",
                value = 2,
                min = 1,
                max = parallel::detectCores()
              )
            ),
            
            checkboxInput(
              "batch_auto_export",
              "Auto-export Results",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.batch_auto_export == true",
              textInput(
                "batch_export_dir",
                "Export Directory",
                value = "~/calcium_results"
              )
            ),
            
            br(),
            
            actionButton(
              "run_batch",
              "Run Batch Analysis",
              icon = icon("play"),
              class = "btn-success btn-lg"
            ),
            
            br(), br(),
            
            h4("Batch Progress"),
            verbatimTextOutput("batch_progress"),
            
            br(),
            
            h4("Batch Results"),
            DT::dataTableOutput("batch_results"),
            
            br(),
            
            downloadButton(
              "batch_download_all",
              "Download All Results",
              class = "btn-primary"
            )
          )
        )
      ),
      
      # Statistical Analysis Tab
      tabItem(
        tabName = "stats",
        mod_statistical_analysis_ui("stats_module")
      ),
      
      # Correlation Analysis Tab
      tabItem(
        tabName = "correlation",
        fluidRow(
          box(
            title = "Correlation Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                h4("Analysis Settings"),
                
                selectInput(
                  "corr_method",
                  "Correlation Method",
                  choices = c(
                    "Pearson" = "pearson",
                    "Spearman" = "spearman",
                    "Kendall" = "kendall"
                  ),
                  selected = "pearson"
                ),
                
                checkboxInput(
                  "corr_cluster",
                  "Hierarchical clustering",
                  value = TRUE
                ),
                
                sliderInput(
                  "corr_threshold",
                  "Display threshold",
                  min = 0,
                  max = 1,
                  value = 0.3,
                  step = 0.05
                ),
                
                hr(),
                
                h4("Time-lagged Correlation"),
                
                checkboxInput(
                  "corr_lag_enable",
                  "Enable lag analysis",
                  value = FALSE
                ),
                
                conditionalPanel(
                  condition = "input.corr_lag_enable",
                  
                  numericInput(
                    "corr_max_lag",
                    "Maximum lag (frames)",
                    value = 10,
                    min = 1,
                    max = 50
                  )
                ),
                
                hr(),
                
                h4("Network Analysis"),
                
                checkboxInput(
                  "corr_network",
                  "Show network view",
                  value = FALSE
                ),
                
                conditionalPanel(
                  condition = "input.corr_network",
                  
                  sliderInput(
                    "corr_network_threshold",
                    "Edge threshold",
                    min = 0.5,
                    max = 1,
                    value = 0.7,
                    step = 0.05
                  )
                )
              ),
              
              column(8,
                tabsetPanel(
                  tabPanel(
                    "Correlation Matrix",
                    br(),
                    withSpinner(plotOutput("corr_matrix_plot", height = "600px"), type = 4)
                  ),
                  
                  tabPanel(
                    "Lag Analysis",
                    br(),
                    conditionalPanel(
                      condition = "input.corr_lag_enable",
                      withSpinner(plotOutput("corr_lag_plot", height = "600px"), type = 4)
                    )
                  ),
                  
                  tabPanel(
                    "Network View",
                    br(),
                    conditionalPanel(
                      condition = "input.corr_network",
                      withSpinner(plotOutput("corr_network_plot", height = "600px"), type = 4)
                    )
                  ),
                  
                  tabPanel(
                    "Statistics",
                    br(),
                    DT::dataTableOutput("corr_stats_table")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Classification Tab
      tabItem(
        tabName = "classification",
        fluidRow(
          box(
            title = "Cell Classification",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                h4("Classification Settings"),
                
                selectInput(
                  "class_method",
                  "Classification Method",
                  choices = c(
                    "Responder/Non-responder" = "responder",
                    "Response Pattern" = "pattern",
                    "K-means Clustering" = "kmeans",
                    "Hierarchical Clustering" = "hclust"
                  ),
                  selected = "responder"
                ),
                
                conditionalPanel(
                  condition = "input.class_method == 'responder'",
                  
                  numericInput(
                    "class_resp_threshold",
                    "Response threshold (ΔF/F₀)",
                    value = 0.1,
                    min = 0,
                    step = 0.05
                  ),
                  
                  numericInput(
                    "class_resp_snr",
                    "Minimum SNR",
                    value = 2,
                    min = 1,
                    step = 0.5
                  )
                ),
                
                conditionalPanel(
                  condition = "input.class_method == 'pattern'",
                  
                  checkboxGroupInput(
                    "class_pattern_features",
                    "Features to use",
                    choices = c(
                      "Peak amplitude" = "peak",
                      "Time to peak" = "time",
                      "Rise time" = "rise",
                      "Decay time" = "decay",
                      "AUC" = "auc"
                    ),
                    selected = c("peak", "time", "rise")
                  )
                ),
                
                conditionalPanel(
                  condition = "input.class_method == 'kmeans'",
                  
                  numericInput(
                    "class_kmeans_k",
                    "Number of clusters",
                    value = 3,
                    min = 2,
                    max = 10
                  )
                ),
                
                hr(),
                
                actionButton(
                  "run_classification",
                  "Run Classification",
                  icon = icon("tags"),
                  class = "btn-primary"
                )
              ),
              
              column(8,
                tabsetPanel(
                  tabPanel(
                    "Classification Results",
                    br(),
                    withSpinner(plotOutput("class_results_plot", height = "500px"), type = 4),
                    br(),
                    DT::dataTableOutput("class_results_table")
                  ),
                  
                  tabPanel(
                    "Class Comparison",
                    br(),
                    withSpinner(plotOutput("class_comparison_plot", height = "500px"), type = 4)
                  ),
                  
                  tabPanel(
                    "Export Classification",
                    br(),
                    downloadButton(
                      "class_download",
                      "Download Classification Results",
                      class = "btn-primary"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Settings Tab
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Application Settings",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            h4("Performance Settings"),
            
            numericInput(
              "max_cells",
              "Maximum Cells to Process",
              value = 1000,
              min = 100,
              max = 5000,
              step = 100
            ),
            
            numericInput(
              "chunk_size",
              "Processing Chunk Size",
              value = 100,
              min = 10,
              max = 500,
              step = 10
            ),
            
            checkboxInput(
              "use_parallel",
              "Enable Parallel Processing",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.use_parallel == true",
              
              numericInput(
                "n_cores",
                "Number of Cores",
                value = 2,
                min = 1,
                max = parallel::detectCores(),
                step = 1
              )
            ),
            
            hr(),
            
            h4("Display Settings"),
            
            selectInput(
              "theme",
              "UI Theme",
              choices = list(
                "Default" = "default",
                "Dark" = "dark",
                "Light" = "light"
              ),
              selected = "default"
            ),
            
            checkboxInput(
              "show_tooltips",
              "Show Tooltips",
              value = TRUE
            ),
            
            checkboxInput(
              "auto_save",
              "Auto-save Session",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.auto_save",
              
              numericInput(
                "auto_save_interval",
                "Auto-save interval (minutes)",
                value = 10,
                min = 1,
                max = 60
              )
            )
          ),
          
          box(
            title = "Analysis Defaults",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            h4("Default Parameters"),
            
            numericInput(
              "default_baseline",
              "Default Baseline Frames",
              value = 20,
              min = 5,
              max = 100,
              step = 5
            ),
            
            selectInput(
              "default_norm_method",
              "Default Normalization",
              choices = list(
                "Mean" = "mean",
                "Median" = "median",
                "Percentile" = "percentile"
              ),
              selected = "mean"
            ),
            
            checkboxInput(
              "auto_detect_peaks",
              "Auto-detect Peaks",
              value = TRUE
            ),
            
            checkboxInput(
              "auto_classify_cells",
              "Auto-classify Cells",
              value = FALSE
            ),
            
            hr(),
            
            h4("Export Defaults"),
            
            selectInput(
              "default_export_format",
              "Default Export Format",
              choices = list(
                "CSV" = "csv",
                "Excel" = "xlsx",
                "R Data" = "rds"
              ),
              selected = "csv"
            ),
            
            numericInput(
              "plot_dpi",
              "Default Plot DPI",
              value = 300,
              min = 72,
              max = 600,
              step = 50
            ),
            
            br(),
            
            actionButton(
              "save_settings",
              "Save Settings",
              icon = icon("save"),
              class = "btn-primary"
            ),
            
            actionButton(
              "reset_settings",
              "Reset to Defaults",
              icon = icon("undo"),
              class = "btn-warning"
            )
          )
        )
      ),
      
      # Help Tab
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            title = "Documentation & Tips",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tags$h3("Calcium Imaging Analysis App - Complete Guide"),
            
            tags$h4("Getting Started"),
            tags$ol(
              tags$li("Group Comparison: Upload processed ΔF/F₀ files and append multiple groups for statistical comparison."),
              tags$li("Individual Analysis: Upload wide matrices, compute ΔF/F₀ in Pre-processing if needed, then analyze individual datasets."),
              tags$li("Use the Metric Guide to understand how each metric is calculated with your data.")
            ),
            
            tags$hr(),
            
            tags$h4("Data Format Requirements"),
            tags$ul(
              tags$li("Files must be CSV, TXT, or Excel format"),
              tags$li("First column should contain time values"),
              tags$li("Subsequent columns contain signal values for each cell/ROI"),
              tags$li("Column headers are recommended but not required"),
              tags$li("Missing values (NA) are handled automatically")
            ),
            
            tags$hr(),
            
            tags$h4("Key Features"),
            
            tags$h5("1. Group Comparison"),
            tags$p("Compare multiple experimental groups with:"),
            tags$ul(
              tags$li("Time course plots with SEM ribbons"),
              tags$li("Statistical testing (ANOVA, t-test, Wilcoxon, Kruskal-Wallis)"),
              tags$li("Post-hoc analysis with multiple comparison corrections"),
              tags$li("Interactive and static visualizations"),
              tags$li("Comprehensive metrics comparison")
            ),
            
            tags$h5("2. Individual Analysis"),
            tags$p("Analyze single datasets with:"),
            tags$ul(
              tags$li("Flexible preprocessing options"),
              tags$li("Multiple baseline correction methods"),
              tags$li("Smoothing and detrending"),
              tags$li("Per-cell and population metrics"),
              tags$li("Interactive metric guide"),
              tags$li("Customizable visualizations")
            ),
            
            tags$h5("3. Advanced Features"),
            tags$ul(
              tags$li("Batch processing for multiple files"),
              tags$li("Correlation and network analysis"),
              tags$li("Cell classification (responders/non-responders)"),
              tags$li("Spike detection and analysis"),
              tags$li("Time-lagged correlation"),
              tags$li("Parallel processing support")
            ),
            
            tags$hr(),
            
            tags$h4("Metrics Explained"),
            
            tags$dl(
              tags$dt("Peak ΔF/F₀"),
              tags$dd("Maximum fluorescence change relative to baseline"),
              
              tags$dt("Time to Peak"),
              tags$dd("Time from stimulus onset to peak response"),
              
              tags$dt("Rise Time (10-90%)"),
              tags$dd("Time taken to rise from 10% to 90% of peak amplitude"),
              
              tags$dt("Half Width (HWHM)"),
              tags$dd("Half width at half maximum amplitude"),
              
              tags$dt("AUC"),
              tags$dd("Area under the curve (integrated response)"),
              
              tags$dt("Ca²⁺ Entry Rate"),
              tags$dd("Maximum rate of fluorescence increase"),
              
              tags$dt("SNR"),
              tags$dd("Signal-to-noise ratio (peak/baseline noise)")
            ),
            
            tags$hr(),
            
            tags$h4("Tips for Best Results"),
            tags$ul(
              tags$li("Ensure consistent sampling rates across files"),
              tags$li("Use at least 20 frames for baseline calculation"),
              tags$li("Apply smoothing cautiously - it may affect peak detection"),
              tags$li("Check the Metric Guide to verify calculations on your data"),
              tags$li("Export results in multiple formats for flexibility"),
              tags$li("Save your session regularly when working with large datasets")
            ),
            
            tags$hr(),
            
            tags$h4("Troubleshooting"),
            
            tags$h5("Common Issues:"),
            tags$dl(
              tags$dt("High memory usage"),
              tags$dd("Clear cache, reduce number of cells, enable chunk processing"),
              
              tags$dt("Slow performance"),
              tags$dd("Enable parallel processing, increase chunk size, close other applications"),
              
              tags$dt("Import errors"),
              tags$dd("Check file format, ensure numeric data, verify time column"),
              
              tags$dt("No metrics calculated"),
              tags$dd("Check baseline frames, verify signal quality, ensure proper normalization")
            ),
            
            tags$hr(),
            
            tags$h4("Citation"),
            tags$p("If you use this app in your research, please cite:"),
            tags$blockquote(
              "Universal Calcium Imaging Analysis App v2.5 (2024)",
              tags$br(),
              "Comprehensive toolkit for calcium imaging data analysis"
            ),
            
            tags$hr(),
            
            tags$h4("Contact & Support"),
            tags$p("For bug reports, feature requests, or questions, please contact the development team."),
            
            tags$hr(),
            
            tags$h4("Version Information"),
            tags$p(paste("App Version: 2.5")),
            tags$p(paste("R Version:", R.version.string)),
            tags$p(paste("Last Updated:", Sys.Date()))
          )
        )
      )
    )
  )
)

# Source the server function
source("app_complete_server.R")

# Run the application
shinyApp(ui = ui, server = server)