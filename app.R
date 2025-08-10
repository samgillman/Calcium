#!/usr/bin/env Rscript

# Universal Calcium Imaging Analysis App - Modularized Version 2.4
# Improved performance, memory management, and code organization

# Load global configuration and utilities
source("R/global.R")
source("R/utils/validation.R")
source("R/utils/calculations.R")

# Load modules
source("R/modules/mod_data_loading.R")
source("R/modules/mod_preprocessing.R")
source("R/modules/mod_metrics.R")
source("R/modules/mod_visualization.R")

# Enable shinyjs
shinyjs::useShinyjs()

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Calcium Imaging Analysis",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('show_help', Math.random())",
        icon("question-circle"),
        "Help"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Import", tabName = "data", icon = icon("upload")),
      menuItem("Preprocessing", tabName = "preprocess", icon = icon("filter")),
      menuItem("Metrics", tabName = "metrics", icon = icon("calculator")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-area")),
      menuItem("Batch Analysis", tabName = "batch", icon = icon("layer-group")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    ),
    
    br(),
    
    # Memory usage indicator
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
      )
    )
  ),
  
  dashboardBody(
    # Custom CSS
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
      "))
    ),
    
    # Tab content
    tabItems(
      # Data Import Tab
      tabItem(
        tabName = "data",
        mod_data_loading_ui("data_module")
      ),
      
      # Preprocessing Tab
      tabItem(
        tabName = "preprocess",
        mod_preprocessing_ui("preprocess_module")
      ),
      
      # Metrics Tab
      tabItem(
        tabName = "metrics",
        mod_metrics_ui("metrics_module")
      ),
      
      # Visualization Tab
      tabItem(
        tabName = "visualization",
        mod_visualization_ui("viz_module")
      ),
      
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
            DT::dataTableOutput("batch_results")
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
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Initialize reactive values for session management
  session_values <- reactiveValues(
    total_memory = 0,
    settings = list(),
    batch_results = NULL
  )
  
  # Load modules
  data_module <- mod_data_loading_server("data_module", session)
  preprocess_module <- mod_preprocessing_server("preprocess_module", data_module)
  metrics_module <- mod_metrics_server("metrics_module", preprocess_module)
  viz_module <- mod_visualization_server("viz_module", preprocess_module, metrics_module)
  
  # Memory management
  output$memory_usage <- renderPrint({
    # Get memory usage
    mem_used <- pryr::mem_used()
    cat("Memory Used:", format(mem_used, units = "MB"), "\n")
    
    # Update total memory
    session_values$total_memory <- as.numeric(mem_used)
    
    # Show warning if memory usage is high
    if (session_values$total_memory > 500) {
      cat("Warning: High memory usage!\n")
      cat("Consider clearing cache.\n")
    }
  })
  
  # Clear memory cache
  observeEvent(input$clear_memory, {
    # Clear unused objects
    gc(verbose = FALSE)
    
    showNotification(
      "Memory cache cleared",
      type = "success"
    )
  })
  
  # Batch processing
  observeEvent(input$run_batch, {
    req(input$batch_files)
    
    withProgress(message = "Running batch analysis...", value = 0, {
      
      files <- input$batch_files
      n_files <- nrow(files)
      results_list <- list()
      
      for (i in 1:n_files) {
        incProgress(1/n_files, detail = paste("Processing", files$name[i]))
        
        tryCatch({
          # Read file
          df <- safe_read(files$datapath[i])
          
          # Apply preprocessing if selected
          if (input$batch_use_current) {
            # Get current preprocessing settings from module
            prep_params <- preprocess_module$preprocessing_params()
            
            if (prep_params$normalized) {
              df <- apply_normalization(df, prep_params$baseline_frames)
            }
            if (prep_params$smoothed) {
              df <- apply_smoothing(df)
            }
            if (prep_params$detrended) {
              df <- apply_detrending(df)
            }
          }
          
          # Calculate metrics
          metrics <- calculate_cell_metrics_improved(
            df,
            baseline_frames = input$default_baseline
          )
          
          metrics$File <- files$name[i]
          results_list[[i]] <- metrics
          
          # Auto-export if selected
          if (input$batch_auto_export) {
            export_path <- file.path(
              input$batch_export_dir,
              paste0(tools::file_path_sans_ext(files$name[i]), "_results.csv")
            )
            write.csv(metrics, export_path, row.names = FALSE)
          }
          
        }, error = function(e) {
          showNotification(
            paste("Error processing", files$name[i], ":", e$message),
            type = "error"
          )
        })
      }
      
      # Combine results
      session_values$batch_results <- dplyr::bind_rows(results_list)
      
      showNotification(
        paste("Batch processing complete!", n_files, "files processed."),
        type = "success"
      )
    })
  })
  
  # Batch results table
  output$batch_results <- DT::renderDataTable({
    req(session_values$batch_results)
    
    DT::datatable(
      session_values$batch_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Save settings
  observeEvent(input$save_settings, {
    settings <- list(
      max_cells = input$max_cells,
      chunk_size = input$chunk_size,
      use_parallel = input$use_parallel,
      n_cores = input$n_cores,
      theme = input$theme,
      show_tooltips = input$show_tooltips,
      auto_save = input$auto_save,
      default_baseline = input$default_baseline,
      default_norm_method = input$default_norm_method,
      auto_detect_peaks = input$auto_detect_peaks,
      default_export_format = input$default_export_format,
      plot_dpi = input$plot_dpi
    )
    
    # Save to file
    saveRDS(settings, "calcium_app_settings.rds")
    
    showNotification(
      "Settings saved successfully",
      type = "success"
    )
  })
  
  # Reset settings
  observeEvent(input$reset_settings, {
    updateNumericInput(session, "max_cells", value = 1000)
    updateNumericInput(session, "chunk_size", value = 100)
    updateCheckboxInput(session, "use_parallel", value = FALSE)
    updateSelectInput(session, "theme", selected = "default")
    updateCheckboxInput(session, "show_tooltips", value = TRUE)
    updateCheckboxInput(session, "auto_save", value = FALSE)
    updateNumericInput(session, "default_baseline", value = 20)
    updateSelectInput(session, "default_norm_method", selected = "mean")
    updateCheckboxInput(session, "auto_detect_peaks", value = TRUE)
    updateSelectInput(session, "default_export_format", selected = "csv")
    updateNumericInput(session, "plot_dpi", value = 300)
    
    showNotification(
      "Settings reset to defaults",
      type = "info"
    )
  })
  
  # Load settings on startup
  observe({
    if (file.exists("calcium_app_settings.rds")) {
      settings <- readRDS("calcium_app_settings.rds")
      
      updateNumericInput(session, "max_cells", value = settings$max_cells)
      updateNumericInput(session, "chunk_size", value = settings$chunk_size)
      updateCheckboxInput(session, "use_parallel", value = settings$use_parallel)
      updateNumericInput(session, "n_cores", value = settings$n_cores)
      updateSelectInput(session, "theme", selected = settings$theme)
      updateCheckboxInput(session, "show_tooltips", value = settings$show_tooltips)
      updateCheckboxInput(session, "auto_save", value = settings$auto_save)
      updateNumericInput(session, "default_baseline", value = settings$default_baseline)
      updateSelectInput(session, "default_norm_method", selected = settings$default_norm_method)
      updateCheckboxInput(session, "auto_detect_peaks", value = settings$auto_detect_peaks)
      updateSelectInput(session, "default_export_format", selected = settings$default_export_format)
      updateNumericInput(session, "plot_dpi", value = settings$plot_dpi)
    }
  })
  
  # Show help modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Calcium Imaging Analysis Help",
      size = "l",
      
      h4("Quick Start Guide"),
      p("1. Import your calcium imaging data (CSV, Excel, or TXT format)"),
      p("2. Apply preprocessing (normalization, smoothing, detrending)"),
      p("3. Calculate metrics (peak, AUC, rise time, etc.)"),
      p("4. Visualize results with various plot types"),
      p("5. Export results and figures"),
      
      hr(),
      
      h4("Data Format"),
      p("Your data should have:"),
      tags$ul(
        tags$li("First column: Time values"),
        tags$li("Subsequent columns: Signal values for each cell/ROI"),
        tags$li("Column headers are recommended")
      ),
      
      hr(),
      
      h4("Key Features"),
      tags$ul(
        tags$li("Batch processing for multiple files"),
        tags$li("Interactive visualizations"),
        tags$li("Spike detection and analysis"),
        tags$li("Group comparisons"),
        tags$li("Memory management for large datasets"),
        tags$li("Customizable analysis parameters")
      ),
      
      hr(),
      
      h4("Tips"),
      tags$ul(
        tags$li("Use demo data to explore features"),
        tags$li("Save your settings for consistent analysis"),
        tags$li("Clear cache if memory usage is high"),
        tags$li("Enable parallel processing for large datasets")
      ),
      
      footer = modalButton("Close")
    ))
  })
  
  # Session cleanup
  session$onSessionEnded(function() {
    # Clear large objects
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    
    # Force garbage collection
    gc(verbose = FALSE)
    
    # Save session if auto-save is enabled
    if (isTRUE(session_values$settings$auto_save)) {
      saveRDS(
        list(
          data = data_module$data(),
          settings = session_values$settings
        ),
        paste0("calcium_session_", Sys.Date(), ".rds")
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)