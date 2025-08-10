# Data Loading Module for Calcium Imaging Analysis App

# Module UI
mod_data_loading_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h3("Data Import", icon("upload")),
        hr()
      )
    ),
    
    fluidRow(
      column(6,
        fileInput(
          ns("file_input"),
          "Choose Data File",
          accept = c(".csv", ".txt", ".xlsx", ".xls"),
          multiple = TRUE,
          width = "100%"
        ),
        
        conditionalPanel(
          condition = "output.file_uploaded == true",
          ns = ns,
          
          checkboxInput(
            ns("has_header"),
            "File has header row",
            value = TRUE
          ),
          
          selectInput(
            ns("time_column"),
            "Select Time Column",
            choices = NULL,
            width = "100%"
          ),
          
          actionButton(
            ns("validate_data"),
            "Validate Data",
            icon = icon("check"),
            class = "btn-primary",
            width = "100%"
          )
        )
      ),
      
      column(6,
        # Validation status
        uiOutput(ns("validation_status")),
        
        # Data preview
        conditionalPanel(
          condition = "output.data_validated == true",
          ns = ns,
          h4("Data Preview"),
          DT::dataTableOutput(ns("data_preview"))
        )
      )
    ),
    
    fluidRow(
      column(12,
        br(),
        conditionalPanel(
          condition = "output.data_validated == true",
          ns = ns,
          
          h4("Data Summary"),
          verbatimTextOutput(ns("data_summary")),
          
          # Group labeling for multiple files
          conditionalPanel(
            condition = "output.multiple_files == true",
            ns = ns,
            h4("Group Labels"),
            uiOutput(ns("group_labels_ui"))
          )
        )
      )
    ),
    
    # Demo data option
    fluidRow(
      column(12,
        br(),
        h4("Or Use Demo Data"),
        actionButton(
          ns("load_demo"),
          "Load Demo Dataset",
          icon = icon("database"),
          class = "btn-info"
        ),
        helpText("Load a simulated calcium imaging dataset for testing")
      )
    )
  )
}

# Module Server
mod_data_loading_server <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      raw_data = NULL,
      validated_data = NULL,
      validation_errors = NULL,
      file_names = NULL,
      group_labels = NULL
    )
    
    # File upload handler
    observeEvent(input$file_input, {
      req(input$file_input)
      
      withProgress(message = "Loading files...", value = 0, {
        files <- input$file_input
        n_files <- nrow(files)
        
        data_list <- list()
        file_names <- character()
        
        for (i in 1:n_files) {
          incProgress(1/n_files, detail = paste("File", i, "of", n_files))
          
          tryCatch({
            # Read file
            df <- safe_read(files$datapath[i])
            
            # Store with file name
            file_names[i] <- tools::file_path_sans_ext(files$name[i])
            data_list[[file_names[i]]] <- df
            
          }, error = function(e) {
            showNotification(
              paste("Error reading", files$name[i], ":", e$message),
              type = "error"
            )
          })
        }
        
        if (length(data_list) > 0) {
          values$raw_data <- data_list
          values$file_names <- file_names
          
          # Update time column choices
          first_df <- data_list[[1]]
          updateSelectInput(
            session,
            "time_column",
            choices = names(first_df),
            selected = names(first_df)[1]
          )
        }
      })
    })
    
    # Demo data loader
    observeEvent(input$load_demo, {
      withProgress(message = "Generating demo data...", value = 0, {
        
        # Generate multiple demo groups
        demo_data <- list()
        
        incProgress(0.3, detail = "Creating control group...")
        control <- simulate_calcium_data(
          label = "Control",
          n_cells = 30,
          time_points = 200,
          sampling_rate = 5,
          peak_time = 40,
          amplitude = 0.5,
          noise = 0.05
        )
        demo_data[["Control"]] <- control$dt
        
        incProgress(0.6, detail = "Creating treatment group...")
        treatment <- simulate_calcium_data(
          label = "Treatment",
          n_cells = 30,
          time_points = 200,
          sampling_rate = 5,
          peak_time = 35,
          amplitude = 0.8,
          noise = 0.06
        )
        demo_data[["Treatment"]] <- treatment$dt
        
        incProgress(0.9, detail = "Finalizing...")
        
        values$raw_data <- demo_data
        values$file_names <- names(demo_data)
        values$validated_data <- demo_data
        values$validation_errors <- NULL
        
        # Update UI
        first_df <- demo_data[[1]]
        updateSelectInput(
          session,
          "time_column",
          choices = names(first_df),
          selected = "Time"
        )
        
        showNotification(
          "Demo data loaded successfully!",
          type = "success"
        )
      })
    })
    
    # Validate data
    observeEvent(input$validate_data, {
      req(values$raw_data)
      
      withProgress(message = "Validating data...", value = 0, {
        errors <- list()
        validated <- list()
        
        n_files <- length(values$raw_data)
        
        for (i in seq_along(values$raw_data)) {
          name <- names(values$raw_data)[i]
          df <- values$raw_data[[name]]
          
          incProgress(1/n_files, detail = paste("Validating", name))
          
          # Ensure time column is first
          if (!is.null(input$time_column)) {
            df <- ensure_time_first(df, input$time_column)
          }
          
          # Coerce to numeric
          df <- coerce_numeric_dt(df)
          
          # Validate
          error <- validate_input_file(df)
          
          if (is.null(error)) {
            validated[[name]] <- df
          } else {
            errors[[name]] <- error
          }
        }
        
        if (length(errors) > 0) {
          values$validation_errors <- errors
          values$validated_data <- NULL
          
          error_msg <- paste(
            names(errors),
            errors,
            sep = ": ",
            collapse = "\n"
          )
          
          showNotification(
            paste("Validation errors:\n", error_msg),
            type = "error",
            duration = 10
          )
        } else {
          values$validated_data <- validated
          values$validation_errors <- NULL
          
          showNotification(
            "Data validated successfully!",
            type = "success"
          )
        }
      })
    })
    
    # Output: File uploaded
    output$file_uploaded <- reactive({
      !is.null(values$raw_data)
    })
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    # Output: Data validated
    output$data_validated <- reactive({
      !is.null(values$validated_data)
    })
    outputOptions(output, "data_validated", suspendWhenHidden = FALSE)
    
    # Output: Multiple files
    output$multiple_files <- reactive({
      length(values$file_names) > 1
    })
    outputOptions(output, "multiple_files", suspendWhenHidden = FALSE)
    
    # Validation status UI
    output$validation_status <- renderUI({
      if (!is.null(values$validation_errors)) {
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong("Validation Failed"),
          br(),
          lapply(names(values$validation_errors), function(name) {
            tagList(
              strong(paste0(name, ": ")),
              values$validation_errors[[name]],
              br()
            )
          })
        )
      } else if (!is.null(values$validated_data)) {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          strong("Data Validated Successfully"),
          br(),
          paste(length(values$validated_data), "file(s) loaded")
        )
      }
    })
    
    # Data preview
    output$data_preview <- DT::renderDataTable({
      req(values$validated_data)
      
      # Show first file preview
      df <- values$validated_data[[1]]
      
      # Show first 10 rows and up to 10 columns
      preview_df <- df[1:min(10, nrow(df)), 1:min(10, ncol(df))]
      
      DT::datatable(
        preview_df,
        options = list(
          pageLength = 5,
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = 1:ncol(preview_df), digits = 3)
    })
    
    # Data summary
    output$data_summary <- renderPrint({
      req(values$validated_data)
      
      for (name in names(values$validated_data)) {
        df <- values$validated_data[[name]]
        cat("Dataset:", name, "\n")
        cat("Dimensions:", nrow(df), "time points x", ncol(df)-1, "cells\n")
        cat("Time range:", round(min(df$Time), 2), "-", round(max(df$Time), 2), "\n")
        cat("Sampling interval:", round(mean(diff(df$Time)), 3), "\n")
        cat("\n")
      }
    })
    
    # Group labels UI
    output$group_labels_ui <- renderUI({
      req(values$file_names)
      
      lapply(seq_along(values$file_names), function(i) {
        textInput(
          ns(paste0("group_label_", i)),
          paste("Label for", values$file_names[i]),
          value = values$file_names[i],
          width = "100%"
        )
      })
    })
    
    # Collect group labels
    observe({
      req(values$file_names)
      
      labels <- character()
      for (i in seq_along(values$file_names)) {
        label_input <- input[[paste0("group_label_", i)]]
        if (!is.null(label_input)) {
          labels[i] <- label_input
        } else {
          labels[i] <- values$file_names[i]
        }
      }
      values$group_labels <- labels
    })
    
    # Return values for use in other modules
    return(
      list(
        data = reactive(values$validated_data),
        group_labels = reactive(values$group_labels),
        file_names = reactive(values$file_names)
      )
    )
  })
}

# Helper function to simulate calcium data
simulate_calcium_data <- function(label = "Group",
                                  n_cells = 30,
                                  time_points = 200,
                                  sampling_rate = 5,
                                  peak_time = 40,
                                  amplitude = 0.6,
                                  noise = 0.08) {
  
  time <- seq(0, time_points/sampling_rate, by = 1/sampling_rate)
  
  # Create base calcium transient shape
  base <- exp(-((time - peak_time)^2) / (2 * (peak_time/6)^2))
  base <- base / max(base)
  
  # Generate cell responses with variability
  cells <- sapply(seq_len(n_cells), function(i) {
    # Add variability to amplitude
    cell_amp <- rlnorm(1, log(amplitude), 0.3)
    
    # Add variability to timing
    time_shift <- rnorm(1, 0, 2)
    shifted_base <- exp(-((time - peak_time - time_shift)^2) / (2 * (peak_time/6)^2))
    shifted_base <- shifted_base / max(shifted_base)
    
    # Create response
    response <- shifted_base * cell_amp
    
    # Add noise
    response <- response + stats::filter(
      rnorm(length(time), 0, noise),
      rep(1/3, 3),
      sides = 2
    )
    
    # Add occasional non-responders
    if (runif(1) < 0.1) {
      response <- response * 0.1
    }
    
    response
  })
  
  # Create data.table
  dt <- data.table::as.data.table(cbind(Time = time, cells))
  data.table::setnames(dt, c("Time", paste0("Cell", seq_len(n_cells))))
  
  list(label = label, dt = dt)
}