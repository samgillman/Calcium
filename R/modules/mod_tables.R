# Tables Module for Calcium Imaging Analysis App

# Module UI
mod_tables_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Data Tables",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        tabsetPanel(
          id = ns("tables_tabs"),
          
          # Per-cell metrics tab
          tabPanel(
            "Cell Metrics",
            icon = icon("table"),
            br(),
            h4("Individual Cell Metrics"),
            DT::dataTableOutput(ns("cell_metrics_table")),
            br(),
            downloadButton(
              ns("download_cell_metrics"),
              "Download Cell Metrics (CSV)",
              class = "btn-primary"
            )
          ),
          
          # Summary statistics tab
          tabPanel(
            "Summary Statistics",
            icon = icon("chart-bar"),
            br(),
            h4("Summary Statistics by Group"),
            DT::dataTableOutput(ns("summary_stats_table")),
            br(),
            downloadButton(
              ns("download_summary"),
              "Download Summary (CSV)",
              class = "btn-primary"
            )
          ),
          
          # Time course summary tab
          tabPanel(
            "Time Course Summary",
            icon = icon("clock"),
            br(),
            h4("Time Course Summary (Mean ± SEM)"),
            DT::dataTableOutput(ns("timecourse_summary_table")),
            br(),
            downloadButton(
              ns("download_timecourse"),
              "Download Time Course (CSV)",
              class = "btn-primary"
            )
          ),
          
          # Raw data tab
          tabPanel(
            "Raw Data",
            icon = icon("database"),
            br(),
            h4("Processed Data (Wide Format)"),
            selectInput(
              ns("raw_data_group"),
              "Select Dataset",
              choices = NULL
            ),
            DT::dataTableOutput(ns("raw_data_table")),
            br(),
            downloadButton(
              ns("download_raw"),
              "Download Raw Data (CSV)",
              class = "btn-primary"
            )
          )
        )
      )
    )
  )
}

# Module Server
mod_tables_server <- function(id, data_module, metrics_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update group selector
    observe({
      data <- data_module$data()
      if (!is.null(data)) {
        updateSelectInput(
          session,
          "raw_data_group",
          choices = names(data),
          selected = names(data)[1]
        )
      }
    })
    
    # Cell metrics table
    output$cell_metrics_table <- DT::renderDataTable({
      req(metrics_module$metrics())
      
      metrics <- metrics_module$metrics()
      
      # Format numeric columns
      numeric_cols <- sapply(metrics, is.numeric)
      
      DT::datatable(
        metrics,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        filter = 'top',
        class = 'display compact'
      ) %>%
        DT::formatRound(columns = which(numeric_cols), digits = 4)
    })
    
    # Summary statistics table
    output$summary_stats_table <- DT::renderDataTable({
      req(metrics_module$group_stats())
      
      stats <- metrics_module$group_stats()
      
      # Pivot wider for better display
      stats_wide <- stats %>%
        select(Group, Metric, Mean, SEM, N) %>%
        pivot_wider(
          names_from = Metric,
          values_from = c(Mean, SEM, N),
          names_glue = "{Metric}_{.value}"
        )
      
      DT::datatable(
        stats_wide,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
        DT::formatRound(columns = 2:ncol(stats_wide), digits = 4)
    })
    
    # Time course summary table
    output$timecourse_summary_table <- DT::renderDataTable({
      req(data_module$data())
      
      data_list <- data_module$data()
      
      # Calculate time course summary
      summary_list <- list()
      
      for (name in names(data_list)) {
        df <- data_list[[name]]
        
        # Calculate mean and SEM for each time point
        time_vec <- df$Time
        cell_cols <- setdiff(names(df), "Time")
        
        if (length(cell_cols) > 0) {
          # Convert to matrix
          mat <- as.matrix(df[, ..cell_cols])
          
          # Calculate statistics
          means <- rowMeans(mat, na.rm = TRUE)
          sds <- apply(mat, 1, sd, na.rm = TRUE)
          ns <- rowSums(!is.na(mat))
          sems <- sds / sqrt(ns)
          
          summary_list[[name]] <- data.frame(
            Group = name,
            Time = time_vec,
            Mean = means,
            SD = sds,
            SEM = sems,
            N = ns,
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (length(summary_list) > 0) {
        summary_df <- dplyr::bind_rows(summary_list)
        
        # Create mean ± SEM column
        summary_df$`Mean ± SEM` <- paste0(
          round(summary_df$Mean, 4),
          " ± ",
          round(summary_df$SEM, 4)
        )
        
        # Pivot for display
        summary_wide <- summary_df %>%
          select(Group, Time, `Mean ± SEM`) %>%
          pivot_wider(
            names_from = Group,
            values_from = `Mean ± SEM`
          )
        
        DT::datatable(
          summary_wide,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = 'display compact'
        )
      }
    })
    
    # Raw data table
    output$raw_data_table <- DT::renderDataTable({
      req(data_module$data(), input$raw_data_group)
      
      data_list <- data_module$data()
      
      if (input$raw_data_group %in% names(data_list)) {
        df <- data_list[[input$raw_data_group]]
        
        # Format for display
        numeric_cols <- sapply(df, is.numeric)
        
        DT::datatable(
          df,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = 'display compact'
        ) %>%
          DT::formatRound(columns = which(numeric_cols), digits = 4)
      }
    })
    
    # Download handlers
    output$download_cell_metrics <- downloadHandler(
      filename = function() {
        paste0("cell_metrics_", Sys.Date(), ".csv")
      },
      content = function(file) {
        metrics <- metrics_module$metrics()
        write.csv(metrics, file, row.names = FALSE)
      }
    )
    
    output$download_summary <- downloadHandler(
      filename = function() {
        paste0("summary_statistics_", Sys.Date(), ".csv")
      },
      content = function(file) {
        stats <- metrics_module$group_stats()
        write.csv(stats, file, row.names = FALSE)
      }
    )
    
    output$download_timecourse <- downloadHandler(
      filename = function() {
        paste0("timecourse_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data_list <- data_module$data()
        
        # Calculate summary
        summary_list <- list()
        
        for (name in names(data_list)) {
          df <- data_list[[name]]
          time_vec <- df$Time
          cell_cols <- setdiff(names(df), "Time")
          
          if (length(cell_cols) > 0) {
            mat <- as.matrix(df[, ..cell_cols])
            means <- rowMeans(mat, na.rm = TRUE)
            sds <- apply(mat, 1, sd, na.rm = TRUE)
            ns <- rowSums(!is.na(mat))
            sems <- sds / sqrt(ns)
            
            summary_list[[name]] <- data.frame(
              Group = name,
              Time = time_vec,
              Mean = means,
              SD = sds,
              SEM = sems,
              N = ns
            )
          }
        }
        
        if (length(summary_list) > 0) {
          summary_df <- dplyr::bind_rows(summary_list)
          write.csv(summary_df, file, row.names = FALSE)
        }
      }
    )
    
    output$download_raw <- downloadHandler(
      filename = function() {
        paste0("raw_data_", input$raw_data_group, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data_list <- data_module$data()
        if (input$raw_data_group %in% names(data_list)) {
          df <- data_list[[input$raw_data_group]]
          write.csv(df, file, row.names = FALSE)
        }
      }
    )
  })
}