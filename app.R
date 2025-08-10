#!/usr/bin/env Rscript

# Posit Connect entrypoint for the Calcium Imaging Analysis app
# Return a shiny.appobj constructed from the complete app file

shiny::shinyAppFile("app_complete.R")
