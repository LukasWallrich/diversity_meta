
  # To launch the app manually, use shiny::shinyAppDir(YOURPATH) or the Run App button in RStudio

  library(dplyr)

  # Ensure required packages are installed
  if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
  }
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
  }
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  }
if (!requireNamespace("glue", quietly = TRUE)) {
  install.packages("glue")
  }
if (!requireNamespace("meta", quietly = TRUE)) {
  install.packages("meta")
  }
if (!requireNamespace("metafor", quietly = TRUE)) {
  install.packages("metafor")
  }
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
  }
if (!requireNamespace("poibin", quietly = TRUE)) {
  install.packages("poibin")
  }
if (!requireNamespace("puniform", quietly = TRUE)) {
  install.packages("puniform")
  }
if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
  }
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
  }
if (!requireNamespace("rlang", quietly = TRUE)) {
  install.packages("rlang")
  }
if (!requireNamespace("robumeta", quietly = TRUE)) {
  install.packages("robumeta")
  }
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
  }
if (!requireNamespace("shinyBS", quietly = TRUE)) {
  install.packages("shinyBS")
  }
if (!requireNamespace("shinycssloaders", quietly = TRUE)) {
  install.packages("shinycssloaders")
  }
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
  }
if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
  }
if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
  }
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
  }
if (!requireNamespace("tibble", quietly = TRUE)) {
  install.packages("tibble")
  }
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
  }
if (!requireNamespace("waffle", quietly = TRUE)) {
  install.packages("waffle")
  }
if (!requireNamespace("weightr", quietly = TRUE)) {
  install.packages("weightr")
  }
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
  }
if (!requireNamespace("zcurve", quietly = TRUE)) {
  install.packages("zcurve")
  }

  # Ensure component files can be found
   f <- '.'

  if(!(file.exists('server.R'))) {
    message('server.R not found in current working directory. Trying to automatically ',
            'identify location of this script - in case of errors, please set the ',
            'working directory.')


    # With thanks to https://stackoverflow.com/a/55322344/10581449
    getCurrentFileLocation <-  function()
    {
      this_file <- commandArgs() %>%
        tibble::enframe(name = NULL) %>%
        tidyr::separate(col=value, into=c('key', 'value'), sep='=', fill='right') %>%
        dplyr::filter(key == '--file') %>%
        dplyr::pull(value)
      if (length(this_file)==0)
      {
        this_file <- rstudioapi::getSourceEditorContext()$path
      }
      return(dirname(this_file))
    }

    f <- getCurrentFileLocation()
  }

  # Source and set elements of app
  library(shiny)
  source(file.path(f, 'helpers.R'))
  source(file.path(f, 'models.R'))
  source(file.path(f, 'labels_and_options.R'))
  source(file.path(f, 'dmetar_contributions.R'))
  server <- source(file.path(f, 'server.R')) %>% purrr::pluck('value')
  ui <- source(file.path(f, 'ui.R'))  %>% purrr::pluck('value')
  metaUI_eff_size_type_label <- 'r (adjusted for attenutation)'
  metaUI__df <- readRDS(file.path(f, 'dataset.rds'))
