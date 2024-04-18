    function(input, output) {

    showModal(modalDialog(
        title = welcome_title,
       welcome_text,
       easyClose = TRUE
      ))

  # Set app states
    state_values <- reactiveValues(
      to_upload = FALSE,
      ever_analyzed = FALSE
    )

  # Create slider to filter by z-scores
  z_sig_dig <- 2

  insertUI(
    selector = "#z_score_filter",
    where = "beforeEnd",
    ui = sliderInput("outliers_z_scores", "Exclude based on z-scores",
      min = signif_floor(min(metaUI__df$metaUI__es_z), z_sig_dig),
      max = signif_ceiling(max(metaUI__df$metaUI__es_z), z_sig_dig), value = c(
        signif_floor(min(metaUI__df$metaUI__es_z), z_sig_dig),
        signif_ceiling(max(metaUI__df$metaUI__es_z, na.rm = TRUE))
      ), sep = ""
    )
  )

  filters <- list(list(col = 'metaUI__filter_Domain', id = 'metaUI__filter_Domain', type = 'selection'),
list(col = 'metaUI__filter_Sub-Domain', id = 'metaUI__filter_Sub-Domain', type = 'selection'),
list(col = 'metaUI__filter_Year', id = 'metaUI__filter_Year', type = 'numeric'),
list(col = 'metaUI__filter_Country', id = 'metaUI__filter_Country', type = 'selection'),
list(col = 'metaUI__filter_Publication type', id = 'metaUI__filter_Publication_type', type = 'selection'),
list(col = 'metaUI__filter_Article focus', id = 'metaUI__filter_Article_focus', type = 'selection'),
list(col = 'metaUI__filter_Top management team', id = 'metaUI__filter_Top_management_team', type = 'selection'),
list(col = 'metaUI__filter_Student sample', id = 'metaUI__filter_Student_sample', type = 'selection'),
list(col = 'metaUI__filter_Industry sector', id = 'metaUI__filter_Industry_sector', type = 'selection'),
list(col = 'metaUI__filter_Team function', id = 'metaUI__filter_Team_function', type = 'selection'),
list(col = 'metaUI__filter_Diversity concept', id = 'metaUI__filter_Diversity_concept', type = 'selection'),
list(col = 'metaUI__filter_Performance measure', id = 'metaUI__filter_Performance_measure', type = 'selection'),
list(col = 'metaUI__filter_Complexity', id = 'metaUI__filter_Complexity', type = 'selection'),
list(col = 'metaUI__filter_Interdependence', id = 'metaUI__filter_Interdependence', type = 'selection'),
list(col = 'metaUI__filter_Longevity', id = 'metaUI__filter_Longevity', type = 'selection'),
list(col = 'metaUI__filter_Power distance', id = 'metaUI__filter_Power_distance', type = 'numeric'),
list(col = 'metaUI__filter_Collectivism', id = 'metaUI__filter_Collectivism', type = 'numeric'))


  file_input <- reactive({
    if (state_values$to_upload == TRUE) {
      return(input$uploadData)
    } else {
      return(NULL)
    }
  })

  # Apply reactive filtering of dataset when clicking on the button
  df_filtered <- eventReactive(input$go, {
    df_reactive()
  })

  # Reset filters
       observeEvent(input$resetFilters, {
        shinyjs::reset("filters")
      })


  df_reactive <- reactive({
    if (!is.null(file_input())) {
      df <- readxl::read_xlsx(input$uploadData$datapath, "dataset")
      filter_values <- readxl::read_xlsx(input$uploadData$datapath, "filters") %>% split(.$id)

      for (i in filters) {
        if (i$type == "numeric") {
          updateSliderInput(inputId = i$id, value = c(as.numeric(filter_values[[i$id]]$selection[1]), as.numeric(filter_values[[i$id]]$selection[2])))
        } else {
          updateCheckboxGroupInput(inputId = i$id, selected = filter_values[[i$id]]$selection)
        }
      }


          updateSliderInput(inputId = "outliers_z_scores", value = c(as.numeric(filter_values[["outliers_z_scores"]]$selection[1]), as.numeric(filter_values[["outliers_z_scores"]]$selection[2])))

      state_values$to_upload <- FALSE
    } else {
      df <- metaUI__df
    }



    # Filter by specified metadata filters
    df <- df[df[['metaUI__filter_Domain']] %in% input[['metaUI__filter_Domain']], ]
df <- df[df[['metaUI__filter_Sub-Domain']] %in% input[['metaUI__filter_Sub-Domain']], ]
df <- df[(df[['metaUI__filter_Year']] >= input[['metaUI__filter_Year']][1] & df[['metaUI__filter_Year']] <= input[['metaUI__filter_Year']][2]) |
(is.na(df[['metaUI__filter_Year']]) & isTRUE(input[['metaUI__filter_Year_include_NA']])), ]
df <- df[df[['metaUI__filter_Country']] %in% input[['metaUI__filter_Country']], ]
df <- df[df[['metaUI__filter_Publication type']] %in% input[['metaUI__filter_Publication_type']], ]
df <- df[df[['metaUI__filter_Article focus']] %in% input[['metaUI__filter_Article_focus']], ]
df <- df[df[['metaUI__filter_Top management team']] %in% input[['metaUI__filter_Top_management_team']], ]
df <- df[df[['metaUI__filter_Student sample']] %in% input[['metaUI__filter_Student_sample']], ]
df <- df[df[['metaUI__filter_Industry sector']] %in% input[['metaUI__filter_Industry_sector']], ]
df <- df[df[['metaUI__filter_Team function']] %in% input[['metaUI__filter_Team_function']], ]
df <- df[df[['metaUI__filter_Diversity concept']] %in% input[['metaUI__filter_Diversity_concept']], ]
df <- df[df[['metaUI__filter_Performance measure']] %in% input[['metaUI__filter_Performance_measure']], ]
df <- df[df[['metaUI__filter_Complexity']] %in% input[['metaUI__filter_Complexity']], ]
df <- df[df[['metaUI__filter_Interdependence']] %in% input[['metaUI__filter_Interdependence']], ]
df <- df[df[['metaUI__filter_Longevity']] %in% input[['metaUI__filter_Longevity']], ]
df <- df[(df[['metaUI__filter_Power distance']] >= input[['metaUI__filter_Power_distance']][1] & df[['metaUI__filter_Power distance']] <= input[['metaUI__filter_Power_distance']][2]) |
(is.na(df[['metaUI__filter_Power distance']]) & isTRUE(input[['metaUI__filter_Power_distance_include_NA']])), ]
df <- df[(df[['metaUI__filter_Collectivism']] >= input[['metaUI__filter_Collectivism']][1] & df[['metaUI__filter_Collectivism']] <= input[['metaUI__filter_Collectivism']][2]) |
(is.na(df[['metaUI__filter_Collectivism']]) & isTRUE(input[['metaUI__filter_Collectivism_include_NA']])), ]



    # Filter by zscore
    df <- df[df$metaUI__es_z >= input$outliers_z_scores[1] & df$metaUI__es_z <= input$outliers_z_scores[2], ]
    df
  })

  # Data for forest plot and table ------------------------------------------

  estimatesreactive <- reactive({
    df <- df_filtered()

    # Check if there are any studies left after filtering
    if (nrow(df) == 0) {
      showModal(modalDialog(
        title = "No effect sizes selected!",
        "Your selection criteria do not match any effect sizes. Please adjust them and try again."
      ))

      return(NULL)
    }

   state_values$ever_analyzed <- TRUE

    if (aggregation_method[1] == "aggregate") {
      # TK - do we want this, or actually just average, despite the problems with that?
      # Aggregate dependent effects based on https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/
      # note that this requires an assumption regarding the degree of correlation
      agg_effects <- function(yi, vi, r = correlation_dependent) {
        corr_mat <- r + diag(1 - r, nrow = length(vi))
        sd_mat <- tcrossprod(sqrt(vi))
        V_inv_mat <- chol2inv(chol(sd_mat * corr_mat))
        V <- 1 / sum(V_inv_mat)
        data.frame(es = V * sum(yi * V_inv_mat), var = V)
      }

      df_agg <-
        df %>%
        dplyr::group_by(metaUI__study_id) %>%
        dplyr::summarise(
          es = list(agg_effects(yi = metaUI__effect_size, vi = metaUI__variance)),
          metaUI__N = max(metaUI__N),
          metaUI__es_type = dplyr::first(metaUI__es_type), # Could also aggregate filters - if length(unique(FILTER)) == 1
          .groups = "drop"
        ) %>%
        tidyr::unnest(cols = "es") %>%
        dplyr::rename(metaUI__effect_size = es, metaUI__variance = var) %>%
        dplyr::mutate(metaUI__se = sqrt(metaUI__variance))
    } else if (aggregation_method[1] == "first") {
      df_agg <-
        df %>%
        dplyr::group_by(metaUI__study_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
    } else {
      stop("Aggregation method not recognized")
    }

    # Run all specified models
    models <- purrr::pmap(models_to_run, \(...){
      mod_spec <- tibble::tibble(...)
      if (mod_spec$aggregated == TRUE) {
        df <- df_agg
      }
      mod <- try(eval(parse(text = mod_spec$code)))
      if ("try-error" %in% class(mod)) {{
        warning("Model ", mod_spec$name, " could not be estimated. Error was ", mod)
        mod <- NULL
        mod_res <- tibble::tibble(
            Model = mod_spec$name,
            es = NA_real_,
            LCL = NA_real_,
            UCL = NA_real_,
            k = NA_real_
          )
      }} else {{
        mod_res <- tibble::tibble(
          Model = mod_spec$name,
          es = eval(parse(text = mod_spec$es)) %>% as.numeric(),
          LCL = eval(parse(text = mod_spec$LCL)) %>% as.numeric(),
          UCL = eval(parse(text = mod_spec$UCL)) %>% as.numeric(),
          k = eval(parse(text = mod_spec$k)) %>% as.numeric()
        )
      }}
      list(mod = mod, mod_res = mod_res)
    }) %>% purrr::transpose()

    # Generate table

    estimates_explo_agg <- models$mod_res %>% dplyr::bind_rows() %>%
      dplyr::mutate(Model = factor(Model, levels = Model))

    print(estimates_explo_agg)

    list(df_agg = df_agg, table = estimates_explo_agg)
  })

  estimatesfiltered <- eventReactive(input$go, {
    estimatesreactive()$table
  })

  # Aggregated values meta-analysis ----------------------------------------------


  df_agg <- reactive({
    estimatesreactive()$df_agg
  })

  output$sample <- renderTable({
    df <- df_filtered()

    overview <- tibble::tribble(
      ~Sources, ~Studies,
      ~Effects, ~`Sample size`,
      length(unique(df$metaUI__article_label)), length(unique(df$metaUI__study_id)),
      length(df$metaUI__study_id), sum(aggregate(metaUI__N ~ metaUI__study_id, data = df, FUN = "min")$metaUI__N) %>% round()
    )

    if (overview$Sources == 0) {
      overview$Sources <- "not specified"
    }

    message(paste("The current dataset contains", overview$Sources, "sources,", overview$Studies,
      "independent studies and", overview$Effects, "effects.",
      sep = " "
    ))

    if (overview$Sources == "not specified") {
      overview$Sources <- NULL
    }

    overview
  })

  # MODEL COMPARISON -----------------------------------------------------
  output$model_comparison <- renderPlot({
    estimates_explo_agg <- estimatesfiltered()

    ggplot2::ggplot() +
      ggplot2::geom_point(data = estimates_explo_agg, ggplot2::aes(x = es, y = Model), stat = "identity") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::xlab(metaUI_eff_size_type_label) +
      ggplot2::geom_errorbar(data = estimates_explo_agg, ggplot2::aes(y = Model, xmin = LCL, xmax = UCL), stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_discrete(limits = rev(levels(estimates_explo_agg$Model))) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  })



  # MODEL COMPARISON TABLE -------------------------------------------------------------------
  output$effectestimate <- renderTable(
    {
      estimatesfiltered()  %>%
        dplyr::mutate(k = as.integer(k)) # Remove decimal points from k
    },
    digits = 2
  )



  # Sample overview per filter/moderator -----------------------------------------------------------


            output$`summary_metaUI__filter_Domain_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Domain']], 'Domain')
})

output$`summary_metaUI__filter_Domain_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Domain']], 'Domain')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Domain']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Sub-Domain_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Sub-Domain']], 'Sub-Domain')
})

output$`summary_metaUI__filter_Sub-Domain_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Sub-Domain']], 'Sub-Domain')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Sub-Domain']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
  output$`summary_metaUI__filter_Year_table` <- renderTable({ # { escapes the glue syntax
  df <- df_filtered()
  summarise_numeric(df[['metaUI__filter_Year']], 'Year')
})

output$`summary_metaUI__filter_Year_plot` <- renderPlot({
  df <- df_filtered()
  ggplot2::ggplot(df, ggplot2::aes(x = `metaUI__filter_Year`)) +
    ggplot2::geom_density() +
    ggplot2::geom_rug(alpha = .1) +
    ggplot2::theme_light() +
    ggplot2::xlab('Year')
})
        output$`summary_metaUI__filter_Country_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Country']], 'Country')
})

output$`summary_metaUI__filter_Country_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Country']], 'Country')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Country']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Publication_type_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Publication type']], 'Publication type')
})

output$`summary_metaUI__filter_Publication_type_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Publication type']], 'Publication type')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Publication type']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Article_focus_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Article focus']], 'Article focus')
})

output$`summary_metaUI__filter_Article_focus_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Article focus']], 'Article focus')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Article focus']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Top_management_team_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Top management team']], 'Top management team')
})

output$`summary_metaUI__filter_Top_management_team_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Top management team']], 'Top management team')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Top management team']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Student_sample_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Student sample']], 'Student sample')
})

output$`summary_metaUI__filter_Student_sample_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Student sample']], 'Student sample')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Student sample']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Industry_sector_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Industry sector']], 'Industry sector')
})

output$`summary_metaUI__filter_Industry_sector_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Industry sector']], 'Industry sector')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Industry sector']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Team_function_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Team function']], 'Team function')
})

output$`summary_metaUI__filter_Team_function_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Team function']], 'Team function')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Team function']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Diversity_concept_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Diversity concept']], 'Diversity concept')
})

output$`summary_metaUI__filter_Diversity_concept_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Diversity concept']], 'Diversity concept')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Diversity concept']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Performance_measure_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Performance measure']], 'Performance measure')
})

output$`summary_metaUI__filter_Performance_measure_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Performance measure']], 'Performance measure')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Performance measure']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Complexity_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Complexity']], 'Complexity')
})

output$`summary_metaUI__filter_Complexity_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Complexity']], 'Complexity')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Complexity']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Interdependence_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Interdependence']], 'Interdependence')
})

output$`summary_metaUI__filter_Interdependence_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Interdependence']], 'Interdependence')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Interdependence']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
        output$`summary_metaUI__filter_Longevity_table` <- renderTable({
  df <- df_filtered()
  summarise_categorical(df[['metaUI__filter_Longevity']], 'Longevity')
})

output$`summary_metaUI__filter_Longevity_plot` <- renderPlot({
  df <- df_filtered()

  counts <- summarise_categorical(df[['metaUI__filter_Longevity']], 'Longevity')

  waffle_counts <- counts$Count %>%
    setNames(counts[['Longevity']])

  waffle_cols <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3', '#E41A1C', '#377EB8', '#4DAF4A')[1:length(waffle_counts)]

  waffle_counts %>%
    waffle::waffle(rows = ceiling(sqrt(sum(.) / 2)), size = max(2, 2 / (sum(.) / 100)),
    # RColorBrewer Set2 extended to allow for up to 10 + Other categories
    colors = waffle_cols)
})
  output$`summary_metaUI__filter_Power_distance_table` <- renderTable({ # { escapes the glue syntax
  df <- df_filtered()
  summarise_numeric(df[['metaUI__filter_Power distance']], 'Power_distance')
})

output$`summary_metaUI__filter_Power_distance_plot` <- renderPlot({
  df <- df_filtered()
  ggplot2::ggplot(df, ggplot2::aes(x = `metaUI__filter_Power distance`)) +
    ggplot2::geom_density() +
    ggplot2::geom_rug(alpha = .1) +
    ggplot2::theme_light() +
    ggplot2::xlab('Power distance')
})
  output$`summary_metaUI__filter_Collectivism_table` <- renderTable({ # { escapes the glue syntax
  df <- df_filtered()
  summarise_numeric(df[['metaUI__filter_Collectivism']], 'Collectivism')
})

output$`summary_metaUI__filter_Collectivism_plot` <- renderPlot({
  df <- df_filtered()
  ggplot2::ggplot(df, ggplot2::aes(x = `metaUI__filter_Collectivism`)) +
    ggplot2::geom_density() +
    ggplot2::geom_rug(alpha = .1) +
    ggplot2::theme_light() +
    ggplot2::xlab('Collectivism')
})



  # Sample table -----------------------------------------------------------

  output$sample_table <- DT::renderDataTable({
    df <- df_filtered()

    out <- df %>%
      dplyr::select(dplyr::any_of("metaUI__article_label"),
        Study =
          "metaUI__study_id", N = "metaUI__N",
        "Effect size" = "metaUI__effect_size", p = "metaUI__pvalue",
        dplyr::starts_with("metaUI__filter_"), dplyr::any_of("metaUI__url")
      ) %>%
      dplyr::rename_with(~ stringr::str_replace(.x, "metaUI__filter_", "") %>%
        stringr::str_replace("metaUI__article_label", "Source") %>%
        stringr::str_replace("metaUI__url", "URL")) %>%
      dplyr::mutate(p = fmt_p(p, include_equal = FALSE)) %>%
      dplyr::arrange(.data$Study)

    if ("Source" %in% names(out)) {
      out <- out %>% dplyr::mutate(Study = stringr::str_remove(Study, Source) %>%
        stringr::str_remove("^[[:punct:] ]+"))
    }

    if ("URL" %in% names(out)) {
      out <- out %>% dplyr::mutate(URL = glue::glue("<a href={URL}>{URL}</a>"))
    }

    out %>% DT::datatable(
      rownames = FALSE,
      caption = tags$caption(
        style = "caption-side: bottom; text-align: left; margin: 8px 0;",
        glue::glue("The effect size is given as {metaUI_eff_size_type_label}")
      )
    )
  })



  # Moderators  -----------------------------------------------------------


  output$moderation_plot <- plotly::renderPlotly({
    df <- df_filtered()
    mod <- df[[input$moderator]]
    df$mod <- df[[input$moderator]]

    if (is.numeric(mod)) {
      p <- ggplot2::ggplot(data = df, ggplot2::aes(y = metaUI__effect_size, x = mod)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::xlab(input$moderator %>% stringr::str_remove("metaUI__filter_")) +
        ggplot2::ylab(metaUI_eff_size_type_label) +
        ggplot2::geom_smooth(data = df, ggplot2::aes(y = metaUI__effect_size, x = mod, color = NULL), formula = y ~ x, method = "lm") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
    } else {
      p <- ggplot2::ggplot(data = df, ggplot2::aes(y = metaUI__effect_size, x = forcats::fct_rev(mod))) +
        ggplot2::geom_violin(fill = NA) +
        ggplot2::theme_bw() +
        ggplot2::geom_jitter(width = .1, alpha = .25) +
        ggplot2::xlab(input$moderator %>% stringr::str_remove("metaUI__filter_")) +
        ggplot2::ylab(metaUI_eff_size_type_label) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::coord_flip()
    }
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })


  moderator_model <- reactive({
    df <- df_filtered()

    if (is.numeric(df[[input$moderator]])) {

      model <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_size,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}`")),
        method = "ML",
        sparse = TRUE
      )
     moderation_text <-     HTML(glue::glue(
      '<br><br>The {ifelse(is.numeric(df[[input$moderator]]), "linear ", "")}relationship between  <b> {input$moderator  %>% stringr::str_remove("metaUI__filter_")} </b>',
      'and the observed effect sizes <b>is {ifelse(model[["QMp"]] < .05, "", "not ")}significant </b> at the 5% level. ',
     ' Test of moderators: <i>F</i>({model[["QMdf"]][1]}, {model[["QMdf"]][2]}) = {round(model[["QM"]], digits = 2)}, ',
      '<i>p</i> {fmt_p(model[["QMp"]])}.'
    ))

    } else {
     model_sig <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_size,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}`")),
        method = "ML",
        sparse = TRUE
      )
     model <- metafor::rma.mv(
        yi = metaUI__effect_size,
        V = metaUI__variance,
        random = ~ 1 | metaUI__study_id/metaUI__effect_size,
        tdist = TRUE,
        data = df,
        mods = as.formula(glue::glue("~`{input$moderator}` - 1")),
        method = "ML",
        sparse = TRUE
      )

     moderation_text <-     HTML(glue::glue(
      '<br><br>The {ifelse(is.numeric(df[[input$moderator]]), "linear ", "")}relationship between  <b> {input$moderator  %>% stringr::str_remove("metaUI__filter_")} </b>',
      'and the observed effect sizes <b>is {ifelse(model_sig[["QMp"]] < .05, "", "not ")} significant</b> at the 5% level. ',
     ' Test of moderators: <i>F</i>({model_sig[["QMdf"]][1]}, {model_sig[["QMdf"]][2]}) = {round(model_sig[["QM"]], digits = 2)}, ',
      '<i>p</i> {fmt_p(model_sig[["QMp"]])}.'
    ))
    }
    model$moderation_text <- moderation_text
  model
  })


  output$moderation_table <- DT::renderDataTable(escape = FALSE, {
    model <- moderator_model()
    df <- df_filtered()
    if (is.numeric(df[[input$moderator]])) {
      modtable <- psych::describe(df[input$moderator] %>% as.data.frame(), fast = TRUE) # [c(2:5, 8, 9)]
      modtable[, 2:6] <- round_(as.data.frame(modtable)[, 2:6], digits = 2)
      modtable <- modtable %>% dplyr::rename(k = n)
      modtable$Intercept <- round_(model$b[1], digits = 2)
      modtable$`&beta;` <- round_(model$b[2], digits = 2)
      modtable$vars <- NULL
      modtable$range <- NULL
      modtable$se <- NULL

      modtable <- modtable %>% dplyr::mutate(Moderator = input$moderator %>% stringr::str_remove("metaUI__filter_"),
                                            dplyr::across(dplyr::everything(), as.character))  %>%
                               dplyr::select("Moderator", Intercept, "&beta;", "k", dplyr::everything()) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Statistic", values_to = "Value")

    } else {
      modtable <- data.frame(
        "Moderator Levels" = rownames(model$b) %>% stringr::str_remove(input$moderator) %>%
        stringr::str_remove_all("`"), #Needed to support spaces in moderator names
        "Effect size" = round(model$b, digits = 3),
        "95% CI" = fmt_ci(model$ci.lb, model$ci.ub, digits = 3), check.names = FALSE
      )

      modtable <- df %>% dplyr::count(!!rlang::sym(input$moderator)) %>% rename(k = n) %>%
      left_join(modtable, ., by = c("Moderator Levels" = input$moderator))

    }

    modtable %>% DT::datatable(options = list(dom = "t"), escape = FALSE)
  })

  output$moderation_text <- shiny::renderText({
    model <- moderator_model()
    model$moderation_text
  })



  # Heterogeneity -----------------------------------------------------------

  output$heterogeneity <- renderTable({
    df <- df_filtered()

    metapp_total <- metafor::rma.mv(
      yi = metaUI__effect_size,
      V = metaUI__variance,
      random = ~ 1 | metaUI__study_id/metaUI__effect_size,
      tdist = TRUE, # knapp-hartung adjustment
      data = df,
      method = "ML", # REML failed to converge in tests
      sparse = TRUE
    )

    het <- data.frame(
      "Sigma2_Level1" = metapp_total$sigma2[1],
      "Sigma2_Level2" = metapp_total$sigma2[2],
      "Tau" = metapp_total$tau2,
      "Q" = round(metapp_total$QE, digits = 2),
      "Q_p" = fmt_p(metapp_total$QEp, include_equal = FALSE)
      )

    print(het)
  })

  # FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
  output$foreststudies <- renderPlot({
    tryCatch({
      df <- df_filtered()
      #TK: consider different forest plot package that is easier to handle
      if(nrow(df) > 250) {
        stop("Forest plots can only be displayed here with 250 effect sizes or fewer. Please filter the dataset further before proceeding.")
      }

      rve <- robumeta::robu(metaUI__effect_size ~ 1, data = df, studynum = metaUI__study_id, var.eff.size = metaUI__variance, small = FALSE)

      robumeta::forest.robu(rve, es.lab = "metaUI__es_label", study.lab = "metaUI__study_id", "Effect size" = metaUI__effect_size)
    }, error = function(e) {
      # This will stop the execution and display the error message in the plot output
      plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
      text(0.5, 0.5, paste(e$message), cex = 1.2, col = "red")
    })
  }, height = function() {
    # Safeguard in case df_filtered() is NULL or has an unexpected error
    df <- tryCatch({
      df_filtered()
    }, error = function(e) {
      return(NULL)
    })
    if(is.null(df) || nrow(df) > 250) {
      return(200)  # Default height if df is NULL
    } else {
      return(400 + 25 * nrow(df_filtered()))
    }
  }, width = 900)

  # FUNNEL PLOT -------------------------------------------------------------

  meta_agg <- reactive({
    df_agg <- df_agg()

    meta::metagen(
      TE = metaUI__effect_size,
      seTE = metaUI__se,
      data = df_agg,
      studlab = df_agg$metaUI__study_id,
      comb.fixed = FALSE,
      comb.random = TRUE,
      method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
      hakn = TRUE,
      prediction = TRUE,
      sm = df_agg$metaUI__es_type[1]
    )
  })

  output$funnel <- renderPlot({
    meta_agg <- meta_agg()

    metafor::funnel(meta_agg, xlab = metaUI_eff_size_type_label, studlab = FALSE, contour = .95, col.contour = "light grey")
  })

  output$eggers <- DT::renderDataTable({
    meta_agg <- meta_agg()

    eggers <- meta::metabias(meta_agg, k.min = 3, method.bias = "Egger")
    eggers_table <- data.frame(
      "Intercept" = eggers$estimate, "Tau<sup>2</sup>" = eggers$tau,
      "t" = eggers$statistic,
      "p" = ifelse(round(eggers$p.value, 3) == 0, "< .001", round(eggers$p.value, 3)),
      check.names = FALSE
    )
    eggers_table[1, ] %>% DT::datatable(options = list(dom = "t"), escape = FALSE) %>%
     DT::formatRound(columns = 1:3, digits=3)

  })


  # PCURVE ------------------------------------------------------------------

  output$pcurve <- renderPlot({
    # Plot is created as side-effect in pcurve function - so needs to be recalculated here
    df <- df_filtered()

    pp_first <- df %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    pcurve_estimates1 <- try(pcurve(pp_first, effect.estimation = FALSE, N = pp_first$n, dmin = 0, dmax = 1), silent = FALSE)

    pcurve_estimates1 <- ifelse(substr(pcurve_estimates1, 1, 5) == "Error", 0, pcurve_estimates1)

    pcurve_estimates1
  })

  # ZCURVE ------------------------------------------------------------------


  output$zcurve <- renderPlot({
    df <- df_filtered()

    # Use only first p-value as they need to be statistically independent
    df_first <- df %>%
      dplyr::group_by(metaUI__study_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        "studlab" = metaUI__study_id,
        "TE" = metaUI__effect_size,
        "seTE" = metaUI__se,
        "n" = metaUI__N
      )

    df_first$z <- abs(df_first$TE / df_first$seTE)

    zcurve_estimates1 <- try(zcurve::zcurve(df_first$z, bootstrap = FALSE), silent = TRUE)

    zcurve::plot.zcurve(zcurve_estimates1, annotation = TRUE, main = "")
  })

  # VIOLIN PLOTLY -------------------------------------------------------------
  output$violin <- plotly::renderPlotly({
    df <- df_filtered()

    efm <- mean(df$metaUI__effect_size)
    efsd <- sd(df$metaUI__effect_size)

    # Outliers in boxplot are quartiles + 1.5 * IQR - so cutsoffs calculated here to show points with labels
    qs <- quantile(df$metaUI__effect_size, c(.25, .75))
    bounds <- qs
    bounds[1] <- bounds[1] - 1.5 * diff(range(qs))
    bounds[2] <- bounds[2] + 1.5 * diff(range(qs))

    outliers <- df %>% dplyr::filter(metaUI__effect_size < bounds[1] | metaUI__effect_size > bounds[2])

    violinplot <- ggplot2::ggplot(data = df, ggplot2::aes(x = 1, y = metaUI__effect_size)) +
      ggplot2::xlab("") +
      ggplot2::geom_violin(fill = grDevices::rgb(100 / 255, 180 / 255, 1, .5)) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = metaUI_eff_size_type_label) +
      ggplot2::geom_jitter(data = outliers, shape = 16, position = ggplot2::position_jitter(width = .1, height = 0), mapping = ggplot2::aes(text = metaUI__study_id)) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::geom_boxplot(width = .25, outlier.shape = NA) +
      ggplot2::theme(text = ggplot2::element_text(size = 10))

    ay <- list(
      tickfont = list(size = 11.7),
      titlefont = list(size = 14.6),
      overlaying = "y",
      nticks = 5,
      side = "right",
      title = "Standardized effect size (z-score)"
    )

    plotly_plot <- plotly::ggplotly(violinplot, tooltip = "text") %>%
      plotly::config(modeBarButtons = list(list("toImage")), displaylogo = FALSE) %>%
      plotly::add_lines(
        x = ~1, y = ~ (metaUI__effect_size - efm) / efsd, colors = NULL, yaxis = "y2",
        data = df, showlegend = FALSE, inherit = FALSE
      ) %>%
      plotly::layout(
        yaxis2 = ay,
        margin = list(
          r = 45
        )
      )

    # Hide boxplot outliers so that they are not shown multiple times
    plotly_plot$x$data <- lapply(plotly_plot$x$data, FUN = function(x) {
      if (x$type == "box") {
        x$marker <- list(opacity = 0)
      }
      return(x)
    })

    plotly_plot
  })

  # DOWNLOAD ----------------------------------------------------------------

  data_list <- reactive({
    filter_selections <- tibble::tibble(id = "outliers_z_scores", selection = input[["outliers_z_scores"]])

    for (i in filters) {
      filter_selections <- rbind(filter_selections, tibble::tibble(id = i$id, selection = input[[i$id]]))
    }


    list(
      dataset = df_filtered(),
      summary = as.data.frame(estimatesfiltered()),
      filters = filter_selections
    )
  })


  output$executeDownload <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-", gsub(" ", "_", dataset_name), "-metaUIdata.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data_list(), file)
    }
  )

  observeEvent(input$downloadData, {
    if (state_values$ever_analyzed == TRUE) {
      shinyjs::runjs("$('#executeDownload')[0].click();")
    } else {
      showModal(modalDialog(title = "Download not yet possible", HTML("Click on <i>Analyze data</i> first. As the download will also include model estimates, these need to be created first.")))
    }

}
)

  # UPLOAD ----------------------------------------------------------------

  observeEvent(input$executeUpload, {
    state_values$to_upload <- TRUE

    if (class(try(nrow(input$uploadData))) != "try-error") {
      sheets <- readxl::excel_sheets(input$uploadData$datapath)
      if (!("dataset" %in% sheets && "filters" %in% sheets)) {
      showModal(modalDialog(title = "Invalid file", "The file needs to contain a dataset and a filters sheet. Typically, you should start from a file downloaded from this application."))
      } else {
        shinyjs::runjs("$('#go')[0].click();")
      }
    } else {
      showModal(modalDialog(title = "No file selected", HTML("Make sure to select a file prior to upload")))
    }
  })


}

