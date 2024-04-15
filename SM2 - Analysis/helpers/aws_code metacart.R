## This long-running code was run on AWS EC2 instance(s) with 8 cores
## using https://aws.amazon.com/marketplace/pp/prodview-o3ygr2bixhiru
## which makes deployment very straight-forward

# List of packages
packages <- c("dplyr", "tidyr", "purrr", "vctrs", "mice",
              "glue", "qs", "cli", "furrr", "future", "magrittr", "metacart")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load packages
lapply(packages, library, character.only = TRUE)

source("helpers/metacart_fix.R") # metacart broke with R 4.2.0 due to a change in if-statement evaluation. This fix changes `class ==` to `inherits()`

# Set parameters
n_datasets <- 10
n_trees <- 50

# Aggregated data
aggregated_data_file <- "data/dataset_aggregated_metacart.qs"

dummy_code_df <- function(df, exclude_column, drop_first = TRUE) {
  if (!is.character(exclude_column)) {
    stop("exclude_column must be a character string.")
  }
  
  for (col_name in names(df)) {
    if (col_name != exclude_column && is.factor(df[[col_name]]) && !is.ordered(df[[col_name]])) {
      
      # Get levels and potentially drop the first one
      levels_to_dummy <- levels(df[[col_name]])
      if (drop_first) {
        levels_to_dummy <- levels_to_dummy[-1]
      }
      
      # Create dummy columns for each level
      for (level in levels_to_dummy) {
        dummy_name <- paste0(col_name, "_", level) %>% vctrs::vec_as_names(repair = "universal_quiet")
        df[[dummy_name]] <- df[[col_name]] == level
      }
      
      # Remove the original factor column if not excluded
      df[[col_name]] <- NULL
    }
  }
  
  return(df)
}

# Imputation code derived from mice package (but noise removed to get single best)
# mice copyright Stef van Buuren - thanks

impute_best_polr <- function(dat, var_name) {
  dat <- dummy_code_df(dat, var_name)
  
  collinear <- mice:::find.collinear(dat)
  
  if (length(collinear) > 0) {
    dat <- dat[-which(names(dat) %in% setdiff(collinear, var_name))]
  }
  
  y_missing <- which(is.na(dat[var_name]))
  
  f <- setdiff(colnames(dat), var_name) %>%
    glue::glue_collapse(sep = " + ") %>%
    paste(var_name, "~", .)
  
  complete_dat <- dat %>% drop_na(everything())
  
  ## polr may fail on sparse data. We revert to multinom in such cases.
  fit <- try({
    MASS::polr(as.formula(f),
               data = complete_dat,
    )
    post <- predict(fit, dat[is.na(dat[var_name]), ], type = "probs")
  })
  if (inherits(fit, "try-error")) {
    message("polr falls back to multinom")
    fit <- nnet::multinom(as.formula(f),
                          data = complete_dat,
                          maxit = 100, trace = FALSE,
                          MaxNWts = 1500
    )
    post <- predict(fit, dat[is.na(dat[var_name]), ], type = "probs")
  }
  
  
  if (length(y_missing) == 1) {
    temp <- matrix(post, nrow = 1, ncol = length(post))
    colnames(temp) <- names(post)
    post <- temp
  }
  
  max_col_index <- apply(post, 1, \(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(which(x == max(x, na.rm = TRUE)))
    }
  })
  
  new_y <- colnames(post)[max_col_index]
  
  
  if (length(dat[[var_name]][y_missing]) != length(new_y)) browser()
  
  dat[[var_name]][y_missing] <- new_y
  
  message("Imputed ", sum(!is.na(dat[[var_name]][y_missing])), " values.")
  
  
  dat[[var_name]]
}

impute_best_polyreg <- function(dat, var_name) {
  dat <- dummy_code_df(dat, var_name)
  
  collinear <- mice:::find.collinear(dat)
  
  if (length(collinear) > 0) {
    dat <- dat[-which(names(dat) %in% setdiff(collinear, var_name))]
  }
  
  y_missing <- which(is.na(dat[var_name]))
  
  f <- setdiff(colnames(dat), var_name) %>%
    glue::glue_collapse(sep = " + ") %>%
    paste(var_name, "~", .)
  
  complete_dat <- dat %>% drop_na(everything())
  
  
  fit <- nnet::multinom(as.formula(f),
                        data = complete_dat,
                        maxit = 100, trace = FALSE,
                        MaxNWts = 3000
  )
  
  tryCatch(
    {
      post <- predict(fit, dat[is.na(dat[var_name]), ], type = "probs")
      if (length(y_missing) == 1) {
        temp <- matrix(post, nrow = 1, ncol = length(post))
        colnames(temp) <- names(post)
        post <- temp
      }
      
      max_col_index <- apply(post, 1, \(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(which(x == max(x, na.rm = TRUE)))
        }
      })
      new_y <- colnames(post)[max_col_index]
      
      
      dat[[var_name]][y_missing] <- new_y
    },
    error = \(e) {
      warning("Prediction failed")
      return(dat[[var_name]])
    }
  )
  
  
  message("Imputed ", sum(!is.na(dat[[var_name]][y_missing])), " values.")
  
  dat[[var_name]]
}

# Imputation strategy: identify missing patterns, then impute for each pattern so that always the largest available set of predictors is used
missing_patterns <- function(dataset, target_var) {
  dataset <- dataset[is.na(dataset[[target_var]]), ]
  dataset %>%
    select(-matches(target_var)) %>%
    mutate(missing_vars = pmap_chr(., function(...) {
      vars <- names(c(...))
      missing <- vars[is.na(c(...))]
      if (length(missing) == 0) {
        return(NA_character_)
      } # Return NA if no missing vars
      paste(missing, collapse = "|")
    })) %>%
    mutate(across(-missing_vars, ~ as.numeric(is.na(.)))) %>%
    mutate(m = rowSums(across(-missing_vars), na.rm = TRUE)) %>%
    group_by(missing_vars, m) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(m, -n)
}

impute_sequentially <- function(dataset, target_var, fun) {
  missings <- missing_patterns(dataset, target_var)
  if (any(is.na(missings$missing_vars))) {
    dataset[[target_var]] <- do.call(fun, list(dataset, target_var))
  }
  missings <- missings %>% filter(!is.na(missing_vars))
  if (nrow(missings) == 0) {
    return(dataset[[target_var]])
  }
  for (i in 1:nrow(missings)) {
    dataset[[target_var]] <- do.call(fun, list(dataset %>% select(-matches(missings[i, ]$missing_vars)), target_var))
  }
  dataset[[target_var]]
}

# For "worst case", impute by drawing random values from observed data
impute_with_random_draw <- function(dataframe) {
  for (col in names(dataframe)) {
    missing_indices <- which(is.na(dataframe[[col]]))
    if (length(missing_indices) > 0) {
      non_missing_values <- dataframe[[col]][!is.na(dataframe[[col]])]
      if (length(non_missing_values) > 0) {
        dataframe[[col]][missing_indices] <- sample(non_missing_values, length(missing_indices), replace = TRUE)
      }
    }
  }
  return(dataframe)
}
########
### Impute datasets
########

if (!file.exists("data/cart_imputed_best.qs") || !file.exists("data/cart_imputed_random.qs")) {
  set.seed(2124)
  dataset_aggregated <- qs::qread(aggregated_data_file)
  
  datasets <- list()
  datasets_imp_best <- list()
  datasets_imp_rand <- list()
  
  # specify variables to include in imputation (those in metaCart and relevant other preds)
  mi_vars <- c(
    "art_focus", "pub_status", "design", "setting", "ind_sector", "team_function",
    "n_teams", "stud_sample",
    "tmt", "domain", "sub_dom", "meas_type", "rater", "interdep", "complexity", "longevity", "power_distance",
    "collectivism", "year_merged", "citation_count"
  )
  
  # Resample datasets
  for (i in seq_len(n_datasets)) {
    datasets[[i]] <- dataset_aggregated %>%
      group_by(articlestudy, domain) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      select(articlestudy, domain, r_adj, se, var_adj, all_of(mi_vars)) %>%
      mutate(across(c(where(is.character), -articlestudy), factor))
  }
  
  # Impute  using best prediction
  
  ## Define models
  
  polr_vars <- c("interdep", "complexity", "longevity")
  
  polyreg_vars <- c(
    "art_focus", "pub_status", "design", "setting", "ind_sector", "team_function",
    "stud_sample", "tmt", "domain", "meas_type", "rater"
  )
  
  special_var <- c("sub_dom")
  
  pmm_vars <- c("power_distance", "collectivism")
  
  for (i in seq_along(datasets)) {
    datasets_imp_best[[i]] <- datasets[[i]]
    
    for (var in polr_vars) {
      if (any(is.na(datasets_imp_best[[i]][[var]]))) {
        message("\n\n Imputing ", var)
        datasets_imp_best[[i]][[var]] <- impute_sequentially(datasets_imp_best[[i]] %>% select(-articlestudy), var, impute_best_polr)
      }
    }
    
    for (var in polyreg_vars) {
      if (any(is.na(datasets_imp_best[[i]][[var]]))) {
        message("\n\n Imputing ", var)
        datasets_imp_best[[i]][[var]] <- impute_sequentially(datasets_imp_best[[i]] %>% select(-articlestudy), var, impute_best_polyreg)
      }
    }
    
    # Impute sub-domains by domain, to ensure valid values
    temp_split <- split(datasets_imp_best[[i]], datasets_imp_best[[i]]$domain)
    for (domain in names(temp_split)) {
      temp_split[[domain]]$sub_dom <- impute_sequentially(temp_split[[domain]] %>% select(-articlestudy), "sub_dom", impute_best_polyreg)
    }
    
    datasets_imp_best[[i]] <- bind_rows(temp_split)
    
    
    # For continuous variables, use PMM function in mice
    imp <- mice(datasets_imp_best[[i]], maxit = 0)
    
    methods <- imp$method
    methods[] <- ""
    methods[pmm_vars] <- "pmm"
    preds <- imp$predictorMatrix
    preds[] <- 0
    
    preds[pmm_vars, mi_vars] <- 1
    preds[pmm_vars, "r_adj"] <- 1
    preds[pmm_vars, "se"] <- 1
    preds[pmm_vars, pmm_vars] <- 0 # Always missing together, so this would not help
    
    imp <- mice(datasets_imp_best[[i]],
                m = 1, seed = 2124 + i, predictorMatrix = preds,
                method = methods, donors = 1
    )
    
    datasets_imp_best[[i]] <- datasets_imp_best[[i]] %>%
      select(-all_of(pmm_vars)) %>%
      left_join(imp %>% complete() %>% select(articlestudy, domain, all_of(pmm_vars)))
  }
  
  
  # Impute  using random prediction
  
  for (i in seq_along(datasets)) {
    datasets_imp_rand[[i]] <- datasets[[i]]
    
    
    datasets_imp_rand[[i]] <- datasets_imp_rand[[i]] %>%
      select(-sub_dom) %>%
      impute_with_random_draw() %>%
      left_join(datasets_imp_rand[[i]] %>% select(articlestudy, domain, sub_dom))
    
    # Impute sub-domains by domain, to ensure valid values
    temp_split <- split(datasets_imp_rand[[i]], datasets_imp_rand[[i]]$domain)
    for (domain in names(temp_split)) {
      temp_split[[domain]] <- impute_with_random_draw(temp_split[[domain]])
    }
    
    datasets_imp_rand[[i]] <- bind_rows(temp_split)
  }
  datasets_imp_best %>% qs::qsave("data/cart_imputed_best.qs")
  datasets_imp_rand %>% qs::qsave("data/cart_imputed_random.qs")
} else {
  datasets_imp_rand <- qs::qread("data/cart_imputed_random.qs")
  datasets_imp_best <- qs::qread("data/cart_imputed_best.qs")
  
  if (!length(datasets_imp_best) == n_datasets || !length(datasets_imp_rand) == n_datasets) {
    warning("Number of loaded datasets does not macth `n_datasets`. Consider re-running imputation.")
  }
}

########
### Estimate trees
########


run_meta_cart <- function(data, reps, seed = NULL, method = c("standard", "ahead")) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  source("helpers/metacart_fix.R") # So that it is available when running in parallel
  
  cli::cli_progress_bar(name = "Fitting trees", total = reps)
  
  if (method[1] == "standard") {
    mods <- list()
    
  for (i in seq_len(reps)) {
    mods[i] <- withCallingHandlers({ # Suppress warnings about no moderator effect
      mod <- try(list(REmrt(r_adj ~ domain + year_merged + complexity + interdep + collectivism + meas_type + rater + design + art_focus + longevity,
                            data,
                            vi = var_adj,
                            c = 0,
                            maxL = 20,
                            xval = 10,
                            lookahead = FALSE
      )), silent = TRUE)
      if (inherits(mod, "try-error")) { # Failed on one dataset (best/job-related) in very few cases due to bug in underlying C code.
        cli::cli_alert_warning("Failed to fit model ", i)
        mod <- NULL
      }
      mod
    }, warning=function(w) {
      if (endsWith(conditionMessage(w), "no moderator effect was detected"))
        invokeRestart("muffleWarning")
    })
    cli::cli_progress_update()
  }
    cli::cli_progress_done()
    return(mods)
  }

  if (method[1] == "ahead") {
    
  mods_ahead <- list()
  
  for (i in seq_len(reps)) {
    mods_ahead[i] <- withCallingHandlers({
      mod <- try(list(REmrt(r_adj ~ domain + year_merged + complexity + interdep + collectivism + meas_type + rater + design + art_focus + longevity,
                            data,
                            vi = var_adj,
                            c = 0,
                            maxL = 20,
                            xval = 10,
                            lookahead = TRUE
      )), silent = TRUE)
      if (inherits(mod, "try-error")) { 
        cli::cli_alert_warning("Failed to fit lookahead model ", i)
        mod <- NULL
      }
      mod
    }, warning=function(w) {
      if (endsWith(conditionMessage(w), "no moderator effect was detected"))
        invokeRestart("muffleWarning")
    })
    cli::cli_progress_update()
    
  }
  cli::cli_progress_done()
  return(mods_ahead)
  } else {
    stop("Invalid method specified")
  }
}

  datasets_imp_best_list <- list()
  datasets_imp_rand_list <- list()
  
  datasets_imp_best <- datasets_imp_best %>% set_names(as.character(seq_along(.)))
  datasets_imp_rand <- datasets_imp_rand %>% set_names(as.character(seq_along(.)))
  
  for (i in seq_along(datasets_imp_best)) {
    datasets_imp_best_list[[i]] <- split(datasets_imp_best[[i]], datasets_imp_best[[i]]$domain)
    datasets_imp_best_list[[i]][["Overall"]] <- datasets_imp_best[[i]]
  }
  
  datasets_imp_best_list <- list_transpose(datasets_imp_best_list)
  
  for (i in seq_along(datasets_imp_best)) {
    datasets_imp_rand_list[[i]] <- split(datasets_imp_rand[[i]], datasets_imp_rand[[i]]$domain)
    datasets_imp_rand_list[[i]][["Overall"]] <- datasets_imp_rand[[i]]
  }
  
  datasets_imp_rand_list <- list_transpose(datasets_imp_rand_list)
  
  datasets_imp <- list(best = datasets_imp_best_list, rand = datasets_imp_rand_list)
  
  datasets_imp <- datasets_imp %>%
    list_flatten() %>%
    list_flatten()
  
  datasets_imp <- datasets_imp[order(sapply(datasets_imp, nrow))]
  
  cores <- future::availableCores()
  
  plan(multisession, workers = cores)
  
if (!(file.exists("data/metacart_trees_standard.qs"))) {


  trees_standard <- future_imap(datasets_imp, 
                       ~{
                         # Extract the index from names to use as seed offset
                         index <- which(names(datasets_imp) == .y)
                         # Calculate a unique seed for each run
                         specific_seed <- 12345 + index
                         
                         # Run the function with the specific seed
                         result <- run_meta_cart(.x, reps = n_trees, seed = specific_seed, method = "standard")
                         
                         # Save the result using the name of the dataset
                         qsave(result, paste0(.y, "_standard.qs"))
                         
                         # Return result if needed
                         result
                       }, 
                       .progress = TRUE, .options = furrr_options(seed = TRUE))
  qs::qsave(trees_standard, "data/metacart_trees_standard.qs")
} else {
  trees <- qs::qread("data/metacart_trees_standard.qs")
}

if (!(file.exists("data/metacart_trees_ahead.qs"))) {
  trees_ahead <- future_imap(datasets_imp, 
                             ~{
                               # Extract the index from names to use as seed offset
                               index <- which(names(datasets_imp) == .y)
                               # Calculate a unique seed for each run
                               specific_seed <- 12345 + index
                               
                               # Run the function with the specific seed
                               result <- run_meta_cart(.x, reps = n_trees, seed = specific_seed, method = "ahead")
                               
                               # Save the result using the name of the dataset
                               qsave(result, paste0(.y, "_ahead.qs"))
                               
                               # Return result if needed
                               result
                             }, 
                             .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  qs::qsave(trees_ahead, "data/metacart_trees_ahead.qs")
} else {
  trees_ahead <- qs::qread("data/metacart_trees_ahead.qs")
}
