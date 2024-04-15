## This long-running code was run on AWS EC2 instance(s) with 8 cores
## using https://aws.amazon.com/marketplace/pp/prodview-o3ygr2bixhiru
## which makes deployment very straight-forward

install.packages("metafor")
install.packages("tidyverse")
install.packages("qs")
install.packages("clubSandwich")
install.packages("job")
install.packages("progressr")
install.packages("furrr")


library(metafor)
library(tidyverse)
library(progressr)
library(furrr)


dataset <- qs::qread("dataset.qs")

datasets_pub <- dataset %>%
  filter(pub_status == "Published") %>%
  split(.$domain)


apply_selmodel <- function(data, boot_method = c("within", "cluster")) {
  if (boot_method == "within") {
    data <- data %>% group_by(articlestudy) %>% sample_n(1) %>% ungroup() %>%   # Sample one effect size per study
      sample_n(nrow(.), replace = TRUE) # Create bootstrap sample
  } else if (boot_method == "cluster") {
    data <- data %>% nest(.by = articlestudy, .key = "data") %>% sample_n(nrow(.), replace = TRUE) %>% unnest(data)
  } else {
    stop("Invalid boot_method")
  }

  run_selmodel <- function (data) {
    # Fit the model
    m.rma <- rma(yi = r_rep,
                 sei = se,
                 data = data,
                 slab = articlestudy,
                 method = "REML",
                 test = "knha")

    selmodel(m.rma,
             type = "stepfun",
             skiphes = TRUE, skiphet = TRUE, # turn off SE and het-test calculations to accelerate,
             steps = c(0.025, .05, .95, .975)) %>%  # Selection for both positive and negative sig values
      summary()

  }

  run_selmodel <- purrr::possibly(run_selmodel, otherwise = NA)

  psm_mod <- run_selmodel(data)

  if (is.na(psm_mod[1])) {
    return(tibble())
  } else {

    # Create dataframe with the required elements
    tibble(
      lrt = psm_mod$LRT,
      p_table = list(psm_mod$ptable %>% mutate(prob = psm_mod$delta))
    )
  }
}

# Bootstrap sampling and applying 3PSM
set.seed(123) # for reproducibility
n_bootstraps <- 5000

handlers("cli")

plan(multisession, workers = 8)

with_progress({
  p <- progressor(steps = n_bootstraps * 3, label = "Bootstrapping")
  sel_models_within <- map_dfr(datasets_pub, \(domain_data) {
    future_map_dfr(seq_len(n_bootstraps) %>% set_names(), \(i) {
      p()
      apply_selmodel(domain_data, boot_method = "within")
    }, .id = "run", .options = furrr_options(seed = TRUE))}, .id = "domain")
})

qs::qsave(sel_models_within, "data/sel_models_within.qs")


set.seed(123) # for reproducibility

with_progress({
  p <- progressor(steps = n_bootstraps * 3, label = "Bootstrapping")
  sel_models_cluster <- map_dfr(datasets_pub, \(domain_data) {
    future_map_dfr(seq_len(n_bootstraps) %>% set_names(), \(i) {
      p()
      apply_selmodel(domain_data, boot_method = "cluster")
    }, .id = "run", .options = furrr_options(seed = TRUE))}, .id = "domain")
})

qs::qsave(sel_models_cluster, "data/sel_models_cluster.qs")
