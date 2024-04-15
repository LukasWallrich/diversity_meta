## This long-running code was run on AWS EC2 instance(s) with 8 cores
## using https://aws.amazon.com/marketplace/pp/prodview-o3ygr2bixhiru
## which makes deployment very straight-forward

install.packages("metafor")
install.packages("tidyverse")
install.packages("qs")
install.packages("clubSandwich")
install.packages("job")
install.packages("progressr")

library(metafor)
library(tidyverse)
library(clubSandwich)
library(progressr)


dataset <- qs::qread("dataset.qs")

cat_moderators <- c("complexity", "interdep", "longevity", "meas_type", "rater", "design", "art_focus", "criterion")
cont_moderators <- c("year_merged", "collectivism", "power_distance")
expl_mods <- c("tmt", "stud_sample", "ind_sector", "team_function", "country_grp", "domain_and_sub")
rho <- .6

# Bootstrap (pseudo) R2 confidence intervals based on guidance by Wolfgang Viechtbauer
# https://www.metafor-project.org/doku.php/tips:ci_for_r2
# https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169
# Showed that for this, BCa CIs are best

boot_R2 <- function(dat, indices, moderator, other_moderators = NULL) {
  p()

  data <- dat[indices, ]

  # Drop rows where moderator or any of other_moderators are NA
  if (!is.null(other_moderators)) {
    data <- data %>% drop_na(all_of(c(moderator, other_moderators)))
  } else {
    data <- data %>% drop_na(all_of(moderator))
  }

  # Null models for R2
  V <- with(data,
            impute_covariance_matrix(vi = var_adj,
                                     cluster = articlestudy,
                                     r = rho))

  if (!is.null(other_moderators)) {
    fml <- paste("r_adj ~", paste(other_moderators, collapse = " + "))

  } else {
    fml <- "r_adj ~ 1"
  }


  mod_null <- try(suppressWarnings(rma.mv(as.formula(fml),
                                          V = V,
                                          random = ~ 1 | articlestudy / effect_id,
                                          test = "t",
                                          dfs = "contain",
                                          data = data,
                                          sparse = TRUE
  )))

  mod_moderated <- try(suppressWarnings(rma.mv(as.formula(paste(fml, moderator, sep = " + ")),
                                               V = V,
                                               random = ~ 1 | articlestudy / effect_id,
                                               test = "t",
                                               dfs = "contain",
                                               data = data,
                                               sparse = TRUE
  )))

  if (inherits(mod_null, "try-error") || inherits(mod_moderated, "try-error")) {
    NA
  } else {
    if (sum(mod_null$sigma2) > 0) {
      max(0, 100 * (sum(mod_null$sigma2) - sum(mod_moderated$sigma2)) / sum(mod_null$sigma2))
    } else {
      # catch cases where sum(res0$sigma2) is 0, to avoid -Inf (if sum(res1$sigma2)
      # is positive) or NaN (if sum(res1$sigma2) is also 0) for R^2; for such cases,
      # define R^2 to be 0
      0
    }
  }
}

reg_mods <- c(cat_moderators, cont_moderators)

if (file.exists("data/bs_R2s_reg_mods.rds")) {
  bs_R2_reg_mods <- read_rds("data/bs_R2s_reg_mods.rds")
} else {
  job::job("Bootstrapping R2 reg mods" = {
    library(boot)
    R <- 5000 # Needed for bca confidence intervals (see https://bugs.r-project.org/show_bug.cgi?id=18647), though that takes 6-7h per moderator (M2 Macbook)
    system.time(with_progress({
      p <- progressor(steps = (R+1) * length(reg_mods), label = "Bootstrapping R2s")
      bs_R2s <- reg_mods %>% set_names() %>% map(\(moderator) {
        set.seed(1234)
        if (!file.exists(glue::glue("data/bs_R2s_{moderator}.rds"))) {
          message("Now bootstrapping ", moderator)
          bs <- boot(dataset, boot_R2, R=R, moderator = moderator, ncpus = 8, parallel = "multicore")
          write_rds(bs, glue::glue("data/bs_R2s_{moderator}.rds"))
          bs
        } else {
          read_rds(glue::glue("data/bs_R2s_{moderator}.rds"))
        }

      })
    }))
    write_rds(bs_R2s, "data/bs_R2s_reg_mods.rds")
  }, import = c(rho, dataset, boot_R2, reg_mods))
  message("Registered R2s will be bootstrapped now in background job - rerun this chunk afterwards to load them. Note that this is very slow.")
}

if (file.exists("data/bs_R2s_expl_mods.rds")) {
  bs_R2_expl_mods <- read_rds("data/bs_R2s_expl_mods.rds")
} else {
  job::job("Bootstrapping R2 reg mods" = {
    library(boot)
    R <- 5000 # Needed for bca confidence intervals (see https://bugs.r-project.org/show_bug.cgi?id=18647), though that takes 6-7h per moderator (M2 Macbook)
    system.time(with_progress({
      p <- progressor(steps = (R+1) * length(expl_mods), label = "Bootstrapping R2s")
      bs_R2s <- expl_mods %>% set_names() %>% map(\(moderator) {
        set.seed(1234)
        if (!file.exists(glue::glue("data/bs_R2s_{moderator}.rds"))) {
          message("Now bootstrapping ", moderator)
          bs <- boot(dataset, boot_R2, R=R, moderator = moderator, ncpus = 5, parallel = "multicore")
          write_rds(bs, glue::glue("data/bs_R2s_{moderator}.rds"))
          bs
        } else {
          read_rds(glue::glue("data/bs_R2s_{moderator}.rds"))
        }

      })
    }))
    write_rds(bs_R2s, "data/bs_R2s_expl_mods.rds")
  }, import = c(rho, dataset, boot_R2, expl_mods))
  message("Exploratory R2s will be bootstrapped now in background job - rerun this chunk afterwards to load them. Note that this is very slow.")
}

