# Create a linear model of PORL/BL ratios for a single experiment.
#
# Parameters
# ----------
# beaker_morphometrics_for_one_experiment: data.frame
#   Data frame of per-beaker summary statistics for a single experiment.
#
# Returns
# -------
# aov
#   Linear model of PORL/BL ratio for an experiment.
po_ratio_aov <- function(beaker_morphometrics_for_one_experiment) {
  po_ratio_aov_formula <-
    po.rel ~ # Dependent variable.
    density * food # Both factors and their interaction.

  beaker_morphometrics_for_one_experiment %>%
    dplyr::ungroup() %>%
    stats::aov(po_ratio_aov_formula, .)
}


# Create a linear model of SL/BL ratios for a single experiment.
#
# Parameters
# ----------
# beaker_morphometrics_for_one_experiment: data.frame
#   Data frame of per-beaker summary statistics for a single experiment.
#
# Returns
# -------
# aov
#   Linear model of SL/BL ratio for an experiment.
sl_ratio_aov <- function(beaker_morphometrics_for_one_experiment) {
  sl_ratio_aov_formula <-
    sl.rel ~ # Dependent variable.
    density * food # Both factors and their interaction.

  beaker_morphometrics_for_one_experiment %>%
    dplyr::ungroup() %>%
    stats::aov(sl_ratio_aov_formula, .)
}


# Create a linear model of PORL/BL ratios for a single experiment.
#
# Parameters
# ----------
# beaker_morphometrics: data.frame
#   Data frame of per-beaker summary statistics for all experiments.
# experiment_name: character
#   Name of experiment to create model for.
#
# Returns
# -------
# aov
#   Linear model of PORL/BL ratio for an experiment.
po_ratio_aov_for_experiment <- function(beaker_morphometrics, experiment_name) {
  dataset <-
    beaker_morphometrics %>%
    filter_experiment(experiment_name) %>%
    po_ratio_aov()
}


# Create a linear model of SL/BL ratios for a single experiment.
#
# Parameters
# ----------
# beaker_morphometrics: data.frame
#   Data frame of per-beaker summary statistics for all experiments.
# experiment_name: character
#   Name of experiment to create model for.
#
# Returns
# -------
# aov
#   Linear model of SL/BL ratio for an experiment.
sl_ratio_aov_for_experiment <- function(beaker_morphometrics, experiment_name) {
  dataset <-
    beaker_morphometrics %>%
    filter_experiment(experiment_name) %>%
    sl_ratio_aov()
}


# Create PORL/BL or SL/BL ratio models keyed by experiment name.
#
# Parameters
# ----------
# beaker_morphometrics: data.frame
#   Per-beaker summary statistics data frame.
# v: character
#   Variable, either "po.rel" or "sl.rel".
#
# Returns
# -------
# aov
#   Linear model of PORL/BL or SL/BL ratio for an experiment.
make_named_ratio_aovs <- function(beaker_morphometrics, v) {
  aov_function <- ifelse(v == "po.rel",
    po_ratio_aov_for_experiment,
    ifelse(v == "sl.rel",
      sl_ratio_aov_for_experiment,
      stop("unexpected variable name")
    )
  )

  dend1_aov <-
    beaker_morphometrics %>%
    aov_function(settings("experiment_level_dend1"))
  dend2_aov <-
    beaker_morphometrics %>%
    aov_function(settings("experiment_level_dend2"))
  lyt1_aov <-
    beaker_morphometrics %>%
    aov_function(settings("experiment_level_lyt1"))
  lyt2_aov <-
    beaker_morphometrics %>%
    aov_function(settings("experiment_level_lyt2"))

  aovs <- list()
  aovs[[settings("experiment_level_dend1")]] <- dend1_aov
  aovs[[settings("experiment_level_dend2")]] <- dend2_aov
  aovs[[settings("experiment_level_lyt1")]] <- lyt1_aov
  aovs[[settings("experiment_level_lyt2")]] <- lyt2_aov

  aovs
}
