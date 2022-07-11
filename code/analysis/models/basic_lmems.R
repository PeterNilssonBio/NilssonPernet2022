# == Basic LMEMs: No interactions with BL == #


# Basic LMEM of PORL used in manuscript.
#
# Parameters
# ----------
# individual_morphometrics_for_one_experiment: data.frame
#   Morphometrics data filtered to include only one experiment.
#
# Returns
# -------
# lmerModLmerTest
#   LMEM of PORL for the given experiment.
po_lmem <- function(individual_morphometrics_for_one_experiment) {
  po_lmem_formula <-
    po ~ # Dependent variable.
    density * food + # Both factors and their interaction as fixed effects.
    bl + # Body length as covariate.
    (1 | beaker) # Random intercept by container. "1" means no random slope.

  # The above formula produces the same results as the below SPSS code:
  #   MIXED po BY beaker density food WITH bl
  #     /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  #     /FIXED=bl density food density*food | NOINT SSTYPE(3)
  #     /METHOD=REML
  #     /RANDOM=beaker | COVTYPE(VC)

  f <- function() {
    individual_morphometrics_for_one_experiment %>%
      lmerTest::lmer(po_lmem_formula, .)
  }

  if (settings("warn_singular")) {
    return(f())
  } else {
    # Stop printing message about singular fit.
    # The only singular fit for PO in the primary set of models occurs for
    # D. excentricus Expt 2. There's nothing we can do about this, as we cannot
    # eliminate any terms from these models without making them no longer LMEMs
    # and thus being unable to compare them. See supplemental materials, section
    # "Linear mixed-effects models with body length interaction terms".
    suppressMessages(
      return(f())
    ) # Message: boundary (singular) fit: see help('isSingular')
  }
}


# Basic LMEM of SL used in manuscript.
#
# Parameters
# ----------
# individual_morphometrics_for_one_experiment: data.frame
#   Morphometrics data filtered to include only one experiment.
#
# Returns
# -------
# lmerModLmerTest
#   LMEM of SL for the given experiment.
sl_lmem <- function(individual_morphometrics_for_one_experiment) {
  sl_lmem_formula <-
    sl ~ # Dependent variable.
    density * food + # Both factors and their interaction as fixed effects.
    bl + # Body length as covariate.
    (1 | beaker) # Random intercept by container. "1" means no random slope.


  f <- function() {
    individual_morphometrics_for_one_experiment %>%
      lmerTest::lmer(sl_lmem_formula, .)
  }

  if (settings("warn_singular")) {
    return(f())
  } else {
    # Stop printing message about singular fit.
    # The only singular fit for SL in the primary set of models occurs for
    # L. pictus Expt 1. There's nothing we can do about this, as we cannot
    # eliminate any terms from these models without making them no longer LMEMs
    # and thus being unable to compare them. See supplemental materials, section
    # "Linear mixed-effects models with body length interaction terms".
    suppressMessages(
      return(f())
    ) # Message: boundary (singular) fit: see help('isSingular')
  }
}


# Basic LMEM of PORL used in manuscript.
#
# Parameters
# ----------
# individual_morphometrics: data.frame
#   All morphometrics
# experiment_name: character
#   Experiment to get PORL LMEM for.
#
# Returns
# -------
# lmerModLmerTest
#   LMEM of PORL for the given experiment.
po_lmem_for_experiment <- function(individual_morphometrics, experiment_name) {
  dataset <-
    individual_morphometrics %>%
    filter_experiment(experiment_name) %>%
    po_lmem()
}


# Basic LMEM of SL used in manuscript.
#
# Parameters
# ----------
# individual_morphometrics: data.frame
#   All morphometrics
# experiment_name: character
#   Experiment to get SL LMEM for.
#
# Returns
# -------
# lmerModLmerTest
#   LMEM of SL for the given experiment.
sl_lmem_for_experiment <- function(individual_morphometrics, experiment_name) {
  dataset <-
    individual_morphometrics %>%
    filter_experiment(experiment_name) %>%
    sl_lmem()
}


# Basic LMEM of PORL used in manuscript for every experiment.
#
# Parameters
# ----------
# individual_morphometrics: data.frame
#   All morphometrics
#
# Returns
# -------
# list
#   Named list of LMEM of PORL for all experiments.
po_lmems_for_all_experiments <- function(individual_morphometrics) {
  make_named_lmems(individual_morphometrics, "po")
}


# Basic SL of PORL used in manuscript for every experiment.
#
# Parameters
# ----------
# individual_morphometrics: data.frame
#   All morphometrics
#
# Returns
# -------
# list
#   Named list of SL of PORL for all experiments.
sl_lmems_for_all_experiments <- function(individual_morphometrics) {
  make_named_lmems(individual_morphometrics, "sl")
}


# Basic LMEM of PORL or SL used in manuscript for every experiment.
#
# Parameters
# ----------
# individual_morphometrics: data.frame
#   All morphometrics
# dependent_var: character
#   Name of dependent variable, either "po" or "sl".
#
# Returns
# -------
# list
#   Named list of LMEM of PORL or SL for all experiments.
make_named_lmems <- function(individual_morphometrics, dependent_var) {
  lmem_function <- ifelse(dependent_var == "po",
    po_lmem_for_experiment,
    ifelse(dependent_var == "sl",
      sl_lmem_for_experiment,
      stop("unexpected variable name")
    )
  )

  dend1_lmem <-
    individual_morphometrics %>%
    lmem_function(settings("experiment_level_dend1"))
  dend2_lmem <-
    individual_morphometrics %>%
    lmem_function(settings("experiment_level_dend2"))
  lyt1_lmem <-
    individual_morphometrics %>%
    lmem_function(settings("experiment_level_lyt1"))
  lyt2_lmem <-
    individual_morphometrics %>%
    lmem_function(settings("experiment_level_lyt2"))

  lmems <- list()
  lmems[[settings("experiment_level_dend1")]] <- dend1_lmem
  lmems[[settings("experiment_level_dend2")]] <- dend2_lmem
  lmems[[settings("experiment_level_lyt1")]] <- lyt1_lmem
  lmems[[settings("experiment_level_lyt2")]] <- lyt2_lmem

  lmems
}
