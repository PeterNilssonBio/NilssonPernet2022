# == LMEMs with density:BL interaction == #

po_lmem_with_density_bl_interaction <- function(individual_morphometrics_for_one_experiment) {
  po_lmem_formula_with_density_bl_interaction <-
    po ~ density * food + bl + food:bl + (1 | beaker)

  f <- function() {
    individual_morphometrics_for_one_experiment %>%
      lmerTest::lmer(po_lmem_formula_with_density_bl_interaction, .)
  }

  if (settings("warn_singular")) {
    return(f())
  } else {
    # Stop printing message about singular fit.
    suppressMessages(
      return(f())
    ) # Message: boundary (singular) fit: see help('isSingular')
  }
}

sl_lmem_with_density_bl_interaction <- function(individual_morphometrics_for_one_experiment) {
  sl_lmem_formula_with_density_bl_interaction <-
    sl ~ density * food + bl + food:bl + (1 | beaker)

  f <- function() {
    individual_morphometrics_for_one_experiment %>%
      lmerTest::lmer(sl_lmem_formula_with_density_bl_interaction, .)
  }

  if (settings("warn_singular")) {
    return(f())
  } else {
    # Stop printing message about singular fit.
    suppressMessages(
      return(f())
    ) # Message: boundary (singular) fit: see help('isSingular')
  }
}


po_lmem_with_density_bl_interaction_for_experiment <- function(individual_morphometrics, experiment_name) {
  dataset <-
    individual_morphometrics %>%
    filter_experiment(experiment_name) %>%
    po_lmem_with_density_bl_interaction()
}

sl_lmem_with_density_bl_interaction_for_experiment <- function(individual_morphometrics, experiment_name) {
  dataset <-
    individual_morphometrics %>%
    filter_experiment(experiment_name) %>%
    sl_lmem_with_density_bl_interaction()
}


po_lmems_with_density_bl_interaction_for_all_experiments <- function(individual_morphometrics) {
  make_named_lmems_with_density_bl_interaction(individual_morphometrics, "po")
}

sl_lmems_with_density_bl_interaction_for_all_experiments <- function(individual_morphometrics) {
  make_named_lmems_with_density_bl_interaction(individual_morphometrics, "sl")
}

make_named_lmems_with_density_bl_interaction <- function(individual_morphometrics,
                                                         dependent_var) {
  lmem_function <- ifelse(dependent_var == "po",
    po_lmem_with_density_bl_interaction_for_experiment,
    ifelse(dependent_var == "sl",
      sl_lmem_with_density_bl_interaction_for_experiment,
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
