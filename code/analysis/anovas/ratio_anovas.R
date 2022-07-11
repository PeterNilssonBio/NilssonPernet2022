# Create a named list of ANOVAs for linear models of ratios.
#
# Parameters
# ----------
# named_models: list
#   A named list of aovs for ratios of either PORL/BL or SL/BL, with
#   experiment names as element names.
#
# Returns
# -------
# list
#   List of ANOVAs with same names as named_models.
make_named_anovas <- function(named_models) {
  named_anovas <- list()
  named_anovas[[settings("experiment_level_dend1")]] <-
    named_models[[settings("experiment_level_dend1")]] %>%
    stats::anova()
  named_anovas[[settings("experiment_level_dend2")]] <-
    named_models[[settings("experiment_level_dend2")]] %>%
    stats::anova()
  named_anovas[[settings("experiment_level_lyt1")]] <-
    named_models[[settings("experiment_level_lyt1")]] %>%
    stats::anova()
  named_anovas[[settings("experiment_level_lyt2")]] <-
    named_models[[settings("experiment_level_lyt2")]] %>%
    stats::anova()

  named_anovas
}
