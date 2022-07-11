# Create a named list of ANOVAs for LMEMs.
#
# Parameters
# ----------
# named_lmems: list
#   A named list of LMEMs for a single dependent variable, with experiment names
#   as element names.
#
# Returns
# -------
# list
#   List of ANOVAs with same names as named_lmems.
make_named_lmem_anovas <- function(named_lmems) {
  named_lmem_anovas <- list()
  named_lmem_anovas[[settings("experiment_level_dend1")]] <-
    named_lmems[[settings("experiment_level_dend1")]] %>%
    stats::anova()
  named_lmem_anovas[[settings("experiment_level_dend2")]] <-
    named_lmems[[settings("experiment_level_dend2")]] %>%
    stats::anova()
  named_lmem_anovas[[settings("experiment_level_lyt1")]] <-
    named_lmems[[settings("experiment_level_lyt1")]] %>%
    stats::anova()
  named_lmem_anovas[[settings("experiment_level_lyt2")]] <-
    named_lmems[[settings("experiment_level_lyt2")]] %>%
    stats::anova()

  named_lmem_anovas
}
