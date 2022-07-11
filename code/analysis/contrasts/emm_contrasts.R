# Create a named list of contrasts for EMMs.
#
# Parameters
# ----------
# named_emms: emm_list
#   A named list of emmGrid objects for LMEMs, with experiment names as element
#   names.
#
# Returns
# -------
# list
#   List of data.frames with same names as named_emms, holding contrast data.
make_named_emm_contrasts_dfs <- function(named_emms) {
  named_emm_contrasts_dfs <- list()
  named_emm_contrasts_dfs[[settings("experiment_level_dend1")]] <-
    named_emms[[settings("experiment_level_dend1")]] %>%
    emm_contrast_df()
  named_emm_contrasts_dfs[[settings("experiment_level_dend2")]] <-
    named_emms[[settings("experiment_level_dend2")]] %>%
    emm_contrast_df()
  named_emm_contrasts_dfs[[settings("experiment_level_lyt1")]] <-
    named_emms[[settings("experiment_level_lyt1")]] %>%
    emm_contrast_df()
  named_emm_contrasts_dfs[[settings("experiment_level_lyt2")]] <-
    named_emms[[settings("experiment_level_lyt2")]] %>%
    emm_contrast_df()

  named_emm_contrasts_dfs
}


# Make a data.frame with contrast information for a single emmGrid.
# Parameters
# ----------
# emm_for_one_experiment: emmGrid
#   EMM for a single dependent variable in a single experiment.
#
# Returns
# -------
# data.frame
#   Data frame with contrast information.
emm_contrast_df <- function(emm_for_one_experiment) {
  emm_for_one_experiment %>%
    emmeans::contrast(
      method = "pairwise",
      # The default behavior changed at some point, which
      # broke everything because the names were different.
      enhance.levels = F
    ) %>%
    as.data.frame()
}
