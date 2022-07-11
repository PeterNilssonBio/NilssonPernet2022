# Create a named list of contrasts for a given set of linear models of ratios.
#
# Parameters
# ----------
# named_aovs: list
#   A named list of aov objects for ratios, with experiment names as element
#   names.
#
# Returns
# -------
# list
#   List of data.frames with same names as named_aovs, holding contrast data.
make_named_aov_contrasts_dfs <- function(named_aovs) {
  named_aov_contrasts_dfs <- list()
  named_aov_contrasts_dfs[[settings("experiment_level_dend1")]] <-
    named_aovs[[settings("experiment_level_dend1")]] %>%
    aov_contrast_df()
  named_aov_contrasts_dfs[[settings("experiment_level_dend2")]] <-
    named_aovs[[settings("experiment_level_dend2")]] %>%
    aov_contrast_df()
  named_aov_contrasts_dfs[[settings("experiment_level_lyt1")]] <-
    named_aovs[[settings("experiment_level_lyt1")]] %>%
    aov_contrast_df()
  named_aov_contrasts_dfs[[settings("experiment_level_lyt2")]] <-
    named_aovs[[settings("experiment_level_lyt2")]] %>%
    aov_contrast_df()

  named_aov_contrasts_dfs
}


# Make a data.frame with contrast information for a single aov.
# Parameters
# ----------
# aov_for_one_experiment: aov
#   Linear model for a single dependent variable in a single experiment.
#
# Returns
# -------
# data.frame
#   Data frame with contrast information.
aov_contrast_df <- function(aov_for_one_experiment) {
  aov_for_one_experiment %>%
    rstatix::tukey_hsd("density:food") %>%
    dplyr::mutate(contrast = stringr::str_replace_all(
      paste0(.data$group1, " - ", .data$group2), ":", " "
    )) %>%
    as.data.frame()
}
