# Create an EMM for a single variable and experiment.
#
#  Parameters
#  ----------
#   lmem_for_one_experiment: lmerModLmerTest
#     LMEM for a single variable and experiment.
#
#  Returns
#  -------
#  emmGrid
#    EMM for the LMEM
emm <- function(lmem_for_one_experiment) {
  lmem_for_one_experiment %>%
    emmeans::emmeans(~ density * food)
}


# Create a named list of EMMs for LMEMs.
#
# Parameters
# ----------
# named_lmems: list
#   A named list of lmerModLmerTest objects for a single dependent variable,
#   with experiment names as element names.
#
# Returns
# -------
# list
#   List of emmGrid objects with same names as named_lmems, holding contrast data.
make_named_emms <- function(named_lmems) {
  emms <- list()
  emms[[settings("experiment_level_dend1")]] <-
    named_lmems[[settings("experiment_level_dend1")]] %>%
    emm()
  emms[[settings("experiment_level_dend2")]] <-
    named_lmems[[settings("experiment_level_dend2")]] %>%
    emm()
  emms[[settings("experiment_level_lyt1")]] <-
    named_lmems[[settings("experiment_level_lyt1")]] %>%
    emm()
  emms[[settings("experiment_level_lyt2")]] <-
    named_lmems[[settings("experiment_level_lyt2")]] %>%
    emm()

  emms
}


# Create a data.frame with EMM information for a single experiment.
emm_df <- function(emm_for_one_experiment, species_name, experiment_name) {
  emm_for_one_experiment %>%
    as.data.frame() %>%
    dplyr::mutate(
      species = species_name,
      experiment = experiment_name
    ) %>%
    dplyr::relocate(.data$species, .data$experiment)
}


# Create a named list of data.frame objects holding EMM info.
#
# Parameters
# ----------
# named_emms: list
#   A named list of emmGrid objects for a single dependent variable,  with
#   experiment names as element names.
#
# Returns
# -------
# list
#   List of data.frame objects with same names as named_emms.
make_named_emm_dfs <- function(named_emms) {
  emm_dfs <- list()
  emm_dfs[[settings("experiment_level_dend1")]] <-
    named_emms[[settings("experiment_level_dend1")]] %>%
    emm_df(settings("species_level_dend"), settings("experiment_level_dend1"))
  emm_dfs[[settings("experiment_level_dend2")]] <-
    named_emms[[settings("experiment_level_dend2")]] %>%
    emm_df(settings("species_level_dend"), settings("experiment_level_dend2"))
  emm_dfs[[settings("experiment_level_lyt1")]] <-
    named_emms[[settings("experiment_level_lyt1")]] %>%
    emm_df(settings("species_level_lyt"), settings("experiment_level_lyt1"))
  emm_dfs[[settings("experiment_level_lyt2")]] <-
    named_emms[[settings("experiment_level_lyt2")]] %>%
    emm_df(settings("species_level_lyt"), settings("experiment_level_lyt2"))

  emm_dfs
}


# Combine a list of data.frames with EMM info into one big data.frame.
join_emm_dfs <- function(emm_df_list) {
  join_all(emm_df_list,
    by = c(
      "species", "experiment", "density", "food", "emmean", "SE",
      "df", "lower.CL", "upper.CL"
    )
  )
}


# Combine a data.frame of PORL EMM info with one of SL EMM info.
combine_po_and_sl_emm_df <- function(po_emm_df, sl_emm_df) {
  dplyr::full_join(po_emm_df, sl_emm_df,
    by = c("species", "experiment", "density", "food"),
    suffix = c(".po", ".sl")
  )
}
