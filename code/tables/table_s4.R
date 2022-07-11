# Create HTML table for Table S4.
#
# This table shows outliers of PORL/BL and SL/BL ratios.
#
# Parameters
# ----------
# analyses: list
#   Object returned by analyze().
#
# Returns
# -------
# data.frame
#   Data frame suitable for output with kable.
render_table_S4 <- function(analyses) {
  make_sub_table <- function(df, experiment, variable) {
    df %>%
      rename_column_with_string(settings("density_label_col_header"), "density") %>%
      rename_column_with_string(settings("food_label_col_header"), "food") %>%
      # Add this info to table itself so it is easier to merge these.
      add_experiment_and_variable_columns(experiment, variable) %>%
      # See note in render_table_S3 about ordering of these columns for this table.
      dplyr::relocate(.data$Experiment, .data$Variable)
  }

  get_outliers <- function(experiment,
                           variable) {
    # Somehow, `variable` is treated as an external vector and must be wrapped
    # in tidyselect::all_of() to avoid message about ambiguity.
    analyses$beaker_summaries %>%
      filter_experiment(experiment) %>%
      dplyr::group_by(food, density) %>%
      rstatix::identify_outliers(tidyselect::all_of(variable)) %>%
      dplyr::rename(Value = tidyselect::all_of(variable)) %>%
      dplyr::select(c("density", "food", "Value"))
  }

  experiments <- c(
    settings("experiment_level_dend1"),
    settings("experiment_level_dend2"),
    settings("experiment_level_lyt1"),
    settings("experiment_level_lyt2")
  )

  po_outliers <- list()
  sl_outliers <- list()
  for (experiment in experiments) {
    po_outliers_for_this_experiment <-
      get_outliers(experiment, "po.rel")

    if (nrow(po_outliers_for_this_experiment) > 0) {
      po_outliers[[experiment]] <-
        po_outliers_for_this_experiment %>%
        make_sub_table(experiment, settings("po_bl_ratio_label"))
    }

    sl_outliers_for_this_experiment <-
      get_outliers(experiment, "sl.rel")

    if (nrow(sl_outliers_for_this_experiment) > 0) {
      sl_outliers[[experiment]] <-
        sl_outliers_for_this_experiment %>%
        make_sub_table(experiment, settings("sl_bl_ratio_label"))
    }
  }

  joined_table <- combine_tables(c(
    po_outliers,
    sl_outliers
  ))

  joined_table %>%
    render_kable()
}
