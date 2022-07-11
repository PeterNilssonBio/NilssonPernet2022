# Create HTML table for Table S3.
#
# This table shows violations of the assumptions of normality for PORL/BL and
# SL/BL ratios.
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
render_table_S3 <- function(analyses) {
  make_sub_table <- function(df, experiment, variable) {
    df %>%
      dplyr::select(!"variable") %>%
      rename_column_with_string(settings("density_label_col_header"), "density") %>%
      rename_column_with_string(settings("food_label_col_header"), "food") %>%
      dplyr::rename(
        W = .data$statistic,
        P = .data$p
      ) %>%
      # Add this info to table itself so it is easier to merge these.
      add_experiment_and_variable_columns(experiment, variable) %>%
      # See note below about ordering of these columns for this table.
      dplyr::relocate(.data$Experiment, .data$Variable) %>%
      # Draw significance stars.
      dplyr::mutate(
        Sig = p_stars_no_dot(.data$P),
        P = p_min(.data$P)
      )
  }

  get_normality_violations <- function(experiment,
                                       dependent_var) {
    analyses$beaker_summaries %>%
      filter_experiment(experiment) %>%
      dplyr::group_by(.data$food, .data$density) %>%
      rstatix::shapiro_test(vars = c(dependent_var)) %>%
      dplyr::filter(.data$p <= settings("shapiro_p"))
  }

  experiments <- c(
    settings("experiment_level_dend1"),
    settings("experiment_level_dend2"),
    settings("experiment_level_lyt1"),
    settings("experiment_level_lyt2")
  )

  po_normality_violations <- list()
  sl_normality_violations <- list()
  # This way of arranging the loop makes sense for this table, because
  # we group things by experiment then by variable, the other way from
  # other tables. That layout makes sense for this table because it is simply
  # listing stuff from each experiment. Whether this clarity is worth the
  # apparent inconsistency in how we order columns as compared to other tables
  # is debatable.
  for (experiment in experiments) {
    po_normality_violations_for_this_experiment <-
      get_normality_violations(experiment, "po.rel")

    if (nrow(po_normality_violations_for_this_experiment) > 0) {
      po_normality_violations[[experiment]] <-
        po_normality_violations_for_this_experiment %>%
        make_sub_table(experiment, settings("po_bl_ratio_label"))
    }

    sl_normality_violations_for_this_experiment <-
      get_normality_violations(experiment, "sl.rel")

    if (nrow(sl_normality_violations_for_this_experiment) > 0) {
      sl_normality_violations[[experiment]] <-
        sl_normality_violations_for_this_experiment %>%
        make_sub_table(experiment, settings("sl_bl_ratio_label"))
    }
  }

  joined_table <- combine_tables(c(
    po_normality_violations,
    sl_normality_violations
  ))

  joined_table %>%
    render_kable()
}
