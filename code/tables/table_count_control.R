# Create HTML table for summaries of count control data.
#
# This table shows the proportion of the nominal amount of larvae found in
# count control beakers. Note there is no data for the 0.015 larvae/ml density
# because these larvae were hand counted exactly during distribution rather
# than aliquoted.
#
# These data are presented in the manuscript in prose but not in tabular form.
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
render_count_control_table <- function(analyses) {
  analyses$count_control_summaries %>%
    rename_column_with_string(settings("spawn_date_label_col_header"), "spawn_date") %>%
    rename_column_with_string(settings("experiment_label_col_header"), "experiment") %>%
    rename_column_with_string(settings("species_label_col_header"), "species") %>%
    rename_column_with_string(settings("density_label_col_header"), "density") %>%
    rename_column_with_string(settings("proportion_label_col_header"), "proportion") %>%
    kableExtra::kbl(
      format = "html",
      digits = settings("table_digits"),
    ) %>%
    kableExtra::kable_classic(full_width = F)
}
