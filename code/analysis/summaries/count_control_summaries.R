# Calculate per-treatment count control summary statistics.
#
# Parameters
# ----------
# count_control_beakers: data.frame
#   Data frame of per-beaker larval counts
#
# Returns
# -------
# data.frame
#   A data frame with a row for each treatment (per experiment).
summarize_count_control <- function(count_control_beakers) {
  count_control_beakers %>%
    dplyr::mutate(proportion = .data$counted / .data$expected) %>%
    dplyr::group_by(.data$spawn_date, .data$species, .data$experiment, .data$density) %>%
    dplyr::summarize(proportion = mean(.data$proportion))
}
