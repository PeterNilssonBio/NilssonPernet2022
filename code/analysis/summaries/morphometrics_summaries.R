# Calculate per-beaker summary statistics.
#
# This is where PORL/BL and SL/BL ratios are calculated, since they have to
# be done for individual larvae but averaged per beaker.
#
# Parameters
# ----------
# individual_morphometrics data.frame
#   A data.frame with a row for each larva.
#
# Returns
# -------
# data.frame
#   A data.frame with a row for each beaker.
summarize_beakers <- function(individual_morphometrics) {
  individual_morphometrics %>%
    dplyr::group_by(.data$species, .data$experiment, .data$beaker, .data$density, .data$food) %>%
    dplyr::summarize(
      po.rel = mean(.data$po / .data$bl),
      sl.rel = mean(.data$sl / .data$bl),
      po = mean(.data$po),
      sl = mean(.data$sl),
      bl = mean(.data$bl),
      n = dplyr::n()
    )
}
