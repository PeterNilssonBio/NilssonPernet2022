# Generate plot object for Figure 4.
#
# This figure is a bar plot showing the estimated marginal means for SL.
#
# Parameters
# ----------
# analyses: list
#   Object returned by analyze().
#
# Returns
# -------
# ggplot
plot_fig_4 <- function(analyses) {
  plot_emm_barplot(
    analyses$emm_dfs$sl,
    analyses$emm_dfs$list$sl,
    analyses$emm_contrast_dfs$list$sl,
    settings("sl_emm_label"),
    settings("figure_4_y_max")
  )
}
