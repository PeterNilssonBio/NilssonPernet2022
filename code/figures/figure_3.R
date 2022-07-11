# Generate plot object for Figure 3.
#
# This figure is a bar plot showing the estimated marginal means for PORL.
#
# Parameters
# ----------
# analyses: list
#   Object returned by analyze().
#
# Returns
# -------
# ggplot
plot_fig_3 <- function(analyses) {
  plot_emm_barplot(
    analyses$emm_dfs$po,
    analyses$emm_dfs$list$po,
    analyses$emm_contrast_dfs$list$po,
    settings("po_emm_label"),
    settings("figure_3_y_max")
  )
}
