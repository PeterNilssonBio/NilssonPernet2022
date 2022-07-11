# Generate plot object for Figure 5.
#
# This figure plots the estimated marginal mean of each treatment as a point,
# with SL on the y-axis and PORL on the x-axis.
#
# Parameters
# ----------
# analyses: list
#   Object returned by analyze().
#
# Returns
# -------
# ggplot
plot_fig_5 <- function(analyses) {
  p <-
    ggpubr::ggscatter(
      analyses$emm_dfs$combined,
      x = "emmean.po",
      y = "emmean.sl",
      fill = "food",
      shape = "density",
      size = settings("density_shape_size"),
      stroke = settings("density_stroke"),
      facet.by = c("experiment"),
      nrow = 1
    ) +
    density_shape() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin  = .data$emmean.sl - .data$SE.sl,
        ymax  = .data$emmean.sl + .data$SE.sl,
        x     = .data$emmean.po
      ),
      color = settings("error_bar_color")
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin  = .data$emmean.po - .data$SE.po,
        xmax  = .data$emmean.po + .data$SE.po,
        y     = .data$emmean.sl
      ),
      color = settings("error_bar_color")
    )

  # This call to ggplot2::geom_line is wrapped in suppressWarnings to suppress
  # an incorrect warning. It claims that it ignores the shape aesthetic.
  # It does not ignore it; in fact, it behaves incorrectly if shape is not
  # specified - the line ends up connecting points in some arbitrary order
  # (or, at least, not a meaningful one) if we don't specify shape below.
  suppressWarnings(
    p <-
      p +
      ggplot2::geom_line(
        ggplot2::aes(
          shape = .data$density
        ), #  Warning: 'Ignoring unknown aesthetics: shape'
        color = settings("density_line_color")
      )
  )

  p <-
    p +
    food_fill() +
    ggplot2::xlab(settings("po_emm_label")) +
    ggplot2::ylab(settings("sl_emm_label")) +
    ggplot2::theme(
      legend.spacing.x = grid::unit(
        settings("figure_5_legend_spacing_mm"),
        "mm"
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title = settings("food_label_legend"),
        override.aes = list(shape = settings("density_shape_sparse"))
      ),
      shape = ggplot2::guide_legend(
        title = settings("density_label_legend")
      )
    ) # Need this to see shapes.
}
