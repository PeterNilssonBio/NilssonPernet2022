# Generate plot object for Figure S1.
#
# This figure is a bar plot showing the PORL/BL and SL/BL ratios for each
# treatment.
#
# Parameters
# ----------
# analyses: list
#   Object returned by analyze().
#
# Returns
# -------
# ggplot
plot_fig_S1 <- function(analyses) {
  po_plot <- plot_aov_barplot(
    analyses$beaker_summaries,
    analyses$ratio_aov_contrast_dfs$po,
    y_var = "po.rel",
    y_label = settings("po_bl_ratio_label"),
    y_limit_max = settings("figure_S1_po_bl_ratio_y_max"),
    row = "top"
  )

  sl_plot <- plot_aov_barplot(
    analyses$beaker_summaries,
    analyses$ratio_aov_contrast_dfs$sl,
    y_var = "sl.rel",
    y_label = settings("sl_bl_ratio_label"),
    y_limit_max = settings("figure_S1_sl_bl_ratio_y_max"),
    row = "bottom"
  )

  po_grob <- ggplot2::ggplotGrob(po_plot)
  sl_grob <- ggplot2::ggplotGrob(sl_plot)
  plot_grid <- rbind(po_grob, sl_grob, size = "first")
  grid::grid.newpage()
  grid::grid.draw(plot_grid)
  plot_grid
}



# Create barplot for a ratio ANOVA from a model aov object.
#
# Parameters
# ----------
# beaker_summaries: data.frame
#   Data frame with per-beaker summary statistics.
# aov_contrast_dfs: data.frame
#   Data frame of contrasts for models.
# y_var: character
#   Name of y-variable in data frame: "po.rel" or "sl.rel".
# y_label: character
#   Name to display on plot for y-axis.
# y_limit_max: numeric
#   Maximum y-value to draw. Warning: discards out-of-bounds points.
# row: character
#   Which row of facets these plots will be: "top" or "bottom".
#
# Returns
# -------
# ggplot
plot_aov_barplot <- function(beaker_summaries,
                             aov_contrast_dfs,
                             y_var,
                             y_label,
                             y_limit_max,
                             row) {
  if (row == "top") {
    show_legend <- T
    show_x_axis_title <- F
  } else if (row == "bottom") {
    show_legend <- F
    show_x_axis_title <- T
  } else {
    stop(paste0("Unexpected plot row name \"", row, "\"."))
  }

  plottable_contrasts <-
    make_plottable_aov_contrasts_df(
      beaker_summaries = beaker_summaries,
      named_aov_contrast_dfs = aov_contrast_dfs,
      y_var
    )

  p <-
    ggpubr::ggbarplot(beaker_summaries,
      x = "density",
      y = y_var,
      fill = "food",
      facet.by = "experiment",
      add = "mean_se_",
      add.params = list(color = settings("error_bar_color")),
      position = ggplot2::position_dodge(
        width = settings("figure_S1_bar_dodge")
      ),
      nrow = 1
    ) +
    ggplot2::xlab(settings("density_label_legend")) +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_limit_max), # Stop p-values at top from being cut off.
      expand = c(0, 0)
    ) + # Start bottom of bar at x-axis.
    food_fill() +
    density_x_labels() +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title = settings("food_label_legend")
    )) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(),
      panel.grid.minor.y = ggplot2::element_line()
    )

  if (!show_x_axis_title) {
    p <-
      p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if (!show_legend) {
    p <-
      p + ggplot2::theme(legend.position = "none")
  }

  p <-
    p +
    ggpubr::stat_pvalue_manual(
      plottable_contrasts,
      x = "density",
      label = "p.text",
      label.size = settings("p_value_font_size"),
      position = ggplot2::position_dodge(settings("figure_S1_bar_dodge"))
    )

  p
}


# Prepare a data frame for use in ggpubr::stat_pvalue_manual.
make_plottable_aov_contrasts_df <- function(beaker_summaries, named_aov_contrast_dfs, y_var) {
  # This improves on the old procedure of calculating label heights; it no
  # longer just assumes that the overall mean for a density corresponds to
  # the height of any obstacles on the plot. Instead, we figure out the height
  # of the higher of the two error bars. This makes L. pictus Expt 2 0.015/ml
  # get a label at a better height (everything else was pretty okay anyway.)
  get_y_pos <- function(morph, exper, dns) {
    low_food_data <-
      filter_experiment(morph, exper) %>%
      filter_density(dns) %>%
      filter_food(settings("food_level_low"))
    high_food_data <-
      filter_experiment(morph, exper) %>%
      filter_density(dns) %>%
      filter_food(settings("food_level_high"))

    top_of_error_bar <- function(d) {
      ys <- d[[y_var]]
      mean(ys) + stats::sd(ys) / sqrt(length(ys))
    }

    max(
      top_of_error_bar(low_food_data),
      top_of_error_bar(high_food_data)
    )
  }


  data.frame(
    experiment = c(
      rep(settings("experiment_level_dend1"), 2),
      rep(settings("experiment_level_dend2"), 3),
      rep(settings("experiment_level_lyt1"), 3),
      rep(settings("experiment_level_lyt2"), 3)
    ),
    density = c(
      settings("density_level_sparse"),
      settings("density_level_dense"),
      rep(c(
        settings("density_level_extrasparse"),
        settings("density_level_sparse"),
        settings("density_level_dense")
      ), 3)
    ),
    group1 = rep(settings("food_level_low"), 11),
    group2 = rep(settings("food_level_high"), 11),
    p = c(
      (named_aov_contrast_dfs[[settings("experiment_level_dend1")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_dend1")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_dend2")]] %>%
        dplyr::filter(.data$contrast == extrasparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_dend2")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_dend2")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_lyt1")]] %>%
        dplyr::filter(.data$contrast == extrasparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_lyt1")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_lyt1")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_lyt2")]] %>%
        dplyr::filter(.data$contrast == extrasparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_lyt2")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.adj %>% dplyr::first(),
      (named_aov_contrast_dfs[[settings("experiment_level_lyt2")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.adj %>% dplyr::first()
    ),
    y.position <- c(
      get_y_pos(beaker_summaries, settings("experiment_level_dend1"), settings("density_level_sparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_dend1"), settings("density_level_dense")),
      get_y_pos(beaker_summaries, settings("experiment_level_dend2"), settings("density_level_extrasparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_dend2"), settings("density_level_sparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_dend2"), settings("density_level_dense")),
      get_y_pos(beaker_summaries, settings("experiment_level_lyt1"), settings("density_level_extrasparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_lyt1"), settings("density_level_sparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_lyt1"), settings("density_level_dense")),
      get_y_pos(beaker_summaries, settings("experiment_level_lyt2"), settings("density_level_extrasparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_lyt2"), settings("density_level_sparse")),
      get_y_pos(beaker_summaries, settings("experiment_level_lyt2"), settings("density_level_dense"))
    )
  ) %>%
    dplyr::mutate(
      p = round(.data$p, settings("p_value_nsmall")),
      stars = p_stars_no_dot(.data$p),
      p.text = p_min(.data$p),
      y.position = y.position * settings("figure_S1_height_mult")
    )
}
