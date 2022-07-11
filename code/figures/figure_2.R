# Generate plot object for Figure 2.
#
# This figure is a bar plot showing the BL, PORL, and SL means for each
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
plot_fig_2 <- function(analyses) {
  food_t_test_df <- food_t_tests(analyses$beaker_summaries)

  # The trick here is to make a facetted plot for each dependent variable,
  # then manually duct-tape them together with rbind().
  # This requires we omit certain parts of each plot - e.g., only the plot that
  # will become the bottom "row" should have an x-axis title.
  # This is why grid.arrange() won't work - it will give each row equal height,
  # which makes the middle row (no legend above or below) have taller plots.

  bl_plot <- plot_dependent_variable(
    y = "bl",
    y_label = settings("bl_label"),
    y_limit_max = settings("figure_2_bl_y_max"),
    beaker_summaries_for_all_experiments = analyses$beaker_summaries,
    food_t_test_df = food_t_test_df,
    row = "top"
  )

  # Equivalent to Kacenas & Podolsky (2018) Fig 2A.
  po_plot <- plot_dependent_variable(
    y = "po",
    y_label = settings("po_label"),
    y_limit_max = settings("figure_2_po_y_max"),
    beaker_summaries_for_all_experiments = analyses$beaker_summaries,
    food_t_test_df = food_t_test_df,
    row = "middle"
  )

  # Equivalent to Kacenas & Podolsky (2018) Fig 2B.
  sl_plot <- plot_dependent_variable(
    y = "sl",
    y_label = settings("sl_label"),
    y_limit_max = settings("figure_2_sl_y_max"),
    beaker_summaries_for_all_experiments = analyses$beaker_summaries,
    food_t_test_df = food_t_test_df,
    row = "bottom"
  )

  # Now assemble each "row" of plots to make one big figure.
  bl_grob <- ggplot2::ggplotGrob(bl_plot)
  po_grob <- ggplot2::ggplotGrob(po_plot)
  sl_grob <- ggplot2::ggplotGrob(sl_plot)
  plot_grid <- rbind(bl_grob, po_grob, sl_grob, size = "first")
  grid::grid.newpage()
  grid::grid.draw(plot_grid)
  plot_grid
}



# Carry out t-tests between food levels of a given density/experiment.
#
# Parameters
# ----------
# beaker_summaries_for_all_experiments: data.frame
#   Data frame with per-beaker summaries for all experiments.
#
# Returns
# -------
# data.frame
#   Data frame organizing t-tests.
food_t_tests <- function(beaker_summaries_for_all_experiments) {
  # We need to do a bunch of t-tests (33 to be precise) to compare the two food
  # levels within each density within each experiment for each dependent variable.
  # First we will build a list of lists, where each list is a row.
  # Then we will convert this to a data frame.
  food_t_test_list <- list()
  i <- 1
  for (exp in unique(beaker_summaries_for_all_experiments$experiment))
  {
    data_in_experiment <-
      beaker_summaries_for_all_experiments %>%
      dplyr::filter(.data$experiment == exp)
    for (dns in unique(data_in_experiment$density))
    {
      data_in_density <-
        data_in_experiment %>%
        dplyr::filter(.data$density == dns)
      for (dependent_variable in c("bl", "po", "sl"))
      {
        high <- (data_in_density %>%
          dplyr::filter(.data$food == settings("food_level_high")))[[dependent_variable]]
        low <- (data_in_density %>%
          dplyr::filter(.data$food == settings("food_level_low")))[[dependent_variable]]
        t_result <- stats::t.test(high, low)
        food_t_test_list[[i]] <-
          list(
            experiment = exp,
            density = dns,
            dependent_variable = dependent_variable,
            group1 = settings("food_level_high"),
            group2 = settings("food_level_low"),
            p = t_result$p.value,
            max_mean = max(mean(high), mean(low))
          )
        i <- i + 1
      }
    }
  }

  # Make data.frame.
  food_t_test_df <- do.call(rbind.data.frame, food_t_test_list)
  food_t_test_df %>%
    dplyr::mutate(
      p.round = round(.data$p, settings("p_value_nsmall")),
      stars = p_stars_no_dot(.data$p),
      p.text = p_min(.data$p),
      y.position = .data$max_mean * settings("figure_2_star_height_mult")
    )
}



# Make a faceted plot for PORL, SL, or BL.
#
# Parameters
# ----------
# y: character
#   y-axis variable.
# y_label: character
#   Name to display for y-axis variable.
# y_limit_max: numeric
#   Cut-off for y-axis. Warning: will discard data out of bounds.
# beaker_summaries_for_all_experiments: data.frame
#   Data frame with all per-beaker summaries.
# food_t_test_df: data.frame
#   Data frame of t-tests for the effect of food.
# row: character
#   One of "top", "middle" or "bottom"; the row of the three rows of facets.
#
# Returns
# -------
# ggplot
#   Facetted plot.
plot_dependent_variable <- function(y,
                                    y_label,
                                    y_limit_max,
                                    beaker_summaries_for_all_experiments,
                                    food_t_test_df,
                                    row) {
  if (row == "top") {
    show_x_label <- F
    show_x_axis_title <- F
    show_legend <- T
  } else if (row == "middle") {
    show_x_label <- F
    show_x_axis_title <- F
    show_legend <- F
  } else if (row == "bottom") {
    show_x_label <- T
    show_x_axis_title <- T
    show_legend <- F
  } else {
    stop(paste0("Unexpected plot row name \"", row, "\"."))
  }

  p <-
    ggpubr::ggbarplot(
      beaker_summaries_for_all_experiments,
      x = "density",
      y = y,
      fill = "food",
      facet.by = "experiment",
      add = "mean_se_",
      add.params = list(color = settings("error_bar_color")),
      position = ggplot2::position_dodge(
        width = settings("figure_2_bar_dodge")
      ),
      nrow = 1
    )

  if (show_x_label) {
    p <- p + ggplot2::xlab(settings("density_label_legend"))
  }

  p <- p +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_limit_max), # Stop stars at top from being cut off.
      expand = c(0, 0)
    ) + # Start bottom of bar at x-axis.
    food_fill() +
    density_x_labels() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = settings("food_label_legend"))) +
    ggpubr::stat_pvalue_manual(
      food_t_test_df %>%
        dplyr::filter(.data$dependent_variable == y),
      x = "density",
      label = "stars",
      position = ggplot2::position_dodge(settings("figure_2_bar_dodge"))
    ) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(),
      panel.grid.minor.y = ggplot2::element_line()
    )

  if (!show_x_axis_title) {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}
