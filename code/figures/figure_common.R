# This file contains logic used by multiple figure creation functions.

# Get the name that emmeans::contrast will use to high vs low food within the
# 0.015 larvae/ml density.
#
# Returns
# -------
# character
extrasparse_contrast_name <- function() {
  paste0(
    settings("density_level_extrasparse"), " ", settings("food_level_low"),
    " - ",
    settings("density_level_extrasparse"), " ", settings("food_level_high")
  )
}


# Get the name that emmeans::contrast will use to high vs low food within the
# 0.05 larvae/ml density.
#
# Returns
# -------
# character
sparse_contrast_name <- function() {
  paste0(
    settings("density_level_sparse"), " ", settings("food_level_low"),
    " - ",
    settings("density_level_sparse"), " ", settings("food_level_high")
  )
}


# Get the name that emmeans::contrast will use to high vs low food within the
# 0.25 larvae/ml density.
#
# Returns
# -------
# character
dense_contrast_name <- function() {
  paste0(
    settings("density_level_dense"), " ", settings("food_level_low"),
    " - ",
    settings("density_level_dense"), " ", settings("food_level_high")
  )
}


# Create a fill scale for food ration.
#
# Returns
# -------
# Scale
food_fill <- function() {
  ggplot2::scale_fill_manual(
    values =
      named_vector(
        c(settings("food_level_low"), settings("food_level_high")),
        c(settings("food_color_low"), settings("food_color_high"))
      ),
    labels =
      named_vector(
        c(settings("food_level_low"), settings("food_level_high")),
        c(settings("food_label_low"), settings("food_label_high"))
      )
  )
}


# Create a color scale for food ration.
#
# Returns
# -------
# Scale
food_color <- function() {
  ggplot2::scale_color_manual(
    values =
      named_vector(
        c(settings("food_level_low"), settings("food_level_high")),
        c(settings("food_color_low"), settings("food_color_high"))
      ),
    labels =
      named_vector(
        c(settings("food_level_low"), settings("food_level_high")),
        c(settings("food_label_low"), settings("food_label_high"))
      )
  )
}


# Create discrete x scale to label density levels.
#
# Returns
# -------
# Scale
density_x_labels <- function() {
  ggplot2::scale_x_discrete(
    limits = c(
      settings("density_level_extrasparse"),
      settings("density_level_sparse"),
      settings("density_level_dense")
    ),
    labels =
      named_vector(
        c(
          settings("density_level_extrasparse"),
          settings("density_level_sparse"),
          settings("density_level_dense")
        ),
        c(
          settings("density_label_extrasparse"),
          settings("density_label_sparse"),
          settings("density_label_dense")
        )
      )
  )
}


# Create shape scale to represent density levels.
#
# Returns
# -------
# Scale
density_shape <- function() {
  ggplot2::scale_shape_manual(
    limits = c(
      settings("density_level_extrasparse"),
      settings("density_level_sparse"),
      settings("density_level_dense")
    ),
    values =
      named_vector(
        c(
          settings("density_level_extrasparse"),
          settings("density_level_sparse"),
          settings("density_level_dense")
        ),
        c(
          settings("density_shape_extrasparse"),
          settings("density_shape_sparse"),
          settings("density_shape_dense")
        )
      ),
    labels =
      named_vector(
        c(
          settings("density_level_extrasparse"),
          settings("density_level_sparse"),
          settings("density_level_dense")
        ),
        c(
          settings("density_label_extrasparse"),
          settings("density_label_sparse"),
          settings("density_label_dense")
        )
      )
  )
}


# == EMM BARPLOTS - FIGURES 3-4 == #

# Shared plotting logic for Fig 3-4.
#
# This creates a bar plot of estimated marginal means.
#
# Parameters
# ----------
# emm_for_one_dependent_variable: emmGrid
#   Merged EMMs for all experiments.
# named_emm_dfs: list
#   List of EMMs as data frames.
# named_emm_contrasts_dfs:
#   List of data frames of contrasts between EMMS.
# y_label: character
#   Name for y-axis.
# y_limit_max: numeric
#   Maximum height of y-axis.
#
# Returns
# -------
# ggplot
plot_emm_barplot <- function(emm_for_one_dependent_variable,
                             named_emm_dfs,
                             named_emm_contrasts_dfs,
                             y_label,
                             y_limit_max) {
  plottable_contrasts <- make_plottable_emm_contrasts_df(
    named_emm_dfs,
    named_emm_contrasts_dfs
  )
  p <-
    ggpubr::ggbarplot(emm_for_one_dependent_variable,
      x = "density",
      y = "emmean",
      fill = "food",
      facet.by = "experiment",
      position = ggplot2::position_dodge(
        width = settings("figure_3_4_bar_dodge")
      ),
      nrow = 1
    )

  # This call to ggplot2::geom_errorbar is wrapped in suppressWarnings to
  # suppress an incorrect warning. It claims that it ignores the fill aesthetic.
  # It does not ignore it; in fact, it behaves incorrectly if fill is not
  # specified - the error bars get drawn in between each pair of bars rather
  # than over them.
  suppressWarnings(
    p <-
      p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data$emmean - .data$SE,
          ymax = .data$emmean + .data$SE,
          x = .data$density,
          fill = .data$food
        ),
        width = settings("figure_3_4_error_width"),
        position = ggplot2::position_dodge(width = settings("figure_3_4_bar_dodge")),
        color = settings("error_bar_color")
      )
  )

  p <-
    p +
    ggplot2::xlab(settings("density_label_legend")) +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_limit_max), # Stop p-values at top from being cut off.
      expand = c(0, 0)
    ) + # Start bottom of bar at x-axis.
    food_fill() +
    density_x_labels() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = settings("food_label_legend"))) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(),
      panel.grid.minor.y = ggplot2::element_line()
    ) +
    ggpubr::stat_pvalue_manual(
      plottable_contrasts,
      x = "density",
      label = "p.text",
      label.size = settings("p_value_font_size"),
      position = ggplot2::position_dodge(settings("figure_3_4_bar_dodge"))
    )
}


# Prepare a data frame for use in ggpubr::stat_pvalue_manual.
make_plottable_emm_contrasts_df <- function(named_emm_dfs, named_emm_contrasts_dfs) {
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
      (named_emm_contrasts_dfs[[settings("experiment_level_dend1")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_dend1")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_dend2")]] %>%
        dplyr::filter(.data$contrast == extrasparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_dend2")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_dend2")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_lyt1")]] %>%
        dplyr::filter(.data$contrast == extrasparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_lyt1")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_lyt1")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_lyt2")]] %>%
        dplyr::filter(.data$contrast == extrasparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_lyt2")]] %>%
        dplyr::filter(.data$contrast == sparse_contrast_name()))$p.value %>% dplyr::first(),
      (named_emm_contrasts_dfs[[settings("experiment_level_lyt2")]] %>%
        dplyr::filter(.data$contrast == dense_contrast_name()))$p.value %>% dplyr::first()
    ),
    y.position = c(
      max((named_emm_dfs[[settings("experiment_level_dend1")]] %>%
        filter_density(settings("density_level_sparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_dend1")]] %>%
        filter_density(settings("density_level_dense")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_dend2")]] %>%
        filter_density(settings("density_level_extrasparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_dend2")]] %>%
        filter_density(settings("density_level_sparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_dend2")]] %>%
        filter_density(settings("density_level_dense")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_lyt1")]] %>%
        filter_density(settings("density_level_extrasparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_lyt1")]] %>%
        filter_density(settings("density_level_sparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_lyt1")]] %>%
        filter_density(settings("density_level_dense")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_lyt2")]] %>%
        filter_density(settings("density_level_extrasparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_lyt2")]] %>%
        filter_density(settings("density_level_sparse")))$emmean),
      max((named_emm_dfs[[settings("experiment_level_lyt2")]] %>%
        filter_density(settings("density_level_dense")))$emmean)
    )
  ) %>%
    dplyr::mutate(
      p = round(.data$p, settings("p_value_nsmall")),
      stars = p_stars_no_dot(.data$p),
      p.text = p_min(.data$p),
      y.position = .data$y.position * settings("figure_3_4_p_height_mult")
    )
}
