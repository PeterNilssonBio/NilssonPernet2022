# Return path for a given plot file name.
#
# Parameters
# ----------
# file_name: character
#   Base name of file, eg "fig_1".
#
# Returns
# -------
# character
#   Path to file.
get_figure_output_path <- function(file_name) {
  get_output_path(paste0(file_name, settings("figure_extension")))
}


# Save ggplot2 object representing Figure 2 to location specified in config.yml.
#
# Parameters
# ----------
# plot: ggplot
save_fig_2 <- function(plot) {
  path <- get_figure_output_path(settings("figure_2_name"))
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    units = "mm",
    width = settings("figure_2_width_mm"),
    height = settings("figure_2_height_mm")
  )
}


# Save ggplot2 object representing Figure 3 to location specified in config.yml.
#
# Parameters
# ----------
# plot: ggplot
save_fig_3 <- function(plot) {
  path <- get_figure_output_path(settings("figure_3_name"))
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    units = "mm",
    width = settings("figure_3_width_mm"),
    height = settings("figure_3_height_mm")
  )
}


# Save ggplot2 object representing Figure 4 to location specified in config.yml.
#
# Parameters
# ----------
# plot: ggplot
save_fig_4 <- function(plot) {
  path <- get_figure_output_path(settings("figure_4_name"))
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    units = "mm",
    width = settings("figure_4_width_mm"),
    height = settings("figure_4_height_mm")
  )
}


# Save ggplot2 object representing Figure 5 to location specified in config.yml.
#
# Parameters
# ----------
# plot: ggplot
save_fig_5 <- function(plot) {
  path <- get_figure_output_path(settings("figure_5_name"))
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    units = "mm",
    width = settings("figure_5_width_mm"),
    height = settings("figure_5_height_mm")
  )
}


# Save ggplot2 object representing Figure S1 to location specified in config.yml.
#
# Parameters
# ----------
# plot: ggplot
save_fig_S1 <- function(plot) {
  path <- get_figure_output_path(settings("figure_S1_name"))
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    units = "mm",
    width = settings("figure_S1_width_mm"),
    height = settings("figure_S1_height_mm")
  )
}
