# This file contains functions for accessing settings derived from
# config.yml. Its main purpose is to make debugging easier by
# failing immediately if a property is missing, rather than silently
# returning NULL and letting things go wrong at some point way later
# in the program execution.

# Global settings object. Access through settings() function.
.settings <- list()

# List of all properties to load from config.yml.
get_settings_property_names <- function() {
  c(
    "data_directory",
    "output_directory",
    "morphometrics_name",
    "count_control_name",
    "data_extension",
    "figure_2_name",
    "figure_3_name",
    "figure_4_name",
    "figure_5_name",
    "figure_S1_name",
    "figure_extension",
    "table_1_name",
    "table_2_name",
    "table_S1_name",
    "table_S2_name",
    "table_S3_name",
    "table_S4_name",
    "table_S5_name",
    "count_table_name",
    "table_extension",
    "database_path_prefix",
    "database_path_suffix",
    "download_morphometrics",
    "download_count_control",
    "overwrite_on_download",
    "morphometrics_min_expected_bytes",
    "count_control_min_expected_bytes",
    "make_figure_2",
    "make_figure_3",
    "make_figure_4",
    "make_figure_5",
    "make_figure_S1",
    "make_table_1",
    "make_table_2",
    "make_table_S1",
    "make_table_S2",
    "make_table_S3",
    "make_table_S4",
    "make_table_S5",
    "make_count_table",
    "postprocess",
    "postprocess_suffix",
    "postprocess_soup",
    "col_spawn_date",
    "col_species",
    "col_experiment",
    "col_beaker",
    "col_density",
    "col_food",
    "col_bl",
    "col_po",
    "col_sl",
    "col_age",
    "col_stage",
    "col_image",
    "col_counted",
    "col_expected",
    "species_level_dend",
    "species_level_lyt",
    "experiment_level_dend1",
    "experiment_level_dend2",
    "experiment_level_lyt1",
    "experiment_level_lyt2",
    "density_level_extrasparse",
    "density_level_sparse",
    "density_level_dense",
    "food_level_low",
    "food_level_high",
    "shapiro_p",
    "levene_p",
    "anova_p",
    "spawn_date_label_col_header",
    "proportion_label_col_header",
    "species_label_col_header",
    "experiment_label_col_header",
    "experiment_label_dend1",
    "experiment_label_dend2",
    "experiment_label_lyt1",
    "experiment_label_lyt2",
    "density_label_extrasparse",
    "density_label_sparse",
    "density_label_dense",
    "density_label_legend",
    "density_label_col_header",
    "food_label_low",
    "food_label_high",
    "food_label_legend",
    "food_label_col_header",
    "po_label",
    "po_label_no_units",
    "po_emm_label",
    "po_lmem_label",
    "po_bl_ratio_label",
    "sl_label",
    "sl_label_no_units",
    "sl_emm_label",
    "bl_emm_label",
    "sl_lmem_label",
    "sl_bl_ratio_label",
    "bl_label",
    "food_color_low",
    "food_color_high",
    "density_shape_extrasparse",
    "density_shape_sparse",
    "density_shape_dense",
    "density_shape_size",
    "density_stroke",
    "density_line_color",
    "error_bar_color",
    "figure_2_bl_y_max",
    "figure_2_po_y_max",
    "figure_2_sl_y_max",
    "figure_3_y_max",
    "figure_4_y_max",
    "figure_S1_po_bl_ratio_y_max",
    "figure_S1_sl_bl_ratio_y_max",
    "figure_2_bar_dodge",
    "figure_3_4_bar_dodge",
    "figure_S1_bar_dodge",
    "figure_2_star_height_mult",
    "figure_3_4_p_height_mult",
    "figure_S1_height_mult",
    "figure_3_4_error_width",
    "figure_S1_error_width",
    "figure_5_legend_spacing_mm",
    "figure_2_width_mm",
    "figure_2_height_mm",
    "figure_3_width_mm",
    "figure_3_height_mm",
    "figure_4_width_mm",
    "figure_4_height_mm",
    "figure_5_width_mm",
    "figure_5_height_mm",
    "figure_S1_width_mm",
    "figure_S1_height_mm",
    "p_value_font_size",
    "p_value_nsmall",
    "p_value_max_cutoff",
    "p_value_min_cutoff",
    "table_digits",
    "p_star_cut_triple",
    "p_star_cut_double",
    "p_star_cut_single",
    "warn_singular",
    "warn_multcomp",
    "break_on_warning"
  )
}


# Only to be called from load_settings.
define_setting <- function(property, value) {
  .settings[[property]] <<- value
}


# Return a property value previously parsed from config.yml.
#
# Parameters:
#   property - name of property as spelled in config.yml.
# Returns: value whose type depends on the property
# Error: if no such property exists.
# NB: Property must be defined in both config.yml and in the function
# get_settings_property_names() for it to be retrieved.
settings <- function(property) {
  value <- .settings[[property]]
  if (is.null(value)) {
    stop(paste0("No value for property \"", property, "\" in settings."))
  }
  value
}
