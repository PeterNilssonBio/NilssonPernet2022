render_kable <- function(anova_obj, caption = NULL) {
  anova_obj %>%
    kableExtra::kbl(
      format = "html",
      digits = settings("table_digits"),
      caption = caption
    ) %>%
    kableExtra::kable_classic(full_width = F)
}


add_experiment_and_variable_columns <- function(table, experiment, variable) {
  table %>%
    dplyr::mutate(Variable = variable, Experiment = experiment) %>%
    dplyr::relocate(.data$Variable, .data$Experiment)
}


rename_column_with_string <- function(table, new_col_name, old_col_name) {
  table %>%
    dplyr::rename_with(
      function(ignored_arg) {
        new_col_name
      }, tidyselect::all_of(old_col_name)
    )
}


create_post_hoc_table <- function(table, experiment, variable) {
  table %>%
    tibble::rownames_to_column(var = "Contrast") %>%
    # Rename contrasts from "X - Y" to "X/ml vs Y/ml"
    dplyr::mutate(
      Contrast =
        paste0(
          stringr::str_replace(
            .data$Contrast, "\\s?-\\s?", "/ml vs "
          ),
          "/ml"
        )
    ) %>%
    # Add this info to table itself so it is easier to merge these.
    add_experiment_and_variable_columns(experiment, variable) %>%
    # Draw significance stars.
    dplyr::mutate(
      Sig = p_stars_no_dot(.data$P),
      P = p_min(.data$P)
    )
}


prettify_lmem_anova <- function(lmem_anova, experiment, variable) {
  lmem_anova %>%
    # Make column names more succinct.
    dplyr::rename(
      SS = .data$`Sum Sq`,
      MS = .data$`Mean Sq`,
      DFn = .data$`NumDF`,
      DFd = .data$`DenDF`,
      `F` = .data$`F value`,
      P = .data$`Pr(>F)`
    ) %>%
    # Extract these from the rownames so we can give them a header.
    tibble::rownames_to_column(var = "Source") %>%
    # Add this info to table itself so it is easier to merge these.
    add_experiment_and_variable_columns(experiment, variable) %>%
    # Draw significance stars.
    dplyr::mutate(
      Sig = p_stars_no_dot(.data$P),
      P = p_min(.data$P)
    )
}


# Format a PORL/BL or SL/BL ratio ANOVA for display.
#
# Parameters
# ----------
# ratio_anova: anova
#   ANOVA of ratio data.
# experiment: character
#   Label for the experiment.
# variable: character
#   Label for the variable.
#
# Returns
# -------
# data.frame
prettify_ratio_anova <- function(ratio_anova, experiment, variable) {
  ratio_anova %>%
    # Make column names more succinct.
    dplyr::rename(
      SS = .data$`Sum Sq`,
      MS = .data$`Mean Sq`,
      DF = .data$`Df`,
      `F` = .data$`F value`,
      P = .data$`Pr(>F)`
    ) %>%
    # Extract these from the rownames so we can give them a header.
    tibble::rownames_to_column(var = "Source") %>%
    # Get rid of "Residuals" row.
    dplyr::filter(.data$Source != "Residuals") %>%
    # Add this info to table itself so it is easier to merge these.
    dplyr::mutate(Variable = variable, Experiment = experiment) %>%
    dplyr::relocate(.data$Variable, .data$Experiment) %>%
    # Draw significance stars.
    dplyr::mutate(
      Sig = p_stars_no_dot(.data$P),
      P = p_min(.data$P)
    )
}
