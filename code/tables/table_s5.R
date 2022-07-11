# Create HTML table for Table S5.
#
# This table shows TukeyHSD contrasts for the density treatment of PORL/BL and
# SL/BL ratio models. It only includes entries for ANOVAs where density had
# a significant effect in the ANOVA.
#
# Parameters
# ----------
# analyses: list
#   Object returned by analyze().
#
# Returns
# -------
# data.frame
#   Data frame suitable for output with kable.
render_table_S5 <- function(analyses) {
  make_sub_table <- function(aov_for_one_experiment,
                             experiment,
                             variable) {
    t <- stats::TukeyHSD(aov_for_one_experiment, "density")
    as.data.frame(t$density) %>%
      dplyr::rename(
        Difference = .data$diff,
        `Lower bound` = .data$lwr,
        `Upper bound` = .data$upr,
        P = .data$`p adj`
      ) %>%
      create_post_hoc_table(experiment, variable)
  }

  is_density_significant_in_anova <- function(one_anova) {
    one_anova["density", "Pr(>F)"] <= settings("anova_p")
  }

  make_tables_for_one_variable <- function(aovs, anovas, variable) {
    names_with_significant_density <- list()
    aovs_with_signficant_density <- list()
    for (i in 1:length(anovas)) {
      if (is_density_significant_in_anova(anovas[[i]])) {
        names_with_significant_density <- extend_vector(names_with_significant_density, names(aovs)[[i]])
        aovs_with_signficant_density <- extend_vector(aovs_with_signficant_density, aovs[[i]])
      }
    }

    tables <-
      mapply(
        function(aov, name) {
          make_sub_table(
            aov,
            name,
            variable
          )
        },
        aovs_with_signficant_density,
        names_with_significant_density,
        # Prevent mapply() from ruining everything.
        SIMPLIFY = F
      )
  }

  po_tables <- make_tables_for_one_variable(
    analyses$ratio_aovs$po,
    analyses$ratio_anovas$po,
    settings("po_bl_ratio_label")
  )

  sl_tables <- make_tables_for_one_variable(
    analyses$ratio_aovs$sl,
    analyses$ratio_anovas$sl,
    settings("sl_bl_ratio_label")
  )


  joined_table <- combine_tables(c(po_tables, sl_tables))

  joined_table %>%
    render_kable()
}
