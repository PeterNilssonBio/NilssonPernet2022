# Create HTML table for Table 2.
#
# This table shows post-hoc contrasts for LMEMs where there was a significant
# effect of density in the ANOVA
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
render_table_2 <- function(analyses) {
  make_sub_table <- function(lmem_for_one_experiment,
                             experiment,
                             variable) {
    f <- function() {
      summary(
        multcomp::glht(
          lmem_for_one_experiment,
          linfct = multcomp::mcp(density = "Tukey")
        )
      )
    }

    if (settings("warn_multcomp")) {
      s <- f()
    } else {
      suppressWarnings(
        s <- f()
      ) # Message: covariate interactions found -- default contrast might be inappropriate
    }


    Estimate <- s$test$coefficients
    SE <- s$test$sigma
    z <- s$test$tstat
    P <- s$test$pvalues
    cbind(
      Estimate,
      SE,
      z,
      P
    ) %>%
      as.data.frame() %>%
      create_post_hoc_table(experiment, variable)
  }

  is_density_significant_in_anova <- function(one_anova) {
    one_anova["density", "Pr(>F)"] <= settings("anova_p")
  }

  make_tables_for_one_variable <- function(lmems, lmem_anovas, variable) {
    names_with_significant_density <- list()
    lmems_with_signficant_density <- list()
    for (i in 1:length(lmem_anovas)) {
      if (is_density_significant_in_anova(lmem_anovas[[i]])) {
        names_with_significant_density <- extend_vector(names_with_significant_density, names(lmems)[[i]])
        lmems_with_signficant_density <- extend_vector(lmems_with_signficant_density, lmems[[i]])
      }
    }

    tables <-
      mapply(
        function(lmem, name) {
          make_sub_table(
            lmem,
            name,
            variable
          )
        },
        lmems_with_signficant_density,
        names_with_significant_density,
        # Prevent mapply() from ruining everything.
        SIMPLIFY = F
      )
  }

  po_tables <- make_tables_for_one_variable(
    analyses$lmems$po,
    analyses$lmem_anovas$po,
    settings("po_label_no_units")
  )

  sl_tables <- make_tables_for_one_variable(
    analyses$lmems$sl,
    analyses$lmem_anovas$sl,
    settings("sl_label_no_units")
  )

  joined_table <- combine_tables(c(po_tables, sl_tables))

  joined_table %>%
    render_kable()
}
