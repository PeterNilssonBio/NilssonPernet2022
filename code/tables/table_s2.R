# Create HTML table for Table S2.
#
# This table shows ANOVAS of PORL/BL and SL/BL ratios.
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
render_table_S2 <- function(analyses) {
  make_tables_for_one_variable <- function(anovas, variable) {
    anova_names <- names(anovas)
    tables <-
      mapply(
        function(ratio_anova, name) {
          prettify_ratio_anova(
            ratio_anova,
            name,
            variable
          )
        },
        anovas,
        anova_names,
        # Prevent mapply() from ruining everything.
        SIMPLIFY = F
      )
  }

  po_tables <- make_tables_for_one_variable(
    analyses$ratio_anovas$po,
    settings("po_bl_ratio_label")
  )

  sl_tables <- make_tables_for_one_variable(
    analyses$ratio_anovas$sl,
    settings("sl_bl_ratio_label")
  )

  joined_table <- combine_tables(c(po_tables, sl_tables))

  joined_table %>%
    render_kable()
}
