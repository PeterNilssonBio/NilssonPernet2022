# Create HTML table for Table 1.
#
# This table shows ANOVAS of LMEMs of PORL and SL.
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
render_table_1 <- function(analyses) {
  po_lmem_names <- names(analyses$lmem_anovas$po)
  po_tables <-
    mapply(
      function(lmem_anova, name) {
        prettify_lmem_anova(
          lmem_anova,
          name,
          settings("po_label_no_units")
        )
      },
      analyses$lmem_anovas$po,
      po_lmem_names,
      # Prevent mapply() from ruining everything.
      SIMPLIFY = F
    )

  sl_lmem_names <- names(analyses$lmem_anovas$sl)
  sl_tables <-
    mapply(
      function(lmem_anova, name) {
        prettify_lmem_anova(
          lmem_anova,
          name,
          settings("sl_label_no_units")
        )
      },
      analyses$lmem_anovas$sl,
      sl_lmem_names,
      # Prevent mapply() from ruining everything.
      SIMPLIFY = F
    )

  joined_table <- combine_tables(c(po_tables, sl_tables))

  joined_table %>%
    render_kable()
}
