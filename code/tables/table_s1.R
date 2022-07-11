# Create HTML table for Table S1.
#
# This table shows ANOVAS for LMEMs that include BL:food interactions.
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
render_table_S1 <- function(analyses) {
  make_tables_for_one_variable <- function(lmems, lmem_anovas, variable, candidates) {
    names_with_density_bl_interaction <- list()
    lmems_anovas_with_density_bl_interaction <- list()
    for (i in 1:length(lmem_anovas)) {
      if (names(lmems)[[i]] %in% candidates) {
        names_with_density_bl_interaction <-
          extend_vector(names_with_density_bl_interaction, names(lmems)[[i]])
        lmems_anovas_with_density_bl_interaction <-
          extend_vector(lmems_anovas_with_density_bl_interaction, lmem_anovas[[i]])
      }
    }

    tables <-
      mapply(
        function(lmem, name) {
          prettify_lmem_anova(
            lmem,
            name,
            variable
          )
        },
        lmems_anovas_with_density_bl_interaction,
        names_with_density_bl_interaction,
        # Prevent mapply() from ruining everything.
        SIMPLIFY = F
      )
  }

  # These are the models where these interactions remained significant
  # even when all other terms (beyond those in the basic LMEMs)
  # were removed. We don't care about showing the rest.
  # TODO: derive these lists automatically.
  po_experiments_to_show <- c(settings("experiment_level_lyt2"))
  sl_experiments_to_show <- c(
    settings("experiment_level_dend2"),
    settings("experiment_level_lyt2")
  )

  po_tables <- make_tables_for_one_variable(
    analyses$lmems_with_density_bl_interaction$po,
    analyses$lmem_with_density_bl_interaction_anovas$po,
    settings("po_label_no_units"),
    po_experiments_to_show
  )

  sl_tables <- make_tables_for_one_variable(
    analyses$lmems_with_density_bl_interaction$sl,
    analyses$lmem_with_density_bl_interaction_anovas$sl,
    settings("sl_label_no_units"),
    sl_experiments_to_show
  )

  joined_table <- combine_tables(c(po_tables, sl_tables))

  joined_table %>%
    render_kable()
}
