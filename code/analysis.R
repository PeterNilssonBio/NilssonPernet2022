# Perform all analyses.
#
# This function produces a list holding all the analyses needed to create
# all plots and tables.
#
# This simplifies the task of passing arguments to the various plot and figure
# rendering functions, and makes it easier to avoid duplicating work. It also
# makes it easier to see exactly what we know about the data, all in one place.
#
# Parameters
# ----------
# morphometrics: data.frame
#   Data frame of morphometrics data.
# count_control: data.frame
#   Data frame of count control data.
# quietly: logical
#   T to prevent this function from printing progress to console.
#
# Returns
# -------
# list
#   A list with the following elements:
#
# Morphometrics data and analyses:
#   - morphometrics: data frame of morphometrics data loaded from file.
#   - lmems$po, lmems$sl: Basic LMEMs of PORL and SL, respectively.
#   - lmem_anovas$po, lmem_anovas$sl: ANOVAs run on basic LMEMS.
#   - lmems_with_density_bl_interaction$po,
#     lmems_with_density_bl_interaction$sl: LMEMs including density:bl term.
#   - lmem_with_density_bl_interaction_anovas$po,
#     lmem_with_density_bl_interaction_anovas$sl: ANOVAs run on LMEMS with
#                                                 density:bl terms.
#   - emms$po, emms$sl: EMMs for basic LMEMs
#   - emm_dfs$list$po, emm_dfs$list$sl: Data frames for each EMM
#   - emm_dfs$po, emm_dfs$sl: Data frames of all PORL and all SL EMMs merged,
#                             respectively.
#   - emm_dfs$combined: Single data frame combining all EMMs data frames.
#   - beaker_summaries: Per-beaker summary statistics for morphometrics.
#   - ratio_aovs$po, ratio_aovs$sl: Linear models of PORL/BL and SL/BL.
#   - ratio_anovas$po, ratio_anovas$sl: ANOVAs run on linear models.
#   - emm_contrast_dfs$list$po,
#     emm_contrast_dfs$list$sl: Data frames showing contrasts between factor
#                               levels in EMMs.
#   - ratio_aov_contrast_dfs$po,
#     ratio_aov_contrast_dfs$sl: Data frames showing contrasts between factor
#                                levels in linear models of ratios.
#
# Count control data and analyses:
#   - count_control: Data frame of count control data loaded from file.
#   - count_control_summaries: Per-treatment summary statistics for count-
#                              control data.
# @export
analyze <- function(morphometrics, count_control, quietly = F) {
  analyses <- list()
  analyses$morphometrics <- morphometrics
  analyses$count_control <- count_control

  timed_subroutine(
    "Constructing LMEMs...         ", function() {
      # Basic LMEMs.
      analyses$lmems <<- list()
      analyses$lmems$po <<- po_lmems_for_all_experiments(analyses$morphometrics)
      analyses$lmems$sl <<- sl_lmems_for_all_experiments(analyses$morphometrics)

      analyses$lmem_anovas <<- list()
      analyses$lmem_anovas$po <<- make_named_anovas(analyses$lmems$po)
      analyses$lmem_anovas$sl <<- make_named_anovas(analyses$lmems$sl)

      # LMEMs with density:BL interaction.
      analyses$lmems_with_density_bl_interaction <<- list()
      analyses$lmems_with_density_bl_interaction$po <<-
        po_lmems_with_density_bl_interaction_for_all_experiments(analyses$morphometrics)
      analyses$lmems_with_density_bl_interaction$sl <<-
        sl_lmems_with_density_bl_interaction_for_all_experiments(analyses$morphometrics)

      analyses$lmem_with_density_bl_interaction_anovas <<- list()
      analyses$lmem_with_density_bl_interaction_anovas$po <<-
        make_named_anovas(analyses$lmems_with_density_bl_interaction$po)
      analyses$lmem_with_density_bl_interaction_anovas$sl <<-
        make_named_anovas(analyses$lmems_with_density_bl_interaction$sl)
    },
    quietly
  )

  timed_subroutine(
    "Computing EMMs...             ", function() {
      analyses$emms <<- list()
      analyses$emms$po <<- make_named_emms(analyses$lmems$po)
      analyses$emms$sl <<- make_named_emms(analyses$lmems$sl)

      analyses$emm_dfs <<- list()
      analyses$emm_dfs$list <<- list()
      analyses$emm_dfs$list$po <<- make_named_emm_dfs(analyses$emms$po)
      analyses$emm_dfs$list$sl <<- make_named_emm_dfs(analyses$emms$sl)
      analyses$emm_dfs$po <<- join_emm_dfs(analyses$emm_dfs$list$po)
      analyses$emm_dfs$sl <<- join_emm_dfs(analyses$emm_dfs$list$sl)
      analyses$emm_dfs$combined <<- combine_po_and_sl_emm_df(
        analyses$emm_dfs$po,
        analyses$emm_dfs$sl
      )
    },
    quietly
  )

  timed_subroutine(
    "Computing ratio models...     ", function() {
      analyses$beaker_summaries <<- summarize_beakers(analyses$morphometrics)

      analyses$ratio_aovs <<- list()
      analyses$ratio_aovs$po <<- make_named_ratio_aovs(
        analyses$beaker_summaries,
        "po.rel"
      )
      analyses$ratio_aovs$sl <<- make_named_ratio_aovs(
        analyses$beaker_summaries,
        "sl.rel"
      )

      analyses$ratio_anovas <<- list()
      analyses$ratio_anovas$po <<- make_named_anovas(analyses$ratio_aovs$po)
      analyses$ratio_anovas$sl <<- make_named_anovas(analyses$ratio_aovs$sl)
    },
    quietly
  )

  timed_subroutine(
    "Computing contrasts...        ", function() {
      analyses$emm_contrast_dfs$list <<- list()
      analyses$emm_contrast_dfs$list$po <<- make_named_emm_contrasts_dfs(analyses$emms$po)
      analyses$emm_contrast_dfs$list$sl <<- make_named_emm_contrasts_dfs(analyses$emms$sl)

      analyses$ratio_aov_contrast_dfs <<- list()
      analyses$ratio_aov_contrast_dfs$po <<- make_named_aov_contrasts_dfs(analyses$ratio_aovs$po)
      analyses$ratio_aov_contrast_dfs$sl <<- make_named_aov_contrasts_dfs(analyses$ratio_aovs$sl)
    },
    quietly
  )

  timed_subroutine(
    "Comparing count control...    ", function() {
      analyses$count_control_summaries <<- summarize_count_control(analyses$count_control)
    },
    quietly
  )

  analyses
}
