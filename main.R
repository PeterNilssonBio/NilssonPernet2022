# This is the top-level file that runs all other code.
#
# To create all the figures and tables, run this line in R:
#   source("main.R")
#
# TIPS:
# 1.  If you wish to customize the output, edit the settings in config.yml.
#
# 2.  This script expects the following data files to exist:
#     - data/density_morphometrics.csv
#     - data/density_counts.csv
#
#     These are not included with the code as they will live in the BCO-DMO
#     database. If these files are missing, the script will attempt to download
#     them for you automatically.
#
# 3.  If running this file fails due to missing software (R or Python library
#     not installed, no Python installation) you can try running the following:
#       install_dependencies()
#
#     That will try to install all the dependencies I think are necessary for
#     this script to work. I have not thoroughly tested whether that will work,
#     since I already have all that installed.

main <- function(quietly = F) {
  figures_made <<- 0
  tables_made <<- 0
  files_postprocessed <<- 0

  source("code/setup.R", chdir = T)
  load_libraries(quietly = T)
  load_r_code(quietly)
  load_python_code(quietly)
  load_settings(quietly)

  if (settings("break_on_warning")) {
    options(warn = 2, error = "recover")
  } else {
    options(warn = 1)
  }

  # Stop dplyr::summarize from printing annoying messages. The only other way to
  # silence these is with the .groups argument, which seems iffy considering
  # it is marked as an "experimental" API so may cause problems for some users.
  options("dplyr.summarise.inform" = F)

  morphometrics_data <- load_morphometrics_data(
    download_if_missing = settings("download_morphometrics"),
    quietly
  )
  count_control_data <- load_count_control_data(
    download_if_missing = settings("download_count_control"),
    quietly
  )
  create_output_directory(quietly)

  analyses <- analyze(morphometrics_data, count_control_data, quietly)

  if (!quietly) {
    cat(paste0(
      "Saving output files to directory: ",
      get_output_path(),
      "\n"
    ))
  }

  main_create_tables(analyses, quietly)
  main_create_figures(analyses, quietly)
  main_postprocess(quietly)

  cat(paste0(
    "Done. Created ", figures_made, " figures and ", tables_made,
    " tables. Postprocessed ", files_postprocessed, " files.\n"
  ))
}


main_create_tables <- function(analyses, quietly) {
  if (settings("make_table_1")) {
    timed_subroutine(
      "Generating Table 1...         ", function() {
        save_table_1(render_table_1(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_table_2")) {
    timed_subroutine(
      "Generating Table 2...         ", function() {
        save_table_2(render_table_2(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_table_S1")) {
    timed_subroutine(
      "Generating Table S1...        ", function() {
        save_table_S1(render_table_S1(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_table_S2")) {
    timed_subroutine(
      "Generating Table S2...        ", function() {
        save_table_S2(render_table_S2(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_table_S3")) {
    timed_subroutine(
      "Generating Table S3...        ", function() {
        save_table_S3(render_table_S3(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_table_S4")) {
    timed_subroutine(
      "Generating Table S4...        ", function() {
        save_table_S4(render_table_S4(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_table_S5")) {
    timed_subroutine(
      "Generating Table S5...        ", function() {
        save_table_S5(render_table_S5(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }

  if (settings("make_count_table")) {
    timed_subroutine(
      "Generating Count Table...     ", function() {
        save_count_control_table(render_count_control_table(analyses))
      },
      quietly
    )
    tables_made <<- tables_made + 1
  }
}


main_create_figures <- function(analyses, quietly) {
  if (settings("make_figure_2")) {
    timed_subroutine(
      "Generating Figure 2...        ", function() {
        save_fig_2(plot_fig_2(analyses))
      },
      quietly
    )
    figures_made <<- figures_made + 1
  }

  if (settings("make_figure_3")) {
    timed_subroutine(
      "Generating Figure 3...        ", function() {
        save_fig_3(plot_fig_3(analyses))
      },
      quietly
    )
    figures_made <<- figures_made + 1
  }

  if (settings("make_figure_4")) {
    timed_subroutine(
      "Generating Figure 4...        ", function() {
        save_fig_4(plot_fig_4(analyses))
      },
      quietly
    )
    figures_made <<- figures_made + 1
  }

  if (settings("make_figure_5")) {
    timed_subroutine(
      "Generating Figure 5...        ", function() {
        save_fig_5(plot_fig_5(analyses))
      },
      quietly
    )
    figures_made <<- figures_made + 1
  }

  if (settings("make_figure_S1")) {
    timed_subroutine(
      "Generating Figure S1...       ", function() {
        save_fig_S1(plot_fig_S1(analyses))
      },
      quietly
    )
    figures_made <<- figures_made + 1
  }
}


main_postprocess <- function(quietly) {
  use_bs4 <- settings("postprocess_soup")

  if (settings("make_table_1")) {
    timed_subroutine(
      "Postprocessing Table 1...     ", function() {
        postprocess_table(
          name = "1",
          source = get_table_output_path(settings("table_1_name")),
          destination = get_table_output_path(paste0(
            settings("table_1_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_table_2")) {
    timed_subroutine(
      "Postprocessing Table 2...     ", function() {
        postprocess_table(
          name = "2",
          source = get_table_output_path(settings("table_2_name")),
          destination = get_table_output_path(paste0(
            settings("table_2_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_table_S1")) {
    timed_subroutine(
      "Postprocessing Table S1...    ", function() {
        postprocess_table(
          name = "S1",
          source = get_table_output_path(settings("table_S1_name")),
          destination = get_table_output_path(paste0(
            settings("table_S1_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_table_S2")) {
    timed_subroutine(
      "Postprocessing Table S2...    ", function() {
        postprocess_table(
          name = "S2",
          source = get_table_output_path(settings("table_S2_name")),
          destination = get_table_output_path(paste0(
            settings("table_S2_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_table_S3")) {
    timed_subroutine(
      "Postprocessing Table S3...    ", function() {
        postprocess_table(
          name = "S3",
          source = get_table_output_path(settings("table_S3_name")),
          destination = get_table_output_path(paste0(
            settings("table_S3_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_table_S4")) {
    timed_subroutine(
      "Postprocessing Table S4...    ", function() {
        postprocess_table(
          name = "S4",
          source = get_table_output_path(settings("table_S4_name")),
          destination = get_table_output_path(paste0(
            settings("table_S4_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_table_S5")) {
    timed_subroutine(
      "Postprocessing Table S5...    ", function() {
        postprocess_table(
          name = "S5",
          source = get_table_output_path(settings("table_S5_name")),
          destination = get_table_output_path(paste0(
            settings("table_S5_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_count_table")) {
    timed_subroutine(
      "Postprocessing Count Table... ", function() {
        postprocess_table(
          name = "CC",
          source = get_table_output_path(settings("count_table_name")),
          destination = get_table_output_path(paste0(
            settings("count_table_name"),
            settings("postprocess_suffix")
          )),
          rearrange = use_bs4
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_figure_2")) {
    timed_subroutine(
      "Postprocessing Figure 2...    ", function() {
        postprocess_figure(
          source = get_figure_output_path(settings("figure_2_name")),
          destination = get_figure_output_path(paste0(
            settings("figure_2_name"),
            settings("postprocess_suffix")
          ))
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_figure_3")) {
    timed_subroutine(
      "Postprocessing Figure 3...    ", function() {
        postprocess_figure(
          source = get_figure_output_path(settings("figure_3_name")),
          destination = get_figure_output_path(paste0(
            settings("figure_3_name"),
            settings("postprocess_suffix")
          ))
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_figure_4")) {
    timed_subroutine(
      "Postprocessing Figure 4...    ", function() {
        postprocess_figure(
          source = get_figure_output_path(settings("figure_4_name")),
          destination = get_figure_output_path(paste0(
            settings("figure_4_name"),
            settings("postprocess_suffix")
          ))
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_figure_5")) {
    timed_subroutine(
      "Postprocessing Figure 5...    ", function() {
        postprocess_figure(
          source = get_figure_output_path(settings("figure_5_name")),
          destination = get_figure_output_path(paste0(
            settings("figure_5_name"),
            settings("postprocess_suffix")
          ))
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }

  if (settings("make_figure_S1")) {
    timed_subroutine(
      "Postprocessing Figure S1...   ", function() {
        postprocess_figure(
          source = get_figure_output_path(settings("figure_S1_name")),
          destination = get_figure_output_path(paste0(
            settings("figure_S1_name"),
            settings("postprocess_suffix")
          ))
        )
      },
      quietly
    )
    files_postprocessed <<- files_postprocessed + 1
  }
}


main()
