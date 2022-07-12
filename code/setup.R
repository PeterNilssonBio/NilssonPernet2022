# This file contains the logic for loading dependencies, settings,
# and data.
# source() this file with chdir = T.

.r_libraries <- c(
  "config",
  "emmeans",
  "ggpubr",
  "grid",
  "kableExtra",
  "lmerTest",
  "multcomp",
  "reticulate",
  "rstatix",
  "stats",
  "tidyverse",
  "utils"
)

# == LOADING CODE AND DATA FROM DISK == #

load_libraries <- function(quietly = F) {
  for (lib in .r_libraries) {
    .load_libraries <- function() {
      library(lib,
        character.only = T,
        warn.conflicts = !quietly,
        quietly = quietly
      )
    }

    if (quietly) {
      # tidyverse doesn't honor warn.conflicts = F or quietly = T, so this is
      # required to keep it from printing an big chunk of useless text each time.
      suppressPackageStartupMessages(
        .load_libraries()
      )
    } else {
      .load_libraries()
    }
  }
}


load_r_code <- function(quietly = F) {
  sources <- c(
    "analysis.R",
    "settings.R",
    "utilities.R",
    "analysis/anovas/lmem_anovas.R",
    "analysis/anovas/ratio_anovas.R",
    "analysis/contrasts/emm_contrasts.R",
    "analysis/contrasts/ratio_contrasts.R",
    "analysis/estimates/emms.R",
    "analysis/models/basic_lmems.R",
    "analysis/models/interaction_lmems.R",
    "analysis/models/ratios.R",
    "analysis/summaries/count_control_summaries.R",
    "analysis/summaries/morphometrics_summaries.R",
    "figures/figure_2.R",
    "figures/figure_3.R",
    "figures/figure_4.R",
    "figures/figure_5.R",
    "figures/figure_common.R",
    "figures/figure_output.R",
    "figures/figure_s1.R",
    "tables/table_1.R",
    "tables/table_2.R",
    "tables/table_common.R",
    "tables/table_count_control.R",
    "tables/table_output.R",
    "tables/table_s1.R",
    "tables/table_s2.R",
    "tables/table_s3.R",
    "tables/table_s4.R",
    "tables/table_s5.R"
  )

  if (!quietly) {
    cat(
      paste0(
        "Looking for ",
        length(sources),
        " R source files in directory: ",
        get_code_path()
      ),
      "\n"
    )
  }

  for (src in sources) {
    source(get_code_path(src), chdir = T)
  }
}


load_python_code <- function(quietly = F) {
  path <- get_code_path("postprocess.py")
  if (!quietly) {
    cat(
      paste0(
        "Looking for postprocessing Python script at file path: ",
        path
      ),
      "\n"
    )
  }


  timed_subroutine(
    "Loading Python script...      ", function() {
      reticulate::source_python(path, envir = globalenv())
    },
    quietly
  )

  if (!quietly) {
    cat(paste0("Using Python version: ", python_version(), "\n"))
  }
}


# settings.R must be sourced before this.
load_settings <- function(quietly = F) {
  path <- get_config_path()
  get_value <- function(property) {
    value <- config::get(property, file = path)
    if (is.null(value)) {
      stop(paste0(
        "No value for property \"", property, "\" in config file \"",
        path, "\"."
      ))
    }
    value
  }

  if (!quietly) {
    cat(
      paste0(
        "Looking for config file at file path: ",
        path
      ),
      "\n"
    )
  }

  timed_subroutine(
    "Reading config file...        ", function() {
      for (property in get_settings_property_names()) {
        define_setting(property, get_value(property))
      }
    },
    quietly
  )
}


# utilities.R must be sourced before this.
load_morphometrics_data <- function(download_if_missing, quietly = F) {
  colClasses <- named_vector(
    c(settings("col_density"), settings("col_food"), settings("col_stage")),
    c("factor", "factor", "factor")
  )

  path <- get_morphometrics_data_path()
  if (!quietly) {
    cat(
      paste0(
        "Looking for morphometrics data at file path: ",
        path
      ),
      "\n"
    )
  }

  if (file.exists(path)) {
    error_if_morphometrics_file_looks_too_small(F)
  } else {
    if (download_if_missing) {
      download_morphometrics(quietly)
    } else {

    }
  }

  result <- NULL
  timed_subroutine(
    "Reading morphometrics data... ", function() {
      result <<- utils::read.csv(path, colClasses = colClasses) %>%
        dplyr::rename(
          species = settings("col_species"),
          experiment = settings("col_experiment"),
          beaker = settings("col_beaker"),
          density = settings("col_density"),
          food = settings("col_food"),
          bl = settings("col_bl"),
          po = settings("col_po"),
          sl = settings("col_sl"),
          age = settings("col_age"),
          stage = settings("col_stage"),
          image = settings("col_image")
        )
    },
    quietly
  )

  result
}


# utilities.R must be sourced before this.
load_count_control_data <- function(download_if_missing, quietly = F) {
  colClasses <- named_vector(
    c(settings("col_density"), settings("col_food")),
    c("factor", "factor")
  )

  path <- get_count_control_data_path()
  if (!quietly) {
    cat(
      paste0(
        "Looking for count control data at file path: ",
        path
      ),
      "\n"
    )
  }

  if (file.exists(path)) {
    error_if_count_control_file_looks_too_small(F)
  } else {
    if (download_if_missing) {
      download_count_control(quietly)
    } else {

    }
  }

  result <- NULL
  timed_subroutine(
    "Reading count control data... ", function() {
      result <<- utils::read.csv(path, colClasses = colClasses) %>%
        dplyr::rename(
          spawn_date = settings("col_spawn_date"),
          species = settings("col_species"),
          experiment = settings("col_experiment"),
          beaker = settings("col_beaker"),
          density = settings("col_density"),
          food = settings("col_food"),
          age = settings("col_age"),
          counted = settings("col_counted"),
          expected = settings("col_expected")
        )
    },
    quietly
  )

  result
}


# Create the output directory for plots and figures specified in config.yml.
create_output_directory <- function(quietly = F) {
  path <- get_output_path()
  created_new <- dir.create(path, showWarnings = F)
  if (!quietly && created_new) {
    cat(paste0("Created output directory \"", path, "\".\n"))
  }
}


# == LOADING FILES FROM THE INTERNET == #

# Attempt to install all needed software.
# Not at all tested!
install_dependencies <- function() {
  for (lib in .r_libraries) {
    utils::install.packages(lib)
  }

  reticulate::install_python("3:latest")
  reticulate::py_install("beautifulsoup4")
}


# Download the morphometrics data from BCO-DMO database, overwriting current file.
#
# Note that this function has not been tested, as we do not yet know the actual
# URL of the data file and this is just a guess.
download_morphometrics <- function(quietly = F) {
  if (!quietly) {
    cat(paste0(
      "Looking for morphometrics data online at: ",
      get_database_url(settings("morphometrics_name")),
      "\n"
    ))
  }

  timed_subroutine(
    "Downloading morphometrics...  ", function() {
      download_from_database(settings("morphometrics_name"),
        get_morphometrics_data_path(),
        clobber = settings("overwrite_on_download")
      )
    },
    quietly
  )

  error_if_morphometrics_file_looks_too_small(T)
}


# Heuristically check whether the alleged morphometrics CSV is likely just an
# error response page from the server.
#
# The expected size is set in config.yml (morphometrics_min_expected_bytes).
#
# Parameters
# ----------
# was_downloaded: logical
#   T if error message should be phrased to indicate the file was just
#   downloaded as opposed to already being on the disk.
#
# Error
# -----
# stop if file is too small
error_if_morphometrics_file_looks_too_small <- function(was_downloaded) {
  error_if_file_looks_too_small(
    "morphometrics",
    get_morphometrics_data_path(),
    settings("morphometrics_min_expected_bytes"),
    was_downloaded
  )
}


# Download the count control data from BCO-DMO database, overwriting current file.
#
# Note that this function has not been tested, as we do not yet know the actual
# URL of the data file and this is just a guess.
download_count_control <- function(quietly = F) {
  if (!quietly) {
    cat(paste0(
      "Looking for count control data online at: ",
      get_database_url(settings("count_control_name")),
      "\n"
    ))
  }

  timed_subroutine(
    "Downloading count control...  ", function() {
      download_from_database(settings("count_control_name"),
        get_count_control_data_path(),
        clobber = settings("overwrite_on_download")
      )
    },
    quietly
  )

  error_if_count_control_file_looks_too_small(T)
}


# Heuristically check whether the alleged count control CSV is likely just an
# error response page from the server.
#
# The expected size is set in config.yml (count_control_min_expected_bytes).
#
# Parameters
# ----------
# was_downloaded: logical
#   T if error message should be phrased to indicate the file was just
#   downloaded as opposed to already being on the disk.
#
# Error
# -----
# stop if file is too small
error_if_count_control_file_looks_too_small <- function(was_downloaded) {
  error_if_file_looks_too_small(
    "count control",
    get_count_control_data_path(),
    settings("count_control_min_expected_bytes"),
    was_downloaded
  )
}


# Download a file from the BCO-DMO database.
#
# Parameters
# ----------
# source_file_no_extenstion: character
#   Base name of the file, e.g., "morphometrics"
# dest_path: character
#   Location to save file to.
# clobber: logical
#   Whether to overwrite the file at dest_path if it exists.
download_from_database <- function(source_file_no_extenstion, dest_path, clobber = F) {
  create_data_directory()
  url <- get_database_url(source_file_no_extenstion)
  if (!clobber && file.exists(dest_path)) {
    warning(paste0("File \"", dest_path, "\" already exists; skipped download."))
  } else {
    utils::download.file(url,
      destfile = dest_path,
      method = "auto"
    )
  }
}


# Get (guess) the URL from which a data set should be downloaded.
#
# Parameters
# ----------
# source_file_no_extenstion: character
#   Base name of the file, e.g., "morphometrics"
#
# Returns
# -------
# character
#   Inferred URL for resource.
get_database_url <- function(source_file_no_extenstion) {
  paste0(
    settings("database_path_prefix"),
    source_file_no_extenstion,
    settings("database_path_suffix")
  )
}


# Heuristically check whether we probably just downloaded an error message.
#
# The expected size is set in config.yml (morphometrics_min_expected_bytes).
#
# Parameters
# ----------
# name: character
#   Human-readable name of file.
# path: character
#   Path to the file.
# min_expected_bytes: numeric
#   Minimum bytes to count as a real data file.
# was_downloaded: logical
#   T if error message should be phrased to indicate the file was just
#   downloaded as opposed to already being on the disk.
#
# Error
# -----
# stop if file is too small
error_if_file_looks_too_small <- function(name,
                                          path,
                                          min_expected_bytes,
                                          was_downloaded) {
  # Heuristically determine if we actually got data rather than error response.
  # BCO-DMO sends back error messages of around 216 bytes when the file does
  # not exist. The actual morphometrics data should be tens of kilobytes; the
  # count control data should be a bit under 2 kilobytes.
  actual_size_in_bytes <- file.info(path)$size
  if (actual_size_in_bytes < min_expected_bytes) {
    bytes_to_read_for_preview <- 72
    bytes_not_read <- actual_size_in_bytes - bytes_to_read_for_preview
    preview_truncated <- bytes_not_read > 0
    preview <- readChar(path,
      bytes_to_read_for_preview,
      useBytes = T
    ) # Assuming error response is ASCII, but don't really care.
    msg <- paste0(
      ifelse(was_downloaded,
        "Downloaded ",
        "Loaded "
      ),
      name,
      " file is only ",
      actual_size_in_bytes,
      " bytes; ",
      ifelse(was_downloaded,
        "it is",
        "if it was downloaded automatically, it was"
      ),
      " probably an error response and there is no",
      " such file on the server. The start of the file looks like",
      " this:\n",
      preview,
      ifelse(preview_truncated, paste0(
        "\n[Output truncated; ",
        bytes_not_read,
        " bytes not displayed.]"
      ))
    )

    stop(msg)
  }
}


# Create the directory specified by config.yml to hold downloaded data.
create_data_directory <- function() {
  dir.create(get_data_path(), showWarnings = F)
}


# == PATHS == #
# These variables will only be correct if this file is sourced with chdir = T.
.code_directory <- getwd()
.script_root_directory <- file.path(.code_directory, "..")


absolute_path <- function(path) {
  # Don't warn if path doesn't exist.
  suppressWarnings(
    normalizePath(path) # 'Warning [...] The system cannot find the file specified'
  )
}


get_code_path <- function(file = "") {
  absolute_path(file.path(
    .code_directory,
    file
  ))
}


get_config_path <- function() {
  absolute_path(file.path(.script_root_directory, "config.yml"))
}


# load_settings must be run before this.
get_morphometrics_data_path <- function() {
  absolute_path(file.path(
    .script_root_directory,
    settings("data_directory"),
    paste0(
      settings("morphometrics_name"),
      settings("data_extension")
    )
  ))
}


# load_settingsmust  be run before this.
get_count_control_data_path <- function() {
  absolute_path(file.path(
    .script_root_directory,
    settings("data_directory"),
    paste0(
      settings("count_control_name"),
      settings("data_extension")
    )
  ))
}


# load_settings must be run before this.
get_data_path <- function() {
  absolute_path(file.path(
    .script_root_directory,
    settings("data_directory")
  ))
}


# load_settings must be run before this.
get_output_path <- function(file = "") {
  absolute_path(file.path(
    .script_root_directory,
    settings("output_directory"),
    file
  ))
}
