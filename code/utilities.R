combine_tables <- function(tables) {
  if (length(tables) == 0) {
    return(tables)
  }

  table <- tables[[1]]
  for (i in 2:length(tables)) {
    table <- rbind.data.frame(table, tables[[i]])
  }
  table
}


extend_vector <- function(head, tail) {
  head[[length(head) + 1]] <- tail
  head
}


filter_experiment <- function(df, experiment_level) {
  df %>%
    dplyr::filter(.data$experiment == experiment_level)
}


filter_density <- function(df, density_level) {
  df %>%
    dplyr::filter(.data$density == density_level)
}

filter_food <- function(df, food_level) {
  df %>%
    dplyr::filter(.data$food == food_level)
}


join_all <- function(tables, by) {
  if (length(tables) == 0) {
    return(tables)
  }

  table <- data.frame(tables[[1]])
  for (i in 2:length(tables)) {
    table <- table %>% dplyr::full_join(data.frame(tables[[i]]), by = by)
  }
  table
}


# Create a named vector from a vector of names and a vector of values.
#
# Parameters
# ----------
# names_vector: vector
#   Vector of names to use as keys.
# value_vector: vector
#   Vector of values to map to names.
#
# Returns
# -------
# vector
#   A vector v such that names(v)\[\[i]] = names_vector[\[i]] and v[\[i]] =
#         value_vector[\[i]]
named_vector <- function(names_vector, value_vector) {
  names(value_vector) <- names_vector
  value_vector
}


# Get a string representing significance with 0-3 stars.
#
# Parameters
# ----------
# p: numeric
#   p-value to represent.
#
# Returns
# -------
# character
#   "", "*", "**", or "***".
p_stars_no_dot <- Vectorize(
  function(p) {
    stats::symnum(p,
      cutpoints = c(
        0,
        settings("p_star_cut_triple"),
        settings("p_star_cut_double"),
        settings("p_star_cut_single"),
        1
      ),
      symbols = c("***", "**", "*", " ")
    )
  }
)


# Get a string representing a p-value succinctly, with p-values below a
# configurable threshold being printed as e.g. "<0.001"
#
# Parameters
# ----------
# p: numeric
#   p-value to represent.
#
# Returns
# -------
# character
#   String version of p-value, or min allowed p-value.
p_min <- Vectorize(
  function(p) {
    p_value_min_cutoff <- settings("p_value_min_cutoff")
    p_value_nsmall <- settings("p_value_nsmall")
    ifelse(p < p_value_min_cutoff,
      paste0("<", p_value_min_cutoff),
      format(round(p, p_value_nsmall),
        nsmall = p_value_nsmall
      )
    )
  }
)


# Log a message describing a task, run the task, print time it took.
#
# Parameters
# ----------
# description: character
#   Brief description of task.
# lambda: function
#   Function to execute.
# quietly: logical
#   Suppress printing.
timed_subroutine <- function(description, lambda, quietly = F) {
  t <- 0
  begin_timer <- function() {
    t_arr <- proc.time()
    t <<- t_arr[[1]] + t_arr[[2]] # user + system
  }

  end_timer <- function() {
    t_arr <- proc.time()
    dt <- t_arr[[1]] + t_arr[[2]] - t
    t <<- 0
    dt
  }

  print_time <- function(s) {
    cat(paste0("finished (", round(s, 2), "s)\n"))
  }

  if (!quietly) {
    cat(description)
  }

  begin_timer()
  return_value <- lambda()
  end_time <- end_timer()
  if (!quietly) {
    print_time(end_time)
  }

  return_value
}
