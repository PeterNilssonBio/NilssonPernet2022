# This file contains logic used for saving tables to files.

# Return path for a given table file name.
#
# Parameters
# ----------
# file_name: character
#   Base name of file, eg "table_1".
#
# Returns
# -------
# character
#   Path to file.
get_table_output_path <- function(file_name) {
  get_output_path(paste0(file_name, settings("table_extension")))
}


save_table <- function(table, file_name) {
  path <- get_table_output_path(file_name)
  kableExtra::save_kable(table,
    file = path
  )
}


save_table_1 <- function(table) {
  save_table(table, settings("table_1_name"))
}


save_table_2 <- function(table) {
  save_table(table, settings("table_2_name"))
}


save_table_S1 <- function(table) {
  save_table(table, settings("table_S1_name"))
}


save_table_S2 <- function(table) {
  save_table(table, settings("table_S2_name"))
}


save_table_S3 <- function(table) {
  save_table(table, settings("table_S3_name"))
}


save_table_S4 <- function(table) {
  save_table(table, settings("table_S4_name"))
}


save_table_S5 <- function(table) {
  save_table(table, settings("table_S5_name"))
}


save_count_control_table <- function(table) {
  save_table(table, settings("count_table_name"))
}
