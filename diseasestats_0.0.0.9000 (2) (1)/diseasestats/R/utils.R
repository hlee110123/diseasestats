#' Validate Database Connection
#'
#' Internal function to validate database connection object
#'
#' @param conn A DBI connection object
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_connection <- function(conn) {
  if (!inherits(conn, "DBIConnection")) {
    stop("Invalid connection object. Must be a DBI connection.", call. = FALSE)
  }
  if (!DBI::dbIsValid(conn)) {
    stop("Database connection is not valid or has been closed.", call. = FALSE)
  }
  TRUE
}

#' Validate Schema Name
#'
#' Internal function to validate schema name
#'
#' @param schema Character string of schema name
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_schema <- function(schema) {
  if (!is.character(schema) || length(schema) != 1 || nchar(schema) == 0) {
    stop("Schema name must be a non-empty character string.", call. = FALSE)
  }
  TRUE
}

#' Validate Category Name
#'
#' Internal function to validate disease category name
#'
#' @param category Character string of category name
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_category <- function(category) {
  if (!category %in% names(DISEASE_CATEGORIES)) {
    stop(paste("Invalid category. Must be one of:",
               paste(names(DISEASE_CATEGORIES), collapse = ", ")),
         call. = FALSE)
  }
  TRUE
}
