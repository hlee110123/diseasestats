#' ATC Categories
#'
#' A list containing all Anatomical Therapeutic Chemical (ATC) classification categories
#' with their codes and full names.
#'
#' @format A list with 14 elements, each representing an ATC category
#' @export
ATC_CATEGORIES <- list(
  A = list(name = "Alimentary tract and metabolism", code = "A"),
  B = list(name = "Blood and blood forming organs", code = "B"),
  C = list(name = "Cardiovascular system", code = "C"),
  D = list(name = "Dermatologicals", code = "D"),
  G = list(name = "Genito-urinary system and sex hormones", code = "G"),
  H = list(name = "Systemic hormonal preparations", code = "H"),
  J = list(name = "Antiinfectives for systemic use", code = "J"),
  L = list(name = "Antineoplastic and immunomodulating agents", code = "L"),
  M = list(name = "Musculoskeletal system", code = "M"),
  N = list(name = "Nervous system", code = "N"),
  P = list(name = "Antiparasitic products", code = "P"),
  R = list(name = "Respiratory system", code = "R"),
  S = list(name = "Sensory organs", code = "S"),
  V = list(name = "Various", code = "V")
)

#' Build ATC Query
#'
#' Builds a SQL query for retrieving prescription statistics for a specific ATC code.
#'
#' @param schema Character string with the database schema name
#' @param atc_code Character string with the ATC code
#'
#' @return A character string containing the SQL query
#' @keywords internal
build_atc_query <- function(schema, atc_code) {
  # Sanitize inputs to prevent SQL injection
  schema <- gsub("[^a-zA-Z0-9_]", "", schema)
  atc_code <- gsub("[^A-Z]", "", atc_code)

  sprintf("
  WITH atc_concepts AS (
      SELECT
          concept_id,
          concept_name,
          concept_code,
          concept_class_id
      FROM %s.concept
      WHERE vocabulary_id = 'ATC'
      AND concept_code LIKE '%s%%'
  ),
  direct_mappings AS (
      SELECT DISTINCT
          c1.concept_id as atc_concept_id,
          c2.concept_id as rx_concept_id
      FROM atc_concepts c1
      JOIN %s.concept_relationship cr
          ON c1.concept_id = cr.concept_id_1
      JOIN %s.concept c2
          ON cr.concept_id_2 = c2.concept_id
      WHERE c2.vocabulary_id IN ('RxNorm', 'RxNorm Extension')
      AND cr.relationship_id = 'Maps to'
      AND c2.invalid_reason IS NULL
      AND cr.invalid_reason IS NULL
  ),
  all_relevant_concepts AS (
      SELECT DISTINCT
          dm.rx_concept_id,
          ca.descendant_concept_id
      FROM direct_mappings dm
      JOIN %s.concept_ancestor ca
          ON dm.rx_concept_id = ca.ancestor_concept_id
  ),
  prescription_stats AS (
      SELECT
          COUNT(*) as category_prescriptions,
          (SELECT COUNT(*)
           FROM %s.drug_exposure
           WHERE drug_concept_id IS NOT NULL
           AND drug_exposure_start_date >= '2016-01-01'
           AND (drug_exposure_end_date IS NULL OR drug_exposure_end_date <= '2024-12-31')
          ) as total_prescriptions,
          (SELECT COUNT(DISTINCT person_id)
           FROM %s.person
          ) as total_patients
      FROM
          %s.drug_exposure de
          JOIN all_relevant_concepts arc
              ON de.drug_concept_id = arc.descendant_concept_id
      WHERE
          de.drug_exposure_start_date >= '2016-01-01'
          AND (de.drug_exposure_end_date IS NULL OR de.drug_exposure_end_date <= '2024-12-31')
  )
  SELECT
      category_prescriptions,
      total_prescriptions,
      total_patients,
      ROUND((CASE WHEN total_prescriptions = 0 THEN 0
            ELSE (category_prescriptions::numeric / total_prescriptions) * 100 END), 2) as percentage_of_total,
      ROUND((CASE WHEN total_patients = 0 THEN 0
            ELSE (category_prescriptions::numeric / total_patients) * 100000 END), 2) as rate_per_100k
  FROM
      prescription_stats",
          schema, atc_code, schema, schema, schema, schema, schema, schema)
}

#' Get ATC Statistics
#'
#' Retrieves prescription statistics for a specific ATC category.
#'
#' @param conn A DatabaseConnector connection object
#' @param schema Character string with the database schema name
#' @param atc_code Character string with the ATC code
#' @param start_date Start date for the analysis period (YYYY-MM-DD)
#' @param end_date End date for the analysis period (YYYY-MM-DD)
#'
#' @return A data frame with prescription statistics
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DatabaseConnector::connect(connectionDetails)
#' stats <- get_atc_stats(conn, "cdm_schema", "C", "2020-01-01", "2020-12-31")
#' DatabaseConnector::disconnect(conn)
#' }
get_atc_stats <- function(conn, schema, atc_code, start_date = "2016-01-01", end_date = "2024-12-31") {
  # Validate inputs
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    stop("Package 'DatabaseConnector' is required. Please install it with install.packages('DatabaseConnector')")
  }

  # Use common validation functions
  validate_connection_db_connector(conn)
  validate_schema(schema)
  validate_atc_code(atc_code)

  # Validate dates
  start_date <- validate_date(start_date, "start_date")
  end_date <- validate_date(end_date, "end_date")

  if (start_date >= end_date) {
    stop("Start date must be before end date")
  }

  # Build the query with proper schema references
  query <- build_atc_query(schema, atc_code)

  # Replace date parameters
  query <- gsub("'2016-01-01'", paste0("'", format(start_date, "%Y-%m-%d"), "'"), query)
  query <- gsub("'2024-12-31'", paste0("'", format(end_date, "%Y-%m-%d"), "'"), query)

  # Execute query with error handling
  tryCatch({
    # Execute query using DatabaseConnector
    result <- DatabaseConnector::querySql(conn, query)

    # Handle empty results
    if (nrow(result) == 0 || is.null(result)) {
      message(sprintf("No results found for ATC category %s", atc_code))
      # Create empty result with correct structure
      result <- data.frame(
        category_prescriptions = 0,
        total_prescriptions = 0,
        total_patients = 0,
        percentage_of_total = 0,
        rate_per_100k = 0
      )
    }

    # Add category name to results
    result$atc_category <- ATC_CATEGORIES[[atc_code]]$name
    result$atc_code <- atc_code
    result$date_range <- paste(format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"))

    return(result)

  }, error = function(e) {
    message(sprintf("Error executing query for ATC code %s: %s", atc_code, e$message))
    return(NULL)
  })
}

#' Get All ATC Statistics
#'
#' Retrieves prescription statistics for all ATC categories.
#'
#' @param conn A DatabaseConnector connection object
#' @param schema Character string with the database schema name
#' @param start_date Start date for the analysis period (YYYY-MM-DD)
#' @param end_date End date for the analysis period (YYYY-MM-DD)
#'
#' @return A data frame with prescription statistics for all ATC categories
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DatabaseConnector::connect(connectionDetails)
#' all_stats <- get_all_atc_stats(conn, "cdm_schema", "2020-01-01", "2020-12-31")
#' DatabaseConnector::disconnect(conn)
#' }
get_all_atc_stats <- function(conn, schema, start_date = "2016-01-01", end_date = "2024-12-31") {
  # Validate inputs using the common validation functions
  validate_connection_db_connector(conn)
  validate_schema(schema)

  results_list <- list()

  # Process each ATC category with error handling
  for (code in names(ATC_CATEGORIES)) {
    tryCatch({
      stats <- get_atc_stats(conn, schema, code, start_date, end_date)
      if (!is.null(stats)) {
        results_list[[code]] <- stats
      }
    }, error = function(e) {
      message(sprintf("Error processing ATC code %s: %s", code, e$message))
    })
  }

  # Check if any results were obtained
  if (length(results_list) == 0) {
    stop("Failed to retrieve statistics for any ATC category")
  }

  # Combine results
  result_df <- do.call(rbind, results_list)
  rownames(result_df) <- NULL
  return(result_df)
}

#' Format ATC Prescription Rates Table
#'
#' Creates a formatted table with prescription rates for ATC categories.
#'
#' @param stats_df A data frame produced by get_all_atc_stats
#' @param include_columns Character vector specifying which columns to include.
#'        Options include: "percentage", "rate", "counts", "all"
#'
#' @return A data frame with formatted prescription rates
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- DatabaseConnector::connect(connectionDetails)
#' all_stats <- get_all_atc_stats(conn, "cdm_schema")
#' rates_table <- format_atc_rates_table(all_stats, c("percentage", "rate"))
#' }
format_atc_rates_table <- function(stats_df, include_columns = c("percentage", "rate")) {
  # Prepare base table
  result <- data.frame(
    atc_code = stats_df$atc_code,
    atc_category = stats_df$atc_category,
    stringsAsFactors = FALSE
  )

  # Add columns based on user choice
  if("all" %in% include_columns || "counts" %in% include_columns) {
    result$category_prescriptions <- stats_df$category_prescriptions
    result$total_prescriptions <- stats_df$total_prescriptions
    result$total_patients <- stats_df$total_patients
  }

  if("all" %in% include_columns || "percentage" %in% include_columns) {
    result$percentage_of_total <- sprintf("%.2f%%", stats_df$percentage_of_total)
  }

  if("all" %in% include_columns || "rate" %in% include_columns) {
    result$rate_per_100k <- sprintf("%.2f", stats_df$rate_per_100k)
  }

  # Add date range
  result$date_range <- stats_df$date_range

  # Order by ATC code
  result <- result[order(result$atc_code), ]

  return(result)
}
