#' Get total number of patients in database
#'
#' @param conn Database connection
#' @param cdm_schema CDM schema name
#' @return Total number of patients
#' @export
get_total_patients <- function(conn, cdm_schema) {
  query <- sprintf("
    SELECT COUNT(DISTINCT person_id) as total_patients
    FROM %s.person", cdm_schema)

  result <- DBI::dbGetQuery(conn, query)
  return(result$total_patients[1])
}

#' Get patient counts for a disease category
#'
#' @param conn Database connection
#' @param cdm_schema CDM schema name
#' @param category Disease category name
#' @return Number of unique patients
#' @export
get_category_count <- function(conn, cdm_schema, category) {
  if (!category %in% names(DISEASE_CATEGORIES)) {
    stop(sprintf("Invalid category. Must be one of: %s",
                 paste(names(DISEASE_CATEGORIES), collapse = ", ")))
  }

  codes <- DISEASE_CATEGORIES[[category]]$code_range

  query <- sprintf("
    WITH standard_concepts AS (
      SELECT DISTINCT
        cr.concept_id_2 as standard_concept_id
      FROM
        %s.concept c
        INNER JOIN %s.concept_relationship cr
          ON c.concept_id = cr.concept_id_1
          AND cr.relationship_id = 'Maps to'
          AND cr.invalid_reason IS NULL
      WHERE
        c.vocabulary_id = 'ICD10CM'
        AND c.concept_code >= '%s'
        AND c.concept_code <= '%s'
        AND c.invalid_reason IS NULL
    )
    SELECT
      COUNT(DISTINCT co.person_id) as total_unique_patients
    FROM
      standard_concepts sc
      INNER JOIN %s.condition_occurrence co
        ON sc.standard_concept_id = co.condition_concept_id
    WHERE
      co.condition_start_date >= '2016-01-01'
      AND (co.condition_end_date IS NULL OR co.condition_end_date <= '2024-12-31')",
                   cdm_schema, cdm_schema, codes[1], codes[2], cdm_schema)

  result <- DBI::dbGetQuery(conn, query)
  return(result$total_unique_patients[1])
}

#' Get prevalence rates for all disease categories
#'
#' @param conn Database connection
#' @param cdm_schema CDM schema name
#' @param start_date Optional start date for the analysis period (default: '2016-01-01')
#' @param end_date Optional end date for the analysis period (default: '2024-12-31')
#' @return Data frame with counts and prevalence rates
#' @export
get_prevalence_rates <- function(conn, cdm_schema,
                                 start_date = '2016-01-01',
                                 end_date = '2024-12-31') {
  total_patients <- get_total_patients(conn, cdm_schema)

  results <- lapply(names(DISEASE_CATEGORIES), function(category) {
    count <- get_category_count(conn, cdm_schema, category)
    prevalence <- (count / total_patients) * 100
    prevalence_per_100k <- (count / total_patients) * 100000

    data.frame(
      category = category,
      category_name = DISEASE_CATEGORIES[[category]]$name,
      code_start = DISEASE_CATEGORIES[[category]]$code_range[1],
      code_end = DISEASE_CATEGORIES[[category]]$code_range[2],
      patient_count = count,
      total_patients = total_patients,
      prevalence_rate = prevalence,
      prevalence_per_100k = prevalence_per_100k,
      date_range = paste(start_date, "to", end_date),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
