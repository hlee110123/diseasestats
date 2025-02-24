#' Health Database Analysis Package
#'
#' A package for analyzing healthcare data in OMOP CDM format, providing
#' tools for disease prevalence calculation and medication prescription analysis.
#'
#' @section Disease Analysis Functions:
#' Functions for analyzing disease prevalence based on ICD-10 codes:
#' \itemize{
#'   \item \code{\link{get_category_count}} - Count patients in a disease category
#'   \item \code{\link{get_total_patients}} - Count total patients in database
#'   \item \code{\link{get_prevalence_rates}} - Calculate disease prevalence rates
#' }
#'
#' @section Medication Analysis Functions:
#' Functions for analyzing medication prescriptions using ATC classification:
#' \itemize{
#'   \item \code{\link{get_atc_stats}} - Get statistics for a specific ATC category
#'   \item \code{\link{get_all_atc_stats}} - Get statistics for all ATC categories
#'   \item \code{\link{format_atc_rates_table}} - Format ATC statistics for presentation
#' }
#'
#' @section Data Categories:
#' The package provides standardized categories for analysis:
#' \itemize{
#'   \item \code{\link{DISEASE_CATEGORIES}} - ICD-10 based disease categories
#'   \item \code{\link{ATC_CATEGORIES}} - Anatomical Therapeutic Chemical categories
#' }
#'
#' @docType package
#' @name healthdbr
NULL
