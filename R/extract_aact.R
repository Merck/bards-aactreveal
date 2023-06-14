# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the aactreveal program.
#
# aactreveal is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' extract_aact: Extract and create analysis AACT data
#'
#' This function performs multiple steps: (1) Querying treatment and/or condition,
#' (2) Based on the identified trials for the matched treatment and/or condition, query
#' the outcome data. Key outputs include study-level information, along with outcome data
#' (treatment-specific, ex: median OS; and treatment-comparisons, ex: Hazard Ratios)
#'
#' @param con Connection information for AACT (needed if connecting to database)
#' @param path_raw File path for raw AACT data (will ignore "con" argument if provided)
#' @param file_type File type for the raw AACT (ex: "txt" or "rda")
#' @param board Pins board
#' @param terms_int Intervention / treatment terms to search for (ex: paclitaxel). Set to NULL if no search.
#' @param terms_cond Search terms for condition(s) of interest (ex: non small lung cancer). Set to NULL if no search.
#' @param date_range Date range of interest (ex: TBD). Set to NULL if no search.
#' @param outcome_list List of outcome-based parameters to search for
#' @param outcome_name Vector of names for the searched outcomes (if NULL, uses generic names).
#' @param outcome_type Pre-specified outcomes (TBD; not currently available)
#' @param related_terms_int Related terms for intervention(s) (terms_int).
#' Default=NULL, which scrapes clinicaltrials.gov for term(s) related to terms_int
#' @param related_terms_cond Related terms for condition(s) (terms_cond).
#' Default=NULL, which scrapes clinicaltrials.gov for term(s) related to terms_cond
#' @param is_shiny Set to TRUE if used within a SHINY app. Default=FALSE
#' @param database Only used if con is not NULL. This is only needed if AACT data-tables are
#' stored such that data-tables are extracted using "select * from database.baseline" etc.
#' @param data_preloaded List of pre-loaded AACT dataset
#' @param verbose If TRUE, messages describing process will be printed
#'
#'
#' @return AACT analysis data(s) with treatments of interest and outcomes of interest
#'
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_elements html_text
#' @importFrom stringdist ain
#' @importFrom stringr str_detect str_split str_trim
#' @importFrom rlang sym
#' @importFrom purrr map2_int
#' @importFrom tidyr pivot_wider
#' @importFrom pins pin_read
#' @import dplyr
#'
#' @examples
#'
#' \donttest{
#'
#' library(aactreveal)
#'
#' # Load package RDA for illustrative purposes #
#' table_names <- c("browse_interventions", "conditions", "design_group_interventions",
#' "design_groups","designs", "interventions", "outcome_analyses", "outcome_analysis_groups",
#' "outcome_measurements", "result_groups", "sponsors", "studies")
#'
#' data(list=table_names, package="aactreveal")
#'
#' terms_int <- "pembrolizumab"
#' terms_cond <- "breast cancer"
#' orr_list <- list(include = c("overall response rate", "objective response rate"),
#' exclude = c("disease control rate"),type = c("number"),range = c(0, 100) )
#' os_list <- list(include = c("overall survival"))
#' outcome_list <- list(orr_list, os_list)
#' outcome_name <- c("ORR", "OS")
#'
#' res <- extract_aact(con=NULL, path_raw=NULL, terms_int=terms_int, terms_cond=terms_cond,
#' outcome_list = outcome_list, outcome_name = outcome_name, verbose = TRUE)
#'
#' }
#'
#' @details Add more details about extract_aact here
#'

extract_aact <- function(con = NULL,
                         path_raw = NULL,
                         file_type = "txt",
                         board = NULL,
                         terms_int,
                         terms_cond,
                         date_range = NULL,
                         related_terms_int = NULL,
                         related_terms_cond = NULL,
                         outcome_type = NULL,
                         outcome_list = NULL,
                         outcome_name = NULL,
                         is_shiny = FALSE,
                         is_shiny_data = FALSE,
                         database = NULL,
                         data_preloaded = NULL,
                         verbose = TRUE) {
  # Argument Check
  # If path for data is provided, don't connect to databases
  if (!is.null(path_raw)) {
    con <- NULL
  }
  # If both terms_int and terms_cond are null, then stop
  null_int <- is.null(terms_int)
  null_cond <- is.null(terms_cond)
  if (null_int & null_cond) {
    stop("Intervention search terms (terms_int) or condition search terms (terms_cond) must be provided")
  }
  # find trials with intervention(s) and/or condition(s) of interest
  query_aact_output <- query_aact(con = con,
                                  path_raw = path_raw,
                                  file_type = file_type,
                                  board = board,
                                  database = database,
                                  is_shiny = is_shiny,
                                  is_shiny_data = is_shiny_data,
                                  data_preloaded = data_preloaded,
                                  terms_int = terms_int,
                                  terms_cond = terms_cond,
                                  date_range = date_range,
                                  related_terms_int = related_terms_int,
                                  related_terms_cond = related_terms_cond,
                                  verbose = verbose)

  # extract NCT IDs of these trials
  user_nct_ids <- query_aact_output$user_nct_ids
  # setup outcome_measures_processed and outcome_analyses_processed
  if (verbose) {
    message("Searching for outcome(s) of interest......")
  }
  process_outcomes_output <- process_outcomes(con = con,
                                              path_raw = path_raw,
                                              file_type = file_type,
                                              board = board,
                                              is_shiny_data = is_shiny_data,
                                              database = database,
                                              data_preloaded = data_preloaded,
                                              user_nct_ids = user_nct_ids)
  outcome_analyses_processed <- process_outcomes_output$outcome_analyses_processed
  outcome_measures_processed <- process_outcomes_output$outcome_measures_processed

  # Search and clean up
  res_out <- query_outcomes(outcome_analyses_processed = process_outcomes_output$outcome_analyses_processed,
                            outcome_measures_processed = process_outcomes_output$outcome_measures_processed,
                            analy = query_aact_output$analy_study,
                            outcome_type = outcome_type,
                            outcome_list = outcome_list,
                            outcome_name = outcome_name,
                            verbose = verbose,
                            search_terms_int = query_aact_output$trt_search_terms)

  # Final output
  list(analy_study = query_aact_output$analy_study,
       outcome_measures_query = res_out$outcome_measures_final, outcome_analyses_query = res_out$outcome_analyses_final,
       outcome_measures_raw = process_outcomes_output$outcome_measures_processed, outcome_analyses_raw = process_outcomes_output$outcome_analyses_processed,
       outcome_analyses_full = res_out$outcome_analyses_full,
       result_groups = process_outcomes_output$result_groups, baseline_dat = process_outcomes_output$baseline_dat,
       trt_search_terms = query_aact_output$trt_search_terms, cond_search_terms = query_aact_output$cond_search_terms)
}
