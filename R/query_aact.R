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


#' query_aact: Find trials with intervention(s) and/or condition(s) of interest
#'
#' For intervention (treatment) and/or disease condition search terms, query the AACT database and output
#' data-set based on the search. By default, for a given search term, this function will web-scrap clinicaltrials.gov to
#' find related terms (ex: input = "breast cancer", related = "breast tumor").
#'
#' @inheritParams extract_aact
#' @param is_shiny Set to TRUE if used within a SHINY app
#' @param is_shiny_data Use tables that exist in the global environment. For Shiny apps.
#' @export
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
#' # Query based on just intervention (any disease indications) #
#' terms_int <- "pembrolizumab" # Intervention / treatment
#' out_query1 <- query_aact(terms_int = terms_int, terms_cond = NULL)
#' out_query1$trt_search_terms # Input search terms + related terms
#' out_query1$cond_search_terms # Input search terms + related terms
#'
#' # Query based on intervention and disease condition #
#' terms_cond <- "breast cancer"
#' out_query2 <- query_aact(terms_int = terms_int, terms_cond = terms_cond)
#' out_query2$trt_search_terms # Input search terms + related terms
#' out_query2$cond_search_terms # Input search terms + related terms
#'
#'
#' }
#'
query_aact <- function(con = NULL,
                       path_raw = NULL,
                       file_type = NULL,
                       board = NULL,
                       database = NULL,
                       is_shiny = FALSE,
                       is_shiny_data = FALSE,
                       data_preloaded = NULL,
                       terms_int,
                       terms_cond = NULL,
                       date_range = NULL,
                       related_terms_int = NULL,
                       related_terms_cond = NULL,
                       verbose = TRUE) {
  # Initialize
  null_int <- is.null(terms_int)
  null_cond <- is.null(terms_cond)
  # Treatment NCT IDs/Search Terms
  trt_nct_ids <- NULL
  trt_search_terms <- NULL
  # Condition NCT IDs/Search Terms
  cond_nct_inds <- NULL
  cond_search_terms <- NULL

  if (!null_int) {
    # Run intervention_setup()
    if (verbose) {
      message("Querying treatment......")
    }
    trt_result <- setup_intervention(con = con,
                                     path_raw = path_raw,
                                     file_type = file_type,
                                     board = board,
                                     database = database,
                                     is_shiny_data = is_shiny_data,
                                     data_preloaded = data_preloaded,
                                     is_shiny = is_shiny,
                                     search_terms = terms_int,
                                     related_terms = related_terms_int)
    # Extract search terms
    trt_search_terms <- trt_result$search_terms_raw
    # Extract data, and NCT IDs
    trt_dat <- trt_result$trt_dat
    trt_nct_ids <- unique(trt_dat$nct_id)
    if (null_cond) {
      cond_result <- setup_conditions(con=con,
                                      path_raw = path_raw,
                                      file_type = file_type,
                                      board = board,
                                      database = database,
                                      is_shiny_data = is_shiny_data,
                                      data_preloaded = data_preloaded,
                                      is_shiny = is_shiny,
                                      user_nct_ids = trt_nct_ids,
                                      search_terms = NULL)
      cond_dat <- cond_result$cond_dat
    }
  }
  if (!null_cond) {
    if (verbose) {
      message("Querying condition......")
    }
    # Run setup_conditions()
    cond_result <- setup_conditions(con = con,
                                    path_raw = path_raw,
                                    file_type = file_type,
                                    board = board,
                                    database = database,
                                    is_shiny_data = is_shiny_data,
                                    data_preloaded = data_preloaded,
                                    is_shiny = is_shiny,
                                    user_nct_ids = NULL,
                                    search_terms = terms_cond,
                                    related_terms = related_terms_cond)
    # Extract search terms
    cond_search_terms <- cond_result$search_terms_raw
    # Extract data and NCT IDs
    cond_dat <- cond_result$cond_dat
    cond_nct_ids <- unique(cond_dat$nct_id)
    if (null_int) {
      trt_dat <- setup_intervention_nct(con = con,
                                        path_raw = path_raw,
                                        file_type = file_type,
                                        board = board,
                                        database = database,
                                        is_shiny_data = is_shiny_data,
                                        data_preloaded = data_preloaded,
                                        user_nct_ids = cond_nct_ids)
    }
  }

  # Combine set of NCT IDs
  if (!null_cond & !null_int) {
    user_nct_ids <- intersect(trt_nct_ids, cond_nct_ids)
  }
  if (null_cond & !null_int) {
    user_nct_ids <- trt_nct_ids
  }
  if (!null_cond & null_int) {
    user_nct_ids <- cond_nct_ids
  }

  if (verbose) {
    message("Querying designs, studies, and sponsor data-tables...")
  }
  # Design
  designs_dat <- setup_designs(con = con,
                               path_raw = path_raw,
                               file_type = file_type,
                               is_shiny_data = is_shiny_data,
                               data_preloaded = data_preloaded,
                               board = board,
                               user_nct_ids = user_nct_ids,
                               database = database)
  # Studies Information
  studies_dat <- setup_studies(con = con,
                               path_raw = path_raw,
                               file_type = file_type,
                               is_shiny_data = is_shiny_data,
                               data_preloaded = data_preloaded,
                               board = board,
                               user_nct_ids = user_nct_ids,
                               database = database)
  # Sponsor Information
  spons_dat <- setup_sponsors(con = con,
                              path_raw = path_raw,
                              file_type = file_type,
                              is_shiny_data = is_shiny_data,
                              data_preloaded = data_preloaded,
                              board = board,
                              user_nct_ids = user_nct_ids,
                              database = database)

  # Divide merge scenarios depending on which terms were provided

  # If BOTH intervention term and condition terms were provided:
  if (!null_cond & !null_int) {
    trt_cond_dat <- trt_dat %>%
      inner_join(cond_dat, by = "nct_id")
  } else {
  # If only one of intervention or condition terms were provided:
    trt_cond_dat <- trt_dat %>%
      left_join(cond_dat, by = "nct_id")
  }
  # Combine Study Information
  analy_study <- trt_cond_dat %>%
    left_join(designs_dat, by = "nct_id") %>%
    left_join(studies_dat, by = "nct_id") %>%
    left_join(spons_dat, by = "nct_id")
  # If date_range is provided
  if (!is.null(date_range)) {
    analy_study <- filter_date_range(analy_study, date_range)
    user_nct_ids <- analy_study %>% pull(nct_id)
  }

  if (is_shiny) {
    search_terms_raw <- c(terms_int, terms_cond)
    search_terms_processed <- search_terms_raw %>%
      unique() %>%
      tolower() %>%
      paste0(.,  collapse="|")
    # Fuzzy search
    analy_study_fuzzy <- analy_study %>%
      filter(((stringdist::ain(tolower(intervention_total), search_terms_raw, method = "lv", maxDist = 1) & !stringr::str_detect(tolower(intervention_total), search_terms_processed)) &
                (stringdist::ain(tolower(design_groups_title), search_terms_raw, method = "lv", maxDist = 1) & !stringr::str_detect(tolower(design_groups_title), search_terms_processed)) &
                (stringdist::ain(tolower(design_groups_description), search_terms_raw, method = "lv", maxDist = 1) & !stringr::str_detect(tolower(design_groups_description), search_terms_processed)) &
                (stringdist::ain(tolower(mesh_term_total), search_terms_raw, method = "lv", maxDist = 1) & !stringr::str_detect(tolower(mesh_term_total), search_terms_processed))) |
               is_fuzzy_cond == 1) %>%
      select(nct_id, design_group_id, design_groups_title, intervention_total, is_combo, condition,
             start_date, completion_date, phase)
    # Direct search
    analy_study_direct <- analy_study %>%
      filter(((stringr::str_detect(tolower(intervention_total), search_terms_processed)) &
                (stringr::str_detect(tolower(design_groups_title), search_terms_processed)) &
                (stringr::str_detect(tolower(design_groups_description), search_terms_processed)) &
                (stringr::str_detect(tolower(mesh_term_total), search_terms_processed))) |
               is_fuzzy_cond == 0) %>%
      select(nct_id, design_group_id, design_groups_title, intervention_total, is_combo, condition,
             start_date, completion_date, phase)

    # final output for Shiny
    list(analy_study = analy_study, analy_study_direct = analy_study_direct, analy_study_fuzzy = analy_study_fuzzy, user_nct_ids = user_nct_ids,
         trt_search_terms = trt_search_terms, cond_search_terms = cond_search_terms)
  } else {
    # final output
    list(analy_study = analy_study, user_nct_ids = user_nct_ids,
         trt_search_terms = trt_search_terms, cond_search_terms = cond_search_terms)
  }

}
