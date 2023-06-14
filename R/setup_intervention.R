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


#' setup_intervention: Find trials with intervention(s) of interest
#'
#' Details here.
#' Expanding on intervention_setup() by using find_related_terms() (web scraping) to get
#' related terms from clinicaltrials.gov and putting these terms in str_detect
#'
#' @inheritParams extract_aact
#' @param search_terms Vector of terms to search for (ex: chemotherapy)
#' @param related_terms If NULL (default), search for related terms using clinicaltrials.gov.
#' Otherwise related_terms should be a vector of related terms
#' @param is_shiny_data Use tables that exist in the global environment. For Shiny apps.
#'
#' @return AACT intervention data with treatments of interest
#'
setup_intervention <- function(con,
                               path_raw,
                               file_type,
                               board,
                               database,
                               is_shiny = FALSE,
                               is_shiny_data = FALSE,
                               data_preloaded = NULL,
                               search_terms,
                               related_terms = NULL) {
  # Load data
  interventions <- load_data(con = con,
                             path_raw = path_raw,
                             file_type = file_type,
                             board = board,
                             database = database,
                             data_preloaded = data_preloaded,
                             table_name = "interventions")
  browse_interventions <- load_data(con = con,
                                    path_raw = path_raw,
                                    file_type = file_type,
                                    board = board,
                                    database = database,
                                    data_preloaded = data_preloaded,
                                    table_name = "browse_interventions")
  design_groups <- load_data(con = con,
                             path_raw = path_raw,
                             file_type = file_type,
                             board = board,
                             database = database,
                             data_preloaded = data_preloaded,
                             table_name = "design_groups")
  design_group_interventions <- load_data(con = con,
                                          path_raw = path_raw,
                                          file_type = file_type,
                                          board = board,
                                          database = database,
                                          data_preloaded = data_preloaded,
                                          table_name = "design_group_interventions")

  # Find related terms (depends on whether function is used in Shiny app)
  if (is_shiny) {
    search_terms_raw <- search_terms
    search_terms_processed <- search_terms_raw %>%
      unique() %>%
      tolower() %>%
      paste0(.,  collapse="|")
  } else {
    # Process search terms
    if (is.null(related_terms)) {
      combine_search_terms_output <- combine_search_terms(search_terms, "other_terms")
      search_terms_raw <- combine_search_terms_output$search_terms_raw
      search_terms_processed <- combine_search_terms_output$search_terms_processed
    } else {
      search_terms_raw <- c(search_terms, related_terms)
      search_terms_processed <- search_terms_raw %>%
        unique() %>%
        tolower() %>%
        paste0(.,  collapse="|")
    }
  }
  # Filter interventions table using direct string search or fuzzy string search
  interventions_processed <- fuzzy_filter(interventions,
                                          search_terms_raw,
                                          search_terms_processed,
                                          "name",
                                          max_dist_lv = 1) %>%
    rename(is_fuzzy_int = is_fuzzy, is_direct_int = is_direct) %>%
    select(-id)
  # Filter browse_interventions table using direct string search or fuzzy string search
  browse_interventions_processed <- fuzzy_filter(browse_interventions,
                                                 search_terms_raw,
                                                 search_terms_processed,
                                                 "downcase_mesh_term",
                                                 max_dist_lv = 1) %>%
    rename(is_fuzzy_bro = is_fuzzy, is_direct_bro = is_direct) %>%
    select(-id)

  is_names <- c("is_direct_int", "is_direct_bro", "is_fuzzy_int", "is_fuzzy_bro")

  # Join processed interventions and browse_interventions tables
  interventions_combined <- interventions_processed %>%
    full_join(browse_interventions_processed, by = "nct_id") %>%
    rename(intervention_description = description,
           intervention_name = name)
    # mutate(is_fuzzy = if_else(
    #   !is_direct_bro & !is_direct_int, 1, 0))

  nct_interventions_combined_total <- interventions_combined %>% pull_nct()

  # Get NCT IDS from direct string match or fuzzy string match on design_groups table
  design_groups_description <- fuzzy_filter(indata = design_groups, search_terms_raw, search_terms_processed, column="description")
  design_groups_title <- fuzzy_filter(indata=design_groups, search_terms_raw, search_terms_processed, column="title")

  design_groups_processed <- design_groups_description %>%
    rbind(design_groups_title) %>%
    unique() %>%
    rename(design_groups_description = description,
           design_groups_title = title,
           is_fuzzy_design = is_fuzzy,
           is_direct_design = is_direct)
    # mutate(is_fuzzy2 = if_else(
    #   !is_direct, 1, 0)
    # )
    # mutate(is_fuzzy = if_else(
    #   !stringr::str_detect(tolower(design_groups_description), search_terms_processed) &
    #     !stringr::str_detect(tolower(design_groups_title), search_terms_processed),
    #   1, 0))

  nct_design_groups_processed_total <- design_groups_processed %>% pull_nct()


  # Combine nct_interventions_combined_total and nct_design_groups_processed_total to get final NCT IDs
  nct_combined <- unique(c(nct_interventions_combined_total, nct_design_groups_processed_total))

  # Filter for nct_combined trials in all tables

  # In interventions
  interventions_final <- interventions %>%
    filter(nct_id %in% nct_combined) %>%
    rename(intervention_description = description,
           intervention_name = name,
           intervention_id = id)
  # In browse_interventions
  browse_interventions_final <- browse_interventions %>%
    filter(nct_id %in% nct_combined) %>%
    group_by(nct_id) %>%
    mutate(mesh_term_collapsed = paste(downcase_mesh_term, collapse = ",")) %>%
    select(nct_id, mesh_term_collapsed) %>%
    unique()
  # In design_groups
  design_groups_final <- design_groups %>%
    filter(nct_id %in% nct_combined) %>%
    rename(design_groups_description = description,
           design_groups_title = title,
           design_group_id = id)

  # Final output
  trt_dat <- design_group_interventions %>%
    inner_join(interventions_final, by = c("nct_id", "intervention_id")) %>%
    full_join(design_groups_final, by = c("nct_id", "design_group_id")) %>%
    full_join(browse_interventions_final, by = "nct_id") %>%
    group_by(nct_id, design_group_id) %>%
    mutate(intervention_collapsed = paste(intervention_name, collapse = " + "),
           # find initial intervention number
           intervention_num = length(intervention_name)) %>%
    ungroup()

  # Identify records with treatment
  trt_dat <- fuzzy_filter(indata=trt_dat,
                            search_terms_raw=search_terms_raw,
                            search_terms_processed=search_terms_processed,
                            column = "intervention_collapsed", max_dist_lv = 1, filter=FALSE) %>%
    rename(is_direct_int = is_direct, is_fuzzy_int = is_fuzzy)

  trt_dat <- fuzzy_filter(indata=trt_dat,
                            search_terms_raw=search_terms_raw,
                            search_terms_processed=search_terms_processed,
                            column = "design_groups_title", max_dist_lv = 1, filter=FALSE) %>%
    rename(is_direct_design = is_direct, is_fuzzy_design = is_fuzzy)

  # Feature creation
  trt_dat <- trt_dat %>%
    mutate(is_trt = if_else(
      is_direct_design | is_direct_int | (is_fuzzy_design & is_fuzzy_int), 1, 0), # Should fuzzy be included here? It can create errors
    ) %>%
    mutate(intervention_num =  purrr::map2_int(intervention_num, intervention_collapsed, find_intervention_num)) %>%
    mutate(is_combo = if_else(
      is_trt == 1 & intervention_num >= 2, 1,
      if_else(
        (is_trt == 1) & (intervention_num == 1) & (intervention_type == "Combination Product" |
                                                     stringr::str_detect(intervention_collapsed, "[+,;]|\\bwith\\b|\\band\\b|\\bor\\b|\\bplus\\b|\\bcombination\\b") |
                                                     stringr::str_detect(design_groups_title, "[+,;]|\\bwith\\b|\\band\\b|\\bor\\b|\\bplus\\b|\\bcombination\\b")
        ), 1, 0)
    )) %>%
    select(-id, -intervention_id, -intervention_type, -intervention_name, -intervention_description) %>%
    unique() %>%
    rename(intervention_total = intervention_collapsed,
           mesh_term_total = mesh_term_collapsed)

  return(list(trt_dat = trt_dat, search_terms_raw = search_terms_raw))
}
