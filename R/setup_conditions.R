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


## Setup Condition Data (type of cancer) ##
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers
# @search_terms: Search terms
# @related_terms: User-provided related terms (NULL by default)

globalVariables(c(".", "name", "downcase_name", "condition"))

setup_conditions <- function(con,
                             path_raw,
                             file_type,
                             board,
                             database,
                             is_shiny = FALSE,
                             is_shiny_data = FALSE,
                             data_preloaded = NULL,
                             user_nct_ids,
                             search_terms,
                             related_terms = NULL) {
  # load data
  conditions <- load_data(con = con,
                          path_raw = path_raw,
                          file_type = file_type,
                          board = board,
                          database = database,
                          data_preloaded = data_preloaded,
                          user_nct_ids = user_nct_ids,
                          table_name = "conditions")

  # Find related terms (depends on whether function is used in Shiny app)
  if (is_shiny) {
    # search terms
    search_terms_raw <- search_terms
    search_terms_processed <- search_terms_raw %>%
      unique() %>%
      tolower() %>%
      paste0(.,  collapse="|")
    # condition table
    # If search_terms is provided, call fuzzy_filter()
    if (search_terms_processed != "") {
      conditions <- fuzzy_filter(indata = conditions,
                                 search_terms_raw,
                                 search_terms_processed,
                                 column="downcase_name")
      # process conditions table
      conditions_processed <- conditions %>%
        select(-name) %>%
        rename(condition = downcase_name) %>%
        group_by(nct_id) %>%
        summarise(condition = paste0(condition, collapse="|")) %>%
        ungroup() %>%
        mutate(is_fuzzy_cond = if_else(
          !stringr::str_detect(tolower(condition), search_terms_processed),
          1, 0))
    # else, don't call fuzzy_filter()
    } else {
      # process conditions table
      conditions_processed <- conditions %>%
        select(-name) %>%
        rename(condition = downcase_name) %>%
        group_by(nct_id) %>%
        summarise(condition = paste0(condition, collapse="|")) %>%
        ungroup() %>%
        mutate(is_fuzzy_cond = 0)
    }


  } else {
    # if search_terms is provided
    if (!is.null(search_terms)) {
      # if related terms is not provided
      if (is.null(related_terms)) {
        res <- combine_search_terms(search_terms = search_terms, type="condition")
        search_terms_raw <- res$search_terms_raw
        search_terms_processed <- res$search_terms_processed
      } else {
        # preprocess search terms
        search_terms_raw <- c(search_terms, related_terms)
        search_terms_processed <- search_terms_raw %>%
          unique() %>%
          tolower() %>%
          paste0(.,  collapse="|")
      }
      # fuzzy filter
      conditions <- fuzzy_filter(indata=conditions,
                                 search_terms_raw,
                                 search_terms_processed,
                                 column="downcase_name")
      # clean up conditions table
      conditions_processed <- conditions %>%
        select(-name) %>%
        rename(condition = downcase_name) %>%
        group_by(nct_id) %>%
        summarise(condition = paste0(condition, collapse="|")) %>%
        ungroup() %>%
        mutate(is_fuzzy_cond = if_else(
          !stringr::str_detect(tolower(condition), search_terms_processed),
          1, 0))
    } else {
      # set search_terms to NULL
      search_terms_raw <- NULL
      search_terms_processed <- NULL
      # clean up conditions table
      conditions_processed <- conditions %>%
        select(-name) %>%
        rename(condition = downcase_name) %>%
        group_by(nct_id) %>%
        summarise(condition = paste0(condition, collapse="|")) %>%
        ungroup() %>%
        mutate(is_fuzzy_cond = 0)
    }
  }

  return(list(cond_dat = conditions_processed, search_terms_raw = search_terms_raw))
}
