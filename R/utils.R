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


# util functions
globalVariables(c("value", "ctgov_group_code", "param", "trt", "base_n"))

globalVariables(c("study_first_submitted_date", "completion_date", "target"))

globalVariables(c(".", "intervention_total", "design_groups_title",
                  "design_groups_description", "mesh_term_total",
                  "is_fuzzy_cond", "design_group_id", "is_combo",
                  "condition", "start_date", "completion_date", "phase"))

globalVariables(c(".", "description", "name", "intervention_name",
                  "downcase_mesh_term", "title", "mesh_term_collapsed",
                  "design_group_id", "intervention_collapsed", "is_trt",
                  "intervention_type", "intervention_id",
                  "intervention_description"))

pull_nct <- function(df, a = "nct_id") {
  df %>%
    pull(a) %>%
    unique()
}

length_nct <- function(df, a = "nct_id") {
  df %>%
    pull(a) %>%
    unique() %>%
    length()
}

# used in final output (setup_intervention() and setup_intervention_num())
find_intervention_num <- function(x, y) {
  if_else(
    x == 1 & str_detect(tolower(y),  "[+,;/]|\\bwith\\b|\\band\\b|\\bplus\\b|\\bcombination\\b|\\bin combination with\\b|\\bcombined with\\b"),
    length(str_split(tolower(y),
                     "[+,;/]|\\bwith\\b|\\band\\b|\\bplus\\b|\\bcombination\\b|\\bin combination with\\b|\\bcombined with\\b")[[1]]),
    x
  )
}

# Filter Data-set Using Fuzzy Matching #
# @param indata: Input dataset (ex: conditions)
# @param search_terms_raw: Vector of search terms
# @param search_terms_processed: Search terms collapsed with "|"
# @param column: Column of indata for searching

fuzzy_filter <- function(indata, search_terms_raw, search_terms_processed, column, max_dist_lv = 1, filter=TRUE) {

  outdata <- indata %>%
    mutate(target = tolower(!!rlang::sym(column))) %>%
    mutate(is_direct = stringr::str_detect(target, search_terms_processed),
           is_fuzzy = stringdist::ain(target, search_terms_raw, method = "lv", maxDist = max_dist_lv)) %>%
    select(-target)

  if (filter) {
    outdata %>% filter(is_direct | is_fuzzy)
  } else {
    outdata
  }
}


trt_identifer <- function(indata, search_terms, column, max_dist_lv = 1) {

  # Process search terms #
  search_terms_raw <- search_terms
  search_terms_processed <- search_terms_raw %>%
    unique() %>%
    tolower() %>%
    paste0(.,  collapse="|")

  # Identify
  outdata <- indata %>%
    mutate(target = tolower(!!rlang::sym(column))) %>%
    mutate(is_direct = stringr::str_detect(target, search_terms_processed),
           is_fuzzy = stringdist::ain(target, search_terms_raw, method = "lv", maxDist = max_dist_lv))

  # Augment query_aact is_trt
  if (is.null(outdata$is_trt)) {
    print("no is_trt found: identifying treatments")
    outdata$is_trt <- NA
    outdata$is_combo <- NA
  }
  outdata  <- outdata %>%
    mutate(is_trt_new = ifelse(!is.na(is_trt), is_trt,
                               ifelse(is.na(is_trt) & (is_direct | is_fuzzy), 1, 0)))

  # Augment query_aact is_combo
  combo_txt <- "[+,;]|\\bwith\\b|\\band\\b|\\bor\\b|\\bplus\\b|\\bcombination\\b"
  outdata <- outdata %>%
    mutate(is_combo_new = ifelse(!is.na(is_combo), is_combo,
                                 ifelse(is_trt_new==1 &
                                          stringr::str_detect(target, combo_txt), 1, 0)))

  print(with(outdata, table(is_trt, is_trt_new, useNA = "ifany")))
  print(with(outdata, table(is_combo, is_combo_new, useNA = "ifany")))

  # Output
  outdata %>%
    select(-is_trt, -is_combo, -is_direct, -is_fuzzy, -target) %>%
    rename(is_trt = is_trt_new, is_combo = is_combo_new)

}


# Filter for NCT IDs in date_range
filter_date_range <- function(dat, date_range) {
  # make date_range a Date class
  date_range <- as.Date(date_range)

  # Find NCT IDs within date_range
  dat %>%
    filter(between(study_first_submitted_date, date_range[1], date_range[2]) &
             between(completion_date, date_range[1], date_range[2]))
}

# Helper to drop columns (without using dplyr) #
drop_cols <- function(df, drop_vars) {
  df <- df[, !(colnames(df) %in% drop_vars)]
  return(df)
}
# Helper to re-name columns (without using dplyr) #
rename_cols <- function(df, org_cols, new_cols) {
  which_cols <- which(colnames(df) %in% org_cols)
  colnames(df)[which_cols] <- new_cols
  return(df)
}

