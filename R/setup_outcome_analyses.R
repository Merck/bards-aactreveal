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


## Parameter Estimates (point-estimates, LCL/UCL, p-values, primary, secondary etc) ##
# @con: Connection
# @nct_ids: NCT identifiers
# @res_grps: Results group data set

globalVariables(c("non_inferiority_type", "param_type", "param_value",
                  "p_value", "p_value_description", "ci_lower_limit",
                  "ci_upper_limit", "p_hold", "pdescr_hold",
                  "outcome_analysis_id", "ctgov_group_code", "trt",
                  "result_type", "trt_all", "title", "outcome_type",
                  "outcome_title", "trt_descr"))

setup_outcome_analyses <- function(con,
                                   path_raw,
                                   file_type,
                                   board,
                                   is_shiny_data = NULL,
                                   data_preloaded = NULL,
                                   database,
                                   user_nct_ids,
                                   res_grps,
                                   outcome_measures = NULL) {

  # load data
  out_dat1_raw <- load_data(con = con,
                            path_raw = path_raw,
                            file_type = file_type,
                            board=board,
                            database = database,
                            data_preloaded = data_preloaded,
                            table_name = "outcome_analyses",
                            user_nct_ids = user_nct_ids)
  # load outcome_analysis_groups table
  out_dat2_raw <- load_data(con = con,
                            path_raw = path_raw,
                            file_type = file_type,
                            board = board,
                            database = database,
                            data_preloaded = data_preloaded,
                            table_name = "outcome_analysis_groups",
                            user_nct_ids = user_nct_ids)
  # load outcomes
  out_type_raw <- load_data(con = con,
                            path_raw = path_raw,
                            file_type = file_type,
                            board = board,
                            database = database,
                            data_preloaded = data_preloaded,
                            table_name = "outcomes",
                            user_nct_ids = user_nct_ids)


  # Preprocessing
  out_dat1 <- out_dat1_raw %>%
    select(id, nct_id, outcome_id, non_inferiority_type,
           param_type, param_value, p_value, p_value_description,
           ci_lower_limit, ci_upper_limit) %>%
    arrange(nct_id, outcome_id) %>%
    rename(outcome_analysis_id = id) %>%
    # In some cases, the p-value is on another row. Fix this.
    group_by(nct_id, outcome_id) %>%
    mutate(p_hold = mean(p_value, na.rm = TRUE),
           pdescr_hold = paste0(p_value_description, collapse = "|")) %>%
    ungroup() %>%
    arrange(nct_id, outcome_id) %>%
    filter(!is.na(param_type)) %>%
    mutate(p_value = ifelse(is.na(p_value), p_hold, p_value)) %>%
    mutate(p_value_description = ifelse(is.na(p_value_description), pdescr_hold, p_value_description)) %>%
    select(-p_hold, -pdescr_hold) %>%
    arrange(nct_id, outcome_analysis_id)

  out_dat2 <- out_dat2_raw %>%
    select(-id) %>%
    arrange(nct_id, outcome_analysis_id) %>%
    left_join(res_grps,
              by = c("nct_id", "result_group_id", "ctgov_group_code")
    ) %>%
    mutate(groups_title = tolower(groups_title)) %>%
    arrange(nct_id, outcome_analysis_id, ctgov_group_code) %>%
    group_by(nct_id, outcome_analysis_id) %>%
    mutate(groups_all = paste0(groups_title, collapse = " | ")) %>%
    select(-result_type)



  ## Combine Analyses with Group ##
  out_dat <- out_dat1 %>%
    left_join(out_dat2, by = c("nct_id", "outcome_analysis_id")) %>%
    mutate(ref = stringr::str_trim(sub(".*\\|", "", groups_all)), # specify the reference group (ex. chemo)
           comp = stringr::str_trim(sub("\\|.*", "", groups_all))) # specify the comparison group (ex. pembro )



  # load outcome_measurements (for outcome title)
  if (is.null(outcome_measures)) {
    # load data
    hold <- load_data(con = con,
                      path_raw = path_raw,
                      file_type = file_type,
                      board = board,
                      database = database,
                      data_preloaded = data_preloaded,
                      table_name = "outcome_measurements",
                      user_nct_ids = user_nct_ids) %>%
      select(nct_id, outcome_id, title) %>%
      rename(outcome_measurements_title = title) %>%
      unique()
  } else {
    hold <- outcome_measures %>%
      select(nct_id, outcome_id, outcome_measurements_title) %>%
      unique()
  }

  out_dat <- left_join(out_dat, hold, by = c("nct_id", "outcome_id"))

  # Preprocessing
  out_type <- out_type_raw %>%
    filter(nct_id %in% user_nct_ids) %>%
    select(id, nct_id, outcome_type) %>%
    rename(outcome_id = id)


    ## Merge ##
  full <- out_dat %>%
    left_join(out_type, by = c("nct_id", "outcome_id")) %>%
    rename(param_comparison = groups_all) %>%
    mutate(param_type = tolower(param_type))

  order_vars <- c(
    "nct_id", "outcome_id", "outcome_analysis_id", "ctgov_group_code",
    "result_group_id", "groups_title", "groups_descr",
    "outcome_measurements_title",
    "param_comparison", "comp", "ref",
    "param_type", "param_value", "p_value",
    "p_value_description", "ci_lower_limit", "ci_upper_limit",
    "non_inferiority_type", "outcome_type"
  )

  full <- full %>%
    relocate(all_of(order_vars)) %>%
    rename(title = outcome_measurements_title)

  return(full)
}
