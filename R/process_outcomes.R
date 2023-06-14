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


#' process_outcomes: Process outcome measurements / analyses data-tables.
#'
#' Function that performs some pre-processing on the outcome measurements and
#' outcome analyses tables, along with pulling some information from the baseline data-table (work in progress).
#'
#' @inheritParams extract_aact
#' @param user_nct_ids Vector of unique NCT IDs (default=NULL)
#'
#' @export
#'
#'@seealso \code{\link{query_outcomes}}

# Setup outcome tables (outcome_measures_processed & outcome_analyses_processed)
# Function is needed in Shiny app as an intermediary function
process_outcomes <- function(con,
                             path_raw,
                             file_type,
                             board,
                             is_shiny_data,
                             data_preloaded,
                             user_nct_ids = NULL,
                             database) {

  # Load result groups for mapping
  result_groups <- setup_result_groups(con = con,
                                       path_raw = path_raw,
                                       file_type = file_type,
                                       board = board,
                                       is_shiny_data = is_shiny_data,
                                       data_preloaded = data_preloaded,
                                       user_nct_ids = user_nct_ids,
                                       database = database)

  # Outcomes Measurements (not parameters): Gives values in the Arm/Group table
  outcome_measures_raw <- setup_outcome_measures(con = con,
                                                 path_raw = path_raw,
                                                 file_type = file_type,
                                                 board = board,
                                                 is_shiny_data = is_shiny_data,
                                                 data_preloaded = data_preloaded,
                                                 user_nct_ids = user_nct_ids,
                                                 res_grps = result_groups,
                                                 database = database)

  # Baseline (demographics, counts, etc)
  baseline_dat <- setup_baseline(con = con,
                                 path_raw = path_raw,
                                 file_type = file_type,
                                 board = board,
                                 is_shiny_data = is_shiny_data,
                                 data_preloaded = data_preloaded,
                                 user_nct_ids = user_nct_ids,
                                 database = database,
                                 res_grps = result_groups)

  # Wrangle baseline_dat
  base_dat_wide <- baseline_dat %>%
    mutate(value = as.numeric(value)) %>%
    group_by(nct_id, param) %>%
    summarise(value = mean(value, na.rm=TRUE)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = param, values_from = value) %>%
    left_join(result_groups,  by=c("nct_id"))
  # Attach sample sizes to outcome_dat
  samp_dat <- base_dat_wide %>%
    select(nct_id, groups_title, base_n) %>%
    unique() %>%
    rename(groups_n = base_n)

  # Outcome Measurements: Outcome Measure Data
  outcome_measures_processed <- left_join(outcome_measures_raw, samp_dat, by=c("nct_id", "groups_title")) %>%
    # make unique name for param_value to differentiate param_value in outcome_analyses
    rename(trt_spec_param_value = param_value)

  # Outcomes Analyses: Gives values in Statistical Analysis tables(P-Value / Parameter Estimate / CI)
  outcome_analyses_processed <- setup_outcome_analyses(con = con,
                                                       path_raw = path_raw,
                                                       file_type = file_type,
                                                       board = board,
                                                       is_shiny_data = is_shiny_data,
                                                       data_preloaded = data_preloaded,
                                                       user_nct_ids = user_nct_ids,
                                                       database=database,
                                                       res_grps = result_groups,
                                                       outcome_measures = outcome_measures_processed)

  # Final touch up #
  drop_vars <- c("groups_descr")
  outcome_analyses_processed <- outcome_analyses_processed %>%
    rename(design_groups_title = groups_title) %>%
    mutate(design_groups_title = tolower(design_groups_title))

  outcome_analyses_processed <- drop_cols(df=outcome_analyses_processed, drop_vars)

  outcome_measures_processed <- outcome_measures_processed %>%
    rename(design_groups_title = groups_title) %>%
    mutate(design_groups_title = tolower(design_groups_title))
  outcome_measures_processed <- drop_cols(df=outcome_measures_processed, drop_vars)


  # Final output
  list(result_groups = result_groups, outcome_measures_processed = outcome_measures_processed,
       outcome_analyses_processed = outcome_analyses_processed,
       baseline_dat = baseline_dat)
}
