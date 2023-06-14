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


## Extract Study Parameters (phase, dates) ##
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers

setup_studies <- function(con,
                          path_raw,
                          file_type,
                          board,
                          database,
                          is_shiny_data = FALSE,
                          data_preloaded = NULL,
                          user_nct_ids) {

  # load data
  studies <- load_data(con = con,
                       path_raw = path_raw,
                       file_type = file_type,
                       board = board,
                       database = database,
                       data_preloaded = data_preloaded,
                       user_nct_ids = user_nct_ids,
                       table_name = "studies")

  # Keep certain variables #
  keep_vars <- c("nct_id", "study_first_submitted_date",
                 "results_first_submitted_date",
                 "start_date", "completion_date", "primary_completion_date",
                 "study_type", "phase", "overall_status", "enrollment",
                 "brief_title", "official_title",
                 "number_of_arms")

  out_sub <- studies %>%
    select(all_of(keep_vars)) %>%
    mutate(phase = tolower(.data$phase)) %>%
    mutate(phase = ifelse(phase=="n/a", NA, phase)) %>%
    mutate(number_of_arms = as.character(.data$number_of_arms),
           enrollment = as.character(.data$enrollment),
           study_first_submitted_date = as.Date(.data$study_first_submitted_date),
           results_first_submitted_date = as.Date(.data$results_first_submitted_date),
           start_date = as.Date(.data$start_date),
           completion_date = as.Date(.data$completion_date),
           primary_completion_date = as.Date(.data$primary_completion_date),
           study_type = as.character(.data$study_type))

  out_sub

}
