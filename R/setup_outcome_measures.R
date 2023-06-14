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


## Setup ORR Outcome Data ##
# @con: Connection
# @nct_ids: NCT identifiers
# @res_grps: Results group data set

setup_outcome_measures <- function(con,
                                   path_raw,
                                   file_type,
                                   board,
                                   is_shiny_data = NULL,
                                   data_preloaded = NULL,
                                   database,
                                   user_nct_ids,
                                   res_grps) {


  # load data
  outcome_measurements <- load_data(con = con,
                                    path_raw = path_raw,
                                    file_type = file_type,
                                    board = board,
                                    database = database,
                                    data_preloaded = data_preloaded,
                                    table_name =  "outcome_measurements",
                                    user_nct_ids = user_nct_ids)

  # Update data
  org_cols <- c("id", "title", "description")
  new_cols <- paste("outcome_measurements", org_cols, sep="_")
  outcome_measurements_processed <- rename_cols(df=outcome_measurements,
                                                org_cols = org_cols,
                                                new_cols = new_cols) %>%
    left_join(res_grps, by=c("nct_id", "result_group_id", "ctgov_group_code"))

  outcome_measurements_processed$units <- tolower(outcome_measurements_processed$units)
  outcome_measurements_processed$param_type <- tolower(outcome_measurements_processed$param_type)

  return(outcome_measurements_processed)
}
