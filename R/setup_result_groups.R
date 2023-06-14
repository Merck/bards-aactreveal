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

## Setup Result Groups ##
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers

setup_result_groups <- function(con,
                                path_raw,
                                file_type,
                                board,
                                is_shiny_data = NULL,
                                data_preloaded = NULL,
                                database,
                                user_nct_ids) {

  # load data
  result_groups <- load_data(con = con,
                             path_raw = path_raw,
                             file_type = file_type,
                             board = board,
                             database = database,
                             data_preloaded = data_preloaded,
                             user_nct_ids = user_nct_ids,
                             table_name = "result_groups")



  # rename column names (use base R to avoid devtool warnings)
  result_groups_processed <- result_groups
  result_groups_processed <- rename_cols(df=result_groups,
                                         org_cols = c("id", "title", "description"),
                                         new_cols =  c("result_group_id", "groups_title", "groups_descr"))
  # result_groups_processed <- result_groups %>%
  #   rename(result_group_id = .data$id, groups_title = .data$title, groups_descr = .data$description)

  result_groups_processed
}
