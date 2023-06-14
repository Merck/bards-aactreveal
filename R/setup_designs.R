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


## Extract Study Design ##
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers

setup_designs <- function(con,
                          path_raw,
                          file_type,
                          board,
                          database,
                          is_shiny_data = FALSE,
                          data_preloaded = NULL,
                          user_nct_ids) {

  # Load data
  designs <- load_data(con = con,
                       path_raw = path_raw,
                       file_type = file_type,
                       board = board,
                       database = database,
                       data_preloaded = data_preloaded,
                       user_nct_ids = user_nct_ids,
                       table_name = "designs")

  # Features (only include allocaton, intervention_model for now)
  out_sub <- designs %>%
    select(.data$nct_id, .data$allocation, .data$intervention_model) %>%
    mutate(allocation = tolower(.data$allocation),
           intervention_model = tolower(.data$intervention_model))

  out_sub
}
