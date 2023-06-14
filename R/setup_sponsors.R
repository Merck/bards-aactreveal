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


## Setup Sponser Data ##
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers

setup_sponsors <- function(con,
                           path_raw,
                           file_type,
                           board,
                           database,
                           is_shiny_data = FALSE,
                           data_preloaded = NULL,
                           user_nct_ids) {

  # Load data
  sponsors <- load_data(con = con,
                        path_raw = path_raw,
                        file_type = file_type,
                        board = board,
                        database = database,
                        data_preloaded = data_preloaded,
                        user_nct_ids = user_nct_ids,
                        table_name = "sponsors")


  # Separate out lead and collaborator #
  lead_df <- sponsors %>%
    filter(.data$lead_or_collaborator=="lead") %>%
    select(.data$nct_id, sponsor_name = .data$name) %>%
    unique()

  collab_df <- sponsors %>%
    filter(.data$lead_or_collaborator=="collaborator") %>%
    select(.data$nct_id, sponsor_collab = .data$name) %>%
    group_by(.data$nct_id) %>%
    mutate(sponsor_collab = paste(.data$sponsor_collab, collapse = ";")) %>%
    ungroup() %>%
    unique()

  out_dat <- lead_df %>% left_join(collab_df, by="nct_id")

  out_dat

}
