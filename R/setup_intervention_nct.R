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


# treatment data setup (no search terms)
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers


globalVariables(c("name", "title", "downcase_mesh_term",
                  "mesh_term_collapsed", "design_group_id",
                  "intervention_name", "intervention_num",
                  "intervention_collapsed", "intervention_id",
                  "intervention_type", "intervention_description"))

setup_intervention_nct <- function(con,
                                   path_raw,
                                   file_type,
                                   board,
                                   database,
                                   is_shiny = FALSE,
                                   is_shiny_data = NULL,
                                   data_preloaded = NULL,
                                   user_nct_ids) {
  # Load data
  interventions <- load_data(con = con,
                             path_raw = path_raw,
                             file_type = file_type,
                             board = board,
                             database = database,
                             data_preloaded = data_preloaded,
                             user_nct_ids = user_nct_ids,
                             table_name = "interventions")
  browse_interventions <- load_data(con = con,
                                    path_raw = path_raw,
                                    file_type = file_type,
                                    board = board,
                                    database = database,
                                    user_nct_ids = user_nct_ids,
                                    data_preloaded = data_preloaded,
                                    table_name = "browse_interventions")
  design_groups <- load_data(con = con,
                             path_raw = path_raw,
                             file_type = file_type,
                             board = board,
                             database = database,
                             data_preloaded = data_preloaded,
                             user_nct_ids = user_nct_ids,
                             table_name = "design_groups")
  design_group_interventions <- load_data(con = con,
                                          path_raw = path_raw,
                                          file_type = file_type,
                                          board = board,
                                          database = database,
                                          data_preloaded = data_preloaded,
                                          user_nct_ids = user_nct_ids,
                                          table_name = "design_group_interventions")

  # join interventions and browse_interventions tables
  interventions_combined <- interventions %>%
    full_join(browse_interventions, by = "nct_id") %>%
    rename(intervention_description = description,
           intervention_name = name)
  nct_interventions_combined <- interventions_combined %>% pull_nct()
  # Design Groups
  design_groups_processed <- design_groups %>%
    rename(design_groups_description = description,
           design_groups_title = title)
  nct_design_groups_processed <- design_groups_processed %>% pull_nct()

  # Combine above steps to get final output
  nct_combined <- unique(c(nct_interventions_combined, nct_design_groups_processed))

  # prepare final output
  # interventions
  interventions_final <- interventions %>%
    filter(nct_id %in% nct_combined) %>%
    rename(intervention_description = description,
           intervention_name = name,
           intervention_id = id)
  # browse_interventions
  browse_interventions_final <- browse_interventions %>%
    filter(nct_id %in% nct_combined) %>%
    group_by(nct_id) %>%
    mutate(mesh_term_collapsed = paste(downcase_mesh_term, collapse = ",")) %>%
    select(nct_id, mesh_term_collapsed) %>%
    unique()
  # design_groups
  design_groups_final <- design_groups %>%
    filter(nct_id %in% nct_combined) %>%
    rename(design_groups_description = description,
           design_groups_title = title,
           design_group_id = id)

  # final output
  trt_dat <- design_group_interventions %>%
    inner_join(interventions_final, by = c("nct_id", "intervention_id")) %>%
    full_join(design_groups_final, by = c("nct_id", "design_group_id")) %>%
    full_join(browse_interventions_final, by = "nct_id") %>%
    group_by(nct_id, design_group_id) %>%
    mutate(intervention_collapsed = paste(intervention_name, collapse = " + "),
           # find initial intervention number
           intervention_num = length(intervention_name)) %>%
    ungroup() %>%
    mutate(# modify intervention_number
      intervention_num =  map2_int(intervention_num, intervention_collapsed, find_intervention_num)) %>%
    select(-id, -intervention_id, -intervention_type, -intervention_name, -intervention_description) %>%
    unique() %>%
    rename(intervention_total = intervention_collapsed,
           mesh_term_total = mesh_term_collapsed)
  return(trt_dat)
}
