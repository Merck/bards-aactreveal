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


#' load_aact_data: Load AACT data into the global R environment
#'
#' Function that loads AACT data-tables into the R environment. Can be used to pre-load data, which can make
#' queries a bit faster.
#'
#' @inheritParams extract_aact
#' @param table_names Vector of AACT table-name(s) to load (ex: c("conditions", "interventions") )
#' @param user_nct_ids Vector of NCT IDs to filter search on (default=NULL; no filter)
#' @param env Environment to save the tables (default=.GlobalEnv)
#'
#' @export
#'
load_aact_data <- function(con, path_raw, board, table_names = NULL, user_nct_ids=NULL,
                           database=NULL,
                           env = .GlobalEnv,
                          verbose = TRUE) {

  if (is.null(table_names)) {
    table_names <- c("browse_interventions", "conditions", "design_group_interventions",
                     "design_groups",
                     "designs", "interventions", "outcome_analyses",
                     "outcome_analysis_groups",
                     "outcome_measurements", "result_groups",
                     "sponsors", "studies")
  }

  looper_load <- function(con, path_raw, table_name, user_nct_ids) {
    if (verbose) message(paste("Loading", table_name))
    dat <- load_data(con=con, path_raw=path_raw, board=board, table_name=table_name,
                     user_nct_ids=user_nct_ids, database=database)
    assign(table_name, dat, envir = env)
    return(paste("loaded", table_name))
  }
  outlist <- lapply(table_names, looper_load, con=con, path_raw=path_raw, board=board,
                    user_nct_ids=user_nct_ids, database=database)

  return("AACT Data Loading Complete")
}
