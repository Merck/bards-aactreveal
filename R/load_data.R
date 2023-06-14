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


#' load_data: Load AACT data from local or AACT database
#'
#' Function to load a single AACT data-table and return as data table. This function
#' is also used internally for the "load_aact_data" function.
#'
#' @inheritParams extract_aact
#' @param table_name AACT table-name (ex: conditions, interventions)
#' @param is_shiny_data Use tables that exist in the global environment. For Shiny apps.
#' @param user_nct_ids Vector of NCT IDs to filter search on (default=NULL; no filter)
#'
#' @return AACT data table
#'
#' @export
#' @import selectr
#' @importFrom readr read_delim
#' @importFrom odbc dbGetQuery

load_data <- function(con=NULL,
                      path_raw=NULL,
                      file_type=NULL,
                      board=NULL,
                      data_preloaded=NULL,
                      table_name,
                      user_nct_ids=NULL,
                      database=NULL) {

  if (!is.null(data_preloaded)) {
    message("Using pre-loaded data")
    outdat <- data_preloaded[[table_name]]
    if (!is.null(user_nct_ids)) {
      outdat <- outdat %>%
        filter(nct_id %in% user_nct_ids)
    }
    return(outdat)
  } else if (!is.null(board)) {
    message("Using board")
    if (is.null(path_raw)) {
      stop("Must provide path_raw when loading with board argument")
    }
    suppressWarnings(outdat <- board %>% pins::pin_read(paste(path_raw, table_name, sep="")))
    if (!is.null(user_nct_ids)) {
      outdat <- outdat %>%
        filter(nct_id %in% user_nct_ids)
    }
    return(outdat)

  } else if (!is.null(path_raw)) {
    message("Using path")
    if (file_type=="txt") {
      # print("txt")
      fname <- paste0(path_raw, "/", table_name, ".txt")
      suppressWarnings(outdat <- readr::read_delim(fname, delim = "|",
                                                   show_col_types = FALSE))
    } else if (file_type=="rda") {
      load(file = paste0(path_raw, "/", table_name, ".rda"))
      outdat <- get(table_name)
    }
    if (!is.null(user_nct_ids)) {
      outdat <- outdat %>%
        filter(nct_id %in% user_nct_ids)
    }
    return(outdat)

  } else if (!is.null(con)) {
    message("Using SQL")
    if (is.null(database)) {
      sql_text <- paste("SELECT * FROM", table_name, sep=" ")
    } else {
      sql_text <- paste("SELECT * FROM", paste(database, table_name, sep="."), sep=" ")
    }
    if (!is.null(user_nct_ids)) {
      quoted_ids <- mapply(paste0, "'", user_nct_ids, "'", USE.NAMES = FALSE)
      quoted_ids <- paste(quoted_ids, collapse=",")
      sql_text <- paste(sql_text, "WHERE nct_id IN", "(", quoted_ids, ")")
    }
    outdat <- odbc::dbGetQuery(con, sql_text)
    return(outdat)
  } else {
    message("Check inputs into load_data function: Loading aactreveal package data")
    outdat <- get(table_name)
    if (!is.null(user_nct_ids)) {
      outdat <- outdat %>%
        filter(nct_id %in% user_nct_ids)
    }
    return(outdat)
  }
}
