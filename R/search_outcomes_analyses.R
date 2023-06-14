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


globalVariables(c("title", "param_type", "title_lower", "param_type_lower", "param_value"))

search_outcomes_analyses <- function(indata,
                                     outcome_type = c("OS", "ORR", "PFS"),
                                     outcome_list = NULL,
                                     outcome_name = NULL) {


  # Make sure its in the right format for loop #
  search_terms <- outcome_list
  if (any(names(search_terms) %in% c("include", "include_param_type", "exclude"))) {
    search_terms <- list(search_terms) # Need if only a single list is passed
  }
  if (is.null(outcome_name)) {
    names(search_terms) <- paste("outcome", seq(1, length(search_terms)), sep="_")
  } else {
    names(search_terms) <- outcome_name
  }

  # initialize final output
  final_list <- vector("list", length(search_terms))
  names(final_list) <- names(search_terms)

  for (ii in 1:length(search_terms)) {
    # extract out elements from list:
    # include
    include_ii_raw <- tolower(search_terms[[ii]]$include)
    include_ii_collapsed <- paste0(include_ii_raw, collapse = "|")
    # include_param_type
    include_param_type_ii_raw <- tolower(search_terms[[ii]]$include_param_type)
    include_param_type_ii_collapsed <- paste0(include_param_type_ii_raw, collapse = "|")
    # exclude
    exclude_ii_raw <- tolower(search_terms[[ii]]$exclude)
    exclude_ii_collapsed <- paste0(exclude_ii_raw, collapse = "|")

    # 1: Search for include term
    if(!is.character(include_ii_raw))
      stop("'include' should be character")
    sub_dat1 <- indata %>%
      # title: treatment specific parameter (e.g. overall survival)
      # param_type: treatment comparison parameter (e.g. hazard ratio)
      mutate(title_lower = tolower(title),
             param_type_lower = tolower(param_type)) %>%
      filter(stringr::str_detect(title_lower, include_ii_collapsed) |
               stringdist::ain(title_lower, include_ii_raw, method = "lv", maxDist = 1))
    # 2: Search for include_param_type term
    if (length(include_param_type_ii_raw)>0) {
      if(!is.character(include_param_type_ii_raw))
        stop("'include_param_type' should be character")
      # param_type: treatment comparison parameter (e.g. hazard ratio)
      sub_dat2 <- sub_dat1 %>%
        filter(stringr::str_detect(param_type_lower, include_param_type_ii_collapsed) |
                 stringdist::ain(param_type_lower, include_param_type_ii_raw, method = "lv", maxDist = 1))
    } else {
      sub_dat2 <- sub_dat1
    }
    # 3: Filter out exclude term
    if (length(exclude_ii_raw)>0) {
      if(!is.character(exclude_ii_raw))
        stop("'exclude' should be character")
      sub_dat3 <- sub_dat2 %>%
        filter(!(stringr::str_detect(title_lower, exclude_ii_collapsed))) %>%
        filter(!(title_lower %in% exclude_ii_raw))
    } else {
      sub_dat3 <- sub_dat2
    }
    # Filter out NA param_value and deselect 2 columns
    final_list[[ii]] <- sub_dat3 %>%
      filter(!is.na(param_value)) %>%
      select(-c(title_lower, param_type_lower))
  }

  # final output
  final_dat <- bind_rows(final_list, .id ="outcome_cat")
  final_dat
}
