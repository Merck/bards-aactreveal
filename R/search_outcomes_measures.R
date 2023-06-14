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


globalVariables(c("outcome_measurements_title", "outcome_measurements_title_lower",
                  "param_type"))

search_outcomes_measures <- function(indata,
                                     outcome_type = c("OS", "ORR", "PFS"),
                                     outcome_list = NULL,
                                     outcome_name = NULL) {


  # Make sure its in the right format for loop #
  search_terms <- outcome_list
  if (any(names(search_terms) %in% c("include", "exclude", "units", "type", "range"))) {
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
    # exclude
    exclude_ii_raw <- tolower(search_terms[[ii]]$exclude)
    exclude_ii_collapsed <- paste0(exclude_ii_raw, collapse = "|")
    # units
    units_ii_raw <- tolower(search_terms[[ii]]$units)
    units_ii_collapsed <- paste0(units_ii_raw, collapse = "|")
    # type
    type_ii_raw <- tolower(search_terms[[ii]]$type)
    type_ii_collapsed <- tolower(paste(type_ii_raw, collapse = "|"))
    # range
    range_ii <- search_terms[[ii]]$range

    # 1: Search for include term (Direct or Fuzzy)
    if(!is.character(include_ii_raw))
      stop("'include' should be character")

    sub_dat1 <- indata %>%
      # outcome_measurements_title: treatment-specific parameter
      mutate(outcome_measurements_title_lower = tolower(outcome_measurements_title)) %>%
      filter(stringr::str_detect(outcome_measurements_title_lower, include_ii_collapsed) |
               stringdist::ain(outcome_measurements_title_lower, include_ii_raw, method = "lv", maxDist = 1)) %>%
      mutate(units_lower = tolower(units))
    # 2: Search for units term (Direct or Fuzzy)
    if (length(units_ii_raw)>0) {
      if(!is.character(units_ii_raw))
        stop("'units' should be character")
      sub_dat2 <- sub_dat1 %>%
        filter(stringr::str_detect(units_lower, units_ii_collapsed) |
                 stringdist::ain(units_lower, units_ii_raw, method = "lv", maxDist = 2))
    } else {
      sub_dat2 <- sub_dat1
    }
    # 3: Exclude term (Direct)
    if (length(exclude_ii_raw)>0) {
      if(!is.character(exclude_ii_raw))
        stop("'exclude' should be character")
      sub_dat3 <- sub_dat2 %>%
        filter(!(stringr::str_detect(outcome_measurements_title_lower, exclude_ii_collapsed))) %>%
        filter(!(outcome_measurements_title_lower %in% exclude_ii_raw))
    } else {
      sub_dat3 <- sub_dat2
    }
    # 4: Search for param_type term (Direct or Fuzzy)
    if (length(type_ii_raw)>0) {
      if(!is.character(type_ii_raw))
        stop("'type' should be character")
      sub_dat4 <- sub_dat3 %>%
        # param_type: e.g. median, number ,etc
        filter(stringr::str_detect(tolower(param_type), type_ii_collapsed))
    } else {
      sub_dat4 <- sub_dat3
    }
    # 5: Search for param_values within range
    if (length(range_ii)>0) {
      if(!(is.double(range_ii) | is.integer(range_ii)))
        stop("'range' should be numeric")
      sub_dat5 <- sub_dat4 %>%
        filter(!is.na(param_value_num)) %>%
        filter(between(param_value_num, range_ii[1], range_ii[2]))
    } else {
      sub_dat5 <- sub_dat4
    }

    final_list[[ii]] <- sub_dat5 %>% select(-c(outcome_measurements_title_lower, units_lower))
  }

  # final output
  final_dat <- bind_rows(final_list, .id ="outcome_cat")
  final_dat
}
