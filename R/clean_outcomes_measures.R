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

globalVariables(c("outcome_cat", "param_value_num", "dispersion_lower_limit",
                  "dispersion_upper_limit", "time_scale", "time_ind",
                  "units_lower", "nct_id", "outcome_id", "result_group_id"))

# clean_outcomes_measures (outcome_measurements table)
clean_outcomes_measures <- function(indata) {
  # Fix ORR (proportion ==> percentage)
  sub_orr <- indata %>%
    filter(outcome_cat=="orr") %>%
    mutate(param_value_num = ifelse(str_detect(tolower(units), "proportion"), 100 * param_value_num, param_value_num)) %>%
    mutate(dispersion_lower_limit = ifelse(str_detect(tolower(units), "proportion"), 100 * dispersion_lower_limit,
                                           dispersion_lower_limit)) %>%
    mutate(dispersion_upper_limit = ifelse(str_detect(tolower(units), "proportion"), 100 * dispersion_upper_limit,
                                           dispersion_upper_limit),
           param_value = as.character(param_value_num),
           units_new = "%")
  # Fix PFS/OS (median survival to months) #
  sub_surv <- indata %>%
    filter(outcome_cat %in% c("pfs", "os")) %>%
    mutate(units_lower = tolower(units)) %>%
    # Fuzzy string search on units
    mutate(time_ind = case_when(
      str_detect(units_lower, "day") |  stringdist::ain(units_lower, "day", method = "lv", maxDist = 2) ~ "day",
      str_detect(units_lower, "week") |  stringdist::ain(units_lower, "week", method = "lv", maxDist = 2) ~ "week",
      str_detect(units_lower, "month|mothnh|monht") |  stringdist::ain(units_lower, "month|mothnh|monht", method = "lv", maxDist = 2) ~ "month",
      TRUE ~ units_lower
    )) %>%
    mutate(time_scale = case_when(
      time_ind == "day" ~ 30.25,
      time_ind == "week" ~ 30.25 / 7,
      TRUE ~ 1
    )) %>%
    mutate(param_value_num = param_value_num / time_scale) %>%
    mutate(dispersion_lower_limit = dispersion_lower_limit / time_scale) %>%
    mutate(dispersion_upper_limit = dispersion_upper_limit / time_scale) %>%
    select(-time_ind) %>%
    mutate(param_value = as.character(param_value_num),
           units_new = "month") %>%
    select(-c(time_scale, units_lower))
  # Other outcomes #
  sub_other <- sub_surv %>% filter(!(outcome_cat %in% c("pfs", "os", "orr")))

  # Re-combine #
  sub_all <- sub_orr %>%
    rbind(sub_surv) %>%
    rbind(sub_other) %>%
    arrange(nct_id, outcome_id, result_group_id, outcome_cat)

  sub_all
}
