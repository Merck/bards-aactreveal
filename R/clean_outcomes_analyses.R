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

clean_outcomes_analyses <- function(indata) {
  indata <- indata %>%
    mutate(estimand = case_when(
      .data$outcome_cat == "orr" & str_detect(tolower(param_type), "difference") ~ "diff",
      .data$outcome_cat == "orr" & str_detect(tolower(param_type), "odds ratio") ~ "OR",
      .data$outcome_cat == "orr" & str_detect(tolower(param_type), "relative risk|risk ratio") ~ "RR",
      .data$outcome_cat == "pfs" & str_detect(tolower(param_type), "hazard ratio|cox") ~ "HR",
      .data$outcome_cat == "os" & str_detect(tolower(param_type), "hazard ratio|cox") ~ "HR",
                                                                          TRUE  ~ NA_character_
    )) %>%
    relocate(.data$outcome_cat, .after = last_col())

  indata
}
