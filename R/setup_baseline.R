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


## Setup Baseline Data ##
# @con: Connection
# @path_raw: Path for raw AACT data
# @user_nct_ids: NCT identifiers
# @res_grps: Results group data set

globalVariables(c("result_type", "scope", "value", "title", "param_type",
                  "classification", "category", "param", "tot.value"))

setup_baseline <- function(con,
                           path_raw,
                           file_type,
                           board,
                           database,
                           is_shiny_data = FALSE,
                           data_preloaded = NULL,
                           user_nct_ids,
                           res_grps) {
  # variables to keep
  keep_vars <- c("nct_id", "result_group_id", "ctgov_group_code", "value", "param")

  # load data
  baseline_measurements <- load_data(con = con,
                                     path_raw = path_raw,
                                     file_type = file_type,
                                     board = board,
                                     database = database,
                                     data_preloaded = data_preloaded,
                                     table_name = "baseline_measurements",
                                     user_nct_ids = user_nct_ids) %>%
    arrange(nct_id, result_group_id, ctgov_group_code)
  # load baseline counts
  baseline_counts <- load_data(con = con,
                               path_raw = path_raw,
                               file_type = file_type,
                               board = board,
                               database = database,
                               data_preloaded = data_preloaded,
                               table_name = "baseline_counts",
                               user_nct_ids = user_nct_ids)



  # Merge
  counts <- baseline_counts %>%
    left_join(res_grps,
              by = c("nct_id", "result_group_id", "ctgov_group_code")) %>%
    arrange(nct_id, ctgov_group_code, result_group_id) %>%
    # Make sure we only use "participant" counts and "baseline" #
    filter(result_type == "Baseline" & units == "Participants" & scope == "overall") %>%
    select(-result_type, -units, -scope) %>%
    rename(value = count) %>%
    mutate(param = "base_n") %>%
    select(all_of(keep_vars)) %>%
    mutate(value = as.character(value))

  # Age #
  age1_dat <- baseline_measurements %>% filter(title=="Age, Continuous" & param_type == "Mean")
  age2_dat <- baseline_measurements %>% filter(title=="Age, Customized" & param_type == "Mean")
  age_dat <- bind_rows(age1_dat, age2_dat)
  age_dat$value <- age_dat$param_value
  age_dat$param <- "age"
  age_dat <- age_dat %>% select(all_of(keep_vars))

  # Geographic Region #
  region1 <- baseline_measurements %>% filter(title=="Geographic Region")
  region2 <- baseline_measurements %>% filter(title=="Region")
  region3 <- baseline_measurements %>% filter(title=="Region of Enrollment")

  region_dat <- region1 %>%
    bind_rows(region2) %>%
    bind_rows(region3) %>%
    mutate(param = ifelse(is.na(classification), as.character(category), as.character(classification))) %>%
    mutate(param = tolower(param)) %>%
    mutate(param = ifelse(stringr::str_detect(param, "rest") | stringr::str_detect(param, "row"), "rest of world", param))

  region_dat$value <- region_dat$param_value
  region_dat$var <- "region"

  asia <- c("asia", "australia", "australia/new zealand",
            "china", "east asia", "singapore")
  east_asia <- c("asia", "east asia", "singapore", "south korea")

  # Categorize regions #
  # north_am <- c("canada", "north america", "united states", "us/canada", "usa")
  # europe <- c("austria", "belgium", "europe", "france", "hungary", "italy",
  #             "spain", "switzerland", "western europe")

  region_dat_processed <- region_dat %>%
    select(all_of(keep_vars), param) %>%
    # Obtain percentages #
    group_by(nct_id, result_group_id, ctgov_group_code) %>%
    mutate(tot.value = sum(as.numeric(value))) %>%
    ungroup() %>%
    mutate(value = as.numeric(value) / tot.value) %>%
    mutate(value = as.character(value)) %>%
    select(-tot.value) %>%
    mutate(param = ifelse(param %in% east_asia, "east_asia",
                          "non_east_asia"),
           param = as.character(param)) %>%
    unique() %>%
    # Deal with duplicates #
    mutate(value = as.numeric(value)) %>%
    group_by(nct_id, result_group_id, ctgov_group_code, param) %>%
    summarise(value = mean(value, na.rm=TRUE)) %>%
    mutate(value = as.character(value))

  # # Turn to wide (each region as a column)
  # wide_region <- region_dat %>% spread(param, value)

  # ECOG (Eastern Cooperative Oncology Group (ECOG) Performance Status) #
  ecog_vals <- paste(seq(0,4), collapse="|")
  ecog1 <- baseline_measurements %>%
    filter(stringr::str_detect(tolower(title), "ecog")) %>%
    mutate(param = ifelse(is.na(classification), category, classification)) %>%
    mutate(param = ifelse(stringr::str_detect(param, "0") | param=="Fully active", "0",
                          ifelse(stringr::str_detect(param, "1|Restricted in physically strenuous activity"), "1",
                                 ifelse(stringr::str_detect(param, "2"), "2",
                                        ifelse(stringr::str_detect(param, "3"), "3",
                                               ifelse(stringr::str_detect(param, "4"), "4",
                                                      NA)
                                        )
                                 )
                          )
    )
    ) %>%
    mutate(param = recode(as.character(param), `2`="24", `3`="24", `4`="24"))

  ecog_dat <- ecog1 %>% mutate(param = paste("ecog", param, sep="_"))
  ecog_dat$value <- ecog_dat$param_value
  ecog_dat <- ecog_dat[,c(keep_vars)]

  # Obtain percentages #
  ecog_dat <- ecog_dat %>%
    group_by(nct_id, result_group_id, ctgov_group_code) %>%
    mutate(tot.value=sum(as.numeric(value))) %>%
    ungroup() %>%
    mutate(value = as.numeric(value)/tot.value) %>%
    select(-tot.value) %>%
    # Deal with duplicates #
    mutate(value = as.numeric(value)) %>%
    group_by(nct_id, result_group_id, ctgov_group_code, param) %>%
    summarise(value = mean(value, na.rm=TRUE)) %>%
    mutate(value = as.character(value))

  # # Turn to wide (each region as a column)
  # wide_ecog <- ecog_dat %>% spread(param, value)

  # Sex #
  sex1 <- baseline_measurements %>% filter(title=="Sex: Female, Male")
  sex2 <- baseline_measurements %>% filter(title=="Sex/Gender, Customized")

  sex_dat <- sex1 %>%
    bind_rows(sex2) %>%
    mutate(param = ifelse(is.na(classification), as.character(category), as.character(classification)))

  sex_dat$value <- sex_dat$param_value
  sex_dat <- sex_dat[,c(keep_vars)]

  # Obtain percentages #
  sex_dat <- sex_dat %>%
    group_by(nct_id, result_group_id, ctgov_group_code) %>%
    mutate(tot.value=sum(as.numeric(value))) %>%
    ungroup() %>%
    mutate(value = as.character(as.numeric(value)/tot.value),
           param = as.character(param)) %>%
    select(-tot.value)

  ## PDL1 ##
  pdl1.names <- paste(c("pd-l1", "programmed cell death ligand 1",
                        "programmed cell death-ligand 1"), collapse="|")

  pd_1 <- baseline_measurements %>%
    filter(stringr::str_detect(tolower(title), pdl1.names)) %>%
    mutate(param = ifelse(is.na(classification), category, classification))

  pd_1$value <- pd_1$param_value
  pd_1 <- pd_1[,c(keep_vars)]

  # TPS #
  uniq_params <- unique(tolower(pd_1 %>% pull(param)))
  uniq_params <- uniq_params[!stringr::str_detect(uniq_params, "cps|total per cohort")]

  unk_found <- uniq_params[stringr::str_detect(uniq_params,
                                               "missing|unknown|indeterminate|not evaluable|not quantifiable|undetermined")]
  unk_vec <- unique(c(unk_found))
  neg_found <- uniq_params[stringr::str_detect(uniq_params,
                                               "tps=0%|negative|tps <1%|quantifiable <1%|tps=<1%|pd-l1 expression <1%") &
                             !is.na(uniq_params)]
  neg_vec <- neg_found
  pos_vec <- setdiff(uniq_params, c(unk_vec, neg_vec))
  pd_dat <- pd_1 %>%
    mutate(param = tolower(param)) %>%
    mutate(param = ifelse(param %in% unk_found, "unk",
                          ifelse(param %in% neg_found, "neg", "pos"))) %>%
    mutate(param = paste("pdl1", param, sep="_"))
  # Sum up counts (within param) #

  pd_dat <- pd_dat %>%
    group_by(nct_id, result_group_id, ctgov_group_code, param) %>%
    summarise(value = sum(as.numeric(value)))

  # Obtain percentages #
  pd_dat <- pd_dat %>%
    group_by(nct_id, result_group_id, ctgov_group_code) %>%
    mutate(tot.value=sum(as.numeric(value))) %>%
    ungroup()

  pd_dat <- pd_dat %>%
    mutate(value = as.character(as.numeric(value)/tot.value)) %>%
    select(-tot.value)

  base_dat <- age_dat %>%
    mutate(value = as.character(value)) %>%
    # Combine datasets #
    bind_rows(counts, sex_dat, ecog_dat,
              region_dat_processed, pd_dat) %>%
    # Add Arm/Group Title
    inner_join(res_grps, by = c("nct_id", "ctgov_group_code", "result_group_id")) %>%
    select(nct_id, result_group_id, ctgov_group_code, result_type, groups_title, groups_descr, value, param)

  base_dat
}
