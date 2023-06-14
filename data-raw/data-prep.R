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


# Code to prepare static daily copy of database inside R package

# Instructions to download static copy of AACT Database:
# 1. Download zip file of static copies from https://aact.ctti-clinicaltrials.org/snapshots.
# 2. Upload the zip file to RStudio environment inside static-copy folder

library(readr)
library(dplyr)
library(stringr)

# Get NCT Ids from conditions table
conditions <- read_delim("static-copy/conditions.txt", delim = "|")
oncology_nct_id <- conditions %>%
  filter(str_detect(downcase_name, "oncology|malignant neoplasm|cancer")) %>%
  pull(nct_id) %>%
  unique()


# Read in txt file (oncology trials only) and assign each file to an object in global env
txt_files <- list.files(path = "./static-copy", pattern = "\\.txt$")
txt_list <- lapply(X = txt_files, FUN = function(x) {
  txt_files_path <- paste0("./static-copy/", x)
  read_delim(txt_files_path, delim = "|") %>%
    filter(nct_id %in% oncology_nct_id)
})
names(txt_list) <- tools::file_path_sans_ext(txt_files)
list2env(txt_list, envir = .GlobalEnv)


usethis::use_data(baseline_counts,
                  baseline_measurements,
                  browse_interventions,
                  conditions,
                  design_group_interventions,
                  design_groups,
                  designs,
                  interventions,
                  outcome_analyses,
                  outcome_analysis_groups,
                  outcome_measurements,
                  sponsors,
                  studies,
                  overwrite = TRUE)
