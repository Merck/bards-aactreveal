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


#' Baseline Counts Dataset
#'
#' Sample size at baseline for each study group; usually a count of participants
#' but can represent other units of measure such as 'hands', 'hips', etc.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 13834 rows and 7 variables.
#'
"baseline_counts"

#' Baseline Measurements Dataset
#'
#' Summaries of demographic & baseline measures collected by arm or comparison group
#' and for the entire population of participants in the clinical study.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 171163 rows and 18 variables.
#'
"baseline_measurements"

#' Brief Summaries Dataset
#'
#' A single text column that provides a brief description of the study.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 42218 rows and 3 variables.
#'
"brief_summaries"

#' Browse Interventions Dataset
#'
#' NLM uses an internal algorithm to assess the data entered for a study and creates a list of standard
#' MeSH terms that describe the intervention(s) being addressed by the clinical trial. This table provides the results of NLM's assessment.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 185518 rows and 5 variables.
"browse_interventions"


#' Conditions Dataset
#'
#' Name(s) of the disease(s) or condition(s) studied in the clinical study, or the focus of the clinical study.
#' Can include NLM's Medical Subject Heading (MeSH)-controlled vocabulary terms.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 100806 row and 4 variables.
"conditions"


#' Design Group Interventions Dataset
#'
#' A cross reference for groups/interventions.  If a study has multiple groups and multiple interventions,
#' this table shows which interventions are associated with which groups.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 102263 row and 4 variables.
"design_group_interventions"

#' Design Groups Dataset
#'
#' Defines the protocol-specified group, subgroup, or cohort of participants in a
#' clinical trial assigned to receive specific intervention(s) or observations according to a protocol.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 65064 rows and 5 variables.
#'
"design_groups"

#' Designs Dataset
#'
#' Description of how the study will be conducted,
#' including comparison group design and strategies for masking and allocating participants.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 41904 rows and 14 variables.
#'
"designs"


#' Interventions Dataset
#'
#' The interventions or exposures (including drugs, medical devices, procedures, vaccines, and other products)
#' of interest to the study, or associated with study arms/groups.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 83911 rows and 5 variables.
#'
"interventions"

#' Keywords Dataset
#'
#'Provides words or phrases that best describe the protocol. Keywords help users find studies in the database.
#'Can include NLM's Medical Subject Heading (MeSH)-controlled vocabulary terms.
#'
#'This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#'
#' @format A data frame with 145789 rows and 4 variables.
"keywords"

#' Outcome Analyses Dataset
#'
#' Results of scientifically appropriate statistical analyses performed on primary and secondary study outcomes. Includes results
#' for treatment effect estimates, confidence intervals and othe rmeasures of dispersion, and p-values.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#'
#' @format A data frame with 9600 rows and 22 variables.
"outcome_analyses"

#' Outcome Analysis Dataset
#'
#' Identifies the comparison groups that were involved with each outcome analysis.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 18015 rows and 5 variables.
"outcome_analysis_groups"


#' Outcome Measurements Dataset
#'
#' Summary data for primary and secondary outcome measures for each study group.
#'  Includes parameter estimates and measures of dispersion/precision.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 222774 row and 19 variable.
"outcome_measurements"


#' Outcomes Dataset
#'
#' Descriptions of outcomes, or observation that were measured to determine patterns of diseases or traits, or associations with exposures, risk factors, or treatment. Includes information such as time frame, population and units.
#'  (Specific measurement results are stored in the Outcome_Measurements table.)
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 36225 rows and 13 variables.
"outcomes"


#' Result Groups Dataset
#'
#' Consolidated, aggregate list of group titles/descriptions used for reporting summary results information.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 54057 rows and 6 variables.
"result_groups"


#' Sponsors Dataset
#'
#' Name of study sponsors and collaborators. The sponsor is the entity or individual initiating the study. Collaborators are other organizations providing support,
#' including funding, design, implementation, data analysis, and reporting.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 71652 row and 5 variables.
"sponsors"


#' Studies Dataset
#'
#' Basic info about study, including study title, date study registered with ClinicalTrials.gov, date results first posted to ClinicalTrials.gov, dates for study start and completion,
#' phase of study, enrollment status, planned or actual enrollment, number of study arms/groups, etc.
#'
#' This illustrative data only includes participants with conditions "oncology", "malignant neoplasm", and "cancer" based on data extracted on 2022-06-07.
#'
#' Definition of each variable can be found in the AACT Data Elements table on the AACT Data Dictionary page:
#' \url{https://aact.ctti-clinicaltrials.org/data_dictionary}
#'
#' @format A data frame with 42218 rows and 64 variables.
"studies"
