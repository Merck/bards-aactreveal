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


## Find related terms based on some search term ##
# @param search_term: Search term to find related terms
# @param type: Type of search (either "other terms" or "condition")

find_related_terms <- function(search_term, type = "other_terms") {
  # Turn empty space into "+" for query
  # for ex) breast cancer --> breast+cancer
  if (stringr::str_detect(search_term, " ")) {
    search_term_processed <- stringr::str_replace(search_term, " ", "+")
    # create url
    if (type == "other_terms") {
      ctgov_url <- paste0("https://clinicaltrials.gov/ct2/results/details?term=",
                          "\"", search_term_processed, "\"")
    } else { # type == "condition"
      ctgov_url <- paste0("https://clinicaltrials.gov/ct2/results/details?cond=",
                          "\"", search_term_processed, "\"")
    }
  } else {
    # create url
    if (type == "other_terms") {
      ctgov_url <- paste0("https://clinicaltrials.gov/ct2/results/details?term=", search_term)
    } else { # type=="condition"
      ctgov_url <- paste0("https://clinicaltrials.gov/ct2/results/details?cond=", search_term)
    }
  }
  # web-scraping
  ct_page <- xml2::read_html(ctgov_url)
  # extract related terms
  ct_page %>%
    # find elements that match a css selector
    rvest::html_elements(".w3-padding-8:nth-child(1)") %>%
    # retrieve text from element (html_text() is much faster than html_text2())
    rvest::html_text()
}

## Generate combined search terms (input + related) by using find_related_terms() ##
# @param search_terms: Search term(s) to find related terms
# @param type: Type of search (either "other_terms" or "condition")
globalVariables(c("."))

combine_search_terms <- function(search_terms, type, intervene = FALSE) {
  search_terms <- tolower(search_terms)
  related_terms <- vector("character")

  for (ii in search_terms) {
    related_terms_scraped <- find_related_terms(ii, type = type)
    # user intervention for search term (ii) that contains space
    # (ex. "breast cancer")
    if (intervene) {
      ii_processed <- stringr::str_trim(ii)
      contains_space <- stringr::str_detect(ii_processed, " ")
      if (contains_space) {
        for (jj in related_terms_scraped) {
          user_yn <- readline(prompt = paste0("Do you want to include ", jj, "? Answer Y/N: "))
          user_yn <- tolower(user_yn)
          if (user_yn == "n") {
            related_terms_scraped <- related_terms_scraped[-match(jj, related_terms_scraped)]
          }
        }
      }
    }
    related_terms <- c(related_terms, related_terms_scraped)
  }

  # preprocess search terms
  search_terms_raw <- c(search_terms, tolower(related_terms))
  search_terms_processed <- search_terms_raw %>%
    unique() %>%
    tolower() %>%
    paste0(.,  collapse="|")

  return(list(search_terms_raw = search_terms_raw, search_terms_processed = search_terms_processed))
}

