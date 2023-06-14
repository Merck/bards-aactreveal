
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aactreveal

The goal of aactreveal is to provide tools for extracting and querying
data from the AACT (Aggregate Content of ClinicalTrials.gov) database
for subsequent analyses. See for more details on the AACT database.

## Installation

You can install the development version of aactreveal from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Merck/bards-aactreveal")
```

## Example

The first step is to load the package. AACT data is either loaded from a
local path (need to download AACT raw txt files) or the AACT database
directly (requires user-id and password). For illustration purposes, we
highlight this step with the pre-loaded data-sets within “aactreveal.”
Pre-loading the data is NOT required however (see below some examples).

``` r
library(aactreveal)
```

In practice, the user can access the latest AACT data by connecting
directly to the database, or by downloading the raw data (in txt file
format) and pointing to the folder containing the downloaded files.
Refer to for details on obtaining a username/password along with
downloading the raw files. The “load_aact_data” function pre-loads the
data for subsequent queries and building analysis-ready data-sets, see
below. Note that “user_name” and “pw” are examples only, the user should
supply their own user-name and passwords after registering an AACT
account.

``` r
# Load Data by connecting to AACT database #
library(RPostgreSQL)
library(dplyr)
drv <- dbDriver('PostgreSQL')
user_name <- "AACT_username"
pw <- "AACT_pw"
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user=user_name, password=pw)
load_aact_data(con=con, path_raw=NULL, table_names = table_names)

# Alternatively, can download the raw txt files and load by using "path_raw" argument.
example_path <- "//location/for/aact_data/"
load_aact_data(con=NULL, path_raw=example_path)
```

The “load_aact_data” function pre-loads the data to the R environment
for subsequent queries and building analysis-ready data-sets. This is
not required for any of the query functions, but can speed things up.
Individual data-tables can also be loaded via the “load_data” function:

``` r
studies <- load_data(con=con, path_raw=NULL, table_name = "studies")
outcome_measurements <- load_data(con=con, path_raw=NULL, table_name = "outcome_measurements")
```

Next, let’s query the data for specific interventions and conditions of
interest. By default, based on input query terms, the package will also
search for “related terms” by web-scraping clinialtrials.gov. See below:

``` r
terms_int <- "pembrolizumab"
terms_cond <- "breast cancer"
query_init <- query_aact(terms_int = terms_int, terms_cond = terms_cond)
#> Querying treatment......
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Querying condition......
#> Check inputs into load_data function: Loading aactreveal package data
#> Querying designs, studies, and sponsor data-tables...
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
user_nct_ids <- query_init$user_nct_ids # Unique NCT ids 
search_terms_int <- query_init$trt_search_terms
names(query_init)
#> [1] "analy_study"       "user_nct_ids"      "trt_search_terms" 
#> [4] "cond_search_terms"
```

The same function can be run without pre-loaded data, for example by
specifying the path or connection argument:

``` r
query_aact(terms_int = terms_int, terms_cond = terms_cond, path_raw=path_raw)
query_aact(terms_int = terms_int, terms_cond = terms_cond, con=con)
```

Next, based on the queried data-set, let’s search for clinical outcomes.
Here we search for objective response rate (ORR) date and overall
survival (OS) outcomes. The key input is a list of arguments, named
“outcome_list”:

``` r
orr_list <- list(
  include = c("overall response rate", "objective response rate"),
  exclude = c("disease control rate"),
  type = c("number"),
  range = c(0, 100)
)
os_list <- list(
  include = c("overall survival")
)
outcome_list <- list(orr_list, os_list)
outcome_name <- c("ORR", "OS")
query_out <- query_outcomes(user_nct_ids = query_init$user_nct_ids,
                          outcome_list = outcome_list, outcome_name = outcome_name,
                          analy_study = query_init$analy_study,
                          search_terms_int = search_terms_int,
                          verbose = TRUE)
#> Querying outcomes......
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> [1] "augmenting is_trt and is_combo from query_aact"
#>       is_trt_new
#> is_trt  0  1
#>   0     8  0
#>   1     0 13
#>   <NA>  6 34
#>         is_combo_new
#> is_combo  0  1
#>     0    17  0
#>     1     0  4
#>     <NA>  6 34
#> Warning: Unknown or uninitialised column: `is_trt`.
#> [1] "no is_trt found: identifying treatments"
#>       is_trt_new
#> is_trt 1
#>   <NA> 6
#>         is_combo_new
#> is_combo 0
#>     <NA> 6
names(query_out)
#> [1] "outcome_measures_final"   "outcome_analyses_final"  
#> [3] "uniq_title"               "uniq_units"              
#> [5] "uniq_measures_param_type" "uniq_analyses_param_type"
#> [7] "outcome_analyses_full"
head(query_out$outcome_analyses_final[,c("outcome_cat", "nct_id", "title",
                                         "param_comparison", "comp", "ref",
                                         "param_type",
                                         "param_value")])
#> # A tibble: 6 × 8
#>   outcome_cat nct_id      title              param…¹ comp  ref   param…² param…³
#>   <chr>       <chr>       <chr>              <chr>   <chr> <chr> <chr>     <dbl>
#> 1 ORR         NCT02555657 Overall Response … pembro… pemb… chem… differ…    8.3 
#> 2 ORR         NCT02555657 Overall Response … pembro… pemb… chem… differ…    2.9 
#> 3 ORR         NCT02555657 Overall Response … pembro… pemb… chem… differ…   -1   
#> 4 OS          NCT02555657 Overall Survival … pembro… pemb… chem… hazard…    0.78
#> 5 OS          NCT02555657 Overall Survival … pembro… pemb… chem… hazard…    0.86
#> 6 OS          NCT02555657 Overall Survival … pembro… pemb… chem… hazard…    0.97
#> # … with abbreviated variable names ¹​param_comparison, ²​param_type,
#> #   ³​param_value
```

Finally, instead of the individual steps, you can run the whole process
using:

``` r
out_all <- extract_aact(con=NULL, path_raw=NULL, terms_int=terms_int, terms_cond=terms_cond, 
                        outcome_list = outcome_list, outcome_name = outcome_name, verbose = TRUE)
#> Querying treatment......
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Querying condition......
#> Check inputs into load_data function: Loading aactreveal package data
#> Querying designs, studies, and sponsor data-tables...
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Searching for outcome(s) of interest......
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Check inputs into load_data function: Loading aactreveal package data
#> Querying outcomes......
#> [1] "augmenting is_trt and is_combo from query_aact"
#>       is_trt_new
#> is_trt  0  1
#>   0     8  0
#>   1     0 13
#>   <NA>  6 34
#>         is_combo_new
#> is_combo  0  1
#>     0    17  0
#>     1     0  4
#>     <NA>  6 34
#> Warning: Unknown or uninitialised column: `is_trt`.
#> [1] "no is_trt found: identifying treatments"
#>       is_trt_new
#> is_trt 1
#>   <NA> 6
#>         is_combo_new
#> is_combo 0
#>     <NA> 6
names(out_all)
#>  [1] "analy_study"            "outcome_measures_query" "outcome_analyses_query"
#>  [4] "outcome_measures_raw"   "outcome_analyses_raw"   "outcome_analyses_full" 
#>  [7] "result_groups"          "baseline_dat"           "trt_search_terms"      
#> [10] "cond_search_terms"
head(out_all$outcome_analyses_query[,c("outcome_cat", "nct_id", "title",
                                         "param_comparison", "comp", "ref",
                                         "param_type",
                                         "param_value")])
#> # A tibble: 6 × 8
#>   outcome_cat nct_id      title              param…¹ comp  ref   param…² param…³
#>   <chr>       <chr>       <chr>              <chr>   <chr> <chr> <chr>     <dbl>
#> 1 ORR         NCT02555657 Overall Response … pembro… pemb… chem… differ…    8.3 
#> 2 ORR         NCT02555657 Overall Response … pembro… pemb… chem… differ…    2.9 
#> 3 ORR         NCT02555657 Overall Response … pembro… pemb… chem… differ…   -1   
#> 4 OS          NCT02555657 Overall Survival … pembro… pemb… chem… hazard…    0.78
#> 5 OS          NCT02555657 Overall Survival … pembro… pemb… chem… hazard…    0.86
#> 6 OS          NCT02555657 Overall Survival … pembro… pemb… chem… hazard…    0.97
#> # … with abbreviated variable names ¹​param_comparison, ²​param_type,
#> #   ³​param_value
```

Overall, aactreveal aims to facilitate analyses using the AACT database
which includes all information posted on clinicaltrials.gov. We caution
the user that the package outputs may still need cleaning (ex: convert
median OS values to month unit).
