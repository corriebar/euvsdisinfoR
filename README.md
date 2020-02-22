
# euvsdisinfoR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of euvsdisinfoR is to provide some easy-to-use wrapper functions for the [API](api.veedoo.io/) provided by [EUvsDisinfo](https://euvsdisinfo.eu/).  The API provides access to the EUvsDisinfo data base which contains over 7000 disinformation cases and debunks. The EUvsDisinfo project started its work in 2015 and identifies, compiles, and exposes disinformation cases originating in pro-Kremlin media that are spread across the EU and Eastern Partnership countries.

The package is based on work done together with my team colleagues, Alix Dumoulin and Alexandra Pavliuc, during the EUvsDisinformation hackathon, Jan 2020. Our hackathon project can be found [here](https://github.com/alix-dumoulin/eu_disinformation).

## Installation

You can install euvsdisinfoR from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("corriebar/euvsdisinfoR")
```

## Example

To download the different data tables from the EUvsDisinfo data base, it is easiest to collect them in a `disinfo()` object:
``` r
library(euvsdisinfoR)
d <- disinfo()
```

It is possible to either add only a few tables or all at once:
``` r
d %>%
  add_claims(pages=3) %>%
  add_reviews("all")

d %>% 
  add_all("all")
```
For claims and claim reviews, it is also possible to query only claims/reviews that were published or reviewed after a certain date:
``` r
library(lubridate)

d %>%
     add_claims(published_since = today() - months(3) ) %>%
     add_reviews(published_since = today() - months(3) )
     
d %>%
     add_reviews(reviewed_since = today() - months(2) )
```
where `published_since` always relates to when the claim was originally published and `reviewed_since` refers to when an item was reviewed.

To then flatten the disinfo object and obtain one single data frame, one can use
``` r
d %>%
  flatten_disinfo()
```
This attempts to merge as much as possible (according to the relations from the data model below) and returns a single data frame. It either returns a data frame where each row is a creative work or where each row is a claim (if the disinfo object contains no creative works).

It is also possible to only download single data tables:
``` r
# get first 3 pages from the claims endpoint
get_claims(pages=3)
# or get all creative works
get_creative_works(pages="all")
```


## The Data
The different data sets form the following data model:

![png](man/data_diagram.png?raw=true "Data Model")

Note that the columns `appearances`, `keywords` and `content_locations` in the claims data frame contain lists  of creative works, keywords, or countries (resp).
