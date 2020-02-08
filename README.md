
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

To download the claims with their summary and disproof collected in the EUvsDisinfo data base, you can use the following command:
``` r
library(euvsdisinfoR)
# get first 3 pages from the claims endpoint
get_claims(pages=3)
# or get all claims
get_claims(pages="all")
```
To download the corresponding news articles or media objects that spread the claims, you can use:
``` r
# retrieve both news articles and media objects
get_creative_works(3)

# retrieve only news articles or media objects
get_news_articles(3)
get_media_objects(pages=2)
```

A few features use IDs, such as country or keyword ID. The data frames with these IDs and their values can be downloaded as follows:
``` r
get_languages("all")
get_keywords(4)
get_countries("all")
get_organizations("all")
```


## The Data
The different data sets form the following data model:

![data model diagram](man/data_diagramm.png)

Note that the columns `appearances`, `keywords` and `content_locations` in claims contain lists  of creative works, keywords, or countries, resp.
