
#' @importFrom rlang .data
ua <- httr::user_agent("https://github.com/corriebar/euvsdisinfoR")

get_page_info <- function(parsed) {
  page_info <- parsed$`hydra:view`

  current_page <- page_info$`@id`
  last_page <- page_info$`hydra:last`
  first_page <- page_info$`hydra:first`
  if (current_page == last_page) {
    next_page <- ""
  } else {
    next_page <- page_info$`hydra:next`
  }

  total_items <- parsed$`hydra:totalItems`
  list(
    total_items = total_items,
    current_page = current_page,
    last_page = last_page,
    next_page = next_page
  )
}



euvsdisinfo_api <- function(path="claims") {
  url <- httr::modify_url("https://api.veedoo.io/", path=path)
  resp <- httr::GET(url, httr::accept("application/ld+json"), ua )

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "EUvsDisinfo API request failed [%s]\n %s",
        httr::status_code(resp),
        httr::http_status(resp)$message
      ),
      call. = FALSE
    )
  }

  if (httr::http_type(resp) != "application/ld+json") {
    stop("API did not return json+ld", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE,
                               simplifyVector = TRUE, simplifyDataFrame = TRUE)

  data <- parsed$`hydra:member`


  page_info <- get_page_info(parsed)

  structure(
    list(
      content = parsed,
      path = path,
      data = dplyr::as_tibble(data),
      response = resp,
      total_items = page_info$total_items,
      current_page = page_info$current_page,
      last_page = page_info$last_page,
      next_page = page_info$next_page
    ),
    class = "disinfo_request"
  )

}


print.disinfo_request <- function(x, ...) {
  cat("<EUvsDisinfo:", x$path, ">\n", sep = "")
  utils::str(x, max.level=1)
  invisible(x)
}

get_data_from_page <- function(page_path, pb) {
  pb$tick()$print()
  resp <- euvsdisinfo_api(page_path)
  resp$data
}

paginate_resps <- function(path, pages) {
  resp <- euvsdisinfo_api(path)
  last_page_no <- as.integer ( stringr::str_extract(resp$last_page, "(?<=page=)\\d+") )
  if (pages == "all") {
    pages <- last_page_no
  }
  if (last_page_no < pages) {
    warning(paste("Only ", last_page_no, " pages were found. Will return all pages.") )
    pages <- max(last_page_no, pages)
  }

  if (pages == 1) {
    # no need for further requests
    d <- resp$data
  }
  else {
    # paginate through remaining pages
    page_paths <- paste0(path, "?page=", 2:pages)

    message(paste("Starting to download the first", pages, "pages (out of", last_page_no, "available pages)."))
    pb <- dplyr::progress_estimated(length(page_paths))

    df <- purrr::map_dfr(page_paths, .f = ~get_data_from_page(., pb) )

    d <- dplyr::bind_rows(resp$data, df)
  }
  janitor::clean_names(d)
}

#' Get claims and claim reviews
#'
#' Returns the claims (containing meta-data regarding the claims) a
#' and claim reviews (containing the summarized claim and disproof).
#'
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @param remove_duplicates Remove claim duplicates. Defaults to TRUE.
#' @export
#' @examples
#' \dontrun{
#' get_claims(2)
#' }
#' \dontrun{
#' get_claim_reviews("all")
#' }
get_claims <- function(pages=1, remove_duplicates=TRUE) {
  path <- "claims"
  claims <- paginate_resps(path, pages)
  if (remove_duplicates) {
    dups <- duplicated(claims)
    claims <- claims[!dups,]
  }
  claims %>%
    dplyr::select(-.data$author) %>%
    dplyr::rename(claims_id = .data$id,
           review_id = .data$claim_review)
}

#' @describeIn get_claims Get claim reviews.
#' @export
get_claim_reviews <- function(pages=1) {
  path <- "claim_reviews"
  reviews <- paginate_resps(path, pages)
  reviews %>%
    dplyr::select(claims_id = .data$item_reviewed, .data$type:.data$text)
}



#' Get authors and organizations
#'
#' Currently, there is no difference between organizations and authors and
#' both download the same data regarding the outlet organization.
#'
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @export
#' @examples
#' \dontrun{
#' get_authors(pages=2)
#' get_organizations("all")
#' }
get_organizations <- function(pages=1) {
  path <- "organizations"
  orgs <- paginate_resps(path, pages)
  orgs %>%
    dplyr::rename(organizations_id = .data$id)
}

#' @describeIn get_organizations Get organizations
#' @export
get_authors <- function(pages=1) {
  path <- "authors"
  authors <- paginate_resps(path, pages)
  authors %>%
    dplyr::rename(organizations_id = .data$id)
}

#' Get issues
#'
#' Returns the weekly issues that are sent by [EU vs Disinfo](https://euvsdisinfo.eu/)
#' in their newsletter.
#' The issues link to the claim IDs contained in the issue.
#'
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @export
#' @examples
#' \dontrun{
#' get_issues(pages=1)
#' }
get_issues <- function(pages=1) {
  path <- "issues"
  issues <- paginate_resps(path, pages)
  issues %>%
    dplyr::select(-.data$id_2) %>%
    dplyr::rename(issues_id = .data$id)
}

#' Get creative works
#'
#' Returns the creative works (containing both news articles and media objects) or
#' only news articles or only media objects (videos).
#' Contains a link to the original source and an archived version, as well as
#' an abstract (if applicable).
#'
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @export
#' @examples
#' \dontrun{
#' get_news_articles(pages=1)
#' get_media_objects("all")
#' }
get_creative_works <- function(pages=1) {
  path <- "creative_works"
  creative_works <- paginate_resps(path, pages)
  creative_works %>%
    dplyr::rename(cw_id = .data$id)
}

#' @describeIn get_creative_works Get only news articles.
#' @export
get_news_articles <- function(pages=1) {
  path <- "news_articles"
  newsarticle <- paginate_resps(path, pages)
  newsarticle %>%
    dplyr::rename(cw_id = .data$id)
}

#' @describeIn get_creative_works Get only media objects (videos).
#' @export
get_media_objects <- function(pages=1) {
  path <- "media_objects"
  media_objects <- paginate_resps(path, pages)
  media_objects %>%
    dplyr::rename(cw_id = .data$id)
}


#' Get countries, keywords and languages
#'
#' These return lookup tables to the country, keyword and language IDs.
#' The language IDs are used by the creative work, news articles, and media objects,
#' whereas the claims data uses keyword and country IDs.
#'
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @export
#' @examples
#' \dontrun{
#' get_languages(pages=2)
#' get_keywords("all")
#' }
get_countries <- function(pages=1) {
  path <- "countries"
  countries <- paginate_resps(path, pages)
  countries %>%
    dplyr::rename(country_id = .data$id)
}

#' @describeIn get_countries Get keywords.
#' @export
get_keywords <- function(pages=1) {
  path <- "keywords"
  keywords <- paginate_resps(path, pages)
  keywords %>%
    dplyr::rename(keyword_id = .data$id)
}

#' @describeIn get_countries Get countries.
#' @export
get_languages <- function(pages=1) {
  path <- "languages"
  langauges <- paginate_resps(path, pages)
  langauges %>%
    dplyr::rename(language_id = .data$id)
}

