
#' @importFrom rlang .data
ua <- httr::user_agent("https://github.com/corriebar/euvsdisinfoR")

get_page_info <- function(parsed) {
  page_info <- parsed$`hydra:view`

  current_page <- page_info$`@id`

  if (exists("hydra:last", page_info)) {
    last_page <- page_info$`hydra:last`
    first_page <- page_info$`hydra:first`
  } else {
    last_page <- NULL
    first_page <- NULL
  }

  total_items <- parsed$`hydra:totalItems`
  list(
    total_items = total_items,
    current_page = current_page,
    last_page = last_page
  )
}



euvsdisinfo_api <- function(path="claims", first_request=TRUE) {
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

  if (first_request) {
    page_info <- get_page_info(parsed)
    response <- structure(
              list(
                content = parsed,
                path = path,
                data = dplyr::as_tibble(data),
                response = resp,
                total_items = page_info$total_items,
                current_page = page_info$current_page,
                last_page = page_info$last_page
              ),
              class = "disinfo_request"
            )
  } else {
    response <- structure(
            list(
              content = parsed,
              path = path,
              data = dplyr::as_tibble(data),
              response = resp
            ),
            class = "disinfo_request"
          )
  }
  response

}


print.disinfo_request <- function(x, ...) {
  cat("<EUvsDisinfo:", x$path, ">\n", sep = "")
  utils::str(x, max.level=1)
  invisible(x)
}


get_data_from_page <- function(page_path, pb) {
  pb$tick()$print()
  resp <- euvsdisinfo_api(page_path, first_request = FALSE)
  resp$data
}

paginate_resps <- function(path, pages, published_since = NULL, reviewed_since = NULL) {
  if ( !is.null(reviewed_since)  & path == "claim_reviews")  {
    request_path <- paste0(path, "?datePublished[after]=", reviewed_since)
  } else if ( !is.null(published_since) & path == "claims") {
    request_path <- paste0(path, "?datePublished[after]=", published_since)
  } else if ( !is.null(published_since) & path == "claim_reviews") {
    request_path <- paste0(path, "?itemReviewed.datePublished[after]=", published_since)
  }
  else {
    request_path <- path
  }
  resp <- euvsdisinfo_api(request_path, first_request = TRUE)
  last_page <- resp$last_page
  if ( is.null(last_page) ) {
    last_page_no <- 1
    pages <- 1
  } else {
    last_page_no <- as.integer ( stringr::str_extract(resp$last_page, "(?<=page=)\\d+") )
  }
  if (pages == "all") {
    pages <- last_page_no
  }
  if (last_page_no < pages) {
    warning(paste("Only ", last_page_no, " pages were found. Will return all pages.") )
    pages <- max(last_page_no, pages)
  }

  message(paste("Starting to download the first", pages, "pages for", path, " (out of", last_page_no, "available pages)."))
  if ( !is.null(published_since) )
    message(paste("Published after", published_since ) )
  if ( !is.null(reviewed_since) )
    message(paste("Reviewed after", reviewed_since ) )

  if (pages == 1) {
    # no need for further requests
    d <- resp$data
  }
  else {
    # paginate through remaining pages
    page_paths <- paste0(path, "?page=", 2:pages)

    pb <- dplyr::progress_estimated(length(page_paths))

    df <- purrr::map_dfr(page_paths, .f = ~get_data_from_page(., pb) )

    d <- dplyr::bind_rows(resp$data, df)
  }
  janitor::clean_names(d)
}

#' Get claims and claim reviews
#'
#' Retrieve the claims and claim reviews.
#'
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @param remove_duplicates Remove claim duplicates. Defaults to TRUE.
#' @param published_since Date string. Only retrieve claims that were published after this date.
#' @param reviewed_since Date string. Only retrieve claim reviews where the claim was reviewed after this date.
#' @param clean_html If TRUE, then add another column `text` which is a plain text version of `html_text`.
#' Defaults to TRUE.
#' @export
#' @examples
#' \dontrun{
#' get_claims(2)
#'
#' get_claim_reviews("all")
#'
#' library(lubridate)
#' get_claims(2, since = today() - months(3) )
#' }
get_claims <- function(pages=1, remove_duplicates=TRUE, published_since=NULL) {
  path <- "claims"
  claims <- paginate_resps(path, pages, published_since=published_since)
  if (remove_duplicates) {
    dups <- duplicated(claims)
    claims <- claims[!dups,]
  }
  if (nrow(claims) > 0 ) {
    claims <- claims %>%
      dplyr::select(-.data$author) %>%
      dplyr::rename(claims_id = .data$id,
             review_id = .data$claim_review,
             claim_published = .data$date_published)
  }
  claims
}

strip_html <- function(s) {
  rvest::html_text(xml2::read_html(s))
}

#' @describeIn get_claims Retrieves the claim reviews which contains the summarized claim and disproof.
#' @export
get_claim_reviews <- function(pages=1, clean_html=TRUE, published_since=NULL, reviewed_since=NULL) {
  path <- "claim_reviews"
  reviews <- paginate_resps(path, pages, published_since, reviewed_since)
  if (nrow(reviews) > 0 ) {
    reviews <- reviews %>%
      dplyr::select(claims_id = .data$item_reviewed, .data$type:.data$text) %>%
      dplyr::rename(review_name = .data$name,
                    html_text = .data$text,
                    review_published = .data$date_published)
    if (clean_html) {
      reviews <- reviews %>%
        dplyr::mutate(text = purrr::map_chr(.data$html_text, .f=strip_html))
    }
  }
  reviews
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
  if (nrow(orgs) > 0) {
    orgs <- orgs %>%
      dplyr::rename(organization_id = .data$id,
                    organization_name = .data$id)
  }
  orgs
}

#' @describeIn get_organizations Get organizations
#' @export
get_authors <- function(pages=1) {
  path <- "authors"
  authors <- paginate_resps(path, pages)
  if (nrow(authors) > 0 ){
    authors <- authors %>%
      dplyr::rename(organization_id = .data$id,
                    organization_name = .data$name)
  }
  authors
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
  if (nrow(issues) > 0 ) {
    issues <- issues %>%
      dplyr::select(-.data$id_2) %>%
      dplyr::rename(issue_id = .data$id)
  }
  issues
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
  if (nrow(creative_works) > 0 ) {
    creative_works <- creative_works %>%
      # date_published always empty for works (so far)
      dplyr::select(-.data$date_published ) %>%
      dplyr::rename(creative_work_id = .data$id)
  }
  creative_works
}

#' @describeIn get_creative_works Get only news articles.
#' @export
get_news_articles <- function(pages=1) {
  path <- "news_articles"
  newsarticle <- paginate_resps(path, pages)
  if (nrow(newsarticle) > 0 ){
    newsarticle <- newsarticle %>%
      # date_published always empty for works (so far)
      dplyr::select(-.data$date_published ) %>%
      dplyr::rename(creative_work_id = .data$id)
  }
  newsarticle
}

#' @describeIn get_creative_works Get only media objects (videos).
#' @export
get_media_objects <- function(pages=1) {
  path <- "media_objects"
  media_objects <- paginate_resps(path, pages)
  if (nrow(media_objects) > 0 ) {
    media_objects <- media_objects %>%
      # date_published always empty for works (so far)
      dplyr::select(-.data$date_published ) %>%
      dplyr::rename(creative_work_id = .data$id)
  }
  media_objects
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
  if (nrow(countries) > 0 ){
    countries <- countries %>%
      dplyr::rename(country_id = .data$id,
                    country_name = .data$name)
  }
  countries
}

#' @describeIn get_countries Get keywords.
#' @export
get_keywords <- function(pages=1) {
  path <- "keywords"
  keywords <- paginate_resps(path, pages)
  if (nrow(keywords) > 0 ) {
    keywords <- keywords %>%
      dplyr::rename(keyword_id = .data$id,
                    keyword_name = .data$name)
  }
  keywords
}

#' @describeIn get_countries Get countries.
#' @export
get_languages <- function(pages=1) {
  path <- "languages"
  languages <- paginate_resps(path, pages)
  if (nrow(languages) > 0) {
    languages <- languages %>%
      dplyr::rename(language_id = .data$id,
                    language_name = .data$name,
                    language_code = .data$alternate_name)
  }
  languages
}

