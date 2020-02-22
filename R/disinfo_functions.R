new_disinfo <- function(claims = data.frame(),
                        reviews = data.frame(),
                        authors = data.frame(),
                        creative_works = data.frame(),
                        issues = data.frame(),
                        keywords = data.frame(),
                        countries = data.frame(),
                        languages = data.frame()) {
  stopifnot(is.data.frame(claims))
  stopifnot(is.data.frame(reviews))
  stopifnot(is.data.frame(authors))
  stopifnot(is.data.frame(creative_works))
  stopifnot(is.data.frame(issues))
  stopifnot(is.data.frame(keywords))
  stopifnot(is.data.frame(countries))
  stopifnot(is.data.frame(languages))

  structure(
    list(
      claims = claims,
      reviews = reviews,
      authors = authors,
      creative_works = creative_works,
      issues = issues,
      keywords = keywords,
      countries = countries,
      languages = languages
    ),
    class = "disinfo_data"
  )
}

print.disinfo_data <- function(x, ...) {
  cat("EUvsDisinfo Data:", "\n", sep = "")
  utils::str(x, max.level=1)
  invisible(x)
}

validate_disinfo <- function(x) {
  df_list <- unclass(x)
  num_rows <- purrr::map_dbl(df_list, nrow )
  num_cols <- purrr::map_dbl(df_list, ncol )
  if ( any( num_cols == 0 ) | any( num_rows == 0 )) {
    stop(
      "At least one data frame is empty",
      .call = FALSE
    )
  }
  x
}

#' Create new disinfo object
#'
#' Creates a disinfo object that contains the collection of data frames obtained from the EUvsDisinfo API.
#'
#' @param claims A claims data frame.
#' @param reviews A reviews data frame.
#' @param authors A authors or organizations data frame.
#' @param creative_works A creative work data frame. Can also be a news article or media object data frame.
#' @param issues An issues data frame.
#' @param keywords A keywords data frame.
#' @param countries A countries data frame.
#' @param languages A languages data frame.
#' @export
#' @examples
#' d <- disinfo()
#' \dontrun{
#' d %>%
#'    add_claims("all")
#'
#' d %>%
#'    add_all("all")
#'    }
disinfo <- function(claims = data.frame(),
                    reviews = data.frame(),
                    authors = data.frame(),
                    creative_works = data.frame(),
                    issues = data.frame(),
                    keywords = data.frame(),
                    countries = data.frame(),
                    languages = data.frame()) {
  new_disinfo(claims, reviews, authors,
              creative_works, issues,
              keywords, countries, languages)
}


#' Add data to disinfo object
#'
#' Add the different data tables to the disinfo object by calling the EUvsDisinfo API.
#'
#' @param disinfo A disinfo object.
#' @param pages Either the number of pages to download or "all". Defaults to 1.
#' @param published_since Date string. Only retrieve claims or claim reviews where the claims were published after this date.
#' It is currently only possible to restrict the claims and claim reviews by the publishing date.
#' @param reviewed_since Date string. Only retrieve claim reviews that were reviewed after this date.
#' It is currently only possible to restrict the claim reviews by the reviewing date.
#' @param clean_html If TRUE, then add another column `text` which is a plain text version of `html_text`.
#' Defaults to TRUE.
#' @export
#' @examples
#' d <- disinfo()
#' \dontrun{
#' d %>%
#'    add_claims()
#'
#' d %>%
#'    add_all()
#' }
add_claims <- function(disinfo, pages = 1, published_since = NULL) {
  claims <- get_claims(pages, published_since = published_since)
  new_disinfo(claims=claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = disinfo$creative_works,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download claim reviews data and add to disinfo object.
#' @export
add_reviews <- function(disinfo, pages = 1, clean_html=TRUE,
                        published_since = NULL, reviewed_since = NULL) {
  reviews <- get_claim_reviews(pages, clean_html=clean_html,
                               published_since = published_since, reviewed_since = reviewed_since)
  new_disinfo(claims=disinfo$claims,
              reviews = reviews,
              authors = disinfo$authors,
              creative_works = disinfo$creative_works,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download authors data and add to disinfo object.
#' @export
add_authors <- function(disinfo, pages = 1) {
  authors <- get_authors(pages)
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = authors,
              creative_works = disinfo$creative_works,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download creative works data and add to disinfo object.
#' @export
add_creative_works <- function(disinfo, pages = 1) {
  creative_works <- get_creative_works(pages)
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = creative_works,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download issues data and add to disinfo object.
#' @export
add_issues <- function(disinfo, pages = 1) {
  issues <- get_issues(pages)
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = disinfo$creative_works,
              issues = issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}


#' @describeIn add_claims Download keywords data and add to disinfo object.
#' @export
add_keywords <- function(disinfo, pages = 1) {
  keywords <- get_keywords(pages)
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = disinfo$creative_works,
              issues = disinfo$issues,
              keywords = keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download countries data and add to disinfo object.
#' @export
add_countries <- function(disinfo, pages = 1) {
  countries <- get_countries(pages)
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = disinfo$creative_works,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download languages data and add to disinfo object.
#' @export
add_languages <- function(disinfo, pages = 1) {
  languages <- get_languages(pages)
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = disinfo$creative_works,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = languages
  )
}

#' @describeIn add_claims Download news articles data and add to disinfo object. Will be
#' written to `creative_works` and overwrites any previous creative works data.
#' If you want to have both news articles and media objects, use `add_creative_works()`.
#'
#' @export
add_news_articles <- function(disinfo, pages = 1) {
  news_articles <- get_news_articles(pages)
  if ( has_creative_works(disinfo) ) {
    warning("Disinfo object already has creative works,will overwrite creative works with\n
              news articles.\n
            If you want to have both news articles and media objects, use `add_creative_works()` instead.")
  }
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = news_articles,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Download media objects data and add to disinfo object. Will be
#' written to `creative_works` and overwrites any previous creative works data.
#' If you want to have both news articles and media objects, use `add_creative_works()`.
#'
#' @export
add_media_objects <- function(disinfo, pages = 1) {
  media_objects <- get_media_objects(pages)
  if ( has_creative_works(disinfo) ) {
    warning("Disinfo object already has creative works, will overwrite creative works with\n
              media objects.\n
            If you want to have both news articles and media objects, use `add_creative_works()` instead.")
  }
  new_disinfo(claims=disinfo$claims,
              reviews = disinfo$reviews,
              authors = disinfo$authors,
              creative_works = media_objects,
              issues = disinfo$issues,
              keywords = disinfo$keywords,
              countries = disinfo$countries,
              languages = disinfo$languages
  )
}

#' @describeIn add_claims Downloads all 8 data tables from the API and adds them to the disinfo object.
#' @export
add_all <- function(disinfo, pages = 1, reviewed_since=NULL, published_since=NULL) {
  disinfo %>%
    add_claims(pages,
               published_since = published_since) %>%
    add_reviews(pages,
                published_since = published_since,
                reviewed_since = reviewed_since) %>%
    add_authors(pages) %>%
    add_creative_works(pages) %>%
    add_issues(pages) %>%
    add_keywords(pages) %>%
    add_countries(pages) %>%
    add_languages(pages)
}


has_data <- function(disinfo, data_name) {
  (nrow(disinfo[[data_name]]) > 0) & (ncol(disinfo[[data_name]]) > 0 )
}
has_claims <- function(disinfo) {
  has_data(disinfo, "claims")
}
has_reviews <- function(disinfo) {
  has_data(disinfo, "reviews")
}
has_creative_works <- function(disinfo) {
  has_data(disinfo, "creative_works")
}
has_authors <- function(disinfo) {
  has_data(disinfo, "authors")
}
has_languages <- function(disinfo) {
  has_data(disinfo, "languages")
}
has_issues <- function(disinfo) {
  has_data(disinfo, "issues")
}
has_keywords <- function(disinfo) {
  has_data(disinfo, "keywords")
}
has_countries <- function(disinfo) {
  has_data(disinfo, "countries")
}

#' Flatten disinfo object
#'
#' Joins the different data frames in a smart way so that either each row corresponds to a claim or a creative work.
#' Only full observations are kept.
#'
#' @param disinfo A disinfo object.
#' @export
#' @examples
#' d <- disinfo()
#' \dontrun{
#' d %>%
#'   add_all("all") %>%
#'   flatten_disinfo()
#'   }
flatten_disinfo <- function(disinfo) {
  hasClaims <- has_claims(disinfo)
  # join claims and reviews
  if ( hasClaims & has_reviews(disinfo)) {
    disinfo <- join_claims_reviews(disinfo)
    message("Joined reviews to claims")
  }


  # join issues to claims
  if ( hasClaims & has_issues(disinfo)) {
    disinfo <- join_claims_issues(disinfo)
    message("Joined issues to claims")
  }

  # join keywords to claims
  if ( hasClaims & has_keywords(disinfo)) {
    disinfo <- join_claims_keywords(disinfo)
    message("Joined keywords to claims")
  }

  # join countries to claims
  if ( hasClaims & has_countries(disinfo)) {
    disinfo <- join_claims_countries(disinfo)
    message("Joined countries to claims")
  }

  hasWorks <- has_creative_works(disinfo)
  # join authors to creative works
  if (hasWorks & has_authors(disinfo)) {
    disinfo <- join_works_authors(disinfo)
    message("Joined authors to creative works")
  }

  # join languages to creative works
  if (hasWorks & has_languages(disinfo)) {
    disinfo <- join_works_languages(disinfo)
    message("Joined languages to creative works")
  }

  # join creative works and claims
  if ( hasClaims & has_creative_works(disinfo)) {
    disinfo <- join_claims_works(disinfo)
    message("Joined claims to creative works")
  }

  extract_merged_df(disinfo)
}
