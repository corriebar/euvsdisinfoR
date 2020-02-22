
join_claims_reviews <- function(disinfo) {
  claims <- dplyr::left_join(disinfo$claims,
                   disinfo$reviews %>%
                     dplyr::select(-.data$type), by="claims_id")
  new_disinfo(
    claims = claims,
    reviews = data.frame(),
    authors = disinfo$authors,
    creative_works = disinfo$creative_works,
    issues = disinfo$issues,
    keywords = disinfo$keywords,
    countries = disinfo$countries,
    languages = disinfo$languages
  )
}

join_claims_issues <- function(disinfo) {
  claims <- dplyr::left_join(disinfo$claims,
                             disinfo$issues %>%
                               dplyr::select(-.data$type),
                             by = c("is_part_of" = "issue_id"))
  new_disinfo(
    claims = claims,
    reviews = disinfo$reviews,
    authors = disinfo$authors,
    creative_works = disinfo$creative_works,
    issues = data.frame(),
    keywords = disinfo$keywords,
    countries = disinfo$countries,
    languages = disinfo$languages
  )

}

join_claims_keywords <- function(disinfo) {
  claims <- disinfo$claims %>%
    tidyr::unnest(.data$keywords) %>%
    dplyr::left_join(disinfo$keywords %>%
                      dplyr::select(-.data$type) , by=c("keywords" = "keyword_id")) %>%
    dplyr::select(-.data$keywords) %>%
    tidyr::nest(keywords = .data$keyword_name)

  new_disinfo(
    claims = claims,
    reviews = disinfo$reviews,
    authors = disinfo$authors,
    creative_works = disinfo$creative_works,
    issues = disinfo$issues,
    keywords = data.frame(),
    countries = disinfo$countries,
    languages = disinfo$languages
  )

}

join_claims_countries <- function(disinfo) {
  claims <- disinfo$claims %>%
    tidyr::unnest(.data$content_locations) %>%
    dplyr::left_join(disinfo$countries %>%
                       dplyr::select(-.data$type) , by=c("content_locations"="country_id")) %>%
    dplyr::select(-.data$content_locations) %>%
    tidyr::nest(content_locations = .data$country_name)

  new_disinfo(
    claims = claims,
    reviews = disinfo$reviews,
    authors = disinfo$authors,
    creative_works = disinfo$creative_works,
    issues = disinfo$issues,
    keywords = disinfo$keywords,
    countries = data.frame(),
    languages = disinfo$languages
  )

}

join_works_authors <- function(disinfo) {
  creative_works <- disinfo$creative_works %>%
    dplyr::left_join(disinfo$authors %>%
                       dplyr::select(-.data$type), by = c("author" = "organization_id"))
  new_disinfo(
    claims = disinfo$claims,
    reviews = disinfo$reviews,
    authors = data.frame(),
    creative_works = creative_works,
    issues = disinfo$issues,
    keywords = disinfo$keywords,
    countries = disinfo$countries,
    languages = disinfo$languages
  )

}

join_works_languages <- function(disinfo) {
  creative_works <- disinfo$creative_works %>%
    dplyr::left_join(disinfo$languages %>%
                       dplyr::select(-.data$type), by = c("in_language" = "language_id"))
  new_disinfo(
    claims = disinfo$claims,
    reviews = disinfo$reviews,
    authors = disinfo$authors,
    creative_works = creative_works,
    issues = disinfo$issues,
    keywords = disinfo$keywords,
    countries = disinfo$countries,
    languages = data.frame()
  )

}

join_claims_works <- function(disinfo) {
  claims <- disinfo$claims %>%
    tidyr::unnest(.data$appearances)
  creative_works <- disinfo$creative_works %>%
    # date_published always empty for works (so far)
    dplyr::select(-.data$date_published) %>%
    dplyr::left_join(claims %>%
                       dplyr::select(-.data$type) ,
                     by = c("creative_work_id" = "appearances"))

  new_disinfo(
    claims = data.frame(),
    reviews = disinfo$reviews,
    authors = disinfo$authors,
    creative_works = creative_works,
    issues = disinfo$issues,
    keywords = disinfo$keywords,
    countries = disinfo$countries,
    languages = disinfo$languages
  )

}

extract_merged_df <- function(disinfo) {
  if ( has_claims(disinfo) ) {
    message("return merged claims")
    return (disinfo$claims)
  }
  if ( has_creative_works(disinfo)) {
    message("return merged creative works")
    return (disinfo$creative_works)
  }

  if (!has_claims(disinfo) & !has_creative_works(disinfo) ) {
    stop(
      "Disinfo object has neither claims nor creative works. Nothing to join.",
      call. = FALSE
    )
  }

}
