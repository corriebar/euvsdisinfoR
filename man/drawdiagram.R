library(euvsdisinfoR)
library(dm)


claims <- get_claims("all")
reviews <- get_claim_reviews("all")
authors <- get_authors("all")
creative_works <- get_creative_works("all")
issues <- get_issues("all")

# these are redundant to creative works
news_articles <- get_news_articles("all")
media_objects <- get_media_objects("all")

keywords <- get_keywords("all")
countries <- get_countries("all")
languages <- get_languages("all")


disinfo_dm <- dm() %>%
  dm_add_tbl(claims, reviews, authors,
             creative_works, issues,
             keywords, countries, languages) %>%
  dm_add_pk(claims, claims_id) %>%
  dm_add_pk(reviews, claims_id) %>%
  dm_add_pk(authors, organization_id) %>%
  dm_add_pk(creative_works, creative_work_id) %>%
  dm_add_pk(issues, issue_id) %>%
  dm_add_pk(keywords, keyword_id) %>%
  dm_add_pk(countries, country_id) %>%
  dm_add_pk(languages, language_id) %>%
  # add foreign keys
  dm_add_fk(reviews, claims_id, claims) %>%
  dm_add_fk(claims, claims_id, reviews) %>%
  dm_add_fk(claims, is_part_of, issues) %>%
  dm_add_fk(claims, first_appearance, creative_works) %>%
  dm_add_fk(creative_works, author, authors) %>%
  dm_add_fk(creative_works, in_language, languages) %>%
  dm_add_fk(claims, appearances, creative_works) %>%
  dm_add_fk(claims, keywords, keywords) %>%
  dm_add_fk(claims, content_locations, countries) %>%
  dm_set_colors(
    maroon4 = c("claims", "reviews"),
    "#70AD47" = creative_works,
    "#ED7D31" = authors,
    "#5B9BD5" = c("keywords", "countries"),
    "salmon" = issues,
    "darkslategray3" = languages
  )


dm_draw(disinfo_dm)



