# Builds shiny_code/references.rds, which links each source in the app dataset
# (metaUI__article_label) to its entry in refs_included.bib, so that the app can
# offer a reference list for the currently filtered studies.
#
# Run from the repository root: Rscript web_app/build_references.R
#
# Source metadata (authors, year, title, DOI per source ID) comes from the
# full-text screening tabs of the public coding sheets and is cached in
# web_app/source_metadata.csv; delete that file to download it again.
#
# Matching cascade: hand-checked matches -> DOI -> exact title (original or
# translated) -> similar title -> first-author surname + year. Citations and
# reference texts are then taken from the article's reference list
# (article_references.txt); sources missing there fall back to the .bib entry
# or, without one, to the screening metadata.

library(dplyr)

# Source metadata -------------------------------------------------------------

metadata_file <- "web_app/source_metadata.csv"
if (!file.exists(metadata_file)) {
  googlesheets4::gs4_deauth()
  read_screening <- function(sheet_id, skip) {
    googlesheets4::read_sheet(sheet_id, sheet = "Full-text screening", skip = skip,
      col_types = "c") %>%
      select(ID, Authors, Year, Title, DOI, Language)
  }
  bind_rows(
    read_screening("1pEYZUZvFr8qmULT077y932BUShpJvhs-asRuyyiBKlQ", skip = 0),
    read_screening("1XCjlC3u7Ws2KCjaRQ1R0VuU0mLQ5mHVipmzuGtivLCs", skip = 1)
  ) %>%
    filter(!is.na(ID)) %>%
    distinct(ID, .keep_all = TRUE) %>%
    readr::write_csv(metadata_file)
}
metadata <- readr::read_csv(metadata_file, col_types = readr::cols(.default = "c"))

# Bibliography ------------------------------------------------------------------

read_bib <- function(path) {
  suppressWarnings(suppressMessages(rbibutils::readBib(path, direct = TRUE)))
}
bib_key <- function(e) attr(unclass(e)[[1]], "key")

# readBib() drops entries that lack a field required for their type (e.g., an
# article without journal); re-read those as @misc so that they are kept
bib_text <- readLines("refs_included.bib", encoding = "UTF-8", warn = FALSE)
bib <- read_bib("refs_included.bib")
entry_lines <- grep("^\\s*@", bib_text)
entry_keys <- trimws(sub("^\\s*@\\s*\\w+\\s*\\{([^,]+),.*$", "\\1", bib_text[entry_lines]))
# Duplicated keys are kept by readBib() with an a/b suffix, so do not count them as dropped
parsed_keys <- vapply(bib, bib_key, character(1))
is_dropped <- !entry_keys %in% parsed_keys & !entry_keys %in% sub("[a-z]$", "", parsed_keys)
if (any(is_dropped)) {
  bib_text[entry_lines[is_dropped]] <- sub("@\\s*\\w+\\s*\\{", "@misc{", bib_text[entry_lines[is_dropped]])
  tmp <- tempfile(fileext = ".bib")
  writeLines(bib_text, tmp, useBytes = TRUE)
  bib <- read_bib(tmp)
}

field <- function(e, f) {
  v <- unclass(e)[[1]][[f]]
  if (is.null(v)) NA_character_ else paste(as.character(v), collapse = " ")
}
# Letters and digits in any script, lower case
norm <- function(x) {
  gsub("[\\p{P}\\p{S}\\s]", "", tolower(stringi::stri_trans_nfkc(x)), perl = TRUE)
}
author_names <- function(e) {
  a <- unclass(e)[[1]]$author
  if (is.null(a)) return("")
  paste(norm(unlist(lapply(a, function(p) c(p$family, p$given)))), collapse = " ")
}
# Title variants: full title, original-language part and translations, which
# appear as "Original [Translation]" in the .bib and "A / B" in the sheets
title_variants <- function(x) {
  x <- x[!is.na(x)]
  x <- sub(" \\((Master's|Doctoral) (thesis|dissertation)\\).*$", "", x)
  parts <- c(x, unlist(strsplit(x, " / ", fixed = TRUE)),
    sub("\\s*\\[.*$", "", x), regmatches(x, regexpr("(?<=\\[).*(?=\\])", x, perl = TRUE)))
  v <- unique(norm(parts))
  v[nchar(v) >= 8]
}

# Plain-text reference from R's default bibentry style, without markdown markup
format_reference <- function(e) {
  format(e, style = "text") %>%
    paste(collapse = " ") %>%
    gsub("\\s+", " ", .) %>%
    gsub("&amp\\\\mathsemicolon", "&", .) %>%
    gsub(" <https://doi\\.org/[^>]*%2F[^>]*>\\.", "", .) %>%
    gsub("doi:(\\S+) <https://doi\\.org/[^>]*>", "https://doi.org/\\1", .) %>%
    gsub("<(https?://[^>]*)>", "\\1", .) %>%
    gsub(" https?://dx\\.doi\\.org/\\S+", "", .) %>%
    gsub("(^|[ (“])[_*]|[_*]([,.()” ]|$)", "\\1\\2", .) %>%
    gsub("&amp;", "&", ., fixed = TRUE) %>%
    # LaTeX and markup left over from the .bib file
    gsub("\\\\Sexpr\\[[^]]*\\]\\{tools:::Rd_expr_doi\\(\"([^\"]+)\"\\)\\}", "https://doi.org/\\1", .) %>%
    gsub("<=ss/?(scp|sup)\\\\greater", "", .) %>%
    gsub("\\\\t\\{ex\\}tquotesingle|\\\\textquotesingle", "'", .) %>%
    gsub("\\\\textendash", "–", .) %>%
    gsub("\\\\ding\\{73\\}|\\\\ast", "", .) %>%
    gsub("\\\\'\\\\i", "í", .) %>%
    gsub("\\\\u([a-zA-Z])", "\\1̈", .) %>%
    gsub("\\\\([#_])", "\\1", .) %>%
    stringi::stri_trans_nfc() %>%
    gsub("(?<!\\.)\\.\\.(?!\\.)", ".", ., perl = TRUE) %>%
    trimws()
}

bd <- tibble(
  i = seq_along(bib),
  key = vapply(bib, bib_key, character(1)),
  doi = tolower(vapply(bib, field, character(1), "doi")),
  title = vapply(bib, field, character(1), "title"),
  year = substr(vapply(bib, field, character(1), "year"), 1, 4),
  names = vapply(bib, author_names, character(1))
)
bd$variants <- lapply(bd$title, title_variants)

# Sources -----------------------------------------------------------------------

dataset <- readRDS("web_app/shiny_code/dataset.rds")

sources <- dataset %>%
  distinct(label = metaUI__article_label, .keep_all = TRUE) %>%
  transmute(label, author_year, title, year = as.character(year)) %>%
  mutate(ids = strsplit(label, "; ", fixed = TRUE))

# Combined labels ("id1; id2") take the metadata of their first listed ID
sources <- sources %>%
  mutate(meta_row = vapply(ids, function(x) match(x, metadata$ID) %>% na.omit() %>% c(NA) %>% .[1], integer(1))) %>%
  mutate(
    meta_authors = metadata$Authors[meta_row],
    meta_year = metadata$Year[meta_row],
    meta_title = metadata$Title[meta_row],
    meta_doi = tolower(metadata$DOI[meta_row]),
    surname = norm(sub("^([A-Z]\\. )?(.*?)( et al\\.| &| and|,| \\().*$", "\\2", author_year, perl = TRUE)),
    bib_i = NA_integer_, match = NA_character_, distance = NA_real_
  )
# One source has its title in the author_year field
sources$variants <- lapply(seq_len(nrow(sources)), function(s) {
  title_variants(c(sources$title[s], sources$meta_title[s],
    if (!grepl("\\(", sources$author_year[s])) sources$author_year[s]))
})

assign_match <- function(s, j, how, d = NA_real_) {
  sources$bib_i[s] <<- j
  sources$match[s] <<- how
  sources$distance[s] <<- d
}

# Matching ----------------------------------------------------------------------

# Romanisation and year variants checked by hand against the screening metadata
# (source label = bib key)
manual_matches <- c(
  "gsne_93_ko" = "Deok-seop2008", # 심덕섭 & 양동민 (Sim/Shim & Yang)
  "gsne_166_ja" = "Nakauchi2008", # 中内 & 飯尾 (Nakuchi/Nakauchi & Iio)
  "gsne_185_ko" = "Roh2017", # 노종호 (No/Roh), same title
  "gsne_200_ja" = "hirai2012", # 平井, 渡部 & 犬塚, same title; year 2013 in the sheet
  # Checked against the coded PDFs: the .bib previously held wrong references
  # for these sources (corrected there), and the article's reference list
  # still does (see article_errata below)
  "gsne_14_ko" = "Yoo2019",
  "gsne_16_ko" = "Cho2020",
  "gsne_17_ko" = "KimSong2015",
  "gsne_46_it" = "Antonelli2012",
  "38031" = "Wu2013",
  # The coded PDF is Kuan's (2008) thesis, also coded as source 39201; the
  # screening sheet lists a different title for this ID
  "38270" = "Kuan2008a"
)
for (s in which(sources$label %in% names(manual_matches))) {
  assign_match(s, match(manual_matches[[sources$label[s]]], bd$key), "manual")
}

# DOI, from the label or the screening sheet
for (s in which(is.na(sources$bib_i))) {
  dois <- na.omit(c(tolower(sources$ids[[s]]), sources$meta_doi[s]))
  j <- which(bd$doi %in% dois)
  if (length(j)) assign_match(s, j[1], "doi")
}

# Exact title, comparing original-language and translated titles
for (s in which(is.na(sources$bib_i))) {
  j <- which(vapply(bd$variants, function(v) any(v %in% sources$variants[[s]]), logical(1)))
  if (length(j)) assign_match(s, j[1], "title")
}

# Similar title (translations, typos, encoding problems) by q-gram cosine
# distance: the closest entry from the same year if below 0.3, otherwise the
# closest entry from an adjacent year if below 0.1
title_distance <- function(a, b) {
  if (!length(a) || !length(b)) return(NA_real_)
  min(stringdist::stringdistmatrix(a, b, method = "cosine", q = 3))
}
for (s in which(is.na(sources$bib_i))) {
  d <- vapply(bd$variants, title_distance, numeric(1), a = sources$variants[[s]])
  if (all(is.na(d))) next
  years <- as.numeric(na.omit(c(sources$year[s], sources$meta_year[s])))
  gap <- vapply(as.numeric(bd$year), function(y) min(abs(y - years), Inf), numeric(1))
  d_year <- ifelse(!is.na(gap) & gap == 0, d, NA)
  d_near <- ifelse(!is.na(gap) & gap <= 1, d, NA)
  if (any(!is.na(d_year)) && min(d_year, na.rm = TRUE) < .3) {
    assign_match(s, which.min(d_year), "similar_title", min(d_year, na.rm = TRUE))
  } else if (any(!is.na(d_near)) && min(d_near, na.rm = TRUE) < .1) {
    assign_match(s, which.min(d_near), "similar_title", min(d_near, na.rm = TRUE))
  }
}

# First-author surname and year, if exactly one entry not yet used fits
for (s in which(is.na(sources$bib_i))) {
  unused <- bd %>% filter(!i %in% sources$bib_i)
  cand <- unused %>% filter(
    year %in% c(sources$year[s], sources$meta_year[s]),
    grepl(paste0("\\b", sources$surname[s], "\\b"), names) | startsWith(norm(key), sources$surname[s])
  )
  if (nrow(cand) == 1) assign_match(s, cand$i, "author_year")
}

# References --------------------------------------------------------------------

# Sources without a .bib entry: authors, year, title and DOI from the screening sheet
metadata_reference <- function(s) {
  au <- coalesce(sources$meta_authors[s], sub(" ?\\(.*$", "", sources$author_year[s]))
  yr <- coalesce(sources$meta_year[s], sources$year[s])
  ti <- coalesce(sources$meta_title[s], sources$title[s])
  doi <- coalesce(sources$meta_doi[s], if (grepl("^10\\.", sources$label[s])) sources$label[s] else NA)
  paste0(au, " (", yr, ").", if (!is.na(ti)) paste0(" ", sub("\\.?$", ".", ti)),
    if (!is.na(doi)) paste0(" https://doi.org/", doi))
}

# Article reference list ----------------------------------------------------------

# Reference list of the published article (Wallrich et al., 2024), one entry per
# line; included studies are marked with "* ". Citations and reference texts in
# the app follow it, so that sources can be looked up in the article.
article <- tibble(text = readLines("web_app/article_references.txt", encoding = "UTF-8")) %>%
  mutate(
    included = startsWith(text, "* "),
    text = sub("^\\* ", "", text),
    authors = sub("^(.*?) \\((\\d{4}[a-z]?|n\\.d\\.[a-z-]*)(, [^)]*)?\\)\\..*$", "\\1", text, perl = TRUE),
    year = sub("^.*? \\((\\d{4}[a-z]?|n\\.d\\.[a-z-]*)(, [^)]*)?\\)\\..*$", "\\1", text, perl = TRUE),
    title = sub("^.*? \\((\\d{4}[a-z]?|n\\.d\\.[a-z-]*)(, [^)]*)?\\)\\. ", "", text, perl = TRUE),
    doi = tolower(sub("\\.$", "", stringr::str_extract(text, "(?<=doi\\.org/)\\S+"))),
    surname = sub(",.*$", "", authors),
    initials = ifelse(grepl(",", authors), trimws(sub("^[^,]*, ([^,&]*).*$", "\\1", authors)), "")
  )
# The title field runs on into the journal details, so titles are compared as prefixes
article$ntitle <- norm(article$title)
article$ntitle_translated <- norm(stringr::str_extract(article$title, "(?<=\\[)[^\\]]*(?=\\])"))

# APA in-text citation from the author list as printed: one author, two
# authors joined by "&", or "et al."; initials where different first authors
# share a surname
shared_surname <- article %>% distinct(surname, initials) %>% count(surname) %>% filter(n > 1) %>% pull(surname)
apa_citation <- function(k) {
  au <- article$authors[k]
  first <- article$surname[k]
  # Drop initials printed after the surname ("Hirai Y.") and given names in
  # parentheses ("X.-H. (Frank)")
  first <- sub(" ([A-Z]\\.)+$", "", first)
  initials <- trimws(gsub("\\([^)]*\\)", "", article$initials[k]))
  if (article$surname[k] %in% shared_surname && nchar(initials)) first <- paste(initials, first)
  before_and <- sub(" & .*$", "", au)
  names <- if (!grepl(" & ", au)) first else if (grepl("(\\.|\\)), \\S", before_and)) paste(first, "et al.") else
    paste(first, "&", sub(" ([A-Z]\\.)+$", "", sub(",.*$", "", sub("^.* & ", "", au))))
  paste0(names, " (", article$year[k], ")")
}
article$citation <- vapply(seq_len(nrow(article)), apa_citation, character(1))

# Article entry for each source: DOI, exact title, or similar title from the same year
sources$article_i <- NA_integer_
for (s in seq_len(nrow(sources))) {
  j <- sources$bib_i[s]
  dois <- na.omit(c(tolower(sources$ids[[s]]), sources$meta_doi[s], if (!is.na(j)) bd$doi[j]))
  k <- which(article$doi %in% dois)
  variants <- unique(c(sources$variants[[s]], if (!is.na(j)) bd$variants[[j]]))
  variants <- variants[nchar(variants) >= 15]
  years <- as.numeric(na.omit(c(sources$year[s], sources$meta_year[s], if (!is.na(j)) bd$year[j])))
  ay <- suppressWarnings(as.numeric(substr(article$year, 1, 4)))
  year_ok <- is.na(ay) | !length(years) | vapply(ay, function(y) any(abs(y - years) <= 1), logical(1))
  if (!length(k) && length(variants)) {
    k <- which(year_ok & vapply(seq_len(nrow(article)), function(a) {
      any(startsWith(article$ntitle[a], variants)) || article$ntitle_translated[a] %in% variants
    }, logical(1)))
    # Short generic titles can be prefixes of other titles: prefer entries whose
    # first author appears among the source's authors
    au <- norm(paste(sources$author_year[s], sources$meta_authors[s], if (!is.na(j)) bd$names[j]))
    by_author <- k[vapply(norm(sub(" .*$", "", article$surname[k])), function(x) grepl(x, au, fixed = TRUE), logical(1))]
    if (length(by_author)) k <- by_author
  }
  if (!length(k) && length(variants)) {
    # Closest title prefix from the same or an adjacent year
    d <- vapply(seq_len(nrow(article)), function(a) {
      min(stringdist::stringdist(variants, substr(article$ntitle[a], 1, nchar(variants)), "jw"),
        stringdist::stringdist(variants, coalesce(article$ntitle_translated[a], ""), "jw"))
    }, numeric(1))
    d[!year_ok] <- NA
    if (any(!is.na(d)) && min(d, na.rm = TRUE) < .1) k <- which.min(d)
  }
  if (length(k)) sources$article_i[s] <- k[1]
}

# Sources whose reference is wrong or missing in the article's reference list,
# with the entry that stands in for them there
article_errata <- c(
  "gsne_14_ko" = "Park et al. (2021)",
  "gsne_16_ko" = "Sun-hee & Sang-yoon (2022)",
  "gsne_17_ko" = "Shin & Geon (2018)",
  "gsne_46_it" = "a duplicate of Inglês (2009)",
  "38031" = "Wang (2013b)"
)
for (s in which(sources$label %in% names(article_errata))) sources$article_i[s] <- NA

# APA in-text citation from a .bib entry, for sources not in the article
bib_citation <- function(j) {
  au <- unclass(bib[[j]])[[1]]$author
  fam <- vapply(au, function(p) paste(p$family, collapse = " "), character(1))
  names <- if (length(fam) == 1) fam else if (length(fam) == 2) paste(fam, collapse = " & ") else paste(fam[1], "et al.")
  paste0(names, " (", coalesce(bd$year[j], "n.d."), ")")
}

# Link for each source: DOI, otherwise the URL in the .bib
bd$link <- ifelse(!is.na(bd$doi), paste0("https://doi.org/", bd$doi), vapply(bib, field, character(1), "url"))

references <- tibble(
  metaUI__article_label = sources$label,
  # Sources not in the article: citation from the .bib, else the coded label
  citation = coalesce(article$citation[sources$article_i],
    vapply(sources$bib_i, function(j) if (is.na(j)) NA_character_ else bib_citation(j), character(1)),
    sub("\\)[a-z]$", ")", sources$author_year)),
  link = coalesce(
    ifelse(!is.na(article$doi[sources$article_i]), paste0("https://doi.org/", article$doi[sources$article_i]), NA),
    bd$link[sources$bib_i],
    ifelse(!is.na(sources$meta_doi), paste0("https://doi.org/", sources$meta_doi), NA)
  ),
  reference = vapply(seq_len(nrow(sources)), function(s) {
    if (!is.na(sources$article_i[s])) article$text[sources$article_i[s]] else
      if (!is.na(sources$bib_i[s])) format_reference(bib[[sources$bib_i[s]]]) else metadata_reference(s)
  }, character(1)),
  bibtex = vapply(sources$bib_i, function(j) {
    if (is.na(j)) NA_character_ else paste(unique(toBibtex(bib[[j]])), collapse = "\n")
  }, character(1)),
  match = coalesce(sources$match, "screening_metadata")
)

# APA references for these sources, checked against the coded PDFs
corrected_references <- c(
  "gsne_14_ko" = "Yoo, K.-H., Lee, B.-C., & Kim, S.-Y. (2019). 팀 다양성이 팀 성과에 미치는 영향: 변혁적 리더십과 팀 구성원 교환관계의 역할 [The effect of team diversity on team performance: The role of transformational leadership and team member exchange relations]. 인적자원개발연구, 22(2), 59–86. https://doi.org/10.24991/KJHRD.2019.06.22.2.59",
  "gsne_16_ko" = "Cho, S., & Lee, S.-M. (2020). 벤처 창업팀의 다양성이 창업 성과에 미치는 영향에 관한 연구 [The effects of the entrepreneurial team's diversity on business performance of new venture]. 중소기업연구 [Asia Pacific Journal of Small Business], 42(1), 107–133. https://doi.org/10.36491/APJSB.42.1.6",
  "gsne_17_ko" = "Kim, T.-H., & Song, J.-H. (2015). 여성의 TMT참여가 기업의 경영성과 및 대리인비용에 미치는 영향 [The effects of women in top management team on the firm performance and agency cost]. 산업경제연구, 28(3), 1267–1282.",
  "gsne_46_it" = "Antonelli, G., & Consiglio, S. (2012). Caratteristiche del gruppo di potenziali imprenditori per la costituzione di una impresa innovativa [Characteristics of the group of potential entrepreneurs for setting up an innovative company] [Book chapter]. https://www.researchgate.net/publication/304363816",
  "38031" = "Wu, C.-L. (2013). 相似或是互補好？探討團隊年資對團隊成員人格特質多元化與團隊創造力關係的干擾效果 [Supplementary or complementary? Exploring the moderating role of team tenure on the relationship between team personality diversity and team creativity] [Master's thesis]. National Sun Yat-sen University."
)
corrected <- match(references$metaUI__article_label, names(corrected_references))
references$reference[!is.na(corrected)] <- corrected_references[corrected[!is.na(corrected)]]

# Point readers of the article to the entry that stands in for these sources there
errata <- references$metaUI__article_label %in% names(article_errata)
references$reference[errata] <- paste0(references$reference[errata],
  " [Listed in error in the article's reference list as ", article_errata[references$metaUI__article_label[errata]], ".]")

# Some identifiers stored as DOIs are not DOIs (e.g., CNKI codes) and would not resolve
references$link[grepl("doi\\.org/", references$link) & !grepl("doi\\.org/10\\.[0-9]{4,9}/", references$link)] <- NA

saveRDS(references, "web_app/shiny_code/references.rds")

# Non-exact matches for checking by eye
sources %>%
  filter(match %in% c("manual", "similar_title", "author_year")) %>%
  transmute(label, author_year, match, distance = round(distance, 2),
    source_authors = meta_authors, source_title = coalesce(meta_title, title),
    bib_key = bd$key[bib_i], bib_authors = bd$names[bib_i], bib_year = bd$year[bib_i], bib_title = bd$title[bib_i]) %>%
  readr::write_csv("web_app/references_matches_to_check.csv")

message("Sources: ", nrow(references))
print(table(references$match))
