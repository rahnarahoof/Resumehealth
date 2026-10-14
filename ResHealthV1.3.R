# =====================================================================
# HealthyResume - CV vs MANY JOBS (JD URLs) ATS ranking dashboard  (R Shiny)
# One CV is scored against every job; jobs are ranked; you can drill
# into any job for gaps, what-if, radar, highlighted CV, format check
# and AI rewrite suggestions.
# Save as app.R, then run:  shiny::runApp("path/to/folder")
# =====================================================================

# One-time install:
# install.packages(c("shiny","bslib","DT","dplyr","stringr","tibble","ggplot2",
#                    "pdftools","officer","SnowballC","tidytext","httr2","rvest",
#                    "xml2","jsonlite","fmsb","commonmark"))

library(shiny)      # web-app framework (ui + server)
library(bslib)      # modern Bootstrap layouts, value boxes, cards
library(DT)         # interactive tables
library(dplyr)      # data wrangling verbs (group_by, summarise, mutate ...)
library(stringr)    # string tools (str_replace_all, str_count, fixed ...)
library(tibble)     # tibble() / tribble() data frames
library(ggplot2)    # plots
library(pdftools)   # read text from PDF files
library(officer)    # read text and structure from .docx files
library(SnowballC)  # wordStem(): "analysing", "analysed" -> "analys"
library(tidytext)   # stop_words: list of common English filler words

options(shiny.maxRequestSize = 20 * 1024^2)   # allow uploads up to 20 MB

# ---------------------------------------------------------------------
# 1. DICTIONARIES (edit these to tune the app)
# ---------------------------------------------------------------------

# Words ignored when extracting keywords; setdiff() keeps "r" and "c" (the languages)
stop_vec <- setdiff(tidytext::stop_words$word, c("r", "c"))
# Extra filler words found in almost every JD
extra_stop <- c("experience", "ability", "work", "team", "including", "required",
                "responsibilities", "skills", "will", "must", "strong", "years",
                "candidate", "role", "job", "position", "looking", "preferred",
                "apply", "company", "opportunity", "equal", "employer",
                "cookie", "cookies", "privacy", "login", "sign", "subscribe", "newsletter", "menu",
                "search", "home", "contact", "share", "linkedin", "facebook", "twitter", "click",
                "submit", "terms", "conditions", "accept", "browser", "listed", "private", "public")
stop_vec <- union(stop_vec, extra_stop)        # union() = merge without duplicates

# IDEA 5 - synonym table: every "variant" is rewritten to its "canonical" form
syn_tbl <- tribble(                            # tribble() lets us type a table row by row
  ~canonical,                     ~variant,
  "excel",                        "ms excel",
  "excel",                        "microsoft excel",
  "power bi",                     "powerbi",
  "power bi",                     "microsoft power bi",
  "artificial intelligence",      "ai",
  "natural language processing",  "nlp",
  "electronic health record",     "ehr",
  "electronic health record",     "emr",
  "electronic health record",     "electronic medical record",
  "randomized controlled trial",  "rct",
  "randomized controlled trial",  "randomised controlled trial",
  "key performance indicator",    "kpi",
  "javascript",                   "js",
  "python",                       "python3",
  "sql",                          "structured query language",
  "statistical analysis",         "stats",
  "statistical analysis",         "statistical analyses",
  "data visualization",           "data visualisation",
  "analyze",                      "analyse",
  "analyzed",                     "analysed",
  "analyzing",                    "analysing",
  "organization",                 "organisation",
  "phd",                          "ph.d",
  "phd",                          "doctorate",
  "phd",                          "doctor of philosophy",
  "master",                       "msc",
  "master",                       "m.sc",
  "master",                       "masters",
  "master",                       "master's",
  "master",                       "master of science",
  "bachelor",                     "bsc",
  "bachelor",                     "b.sc",
  "bachelor",                     "bachelors",
  "bachelor",                     "bachelor's",
  "bachelor",                     "bachelor of science"
)
# Escape regex symbols so "m.sc" or "c++" are matched literally
esc <- function(x) gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x, perl = TRUE)
syn_tbl <- syn_tbl[order(-nchar(syn_tbl$variant)), ]   # longest variants first so they win
# Named vector: names = regex patterns, values = replacement (used by str_replace_all)
syn_pats <- setNames(
  syn_tbl$canonical,
  paste0("(?<![a-z0-9.])", esc(syn_tbl$variant), "(?![a-z0-9])")  # lookarounds = whole-word match
)

# IDEA 3 - keyword categories for the radar chart
cat_dict <- list(
  "Tools & software" = c("r", "python", "sql", "excel", "spss", "stata", "sas", "tableau",
    "power bi", "redcap", "git", "github", "docker", "aws", "azure", "google cloud", "javascript", "java",
    "c++", "c#", "matlab", "nvivo", "endnote", "zotero", "latex", "jupyter", "shiny",
    "tensorflow", "pytorch", "linux", "jira", "sharepoint", "powerpoint", "microsoft word",
    "microsoft office", "salesforce", "sap", "oracle", "postgresql", "mysql", "mongodb", "hadoop",
    "snowflake", "looker", "google analytics", "vba", "power query", "minitab", "epi info",
    "dhis2", "kobotoolbox", "odk", "arcgis", "qgis", "html", "css", "react", "api", "asana",
    "trello", "confluence", "slack", "quickbooks", "ms project", "nonmem"),
  "Methods & analytics" = c("statistical analysis", "statistics", "regression", "logistic regression",
    "linear regression", "survival analysis", "meta-analysis", "systematic review", "literature review",
    "machine learning", "deep learning", "data science", "data analysis", "data visualization",
    "data cleaning", "data mining", "data management", "data collection", "data quality",
    "data governance", "data engineering", "data warehouse", "business intelligence", "reporting",
    "modeling", "predictive modeling", "forecasting", "hypothesis testing", "sampling",
    "biostatistics", "bayesian", "multivariate analysis", "time series", "cluster analysis",
    "latent class", "causal inference", "propensity score", "natural language processing",
    "randomized controlled trial", "cohort study", "case control", "cross sectional", "study design",
    "protocol development", "research methodology", "mixed methods", "qualitative analysis",
    "quantitative analysis", "monitoring and evaluation", "impact evaluation", "health economics",
    "cost effectiveness", "economic evaluation", "risk assessment", "gap analysis",
    "root cause analysis", "process improvement", "lean six sigma", "agile", "scrum",
    "software development", "version control", "quality assurance", "quality control", "validation",
    "audit", "survey", "etl", "dashboard", "automation"),
  "Domain knowledge" = c("epidemiology", "public health", "global health", "health systems",
    "clinical", "clinical trial", "clinical research", "clinical operations", "clinical data management",
    "healthcare", "health policy", "health technology assessment", "real world evidence",
    "health informatics", "digital health", "telehealth", "mental health", "maternal health",
    "child health", "infectious disease", "hiv", "tuberculosis", "malaria", "vaccine", "immunization",
    "non communicable disease", "one health", "stroke", "cardiovascular", "registry", "surveillance",
    "patient", "disease", "pharmacology", "pharmacovigilance", "drug safety", "pharmaceutical",
    "biotechnology", "medical device", "regulatory affairs", "regulatory submission",
    "common technical document", "ctd", "good clinical practice", "good manufacturing practice",
    "gcp", "gmp", "oncology", "diabetes", "hospital", "medical", "electronic health record",
    "nutrition", "dental", "humanitarian", "sustainable development", "grant writing", "fundraising",
    "donor", "procurement", "logistics", "supply chain", "finance", "accounting", "budgeting",
    "marketing", "sales", "customer service", "human resources", "recruitment", "policy",
    "advocacy", "capacity building", "community engagement", "artificial intelligence",
    "cybersecurity", "compliance", "regulatory", "risk management"),
  "Education & credentials" = c("phd", "master", "bachelor", "degree", "mph", "mba", "mbbs", "md",
    "pharmd", "postdoctoral", "doctoral", "thesis", "dissertation", "certification", "certified",
    "diploma", "license", "fellowship", "residency", "accredited", "pmp", "prince2", "six sigma",
    "publication", "peer reviewed"),
  "Soft skills" = c("communication", "leadership", "teamwork", "collaboration",
    "problem solving", "critical thinking", "project management", "program management", "stakeholder",
    "time management", "mentoring", "coaching", "training", "facilitation", "presentation",
    "public speaking", "report writing", "adaptability", "negotiation", "decision making",
    "analytical", "attention to detail", "creativity", "innovation", "initiative", "interpersonal",
    "organizational skills", "prioritization", "multitasking", "conflict resolution",
    "relationship building", "strategic thinking", "planning", "cross functional")
)

# IDEA 4 - how much a keyword is worth depending on the CV section it appears in
sec_weights <- c(Experience = 1.0, Projects = 0.9, Summary = 0.7, Education = 0.6,
                 Certifications = 0.6, Skills = 0.5, Other = 0.4)

# Regexes that recognise CV section headings (whole line, lower-case)
sec_patterns <- c(
  Summary        = "^((professional|career) )?(summary|profile|objective)$|^about me$",
  Experience     = "^(work |professional |relevant |research |clinical )?(experience|employment|work history|career history)$",
  Education      = "^(education|academic background|academic qualifications?|qualifications?|education (&|and) training)$",
  Skills         = "^(technical |key |core |soft )?(skills|competencies|expertise|technologies|tools)( (&|and) [a-z ]+)?$",
  Projects       = "^(projects?|research projects?|academic projects?|publications?)$",
  Certifications = "^(certifications?|certificates?|licen[cs]es?( (&|and) certifications?)?|training|courses)$"
)

# ---------------------------------------------------------------------
# 2. TEXT HELPERS
# ---------------------------------------------------------------------

# Lower-case the text and (optionally) rewrite synonyms to canonical forms
apply_syn <- function(txt) str_replace_all(tolower(txt), syn_pats)   # named vector = many regex replacements in one call
prep_text <- function(txt, use_syn = TRUE) if (use_syn) apply_syn(txt) else tolower(txt)

# Split text into words, drop stop words, return original + stemmed form
tokenise <- function(txt) {
  w <- txt |>
    tolower() |>                                 # case-insensitive matching
    str_replace_all("[^a-z0-9+#]+", " ") |>      # keep letters, digits, + and # (c++, c#); rest -> space
    str_squish() |>                              # collapse repeated spaces
    str_split(" ") |>                            # one character vector inside a list
    unlist()                                     # flatten to a plain vector
  keep <- w != "" &                              # drop empty strings
    !(w %in% stop_vec) &                         # drop stop words
    (nchar(w) >= 2 | w %in% c("r", "c")) &       # drop 1-letter tokens except R and C
    nchar(w) <= 25 &                             # drop glued-together junk strings
    !str_detect(w, "^[0-9]+$")                   # drop pure numbers
  w <- w[keep]                                   # apply the filter
  tibble(orig = w, stem = SnowballC::wordStem(w))   # wordStem(): English Snowball stemmer
}

# Single words + two-word phrases from a token table
make_terms <- function(tk) {
  uni <- tibble(term = tk$orig, key = tk$stem, type = "word")
  if (nrow(tk) > 1) {
    bi <- tibble(
      term = paste(head(tk$orig, -1), tail(tk$orig, -1)),   # head(-1) drops last, tail(-1) drops first -> neighbour pairs
      key  = paste(head(tk$stem, -1), tail(tk$stem, -1)),
      type = "phrase")
  } else bi <- uni[0, ]                          # zero-row tibble when text is too short
  bind_rows(uni, bi)                             # stack both tables
}

# Stem key of any phrase typed by a human (used for dictionaries)
phrase_key <- function(p) paste(tokenise(apply_syn(p))$stem, collapse = " ")

# Pre-compute stem keys for the dictionaries (done once at start-up)
dict_tbl <- bind_rows(lapply(names(cat_dict), function(cn) {   # loop over categories
  tibble(category = cn, term = cat_dict[[cn]],
         key = vapply(cat_dict[[cn]], phrase_key, character(1), USE.NAMES = FALSE))
})) |> filter(key != "") |> distinct(key, .keep_all = TRUE)    # STANDARD keyword list used for scoring
cat_keys <- dict_tbl |> select(category, key)                  # used to label every keyword with a category

# Optional external standard skills list (.csv or .txt): ESCO skills_en.csv, O*NET Technology Skills, Lightcast, or your own
load_kw_list <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  x <- if (ext == "csv") {
    d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
    pref <- c("preferredLabel", "Example", "Commodity Title", "Element Name", "name", "Name",
              "skill", "Skill", "term", "keyword", "Keyword")      # known column names; otherwise the first column
    col <- intersect(pref, names(d))
    if (length(col)) d[[col[1]]] else d[[1]]
  } else readLines(path, warn = FALSE, encoding = "UTF-8")          # .txt: one skill per line
  x <- unique(str_squish(as.character(x)))
  x <- x[!is.na(x) & nchar(x) >= 2 & nchar(x) <= 50 & str_count(x, "\\S+") <= 4]   # short skill phrases only
  tibble(category = "Imported list", term = x,
         key = vapply(x, phrase_key, character(1), USE.NAMES = FALSE)) |>
    filter(key != "") |> distinct(key, .keep_all = TRUE)
}
syn_tbl$ckey <- vapply(syn_tbl$canonical, phrase_key, character(1), USE.NAMES = FALSE)

# Give each keyword its category (first dictionary entry contained in the keyword)
assign_cat <- function(keys) {
  vapply(keys, function(k) {
    padded <- paste0(" ", k, " ")                # spaces make the match whole-word
    hit <- vapply(paste0(" ", cat_keys$key, " "),
                  function(d) grepl(d, padded, fixed = TRUE), logical(1), USE.NAMES = FALSE)
    if (any(hit)) cat_keys$category[which(hit)[1]] else "Other"
  }, character(1), USE.NAMES = FALSE)
}

# ---------------------------------------------------------------------
# 3. READING FILES, SECTIONS, CV OBJECT
# ---------------------------------------------------------------------

# Read PDF / DOCX into one string, keeping line breaks (needed for headings)
read_doc <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "pdf") {
    paste(pdftools::pdf_text(path), collapse = "\n")       # pdf_text() = one string per page
  } else if (ext == "docx") {
    s <- officer::docx_summary(officer::read_docx(path))   # one row per paragraph / table cell
    paste(s$text[!is.na(s$text)], collapse = "\n")
  } else if (ext %in% c("txt", "md")) {
    paste(readLines(path, warn = FALSE), collapse = "\n")  # plain text: read every line (warn = FALSE hides missing-final-newline notes)
  } else stop("Please upload a .pdf, .docx or .txt file")
}

# IDEA 4 - cut the CV into sections using heading lines
split_sections <- function(txt) {
  lines <- str_split(txt, "\n")[[1]]             # one element per line
  current <- "Other"                             # text before the first heading (name, contacts)
  labels <- character(length(lines))             # section label for every line
  for (i in seq_along(lines)) {                  # walk through the lines
    clean <- lines[i] |> str_squish() |> tolower() |>
      str_remove("[\\-\u2013\u2022:]+$") |> str_squish()   # drop trailing ":" "-" bullets
    if (nchar(clean) > 0 && nchar(clean) <= 40) {          # headings are short lines
      hit <- names(sec_patterns)[vapply(sec_patterns, function(p) str_detect(clean, p), logical(1))]
      if (length(hit) > 0) current <- hit[1]     # a heading starts a new section
    }
    labels[i] <- current
  }
  lapply(split(lines, labels), paste, collapse = "\n")   # named list: section -> text
}

# Everything we need to know about the CV, computed once
build_cv <- function(path, name, use_syn) {
  txt  <- read_doc(path, name)
  secs <- split_sections(txt)
  stem_str <- function(s) paste0(" ", paste(tokenise(prep_text(s, use_syn))$stem, collapse = " "), " ")
  sec_w <- sec_weights
  only_other <- identical(names(secs), "Other")  # no headings recognised at all
  if (only_other) sec_w["Other"] <- 1            # then do not penalise anything
  list(txt = txt,
       stem_all = stem_str(txt),                 # whole CV as a stemmed string
       sec_stem = lapply(secs, stem_str),        # stemmed string per section
       sec_w = sec_w, only_other = only_other, sections = names(secs))
}

# ---------------------------------------------------------------------
# 4. FETCHING JOB DESCRIPTIONS FROM URLS  (idea 10)
# ---------------------------------------------------------------------

# Remove HTML tags from a string
space_tags <- function(h) gsub("(<[a-zA-Z/][^>]*>)", " \\1 ", h, perl = TRUE)   # a space around every tag so neighbouring words never glue together
strip_html <- function(x) rvest::html_text2(rvest::read_html(paste0("<div>", space_tags(x), "</div>")))

# Recursively search parsed JSON-LD for an object of @type "JobPosting"
find_job <- function(x) {
  if (!is.list(x)) return(NULL)                  # leaves cannot contain a job
  ty <- x[["@type"]]                             # NULL for unnamed lists
  if (!is.null(ty) && any(unlist(ty) == "JobPosting")) return(x)
  for (el in x) {                                # otherwise search the children
    r <- find_job(el)
    if (!is.null(r)) return(r)
  }
  NULL
}

# Turn a typed line into a fetchable link: DOIs become https://doi.org/..., web links stay, anything else -> NA
normalise_link <- function(x) {
  m <- str_match(x, regex("^(?:doi:\\s*)?(?:https?://(?:dx\\.)?doi\\.org/)?(10\\.[0-9]{4,9}/\\S+)$",
                          ignore_case = TRUE))   # group 1 = the bare DOI such as 10.1000/xyz123
  ifelse(!is.na(m[, 2]), paste0("https://doi.org/", m[, 2]),
         ifelse(str_detect(x, "^https?://"), x, NA_character_))
}

# Download one JD page and return title + plain text + status message
fetch_jd <- function(url) {
  tryCatch({                                     # any error becomes a status message
    resp <- httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/124 Safari/537.36") |>
      httr2::req_timeout(25) |>                  # give up after 25 s
      httr2::req_perform()                       # redirects (e.g. doi.org -> publisher) are followed automatically
    if (isTRUE(grepl("pdf", httr2::resp_content_type(resp)))) {   # the link resolved to a PDF, not a web page
      tmp <- tempfile(fileext = ".pdf")
      writeBin(httr2::resp_body_raw(resp), tmp)                   # save the raw bytes
      return(list(title = basename(url), text = paste(pdftools::pdf_text(tmp), collapse = "\n"),
                  status = "OK (PDF)"))
    }
    doc <- rvest::read_html(space_tags(httr2::resp_body_string(resp)))
    page_title <- doc |> rvest::html_element("title") |> rvest::html_text2()
    if (is.na(page_title)) page_title <- url

    # Step 1: many job sites embed clean "JobPosting" JSON-LD data
    ld <- doc |> rvest::html_elements("script[type='application/ld+json']") |> rvest::html_text()
    job <- NULL
    for (x in ld) {
      j <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(j)) job <- find_job(j)
      if (!is.null(job)) break
    }
    if (!is.null(job) && !is.null(job$description)) {
      txt <- strip_html(job$description)
      if (grepl("<[a-z]+", txt)) txt <- strip_html(txt)    # description was double-escaped HTML
      ttl <- if (!is.null(job$title)) job$title else page_title
      return(list(title = ttl, text = paste(ttl, txt), status = "OK (JobPosting data)"))
    }

    # Step 2: fall back to the visible page text
    xml2::xml_remove(rvest::html_elements(doc, "script, style, nav, footer, header, noscript, svg"))
    txt <- doc |> rvest::html_element("body") |> rvest::html_text2()
    if (is.na(txt)) txt <- ""
    st <- if (nchar(txt) < 300) "Page text too short (JS-rendered or login wall) - paste the JD instead" else "OK (page text)"
    list(title = page_title, text = txt, status = st)
  }, error = function(e) list(title = url, text = "", status = paste("Failed:", conditionMessage(e))))
}

# ---------------------------------------------------------------------
# 5. CORE SCORING OF ONE JOB
# ---------------------------------------------------------------------
analyse_job <- function(cv, jd_txt, top_n, min_phrase, custom, use_syn, mode = "std", extra = NULL) {
  dict <- if (is.null(extra)) dict_tbl else bind_rows(dict_tbl, extra) |> distinct(key, .keep_all = TRUE)   # built-in + imported list
  jd_tk   <- tokenise(prep_text(jd_txt, use_syn))
  jd_stem <- paste0(" ", paste(jd_tk$stem, collapse = " "), " ")
  pad <- function(k) paste0(" ", k, " ")         # whole-word padding

  # STANDARD keywords: dictionary terms (tools, methods, domain, credentials, soft skills) the JD really mentions
  std <- dict |>
    mutate(n = vapply(key, function(k) str_count(jd_stem, fixed(pad(k))), numeric(1), USE.NAMES = FALSE)) |>
    filter(n > 0) |>
    transmute(key, type = "standard", term, n, weight = n) |>   # importance = how often the JD mentions it
    arrange(desc(n), term) |>
    slice_head(n = top_n)

  # optional extra: frequent JD words / phrases that are NOT in the dictionary
  auto <- tibble(key = character(), type = character(), term = character(), n = integer(), weight = integer())
  if (mode == "hybrid") {
    auto <- make_terms(jd_tk) |>
      group_by(key, type) |>
      summarise(term = first(term), n = n(), .groups = "drop") |>
      filter(n >= 2, type == "word" | n >= min_phrase, !key %in% std$key) |>
      arrange(desc(n), term) |>
      slice_head(n = max(top_n - nrow(std), 0)) |>
      mutate(weight = n)
  }
  kw <- bind_rows(std, auto)

  # must-have keywords typed by the user get the highest weight
  if (length(custom) > 0) {
    ckey <- vapply(custom, function(s) paste(tokenise(prep_text(s, use_syn))$stem, collapse = " "),
                   character(1), USE.NAMES = FALSE)
    cust <- tibble(term = custom, key = ckey, type = "must-have") |>
      filter(key != "") |>
      mutate(n = str_count(jd_stem, fixed(pad(key))),        # fixed() = literal match, not regex
             weight = max(c(kw$weight, 1)))
    kw <- bind_rows(cust, kw) |> distinct(key, .keep_all = TRUE)   # must-haves win duplicates
  }
  if (nrow(kw) == 0) return(NULL)

  # counts per CV section -> matrix (rows = keywords, columns = sections)
  cnt_mat <- do.call(cbind, lapply(cv$sec_stem, function(st)
    vapply(kw$key, function(k) str_count(st, fixed(pad(k))), numeric(1), USE.NAMES = FALSE)))
  cv_count <- vapply(kw$key, function(k) str_count(cv$stem_all, fixed(pad(k))),
                     numeric(1), USE.NAMES = FALSE)
  found <- cv_count > 0
  # section credit = weight of the BEST section where the keyword appears
  credit <- apply(cnt_mat, 1, function(r) if (any(r > 0)) max(cv$sec_w[names(r)[r > 0]]) else 0)
  credit <- ifelse(found & credit == 0, 0.4, credit)         # found only across a section border
  found_in <- apply(cnt_mat, 1, function(r) paste(names(r)[r > 0], collapse = ", "))

  kw <- kw |> mutate(category = ifelse(assign_cat(key) != "Other", assign_cat(key),
                                     ifelse(is.na(dict$category[match(key, dict$key)]), "Other", dict$category[match(key, dict$key)])), cv_count = cv_count, found = found,
                     status = ifelse(found, "Matched", "Missing"),
                     credit = credit, found_in = found_in)

  list(kw = kw,
       score     = round(100 * sum(kw$weight * kw$found)  / sum(kw$weight), 1),   # basic weighted ATS score
       sec_score = round(100 * sum(kw$weight * kw$credit) / sum(kw$weight), 1),   # section-aware score
       cover     = round(100 * mean(kw$found), 1),                                # % of keywords present
       n_match = sum(kw$found), n_miss = sum(!kw$found))
}

# ---------------------------------------------------------------------
# 6. ATS FORMAT CHECK (idea 7), HIGHLIGHTING (idea 2)
# ---------------------------------------------------------------------
fmt_check <- function(path, name, txt, sections) {
  ext   <- tolower(tools::file_ext(name))
  words <- str_count(txt, "\\S+")                # count non-space chunks
  pages <- if (ext == "pdf") tryCatch(pdftools::pdf_info(path)$pages, error = function(e) NA) else ceiling(words / 500)
  chk <- function(check, status, detail) tibble(Check = check, Status = status, Detail = detail)
  miss_head <- setdiff(c("Experience", "Education", "Skills"), sections)   # standard headings not found

  out <- list(
    chk("Text can be extracted", if (words >= 150) "Pass" else "Fail",
        paste(words, "words read", if (words < 150) "- scanned/image CV? ATS cannot read it" else "")),
    chk("Length", if (!is.na(pages) && pages <= 3) "Pass" else "Warn",
        paste("About", pages, "page(s); aim for 1-3 unless academic")),
    chk("Email address", if (str_detect(txt, "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}")) "Pass" else "Fail",
        "Recruiters and ATS parse contact details first"),
    chk("Phone number", if (str_detect(txt, "\\+?\\d[\\d\\s().-]{8,}\\d")) "Pass" else "Warn",
        "A clearly written phone number"),
    chk("Dates present", if (str_count(txt, "(19|20)[0-9]{2}") >= 2) "Pass" else "Warn",
        "ATS builds your timeline from years like 2021"),
    chk("Standard headings", if (length(miss_head) == 0) "Pass" else "Warn",
        if (length(miss_head) == 0) "Experience, Education and Skills headings found"
        else paste("Not recognised:", paste(miss_head, collapse = ", "), "- use plain standard headings")),
    chk("Special characters", if (str_count(txt, "\\P{ASCII}") / max(nchar(txt), 1) < 0.03) "Pass" else "Warn",
        "Fancy icons / symbols often turn into junk in ATS")
  )

  if (ext == "docx") {                           # structure checks via the Word XML
    st <- tryCatch({
      x  <- officer::read_docx(path)$doc_obj$get()      # xml_document of the Word body
      ns <- xml2::xml_ns(x)                             # namespaces (w: = Word)
      list(tbl = length(xml2::xml_find_all(x, "//w:tbl", ns)),
           img = length(xml2::xml_find_all(x, "//w:drawing|//w:pict", ns)),
           box = length(xml2::xml_find_all(x, "//w:txbxContent", ns)),
           cols = suppressWarnings(max(c(1, as.numeric(
             xml2::xml_attr(xml2::xml_find_all(x, "//w:cols", ns), "w:num", ns))), na.rm = TRUE)))
    }, error = function(e) NULL)
    if (is.null(st)) {
      out <- c(out, list(chk("Word structure", "Warn", "Could not inspect the .docx structure")))
    } else {
      out <- c(out, list(
        chk("No tables", if (st$tbl == 0) "Pass" else "Warn", paste(st$tbl, "table(s); some ATS read tables out of order")),
        chk("No images / graphics", if (st$img == 0) "Pass" else "Warn", paste(st$img, "image(s); photos and logos are ignored")),
        chk("No text boxes", if (st$box == 0) "Pass" else "Fail", paste(st$box, "text box(es); many ATS skip them")),
        chk("Single column", if (st$cols < 2) "Pass" else "Warn", paste(st$cols, "column layout"))
      ))
    }
  } else {                                       # PDF: rough two-column guess from word positions
    col <- tryCatch({
      d  <- pdftools::pdf_data(path)[[1]]        # word boxes of page 1 (x, width, text ...)
      pw <- pdftools::pdf_pagesize(path)$width[1]
      cross <- mean(d$x < 0.52 * pw & (d$x + d$width) > 0.48 * pw)   # share of words crossing the page centre
      left  <- mean((d$x + d$width / 2) < 0.5 * pw)
      nrow(d) > 100 && cross < 0.02 && left > 0.3 && left < 0.7
    }, error = function(e) NA)
    out <- c(out, list(chk("Single column", if (isTRUE(col)) "Warn" else "Pass",
                           if (isTRUE(col)) "Looks like a two-column layout (guess) - single column is safer" else "No two-column pattern detected")))
  }
  bind_rows(out)
}

# Wrap matched words of the CV in green <mark> tags
highlight_cv <- function(txt, hl_stems) {
  loc <- str_locate_all(txt, "[A-Za-z0-9+#]+")[[1]]        # start/end of every word
  if (nrow(loc) == 0) return(htmltools::HTML(htmltools::htmlEscape(txt)))
  n     <- nrow(loc)
  words <- substring(txt, loc[, 1], loc[, 2])
  hit   <- SnowballC::wordStem(tolower(words)) %in% hl_stems   # stem match, like the scoring
  gaps  <- substring(txt, c(1, loc[, 2] + 1), c(loc[, 1] - 1, nchar(txt)))  # text between words
  w_html <- ifelse(hit,
                   paste0("<mark style='background:#c8e6c9;padding:0 2px'>", htmltools::htmlEscape(words), "</mark>"),
                   htmltools::htmlEscape(words))
  gaps <- htmltools::htmlEscape(gaps)
  out  <- paste0(paste0(gaps[seq_len(n)], w_html, collapse = ""), gaps[n + 1])
  htmltools::HTML(gsub("\n", "<br>", out))
}

score_theme <- function(s) if (s >= 75) "success" else if (s >= 50) "warning" else "danger"

# ---------------------------------------------------------------------
# 6b. COVER LETTER HELPERS (about 300-word body in standard business-letter format)
# ---------------------------------------------------------------------

# Join words naturally: "a, b and c" (single letters such as r become R)
join_and <- function(x) {
  x <- ifelse(nchar(x) == 1, toupper(x), x)      # "r" -> "R"
  if (length(x) == 0) "the skills described in the advertisement"
  else if (length(x) == 1) x
  else paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))   # head(-1) = all but last
}

# Fixed opening of the closing motivation sentence (career-growth perspective)
MOTIVE_STEM <- "I am especially interested in the position because"

# Top matched keywords of the given categories (falls back to all matched keywords)
pick_terms <- function(kw, cats, n) {
  m <- kw |> filter(found) |> arrange(desc(weight))
  x <- m |> filter(category %in% cats) |> pull(term)
  if (length(x) == 0) x <- pull(m, term)
  head(x, n)
}

# One closing sentence: always starts with MOTIVE_STEM. Uses the user's own reason if typed.
motive_sentence <- function(motive, company, kw) {
  m <- trimws(motive)
  m <- trimws(sub(paste0("^", MOTIVE_STEM, "\\s*"), "", m, ignore.case = TRUE))   # avoid repeating the stem
  m <- sub("[.!?]+$", "", m)
  if (nzchar(m)) return(paste0(MOTIVE_STEM, " ", m, "."))
  dom   <- pick_terms(kw, c("Domain knowledge", "Methods & analytics"), 2)
  area  <- if (length(dom)) join_and(dom) else "my field"
  where <- if (nzchar(company)) company else "your organisation"
  paste0(MOTIVE_STEM, " it offers a clear path to deepen my expertise in ", area,
         " and to grow into greater responsibility at ", where, ".")
}

# TRUE when the last sentence of the text begins with MOTIVE_STEM
ends_with_motive <- function(txt) {
  s <- trimws(str_split(trimws(txt), regex("(?<=[.!?])\\s+"))[[1]])
  length(s) > 0 && startsWith(tail(s, 1), MOTIVE_STEM)
}

# Offline template body: fit paragraph, education, experience, skills, closing + motivation sentence
tpl_body <- function(job, company, bg, kw, edu, exp, motive) {
  comp <- if (nzchar(company)) company else "your organisation"
  all_ <- pick_terms(kw, c("Domain knowledge", "Methods & analytics"), 6)
  e_kw <- head(all_, 3)                                              # keywords for the education paragraph
  x_kw <- if (length(all_) > 3) all_[4:length(all_)] else e_kw       # next ones for experience
  t_kw <- pick_terms(kw, c("Tools & software", "Methods & analytics"), 5)
  s_kw <- pick_terms(kw, "Soft skills", 3)
  gap  <- head(kw |> filter(!found) |> arrange(desc(weight)) |> pull(term), 2)
  edu_txt <- if (nzchar(edu)) sub("[.]+$", "", edu) else "[your degree, institution and year]"
  exp_txt <- if (nzchar(exp)) sub("[.]+$", "", exp) else "[your most relevant roles, employers and one measurable achievement]"
  s_txt   <- if (length(s_kw)) join_and(s_kw) else "communication and problem-solving skills"

  p1 <- sprintf("I am writing to apply for the %s position at %s. With a background in %s, I believe my education, professional experience and technical skills make me a strong match for the requirements set out in your advertisement, and I would welcome the chance to put them to work for your team.",
                job, comp, bg)
  p2 <- sprintf("My education provides a solid foundation for this role. My education includes %s. This training developed my competence in %s, which are central to the position, and taught me to work rigorously and to base conclusions on evidence.",
                edu_txt, join_and(e_kw))
  p3 <- sprintf("My professional experience is closely aligned with your needs. My experience includes %s. In these roles I have worked with %s, turning complex information into reliable results that colleagues can act on, while consistently meeting deadlines and quality standards.",
                exp_txt, join_and(x_kw))
  p4 <- paste0(sprintf("In terms of skills, I bring working proficiency in %s, together with strong %s. I communicate findings clearly to specialist and non-specialist audiences and collaborate effectively across disciplines.",
                       join_and(t_kw), s_txt),
               if (length(gap)) sprintf(" I am also keen to strengthen my skills in %s and learn new methods quickly.", join_and(gap)) else "")
  p5 <- paste(sprintf("Thank you for considering my application; I would welcome the opportunity to discuss how I can contribute to %s.", comp),
              motive_sentence(motive, company, kw))                  # last sentence of the body
  paste(c(p1, p2, p3, p4, p5), collapse = "\n\n")                    # blank line between paragraphs
}

# Send one request to the Anthropic Messages API and return the reply text
claude_text <- function(key, model, system, messages, max_tokens = 1200) {
  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01") |>   # backticks: header names contain "-"
    httr2::req_body_json(list(model = model, max_tokens = max_tokens,
                              system = system, messages = messages)) |>
    httr2::req_timeout(90) |>
    httr2::req_perform()
  httr2::resp_body_json(resp)$content[[1]]$text
}

# AI body: ~300 words focused on suitability; last sentence must start with MOTIVE_STEM
ai_cover_body <- function(key, model, cv_txt, jd_txt, job, company, bg, kw, edu, exp, motive) {
  matched <- kw |> filter(found)  |> arrange(desc(weight)) |> pull(term)
  missing <- kw |> filter(!found) |> arrange(desc(weight)) |> pull(term)
  sys <- paste("You write honest, specific cover letters.",
               "Use ONLY facts found in the CV; never invent employers, numbers, tools or qualifications.")
  prompt <- paste0(
    "Write the BODY of a cover letter for the job '", job, "'",
    if (nzchar(company)) paste0(" at ", company) else "", ".\n",
    "The letter must focus on the applicant's SUITABILITY for this job, shown through (1) education, ",
    "(2) professional experience and (3) skills, each taken from the CV and tied to what the job description asks for.\n",
    "Applicant background: ", bg, "\n",
    if (nzchar(edu)) paste0("Education to highlight: ", edu, "\n") else "",
    if (nzchar(exp)) paste0("Experience to highlight: ", exp, "\n") else "",
    "Keywords the CV already matches (weave in naturally): ", paste(head(matched, 10), collapse = ", "), "\n",
    "Skills the job wants but the CV lacks (do NOT claim them; at most mention eagerness to develop one): ",
    paste(head(missing, 5), collapse = ", "), "\n\n",
    "Structure: paragraph 1 = application and overall fit; paragraph 2 = education; paragraph 3 = experience with concrete evidence from the CV; ",
    "paragraph 4 = technical and soft skills; paragraph 5 = short thank-you and close.\n",
    "The LAST sentence of the body must be ONE sentence on motivation from a career-growth perspective (why this role or organisation) ",
    "and must begin exactly with: \"", MOTIVE_STEM, "\"",
    if (nzchar(motive)) paste0(". Base it on this reason: ", motive) else ". Base it on what the job description says about the role or organisation",
    ".\n",
    "Rules: 5 paragraphs, between 290 and 310 words, professional first-person tone. ",
    "Return ONLY the body paragraphs: no greeting, no sign-off, no headings, no Markdown, no placeholders.\n\n",
    "CV:\n", substr(cv_txt, 1, 15000), "\n\nJOB DESCRIPTION (full text):\n", jd_txt)   # JD is sent in full (no limit)
  msgs <- list(list(role = "user", content = prompt))
  txt  <- trimws(claude_text(key, model, sys, msgs, max_tokens = 1500))
  wc   <- str_count(txt, "\\S+")
  if (wc < 270 || wc > 330 || !ends_with_motive(txt)) {                  # length or ending wrong -> one retry
    msgs <- c(msgs, list(list(role = "assistant", content = txt),
                         list(role = "user", content = sprintf("That draft has %d words%s. Rewrite it to about 300 words (290-310), same rules, body paragraphs only, and make the final sentence begin exactly with \"%s\".",
                                                               wc, if (ends_with_motive(txt)) "" else " and its last sentence does not start with the required phrase", MOTIVE_STEM))))
    txt <- trimws(claude_text(key, model, sys, msgs, max_tokens = 1500))
  }
  if (!ends_with_motive(txt)) txt <- paste(txt, motive_sentence(motive, company, kw))   # safety net
  txt
}

# Wrap the body in a standard business-letter layout
build_letter <- function(name, city, contact, manager, company, job, body) {
  parts <- trimws(c(city, contact))
  addr  <- paste(parts[nzchar(parts)], collapse = " | ")                 # "City | email | phone"
  manager <- trimws(manager); company <- trimws(company)
  recip <- c(if (nzchar(manager)) manager else "Hiring Manager", if (nzchar(company)) company)
  sal   <- if (nzchar(manager)) paste0("Dear ", manager, ",") else "Dear Hiring Manager,"
  paste(c(name, if (nzchar(addr)) addr,                                  # sender block
          "", format(Sys.Date(), "%d %B %Y"),                            # date, e.g. 14 October 2026
          "", recip,                                                     # recipient block
          "", paste0("Subject: Application for ", job),                  # subject line
          "", sal, "", body,                                             # salutation + body
          "", "Sincerely,", "", name),                                   # closing + signature
        collapse = "\n")
}

# ---------------------------------------------------------------------
# 7. USER INTERFACE
# ---------------------------------------------------------------------
ui <- page_sidebar(
  title = "HealthyResume v3 (standard keywords)",                       # name shown in the app's top bar
  window_title = "HealthyResume",                # name shown on the browser tab
  theme = bs_theme(
    version   = 5,            # Bootstrap 5 (needed by bslib cards and value boxes)
    bg        = "#ffffff",    # page background: white
    fg        = "#2e2a45",    # main text: very dark purple-grey (easier on the eye than pure black)
    primary   = "#8e6bd8",    # main purple: buttons, active tab, "Matched / Missing" box
    secondary = "#b39ddb"     # lighter purple: secondary value boxes
  ) |>
    bs_add_rules("
      .navbar { background-color: #ede7f6 !important; }                 /* top bar: light purple */
      .navbar-brand { color: #4a3a7a !important; font-weight: 500; }    /* app name colour */
      .bslib-sidebar-layout > .sidebar { background-color: #f6f2fc; }   /* sidebar: very light purple */
      .nav-tabs .nav-link { color: #6b5f8f; }                           /* inactive tab text */
      .nav-tabs .nav-link.active { color: #5e3fb0; font-weight: 500; border-bottom: 2px solid #8e6bd8; }
      .card-header { background-color: #f6f2fc; }                       /* card headers match the sidebar */
    "),
  sidebar = sidebar(
    width = 360,
    fileInput("cv", "1. Upload CV (.pdf / .docx)", accept = c(".pdf", ".docx")),
    textAreaInput("urls", "2. Job description URLs or DOIs (one per line)", rows = 5,
                  placeholder = "https://company.com/careers/job-1\n10.1000/xyz123\nhttps://doi.org/10.1000/xyz123"),
    fileInput("jd_files", "...or upload JD files (.pdf / .docx / .txt)", multiple = TRUE,
              accept = c(".pdf", ".docx", ".txt")),
    textAreaInput("jd_paste", "...or paste JD text (no length limit; separate jobs with a line of ---)", rows = 6),
    textAreaInput("custom", "3. Must-have keywords (optional, comma separated)", rows = 2,
                  placeholder = "e.g. R, SPSS, survival analysis"),
    radioButtons("kw_mode", "Keywords tested",
                 c("Standard keywords only (recommended)" = "std", "Standard + frequent JD terms" = "hybrid")),
    fileInput("kw_file", "Optional: standard skills list (.csv / .txt) e.g. ESCO, O*NET, Lightcast or your own",
              accept = c(".csv", ".txt")),
    sliderInput("top_n", "Max keywords tested per job", 10, 100, 40, step = 5),
    sliderInput("min_phrase", "Min. JD frequency for 2-word phrases (frequent-terms mode only)", 1, 5, 2, step = 1),
    checkboxInput("use_syn", "Use synonym dictionary (RCT = randomized controlled trial, ...)", TRUE),
    actionButton("go", "Rank jobs", class = "btn-primary w-100"),
    hr(),
    selectInput("rank_by", "Rank jobs by", c("ATS score", "Section-aware")),
    selectInput("job", "Inspect job", choices = NULL),
    downloadButton("dl_rank", "Ranking (CSV)", class = "w-100 mb-2"),
    downloadButton("dl_job", "Selected job keywords (CSV)", class = "w-100")
  ),
  navset_card_tab(
    nav_panel("Job ranking",
              DTOutput("rank_tbl"),
              plotOutput("rank_plot", height = 340)),
    nav_panel("Gap analysis & what-if",
              uiOutput("boxes"),
              card(card_header("What-if simulator"),
                   sliderInput("n_add", "If I add this many missing keywords to my CV...", 0, 25, 5, width = "100%"),
                   uiOutput("whatif_txt"),
                   plotOutput("whatif_plot", height = 260)),
              uiOutput("missing"),
              plotOutput("gap_plot", height = 460)),
    nav_panel("Skill radar",
              plotOutput("radar", height = 480),
              tableOutput("radar_tbl")),
    nav_panel("Keyword table", DTOutput("tbl")),
    nav_panel("Highlighted CV", uiOutput("hl")),
    nav_panel("ATS format check", uiOutput("fmt_box"), DTOutput("fmt_tbl")),
    nav_panel("AI rewrite assistant",
              layout_columns(
                passwordInput("api_key", "Anthropic API key (or set ANTHROPIC_API_KEY)"),
                textInput("model", "Model", "claude-sonnet-5-5"),
                actionButton("ai_go", "Generate suggestions", class = "btn-primary mt-4")),
              helpText("Only your CV text, the selected JD and the missing keywords are sent, and only when you click the button."),
              uiOutput("ai_out")),
    nav_panel("Cover letter",
              layout_columns(
                textInput("cl_name", "Your name"),
                textInput("cl_city", "City"),
                textInput("cl_contact", "Email | phone")),
              layout_columns(
                textInput("cl_manager", "Hiring manager (optional)"),
                textInput("cl_company", "Company / organisation"),
                textInput("cl_job", "Job title (optional)", placeholder = "auto-filled from the ranked job or URL/DOI")),
              card(card_header("Job description for this letter"),
                   radioButtons("cl_jd_src", NULL, inline = TRUE,
                                c("Job chosen in 'Inspect job'" = "sel", "DOI / URL" = "url", "Free text (no limit)" = "txt")),
                   conditionalPanel("input.cl_jd_src == 'url'",
                                    textInput("cl_jd_url", "Job description URL or DOI", width = "100%",
                                              placeholder = "https://company.com/careers/job-1  or  10.1000/xyz123")),
                   conditionalPanel("input.cl_jd_src == 'txt'",
                                    textAreaInput("cl_jd_txt", "Paste the full job description (no length limit)",
                                                  rows = 10, width = "100%"))),
              layout_columns(
                textInput("cl_bg", "Your background in a few words", placeholder = "e.g. public health research"),
                textAreaInput("cl_edu", "Education to highlight (optional)", rows = 2,
                              placeholder = "e.g. an MPH in Epidemiology from University X (2021)"),
                textAreaInput("cl_exp", "Experience to highlight (optional)", rows = 2,
                              placeholder = "e.g. five years running a stroke registry and leading statistical analyses")),
              textAreaInput("cl_motive", "Career-growth reason for this role / organisation (optional; finishes the last sentence)",
                            rows = 2, width = "100%",
                            placeholder = "e.g. it combines large-scale registry research with the chance to lead analytic projects"),
              layout_columns(
                radioButtons("cl_mode", "Write with",
                             c("Template (offline, no key)" = "tpl", "Claude (key from the AI tab)" = "ai")),
                actionButton("cl_go", "Write 300-word cover letter", class = "btn-primary mt-4"),
                downloadButton("cl_dl", "Download .docx", class = "mt-4")),
              uiOutput("cl_wc"),
              textAreaInput("cl_text", NULL, rows = 22, width = "100%"),
              helpText("The letter focuses on your suitability (education, experience, skills) and ends with one career-growth motivation sentence starting 'I am especially interested in the position because'. Fill any [bracketed] placeholders before sending. Claude mode sends your CV and the full JD text to the API."))
  )
)

# ---------------------------------------------------------------------
# 8. SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # imported standard skills list (NULL when no file is uploaded)
  ext_dict <- reactive({
    if (is.null(input$kw_file)) return(NULL)
    withProgress(message = "Reading skills list...", value = 0.5,
      tryCatch(load_kw_list(input$kw_file$datapath, input$kw_file$name),
               error = function(e) { showNotification(paste("Skills list not read:", conditionMessage(e)), type = "error"); NULL }))
  })

  # ----- main computation: runs only when "Rank jobs" is clicked -----
  res <- eventReactive(input$go, {
    validate(need(!is.null(input$cv), "Upload a CV first."))
    cv <- build_cv(input$cv$datapath, input$cv$name, input$use_syn)

    raw  <- str_squish(str_split(input$urls, "\n")[[1]])
    urls <- unique(na.omit(normalise_link(raw[nzchar(raw)])))        # DOIs -> doi.org links; drop non-links and duplicates
    n_files <- if (is.null(input$jd_files)) 0 else nrow(input$jd_files)   # number of uploaded JD files
    pasted <- str_split(paste0("\n", input$jd_paste, "\n"), "\\n\\s*-{3,}\\s*\\n")[[1]]
    pasted <- pasted[nchar(str_squish(pasted)) > 50]                 # ignore empty chunks
    validate(need(length(urls) + n_files + length(pasted) > 0,
                  "Add at least one JD URL, DOI, file or pasted JD."))

    skills <- str_squish(str_split(input$custom, "[,\n]")[[1]])
    skills <- skills[skills != ""]

    jobs <- list()
    add_job <- function(title, source, text, status) {               # score one job and store it
      a <- if (nchar(text) >= 100) analyse_job(cv, text, input$top_n, input$min_phrase, skills, input$use_syn, input$kw_mode, ext_dict()) else NULL
      if (is.null(a) && startsWith(status, "OK")) status <- if (input$kw_mode == "std") "No standard keywords found - try 'Standard + frequent JD terms'" else "No keywords could be extracted"
      lab <- paste0(length(jobs) + 1, ". ", str_trunc(str_squish(title), 55))
      jobs[[lab]] <<- list(label = lab, source = source, text = text, status = status, a = a)
    }
    n_all <- length(urls) + n_files + length(pasted)
    withProgress(message = "Fetching and scoring jobs", value = 0, {
      for (u in urls) {
        incProgress(1 / n_all, detail = u)
        f <- fetch_jd(u)
        add_job(f$title, u, f$text, f$status)
      }
      for (k in seq_len(n_files)) {                                  # uploaded JD files (.pdf / .docx / .txt)
        incProgress(1 / n_all, detail = input$jd_files$name[k])
        txt <- tryCatch(read_doc(input$jd_files$datapath[k], input$jd_files$name[k]), error = function(e) "")
        add_job(tools::file_path_sans_ext(input$jd_files$name[k]), input$jd_files$name[k], txt,
                if (nzchar(txt)) "OK (file)" else "Could not read file")
      }
      for (k in seq_along(pasted)) {
        incProgress(1 / n_all, detail = paste("Pasted JD", k))
        add_job(paste("Pasted JD", k), "pasted text", pasted[k], "OK (pasted)")
      }
    })
    list(cv = cv, jobs = jobs,
         fmt = fmt_check(input$cv$datapath, input$cv$name, cv$txt, cv$sections))
  })

  # ----- ranking table (idea 10) -----
  rank_df <- reactive({
    r <- res()
    df <- bind_rows(lapply(r$jobs, function(j) tibble(
      Job = j$label,
      `ATS score`     = if (is.null(j$a)) NA_real_ else j$a$score,
      `Section-aware` = if (is.null(j$a)) NA_real_ else j$a$sec_score,
      Coverage        = if (is.null(j$a)) NA_real_ else j$a$cover,
      Matched         = if (is.null(j$a)) NA_integer_ else as.integer(j$a$n_match),
      Missing         = if (is.null(j$a)) NA_integer_ else as.integer(j$a$n_miss),
      Status = j$status, Source = j$source)))
    col <- if (input$rank_by == "ATS score") "ATS score" else "Section-aware"
    df |> arrange(desc(.data[[col]])) |>                          # NA scores sink to the bottom
      mutate(Rank = ifelse(is.na(.data[[col]]), NA, row_number()), .before = 1)
  })

  # fill the "Inspect job" drop-down after each run (best job pre-selected)
  observeEvent(res(), {
    ok <- rank_df() |> filter(!is.na(`ATS score`))
    updateSelectInput(session, "job", choices = ok$Job, selected = ok$Job[1])
  })
  # clicking a table row also selects that job
  observeEvent(input$rank_tbl_rows_selected, {
    df <- rank_df(); i <- input$rank_tbl_rows_selected
    if (!is.na(df$`ATS score`[i])) updateSelectInput(session, "job", selected = df$Job[i])
  })

  output$rank_tbl <- renderDT({
    datatable(rank_df(), rownames = FALSE, selection = "single",
              options = list(pageLength = 10, dom = "tip")) |>
      formatStyle(c("ATS score", "Section-aware"),
                  background = styleColorBar(c(0, 100), "#d1c4e9"),   # in-cell bars
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$rank_plot <- renderPlot({
    df <- rank_df() |> filter(!is.na(`ATS score`))
    validate(need(nrow(df) > 0, "No job could be scored - check the Status column above."))
    d <- bind_rows(                                              # long format without extra packages
      transmute(df, Job, Rank, Score = "ATS score", value = `ATS score`),
      transmute(df, Job, Rank, Score = "Section-aware", value = `Section-aware`))
    ggplot(d, aes(x = reorder(Job, -Rank), y = value, fill = Score)) +
      geom_col(position = "dodge") + coord_flip() +
      scale_fill_manual(values = c("ATS score" = "#8e6bd8", "Section-aware" = "#b39ddb")) +
      labs(x = NULL, y = "Match %", title = "CV match by job") + theme_minimal(base_size = 13)
  })

  # ----- selected job -----
  sel <- reactive({
    req(res(), input$job)
    j <- res()$jobs[[input$job]]
    req(j, j$a)
    j
  })

  output$boxes <- renderUI({
    a <- sel()$a
    layout_columns(
      value_box("ATS match (weighted)", paste0(a$score, "%"), theme = score_theme(a$score)),
      value_box("Section-aware match", paste0(a$sec_score, "%"), theme = score_theme(a$sec_score)),
      value_box("Matched / Missing", paste(a$n_match, "/", a$n_miss), theme = "primary"),
      value_box("Keyword coverage", paste0(a$cover, "%"), theme = "secondary"))
  })

  # ----- idea 1: what-if simulator -----
  curve <- reactive({
    kw <- sel()$a$kw
    gaps <- kw |> filter(!found) |> arrange(desc(weight))        # best gaps to close first
    m <- min(nrow(gaps), 25)
    tibble(added = 0:m,
           score = 100 * (sum(kw$weight[kw$found]) + c(0, cumsum(gaps$weight[seq_len(m)]))) / sum(kw$weight))
  })
  output$whatif_txt <- renderUI({
    cv_ <- curve(); n <- min(input$n_add, max(cv_$added))
    gaps <- sel()$a$kw |> filter(!found) |> arrange(desc(weight)) |> slice_head(n = n)
    tagList(h5(sprintf("Adding the top %d missing keywords lifts the match from %.1f%% to %.1f%%",
                       n, cv_$score[1], cv_$score[n + 1])),
            if (n > 0) p(strong("Add: "), paste(gaps$term, collapse = ", "), " (only where truthful)"))
  })
  output$whatif_plot <- renderPlot({
    cv_ <- curve(); n <- min(input$n_add, max(cv_$added))
    ggplot(cv_, aes(added, score)) + geom_line(colour = "#8e6bd8", linewidth = 1) + geom_point() +
      geom_point(data = cv_[cv_$added == n, ], colour = "#c62828", size = 5) +
      ylim(0, 100) + labs(x = "Missing keywords added", y = "Projected ATS match %") +
      theme_minimal(base_size = 13)
  })

  output$missing <- renderUI({
    m <- sel()$a$kw |> filter(!found) |> arrange(desc(weight))
    if (nrow(m) == 0) return(h5("No missing keywords - excellent match!"))
    tagList(h5("Missing keywords, highest priority first:"),
            lapply(seq_len(nrow(m)), function(i) span(class = "badge text-bg-danger me-1 mb-1", m$term[i])))
  })
  output$gap_plot <- renderPlot({
    d <- sel()$a$kw |> arrange(desc(weight)) |> slice_head(n = 25)
    ggplot(d, aes(x = reorder(term, weight), y = weight, fill = status)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c(Matched = "#2e7d32", Missing = "#c62828")) +
      labs(x = NULL, y = "JD importance (frequency / weight)", fill = NULL,
           title = "Top JD keywords: matched vs missing") + theme_minimal(base_size = 13)
  })

  # ----- idea 3: radar by category -----
  radar_df <- reactive({
    sel()$a$kw |> filter(category != "Other") |> group_by(category) |>
      summarise(Keywords = n(), Matched = sum(found), `Coverage %` = round(100 * sum(weight * found) / sum(weight), 1),
                .groups = "drop")
  })
  output$radar <- renderPlot({
    d <- radar_df(); validate(need(nrow(d) >= 1, "No categorised keywords for this job."))
    if (nrow(d) < 3) {                                           # radar needs >= 3 axes
      barplot(d$`Coverage %`, names.arg = d$category, ylim = c(0, 100), col = "#8e6bd8", ylab = "Coverage %")
    } else {
      df <- as.data.frame(rbind(rep(100, nrow(d)), rep(0, nrow(d)), d$`Coverage %`))   # rows 1-2 = axis max / min
      colnames(df) <- d$category
      fmsb::radarchart(df, axistype = 1, seg = 4, caxislabels = c("0", "25", "50", "75", "100"),
                       pcol = "#8e6bd8", pfcol = adjustcolor("#8e6bd8", 0.35), plwd = 3,
                       cglcol = "grey70", cglty = 1, axislabcol = "grey40", vlcex = 1.1)
    }
  })
  output$radar_tbl <- renderTable(radar_df() |> rename(Category = category))

  # ----- keyword table with section evidence (idea 4) -----
  output$tbl <- renderDT({
    d <- sel()$a$kw |> transmute(
      Keyword = term, Category = category, Type = type, `JD count` = n, `CV count` = cv_count,
      `Found in` = ifelse(found_in == "", "-", found_in), Status = status,
      Tip = ifelse(!found, "Add if truthful",
                   ifelse(credit <= 0.5, "Show it in an Experience bullet", "OK")))
    datatable(d, rownames = FALSE, options = list(pageLength = 15)) |>
      formatStyle("Status", color = styleEqual(c("Matched", "Missing"), c("#2e7d32", "#c62828")), fontWeight = "bold")
  })

  # ----- idea 2: highlighted CV -----
  output$hl <- renderUI({
    kw <- sel()$a$kw
    keys <- kw$key[kw$found]
    syn_hit <- syn_tbl$ckey %in% keys                             # synonym variants of matched keywords
    var_stems <- unlist(lapply(syn_tbl$variant[syn_hit], function(v) tokenise(v)$stem))
    stems <- unique(c(unlist(str_split(keys, " ")), var_stems))
    tagList(
      p(span(style = "background:#c8e6c9;padding:0 4px", "green"), " = words that matched this job's keywords"),
      if (res()$cv$only_other) p(em("No standard section headings were detected in your CV.")),
      div(style = "max-height:640px;overflow:auto;background:#fff;padding:14px;border:1px solid #ddd;line-height:1.5",
          highlight_cv(res()$cv$txt, stems)))
  })

  # ----- idea 7: ATS format check -----
  output$fmt_box <- renderUI({
    f <- res()$fmt
    pct <- round(100 * mean(c(Pass = 1, Warn = 0.5, Fail = 0)[f$Status]))
    value_box("ATS format health", paste0(pct, "%"), theme = score_theme(pct), max_height = "130px")
  })
  output$fmt_tbl <- renderDT({
    datatable(res()$fmt, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("Status", fontWeight = "bold",
                  color = styleEqual(c("Pass", "Warn", "Fail"), c("#2e7d32", "#ef6c00", "#c62828")))
  })

  # ----- idea 9: AI rewrite assistant (Anthropic API via httr2) -----
  ai_text <- reactiveVal(NULL)
  observeEvent(input$ai_go, {
    key <- trimws(input$api_key)
    if (!nzchar(key)) key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (!nzchar(key)) { showNotification("Enter an Anthropic API key first.", type = "error"); return() }
    j <- sel()
    miss <- j$a$kw |> filter(!found) |> arrange(desc(weight)) |> slice_head(n = 15)
    prompt <- paste0(
      "CV:\n", substr(res()$cv$txt, 1, 9000), "\n\nJOB DESCRIPTION:\n", j$text,
      "\n\nMISSING KEYWORDS (by priority): ", paste(miss$term, collapse = ", "),
      "\n\nTasks:\n1. Write a tailored professional summary (max 60 words) using ONLY facts in the CV.\n",
      "2. For up to 8 missing keywords: if the CV contains real supporting evidence, rewrite ONE existing CV bullet ",
      "(quote the original briefly) so it naturally includes the keyword. If there is no evidence, write ",
      "'Only add if you genuinely have this experience' and suggest how to gain or demonstrate it.\n",
      "3. Give 3 quick wins to improve ATS match.\nUse Markdown.")
    withProgress(message = "Asking Claude...", value = 0.5, {
      out <- tryCatch({
        resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
          httr2::req_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01") |>
          httr2::req_body_json(list(
            model = input$model, max_tokens = 1500,
            system = "You are a careful career coach. Never invent experience, employers, numbers or qualifications.",
            messages = list(list(role = "user", content = prompt)))) |>
          httr2::req_timeout(90) |>
          httr2::req_perform()
        httr2::resp_body_json(resp)$content[[1]]$text
      }, error = function(e) paste("**Error:**", conditionMessage(e)))
    })
    ai_text(out)
  })
  output$ai_out <- renderUI({ req(ai_text()); markdown(ai_text()) })

  # ----- cover letter (about 300-word body: suitability + one career-growth sentence) -----
  observeEvent(input$cl_go, {
    if (is.null(input$cv)) { showNotification("Upload your CV in the sidebar first.", type = "error"); return() }
    skills <- str_squish(str_split(input$custom, "[,\n]")[[1]]); skills <- skills[skills != ""]
    src <- input$cl_jd_src
    job <- trimws(input$cl_job)

    # 1) get the job description (ranked job, DOI/URL, or unlimited free text) and its keyword analysis
    if (src == "sel") {
      j <- tryCatch(sel(), error = function(e) NULL)
      if (is.null(j)) {
        showNotification("Click 'Rank jobs' and pick a job, or switch the source to DOI / URL or Free text.", type = "error"); return()
      }
      cv <- res()$cv; jd_txt <- j$text; a <- j$a
      if (!nzchar(job)) job <- sub("^[0-9]+\\. ", "", j$label)
    } else {
      cv <- build_cv(input$cv$datapath, input$cv$name, input$use_syn)
      if (src == "url") {
        link <- normalise_link(trimws(input$cl_jd_url))
        if (is.na(link)) { showNotification("Enter a valid web link or DOI.", type = "error"); return() }
        f <- NULL
        withProgress(message = "Fetching job description...", value = 0.5, { f <- fetch_jd(link) })
        if (nchar(f$text) < 100) { showNotification(paste("Could not read the job description:", f$status), type = "error"); return() }
        jd_txt <- f$text
        if (!nzchar(job)) job <- str_trunc(str_squish(f$title), 80)
      } else {
        jd_txt <- input$cl_jd_txt
        if (nchar(str_squish(jd_txt)) < 100) { showNotification("Paste the job description text (at least a few sentences).", type = "error"); return() }
        if (!nzchar(job)) job <- "[Job title]"
      }
      a <- analyse_job(cv, jd_txt, input$top_n, input$min_phrase, skills, input$use_syn, input$kw_mode, ext_dict())
      if (is.null(a)) { showNotification("No keywords could be extracted from this job description.", type = "error"); return() }
    }

    # 2) letter details
    name    <- if (nzchar(trimws(input$cl_name))) trimws(input$cl_name) else "[Your name]"
    company <- trimws(input$cl_company)
    bg      <- if (nzchar(trimws(input$cl_bg))) trimws(input$cl_bg) else "[your field of expertise]"
    edu     <- trimws(input$cl_edu); exp <- trimws(input$cl_exp); motive <- trimws(input$cl_motive)

    # 3) write the body
    if (input$cl_mode == "tpl") {
      body <- tpl_body(job, company, bg, a$kw, edu, exp, motive)             # offline template
    } else {
      key <- trimws(input$api_key)
      if (!nzchar(key)) key <- Sys.getenv("ANTHROPIC_API_KEY")
      if (!nzchar(key)) { showNotification("Enter an Anthropic API key in the AI tab first.", type = "error"); return() }
      body <- NULL
      withProgress(message = "Writing cover letter...", value = 0.5, {
        body <- tryCatch(
          ai_cover_body(key, input$model, cv$txt, jd_txt, job, company, bg, a$kw, edu, exp, motive),
          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      })
      if (is.null(body)) return()
    }
    updateTextAreaInput(session, "cl_text",
      value = build_letter(name, input$cl_city, input$cl_contact, input$cl_manager, company, job, body))
  })

  # live word count of the body (text between "Dear ..." and "Sincerely,")
  output$cl_wc <- renderUI({
    req(nzchar(input$cl_text))
    m  <- str_match(input$cl_text, "(?s)Dear[^\n]*\n(.*)\nSincerely,")        # (?s) lets . match line breaks
    wc <- if (is.na(m[1, 2])) str_count(input$cl_text, "\\S+") else str_count(m[1, 2], "\\S+")
    p(class = if (wc >= 270 && wc <= 330) "text-success" else "text-warning",
      strong(wc), " words in the letter body (target about 300)")
  })

  output$cl_dl <- downloadHandler(
    filename = function() paste0("cover_letter_", Sys.Date(), ".docx"),
    content = function(file) {
      doc <- officer::read_docx()                                             # blank Word document
      for (ln in str_split(input$cl_text, "\n")[[1]]) doc <- officer::body_add_par(doc, ln)   # one paragraph per line
      print(doc, target = file)                                               # print() on an rdocx writes the file
    })

  # ----- downloads -----
  output$dl_rank <- downloadHandler(
    filename = function() paste0("job_ranking_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rank_df(), file, row.names = FALSE))
  output$dl_job <- downloadHandler(
    filename = function() paste0("job_keywords_", Sys.Date(), ".csv"),
    content = function(file) write.csv(sel()$a$kw |> select(term, category, type, jd_count = n, cv_count, found_in, status),
                                       file, row.names = FALSE))
}

shinyApp(ui, server)
