# Draft function from Claude Sonnet 5.5 to read an advice sheet

# install.packages(c("pdftools", "dplyr", "stringr", "purrr", "tibble", "tidyr", "lubridate"))
library(pdftools)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(lubridate)

pdf_path <- "dgs_27_nea.pdf"

# ---- Read the PDF two ways: text per page, and every word with x/y coordinates ----
norm      <- function(x) str_replace_all(x, "\\p{Zs}", " ")      # NBSP/thin spaces -> normal space
pages_txt <- norm(pdf_text(pdf_path))
pages_xy  <- pdf_data(pdf_path)
page_h    <- pdf_pagesize(pdf_path)$height[1]

region_text <- function(page, y_from, y_to) {
  pages_xy[[page]] |>
    filter(y >= y_from, y <= y_to) |>
    arrange(round(y / 4), x) |>
    pull(text) |>
    paste(collapse = " ") |>
    norm()
}

# ---- 1. Header (page 1): publication date ----
header_txt <- region_text(1, 0, 0.07 * page_h)
pub_date <- header_txt |>
  str_match("Published\\s+(\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4})") |>
  (\(m) m[, 2])() |>
  dmy()

# ---- 2. Footer (page 1): stock code and link to the advice sheet ----
footer_txt <- region_text(1, 0.88 * page_h, page_h)
stock_code <- str_match(footer_txt, "\\b([a-z]{3}\\.\\d{2}\\.[a-z0-9.]+?)\\s+[–-]")[, 2]
advice_url <- str_match(footer_txt, "(https?://\\S+)")[, 2]

# ---- 3. Catch advice from the first section (page 1), with years ----
section <- str_match(
  pages_txt[1],
  "(?s)\\n\\s*ICES advice on fishing opportunities\\s*\\n(.*?)\\n\\s*Non-fisheries conservation considerations"
)[, 2]

m <- str_match(
  str_squish(section),
  "catches in (\\d{4}) and (\\d{4}) should be no more than ([\\d\\s]+?)\\s*tonnes and ([\\d\\s]+?)\\s*tonnes"
)

advice_tbl <- tibble(
  year           = as.integer(m[, 2:3]),
  catch_advice_t = as.numeric(str_remove_all(m[, 4:5], "\\D"))   # "20 124" -> 20124
)

# ---- 4. Full table whose caption mentions "catch scenarios" ----
# Caption title is "Annual catch scenarios" (Table 2). Table 10's caption also mentions
# "catch scenarios", but only in passing, so this pattern matches on the caption's title sentence.
cap_pat <- regex("^Table\\s+(\\d+[a-z]?)\\s+[^.]*\\.[^.]*catch scenarios?", multiline = TRUE)

hits <- map(seq_along(pages_txt), \(p) {
  mm <- str_match(pages_txt[p], cap_pat)
  if (is.na(mm[1, 1])) NULL else tibble(page = p, table_no = mm[1, 2], caption = str_squish(mm[1, 1]))
}) |> list_rbind()
hits   # check which caption(s) matched

tbl_page <- hits$page[1]
tbl_no   <- hits$table_no[1]

lines <- str_split(pages_txt[tbl_page], "\n")[[1]] |> str_squish()
start <- which(str_detect(lines, paste0("^Table\\s+", tbl_no, "\\s")))[1]
end   <- start + which(str_detect(lines[-seq_len(start)], "^\\*"))[1]      # first footnote line
body  <- lines[(start + 1):(end - 1)] |> str_replace_all("\u2212", "-")     # real minus sign -> "-"

# A data row = optional label + 10 numbers + "< 1"
num_pat <- "^(.*?)\\s*((?:-?\\d[\\d.]*\\s+){9}-?\\d[\\d.]*)\\s*<\\s*1\\s*$"

group <- NA_character_; pending <- character(); rows <- list()
for (ln in body) {
  if (ln %in% c("ICES advice basis", "Other scenarios")) {
    group <- ln; pending <- character(); next
  }
  mm <- str_match(ln, num_pat)
  if (!is.na(mm[1, 1])) {
    # the first row's label is split across the lines above its numbers
    label <- str_squish(paste(c(pending, mm[1, 2]), collapse = " "))
    rows[[length(rows) + 1]] <- tibble(
      scenario_group = group,
      basis          = label,
      values         = list(as.numeric(str_split_1(str_squish(mm[1, 3]), " ")))
    )
    pending <- character()
  } else {
    pending <- c(pending, ln)
  }
}

col_names <- c("catch_2027", "catch_2028",
               "harvest_rate_2027", "harvest_rate_2028",
               "btot_2028", "btot_2029",
               "btot_change_pct_2028", "btot_change_pct_2029",
               "advice_change_pct_2027", "advice_change_pct_2028")

scenario_tbl <- bind_rows(rows) |>
  mutate(values = map(values, \(v) set_names(v, col_names))) |>
  unnest_wider(values) |>
  mutate(prob_ssb_2029_below_blim_pct = "<1")      # reported as "< 1" for every scenario

stopifnot(nrow(scenario_tbl) == 4)

# ---- 5. Total catch 2025 and discards from Table 7 (coordinates, since the layout is graphic-like) ----
p7  <- which(str_detect(str_squish(pages_txt), "Table 7 Spurdog in the Northeast Atlantic\\. Catch distribution"))[1]
w   <- pages_xy[[p7]]
cap <- which(w$text == "Table" & lead(w$text) == "7")[1]
below <- w |> filter(y > w$y[cap] + 5)

hdr_land <- below |> filter(text == "Landings") |> slice(1)
hdr_disc <- below |> filter(text == "Discards") |> slice(1)
tn       <- below |> filter(text == "tonnes")

vals <- map(seq_len(nrow(tn)), \(i) {
  below |>
    filter(str_detect(text, "^\\d+$"), abs(y - tn$y[i]) <= 3, x < tn$x[i]) |>
    slice_max(x, n = 1) |>
    transmute(tonnes = as.numeric(text), cx = x + width / 2)
}) |> list_rbind()

total_catch_2025 <- max(vals$tonnes)
others <- vals |> filter(tonnes != total_catch_2025) |>
  mutate(d_land = abs(cx - (hdr_land$x + hdr_land$width / 2)),
         d_disc = abs(cx - (hdr_disc$x + hdr_disc$width / 2)))
landings_2025 <- others$tonnes[which.min(others$d_land)]
discards_2025 <- others$tonnes[which.min(others$d_disc)]

# Sanity checks: parts must sum to the total, and agree with the 2025 row of Table 6
stopifnot(landings_2025 + discards_2025 == total_catch_2025)
t6 <- str_match(pages_txt, regex("^2025\\s+MSY approach\\s+\\d+\\s+\\d+§\\s+(\\d+)\\s+(\\d+)\\s*$", multiline = TRUE))
t6 <- t6[!is.na(t6[, 1]), , drop = FALSE]
if (nrow(t6) && (as.numeric(t6[1, 2]) != landings_2025 || as.numeric(t6[1, 3]) != discards_2025))
  warning("Landings/discards from Table 7 disagree with Table 6, so check the x-position assignment.")

discard_rate_2025 <- discards_2025 / total_catch_2025

# ---- 6. Final tibble ----
result <- tibble(
  source_file       = basename(pdf_path),
  publication_date  = pub_date,
  stock_code        = stock_code,
  advice_url        = advice_url,
  total_catch_2025_t = total_catch_2025,
  discards_2025_t   = discards_2025,
  discard_rate_2025 = discard_rate_2025,
  catch_advice      = list(advice_tbl),
  catch_scenarios   = list(scenario_tbl)
)

result
advice_tbl
scenario_tbl

# One row per advice year, with the scalar fields repeated:
result |> select(-catch_scenarios) |> unnest(catch_advice)