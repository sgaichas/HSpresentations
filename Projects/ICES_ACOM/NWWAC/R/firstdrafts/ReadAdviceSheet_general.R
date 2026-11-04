# Draft code from Claude Sonnet 5.5, more generalized with three sheets

# install.packages(c("pdftools", "dplyr", "stringr", "purrr", "tibble", "tidyr", "lubridate"))
library(pdftools); library(dplyr); library(stringr)
library(purrr);    library(tibble); library(tidyr); library(lubridate)

extract_ices_advice <- function(pdf_path,
                                header_frac = 0.15,   # top share of page 1 searched for "Published ..."
                                footer_frac = 0.15,   # bottom share of page 1 searched for code + link
                                caption_pattern = "catch scenarios?") {
  
  safe <- function(expr, default = NA) tryCatch(expr, error = \(e) default)
  norm <- function(x) {
    x |> str_replace_all("\\p{Zs}", " ") |>
      str_replace_all("\u2212", "-") |>                          # true minus sign -> "-"
      str_replace_all("(?<=[\\s(])\u2013(?=\\d)", "-")           # en dash used as a minus
  }
  
  pages_txt <- norm(pdf_text(pdf_path))
  pages_xy  <- pdf_data(pdf_path)
  page_h    <- pdf_pagesize(pdf_path)$height[1]
  tab_lines <- map(pages_txt, \(p) str_squish(str_split_1(p, "\n")))
  
  region_text <- function(page, y_from, y_to) {
    pages_xy[[page]] |>
      filter(y >= y_from, y <= y_to) |>
      arrange(round(y / 4), x) |>
      pull(text) |> paste(collapse = " ") |> norm()
  }
  
  # ---------- 1. Header: publication date ----------
  pub_pat  <- "Published\\s+(\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4})"
  pub_date <- safe({
    d <- str_match(region_text(1, 0, header_frac * page_h), pub_pat)[, 2]
    if (is.na(d)) d <- str_match(pages_txt[1], pub_pat)[, 2]
    dmy(d)
  }, as.Date(NA))
  
  # ---------- 2. Footer: stock code + link ----------
  foot_pat <- "ICES Advice\\s+\\d{4}\\s+[\u2013-]\\s+(\\S+)\\s+[\u2013-]\\s+(https?://\\S+)"
  foot <- safe({
    m <- str_match(region_text(1, (1 - footer_frac) * page_h, page_h), foot_pat)
    if (is.na(m[1, 1])) m <- str_match(pages_txt[1], foot_pat)
    m
  }, matrix(NA_character_, 1, 3))
  stock_code <- foot[1, 2]
  advice_url <- foot[1, 3]
  
  # ---------- 3. Catch / landings advice (first section) ----------
  advice_tbl <- safe({
    sec   <- str_match(pages_txt[1],
                       "(?s)ICES advice on fishing opportunities\\s*\\n(.*?)Non-fisheries conservation")[, 2]
    sents <- str_split_1(str_squish(sec), "(?<=\\.)\\s+(?=[A-Z])")
    s     <- sents[str_detect(sents, "tonnes|zero|cannot provide")][1]
    yrs   <- as.integer(str_extract_all(s, "\\b20\\d{2}\\b")[[1]])
    nums  <- str_extract_all(str_remove_all(s, "\\b20\\d{2}\\b"), "\\d{1,3}(?:\\s\\d{3})+|\\d+")[[1]]
    vals  <- as.numeric(str_remove_all(nums, "\\s"))
    if (length(vals) == 1) vals <- rep(vals, length(yrs))   # "51 tonnes in each of 2027 and 2028"
    typ <- str_extract(str_to_lower(s), "catches|catch|landings")
    tibble(year = yrs, advice_t = vals[seq_along(yrs)],
           advice_type = if_else(typ == "landings", "landings", "catch"))
  }, tibble(year = integer(), advice_t = numeric(), advice_type = character()))
  
  # ---------- 4. "Catch scenarios" table(s) ----------
  # Finds every table whose caption names the phrase within its first two sentences, then parses it as
  #  (a) a scenario grid (rows of numbers under "ICES advice basis" / "Other scenarios"), or
  #  (b) a basis table (label + value + unit rows, as in Category 3 stocks).
  cap_hits <- imap(tab_lines, \(ln, p) {
    map(which(str_detect(ln, "^Table\\s+\\d+[a-z]?\\s")), \(i) {
      cap    <- str_squish(paste(ln[i:min(i + 2, length(ln))], collapse = " "))
      first2 <- str_split_1(cap, "(?<=\\.)\\s+(?=[A-Z])")[1:2] |> na.omit() |> paste(collapse = " ")
      if (str_detect(first2, regex(caption_pattern, ignore_case = TRUE)))
        tibble(page = p, line = i, table_no = str_match(ln[i], "^Table\\s+(\\d+[a-z]?)")[, 2], caption = first2)
    }) |> list_rbind()
  }) |> list_rbind()
  
  parse_scenario_table <- function(hit) {
    ln    <- tab_lines[[hit$page]]
    after <- ln[-seq_len(hit$line)]
    end   <- which(str_detect(after, "^\\*"))[1]             # first footnote line ends the table
    body  <- if (is.na(end)) after else after[seq_len(end - 1)]
    body  <- body[nzchar(body)]
    grp_pat <- regex("^(ICES advice basis|Other scenarios)$", ignore_case = TRUE)
    
    if (!any(str_detect(body, grp_pat))) {                   # ---- (b) basis table ----
      unit <- "(?:tonnes?|kg(?: km-\\d)?|km|cm|mm|g|m|years?)"
      return(map(body, \(l) {
        m <- str_match(l, paste0("^(.*?)\\s+(-?\\d+(?:\\.\\d+)?%?)(?:\\s+(", unit, "))?$"))
        if (!is.na(m[1, 1]))
          return(tibble(table_no = hit$table_no, table_type = "basis_table", item = str_squish(m[1, 2]),
                        value_num = as.numeric(str_remove(m[1, 3], "%")), value_text = NA_character_, unit = m[1, 4]))
        m2 <- str_match(l, "^(.*?)\\s+(Not applied|Unquantified|Not quantified)$")
        if (!is.na(m2[1, 1]))
          tibble(table_no = hit$table_no, table_type = "basis_table", item = str_squish(m2[1, 2]),
                 value_num = NA_real_, value_text = m2[1, 3], unit = NA_character_)
      }) |> list_rbind())
    }
    
    # ---- (a) scenario grid ----
    tail_pat <- "^(.*?)(?:^|\\s)([-<\\d. ]*\\d[-<\\d. ]*)$"    # label + trailing run of numbers
    tok_pat  <- "<\\s*[\\d.]+|-?(?:\\d{1,3}(?: \\d{3})+|\\d+)(?:\\.\\d+)?"   # handles "7 337" and "< 0.01"
    is_year_line <- \(prefix, toks) length(toks) > 0 && prefix == "" && all(str_detect(toks, "^20\\d{2}$"))
    
    yr_lines <- body[str_detect(body, "^(?:20\\d{2}\\s*)+$")]
    yrs <- integer()
    if (length(yr_lines)) {
      best <- yr_lines[which.max(str_count(yr_lines, "20\\d{2}"))]
      yrs  <- as.integer(str_extract_all(best, "20\\d{2}")[[1]])
      if (length(yrs) < 2) yrs <- integer()
    }
    
    group <- NA_character_; pending <- character(); rows <- list()
    for (l in body) {
      if (str_detect(l, grp_pat)) { group <- l; pending <- character(); next }
      m      <- str_match(l, tail_pat)
      toks   <- if (is.na(m[1, 1])) character() else str_extract_all(m[1, 3], tok_pat)[[1]]
      prefix <- if (is.na(m[1, 1])) "" else str_squish(m[1, 2])
      if (is_year_line(prefix, toks)) next                   # header row of years
      if (length(toks) >= 3) {
        if (str_detect(prefix, "=$")) { prefix <- paste(prefix, toks[1]); toks <- toks[-1] }  # "F = 0 ..." -> label "F = 0"
        rows[[length(rows) + 1]] <- list(group = group,
                                         label = str_squish(paste(c(pending, prefix), collapse = " ")),
                                         toks  = toks)
        pending <- character()
      } else pending <- c(pending, l)                        # wrapped label line
    }
    if (!length(rows)) return(NULL)
    
    strip_prob <- \(t) if (length(t) && str_detect(t[length(t)], "^<")) t[-length(t)] else t
    k <- min(map_int(rows, \(r) length(strip_prob(r$toks))))       # value columns; any extra token = probability
    nm <- if (k == 10) {
      sfx <- if (length(yrs) >= 10) yrs[1:10] else rep(c("1", "2"), 5)
      paste0(rep(c("catch", "harvest_rate", "btot", "btot_change_pct", "advice_change_pct"), each = 2), "_", sfx)
    } else if (k == 5) {
      c("landings_or_catch_t", "f_ratio", "b_ratio_next_year", "pct_biomass_change", "advice_change_pct")
    } else paste0("v", seq_len(k))
    prob_nm <- if (length(yrs) == k + 1) paste0("prob_below_blim_", yrs[k + 1]) else "prob_below_blim"
    yr_cap  <- as.integer(str_match(hit$caption, "\\bfor (20\\d{2})")[, 2])
    
    map(rows, \(r) {
      t <- r$toks; prob <- NA_character_
      if (length(t) && str_detect(t[length(t)], "^<")) { prob <- str_remove_all(t[length(t)], "\\s"); t <- t[-length(t)] }
      else if (length(t) > k) prob <- t[k + 1]
      v <- as.numeric(str_remove_all(t[seq_len(k)], " "))
      as_tibble(c(list(table_no = hit$table_no, table_type = "scenario_grid", year = yr_cap,
                       scenario_group = r$group, basis = r$label),
                  set_names(as.list(v), nm), set_names(list(prob), prob_nm)))
    }) |> list_rbind()
  }
  
  scen <- if (nrow(cap_hits)) {
    map(seq_len(nrow(cap_hits)), \(r) safe(parse_scenario_table(cap_hits[r, ]), NULL)) |> list_rbind()
  } else tibble()
  
  # ---------- 5. Catch distribution by fleet: catch, landings, discards ----------
  num_left_of <- function(reg, tn_row) {                     # number (possibly split by spaces) just left of "tonnes"
    cand <- reg |> filter(abs(y - tn_row$y) <= 3, x < tn_row$x) |> arrange(desc(x))
    keep <- character(); edge <- tn_row$x
    for (i in seq_len(nrow(cand))) {
      if (!str_detect(cand$text[i], "^\\d+(?:\\.\\d+)?$") || edge - (cand$x[i] + cand$width[i]) > 8) break #"^\\d+$"
      keep <- c(cand$text[i], keep); edge <- cand$x[i]
    }
    if (!length(keep)) return(NULL)
    tibble(value = as.numeric(paste(keep, collapse = "")),
           cx = (edge + tn_row$x + tn_row$width) / 2, txt = "number")
  }
  
  dist <- safe({
    p   <- which(str_detect(str_squish(pages_txt), "Catch distribution by fleet in \\d{4}"))[1]
    w   <- pages_xy[[p]]
    yr  <- as.integer(str_match(str_squish(pages_txt[p]), "Catch distribution by fleet in (\\d{4})")[, 2])
    i   <- which(w$text == "distribution" & lead(w$text) == "by" & lead(w$text, 2) == "fleet")[1]
    reg <- w |> filter(y > w$y[i] + 4, y < w$y[i] + 0.2 * page_h)
    hl  <- reg |> filter(text == "Landings") |> slice(1)
    hd  <- reg |> filter(text == "Discards") |> slice(1)
    hw  <- reg |> filter(abs(y - hl$y) <= 3, x < hl$x)              # "Total catch (2025)" / "Catch (2025)"
    centers <- c(catch    = (min(hw$x) + max(hw$x + hw$width)) / 2,
                 landings = hl$x + hl$width / 2,
                 discards = hd$x + hd$width / 2)
    body <- reg |> filter(y > hl$y + 3)
    tn   <- body |> filter(text == "tonnes")
    vals <- list_rbind(c(
      map(seq_len(nrow(tn)), \(j) num_left_of(body, tn[j, ])),
      list(body |> filter(text == "Unknown") |> transmute(value = NA_real_, cx = x + width / 2, txt = "Unknown"),
           body |> filter(text == "Unquantified") |> transmute(value = NA_real_, cx = x + width / 2, txt = "Unquantified"))
    ))
    picked <- map(centers, \(cx) vals[which.min(abs(vals$cx - cx)), ])
    if (anyDuplicated(map_dbl(picked, \(d) d$cx)))
      warning(basename(pdf_path), ": one value matched two column headers; check the catch distribution table.")
    unq <- names(picked)[map_chr(picked, \(d) d$txt) == "Unquantified"]
    list(year = yr, catch = picked$catch$value, landings = picked$landings$value,
         discards = picked$discards$value,
         note = if (length(unq)) paste0(unq, ": Unquantified", collapse = "; ") else NA_character_)
  }, NULL)
  
  catch_t <- dist$catch    %||% NA_real_
  land_t  <- dist$landings %||% NA_real_
  disc_t  <- dist$discards %||% NA_real_
  if (!anyNA(c(catch_t, land_t, disc_t)) && land_t + disc_t != catch_t)
    warning(basename(pdf_path), ": landings + discards != catch; check the catch distribution table.")
  discard_rate <- if (!is.na(disc_t) && !is.na(catch_t) && catch_t > 0) disc_t / catch_t else NA_real_
  
  # ---------- 6. Advice change vs previous advice, with reason ----------
  adv_pct <- tibble(year = integer(), pct = numeric())      # % change figures found in the scenario tables
  if (nrow(scen) && "scenario_group" %in% names(scen)) {
    g <- scen |> filter(str_detect(coalesce(scenario_group, ""), regex("advice basis", ignore_case = TRUE)))
    if (nrow(g) && any(str_detect(names(g), "^advice_change_pct"))) {
      if (!"year" %in% names(g)) g$year <- NA_integer_
      adv_pct <- g |> select(year, matches("^advice_change_pct")) |>
        pivot_longer(matches("^advice_change_pct"), names_to = "nm", values_to = "pct") |>
        mutate(year = coalesce(as.integer(str_extract(nm, "\\d{4}")), as.integer(year))) |>
        select(year, pct) |> filter(!is.na(pct)) |> distinct()
    }
  }
  if (nrow(scen) && "item" %in% names(scen)) {
    kvp <- scen |> filter(str_detect(coalesce(item, ""), regex("advice change", ignore_case = TRUE)), !is.na(value_num))
    if (nrow(kvp)) adv_pct <- bind_rows(adv_pct, tibble(year = NA_integer_, pct = kvp$value_num))
  }
  
  advice_change_note <- safe({
    cue   <- "(?:due to|because of|owing to|as a result of|driven by|reflect(?:s|ing)?)"
    sents <- pages_txt |> paste(collapse = " ") |> str_squish() |> str_split_1("(?<=\\.)\\s+(?=[A-Z])")
    ok <- str_detect(sents, regex("\\badvice\\b", ignore_case = TRUE)) &
      str_detect(sents, regex("chang|increas|decreas|higher|lower|reduc|declin|rise", ignore_case = TRUE)) &
      str_detect(sents, regex(cue, ignore_case = TRUE)) &
      !str_detect(sents, "Table") & nchar(sents) < 500
    s <- sents[ok][1]
    if (is.na(s)) return(NA_character_)
    
    reason <- str_match(s, regex(paste0(cue, "\\s+(.*?)\\.?$"), ignore_case = TRUE))[, 2]
    before <- str_match(s, regex(paste0("^(.*?)", cue), ignore_case = TRUE))[, 2]
    if (is.na(reason)) return(NA_character_)
    
    pcts <- str_extract_all(before, "[+-]?\\d+(?:\\.\\d+)?\\s*%")[[1]] |> str_remove_all("\\s")
    if (length(pcts)) {                                       # percentage stated in the sentence
      dir  <- case_when(str_detect(before, regex("decreas|lower|reduc|declin|downward", TRUE)) ~ "-",
                        str_detect(before, regex("increas|higher|rise|upward", TRUE)) ~ "+", TRUE ~ "")
      pcts <- if_else(str_detect(pcts, "^[+-]"), pcts, paste0(dir, pcts))
      if (all(as.numeric(str_remove_all(pcts, "[%+]")) == 0)) return(NA_character_)
      pct_label <- paste(pcts, collapse = " / ")
    } else {                                                  # otherwise take it from the table(s), for the years named
      yrs_s <- as.integer(str_extract_all(before, "\\b20\\d{2}\\b")[[1]])
      tb <- adv_pct
      if (length(yrs_s) && any(tb$year %in% yrs_s)) tb <- filter(tb, year %in% yrs_s)
      if (nrow(tb) && all(tb$pct == 0)) return(NA_character_)
      pct_label <- if (nrow(tb)) paste0(if_else(tb$pct > 0, "+", ""), tb$pct, "%",
                                        if_else(is.na(tb$year), "", paste0(" (", tb$year, ")")), collapse = " / ")
      else "(% not stated)"
    }
    paste0("Advice changed ", pct_label, " relative to previous advice, due to ", str_squish(reason), ".")
  }, NA_character_)
  
  tibble(
    source_file        = basename(pdf_path),
    publication_date   = pub_date,
    stock_code         = stock_code,
    advice_url         = advice_url,
    advice_type        = advice_tbl$advice_type[1] %||% NA_character_,
    catch_year         = dist$year %||% NA_integer_,
    total_catch_t      = catch_t,
    landings_t         = land_t,
    discards_t         = disc_t,
    discard_rate       = discard_rate,
    catch_dist_note    = dist$note %||% NA_character_,
    advice_change_note = advice_change_note,
    catch_advice       = list(advice_tbl),
    catch_scenarios    = list(scen)
  )
}

# ---- Run on a folder ----
files <- list.files("~/Documents/Work/ICES-ACOM/AdviceSheets/Elasmobranchs", pattern = "\\.pdf$", full.names = TRUE)
all_results <- map(files, possibly(extract_ices_advice, otherwise = NULL)) |> list_rbind()

all_results |> select(-catch_advice, -catch_scenarios)                     # one row per file
all_results |> select(source_file, stock_code, catch_advice) |> unnest(catch_advice)
all_results |> select(source_file, stock_code, catch_scenarios) |> unnest(catch_scenarios)