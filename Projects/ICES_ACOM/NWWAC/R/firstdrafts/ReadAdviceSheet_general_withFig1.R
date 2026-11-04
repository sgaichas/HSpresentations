# Draft by Claude Sonnet 5.5

# install.packages(c("pdftools", "png", "dplyr", "stringr", "purrr", "tibble",
#                    "tidyr", "tidyselect", "lubridate"))

extract_ices_advice <- function(pdf_path,
                                out_dir         = "figures",   # where Figure 1 PNGs are saved
                                dpi             = 200,
                                header_frac     = 0.15,
                                footer_frac     = 0.15,
                                caption_pattern = "catch scenarios?") {
  
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  safe   <- function(expr, default = NA) tryCatch(expr, error = \(e) default)
  norm   <- function(x) {
    x |> stringr::str_replace_all("\\p{Zs}", " ") |>
      stringr::str_replace_all("\u2212", "-") |>
      stringr::str_replace_all("(?<=[\\s(])\u2013(?=\\d)", "-")
  }
  
  pages_txt <- norm(pdftools::pdf_text(pdf_path))
  pages_xy  <- pdftools::pdf_data(pdf_path)
  pg        <- pdftools::pdf_pagesize(pdf_path)
  page_w    <- pg$width[1]
  page_h    <- pg$height[1]
  tab_lines <- purrr::map(pages_txt, \(p) stringr::str_squish(stringr::str_split_1(p, "\n")))
  
  region_text <- function(page, y_from, y_to) {
    pages_xy[[page]] |>
      dplyr::filter(y >= y_from, y <= y_to) |>
      dplyr::arrange(round(y / 4), x) |>
      dplyr::pull(text) |> paste(collapse = " ") |> norm()
  }
  
  # Finds tables whose caption names `pattern` within its first `n_sent` sentences
  find_captions <- function(pattern, n_sent = 2) {
    purrr::imap(tab_lines, \(ln, p) {
      purrr::map(which(stringr::str_detect(ln, "^Table\\s+\\d+[a-z]?\\s")), \(i) {
        cap  <- stringr::str_squish(paste(ln[i:min(i + 2, length(ln))], collapse = " "))
        sent <- stringr::str_split_1(cap, "(?<=\\.)\\s+(?=[A-Z])")
        head <- paste(sent[seq_len(min(n_sent, length(sent)))], collapse = " ")
        if (stringr::str_detect(head, stringr::regex(pattern, ignore_case = TRUE)))
          tibble::tibble(page = p, line = i,
                         table_no = stringr::str_match(ln[i], "^Table\\s+(\\d+[a-z]?)")[, 2],
                         caption = head)
      }) |> purrr::list_rbind()
    }) |> purrr::list_rbind()
  }
  
  # ---------- 1. Header: publication date ----------
  pub_pat  <- "Published\\s+(\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4})"
  pub_date <- safe({
    d <- stringr::str_match(region_text(1, 0, header_frac * page_h), pub_pat)[, 2]
    if (is.na(d)) d <- stringr::str_match(pages_txt[1], pub_pat)[, 2]
    lubridate::dmy(d)
  }, as.Date(NA))
  
  # ---------- 2. Footer: stock code + link ----------
  foot_pat <- "ICES Advice\\s+\\d{4}\\s+[\u2013-]\\s+(\\S+)\\s+[\u2013-]\\s+(https?://\\S+)"
  foot <- safe({
    m <- stringr::str_match(region_text(1, (1 - footer_frac) * page_h, page_h), foot_pat)
    if (is.na(m[1, 1])) m <- stringr::str_match(pages_txt[1], foot_pat)
    m
  }, matrix(NA_character_, 1, 3))
  stock_code <- foot[1, 2]
  advice_url <- foot[1, 3]
  file_stub  <- if (is.na(stock_code)) tools::file_path_sans_ext(basename(pdf_path)) else stock_code
  
  # ---------- 3. Catch / landings advice ----------
  advice_tbl <- safe({
    sec   <- stringr::str_match(pages_txt[1],
                                "(?s)ICES advice on fishing opportunities\\s*\\n(.*?)Non-fisheries conservation")[, 2]
    sents <- stringr::str_split_1(stringr::str_squish(sec), "(?<=\\.)\\s+(?=[A-Z])")
    s     <- sents[stringr::str_detect(sents, "tonnes|zero|cannot provide")][1]
    yrs   <- as.integer(stringr::str_extract_all(s, "\\b20\\d{2}\\b")[[1]])
    nums  <- stringr::str_extract_all(stringr::str_remove_all(s, "\\b20\\d{2}\\b"),
                                      "\\d{1,3}(?:\\s\\d{3})+|\\d+")[[1]]
    vals  <- as.numeric(stringr::str_remove_all(nums, "\\s"))
    if (length(vals) == 1) vals <- rep(vals, length(yrs))
    typ <- stringr::str_extract(stringr::str_to_lower(s), "catches|catch|landings")
    tibble::tibble(year = yrs, advice_t = vals[seq_along(yrs)],
                   advice_type = dplyr::if_else(typ == "landings", "landings", "catch"))
  }, tibble::tibble(year = integer(), advice_t = numeric(), advice_type = character()))
  
  # ---------- 4. "Catch scenarios" table(s) ----------
  parse_scenario_table <- function(hit) {
    ln    <- tab_lines[[hit$page]]
    after <- ln[-seq_len(hit$line)]
    end   <- which(stringr::str_detect(after, "^\\*"))[1]
    body  <- if (is.na(end)) after else after[seq_len(end - 1)]
    body  <- body[nzchar(body)]
    grp_pat <- stringr::regex("^(ICES advice basis|Other scenarios)$", ignore_case = TRUE)
    
    if (!any(stringr::str_detect(body, grp_pat))) {            # (b) basis table
      unit <- "(?:tonnes?|kg(?: km-\\d)?|km|cm|mm|g|m|years?)"
      return(purrr::map(body, \(l) {
        m <- stringr::str_match(l, paste0("^(.*?)\\s+(-?\\d+(?:\\.\\d+)?%?)(?:\\s+(", unit, "))?$"))
        if (!is.na(m[1, 1]))
          return(tibble::tibble(table_no = hit$table_no, table_type = "basis_table",
                                item = stringr::str_squish(m[1, 2]),
                                value_num = as.numeric(stringr::str_remove(m[1, 3], "%")),
                                value_text = NA_character_, unit = m[1, 4]))
        m2 <- stringr::str_match(l, "^(.*?)\\s+(Not applied|Unquantified|Not quantified)$")
        if (!is.na(m2[1, 1]))
          tibble::tibble(table_no = hit$table_no, table_type = "basis_table",
                         item = stringr::str_squish(m2[1, 2]), value_num = NA_real_,
                         value_text = m2[1, 3], unit = NA_character_)
      }) |> purrr::list_rbind())
    }
    
    # (a) scenario grid
    tail_pat <- "^(.*?)(?:^|\\s)([-<\\d. ]*\\d[-<\\d. ]*)$"
    tok_pat  <- "<\\s*[\\d.]+|-?(?:\\d{1,3}(?: \\d{3})+|\\d+)(?:\\.\\d+)?"
    is_year_line <- \(prefix, toks) length(toks) > 0 && prefix == "" &&
      all(stringr::str_detect(toks, "^20\\d{2}$"))
    
    yr_lines <- body[stringr::str_detect(body, "^(?:20\\d{2}\\s*)+$")]
    yrs <- integer()
    if (length(yr_lines)) {
      best <- yr_lines[which.max(stringr::str_count(yr_lines, "20\\d{2}"))]
      yrs  <- as.integer(stringr::str_extract_all(best, "20\\d{2}")[[1]])
      if (length(yrs) < 2) yrs <- integer()
    }
    
    group <- NA_character_; pending <- character(); rows <- list()
    for (l in body) {
      if (stringr::str_detect(l, grp_pat)) { group <- l; pending <- character(); next }
      m      <- stringr::str_match(l, tail_pat)
      toks   <- if (is.na(m[1, 1])) character() else stringr::str_extract_all(m[1, 3], tok_pat)[[1]]
      prefix <- if (is.na(m[1, 1])) "" else stringr::str_squish(m[1, 2])
      if (is_year_line(prefix, toks)) next
      if (length(toks) >= 3) {
        if (stringr::str_detect(prefix, "=$")) { prefix <- paste(prefix, toks[1]); toks <- toks[-1] }
        rows[[length(rows) + 1]] <- list(group = group,
                                         label = stringr::str_squish(paste(c(pending, prefix), collapse = " ")), toks = toks)
        pending <- character()
      } else pending <- c(pending, l)
    }
    if (!length(rows)) return(NULL)
    
    strip_prob <- \(t) if (length(t) && stringr::str_detect(t[length(t)], "^<")) t[-length(t)] else t
    k  <- min(purrr::map_int(rows, \(r) length(strip_prob(r$toks))))
    nm <- if (k == 10) {
      sfx <- if (length(yrs) >= 10) yrs[1:10] else rep(c("1", "2"), 5)
      paste0(rep(c("catch", "harvest_rate", "btot", "btot_change_pct", "advice_change_pct"), each = 2), "_", sfx)
    } else if (k == 5) {
      c("landings_or_catch_t", "f_ratio", "b_ratio_next_year", "pct_biomass_change", "advice_change_pct")
    } else paste0("v", seq_len(k))
    prob_nm <- if (length(yrs) == k + 1) paste0("prob_below_blim_", yrs[k + 1]) else "prob_below_blim"
    yr_cap  <- as.integer(stringr::str_match(hit$caption, "\\bfor (20\\d{2})")[, 2])
    
    purrr::map(rows, \(r) {
      t <- r$toks; prob <- NA_character_
      if (length(t) && stringr::str_detect(t[length(t)], "^<")) {
        prob <- stringr::str_remove_all(t[length(t)], "\\s"); t <- t[-length(t)]
      } else if (length(t) > k) prob <- t[k + 1]
      v <- as.numeric(stringr::str_remove_all(t[seq_len(k)], " "))
      tibble::as_tibble(c(list(table_no = hit$table_no, table_type = "scenario_grid", year = yr_cap,
                               scenario_group = r$group, basis = r$label),
                          purrr::set_names(as.list(v), nm),
                          purrr::set_names(list(prob), prob_nm)))
    }) |> purrr::list_rbind()
  }
  
  cap_hits <- find_captions(caption_pattern)
  scen <- if (nrow(cap_hits)) {
    purrr::map(seq_len(nrow(cap_hits)), \(r) safe(parse_scenario_table(cap_hits[r, ]), NULL)) |>
      purrr::list_rbind()
  } else tibble::tibble()
  
  # ---------- 5. Catch distribution by fleet: catch, landings, discards ----------
  num_left_of <- function(reg, tn_row) {
    cand <- reg |> dplyr::filter(abs(y - tn_row$y) <= 3, x < tn_row$x) |> dplyr::arrange(dplyr::desc(x))
    keep <- character(); edge <- tn_row$x
    for (i in seq_len(nrow(cand))) {
      if (!stringr::str_detect(cand$text[i], "^\\d+(?:\\.\\d+)?$") || edge - (cand$x[i] + cand$width[i]) > 8) break
      keep <- c(cand$text[i], keep); edge <- cand$x[i]
    }
    if (!length(keep)) return(NULL)
    tibble::tibble(value = as.numeric(paste(keep, collapse = "")),
                   cx = (edge + tn_row$x + tn_row$width) / 2, txt = "number")
  }
  
  dist <- safe({
    p   <- which(stringr::str_detect(stringr::str_squish(pages_txt), "Catch distribution by fleet in \\d{4}"))[1]
    w   <- pages_xy[[p]]
    yr  <- as.integer(stringr::str_match(stringr::str_squish(pages_txt[p]),
                                         "Catch distribution by fleet in (\\d{4})")[, 2])
    i   <- which(w$text == "distribution" & dplyr::lead(w$text) == "by" & dplyr::lead(w$text, 2) == "fleet")[1]
    reg <- w |> dplyr::filter(y > w$y[i] + 4, y < w$y[i] + 0.2 * page_h)
    hl  <- reg |> dplyr::filter(text == "Landings") |> dplyr::slice(1)
    hd  <- reg |> dplyr::filter(text == "Discards")  |> dplyr::slice(1)
    hw  <- reg |> dplyr::filter(abs(y - hl$y) <= 3, x < hl$x)
    centers <- c(catch    = (min(hw$x) + max(hw$x + hw$width)) / 2,
                 landings = hl$x + hl$width / 2,
                 discards = hd$x + hd$width / 2)
    body <- reg |> dplyr::filter(y > hl$y + 3)
    tn   <- body |> dplyr::filter(text == "tonnes")
    vals <- purrr::list_rbind(c(
      purrr::map(seq_len(nrow(tn)), \(j) num_left_of(body, tn[j, ])),
      list(body |> filter(text == "Unknown") |> transmute(value = NA_real_, cx = x + width / 2, txt = "Unknown"),
           body |> dplyr::filter(text == "Unquantified") |>
             dplyr::transmute(value = NA_real_, cx = x + width / 2, txt = "Unquantified"))))
    picked <- purrr::map(centers, \(cx) vals[which.min(abs(vals$cx - cx)), ])
    if (anyDuplicated(purrr::map_dbl(picked, \(d) d$cx)))
      warning(basename(pdf_path), ": one value matched two column headers; check the catch distribution table.")
    unq <- names(picked)[purrr::map_chr(picked, \(d) d$txt) == "Unquantified"]
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
  adv_pct <- tibble::tibble(year = integer(), pct = numeric())
  if (nrow(scen) && "scenario_group" %in% names(scen)) {
    g <- scen |> dplyr::filter(stringr::str_detect(dplyr::coalesce(scenario_group, ""),
                                                   stringr::regex("advice basis", ignore_case = TRUE)))
    if (nrow(g) && any(stringr::str_detect(names(g), "^advice_change_pct"))) {
      if (!"year" %in% names(g)) g$year <- NA_integer_
      adv_pct <- g |> dplyr::select(year, tidyselect::matches("^advice_change_pct")) |>
        tidyr::pivot_longer(tidyselect::matches("^advice_change_pct"), names_to = "nm", values_to = "pct") |>
        dplyr::mutate(year = dplyr::coalesce(as.integer(stringr::str_extract(nm, "\\d{4}")), as.integer(year))) |>
        dplyr::select(year, pct) |> dplyr::filter(!is.na(pct)) |> dplyr::distinct()
    }
  }
  if (nrow(scen) && "item" %in% names(scen)) {
    kvp <- scen |> dplyr::filter(stringr::str_detect(dplyr::coalesce(item, ""),
                                                     stringr::regex("advice change", ignore_case = TRUE)), !is.na(value_num))
    if (nrow(kvp)) adv_pct <- dplyr::bind_rows(adv_pct, tibble::tibble(year = NA_integer_, pct = kvp$value_num))
  }
  
  advice_change_note <- safe({
    cue   <- "(?:due to|because of|owing to|as a result of|driven by|reflect(?:s|ing)?)"
    sents <- pages_txt |> paste(collapse = " ") |> stringr::str_squish() |>
      stringr::str_split_1("(?<=\\.)\\s+(?=[A-Z])")
    ok <- stringr::str_detect(sents, stringr::regex("\\badvice\\b", ignore_case = TRUE)) &
      stringr::str_detect(sents, stringr::regex("chang|increas|decreas|higher|lower|reduc|declin|rise", ignore_case = TRUE)) &
      stringr::str_detect(sents, stringr::regex(cue, ignore_case = TRUE)) &
      !stringr::str_detect(sents, "Table") & nchar(sents) < 500
    s <- sents[ok][1]
    if (is.na(s)) return(NA_character_)
    reason <- stringr::str_match(s, stringr::regex(paste0(cue, "\\s+(.*?)\\.?$"), ignore_case = TRUE))[, 2]
    before <- stringr::str_match(s, stringr::regex(paste0("^(.*?)", cue), ignore_case = TRUE))[, 2]
    if (is.na(reason)) return(NA_character_)
    pcts <- stringr::str_extract_all(before, "[+-]?\\d+(?:\\.\\d+)?\\s*%")[[1]] |> stringr::str_remove_all("\\s")
    if (length(pcts)) {
      dir <- dplyr::case_when(
        stringr::str_detect(before, stringr::regex("decreas|lower|reduc|declin|downward", TRUE)) ~ "-",
        stringr::str_detect(before, stringr::regex("increas|higher|rise|upward", TRUE)) ~ "+",
        TRUE ~ "")
      pcts <- dplyr::if_else(stringr::str_detect(pcts, "^[+-]"), pcts, paste0(dir, pcts))
      if (all(as.numeric(stringr::str_remove_all(pcts, "[%+]")) == 0)) return(NA_character_)
      pct_label <- paste(pcts, collapse = " / ")
    } else {
      yrs_s <- as.integer(stringr::str_extract_all(before, "\\b20\\d{2}\\b")[[1]])
      tb <- adv_pct
      if (length(yrs_s) && any(tb$year %in% yrs_s)) tb <- dplyr::filter(tb, year %in% yrs_s)
      if (nrow(tb) && all(tb$pct == 0)) return(NA_character_)
      pct_label <- if (nrow(tb)) paste0(dplyr::if_else(tb$pct > 0, "+", ""), tb$pct, "%",
                                        dplyr::if_else(is.na(tb$year), "", paste0(" (", tb$year, ")")),
                                        collapse = " / ") else "(% not stated)"
    }
    paste0("Advice changed ", pct_label, " relative to previous advice, due to ",
           stringr::str_squish(reason), ".")
  }, NA_character_)
  
  # ---------- 7. Stock development over time: fishing pressure / stock size sentences ----------
  stock_dev <- safe({
    sec <- stringr::str_match(pages_txt[1],
                              "(?s)Stock development over time\\s*\\n(.*?)(?=\\n\\s*Figure\\s+1\\b)")[, 2]
    sents   <- stringr::str_split_1(stringr::str_squish(sec), "(?<=\\.)\\s+(?=[A-Z])")
    # one sentence can hold both statements: "..., and <second clause with a verb>"
    clauses <- unlist(purrr::map(sents, \(s)
                                 stringr::str_split_1(s, ",\\s+and\\s+(?=[^,]*\\b(?:is|are)\\b)")))
    clauses <- stringr::str_squish(clauses)
    clauses <- dplyr::if_else(stringr::str_detect(clauses, "\\.$"), clauses, paste0(clauses, "."))
    clauses <- paste0(toupper(substring(clauses, 1, 1)), substring(clauses, 2))
    is_fp <- stringr::str_detect(clauses, stringr::regex("fishing pressure|fishing mortality|harvest rate", TRUE))
    is_ss <- stringr::str_detect(clauses, stringr::regex("stock size|biomass|\\bSSB\\b", TRUE)) & !is_fp
    list(fishing_pressure = if (any(is_fp)) paste(clauses[is_fp], collapse = " ") else NA_character_,
         stock_size       = if (any(is_ss)) paste(clauses[is_ss], collapse = " ") else NA_character_)
  }, list(fishing_pressure = NA_character_, stock_size = NA_character_))
  
  # ---------- 8. Figure 1 (all panels) -> one PNG ----------
  figure_png <- safe({
    w   <- pages_xy[[1]]
    h_i <- which(w$text == "Stock" & dplyr::lead(w$text) == "development" & dplyr::lead(w$text, 2) == "over")[1]
    f_i <- which(w$text == "Figure" & dplyr::lead(w$text) == "1" & w$x < 0.2 * page_w)[1]
    between <- w[w$y > w$y[h_i] + 3 & w$y < w$y[f_i] - 3, ]       # status sentence(s) only; panels are images
    y_top <- max(between$y + between$height) + 4
    y_bot <- w$y[f_i] - 3                                          # stop just above the "Figure 1" caption
    sc <- dpi / 72
    bm <- pdftools::pdf_render_page(pdf_path, page = 1, dpi = dpi, numeric = TRUE)
    r  <- seq(max(1, floor(y_top * sc)), min(dim(bm)[1], ceiling(y_bot * sc)))
    cc <- seq(max(1, floor(0.04 * page_w * sc)), min(dim(bm)[2], ceiling(0.96 * page_w * sc)))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out <- file.path(out_dir, paste0(file_stub, "_figure1.png"))
    png::writePNG(bm[r, cc, , drop = FALSE], out)
    out
  }, NA_character_)
  
  # ---------- 9. "Values in the forecast and the interim year" table ----------
  parse_forecast <- function(hit) {
    ln    <- tab_lines[[hit$page]]
    after <- ln[-seq_len(hit$line)]
    stop_i <- which(stringr::str_detect(after, "^(Table\\s+\\d|\\*|Basis of)"))[1]
    body  <- if (is.na(stop_i)) after else after[seq_len(stop_i - 1)]
    h <- which(stringr::str_detect(body, "^Variable\\s+Value"))[1]
    if (!is.na(h)) body <- body[-seq_len(h)]
    body <- body[nzchar(body)]
    
    val_pat <- "^(.*?)\\s+(\\d{1,3}(?: \\d{3})+|-?\\d+(?:\\.\\d+)?)(?:\\s+(.*))?$"
    rows <- list(); pending <- character()
    for (l in body) {
      m <- stringr::str_match(l, val_pat)
      if (is.na(m[1, 1])) { pending <- c(pending, l); next }
      lab  <- m[1, 2]
      note <- if (is.na(m[1, 4])) "" else m[1, 4]
      if (length(pending)) {
        if (stringr::str_count(lab, "\\)") > stringr::str_count(lab, "\\(")) {
          lab <- paste(paste(pending, collapse = " "), lab)        # label began on the line(s) above
        } else if (length(rows)) {                                 # otherwise: note overflow from previous row
          rows[[length(rows)]]$note <- stringr::str_squish(paste(rows[[length(rows)]]$note,
                                                                 paste(pending, collapse = " ")))
        }
        pending <- character()
      }
      rows[[length(rows) + 1]] <- list(lab = lab, val = m[1, 3], note = note)
    }
    if (!length(rows)) return(NULL)
    df <- purrr::map(rows, \(r) tibble::tibble(
      table_no = hit$table_no,
      variable = stringr::str_squish(r$lab),
      value    = as.numeric(stringr::str_remove_all(r$val, " ")),
      notes    = stringr::str_squish(r$note))) |> purrr::list_rbind()
    df$year <- as.integer(stringr::str_match(df$variable, "(20\\d{2})")[, 2])
    # a note that continues across two rows (starts lowercase) is shared by both
    for (i in which(stringr::str_detect(df$notes, "^[a-z]") & seq_len(nrow(df)) > 1)) {
      df$notes[i]     <- paste(df$notes[i - 1], df$notes[i])
      df$notes[i - 1] <- df$notes[i]
    }
    df[, c("table_no", "variable", "year", "value", "notes")]
  }
  
  fc_hit <- find_captions("forecast and (?:for )?the interim year")
  forecast_tbl <- if (nrow(fc_hit)) safe(parse_forecast(fc_hit[1, ]), tibble::tibble()) else tibble::tibble()
  
  # ---------- 10. Reference points table ----------
  parse_refpts <- function(hit) {
    ln    <- tab_lines[[hit$page]]
    after <- ln[-seq_len(hit$line)]
    h <- which(stringr::str_detect(after, "Technical basis\\s*\\*?\\s*Source"))[1]  # end of the column header
    if (!is.na(h)) after <- after[-seq_len(h)]
    stop_i <- which(stringr::str_detect(after, "^(Table\\s+\\d|\\*\\s|Basis of the|n/a\\s*=)"))[1]
    body <- if (is.na(stop_i)) after else after[seq_len(stop_i - 1)]
    body <- body[nzchar(body)]
    
    anchor <- "^(MSY Btrigger|Itrigger|HR(?:MSY|PA|mgt|lim)|F(?:MSY proxy|MSY|PA|lim|mgt)|Blim|BPA|SSBmgt)\\b(?:\\s*\\([^)]*\\))?"
    fw_pat <- "^(?:Maximum|sustainable|yield \\(MSY\\)|approach|Precautionary|Management|plan|MSY approach|Precautionary approach|Management plan)$"
    a <- which(stringr::str_detect(body, anchor))
    if (!length(a)) return(NULL)
    
    fw_start <- purrr::map_int(a, \(k) { j <- k; while (j > 1 && stringr::str_detect(body[j - 1], fw_pat)) j <- j - 1; j })
    src_pat  <- "((?:\\(ICES,?\\s*\\d{4}\\)|ICES\\s*\\(\\d{4}\\))(?:;?\\s*(?:\\(ICES,?\\s*\\d{4}\\)|ICES\\s*\\(\\d{4}\\)))*)\\s*$"
    framework <- NA_character_
    
    purrr::map(seq_along(a), \(i) {
      if (fw_start[i] < a[i]) framework <<- stringr::str_squish(paste(body[fw_start[i]:(a[i] - 1)], collapse = " "))
      last <- if (i < length(a)) fw_start[i + 1] - 1 else length(body)
      txt  <- stringr::str_squish(paste(body[a[i]:last], collapse = " "))
      nm   <- stringr::str_match(txt, anchor)[, 1]
      rest <- stringr::str_squish(stringr::str_remove(txt, stringr::fixed(nm)))
      
      src  <- stringr::str_match(rest, src_pat)[, 2]
      if (!is.na(src)) rest <- stringr::str_squish(stringr::str_remove(rest, src_pat))
      
      val_num <- NA_real_; val_txt <- NA_character_
      t1 <- stringr::str_match(rest, "^(n/a|Not defined|Not applicable|Not available)\\b")
      n2 <- stringr::str_match(rest, "^(\\d{1,3}(?: \\d{3})+|\\d+(?:\\.\\d+)?)\\b")
      n3 <- stringr::str_match(rest, "=\\s*(\\d+(?:\\.\\d+)?)\\s*\\*?")      # relative values: "B/BMSY = 0.5 *"
      if (!is.na(t1[1, 1])) { val_txt <- t1[1, 2]; rest <- stringr::str_remove(rest, stringr::fixed(t1[1, 1])) }
      else if (!is.na(n2[1, 1])) { val_num <- as.numeric(stringr::str_remove_all(n2[1, 2], " ")); rest <- stringr::str_remove(rest, stringr::fixed(n2[1, 1])) }
      else if (!is.na(n3[1, 1])) { val_num <- as.numeric(n3[1, 2]); rest <- stringr::str_remove(rest, stringr::fixed(n3[1, 1])); rest <- stringr::str_remove(rest, "^.*?(?=[A-Z][a-z])") }
      basis <- stringr::str_squish(stringr::str_replace_all(rest, "(?<=\\d[\u2013-])\\s+(?=\\d)", ""))
      tibble::tibble(table_no = hit$table_no, framework = framework,
                     reference_point = stringr::str_squish(nm),
                     value_num = val_num, value_text = val_txt,
                     technical_basis = dplyr::if_else(nzchar(basis), basis, NA_character_),
                     source = src)
    }) |> purrr::list_rbind()
  }
  
  rp_hit <- find_captions("reference points, values")
  refpts_tbl <- if (nrow(rp_hit)) safe(parse_refpts(rp_hit[1, ]), tibble::tibble()) else tibble::tibble()
  
  # ---------- Result ----------
  tibble::tibble(
    source_file        = basename(pdf_path),
    publication_date   = pub_date,
    stock_code         = stock_code,
    advice_url         = advice_url,
    advice_type        = advice_tbl$advice_type[1] %||% NA_character_,
    fishing_pressure   = stock_dev$fishing_pressure,
    stock_size         = stock_dev$stock_size,
    catch_year         = dist$year %||% NA_integer_,
    total_catch_t      = catch_t,
    landings_t         = land_t,
    discards_t         = disc_t,
    discard_rate       = discard_rate,
    catch_dist_note    = dist$note %||% NA_character_,
    advice_change_note = advice_change_note,
    figure_png         = figure_png,
    catch_advice       = list(advice_tbl),
    catch_scenarios    = list(scen),
    forecast_values    = list(forecast_tbl),
    reference_points   = list(refpts_tbl)
  )
}

# ---- Run on a folder ----
files <- list.files("~/Documents/Work/ICES-ACOM/AdviceSheets/Elasmobranchs2026", pattern = "\\.pdf$", full.names = TRUE)
all_results <- purrr::map(files, purrr::possibly(extract_ices_advice, otherwise = NULL)) |>
  purrr::list_rbind()

all_results |> dplyr::select(-catch_advice, -catch_scenarios, -forecast_values, -reference_points)
all_results |> dplyr::select(stock_code, forecast_values)  |> tidyr::unnest(forecast_values)
all_results |> dplyr::select(stock_code, reference_points) |> tidyr::unnest(reference_points)