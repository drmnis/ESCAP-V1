# ESCAP: Easy Statistics, Chemometrics and Plotting
# Copyright (c) 2026 Dr. Md. Nahidul Islam, Gazipur Agricultural University.
# Data input: one reader for .csv, .txt, .tsv, .dat, .xls and .xlsx files.

ESC_ACCEPT <- c(".csv", ".txt", ".tsv", ".dat", ".xls", ".xlsx",
                "text/csv", "text/plain", "text/tab-separated-values",
                "application/vnd.ms-excel",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

esc_read_data <- function(path, name = path, sheet = 1) {
  ext <- tolower(tools::file_ext(name))
  df <- switch(ext,
    xls = , xlsx = as.data.frame(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"),
                                 stringsAsFactors = FALSE),
    esc_read_text(path))
  orig <- esc_utf8(names(df)); orig[is.na(orig)] <- ""
  names(df) <- esc_clean_names(names(df))
  orig_map <- stats::setNames(ifelse(nzchar(trimws(orig)), trimws(orig), names(df)), names(df))
  for (j in seq_along(df)) {
    x <- df[[j]]
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- esc_utf8(x)
      x <- trimws(x); x[x %in% c("", ".", "NaN", "NA", "N/A")] <- NA
      num <- suppressWarnings(as.numeric(x))
      # convert text columns that are really numbers (e.g. read from Excel as text)
      if (sum(!is.na(x)) > 0 && all(is.na(x) | !is.na(num))) x <- num
      df[[j]] <- x
    } else if (inherits(x, "integer64")) df[[j]] <- as.numeric(x)
  }
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  rownames(df) <- NULL
  attr(df, "orig_names") <- orig_map[names(df)]   # original headers, for axis labels
  df
}

# Valid UTF-8 text without control characters (invalid bytes crash the browser connection)
esc_utf8 <- function(x) {
  x <- as.character(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "")
  x <- enc2utf8(x)
  gsub("[[:cntrl:]]", " ", x)
}

# Safe, readable column names: "Color (L*)??? $@#@#" -> "Color_Lstar", "Temp (\u00b0C)" -> "Temp_degC"
esc_clean_names <- function(nm) {
  nm <- esc_utf8(nm); nm[is.na(nm)] <- ""
  nm <- gsub("\u00b0|\u00ba", "deg", nm); nm <- gsub("\u00b5|\u03bc", "u", nm)
  nm <- gsub("%", "pct", nm, fixed = TRUE); nm <- gsub("*", "star", nm, fixed = TRUE)
  nm <- gsub("\u00b2", "2", nm); nm <- gsub("\u00b3", "3", nm)
  a <- suppressWarnings(iconv(nm, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
  a[is.na(a)] <- ""
  isnum <- grepl("^\\s*-?[0-9]+([.,][0-9]+)?\\s*$", a)            # spectral variables: 908, 1206.5
  a[isnum] <- paste0("X", gsub(",", ".", trimws(a[isnum])))
  a[!isnum] <- gsub("[^A-Za-z0-9]+", "_", a[!isnum])
  a <- gsub("^_+|_+$", "", a)
  a[a == ""] <- paste0("V", which(a == ""))
  a <- ifelse(grepl("^[A-Za-z]", a), a, paste0("X", a))
  make.unique(a, sep = "_")
}

# Delimited text: encoding (UTF-8 or Windows-1252), separator and decimal mark detected;
# standard CSV quoting ("" inside quotes) is honoured.
esc_read_text <- function(path) {
  con <- file(path, "rb"); bytes <- readBin(con, "raw", min(file.size(path), 2e6)); close(con)
  if (length(bytes) >= 3 && all(bytes[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) bytes <- bytes[-(1:3)]
  bytes <- bytes[bytes != as.raw(0)]
  nl <- which(bytes == as.raw(10)); if (length(nl) && length(bytes) >= 2e6) bytes <- bytes[seq_len(max(nl))]  # whole lines only
  head_txt <- rawToChar(bytes)
  enc <- if (validUTF8(head_txt)) "UTF-8-BOM" else "latin1"
  if (enc == "latin1") head_txt <- iconv(head_txt, "latin1", "UTF-8", sub = "")
  first <- strsplit(head_txt, "\r?\n", useBytes = TRUE)[[1]]
  first <- first[nzchar(trimws(first))][1:min(5, sum(nzchar(trimws(first))))]
  unq <- gsub('"[^"]*"', "", first)                       # ignore separators inside quotes
  cnt <- vapply(c(",", ";", "\t", "|"), function(sp) min(lengths(regmatches(unq, gregexpr(sp, unq, fixed = sp != "\t")))), 0)
  sep <- if (max(cnt) > 0) c(",", ";", "\t", "|")[which.max(cnt)] else ""
  dec <- if (sep == ";" && any(grepl("[0-9],[0-9]", unq[-1]))) "," else "."
  if (file.size(path) > 3e7) {
    return(data.table::fread(path, sep = if (sep == "") "auto" else sep, dec = dec, data.table = FALSE, check.names = FALSE,
                             encoding = if (enc == "latin1") "Latin-1" else "UTF-8", showProgress = FALSE))
  }
  tryCatch(
    utils::read.table(path, header = TRUE, sep = sep, quote = "\"", dec = dec, fileEncoding = enc, check.names = FALSE,
                      comment.char = "", stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", ".", "NaN", "-"),
                      strip.white = TRUE, fill = TRUE, blank.lines.skip = TRUE),
    error = function(e) {
      # irregular files (ragged rows, stray quotes): fall back to the tolerant fread parser
      data.table::fread(path, sep = if (sep == "") "auto" else sep, dec = dec, data.table = FALSE, check.names = FALSE,
                        fill = TRUE, encoding = if (enc == "latin1") "Latin-1" else "UTF-8", showProgress = FALSE)
    })
}

esc_excel_sheets <- function(path, name = path) {
  if (tolower(tools::file_ext(name)) %in% c("xls", "xlsx")) readxl::excel_sheets(path) else character(0)
}
