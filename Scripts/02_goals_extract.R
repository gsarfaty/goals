# AUTHOR:   G. Sarfaty
# PURPOSE:  SP Goal Metrics
# LICENSE:  MIT
# DATE:     2026-01-13
# UPDATED: 


# LOCALS & SETUP ===============================================================

#Libraries

library(tidyverse)
library(janitor)
library(readxl)
library(gagglr)
library(rJava)
library(tabulapdf)
library(pdftools)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)


# LOAD DATA ==================================================================== 

# GLOBAL VARIABLES --------------------------------------------------------


data_in<-"Data/"


# IMPORT GOAL REPORT------------------------------------------------------------

pdf_file <- data_in%>% 
  return_latest("Goal 2") 


app_a_pages <- 97:108  # change this to the pages where Appendix lives
page_text <- pdftools::pdf_text(pdf_file)


tables_list <- extract_tables(
  pdf_file,
  pages = app_a_pages,
  output = "tibble",   # gives tibbles instead of raw matrices
  guess = TRUE
)

length(tables_list)


# ---- helpers ----
extract_groups_from_header <- function(tbl) {
  # Right-side group headers typically live in row 2, cols 2:n
  right_groups <- tbl %>%
    slice(2) %>%
    select(-1) %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    str_squish() %>%
    discard(~ is.na(.x) || .x == "")
  
  # Left header is spread across rows 1 and 3 in col 1 (e.g., "All ML" + "Students (ELP 9)")
  left_top <- tbl %>% slice(1) %>% pull(1) %>% as.character() %>% str_squish()
  left_bot <- tbl %>% slice(3) %>% pull(1) %>% as.character() %>% str_squish()
  left_first <- str_squish(paste(left_top, left_bot))
  
  # Row 2 col 1 often contains multiple group labels merged
  merged <- tbl %>% slice(2) %>% pull(1) %>% as.character() %>% str_squish()
  
  # Use known group tokens (keeps multi-word groups intact)
  known <- c("All Students","Econ Dis","SWD","504 Plan","Asian","Black","Hispanic","Multiracial","White",
             "Current MLs","Former MLs","All ML","All MLs","ML","MLs")
  
  # Pull groups in the order they appear
  pos <- map_int(known, ~ str_locate(merged, fixed(.x))[,1])
  merged_groups <- known[!is.na(pos)]
  merged_groups <- merged_groups[order(pos[!is.na(pos)])]
  
  # Compose left groups (first special group + merged list)
  left_groups <- c(left_first, merged_groups)
  
  list(left_groups = left_groups, right_groups = right_groups)
}

split_rowtype_and_vals <- function(x) {
  x <- str_squish(as.character(x))
  if (is.na(x) || x == "") return(NULL)
  
  # Match row type at the start (handles 1-2 word types + SY yyyy-yy)
  m <- str_match(
    x,
    "^(Baseline|Numerator|Denominator|Interim Target|Performance Target|Target|SY\\s*\\d{4}-\\d{2})\\s+(.*)$"
  )
  if (is.na(m[1,1])) return(NULL)
  
  row_type <- m[1,2]
  rest <- m[1,3]
  
  # Split remaining values on whitespace
  vals <- str_split(rest, "\\s+", simplify = TRUE) %>% as.character()
  vals <- vals[vals != ""]
  
  list(row_type = row_type, vals = vals)
}

tidy_one_appendix_table <- function(tbl, table_id = NA_integer_) {
  metric <- names(tbl)[1]
  
  # header-derived groups
  g <- extract_groups_from_header(tbl)
  left_groups  <- g$left_groups
  right_groups <- g$right_groups
  n_right <- length(right_groups)
  
  # data rows usually start around row 4
  data_rows <- tbl %>% slice(4:n())
  
  map_dfr(seq_len(nrow(data_rows)), function(i) {
    r <- data_rows[i, , drop = FALSE]
    
    left_cell <- r[[1]] %>% as.character()
    
    parsed <- split_rowtype_and_vals(left_cell)
    if (is.null(parsed)) return(NULL)
    
    row_type <- parsed$row_type
    left_vals <- parsed$vals
    
    # right-side values from cols 2:n
    right_vals <- r %>%
      select(-1) %>%
      unlist(use.names = FALSE) %>%
      as.character() %>%
      str_squish()
    
    # how many left groups should we have?
    # if left_vals count is one more than left_groups, assume missing "All Students"
    if (length(left_vals) == length(left_groups) + 1) {
      left_groups2 <- c("All Students", left_groups)
    } else {
      left_groups2 <- left_groups
    }
    
    # align lengths (pad/truncate)
    if (length(left_vals) < length(left_groups2)) left_vals <- c(left_vals, rep(NA_character_, length(left_groups2) - length(left_vals)))
    if (length(left_vals) > length(left_groups2)) left_vals <- left_vals[1:length(left_groups2)]
    
    # build long output
    out_left <- tibble(
      metric = metric,
      row_type = row_type,
      student_group = left_groups2,
      value = left_vals,
      table_id = table_id
    )
    
    out_right <- tibble(
      metric = metric,
      row_type = row_type,
      student_group = right_groups,
      value = right_vals[1:length(right_groups)],
      table_id = table_id
    )
    
    bind_rows(out_left, out_right)
  })
}

# tidy 1 table test ------------------------------------------------------------
tidy1 <- tidy_one_appendix_table(tables_list[[1]], table_id = 1)

# clean up

tidy1_fixed <- tidy1 %>%
  mutate(
    # 1) identify "period" rows (these are currently living in row_type)
    period = case_when(
      row_type == "Baseline" ~ "Baseline",
      str_detect(row_type, "^SY\\s*\\d{4}-\\d{2}$") ~ row_type,
      row_type == "Interim Target" ~ "Interim Target",
      TRUE ~ NA_character_
    ),
    
    # 2) convert the current row_type into a real row_type classification
    row_type2 = case_when(
      row_type %in% c("Numerator", "Denominator") ~ str_to_lower(row_type),
      row_type == "Interim Target" ~ "target_percent",
      row_type == "Performance Target" ~ "target_status",
      row_type == "Baseline" | str_detect(row_type, "^SY\\s*\\d{4}-\\d{2}$") ~ "percent",
      TRUE ~ "other"
    ),
    
    # keep the original label if you ever need it for debugging
    row_label_raw = row_type
  ) %>%
  group_by(metric, table_id) %>%
  tidyr::fill(period, .direction = "down") %>%
  ungroup() %>%
  # Performance Target is not a time period; optionally blank it out
  mutate(period = if_else(row_type2 == "target_status", NA_character_, period)) 


# tidy all ---------------------------------------------------------------------

tidy_one_safe <- function(x, table_id) {
  # Skip NULLs / character vectors / anything not tabular
  if (is.null(x)) return(tibble())
  if (!inherits(x, c("data.frame", "tbl_df"))) return(tibble())
  
  # Ensure tibble
  x <- as_tibble(x, .name_repair = "unique")
  
  # Skip “empty” tables
  if (nrow(x) == 0 || ncol(x) < 2) return(tibble())
  
  tidy_one_appendix_table(x, table_id = table_id)
}

tidy_all <- imap_dfr(tables_list, ~ tidy_one_safe(.x, .y))

# Tidy all clean up ------------------------------------------------------------

tidy_all_fixed <- tidy_all %>%
  mutate(
    period = case_when(
      row_type == "Baseline" ~ "Baseline",
      str_detect(row_type, "^SY\\s*\\d{4}-\\d{2}$") ~ row_type,
      row_type == "Interim Target" ~ "Interim Target",
      TRUE ~ NA_character_
    ),
    row_type2 = case_when(
      row_type %in% c("Numerator", "Denominator") ~ str_to_lower(row_type),
      row_type == "Interim Target" ~ "target_percent",
      row_type == "Performance Target" ~ "target_status",
      row_type == "Baseline" | str_detect(row_type, "^SY\\s*\\d{4}-\\d{2}$") ~ "percent",
      TRUE ~ "other"
    ),
    row_label_raw = row_type
  ) %>%
  group_by(metric, table_id) %>%
  fill(period, .direction = "down") %>%
  ungroup() %>%
  mutate(period = if_else(row_type2 == "target_status", NA_character_, period)) 

tidy_all_fixed <- tidy_all_fixed %>%
  mutate(
    value_num = readr::parse_number(value))


# EXPORT -----------------------------------------------------------------------
write_csv(tidy_all_fixed, file = "Data_public/FCPS_Goal3_Metrics.csv")
