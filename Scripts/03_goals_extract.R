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
  return_latest("Goal 3") 


app_a_pages <- 51:74  # change this to the pages where Appendix lives
page_text <- pdftools::pdf_text(pdf_file)


tables_list <- extract_tables(
  pdf_file,
  pages = app_a_pages,
  output = "tibble",   # gives tibbles instead of raw matrices
  guess = TRUE
)



#pull specific table and tidy --------------------------------------------------

df_table <- as.data.frame(tables_list[[30]]) #select from list

df_edits <- df_table %>%
  rename(raw = 1) %>% 
  mutate(
  row_type = case_when(
    str_detect(raw, "Students") ~ "Student Group",
    str_detect(raw, "^Baseline") ~ "Baseline",
    str_detect(raw, "^SY") ~ "SY",
    str_detect(raw, "^Target") ~ "Target",
    str_detect(raw, "^Numerator") ~ "Numerator",
    str_detect(raw, "^Denominator") ~ "Denominator",
    TRUE ~ "Performance"
  )) %>% 
  mutate(
    values = case_when(
    # row_type == "Performance" ~ str_trim(str_remove(raw, "^Performance\\s*")),
    row_type == "Performance" ~ raw %>%
        stringr::str_remove("^Performance\\s*") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("Target\\s+Met", "Target_Met"),
    row_type == "SY" ~ str_remove(raw, "^SY\\s\\d{4}-\\d{2} "),
    TRUE ~
      str_trim(str_remove(raw, "^[A-Za-z ]+\\s*"))
  )) %>% 
  mutate(label = case_when(
    row_type == "Numerator"   ~ "Numerator",
    row_type == "Denominator" ~ "Denominator",
    row_type == "Baseline"    ~ "Baseline",
    row_type == "Target"      ~ "Target",
    row_type =="Student Group" ~ "Student Group",
    row_type == "Performance" ~ "Performance",
    row_type == "SY"          ~ str_extract(raw, "^SY\\s\\d{4}-\\d{2}"),  # keep year only
    TRUE ~ row_type)) %>% 
  filter(!row_type=="Student Group") %>%  #update based on specific table %>% 
  select(raw,row_type,label,values,everything())

df_final <- df_edits %>%
  separate(values, into = paste0("grp_", 1:20), sep = "\\s+", fill = "right") %>% 
  # filter(!grp_1=="(1-4)") %>% 
  select(raw,row_type,label,everything()) %>% 
  select(1:13) %>% 
  set_names(c("raw","row_type","label",  #CHECK THESE AS THEY CHANGE
          "All Students",
          "Econ Dis",
          "ML (1-6d)",
          "SWD",
          "504 Plan",
          "Asian",
          "Black",
          "Hispanic",
          "Multiracial",
          "White")) %>% 
  mutate(
    period = case_when(
      row_type == "Baseline" ~ "Baseline",
      row_type == "SY" ~ label,  # label is "SY 2023-24", "SY 2024-25"
      TRUE ~ NA_character_
    )
  ) %>%
  fill(period, .direction = "down") %>% 
  pivot_longer(cols='All Students':'White',
               names_to = "student_group",
               values_to = "value") %>% 
  mutate(metric="Pass Rate on the Grade 3 Reading SOL")



# EXPORT -----------------------------------------------------------------------
write_csv(df_final, file = "Data_public/Goal3_Gr3_reading_SOL.csv")
