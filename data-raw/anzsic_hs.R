## code to prepare `anzsic_hs` dataset goes here
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(strayr)
library(janitor)
library(stringr)

anzsic_hs <- read_excel("data-raw/merchandise_trade_appendices_2018.xlsx",
                  sheet = "Appendix 6.1",
                  range = "A23:K11420") %>%
  clean_names() %>%
  filter(is.na(end_date)) %>%
  mutate(hs_4 = str_sub(ahecc, 1, 4)) %>%
  mutate(anzsic_2006 = str_replace(anzsic_2006, "^0+", "")) %>%
  left_join(anzsic, by = c("anzsic_2006" = "anzsic_class_code")) %>%
  distinct(hs_4, anzsic_subdivision)

usethis::use_data(anzsic_hs, overwrite = TRUE)
