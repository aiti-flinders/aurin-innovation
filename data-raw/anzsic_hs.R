## code to prepare `anzsic_hs` dataset goes here
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(strayr)
library(janitor)
library(stringr)

#I'm not sure how stable this URL is
url <- 'https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&5489.0_2018.xlsx&5489.0&Data%20Cubes&E6F604B67A13BA6CCA2586640012E3DD&0&2018&30.06.2021&Latest'

download.file(url,
              destfile = "data-raw/merchandise_trade_appendices_2018.xlsx",
              mode = 'wb')

anzsic_hs <- read_excel("data-raw/merchandise_trade_appendices_2018.xlsx",
                  sheet = "Appendix 6.1",
                  range = "A23:K11420") %>%
  clean_names() %>%
  filter(is.na(end_date)) %>%
  mutate(hs_product_code = str_sub(ahecc, 1, 4)) %>%
  mutate(anzsic_2006 = str_replace(anzsic_2006, "^0+", "")) %>%
  left_join(anzsic2006, by = c("anzsic_2006" = "anzsic_class_code")) %>%
  distinct(hs_product_code, anzsic_subdivision)

usethis::use_data(anzsic_hs, overwrite = TRUE)
