## code to prepare `regional_innovation` dataset goes here
library(aurininnovation)
library(purrr)
library(sf)
library(scales)
library(dplyr)

regional_innovation <- map_dfr(.x = c(2011, 2016),
                               .f = ~create_regional_innovation(year = .x, geography = "sa2", adjust = FALSE)) %>%
  mutate(across(c(innovation, human_knowledge, patent_output), ~rescale(.x, to = c(0, 100)))) %>%
  aurininnovation:::add_sa2_codes() %>%
  relocate(sa2_code, .after = sa2_name)

usethis::use_data(regional_innovation, overwrite = TRUE)