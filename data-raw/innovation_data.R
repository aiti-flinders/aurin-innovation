## code to prepare `sa3_innovation` dataset goes here
library(readr)
library(dplyr)

sa3_innovation <- read_csv("data-raw/sa3-region-innovation-data-2009-to-2016.csv") %>%
  select(sa3_code = `SA3 Code`,
         sa3_name = SA3_Name,
         year = Year,
         berd = `Business Research and Development Expenditure`,
         unis = `Count of University Campuses (2015)`,
         tafes = `Count of TAFEs (2015)`,
         ris = `Count of Research Institutes (2015)`)

usethis::use_data(sa3_innovation, overwrite = TRUE)
