## code to prepare `education` dataset goes here
library(dplyr)
library(vroom)

education <- vroom("data-raw/sa2_pow_heap1_2016.csv",
                   skip = 9,
                   n_max = 27745,
                   col_select = c(`SA2 (POW)`, `HEAP - 1 Digit Level`, Count))

education <- education %>%
  rename("sa2_name" = `SA2 (POW)`,
         "education_level" = `HEAP - 1 Digit Level`,
         "value" = Count) %>%
  check_sa2(geography = "sa2_name",
            other = "education_level") %>%
  group_by(sa2_name) %>%
  mutate(share = value/sum(value)) %>%
  ungroup() %>%
  pivot_wider(id_cols = -share,
              names_from = education_level,
              values_from =  value) %>%
  mutate(university = `Bachelor Degree Level` + `Postgraduate Degree Level`,
         tafe = `Certificate III & IV Level` + `Certificate I & II Level` + `Advanced Diploma and Diploma Level`,
         school = `Secondary Education - Years 10 and above`,
         no_school = `Secondary Education - Years 9 and below`) %>%
  select(sa2_name, university, tafe, school, no_school)

usethis::use_data(education, overwrite = TRUE)
