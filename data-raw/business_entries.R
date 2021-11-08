## code to prepare `business_entries` dataset goes here
library(readxl)
library(purrr)
library(dplyr)


parse_cabee <- function(path, sheet, max_read) {

  read_xls(path, sheet, skip = 7, col_names = c("industry_code",
                                                "industry_label",
                                                "sa2_code",
                                                "sa2_label",
                                                "non_employing",
                                                "employing_1_4",
                                                "employing_5_19",
                                                "employing_20_199",
                                                "employing_200_plus",
                                                "total")) %>%
    mutate(year = sheet)

}

cabee <- map_dfr(.x = c("June 2015", "June 2016"), .f = ~parse_cabee("data-raw/816508.xls", sheet = .x))

business_entries <- cabee %>%
  select(industry_label, sa2_label, total, year) %>%
  filter(!is.na(sa2_label),
         !str_detect(industry_label, "Total"),
         !str_detect(industry_label, "Currently Unknown")) %>%
  pivot_wider(names_from = industry_label, values_from = total) %>%
  filter(year == "June 2016") %>%
  mutate(total_businesses = rowSums(across(where(is.double)))) %>%
  pivot_longer(cols = 3:(length(.) - 1)) %>%
  group_by(sa2_label) %>%
  mutate(value = value/sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = name, values_from = value) %>%
  ungroup() %>%
  rename(sa2_name = sa2_label) %>%
  select(-year)

usethis::use_data(business_entries, overwrite = TRUE)
