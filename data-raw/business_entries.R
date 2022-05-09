## code to prepare `business_entries` dataset goes here
library(readxl)
library(purrr)
library(dplyr)


parse_cabee <- function(path, sheet, max_read) {

  read_xls(path, sheet, skip = 7, col_names = c("industry_code",
                                                "industry",
                                                "sa2_code",
                                                "sa2_name",
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
  select(industry, sa2_name, total, year) %>%
  filter(!is.na(sa2_name),
         !str_detect(industry, "Total"),
         !str_detect(industry, "Currently Unknown")) %>%
  left_join(sa2_2016) %>%
  group_by(sa2_name, year) %>%
  summarise(across(total, sum), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = total) %>%
  rename(businesses_2015 = `June 2015`,
         businesses_2016 = `June 2016`)

usethis::use_data(business_entries, overwrite = TRUE)
