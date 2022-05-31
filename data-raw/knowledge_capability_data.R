## code to prepare `knowledge_capability_data` dataset goes here
library(purrr)
library(dplyr)
library(tidyr)
library(aurininnovation)

kc <- map_df(.x = c(2011:2017), ~aurininnovation:::create_knowledge_capability(.x, "sa2")) %>%
  filter(!is.na(sa2_name)) %>%
  replace_na(list(designs = 0,
    patents = 0,
    backwards_citations = 0,
    trademarks = 0,
    plants = 0))

pow <- map_df(.x = c(2011, 2016), ~pow(.x, "sa2") %>% dplyr::mutate(year = .x)) %>%
  pivot_wider(names_from = year,
    values_from = employment,
    names_prefix = "y") %>%
  mutate(y2012 = y2011,
    y2013 = y2011,
    y2014 = y2011,
    y2015 = y2011,
    y2017 = y2016) %>%
  pivot_longer(cols = 2:length(.),
    names_to = "year",
    values_to = "employment") %>%
  mutate(year = gsub("y", "", year),
    year = as.numeric(year)) %>%
  filter(!is.na(employment))

knowledge_capability_data <- kc %>%
  left_join(pow, by = c("sa2_name", "year")) %>%
  add_sa2_codes()

usethis::use_data(knowledge_capability_data, compress = "xz", overwrite = TRUE)
