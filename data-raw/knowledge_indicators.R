## code to prepare `knowledge_intensity` dataset goes here
#Knowledge intensity includes:
#patents, designs, trademarks
library(vroom)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(absmapsdata)
library(lavaan)

devtools::load_all()


patents <- vroom("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/846990df-db42-4ad7-bbd6-567fd37a2797/download/ipgod102.csv")
patents_info <- vroom("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/e5cbeafc-5fb3-4dfd-bd22-afe81b6ab1e1/download/ipgod101.csv")


designs <- vroom("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/4b802e80-c667-4b84-8f50-72c2624c59c1/download/ipgod302.csv")
designs_info <- vroom("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/71770a53-8727-4f00-a423-db4878f910f6/download/ipgod301.csv")
designs_info <- designs_info %>%
  rename(application_year = filing_year)

tms <- vroom("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/aae1c14d-f8c0-4540-b5d3-1ed21500271e/download/ipgod202.csv")
tms_info <- vroom("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/3066e8bc-ccfa-4285-bee2-492086886663/download/ipgod201.csv")
tms_info <- tms_info %>%
  rename(application_year = filing_year)

pbr <- vroom("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/43d22a58-b182-4e24-97b4-7a68dd0f9add/download/ipgod402-update.csv")
pbr_info <- vroom("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/8968df95-6268-4c3c-8007-23cb3de1066a/download/ipgod401.csv")
pbr_info <- pbr_info %>%
  rename(application_year = appl_received_year)

ipgod_filter_year <- function(geography_data, year_data, year) {
  geography_data %>%
    left_join(year_data) %>%
    filter(application_year == {{year}},
           australian == TRUE)
}


patents_data <- function(year) {
  patents %>%
    mutate(type = "patents",
           year = {{year}}) %>%
    ipgod_filter_year(patents_info, {{year}}) %>%
    filter(!patent_status_type %in% c("REFUSED", "REVOKED"),
           patent_type != "Petty",
           name != "non-entity") %>%
    group_by(sa2_name, type, year) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(names_from = type,
                values_from = n) %>%
    filter(!is.na(sa2_name))
}

designs_data <- function(year) {
  designs %>%
    mutate(type = "designs",
           year = {{year}}) %>%
    ipgod_filter_year(designs_info, {{year}}) %>%
    filter(status_code != "Currently not in force") %>%
    filter(name != "non_entity") %>%
    group_by(sa2_name, type, year) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(names_from = type,
                values_from = n) %>%
    filter(!is.na(sa2_name))
}

tms_data <- function(year) {
  tms %>%
    mutate(type = "trademarks",
           year = {{year}}) %>%
    ipgod_filter_year(tms_info, {{year}}) %>%
    filter(!stringr::str_detect(status_code_desc, "Refused|Rejected"),
           name != "non-entity") %>%
    group_by(sa2_name, type, year) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(names_from = type,
                values_from = n) %>%
    filter(!is.na(sa2_name))
}

plants_data <- function(year) {
  pbr %>%
    mutate(type = "plant_breeder_rights",
           year = {{ year }}) %>%
    ipgod_filter_year(pbr_info, {{year}}) %>%
    filter(appl_status_code %in% c("GRANTED", "ACCEPTED", "RECEIVED"),
           name != "non-entity") %>%
    group_by(sa2_name, type, year) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(names_from = type,
                values_from = n) %>%
    filter(!is.na(sa2_name))
}

knowledge_indicators <- left_join(x = map_dfr(.x = c(2011:2018), .f = ~patents_data(.x)),
                                  y = map_dfr(.x = c(2011:2018), .f = ~designs_data(.x)),
                                  by = c("sa2_name", "year")) %>%
  left_join(y = map_dfr(.x = c(2011:2017), .f = ~tms_data(.x)),
            by = c('sa2_name', "year")) %>%
  left_join(y = map_df(.x = c(2011:2017), .f = ~plants_data(.x)),
            by = c("sa2_name", "year"))

usethis::use_data(knowledge_indicators, compress = "xz", overwrite = TRUE)




