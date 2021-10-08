## code to prepare `knowledge_intensity` dataset goes here
#Knowledge intensity includes:
#patents, designs, trademarks
library(vroom)
library(dplyr)
library(tidyr)


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

ipgod_filter_year <- function(geography_data, year_data) {
  geography_data %>%
    left_join(year_data) %>%
    filter(application_year == 2016,
           australian == TRUE)
}




working_population <- sa2_indp2_2016 %>%
  distinct(sa2_name, region_employment)

ip <- bind_rows(
  patents %>% mutate(type = "patents") %>% ipgod_filter_year(patents_info),
  designs %>% mutate(type = "designs") %>% ipgod_filter_year(designs_info),
  tms %>% mutate(type = "trademarks") %>% ipgod_filter_year(tms_info),
  pbr %>% mutate(type = "plants") %>% ipgod_filter_year(pbr_info)
  ) %>%
  filter(name != "non-entity") %>%
  group_by(sa2_name, type) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = type,
              values_from = n) %>%
  filter(!is.na(sa2_name))

knowledge_intensity <- ip %>%
  right_join(working_population, by = "sa2_name") %>%
  left_join(education, by = "sa2_name") %>%
  left_join(employment_kibs, by = "sa2_name") %>%
  filter(region_employment > 10) %>%
  mutate(across(c(designs, patents, trademarks, plants), .fns = ~.x * 1000 / region_employment),
         across(c(university, tafe, school, no_school, kibs), ~.x * 100 / region_employment)) %>%
  replace_na(list(trademarks = 0,
                  designs = 0,
                  patents = 0,
                  plants = 0))


usethis::use_data(knowledge_intensity, overwrite = TRUE)

model <- "KNOW =~ patents  +  designs + trademarks + plants"


fit <- lavaan::cfa(model, knowledge_intensity)

know <- merge(data.frame(know = lavPredict(fit)) %>% rownames_to_column(),
              knowledge_intensity %>% rownames_to_column())  %>%
  as_tibble() %>%
  select(-rowname)
summary(fit, fit.measures = TRUE)
