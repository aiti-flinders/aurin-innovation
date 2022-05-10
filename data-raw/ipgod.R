## code to prepare `plants` dataset goes here
library(dplyr)
library(readr)


# Plant Breeder Rights ------------------------------------------------------------------


plants <- read_csv("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/43d22a58-b182-4e24-97b4-7a68dd0f9add/download/ipgod402-update.csv",
                          show_col_types = FALSE) %>%
  mutate(sa2_code = as.character(sa2_code))

plants_info <- read_csv("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/8968df95-6268-4c3c-8007-23cb3de1066a/download/ipgod401.csv",
                        show_col_types = FALSE) %>%
  rename(year = appl_received_year)

plants <- plants %>%
  left_join(plants_info, by = c("appl_number", "country", "entity")) %>%
  select(appl_number, sa2_name, year) %>%
  filter(!is.na(sa2_name))


usethis::use_data(plants, compress = "xz", overwrite = TRUE)


# Patent Applications -----------------------------------------------------

patents <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/846990df-db42-4ad7-bbd6-567fd37a2797/download/ipgod102.csv",
                    show_col_types = FALSE) %>%
  mutate(sa2_code = as.character(sa2_code))

patents_info <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/e5cbeafc-5fb3-4dfd-bd22-afe81b6ab1e1/download/ipgod101.csv",
                         show_col_types = FALSE) %>%
  rename(year = application_year)

patents_citations <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/12d28b0f-b16e-487d-b6ca-b44ea7b97d8c/download/ipgod110.csv",
                              show_col_types = FALSE,
                              col_types = c("nccccccc"))

patents <- patents %>%
  left_join(patents_info, by = c("australian_appl_no", "australian", "entity")) %>%
  left_join(patents_citations, by = "australian_appl_no") %>%
  select(australian_appl_no, sa2_name, year, primary_ipc_mark_value) %>%
  filter(!is.na(sa2_name))

usethis::use_data(patents, compress = "xz", overwrite = TRUE)


# Trademarks --------------------------------------------------------------

trademarks <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/aae1c14d-f8c0-4540-b5d3-1ed21500271e/download/ipgod202.csv",
                       show_col_types = FALSE) %>%
  mutate(sa2_code = as.character(sa2_code))

trademarks_info <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/3066e8bc-ccfa-4285-bee2-492086886663/download/ipgod201.csv",
                            show_col_types = FALSE) %>%
  rename(year = filing_year)

trademarks <- trademarks %>%
  left_join(trademarks_info, by = c("tm_number", "australian", "entity")) %>%
  select(tm_number, sa2_name, year) %>%
  filter(!is.na(sa2_name))

usethis::use_data(trademarks, compress = "xz", overwrite = TRUE)


# Designs -----------------------------------------------------------------

designs <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/4b802e80-c667-4b84-8f50-72c2624c59c1/download/ipgod302.csv",
                    show_col_types = FALSE) %>%
  mutate(sa2_code = as.character(sa2_code))

designs_info <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/71770a53-8727-4f00-a423-db4878f910f6/download/ipgod301.csv",
                         show_col_types = FALSE) %>%
  rename(year = filing_year)

designs <- designs %>%
  left_join(designs_info, by = c("application_id", "australian", "entity")) %>%
  select(application_id, sa2_name, year) %>%
  filter(!is.na(sa2_name))

usethis::use_data(designs, compress = "xz", overwrite = TRUE)
