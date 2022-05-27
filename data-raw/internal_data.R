## code to prepare `table_builder_data` dataset goes here.
library(readr)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)
library(dplyr)
library(strayr)
library(absmapsdata)
library(ecomplexity)
library(aurintools)


# ABS Table Builder -------------------------------------------------------



# SA2 POW QALLP1 (2016, 2011)

sa2_pow_qallp <- function(path, skip, n_max, year) {

  read_csv(file = path,
           skip = {{skip}},
           n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          education_level = 3,
                          employment = 4),
           show_col_types = FALSE) %>%
    check_sa2(geography = "sa2_name",
              other = "education_level")  %>%
   mutate(year = {{year}})

}

sa2_qallp1_2016 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2016.csv",
                               skip = 9,
                               n_max = 20808,
                               year = 2016)

sa2_qallp1_2011 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2011.csv",
                               skip = 9,
                               n_max = 20115,
                               year = 2011)




# SA2 POW OCCP4 (2016, 2011)

sa2_pow_occp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          anzsco_name = 3,
                          employment = 4)) %>%
    fill(sa2_name, .direction = "down") %>%
    check_sa2(geography = "sa2_name",
              other = "anzsco_name") %>%
    mutate(year = {{year}})

}

sa2_occp4_2016 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2016.csv",
                                skip = 10,
                                n_max = 1105136,
                               year = 2016)

sa2_occp4_2011 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2011.csv",
                               skip = 9,
                               n_max = 1068330,
                               year = 2011)

# SA2 POW INDP4 (2016, 2011)

## code to prepare `sa2_indp4_2016` dataset goes here
sa2_pow_ind4 <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},  show_col_types = F,
           col_select = c(sa2_name = 2,
                          anzsic_class = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "anzsic_class") %>%
    mutate(year = {{year}})

}

sa2_indp4_2016 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2016.csv",
                               skip = 9,
                               n_max = 1666952,
                               year = 2016)

sa2_indp4_2011 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2011.csv",
                               skip = 9,
                               n_max = 1611435,
                               year = 2011)


# SA2 POW INDP2 (2016, 2011)
sa2_pow_ind <- function(path, skip, n_max, year) {


  read_csv(path, skip = {{skip}}, n_max = {{n_max}}, show_col_types = F) %>%
    slice(-1) %>%
    rename("sa2_name" = 1) %>%
    pivot_longer(names_to = "industry",
                 values_to = "employment",
                 cols = where(is.double))  %>%
    check_sa2(geography = "sa2_name",
              other = "industry") %>%
    mutate(year = {{year}})


}

sa2_indp2_2016 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2016.csv",
                              skip = 9,
                              n_max = 2313,
                              year = 2016)

sa2_indp2_2011 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2011.csv",
                              skip = 9,
                              n_max = 2236,
                              year = 2011)





# SA2 POW QALFP2 (2016, 2011)

sa2_pow_qalfp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          qualification = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "qualification") %>%
    mutate(year = {{year}})

}

sa2_qalfp2_2016 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2016.csv",
                                 skip = 9,
                                 n_max = 36992,
                                 year = 2016)

sa2_qalfp2_2011 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2011.csv",
                                 skip = 9,
                                 n_max = 35760,
                                 year = 2011)


# SA2 POW SIEMP (2016, 2011)

sa2_pow_siemp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          employment_status = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "employment_status") %>%
    mutate(year = {{year}})
}

#SIEMP was introduced in 2016 to replace 2011 - back cast to ensure they're equal.

sa2_siemp_2016 <- sa2_pow_siemp("data-raw/sa2_pow_siemp_2016.csv",
                                skip = 9,
                                n_max = 25432,
                                year = 2016) %>%
  pivot_wider(names_from = employment_status,
              values_from = employment) %>%
  mutate("Owner managers of incorporated enterprises" = `Owner manager of incorporated enterprise with employees` + `Owner manager of incorporated enterprise without employees` + `Owner manager of incorporated enterprise - employees not stated`,
         "Owner managers of unincorporated enterprises" = `Owner manager of unincorporated enterprise with employees` + `Owner manager of unincorporated enterprise without employees` + `Owner manager of unincorporated enterprise - employees not stated`) %>%
  select(sa2_name,
         `Employee not owning business` = Employee,
         `Owner managers of incorporated enterprises`,
         `Owner managers of unincorporated enterprises`,
         `Contributing family workers` = `Contributing family worker`,
         year) %>%
  pivot_longer(cols = c(-sa2_name, -year),
               names_to = "employment_status",
               values_to = "employment")

sa2_siemp_2011 <- sa2_pow_siemp("data-raw/sa2_pow_siemp_2011.csv",
                                skip = 9,
                                n_max = 15645,
                                year = 2011)


# Economic Complexity -----------------------------------------------------

load("data-raw/hs_product.RData")

#Clear stata attributes
attr(table$product_id, "format.stata") <- NULL
attr(table$hs_product_code, "format.stata") <- NULL
attr(table$hs_product_name_short_en, "format.stata") <- NULL
attr(table$level, "format.stata") <- NULL
attr(table$parent_id, "format.stata") <- NULL

product_data <- table %>%
  as_tibble()

state_economic_complexity <- get_data(c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")) %>%
  mutate(location_code = clean_state(location_code, to = "state_name"))

#ANZSIC to HS

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

# Maps --------------------------------------------------------------------

sa2_2016 <- read_absmap("sa22016", remove_year_suffix = TRUE)

sa2_2011 <- read_absmap("sa22011", remove_year_suffix = TRUE)

sa2_2016_to_sa2_2011 <- get_correspondence_absmaps("sa2", 2016, "sa2", 2011) %>%
  select(sa2_name_2016 = SA2_NAME_2016,
         sa2_name_2011 = SA2_NAME_2011,
         ratio)

if (any(!file.exists(c("data-raw/unis.geoJSON", "data-raw/tafes.geoJSON")))) {

  uni_id <- aurin_id("AURIN - National Education Facilities - Universities (Point) 2018", exact = TRUE)
  tafe_id <- aurin_id("AURIN - National Education Facilities - TAFEs (Point) 2018", exact = TRUE)

  map2_chr(.x = c(uni_id, tafe_id),
           .y = c("unis", "tafes"),
           .f = ~aurin_download_file(api_id = .x, out_file_name = .y, out_folder = "data-raw"))

} else {


  unis <- read_aurin("data-raw/unis.geoJSON") %>%
    st_transform(4326)

  tafes <- read_aurin("data-raw/tafes.geoJSON") %>%
    st_transform(4326)

}

infrastructure_in_geography <- function(data, map, geography, name, year) {

  st_join(data, map) %>%
    group_by(.data[[geography]]) %>%
    tally(name = name) %>%
    st_drop_geometry()

}


education_location_2011 <- full_join(infrastructure_in_geography(unis, sa2_2011, "sa2_name", "unis", 2011),
                                     infrastructure_in_geography(tafes, sa2_2011, "sa2_name", "tafes", 2011),
                                     by = c("sa2_name")) %>%
  replace_na(list(unis = 0,
                  tafes = 0)) %>%
  mutate(year = 2011)



education_location_2016 <- full_join(infrastructure_in_geography(unis, sa2_2016, "sa2_name", "unis", 2016),
                                     infrastructure_in_geography(tafes, sa2_2016, "sa2_name", "tafes", 2016),
                                     by = c("sa2_name")) %>%
  replace_na(list(unis = 0,
                  tafes = 0)) %>%
  mutate(year = 2016)


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




# Patent Applications -----------------------------------------------------

patents <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/846990df-db42-4ad7-bbd6-567fd37a2797/download/ipgod102.csv",
                    show_col_types = FALSE) %>%
  mutate(sa2_code = as.character(sa2_code))

patents_info <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/e5cbeafc-5fb3-4dfd-bd22-afe81b6ab1e1/download/ipgod101.csv",
                         show_col_types = FALSE) %>%
  rename(year = application_year)

patents_citations <- read_csv("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/12d28b0f-b16e-487d-b6ca-b44ea7b97d8c/download/ipgod110.csv",
                              show_col_types = FALSE,
                              col_types = c("nccccccc")) %>%
  group_by(australian_appl_no) %>%
  tally(name = "backwards_citations")

patents <- patents %>%
  left_join(patents_info, by = c("australian_appl_no", "australian", "entity")) %>%
  left_join(patents_citations, by = "australian_appl_no") %>%
  select(australian_appl_no, sa2_name, year, primary_ipc_mark_value, backwards_citations) %>%
  filter(!is.na(sa2_name))



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

usethis::use_data(sa2_siemp_2011,
                  sa2_siemp_2016,
                  sa2_qalfp2_2016,
                  sa2_qalfp2_2011,
                  sa2_indp2_2011,
                  sa2_indp2_2016,
                  sa2_indp4_2016,
                  sa2_indp4_2011,
                  sa2_occp4_2016,
                  sa2_occp4_2011,
                  sa2_qallp1_2011,
                  sa2_qallp1_2016,
                  sa2_2011,
                  sa2_2016,
                  sa2_2016_to_sa2_2011,
                  education_location_2011,
                  education_location_2016,
                  state_economic_complexity,
                  product_data,
                  anzsic_hs,
                  designs,
                  trademarks,
                  plants,
                  patents,
                  compress = "xz",
                  internal = TRUE,
                  overwrite = TRUE)




