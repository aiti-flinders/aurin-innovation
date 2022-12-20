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

# SA2 POW OCCP1 (2016, 2011)

# sa2_pow_occp <- function(path, skip, n_max, year) {
#
#   read_csv(file = path,
#            skip = {{skip}},
#            n_max = {{n_max}},
#            col_select = c(sa2_name = 3,
#                           occupation = 2,
#                           employment = 4),
#            show_col_types = FALSE) %>%
#     check_sa2(geography = "sa2_name",
#               other = "occupation") %>%
#     mutate(year = {{year}}) %>%
#     pivot_wider(names_from = occupation,
#                 values_from = employment)
#
# }
#
# sa2_occp1_2016 <- sa2_pow_occp("data-raw/sa2_pow_occp1_2016.csv",
#                                skip = 9,
#                                n_max = 27744,
#                                year = 2016)


# SA2 POW QALLP1 (2021, 2016, 2011) ---------------------------------------


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

sa2_qallp1_2021 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2021.csv",
                                 skip = 9,
                                 n_max = 22266,
                                 year = 2021)

sa2_qallp1_2016 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2016.csv",
                               skip = 9,
                               n_max = 20808,
                               year = 2016)

sa2_qallp1_2011 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2011.csv",
                               skip = 9,
                               n_max = 20115,
                               year = 2011)







# SA2 POW OCCP4 (2021, 2016, 2011) ----------------------------------------


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

sa2_occp4_2021 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2021.csv",
                               skip = 9,
                               n_max = 1182572,
                               year = 2021)

sa2_occp4_2016 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2016.csv",
                                skip = 10,
                                n_max = 1105136,
                               year = 2016)

sa2_occp4_2011 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2011.csv",
                               skip = 9,
                               n_max = 1068330,
                               year = 2011)

# SA2 POW INDP4 (2021, 2016, 2011) ----------------------------------------



sa2_pow_ind4 <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},  show_col_types = F,
           col_select = c(sa2_name = 2,
                          anzsic_class = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "anzsic_class") %>%
    mutate(year = {{year}})

}

sa2_indp4_2021 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2021.csv",
                                skip = 9,
                                n_max = 1783754,
                                year = 2021)

sa2_indp4_2016 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2016.csv",
                               skip = 9,
                               n_max = 1666952,
                               year = 2016)

sa2_indp4_2011 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2011.csv",
                               skip = 9,
                               n_max = 1611435,
                               year = 2011)



# SA2 POW INDP2 (2021, 2016, 2011) ----------------------------------------

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

sa2_indp2_2021 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2021.csv",
                              skip = 9,
                              n_max = 2474,
                              year = 2021)

sa2_indp2_2016 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2016.csv",
                              skip = 9,
                              n_max = 2313,
                              year = 2016)

sa2_indp2_2011 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2011.csv",
                              skip = 9,
                              n_max = 2236,
                              year = 2011)






# SA2 POW QALFP2 (2021, 2016, 2011) ---------------------------------------


sa2_pow_qalfp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          qualification = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "qualification") %>%
    mutate(year = {{year}})

}

sa2_qalfp2_2021 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2021.csv",
                                 skip = 9,
                                 n_max = 39584,
                                 year = 2021)

sa2_qalfp2_2016 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2016.csv",
                                 skip = 9,
                                 n_max = 36992,
                                 year = 2016)

sa2_qalfp2_2011 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2011.csv",
                                 skip = 9,
                                 n_max = 35760,
                                 year = 2011)



# SA2 POW SIEMP1 (2021, 2016, 2011) ---------------------------------------------


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

sa2_siemp_2021 <- sa2_pow_siemp("data-raw/sa2_pow_siemp_2021.csv",
                                skip = 9,
                                n_max = 27214,
                                year = 2021) %>%
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

# Misc. -------------------------------------------------------------------


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
sa2_2021 <- read_absmap("sa22021", remove_year_suffix = TRUE)

sa2_2016 <- read_absmap("sa22016", remove_year_suffix = TRUE)

sa2_2011 <- read_absmap("sa22011", remove_year_suffix = TRUE)

sa2_2016_to_sa2_2011 <- get_correspondence_absmaps("sa2", 2016, "sa2", 2011) %>%
  select(sa2_name_2016 = SA2_NAME_2016,
         sa2_name_2011 = SA2_NAME_2011,
         ratio)

sa2_2021_to_sa2_2016 <- read_csv("data-raw/CG_SA2_2021_SA2_2016_GRID21_All.csv") %>%
  select(sa2_name_2021 = SA2_NAME_2021,
         sa2_name_2016 = SA2_NAME_2016,
         ratio = RATIO_FROM_TO)


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

education_location_2021 <- full_join(infrastructure_in_geography(unis, sa2_2021, "sa2_name", "unis", 2021),
                                     infrastructure_in_geography(tafes, sa2_2021, "sa2_name", "tafes", 2021),
                                     by = c("sa2_name"))  %>%
  replace_na(list(unis = 0,
                  tafes = 0)) %>%
  mutate(year = 2021)


# IP Data -----------------------------------------------------------------

# As of 2020 IPGOD release, geocoding is only at the postcode level. Use a population weighted concordance to convert data to SA2

postal_areas <- read_excel("data-raw/POA_2021_AUST.xlsx")
mesh_aus <- read_excel("data-raw/MB_2021_AUST.xlsx")

population_sa2 <- pow(2021, "sa2")

postcode_to_sa2 <- function(data) {

  left_join(postal_areas, mesh_aus, by = c("MB_CODE_2021", "AUS_CODE_2021", "AUS_NAME_2021", "AREA_ALBERS_SQKM", "ASGS_LOCI_URI_2021")) %>%
    left_join(data, by = c("POA_CODE_2021" = "post_code")) %>%
    left_join(population_sa2, by = c("SA2_NAME_2021" = "sa2_name")) %>%
    group_by(year, POA_CODE_2021) %>%
    mutate(share = employment / sum(employment, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(n = n * share) %>%
    group_by(year, SA2_NAME_2021) %>%
    summarise(n = ceiling(sum(n, na.rm = T)), .groups = "drop") %>%
    rename(sa2_name = SA2_NAME_2021)

}


# Plant Breeder Rights ------------------------------------------------------------------


plants <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/385d1ca3-fb91-4c58-acd6-f076744a9122/download/pbr-party-activity.csv",
                   show_col_types = FALSE) %>%
  mutate(post_code = as.character(post_code))

plants_info <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/952d7cf9-8412-4205-80de-405a725a2017/download/pbr-application.csv",
                        show_col_types = FALSE) %>%
  mutate(year = as.double(format(earliest_filed_date, "%Y")))

plants_sa2 <- plants %>%
  left_join(plants_info, by = c("application_number", "ip_right_type")) %>%
  filter(country_code == "au") %>%
  select(application_number, post_code, year) %>%
  group_by(year, post_code) %>%
  tally() %>%
  ungroup() %>%
  postcode_to_sa2()




# Patent Applications -----------------------------------------------------

patents <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/7d8c5138-60d9-4993-b7c9-ecbec25bd57f/download/patent-party-activity.csv",
                    show_col_types = FALSE) %>%
  mutate(post_code = as.character(post_code))

patents_info <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/e0f36eb0-7c83-4bff-a66b-2ba128f07d56/download/patent-application.csv",
                         show_col_types = FALSE) %>%
  mutate(year = as.double(format(earliest_filed_date, "%Y")))

patents_citations <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/2958b4d0-de04-4884-8aaa-0d9121219eba/download/patent-application-links.csv",
                              show_col_types = FALSE) %>%
  group_by(application_number) %>%
  tally(name = "backwards_citations")

patents_citations_sa2 <- patents %>%
  left_join(patents_info, by = c("ip_right_type", "application_number")) %>%
  left_join(patents_citations, by = "application_number") %>%
  filter(country_code == "au") %>%
  group_by(year, post_code) %>%
  summarise(n = sum(backwards_citations, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  postcode_to_sa2() %>%
  rename(backwards_citations = n)

patents_sa2 <- patents %>%
  left_join(patents_info, by = c("application_number", "ip_right_type")) %>%
  filter(country_code == "au")  %>%
  select(application_number, post_code, year) %>%
  group_by(year, post_code) %>%
  tally() %>%
  ungroup() %>%
  postcode_to_sa2()

patents_sa2 <- left_join(patents_sa2, patents_citations_sa2, by = c("year", "sa2_name"))



# Trademarks --------------------------------------------------------------

trademarks <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/9197f81e-8c43-4146-a6b4-e0d380f9dc1d/download/trade-mark-party-activity.csv",
                       show_col_types = FALSE) %>%
  mutate(post_code = as.character(post_code))

trademarks_info <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/71f3c2ba-c390-44bb-8713-1fe4c42dd238/download/trade-mark-application.csv",
                            show_col_types = FALSE) %>%
  mutate(year = as.double(format(earliest_filed_date, "%Y")))

trademarks_sa2 <- trademarks %>%
  left_join(trademarks_info, by = c("application_number", "ip_right_type")) %>%
  filter(country_code == "au") %>%
  select(application_number, post_code, year) %>%
  group_by(year, post_code) %>%
  tally() %>%
  ungroup() %>%
  postcode_to_sa2()



# Designs -----------------------------------------------------------------

designs <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/89dacae9-4649-4612-aeb8-489af3aec379/download/design-party-activity.csv",
                    show_col_types = FALSE) %>%
  mutate(post_code = as.character(post_code))

designs_info <- read_csv("https://data.gov.au/data/dataset/49017fd0-e7be-4fc0-88c8-046fc366d980/resource/db4da728-d954-4539-adb3-33ac50c50070/download/design-application.csv",
                         show_col_types = FALSE) %>%
  mutate(year = as.double(format(earliest_filed_date, "%Y")))

designs_sa2 <- designs %>%
  left_join(designs_info, by = c("application_number", "ip_right_type")) %>%
  filter(country_code == "au") %>%
  select(application_number, post_code, year) %>%
  group_by(year, post_code) %>%
  tally() %>%
  ungroup() %>%
  postcode_to_sa2()


usethis::use_data(sa2_siemp_2011,
                  sa2_siemp_2016,
                  sa2_siemp_2021,
                  sa2_qalfp2_2011,
                  sa2_qalfp2_2016,
                  sa2_qalfp2_2021,
                  sa2_indp2_2011,
                  sa2_indp2_2016,
                  sa2_indp2_2021,
                  sa2_indp4_2011,
                  sa2_indp4_2016,
                  sa2_indp4_2021,
                  sa2_occp1_2016,
                  sa2_occp4_2011,
                  sa2_occp4_2016,
                  sa2_occp4_2021,
                  sa2_qallp1_2011,
                  sa2_qallp1_2016,
                  sa2_qallp1_2021,
                  sa2_2011,
                  sa2_2016,
                  sa2_2021,
                  sa2_2016_to_sa2_2011,
                  sa2_2021_to_sa2_2016,
                  education_location_2011,
                  education_location_2016,
                  education_location_2021,
                  state_economic_complexity,
                  product_data,
                  anzsic_hs,
                  designs_sa2,
                  trademarks_sa2,
                  plants_sa2,
                  patents_sa2,
                  compress = "xz",
                  internal = TRUE,
                  overwrite = TRUE)




