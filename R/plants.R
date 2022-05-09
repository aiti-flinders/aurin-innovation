create_plants <- function(year, geography = NULL) {

  if (!file.exists("data-raw/ipgod402.csv")) {

    download.file("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/43d22a58-b182-4e24-97b4-7a68dd0f9add/download/ipgod402-update.csv",
                  destfile = "data-raw/ipgod402.csv")

    plants <- readr::read_csv("data-raw/ipgod402.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    plants <- readr::read_csv("data-raw/ipgod402.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod401.csv")) {

    download.file("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/8968df95-6268-4c3c-8007-23cb3de1066a/download/ipgod401.csv",
                  destfile = "data-raw/ipgod401.csv")

    plants_info <- readr::read_csv("data-raw/ipgod401.csv") %>%
      dplyr::rename(application_year = appl_received_year)

  } else {

    plants_info <- readr::read_csv("data-raw/ipgod401.csv") %>%
      dplyr::rename(application_year = appl_received_year)
  }

  if (year == 2016) {
    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
  }
  else {
    geog <- sa2_2011  %>%
      sf::st_drop_geometry()
  }

  plants %>%
    dplyr::mutate(type = "plants",
                  year = {{year}}) %>%
    dplyr::left_join(plants_info) %>%
    dplyr::filter(application_year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], type, year) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = n) %>%
    dplyr::filter(!is.na(.data[[geography]]))
}
