create_designs <- function(year, geography = NULL) {

  if (!file.exists("data-raw/ipgod302.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/4b802e80-c667-4b84-8f50-72c2624c59c1/download/ipgod302.csv",
                  destfile = "data-raw/ipgod302.csv")

    designs <- readr::read_csv("data-raw/ipgod302.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    designs <- readr::read_csv("data-raw/ipgod302.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod301.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/71770a53-8727-4f00-a423-db4878f910f6/download/ipgod301.csv",
                  destfile = "data-raw/ipgod301.csv")

    designs_info <- readr::read_csv("data-raw/ipgod301.csv") %>%
      dplyr::rename(application_year = filing_year)

  } else {

    designs_info <- readr::read_csv("data-raw/ipgod301.csv") %>%
      dplyr::rename(application_year = filing_year)
  }

  if (year == 2016) {
    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
  }
  else {
    geog <- sa2_2011 %>%
      sf::st_drop_geometry()
  }

  designs %>%
    dplyr::mutate(type = "designs",
                  year = {{year}}) %>%
    dplyr::left_join(designs_info) %>%
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
