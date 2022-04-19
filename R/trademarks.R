create_trademarks <- function(year, geography = NULL) {

  if (!file.exists("data-raw/ipgod202.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/aae1c14d-f8c0-4540-b5d3-1ed21500271e/download/ipgod202.csv",
                  destfile = "data-raw/ipgod202.csv")

    trademarks <- readr::read_csv("data-raw/ipgod202.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    trademarks <- readr::read_csv("data-raw/ipgod202.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod201.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/3066e8bc-ccfa-4285-bee2-492086886663/download/ipgod201.csv",
                  destfile = "data-raw/ipgod201.csv")

    trademarks_info <- readr::read_csv("data-raw/ipgod201.csv") %>%
      dplyr::rename(application_year = filing_year)

  } else {

    trademarks_info <- readr::read_csv("data-raw/ipgod201.csv") %>%
      dplyr::rename(application_year = filing_year)
  }

  if (year < 2016) {
    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()
  }
  else {
    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()
  }

  trademarks %>%
    dplyr::mutate(type = "trademarks",
                  year = {{year}}) %>%
    dplyr::left_join(trademarks_info) %>%
    dplyr::filter(application_year == {{year}},
                  australian == TRUE,
                  !stringr::str_detect(status_code_desc, "Refused|Rejected")) %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], type, year) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = n) %>%
    dplyr::filter(!is.na(.data[[geography]]))
}
