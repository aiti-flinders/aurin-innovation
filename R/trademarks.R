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

  if (year == 2011) {
    geog <- sa2_2011 %>%
      sf::st_drop_geometry()
  }
  else if (year == 2016) {
    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
  } else {

    stop("Year must be 2011 or 2016.")

  }

  trademarks %>%
    dplyr::left_join(trademarks_info) %>%
    dplyr::filter(application_year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], application_year) %>%
    dplyr::summarise(trademarks = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(year = application_year)
    # tidyr::pivot_wider(names_from = trademark_type,
    #                    values_from = trademarks) %>%
    # tidyr::replace_na(list(Fancy = 0, Figurative = 0, Word = 0, Shape = 0, Colour = 0, Movement = 0)) %>%
    # dplyr::rename(year = application_year) %>%
    # dplyr::filter(!is.na(.data[[geography]]))
}
