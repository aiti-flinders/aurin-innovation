create_patents <- function(year, geography = NULL) {

  if (!file.exists("data-raw/ipgod102.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/846990df-db42-4ad7-bbd6-567fd37a2797/download/ipgod102.csv",
                  destfile = "data-raw/ipgod102.csv")

    patents <- readr::read_csv("data-raw/ipgod102.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    patents <- readr::read_csv("data-raw/ipgod102.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod101.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/e5cbeafc-5fb3-4dfd-bd22-afe81b6ab1e1/download/ipgod101.csv",
                  destfile = "data-raw/ipgod101.csv")

    patents_info <- readr::read_csv("data-raw/ipgod101.csv")

  } else {

    patents_info <- readr::read_csv("data-raw/ipgod101.csv")
  }

  if (year < 2016) {
    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()
    }
  else {
    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()
    }

  patents %>%
    dplyr::mutate(type = "patents",
                  year = {{year}}) %>%
    dplyr::left_join(patents_info) %>%
    dplyr::filter(application_year == {{year}},
                  australian == TRUE,
                  !patent_status_type %in% c("LAPSED", "REFUSED", "REVOKED", "WITHDRAWN"),
                  patent_type != "Provisional") %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], type, year) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = n) %>%
    dplyr::filter(!is.na(.data[[geography]]))
}
