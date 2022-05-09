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

  if (!file.exists("data-raw/ipgod110.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/12d28b0f-b16e-487d-b6ca-b44ea7b97d8c/download/ipgod110.csv",
                  destfile = "data-raw/ipgod110.csv")

    patents_citations <- readr::read_csv("data-raw/ipgod110.csv")

  } else {

    patents_citations <- readr::read_csv("data-raw/ipgod110.csv")

  }

  if (year == 2011) {

    geog <- sa2_2011 %>%
      sf::st_drop_geometry()

  }

  else if (year == 2016) {

    geog <- sa2_2016 %>%
      sf::st_drop_geometry()

  } else {

      stop("Year must be one of 2011 or 2016.")
    }

  patents %>%
    dplyr::left_join(patents_info) %>%
    dplyr::left_join(patents_citations) %>%
    dplyr::mutate(ipc = stringr::str_sub(primary_ipc_mark_value, 0, 1)) %>%
    dplyr::filter(application_year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::group_by(australian_appl_no) %>%
    dplyr::mutate(backwards_citations = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], application_year) %>%
    dplyr::summarise(patents = dplyr::n(),
                     backwards_citations = mean(backwards_citations)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(year = application_year)
    # tidyr::pivot_wider(names_from = ipc,
    #                    values_from = patents) %>%
    # dplyr::filter(!is.na(.data[[geography]])) %>%
    # tidyr::replace_na(list(A = 0, B = 0, C = 0, D = 0, E = 0, `F` = 0, G = 0, H = 0)) %>%
    # dplyr::rename(year = application_year) %>%
    # dplyr::group_by(.data[[geography]], year) %>%
    # dplyr::summarise(dplyr::across(tidyselect:::where(is.integer), ~sum(.x)), .groups = "drop")
}
