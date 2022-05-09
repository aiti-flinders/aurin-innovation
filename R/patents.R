#' Patents data
#'
#' Creates a dataframe with the number of patents filed in a geographic area for a given year, and
#' the average number of backwards citations that a patent application included.
#'
#' @param year numeric. The year the patents were filed.
#' @param geography string. The geographic region of interest. Defaults to SA2.
#'
#' @return a dataframe of patents by geography for a given year.
#' @export
#'
#' @examples \dontrun{
#' create_patents(2016, "sa2")
#' }
create_patents <- function(year, geography = "sa2") {

  if (!year %in% c(2011, 2016, 2021)) {
    warning("A non-census year was specified.
            Other indicators of innovation are only available for the census years 2011, 2016, and 2021.
            Proceed with caution. ")
  }

 if (year == 2011) {
    geog <- sa2_2011
  } else {
    geog <- sa2_2016
  }

  stopifnot(tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (!file.exists("data-raw/ipgod102.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/846990df-db42-4ad7-bbd6-567fd37a2797/download/ipgod102.csv",
                  destfile = "data-raw/ipgod102.csv")

    patents <- readr::read_csv("data-raw/ipgod102.csv",
                               show_col_types = FALSE) %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    patents <- readr::read_csv("data-raw/ipgod102.csv",
                               show_col_types = FALSE) %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod101.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/e5cbeafc-5fb3-4dfd-bd22-afe81b6ab1e1/download/ipgod101.csv",
                  destfile = "data-raw/ipgod101.csv")

    patents_info <- readr::read_csv("data-raw/ipgod101.csv",
                                    show_col_types = FALSE) %>%
      dplyr::rename(year = application_year)

  } else {

    patents_info <- readr::read_csv("data-raw/ipgod101.csv",
                                    show_col_types = FALSE) %>%
      dplyr::rename(year = application_year)
  }

  if (!file.exists("data-raw/ipgod110.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/12d28b0f-b16e-487d-b6ca-b44ea7b97d8c/download/ipgod110.csv",
                  destfile = "data-raw/ipgod110.csv")

    patents_citations <- readr::read_csv("data-raw/ipgod110.csv",
                                         col_types = c("nccccccc"),
                                         show_col_types = FALSE)

  } else {

    patents_citations <- readr::read_csv("data-raw/ipgod110.csv",
                                         col_types = c("nccccccc"),
                                         show_col_types = FALSE)

  }

  p <- patents %>%
    dplyr::left_join(patents_info, by = c("australian_appl_no", "australian", "entity")) %>%
    dplyr::left_join(patents_citations, by = "australian_appl_no") %>%
    dplyr::mutate(ipc = stringr::str_sub(primary_ipc_mark_value, 0, 1)) %>%
    dplyr::filter(year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::group_by(australian_appl_no) %>%
    dplyr::mutate(backwards_citations = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sa2_name, year) %>%
    dplyr::summarise(patents = dplyr::n(),
                     backwards_citations = sum(backwards_citations),
                     .groups = "drop") %>%
    dplyr::ungroup()

  if (year < 2016) {
    p <- p %>% sa2_16_to_sa2_11(var = c("patents", "backwards_citations"))
  }

  p %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(patents = sum(patents),
                     backwards_citations = mean(backwards_citations)) %>%
    dplyr::ungroup()
}
