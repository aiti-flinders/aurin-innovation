#' Trademarks data
#'
#' Creates a dataframe with the number of trademarks filed in a geographic area for a given year.
#'
#'
#' @param year numeric. The year the trademarks were filed.
#' @param geography string. The geographic region of interest. Defaults to SA2.
#'
#' @return dataframe of trademarks by geography for a given year.
#' @export
#'
#' @examples \dontrun{
#' create_trademarks(2016, "sa2")
#' }
create_trademarks <- function(year, geography = "sa2") {

  # The IPGOD dataset for 2019 uses the 2016 ASGS for naming SA2 Regions, even if they did not exist in 2011.

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

  if (!file.exists("data-raw/ipgod202.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/aae1c14d-f8c0-4540-b5d3-1ed21500271e/download/ipgod202.csv",
                  destfile = "data-raw/ipgod202.csv")

    trademarks <- readr::read_csv("data-raw/ipgod202.csv",
                                  show_col_types = FALSE) %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    trademarks <- readr::read_csv("data-raw/ipgod202.csv",
                                  show_col_types = FALSE) %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod201.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/3066e8bc-ccfa-4285-bee2-492086886663/download/ipgod201.csv",
                  destfile = "data-raw/ipgod201.csv")

    trademarks_info <- readr::read_csv("data-raw/ipgod201.csv",
                                       show_col_types = FALSE) %>%
      dplyr::rename(year = filing_year)

  } else {

    trademarks_info <- readr::read_csv("data-raw/ipgod201.csv",
                                       show_col_types = FALSE) %>%
      dplyr::rename(year = filing_year)
  }

  tms <- trademarks %>%
    dplyr::left_join(trademarks_info, by = c("tm_number", "australian", "entity")) %>%
    dplyr::filter(year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::group_by(sa2_name, year) %>%
    dplyr::summarise(trademarks = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup()

  if (year < 2016) {

    tms <- tms %>% sa2_16_to_sa2_11(var = "trademarks")

  }

  tms %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(trademarks = sum(trademarks)) %>%
    dplyr::ungroup()

}
