#' Create Trademarks IP data.
#'
#' `create_trademarks()` creates a data frame with the number of trademarks filed in a geographic area for a given year.
#'
#'
#' Only trademarks filed in Australia are included. This data is derived from the
#'  Intellectual Property Government Open Data, and includes:
#'  \itemize{
#'  \item{IPGOD 201 Trade Marks Summary}
#'  \item{IPGOD 202 Trade Marks Applicant Information}
#'  }
#'
#' More information about the IPGOD data is available through the
#' \href{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019/resource/8d2855ce-8e39-4bc0-9d6d-e19a4d9e2183}{IPGOD Data Dictionary.}
#'
#' @param year numeric. The year the trademarks were filed.
#' @param geography string. The geographic region of interest. Defaults to SA2.
#'
#' @return a data frame of trademarks by geography for a given year.
#' @export
#'
#' @examples
#' create_trademarks(2016, "SA2")
#'
create_trademarks <- function(year, geography = "sa2") {

  # The IPGOD dataset for 2019 uses the 2016 ASGS for naming SA2 Regions, even if they did not exist in 2011.

  if (!year %in% c(2011, 2016, 2021)) {
    warning("A non-census year was specified.
            Other indicators of innovation are only available for the census years 2011, 2016, and 2021.
            Proceed with caution. ")
  }

  if (year == 2011) {
    geog <- sa2_2011 %>%
      sf::st_drop_geometry()
  } else {
    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
  }

  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")


  tms <- trademarks %>%
    dplyr::filter(year == {{year}}) %>%
    dplyr::group_by(.data$sa2_name, year) %>%
    dplyr::summarise(trademarks = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup()

  if (year < 2016) {

    tms <- tms %>% sa2_16_to_sa2_11(var = "trademarks")

  }

  tms %>%
    dplyr::left_join(geog, by = geography) %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(trademarks = sum(trademarks)) %>%
    dplyr::ungroup()

}
