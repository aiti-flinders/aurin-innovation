#' Create Plant Breeder Rights IP data.
#'
#' `create_plants()` creates a data frame with the number of plant breeder rights applied for in a
#' geographic area for a given year.
#'
#' Only Plant Breeder Rights lodged in Australia are included. This data
#' is derived from the Intellectual Property Government Open Data, and includes:
#' \itemize{
#' \item{IPGOD 401 Plant Breeder's Rights Summary}
#' \item{IPGOD 402 Plant Breeder's Rights Applicant Information}
#' }
#'
#' More information about the IPGOD data is available through the
#' \href{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019/resource/8d2855ce-8e39-4bc0-9d6d-e19a4d9e2183}{IPGOD Data Dictionary.}
#'
#' @param year numeric. The year the plant breeder right application was submitted.
#' @param geography string. The geographic region of interest. Defaults to SA2
#'
#' @return A data frame of plant breeder rights by geography for a given year.
#' @export
#'
#' @examples
#' create_plants(2016, "SA2")

create_plants <- function(year, geography = "sa2") {

  if (!year %in% c(2011, 2016, 2021)) {
    warning("A non-census year was specified.
            Other indicators of innovation are only available for the census years 2011, 2016, and 2021.
            Proceed with caution.")
  }

  if (year == 2011) {
    geog <- sa2_2011 %>%
      sf::st_drop_geometry()
  } else if (year == 2016) {
    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
  } else if (year == 2021) {
    geog <- sa2_2021 %>%
      sf::st_drop_geometry()
  }

  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  p <- plants_sa2 %>%
    dplyr::filter(year %in% ({{year}}-3):{{year}}) %>%
    dplyr::rename(plants = n) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(plants = mean(.data$plants)) %>%
    dplyr::mutate(year = {{year}}) %>%
    dplyr::ungroup()

  if (year == 2011) {
    p <- p %>% sa2_16_to_sa2_11(var = "plants")
  } else if (year == 2016) {
    p <- p %>% sa2_21_to_sa2_16(var = "plants")
  }

  p %>%
    dplyr::left_join(geog, by = "sa2_name") %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(plants = sum(.data$plants, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup()

}
