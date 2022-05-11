#' Create Infrastructure Data
#'
#' `create_infrastructure()` creates a data frame containing the number of universities and TAFE campuses by a geographic area for
#' a given year.
#'
#' Currently only point data from 2018 is available. It is assumed that physical locations are constant between 2011 and 2016 with
#' the only differences arising due to changes in geographic boundaries. This will likely change in future releases if additional location
#' data becomes available.
#'
#' @param year numeric. The year of the geographic boundaries.
#' @param geography string. The geographic region of interest. Defaults to SA2.
#'
#' @return A data frame with the number of university and tafe locations by a geographic area.
#' @export
#'
#' @examples
#' create_infrastructure(2011)
create_infrastructure <- function(year, geography = "sa2") {

  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (year == 2011) {

    geog <- sa2_2011 %>%
      sf::st_drop_geometry()
    data <- education_location_2011

  } else if (year == 2016) {

    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
    data <- education_location_2016

  }



  data %>%
    dplyr::left_join(geog, by = geography) %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(dplyr::across(c(unis, tafes), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::ungroup()



}
