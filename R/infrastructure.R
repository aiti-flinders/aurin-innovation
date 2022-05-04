create_infrastructure <- function(year, geography = "sa3_name") {

  if (year == 2011) {

    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE)
    data <- education_location

  } else if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE)
    data <- education_location
  }

  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(dplyr::across(c(unis, tafes), ~sum(.x, na.rm = TRUE))) %>%
    dplyr::ungroup()



}
