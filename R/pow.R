pow <- function(year, geography) {

  if (year == 2016) {

    data <- sa2_indp2_2016
    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE)

  } else if (year == 2011) {

    data <- sa2_indp2_2011
    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE)

  }

  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(employment = sum(employment))



}

