pow <- function(year, geography) {

  if (year == 2016) {

    data <- sa2_indp2_2016
    geog <- sa2_2016

  } else if (year == 2011) {

    data <- sa2_indp2_2011
    geog <- sa2_2011

  }

  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(employment = sum(employment))



}

