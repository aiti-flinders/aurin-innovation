#' Create Regional Knowledge Intensive Business Services Data
#'
#' `create_kibs()` creates a data frame of the proportion of workers in a geographic area who are employed in a knowledge intensive business service industry.
#'
#' Knowledge Intensive Business Services are defined based on the list provided in the New Zealand Government
#' \href{https://www.mbie.govt.nz/assets/bd287dd4f2/Knowledge-intensive-services-report.pdf}{Knowledge Intensive Services Report}
#'
#'
#' @param year numeric. The year to create the regional knowledge intensive business services data. Must be a census year (2011, 2016).
#' @param geography string. The geography level to create the regional knowledge intensive business services data. Defaults to SA2.
#' @param adjust logical. TRUE to remove industries classified as "not further defined"
#'
#' @return A data frame of the proportion of workers in a geographic area who are employed in knowledge intensive business service industry.
#' @export
#'
#' @examples
#' create_kibs(2016)
create_kibs <- function(year, geography = "sa2", adjust = FALSE) {

  stopifnot("Year must be one of 2011, 2016, or 2021" = year %in% c(2011, 2016, 2021))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (year == 2021) {
    geog <- sa2_2021 %>%
      sf::st_drop_geometry()
    data <- sa2_indp4_2021
  }

  else if (year == 2016) {

    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
    data <- sa2_indp4_2016


  } else if (year == 2011) {
      geog <- sa2_2011 %>%
        sf::st_drop_geometry()
      data <- sa2_indp4_2011
  }

  if (adjust) {

    data <- data %>%
      adjust_nfd(nfd_col = "anzsic_class",
                 nfd_level = "anzsic_class",
                 anz = "anzsic") %>%
      dplyr::select(.data$sa2_name,
                    .data$anzsic_class,
                    employment = .data$employment_adj)

  } else {

    data <- data

  }

  employment_kibs <- data %>%
    dplyr::left_join(geog, by = "sa2_name") %>%
    dplyr::group_by(kibs = .data$anzsic_class %in% kibs(),
                    .data[[geography]]) %>%
    dplyr::summarise(kibs_employment = sum(.data$employment), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$kibs, names_prefix = "kibs", values_from = .data$kibs_employment) %>%
    dplyr::mutate(kibs = .data$kibsTRUE, #/ (.data$kibsFALSE + .data$kibsTRUE),
                  year = {{year}}) %>%
    dplyr::select(geography,
                  .data$kibs,
                  .data$year)



  return(employment_kibs)


}
