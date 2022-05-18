#' Industrial Comparative Advantage
#'
#' `ica()` creates a data frame of the industrial comparative advantages of Australian regions for 2011 and 2016.
#'
#'  The industrial comparative advantage data is derived from employment by industry data from the Australian Bureau of Statistics (ABS)
#'  Census based on the place that people work, rather than their place of residence. Industry of employment is measured at the
#'  2006 ANZSIC Subdivision. Employment by industry alone is not an adequate measure of industrial strength, because it is not clear where one region outperforms another, as
#'  total employment differs across regions. For example, Preschool and School education is the largest employing industry in more
#'  than 25\% of all regions. To accommodate this, the industrial comparative advantage uses a location quotient measure to identify
#'  in which industries a region employs a higher share than the Australian average.
#'
#'  Census data is randomised in cases where a small number of people work in a specific industry in a region.
#'  This is done to avoid the release of confidential data. As such, regions in which fewer than 150 people work, are excluded by default.
#'
#'
#'  See `vignette("industrial_growth_opportunities")` for more details.
#'
#'
#' @param year numeric. The year to calculate the industrial comparative advantage. Must be a Census year (2011, 2016, 2021).
#' @param geography string. The geography level to calculate the industrial comparative advantage. Defaults to SA2.
#' @param adjust logical. TRUE to remove industries classified as "not further defined"
#' @param ... Additional options passed to the calculation of the location quotient. See `location_quotient` for more information.
#'
#' @return a data frame of industrial comparative advantages.
#' @export
#' @examples
#' ica(year = 2016, geography = "sa3")
#'
#' # Remove industries classified as "not further defined"
#' ica(year = 2016, adjust = TRUE)
#'
#' # Increase the minimum regional employment
#' ica(year = 2016, min_value = 1000)
#'
#' # Remove industries which do not meet a minimum employment threshold
#' ica(year = 2016, total_var = "industry_employment", min_value = 2000)
#'
ica <- function(year, geography = "sa2", adjust = FALSE, ...) {

  stopifnot("Year must be one of 2011, 2016" = year %in% c(2011, 2016))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (year == 2011) {

    data <- sa2_indp2_2011
    geog <- sa2_2011 %>%
      sf::st_drop_geometry() %>%
      dplyr::select(sa2_name, sa3_name, sa4_name, gcc_name, state_name)

  } else if (year == 2016) {

    data <- sa2_indp2_2016
    geog <- sa2_2016 %>%
      sf::st_drop_geometry() %>%
      dplyr::select(sa2_name, sa3_name, sa4_name, gcc_name, state_name)
  }

  ellipse_arg <- list(...)


  if (isTRUE(adjust)) {

    data <- data %>%
      adjust_nfd(nfd_col = "industry",
                 nfd_level = "anzsic_subdivision",
                 anz = "anzsic")
    value_var <- "employment_adj"
    ellipse_arg$value_var <- "employment_adj"
  }



  if ("industry_employment" %in% ellipse_arg$total_var) {

    data <- data %>%
      dplyr::group_by(industry) %>%
      dplyr::mutate(industry_employment = sum(employment)) %>%
      dplyr::ungroup()
  }

  if ("sa2_employment" %in% ellipse_arg$total_var) {

    data <- data %>%
      dplyr::group_by(sa2_name) %>%
      dplyr::mutate(sa2_employment = sum(employment)) %>%
      dplyr::ungroup()
  }

  if (is.null(ellipse_arg$value_var)) {

    value_var <- "employment"
    ellipse_arg$value_var = "employment"
  }

  if (geography != "sa2_name") {
  data <- data %>%
    dplyr::left_join(geog, by = "sa2_name") %>%
    dplyr::group_by(.data[[geography]], industry) %>%
    dplyr::summarise(dplyr::across(.data[[ellipse_arg$value_var]], sum), .groups = "drop") %>%
    dplyr::ungroup()

  } else {
    data <- data
  }


  location_quotient(data, geography = {{geography}}, value_var = value_var) %>%
    dplyr::mutate(year = {{year}}) %>%
    dplyr::rename(ica = lq) %>%
    dplyr::filter(!is.nan(ica))


}
