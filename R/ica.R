#' Industrial Comparative Advantage
#'
#' @param years Which year to calculate the industrial comparative advantage. The default, "all", will calculate the
#' industrial comparative advantage for all available years. Available years are 2011 and 2016.
#' @param ... Additional options passed to the calculation of the location quotient. See `location_quotient` for more information.
#'
#' @return
#' @export
#'
#' @examples
ica <- function(years = "all", geography = "sa2_name", ...) {

  years <- as.numeric(years)

  if (years == 2011) {

    pow <- sa2_indp2_2011

  } else if (years == 2016) {

    pow <- sa2_indp2_2016
  }

  ellipse_arg <- list(...)

  if ("employment_adj" %in% ellipse_arg$value_var) {

    pow <- pow %>%
      adjust_nfd(nfd_col = "industry",
                 nfd_level = "anzsic_subdivision",
                 anz = "anzsic")
  }

  if ("industry_employment" %in% ellipse_arg$total_var) {

    pow <- pow %>%
      dplyr::group_by(industry) %>%
      dplyr::mutate(industry_employment = sum(employment)) %>%
      dplyr::ungroup()
  }

  if ("sa2_employment" %in% ellipse_arg$total_var) {

    pow <- pow %>%
      dplyr::group_by(sa2_name) %>%
      dplyr::mutate(sa2_employment = sum(employment)) %>%
      dplyr::ungroup()
  }

  if (is.null(ellipse_arg$value_var)) {

    value_var <- "employment"
    ellipse_arg$value_var = "employment"
  }
  pow <- pow %>%
    dplyr::left_join(strayr::read_absmap("sa22016", remove_year_suffix = TRUE)) %>%
    dplyr::group_by(.data[[geography]], industry) %>%
    dplyr::summarise(dplyr::across(.data[[ellipse_arg$value_var]], sum)) %>%
    dplyr::ungroup()




  location_quotient(pow, ...) %>%
    dplyr::mutate(year = {{years}}) %>%
    dplyr::rename(ica = lq)


}
