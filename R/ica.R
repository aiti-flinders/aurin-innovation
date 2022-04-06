#' Industrial Comparative Advantage
#'
#' @param years Which year to calculate the industrial comparative advantage. The default, "all", will calculate the
#' industrial comparative advantage for all available years. Available years are 2011 and 2016.
#' @param ... Additional options passed to the calculation of the location quotient. See ?location_quotient for more information.
#'
#' @return
#' @export
#'
#' @examples
ica <- function(years = "all", ...) {

  years <- as.numeric(years)

  if (years == 2011) {

    pow <- sa2_indp2_2011

  } else if (years == 2016) {

    pow <- sa2_indp2_2016
  }

  location_quotient(pow, ...) %>%
    dplyr::mutate(year = {{years}}) %>%
    dplyr::rename(ica = lq)


}
