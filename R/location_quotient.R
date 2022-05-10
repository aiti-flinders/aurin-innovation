#' Location Quotient
#'
#' Calculate location quotient from any data which contains a column
#' specifying a geographic variable, a second variable such as industry of employment, and a value. The data provided
#' is converted into a matrix with dimensions x_var * y_var. The variable specified as value_var fills the matrix.
#'
#'
#' Examples include:
#' Revealed Comparative Advantage from country-product-export data.
#' Industrial Comparative Advantage from region-industry-employment data.
#'
#' @param min_value numeric. the smallest total value allowable in a geographic region.
#' @param total_var character. the name of the variable which must be greater than the `min_value`. This variable
#' must be present in the data passed to the function.
#' @param x_var character. the name of the variable to be treated as rows.
#' @param y_var character. the name of the variable to be treated as columns.
#' @param value_var character. the name of the variable to be treated as the value.
#' @param data a data frame
#'
#' @return
#' @export location_quotient
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#'
#' @examples \dontrun{
#' location_quotient(sa2_indp2, options = list("min_value" = 100))
#' }
location_quotient <- function(data, min_value = 0, total_var = NULL, geography = NULL, y_var = NULL, value_var = NULL) {


  if (is.null(geography)) {
    geography <- "sa2_name"
  }

  if (is.null(y_var)) {
    y_var <- "industry"
  }

  if (is.null(value_var)) {
    value_var <- "employment"
  }


  if (!all(geography %in% colnames(data),
           y_var %in% colnames(data),
           value_var %in% colnames(data))) {
    stop("A nice stop message about conforming names in supplied data")
  }

  if (!is.null(min_value) & !is.null(total_var)) {

    data <- data %>%
      dplyr::filter(.data[[total_var]] >= min_value)

  }


#
#   if (any(data[[options$total]] == 0) & options$min_value == 0) {
#     warning("Some of the values you specified are zero for a geographic unit. Output is unreliable")
#   }


  data_array <- reshape2::acast(data, list(geography, y_var), value.var = value_var)

  #Strip the column containing the sa2_names ...
  row_names <- rownames(data_array)
  col_names <- colnames(data_array)


  lq <- t(t(data_array/rowSums(data_array)) / (colSums(data_array)/sum(data_array)))

  debug_x <- data_array/rowSums(data_array)
  debug_y <- (colSums(data_array)/sum(data_array))

  lq <- lq %>%
    tibble::as_tibble() %>%
    dplyr::mutate("{geography}" := row_names) %>%
    tidyr::pivot_longer(cols = -.data[[geography]],
                        names_to = "industry",
                        values_to = "lq")


  return(lq)

}
