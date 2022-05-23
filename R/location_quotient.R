#' Location Quotient
#'
#' `location quotient()` calculates the location quotient for any data for geography based data, such as
#' revealed comparative advantage (RCA) from country-product-export data, or industrial comparative advantage (ICA) from
#' region-industry-employment data.
#'
#' This function can not be called explicitly. Assumes the input data contains region-industry-employment data.
#'
#'
#' @param data a data frame or data frame like object.
#' @param min_value numeric. The minimum value of `total_var` for inclusion in the location quotient calculation.
#' @param total_var string. The name of the variable whose values must be greater than the `min_value`.
#' @param y_var strin). The name of the non geography variable in the data. Defaults to 'industry'.
#' @param value_var string.  The name of the variable which contains the values for the geography and `y_var` pairs. Defaults to employment.
#' @param geography string. The region to calculate the location quotient.
#'
#' @keywords Internal
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
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
    stop("One or more of the variables specified (geography, y_var, or value_var) were not found in the data.")
  }

  if (!is.null(min_value) & !is.null(total_var)) {

    data <- data %>%
      dplyr::filter(.data[[total_var]] >= min_value)

  }


  row_names <- data %>%
    dplyr::pull(geography) %>%
    unique()

  col_names <- data %>%
    dplyr::pull(y_var) %>%
    unique()

  data <- data %>%
    tidyr::pivot_wider(id_cols = {{geography}},
                       names_from = {{y_var}},
                       values_from = {{value_var}})

  data_array <- as.matrix(data[2:length(data)])



  lq <- t(t(data_array/rowSums(data_array)) / (colSums(data_array)/sum(data_array)))

  lq <- lq %>%
    tibble::as_tibble() %>%
    dplyr::mutate("{geography}" := row_names) %>%
    tidyr::pivot_longer(cols = -.data[[geography]],
                        names_to = "industry",
                        values_to = "lq")


  return(lq)

}
