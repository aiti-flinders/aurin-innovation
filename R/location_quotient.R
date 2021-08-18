#' Location Quotient
#'
#' @description Calculate location quotient from any data which contains a column
#' specifying a geographic variable, a second variable (???) and a value
#' (this is not written well).
#'
#' @param data a dataframe
#' @param options a list of options.
#' Available options include "min_value", "x", "y", and "value".
#' "min_value" specifies the smallest total value allowable in a geographic
#' region.
#' "x" specifies the name of the x variable in the dataframe
#' "y" specifies the name of the y variable in the dataframe
#' "value" specifies the name of the value variable in the dataframe
#'
#' @return tibble
#' @export location_quotient
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#'
#' @examples \dontrun{
#' location_quotient(sa2_indp2, options = list("min_value" = 100))
#' }
location_quotient <- function(data, options = list("min_value" = 150,
                                                 "x" = "sa2_name_2016",
                                                 "y" = "industry_2",
                                                 "value" = "employment")) {

  if (!all(options$x %in% colnames(data),
           options$y %in% colnames(data),
           options$value %in% colnames(data))) {
    stop("A nice stop message about conforming names in supplied data")
  }

  if ("min_value" %in% names(options)) {

    data <- data %>%
      dplyr::filter(.data$region_employment >= options$min_value)

  }

  if (any(data$region_employment == 0) & options$min_value == 0) {
    warning("Some of the values you specified are zero for a geographic unit. Output is unreliable")
  }


  data_array <- reshape2::acast(data, list(options$x,options$y), value.var = options$value)

  #Strip the column containing the sa2_names ...
  row_names <- rownames(data_array)
  col_names <- colnames(data_array)


  lq <- t(t(data_array/colSums(data_array)) / (rowSums(data_array)/sum(data_array)))

  lq <- lq %>%
    tibble::as_tibble() %>%
    dplyr::mutate(sa2_name_2016 = row_names) %>%
    tidyr::pivot_longer(cols = -.data$sa2_name_2016,
                        names_to = "industry",
                        values_to = "rca")


  return(lq)

}
