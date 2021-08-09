#' Calculate location quotient
#' Data should be a data.frame in a wide format. See the
#' included `sa2_indp2` data for an example.
#'
#' @description
#' @param data
#'
#' @return
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#'
#' @examples
location_quotient <- function(data) {

  #Strip the column containing the sa2_names ...
  row_names <- dplyr::pull(sa2_indp2, sa2_name_2016)

  data <- data.matrix(data[1:nrow(data), 2:length(data)])


  lq <- t(t(data/colSums(data)) / (rowSums(data)/sum(data)))

  lq <- lq %>%
    tibble::as_tibble() %>%
    dplyr::mutate(sa2_name_2016 = row_names) %>%
    tidyr::pivot_longer(cols = -sa2_name_2016,
                        names_to = "industry",
                        values_to = "rca")

  return(lq)

}
