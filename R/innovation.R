#' Title
#'
#' @param data
#' @param model
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
regional_innovation <- function(year, geography, ...) {

  scale <- function(x) {
    (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  }

  data <- sem_data(year, geography, ...)

  sem_model <- data %>%
    dplyr::left_join(pow({{year}}, {{geography}})) %>%
    dplyr::filter(employment >= 150) %>%
    dplyr::mutate(infrastructure = unis + tafes) %>%
    dplyr::mutate(dplyr::across(c(patents, designs, trademarks, plants), ~.x / (1000 * employment)),
                  dplyr::across(c(-year, -geography), ~scale(.x)))

  if (is.null(model)) {

  model <- "F1 =~ skill + rank + kibs
  F2 =~ backwards_citations + patents + plants + trademarks
  F3 =~ 1*F1  + 1*F2
  F3 ~~ F3
  rank ~~ skill"

  }

  fit <- lavaan::cfa(model, sem_model, std.lv = TRUE)

  innovation <- sem_model %>%
    dplyr::mutate(as.data.frame(lavaan::predict(fit)))

  return(innovation)

}
