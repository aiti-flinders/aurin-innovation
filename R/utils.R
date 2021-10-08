check_sa2 <- function(data, geography, other) {


  data %>%
    dplyr::filter(!stringr::str_detect(.data[[geography]], "Migratory - Offshore - Shipping"),
                  !stringr::str_detect(.data[[geography]], "POW No Fixed Address"),
                  !stringr::str_detect(.data[[geography]], "POW not applicable"),
                  !stringr::str_detect(.data[[geography]], "POW not stated"),
                  .data[[geography]] != "Total",
                  !.data[[other]] %in% c("Inadequately described",
                                "Not stated",
                                "Not applicable",
                                "Total"))
}
