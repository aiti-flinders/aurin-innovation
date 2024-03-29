#' @importFrom rlang .data :=

check_sa2 <- function(.data, geography, other) {

  .data %>%
    dplyr::filter(!stringr::str_detect(.data[[geography]], "Migratory - Offshore - Shipping"),
      !stringr::str_detect(.data[[geography]], "POW No Fixed Address"),
      !stringr::str_detect(.data[[geography]], "POW not applicable"),
      !stringr::str_detect(.data[[geography]], "POW not stated"),
      !stringr::str_detect(.data[[geography]], "No usual address"),
      !stringr::str_detect(.data[[geography]], "POW State/Territory undefined"),
      !stringr::str_detect(.data[[geography]], "POW Capital city undefined"),
      .data[[geography]] != "Total",
      !.data[[other]] %in% c("Inadequately described",
        "Not stated",
        "Not applicable",
        "Total"))
}


sa2_16_to_sa2_11 <- function(data, var) {
  data %>%
    dplyr::left_join(sa2_2016_to_sa2_2011, by = c("sa2_name" = "sa2_name_2016")) %>%
    dplyr::group_by(.data$sa2_name_2011, .data$year) %>%
    dplyr::summarise(dplyr::across(var, ~ round(sum(ratio * .x)))) %>%
    dplyr::ungroup() %>%
    dplyr::rename(sa2_name = .data$sa2_name_2011)
}

sa2_21_to_sa2_16 <- function(data, var) {
  data %>%
    dplyr::left_join(sa2_2021_to_sa2_2016, by = c("sa2_name" = "sa2_name_2021")) %>%
    dplyr::group_by(.data$sa2_name_2016, .data$year) %>%
    dplyr::summarise(dplyr::across(var, ~ round(sum(ratio * .x)))) %>%
    dplyr::ungroup() %>%
    dplyr::rename(sa2_name = .data$sa2_name_2016)
}

kibs <- function() {
  k <- c(
    "Petroleum Exploration",
    "Mineral Exploration",
    "Commission-Based Wholesaling",
    "Antique and Used Goods Retailing",
    "Postal Services",
    "Courier Pick-up and Delivery Services",
    "Directory and Mailing List Publishing",
    "Software Publishing",
    "Music Publishing",
    "Music and Other Sound Recording Activities",
    "Internet Publishing and Broadcasting",
    "Wired Telecommunications Network Operation",
    "Other Telecommunications Network Operation",
    "Other Telecommunications Services",
    "Internet Service Providers and Web Search Portals",
    "Data Processing and Web Hosting Services",
    "Electronic Information Storage Services",
    "Other Information Services",
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_division   == "Financial and Insurance Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_subdivision == "Rental and Hiring Services (except Real Estate)") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Scientific Research Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Architectural, Engineering and Technical Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Legal and Accounting Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Advertising Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Market Research and Statistical Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Management and Related Consulting Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Other Professional, Scientific and Technical Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_subdivision == "Computer System Design and Related Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Employment Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(.data$anzsic_group == "Other Administrative Services") %>% dplyr::pull(.data$anzsic_class) %>% unique(),
    "Building and Other Industrial Cleaning Services",
    "Building Pest Control Services",
    "Packaging Services",
    "Investigation and Security Services",
    "Educational Support Services",
    "Electronic (except Domestic Appliance) and Precision Equipment Repair",
    "Photographic Film Processing")

  return(k)

}


adjust_nfd <- function(data, nfd_col, nfd_level, anz = c("anzsic", "anzsco")) {

  if (anz == "anzsic") {
    anz <- dplyr::distinct(strayr::anzsic2006, .data$anzsic_division, .data[[nfd_level]]) %>%
      dplyr::mutate(dplyr::across(.data[[nfd_level]], ~ stringr::str_remove_all(.x, "\\.")))
    fill_var <- "anzsic_division"
  } else {
    anz <- dplyr::distinct(strayr::anzsco2021, .data$anzsco_major, .data[[nfd_level]])
    fill_var <- "anzsco_major"
  }

  data %>%
    dplyr::left_join(anz, by = stats::setNames(nfd_level, nfd_col)) %>%
    tidyr::fill(.data[[fill_var]], .direction = "updown") %>%
    dplyr::group_by(.data$sa2_name, .data[[fill_var]]) %>%
    dplyr::mutate(employment_share = .data$employment / sum(.data$employment),
      nfd = ifelse(stringr::str_detect(.data[[nfd_col]], "nfd"), .data$employment, NA)) %>%
    tidyr::fill(.data$nfd, .direction = "down") %>%
    dplyr::mutate(employment_adj = round(.data$employment + (.data$employment_share * .data$nfd), digits = 0),
      employment_adj = ifelse(is.nan(.data$employment_adj), 0, .data$employment_adj)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(employment_adj = ifelse(stringr::str_detect(.data[[nfd_col]], "nfd"), 0, .data$employment_adj)) %>%
    dplyr::select(.data$sa2_name,
      {{nfd_col}},
      .data$employment,
      .data$employment_adj)
}



add_product_names <- function(data, digits) {

  if (!digits %in% c("four", "4", 4, "six", "6", 6)) {
    stop()
  } else if (digits %in% c("four", "4", 4)) {
    digits <- "4digit"
  } else {
    digits <- "6digit"
  }

  prod_data <- product_data %>%
    dplyr::filter(.data$level == digits) %>%
    dplyr::select(.data$hs_product_name_short_en,
      .data$hs_product_code)

  data %>%
    dplyr::left_join(prod_data, by = "hs_product_code")
}

add_sa2_codes <- function(data) {

  data_2011 <- data %>%
    dplyr::filter(year < 2016) %>%
    dplyr::left_join(sa2_2011, by = "sa2_name")

  data_2016 <- data %>%
    dplyr::filter(year >= 2016) %>%
    dplyr::left_join(sa2_2016, by = "sa2_name")

  dplyr::bind_rows(data_2011, data_2016) %>%
    dplyr::select(-c(geometry, cent_lat, cent_long, areasqkm, state_name, state_code, gcc_name, gcc_code,
      sa4_name, sa4_code, sa3_name, sa3_code, sa2_5dig))

}

