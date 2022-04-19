check_sa2 <- function(.data, geography, other) {

  .data %>%
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


read_abs_table_builder <- function(path, skip, n_max, year = NULL) {

  readr::read_csv(file = path,
                  skip = {{skip}},
                  n_max = {{n_max}}) %>%
    select(2:4) %>%
    mutate(year = {{year}})


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
    strayr::anzsic2006 %>% dplyr::filter(anzsic_division   == "Financial and Insurance Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_subdivision == "Rental and Hiring Services (except Real Estate)") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Scientific Research Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Architectural, Engineering and Technical Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Legal and Accounting Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Advertising Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Market Research and Statistical Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Management and Related Consulting Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Other Professional, Scientific and Technical Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_subdivision == "Computer System Design and Related Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Employment Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% dplyr::filter(anzsic_group == "Other Administrative Services") %>% dplyr::pull(anzsic_class) %>% unique(),
    "Building and Other Industrial Cleaning Services",
    "Building Pest Control Services",
    "Packaging Services",
    "Investigation and Security Services",
    "Educational Support Services",
    "Electronic (except Domestic Appliance) and Precision Equipment Repair",
    "Photographic Film Processing")

  return(k)

}

sa2_to_sa3 <- function(sa2_data, two) {

  sa2 <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
    sf::st_drop_geometry(x = .)

  sa2_data %>%
    dplyr::right_join(sa2, by = c("sa2_name")) %>%
    dplyr::group_by(sa3_name, {{two}}) %>%
    dplyr::summarise(across(where(is.double), ~sum(.x)), .groups = "drop")

}

adjust_nfd <- function(data, nfd_col, nfd_level, anz = c("anzsic", "anzsco")) {

  if (anz == "anzsic") {
    anz <- dplyr::distinct(strayr::anzsic2006, anzsic_division, .data[[nfd_level]])
    fill_var <- "anzsic_division"
  } else {
    anz <- dplyr::distinct(strayr::anzsco2021, anzsco_major, .data[[nfd_level]])
    fill_var <- "anzsco_major"
  }

  data %>%
    dplyr::left_join(anz, by = setNames(nfd_level, nfd_col)) %>%
    tidyr::fill(.data[[fill_var]], .direction = "up") %>%
    dplyr::group_by(sa2_name, .data[[fill_var]]) %>%
    dplyr::mutate(employment_share = employment / sum(employment),
                  nfd = ifelse(stringr::str_detect(.data[[nfd_col]], "nfd"), employment, NA)) %>%
    tidyr::fill(nfd, .direction = "down") %>%
    dplyr::mutate(employment_adj = round(employment + (employment_share * nfd), digits = 0),
                  employment_adj = ifelse(is.nan(employment_adj), 0, employment_adj)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(employment_adj = ifelse(stringr::str_detect(.data[[nfd_col]], "nfd"), 0, employment_adj)) %>%
    dplyr::select(sa2_name,
                  {{nfd_col}},
                  employment,
                  employment_adj)
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
    dplyr::filter(level == digits) %>%
    dplyr::select(hs_product_name_short_en, hs_product_code)

  data %>%
    dplyr::left_join(prod_data, by = "hs_product_code")
}
