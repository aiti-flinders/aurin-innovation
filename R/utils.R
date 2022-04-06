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
    strayr::anzsic2006 %>% filter(anzsic_division   == "Financial and Insurance Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_subdivision == "Rental and Hiring Services (except Real Estate)") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Scientific Research Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Architectural, Engineering and Technical Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Legal and Accounting Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Advertising Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Market Research and Statistical Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Management and Related Consulting Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Other Professional, Scientific and Technical Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_subdivision == "Computer System Design and Related Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Employment Services") %>% pull(anzsic_class) %>% unique(),
    strayr::anzsic2006 %>% filter(anzsic_group == "Other Administrative Services") %>% pull(anzsic_class) %>% unique(),
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

adjust_nfd <- function(data) {

  anz <- dplyr::distinct(strayr::anzsic2006, anzsic_division, anzsic_subdivision)

  data %>%
    dplyr::left_join(anz,
                     by = c("industry_2" = "anzsic_subdivision")) %>%
    tidyr::fill(anzsic_division, .direction = "up") %>%
    dplyr::group_by(sa2_name, anzsic_division) %>%
    dplyr::mutate(employment_share = employment / sum(employment),
                  nfd = ifelse(stringr::str_detect(industry_2, "nfd"), employment, NA)) %>%
    tidyr::fill(nfd, .direction = "down") %>%
    dplyr::mutate(employment_adj = round(employment + (employment_share * nfd), digits = 0),
                  employment_adj = ifelse(is.nan(employment_adj), 0, employment_adj)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(employment_adj = ifelse(stringr::str_detect(industry_2, "nfd"), 0, employment_adj)) %>%
    dplyr::select(sa2_name,
                  industry_2,
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

