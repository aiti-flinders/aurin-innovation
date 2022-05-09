## code to prepare `workforce_density` dataset goes here
sa2_size_2016 <- sa2_2016 %>%
  select(sa2_name, areasqkm) %>%
  sf::st_drop_geometry(x = .) %>%
  as_tibble()

sa2_density <- left_join(sa2_size_2016, working_population %>% filter(year == 2016)) %>%
  left_join(business_count) %>%
  mutate(employment_density = sqrt(region_employment / areasqkm),
         business_density = sqrt(business_count / areasqkm) )

usethis::use_data(sa2_density, overwrite = TRUE)
