## code to prepare `kibs` dataset goes here
library(dplyr)
library(aitidata)

kibs <- c(
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
  anzsic %>% filter(division == "Financial and Insurance Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(subdivision == "Rental and Hiring Services (except Real Estate)") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Scientific Research Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Architectural, Engineering and Technical Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Legal and Accounting Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Advertising Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Market Research and Statistical Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Management and Related Consulting Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Other Professional, Scientific and Technical Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(subdivision == "Computer System Design and Related Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Employment Services") %>% pull(class) %>% unique(),
  anzsic %>% filter(group == "Other Administrative Services") %>% pull(class) %>% unique(),
  "Building and Other Industrial Cleaning Services",
  "Building Pest Control Services",
  "Packaging Services",
  "Investigation and Security Services",
  "Educational Support Services",
  "Electronic (except Domestic Appliance) and Precision Equipment Repair",
  "Photographic Film Processing")

sa2_indp4 <- vroom("data-raw/sa2_pow_indp4_2016.csv",
                   skip = 9,
                   col_select = c(sa2_name = `SA2 (POW)`,
                                  industry_4 = `INDP - 4 Digit Level`,
                                  employment = `Count`),
                   n_max = 1666952) %>%
  check_sa2(geography = "sa2_name",
            other = "industry_4")

employment_kibs <- sa2_indp4 %>%
  group_by(kibs = industry_4 %in% kibs,
           sa2_name) %>%
  mutate(employment = sum(employment)) %>%
  ungroup() %>%
  distinct(sa2_name, employment, kibs) %>%
  pivot_wider(names_from = kibs,
              names_prefix = "kibs",
              values_from = employment) %>%
  mutate(total_employment = kibsTRUE + kibsFALSE) %>%
  rename(non_kibs = kibsFALSE,
         kibs = kibsTRUE)




usethis::use_data(employment_kibs, overwrite = TRUE)
