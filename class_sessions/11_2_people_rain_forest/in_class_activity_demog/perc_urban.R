library(tidyverse)
library(readxl)
library(here)
library(viridis)

perc_urb <- read_excel(here("class_materials", "class_sessions", "/11_people_rain_forest", "WUP2018-F21-Proportion_Urban_Annual.xls"),
  sheet = "Data", skip = 16
)


rows <- nrow(perc_urb)
perc_urb <- perc_urb %>%
  slice_tail(n = rows - 12) %>%
  select(-Index, -Note) %>%
  rename("region" = "Region, subregion, country or area", "country_code" = "Country\ncode")

names(perc_urb)
region <- c(
  "AFRICA",
  "Eastern Africa",
  "Middle Africa",
  "Northern Africa",
  "Southern Africa",
  "Western Africa",
  "ASIA",
  "Eastern Asia",
  "South-Central Asia",
  "Central Asia",
  "Southern Asia",
  "South-Eastern Asia",
  "Western Asia",
  "EUROPE",
  "Eastern Europe",
  "Northern Europe",
  "Southern Europe",
  "Western Europe",
  "LATIN AMERICA AND THE CARIBBEAN",
  "Caribbean",
  "Central America",
  "South America",
  "NORTHERN AMERICA",
  "OCEANIA",
  "Australia/New Zealand",
  "Melanesia",
  "Micronesia",
  "Polynesia"
)

region <- as.data.frame(region)
perc_urb <- anti_join(perc_urb, region)

non_trop <- c(
  "Algeria",
  "Egypt",
  "Libya",
  "Morocco",
  "Sudan",
  "Tunisia",
  "Western Sahara",
  "Lesotho",
  "Swaziland",
  "Japan",
  "Mongolia",
  "Republic of Korea",
  "Kazakhstan",
  "Kyrgyzstan",
  "Tajikistan",
  "Turkmenistan",
  "Uzbekistan",
  "Afghanistan",
  "Bhutan",
  "Iran (Islamic Republic of)",
  "Nepal",
  "Pakistan",
  "Armenia",
  "Azerbaijan",
  "Bahrain",
  "Cyprus",
  "Georgia",
  "Iraq",
  "Israel",
  "Jordan",
  "Kuwait",
  "Lebanon",
  "Oman",
  "Qatar",
  "Saudi Arabia",
  "State of Palestine",
  "Syrian Arab Republic",
  "Turkey",
  "United Arab Emirates",
  "Yemen",
  "Belarus",
  "Bulgaria",
  "Czechia",
  "Hungary",
  "Poland",
  "Republic of Moldova",
  "Romania",
  "Russian Federation",
  "Slovakia",
  "Ukraine",
  "Channel Islands",
  "Denmark",
  "Estonia",
  "Faeroe Islands",
  "Finland",
  "Iceland",
  "Ireland",
  "Isle of Man",
  "Latvia",
  "Lithuania",
  "Norway",
  # "Sweden",
  "United Kingdom",
  "Albania",
  "Andorra",
  "Bosnia and Herzegovina",
  "Croatia",
  "Gibraltar",
  "Greece",
  "Holy See",
  "Italy",
  "Malta",
  "Montenegro",
  "Portugal",
  "San Marino",
  "Serbia",
  "Slovenia",
  # "Spain",
  "TFYR Macedonia",
  "Austria",
  # "Belgium",
  "France",
  "Germany",
  "Liechtenstein",
  "Luxembourg",
  "Monaco",
  "Netherlands",
  "Switzerland",
  "Argentina",
  "Chile",
  "Uruguay",
  "Bermuda",
  "Canada",
  "Greenland",
  "Saint Pierre and Miquelon",
  # "United States of America",
  "Australia",
  "New Zealand"
)

perc_urb <- perc_urb %>%
  rename_with(~ paste0("perc_urban_", .x), starts_with("19") | starts_with("20")) %>%
  rename("country" = "region")

non_trop <- as.data.frame(non_trop)
names(non_trop) <- c("country")

perc_urb <- anti_join(perc_urb, non_trop)

perc_urb$country <- tolower(perc_urb$country)

perc_urb_long <- perc_urb %>%
  pivot_longer(
    cols = starts_with("perc_urban_"),
    names_to = "year",
    names_prefix = "perc_urban_",
    values_to = "perc_urban",
    values_drop_na = TRUE
  )

perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country == "united states of america", "usa", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 638, "reunion island", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 834, "tanzania", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 384, "ivory coast", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 158, "taiwan", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 446, "macao", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 344, "hong kong", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 418, "laos", country))

perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 408, "north korea", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 626, "east timor", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 531, "curacao", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 850, "us virgin islands", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 68, "bolivia", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 238, "falkland islands", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 862, "venezuela", country))
perc_urb_long <- perc_urb_long %>% mutate(country = ifelse(country_code == 583, "federated states of micronesia", country))

perc_urb_long <- perc_urb_long %>% filter(country != "north korea")
perc_urb_long <- perc_urb_long %>% filter(country != "falnkland islands")
perc_urb_long <- perc_urb_long %>% filter(country != "tokelau")
perc_urb_long <- perc_urb_long %>% filter(country != "wallis and futuna islands")

perc_urb_long <- perc_urb_long %>%
  select(-country_code)

write_csv(perc_urb_long, here("class_materials", "class_sessions", "/11_people_rain_forest", "perc_urb_long.csv"))

# perc_urb_long<-as.data.frame(perc_urb_long)
perc_urb_wide <- perc_urb_long %>%
  pivot_wider(names_from = year, values_from = perc_urban, names_prefix = "yr_")
perc_urb_wide <- perc_urb_long %>%
  pivot_wider(names_from = year, values_from = perc_urban)
write_csv(perc_urb_wide, here("class_materials", "class_sessions", "/11_people_rain_forest", "perc_urb_wide.csv"))

perc_urb_long<-read_csv(here("class_sessions", "/11_2_people_rain_forest", "in_class_activity_demog","perc_urb_long.csv"))


perc_urb_long <- perc_urb_long %>%
  mutate(year = as.numeric(year))
unique(perc_urb_long$country)
data <- filter(perc_urb_long,
               country == "ecuador" |
                 country == "usa" |
                 country == "peru" |
                 country == "india" |
                 country == "malaysia" |
                 # country == "belgium" |
                 country == "rwanda" |
                 country == "congo" |
                 # country == "sweden" |
                 country == "indonesia" |
                 # country == "philippines" |
                 country == "brazil" |
                 # country == "dominican republic" |
                 # country == "united kingdom" |
                 country == "spain" |
                 country == "senegal" |
                 # country == "nigeria" |
                 country == "cambodia"
               )



shapes<-seq(1:n_distinct(data$country))
# data<-perc_urb_long
data
urban_plot <- ggplot(data, aes(
  x = year,
  y = perc_urban,
  group = country,
  color = country,
  shape=country
)) +
  geom_point(
  ) +
  scale_shape_manual(values = c(seq(1:n_distinct(data$country)))) +
  scale_color_viridis(discrete = TRUE, option = "viridis") +
  geom_line() +
  labs(
    x = "Year",
    y = "% of the Population"
  ) +
  geom_hline(
    yintercept = 50, linetype = "dashed",
    color = "gray", size = 1
  ) +
  # scale_x_continuous(n.breaks = 25)
  scale_x_continuous(breaks = seq(from = 1950, to = 2050, by = 2))
urban_plot
urban_plot + theme_classic() +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

# +
#   ggtitle("% of the Population Living in an Urban Setting")



# spag plot ---------------------------------------------------------------


tmp <- data %>%
  mutate(name2=country)

tmp %>%
  ggplot( aes(x=year, y=perc_urban)) +
  geom_line( data=tmp %>% dplyr::select(-country), aes(group=name2), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=country), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  geom_hline(
    yintercept = 50, linetype = "dashed",
    color = "gray", size = 1
  ) +
  # scale_x_continuous(n.breaks = 25)
  scale_x_continuous(breaks = seq(from = 1950, to = 2050, by = 10))+
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("Percent of population living in urban areas") +
  facet_wrap(~country)

n_countries<-perc_urb_long %>% summarize(n=n_distinct(country))

below50_subset_1950<-perc_urb_long %>% 
  filter(year==1950) %>% 
  filter(perc_urban<50)

(below50_subset_1950 %>% summarize(n=n_distinct(country)))/
n_countries*100

below50_all_2050<-perc_urb_long %>% 
  filter(year==2050) %>% 
  filter(perc_urban<50)


(below50_all_2050 %>% summarize(n=n_distinct(country)))/
  n_countries*100



below50_subset_2050<-data %>% 
  filter(year==2050) %>% 
  filter(perc_urban<50)


# How many countries


countries_2050<-perc_urb_long %>% filter(year=="2050") %>% filter(perc_urban<50) %>% summarise(n_distinct(country))