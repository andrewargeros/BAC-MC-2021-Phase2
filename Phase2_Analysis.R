library(tidyverse)
library(magrittr)
library(readxl)
library(glue)
library(sf)
library(osmdata)
library(gganimate)
library(showtext)


# Setup / Plotting Functions ----------------------------------------------

font_add_google(name = "Pathway Gothic One", family = "Pathway Gothic One")
font_add_google(name = "PT Sans", family = "PT Sans")
showtext::showtext_auto()

path = "C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data"

red = '#EE2737'

theme_arg = function(factor = 1){
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = '#121212'),
        panel.grid.major = element_line(color = 'gray30'),
        panel.grid.minor = element_line(color = 'gray30'),
        axis.text = element_text(color = 'gray50'), 
        panel.border = element_blank(),
        text = element_text(family = 'PT Sans', size = factor*20, color = 'gray80'),
        plot.title = element_text(family = 'Pathway Gothic One', 
                                  color = 'gray80', 
                                  size = factor*40, 
                                  lineheight = 1.5),
        axis.title = element_text(family = 'Pathway Gothic One', 
                                  color = 'gray80', 
                                  size = factor*30, 
                                  lineheight = 2.5),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(lineheight = 1.5))
}

# COVID Time Series -------------------------------------------------------
### Slide 3

data = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  filter(state == 'New York')

data %>% 
  filter(county == 'New York City') %>% 
  ggplot() +
  aes(x = date, y = cases) +
  geom_area(size = 2, color = red, fill = red, alpha = 0.25) +
  geom_segment(aes(x = as.Date('2020-03-13'), xend = as.Date('2020-03-13'),
                   y = 5000, yend = 250000), 
               color = 'gray80', size = 2, lineend = 'round') +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'COVID-19 Cases in New York City',
       x = "Date",
       y = "COVID-19 Cases") +
  theme_arg() 
ggsave("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\COVID_Cases_by_day.png", height = 5, width = 7, units = 'in')

ta = data %>%
  mutate(date = as.Date(date)) %>% 
  filter(county == 'New York City') %>% 
  ggplot() +
  aes(x = date, y = cases) +
  geom_area(size = 2, color = red, fill = red, alpha = 0.25) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'COVID-19 Cases in New York City',
       x = "Date",
       y = "COVID-19 Cases",
       caption = '{frame_along}') +
  theme_arg(factor = 0.8) +
  transition_reveal(date)
animate(ta, height = 5, width = 7, nframes = 200, fps = 20, units = 'in', res = 100, renderer = gifski_renderer(loop = F))
anim_save("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\COVID_timeline.gif")


# COVID Cases by Borough --------------------------------------------------
### Slide 4

jhu = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv') %>% 
  filter(Province_State == 'New York') %>% 
  filter(Admin2 %in% c("Bronx", 'Queens', 'Kings', 'Richmond', 'New York')) %>% 
  select(Admin2, `1/22/20`:last_col()) %>% 
  pivot_longer(`1/22/20`:last_col(), names_to = 'date') %>% 
  mutate(date = as.Date(date, '%m/%d/%y'))

ta = jhu %>% 
  mutate(boro = case_when(Admin2 == 'Kings' ~ 'Brooklyn',
                          Admin2 == 'Richmond' ~ 'Staten Island',
                          Admin2 == 'New York' ~ 'Manhattan', 
                          T ~ as.character(Admin2))) %>% 
  ggplot() +
  aes(x = reorder(boro, value), xend = reorder(boro, value), y = 0, yend = value, group = boro) +
  geom_segment(size = 4, color = red, lineend = 'round') +
  scale_y_continuous(labels = scales::comma) +
  coord_flip(clip = 'off') +
  transition_time(date) +
  theme_arg(factor = 0.8) +
  labs(title = 'COVID-19 Cases Over Time',
       x = 'Borough',
       y = 'Cases',
       caption = '{frame_time}')
animate(ta, height = 5, width = 7, nframes = 200, fps = 20, units = 'in', res = 100, end_pause = 50)
anim_save("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\COVID_by_boro.gif")

# Linear Models and Analytics ---------------------------------------------
### Slide 5

test = read_xlsx("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data\\NYC-demographic-other-data.xlsx",
          sheet = 'population') %>%
  janitor::clean_names('snake') %>% 
  select(sub_borough_area, x2018) %>% 
  rename('population' = 2) %>% 
  inner_join(., read_xlsx("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data\\NYC-housing-data.xlsx",
                          sheet = 'homeowner income') %>% 
                janitor::clean_names('snake') %>% 
                select(sub_borough_area, x2018) %>% 
                rename('h_income' = 2),
             by = 'sub_borough_area') %>% 
  inner_join(., read_xlsx("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data\\NYC-housing-data.xlsx",
                          sheet = 'renter income') %>% 
               janitor::clean_names('snake') %>% 
               select(sub_borough_area, x2018) %>% 
               rename('r_income' = 2),
             by = 'sub_borough_area') %>%
  inner_join(., read_xlsx("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data\\NYC-housing-data.xlsx",
                          sheet = 'home ownership rate') %>% 
               janitor::clean_names('snake') %>% 
               select(sub_borough_area, x2018) %>% 
               rename('own_pct' = 2),
             by = 'sub_borough_area') %>%
  inner_join(., read_xlsx("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data\\sub_boro_cd_conversion.xlsx", 
                          sheet = 1) %>% 
                mutate(sub_borough_area = ifelse(is.na(sub_borough_area), census_name, sub_borough_area)),
             by = 'sub_borough_area') %>% 
  mutate(boro_code = strsplit(as.character(boro_cd), ", ")) %>% 
  unnest(boro_code) %>% 
  mutate(boro_code = as.numeric(boro_code)) %>% 
  inner_join(., read_sf("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Shapefiles\\zcta_borocd_merged.geojson") %>% 
                tibble() %>% 
                select(BoroCD, COVID_CASE_COUNT) %>%
                distinct() %>%
                group_by(BoroCD) %>%
                summarise(n = mean(COVID_CASE_COUNT, na.rm=T)),
             by = c('boro_code' = 'BoroCD')) %>% 
  mutate(nrate = n/population,
         income = own_pct*h_income + (1-own_pct)*r_income)

mod = lm(log(nrate) ~ log(income), data = test) %>% summary()

read_csv(glue('{path}/Borough_Density.csv')) %>% 
  inner_join(., read_csv('https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-boro.csv') %>% 
                  mutate(boro = ifelse(str_detect(BOROUGH_GROUP, 'Staten'), 
                                       'Staten Island', BOROUGH_GROUP))) %$% cor(ppl_per_hh, CASE_RATE)

# Open Street Maps --------------------------------------------------------
### Slide 6

nyc = getbb("New York City")

streets = nyc %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link",
                            'secondary')) %>%
  osmdata_sf()

st = c("motorway", "primary", "secondary", "tertiary", 
       "residential", "living_street","unclassified",
       "service", "footway")

hospitals = nyc %>% 
  opq() %>% 
  add_osm_feature(key = 'amenity', value = 'hospital') %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines, color = "gray70",
          size = .1, alpha = .8) +
  geom_sf(data = hospitals$osm_points, size = 3, color = red, alpha = 0.5) +
  coord_sf(xlim = nyc[1,], 
           ylim = nyc[2,],
           expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#121212'))
ggsave('C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\nyc_map.pdf', height = 7, width = 5, units = 'in')  


read_sf("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Shapefiles\\zcta_borocd_merged.geojson") %>% 
  st_join(., hospitals$osm_points) %>% 
  tibble() %>% 
  group_by(BoroCD) %>% 
  summarise(hosps = n_distinct(osm_id), 
            cov = mean(COVID_CASE_RATE)) %$% cor(hosps, cov)

vax = nyc %>% 
  opq() %>% 
  add_osm_feature(key = 'healthcare', value = 'vaccination_centre') %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines, color = "gray70",
          size = .1, alpha = .8) +
  geom_sf(data = vax$osm_points, size = 3, color = red, alpha = 0.5) +
  coord_sf(xlim = nyc[1,], 
           ylim = nyc[2,],
           expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#121212'))
ggsave('C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\nyc_vax_map.pdf', height = 7, width = 5, units = 'in') 

# Infection Rates ---------------------------------------------------------

read_sf("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Shapefiles\\zcta_borocd_merged.geojson") %>% 
  mutate(n = ntile(COVID_CASE_RATE, 4)) %>% 
  ggplot() +
  geom_sf(aes(fill = n), color = 'gray20', show.legend = F) +
  scale_fill_gradient(low = colorspace::darken(red, 0.95), high = red) +
  theme_arg() 
ggsave('C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\nyc_covdi_rate.png', height = 11, width = 8.5, units = 'in') 

# Vaccine Rate ------------------------------------------------------------

ta = read_csv(glue('{path}/vaccine_rates.csv')) %>% 
  mutate(date = as.Date(Date, '%m/%d/%Y')) %>% 
  ggplot() +
  aes(x = date, y = doses) +
  geom_area(color = red, fill = red, alpha = 0.25, size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'COVID-19 Vaccinations in New York City',
       x = "Date",
       y = "Vaccines Given",
       caption = '{frame_along}') +
  theme_arg(factor = 0.8) +
  transition_reveal(date)
animate(ta, height = 5, width = 7, nframes = 200, fps = 20, units = 'in', res = 100, renderer = gifski_renderer(loop = F))
anim_save("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\Vax_timeline.gif")

read_csv(glue('{path}/vaccine_rates.csv')) %>% 
  mutate(date = as.Date(Date, '%m/%d/%Y')) %>% 
  filter(date == max(date))

# Vax Hesitancy -----------------------------------------------------------

hesit = read_csv(glue('{path}/Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv')) %>% 
  janitor::clean_names('snake') %>%
  select(!c(state_boundary, geographical_point, county_boundary)) %>% 
  filter(state_code == 'NY') %>% 
  mutate(county = str_remove_all(county_name, ', New York$')) %>% 
  filter(county %in% c('Kings County', "Queens County", 'New York County', 'Bronx County', 'Richmond County')) %>% 
  mutate(total_hes = estimated_hesitant + estimated_strongly_hesitant, 
         boro = case_when(county == 'Kings County' ~ 3,
                          county == 'Richmond County' ~ 5,
                          county == 'New York County' ~ 1, 
                          county == 'Bronx County' ~ 2,
                          T ~ 4))

hesit %$% cor(total_hes, social_vulnerability_index_svi)

read_sf("C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Shapefiles\\zcta_borocd_merged.geojson") %>% 
  mutate(boro = str_extract(as.character(BoroCD), '^\\d{1}') %>% as.numeric()) %>% 
  inner_join(., hesit, by = 'boro') %>% 
  ggplot() +
  geom_sf(aes(fill = total_hes), color = 'gray20', show.legend = F) +
  scale_fill_gradient(low = colorspace::darken(red, 0.75), high = red) +
  theme_arg() +
  labs(title = 'Vaccination Hesitancy in New York City')
ggsave('C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\nyc_vaxhes_rate.png', height = 7, width = 7, units = 'in') 


# Reopening ---------------------------------------------------------------

data %>% 
  filter(county == 'New York City') %>% 
  left_join(., read_csv(glue('{path}/covid_milestones.csv')) %>% 
                filter(type == 2) %>% 
                filter(str_detect(milestone, 'Phase|phase|dining|Dining')), by = 'date') %>% 
  mutate(new_cases = cases-lag(cases)) %>% 
  ggplot() +
  aes(x = date, y = new_cases) +
  geom_line(color = red, size = 1.5) +
  geom_vline(aes(xintercept = date, linetype = factor(type)), 
             size = 2, color = '#9ad7f7', show.legend = F) +
  theme_arg(2) +
  labs(title = "New Cases in New York City",
       subtitle = "Blue Lines Represent Major Reopenings",
       x = "Date",
       y = "New Cases")
ggsave('C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Plots\\New_cases.png', height = 5, width = 7, units = 'in') 
