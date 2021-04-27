library(tidyverse)
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



