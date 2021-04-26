library(tidyverse)
library(glue)
library(sf)
library(osmdata)
library(gganimate)
library(showtext)

font_add_google(name = "Pathway Gothic One", family = "Pathway Gothic One")
font_add_google(name = "PT Sans", family = "PT Sans")
showtext::showtext_auto()

path = "C:\\RScripts\\BAC@MC 2021\\BAC-MC-2021-Phase2\\Data"

red = '#EE2737'

theme_arg = function(){
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = '#121212'),
        panel.grid.major = element_line(color = 'gray30'),
        panel.grid.minor = element_line(color = 'gray30'),
        axis.text = element_text(color = 'gray50'), 
        panel.border = element_blank(),
        text = element_text(family = 'PT Sans', size = 20),
        plot.title = element_text(family = 'Pathway Gothic One', 
                                  color = 'gray80', 
                                  size = 40, 
                                  lineheight = 3.5),
        plot.margin = margin(1,0.5,0.5,0.5, unit = 'in'),
        axis.title = element_text(family = 'Pathway Gothic One', 
                                  color = 'gray80', 
                                  size = 30, 
                                  lineheight = 2.5),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(lineheight = 3, hjust = ))
}
# COVID Time Series -------------------------------------------------------

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
  theme_arg() +
  transition_reveal(date)
animate(ta, height = 5, width = 7, nframes = 500, fps = 20, units = 'in', res = 500)
anim_save("test.gif")

jhu = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv') %>% 
  filter(Province_State == 'New York') %>% 
  filter(Admin2 %in% c("Bronx", 'Queens', 'Kings', 'Richmond', 'New York')) %>% 
  select(Admin2, `1/22/20`:last_col()) %>% 
  pivot_longer(`1/22/20`:last_col(), names_to = 'date')
