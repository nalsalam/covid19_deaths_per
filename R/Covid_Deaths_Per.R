# download data from:
# https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv

library("tidyverse")
library("lubridate")
library("ggrepel")

deaths_US_wide <- read_csv("data/time_series_covid19_deaths_US.csv")

deaths_US <-
  deaths_US_wide %>% select(Province_State, Admin2, Population, Lat, Long_, matches("/20$")) %>%
  pivot_longer(cols = matches("/20$"), names_to = "date", values_to = "deaths") %>%
  rename(date_chr = date, population = Population) %>%
  mutate(date = as_date(mdy(str_replace(date_chr, "(20$)", "20\\1")))) %>%
  mutate(
    state = case_when(
      Province_State == "District of Columbia" | 
      (Province_State == "Virginia" & Lat > 38.7 & Long_ > -77.7) |        
      (Province_State == "Maryland" & Lat > 38.4 & Lat < 39.5 & Long_ > -77.25 & Long_ < -76.5) ~ "DC-Area",
      Province_State == "New York" ~ "NY",
      Province_State == "New Jersey" ~ "NJ",
      Province_State == "Washington" ~ "WA",
      Province_State == "California" ~ "CA",
      Province_State == "Michigan" ~ "MI",
      Province_State == "Louisiana" ~ "LA",
      TRUE ~ Province_State
    ))

# Identify Counties in Northern VA
deaths_US %>% filter(Province_State == "Virginia", Lat > 38.7, Long_ > -77.7) %>%
  group_by(Admin2) %>% filter(row_number() == n()) %>% 
  ggplot(mapping = aes(x = Long_, y = Lat)) +
  geom_point() +
  geom_label_repel(mapping = aes(label = Admin2))

# Identify Counties near DC
deaths_US %>% filter(Province_State == "Maryland", Lat > 38.4, Lat < 39.5, Long_ > -77.25, Long_ < -76.5) %>%
  group_by(Admin2) %>% filter(row_number() == n()) %>% 
  ggplot(mapping = aes(x = Long_, y = Lat)) +
  geom_point() +
  geom_label_repel(mapping = aes(label = Admin2))


dat <-
  deaths_US %>%
  filter(state %in% c("DC-Area", "NY", "NJ", "WA", "CA", "MI", "LA")) %>%
  group_by(state, date) %>%
  summarize(
    deaths = sum(deaths),
    population = sum(population),
  ) %>%
  mutate(deaths_per = deaths / (population / 1e5)) %>% 
  filter(deaths_per > .1) %>%
  # filter(deaths > 10) %>%
  group_by(state) %>%
  mutate(days = date - date[1]) 

ggplot(data = dat,
       mapping = aes(x = days, y = deaths_per, color = state)) + 
  geom_line() + 
  geom_label(data = dat %>% filter(row_number() == n()),
             mapping = aes(label = state),
             nudge_x = 0, hjust = 0) +
  scale_y_continuous(trans = "log", breaks = c(.1, 1, 10, 100)) +
  labs(
    title = "Progression of COVID Daily Deaths Adjusted for Population",
    subtitle = "Deaths Per 100k of Population",
    y = NULL,
    x = "Days Since Deaths Per 100k Population Passed 0.1",
    caption = "Data from https://github.com/CSSEGISandData/COVID-19"
  ) +
  theme(legend.position = "none", plot.title.position = "plot") 

ggsave("output/Covid19_Deaths_Per.pdf", width = 6.5, height = 5)
