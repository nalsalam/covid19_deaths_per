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

# US Mean
deaths_US_mean <-
  deaths_US %>%
  filter(!str_detect(state, "Princess")) %>% # no population data
  filter(date == mdy("4/10/2020")) %>%
  # group_by(state) %>%
  summarize(deaths = sum(deaths),
            population = sum(population)
  ) %>%
  mutate(deaths_per = deaths / (population / 1e5)) 

# Current Death Rate by State
day <- mdy("4/10/2020")
dat2 <-
  deaths_US %>%
  filter(date == day) %>%
  filter(!str_detect(state, "Princess")) %>% # no population data
  mutate(state = if_else(state %in% c("NY", "NJ", "WA", "LA", "MI", "CA"), Province_State, state)) %>%
  mutate(state = if_else(state %in% c("Virginia", "Maryland"), paste("Rest of", state), state)) %>%
  group_by(state) %>%
  summarize(
    deaths = sum(deaths),
    population = sum(population),
  ) %>%
  mutate(deaths_per = deaths / (population / 1e5)) %>% 
  group_by(state) %>% summarize(deaths_per = max(deaths_per)) %>% 
  ungroup() %>% arrange(desc(deaths_per))

ggplot(data = dat2,
       mapping = aes(x = reorder(state, deaths_per), y = deaths_per, 
                     fill = state == "DC-Area")) +
  geom_col() + 
  geom_hline(yintercept = deaths_US_mean$deaths_per) +
  coord_flip() + 
  labs(
    title = paste("Deaths Per 100k of Population in the US on", day),
    x = NULL, y = NULL,
    caption = "Data from https://github.com/CSSEGISandData/COVID-19"
  ) +
  scale_fill_manual(values = c("gray", "blue")) +
  scale_y_continuous(breaks = c(0, round(deaths_US_mean$deaths_per, 1), 10, 20, 30)) +
  theme(legend.position = "none")

ggsave("output/covid19_deaths_per_today.pdf", width = 6.5, height = 8)

