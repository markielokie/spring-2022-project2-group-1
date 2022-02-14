### This R file extracts COVID-19 case and death data from NY Times
### Data is accessed from https://github.com/nytimes/covid-19-data

# Load libraries
library(tidyverse)
library(zoo)

# Specify source link
urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

# Read latest data from source
dat = read_csv(url(urlfile))

# Filter data for New York City
# Create 7-day moving averages for cases and deaths
dat_ma = dat %>%
  filter(county == "New York City") %>%
  arrange(date) %>%
  mutate(cases_7dayMA = rollmean(cases, k=7, fill=NA),
         deaths_7dayMA = rollmean(deaths, k=7, fill=NA))

# Plot 7-day MA for cases and deaths
dat_ma %>%
  ggplot(aes(x=date, y=cases_7dayMA)) +
  geom_line(color="orange") +
  theme(legend.position="none") +
  geom_line(aes(x=date, y=deaths_7dayMA*30), color="red") +
  scale_y_continuous(
    labels=scales::comma,
    name="Cases",
    sec.axis=sec_axis(deaths_7dayMA~./30,
                      name="Deaths",
                      labels=scales::comma)
  ) +
  theme(
    axis.title.y=element_text(color="orange", size=13),
    axis.title.y.right=element_text(color="red", size=13),
    axis.text.x=element_text(size=6, angle=45, hjust=1),
    axis.text.y=element_text(size=6)
  ) +
  labs(
    title="Cumulative Cases & Deaths in NYC",
    subtitle="7-Day Moving Average",
    x="Date"
  ) 
