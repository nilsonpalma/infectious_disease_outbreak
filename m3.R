library(tidyverse)
library(readr)
library(janitor)

capop <- read_csv("outbreak_data/ca_pop_2023.csv")
simca <- read_csv("outbreak_data/sim_novelid_CA.csv")
simla <- read_csv("outbreak_data/sim_novelid_LACounty.csv")

simca <- simca %>% clean_names()
simla <- simla %>% clean_names()

# Standardize naming and select for columns
simca1 <- simca %>% 
  select(
    county,
    age_cat,
    sex,
    race_eth = race_ethnicity,
    dt_diagnosis,
    time_int,
    new_infections,
    cumulative_infected,
    new_severe,
    cumulative_severe
  )
simla1 <- simla %>% 
  mutate(county = "Los Angeles") %>% 
  select(
    county,
    age_cat = age_category,
    sex,
    race_eth,
    dt_diagnosis = dt_dx,
    new_infections = dx_new,
    cumulative_infected = infected_cumulative,
    new_severe = severe_new,
    cumulative_severe = severe_cumulative
  )

simca1 <- simca1 %>% drop_na()
simla1 <- simla1 %>% drop_na()


str(simca1)
str(simla1)

simla1 <- simla1 %>%
  mutate(dt_diagnosis = dmy(dt_diagnosis))

?week

library(aweek)
set_week_start(7) # CDC Has a Sunday start date
## https://www.epirhandbook.com/en/new_pages/dates.html#dates_epi_wks

simca2 <- simca1 %>%
  mutate(
    year = substr(time_int, 1, 4),   # Extract 2023
    week = substr(time_int, 5, 6),    # Extract 22–52
    cdc_week = aweek::get_date(week = week, year = year)
  ) %>%
  select(-year, -week)

simla2 <- simla1 %>% 
  mutate(
    cdc_week = get_date(
      week = isoweek(dt_diagnosis),
      year = year(dt_diagnosis)
    )
  )

# Convert race numeric to named factor
simca3 <- simca2 %>%
  mutate(
    race_eth = recode_factor(race_eth,
                             `1` = "White, Non-Hispanic",
                             `2` = "Black, Non-Hispanic",
                             `3` = "American Indian or Alaska Native, Non-Hispanic",
                             `4` = "Asian, Non-Hispanic",
                             `5` = "Native Hawaiian or Pacific Islander, Non-Hispanic",
                             `6` = "Multiracial (two or more of above races), Non-Hispanic",
                             `7` = "Hispanic (any race)",
                             `9` = "Unknown"
    )
  ) %>% 
  select(-time_int)

# Convert race (already character) to factor
simla3 <- simla2 %>%
  mutate(race_eth = factor(race_eth))

sim_all <- bind_rows(simca3, simla3)
