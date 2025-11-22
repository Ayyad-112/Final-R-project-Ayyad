library(readxl)
library(dplyr)
library(janitor)
deaths <- read_excel("final_cleaned_deaths.xlsx") %>% clean_names()
user_types <- read_excel("final_cleaned_user_types.xlsx") %>% clean_names()
standards <- read_excel("final_cleaned_standards.xlsx") %>% clean_names()
deaths_clean <- deaths %>%
  remove_empty("cols") %>%     # remove empty columns
  remove_empty("rows") %>%     # remove empty rows
  mutate(across(where(is.character), trimws))  # clean spaces
user_types_clean <- user_types %>%
  remove_empty("cols") %>%
  remove_empty("rows") %>%
  mutate(across(where(is.character), trimws))
standards_clean <- standards %>%
  remove_empty("cols") %>%
  remove_empty("rows") %>%
  mutate(across(where(is.character), trimws))
standards_raw <- read_excel("final_cleaned_standards.xlsx") %>%
  clean_names()
# Columns now: location, dim1, value, fact_value_translation_id

standards_clean <- standards_raw %>%
  select(
    country = location,
    standard = dim1,
    status = value     # "Yes"/"No"
  )
# -> one row per (country, standard), status = Yes/No
user_types_raw <- read_excel("final_cleaned_user_types.xlsx") %>%
  clean_names()
# Columns: location, period, is_latest_year, dim1, fact_value_numeric, value

user_types_clean <- user_types_raw %>%
  select(
    country = location,
    year = period,
    road_user_type = dim1,
    percent = fact_value_numeric
  ) %>%
  mutate(
    year = as.integer(year),
    percent = as.numeric(percent)
  )


deaths_raw <- read_excel("final_cleaned_deaths.xlsx") %>%
  clean_names()
# Columns: location, fact_value_numeric, fact_value_numeric_low,
#          fact_value_numeric_high, value

deaths_clean <- deaths_raw %>%
  select(
    country = location,
    deaths = fact_value_numeric,
    deaths_ci_low = fact_value_numeric_low,
    deaths_ci_high = fact_value_numeric_high
  ) %>%
  mutate(
    deaths = as.numeric(deaths),
    deaths_ci_low = as.numeric(deaths_ci_low),
    deaths_ci_high = as.numeric(deaths_ci_high)
  )

glimpse(standards_clean)
glimpse(user_types_clean)
glimpse(deaths_clean)
# I jsut did this to make its working
standards_clean <- standards_clean %>%
  filter(!is.na(country), !is.na(standard), !is.na(status)) %>%
  mutate(
    country = trimws(country),
    standard = trimws(standard),
    status = trimws(status),
    status = factor(status, levels = c("No", "Yes"))
  ) %>%
  arrange(country, standard)

user_types_clean <- user_types_clean %>%
  filter(!is.na(country), !is.na(road_user_type)) %>%
  mutate(
    country = trimws(country),
    road_user_type = trimws(road_user_type),
    year = as.integer(year),
    percent = as.numeric(percent)
  ) %>%
  filter(!is.na(percent), percent >= 0, percent <= 100) %>%
  arrange(country, year, road_user_type)

deaths_clean <- deaths_clean %>%
  mutate(
    country = trimws(country),
    deaths = as.numeric(deaths),
    deaths_ci_low = as.numeric(deaths_ci_low),
    deaths_ci_high = as.numeric(deaths_ci_high)
  ) %>%
  filter(!is.na(country), !is.na(deaths), deaths >= 0) %>%
  arrange(desc(deaths)) %>%
  mutate(rank_deaths = row_number())
## GG plot time? 
library(ggplot2)
# Top 10 countries by deaths
deaths_clean %>%
  arrange(desc(deaths)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, deaths), y = deaths)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 countries by number of road traffic deaths",
    x = "Country",
    y = "Estimated number of deaths"
  )