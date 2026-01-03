library(readr)
library(dplyr)
library(janitor)
library(stringr)
carac_raw <- read_csv("caracteristiques-2019.csv", show_col_types = FALSE) %>% clean_names()
veh_raw   <- read_csv("vehicules-2019.csv", show_col_types = FALSE) %>% clean_names()
usa_raw   <- read_csv("usagers-2019.csv", show_col_types = FALSE) %>% clean_names()
cat("carac rows:", nrow(carac_raw), "cols:", ncol(carac_raw), "\n")
cat("veh rows:", nrow(veh_raw), "cols:", ncol(veh_raw), "\n")
cat("usa rows:", nrow(usa_raw), "cols:", ncol(usa_raw), "\n")
carac_raw %>% select(num_acc, jour, mois, an, hrmn, lum, dep, com, agg) %>% glimpse()
veh_raw   %>% select(num_acc, id_vehicule, catv) %>% glimpse()
usa_raw   %>% select(num_acc, id_vehicule, grav, catu, sexe, secu1) %>% glimpse()
carac_clean <- carac_raw %>%
  transmute(
    num_acc,
    dep,
    agg,
    lum,
    jour,
    mois,
    an,
    hrmn
  )
veh_clean <- veh_raw %>%
  transmute(
    num_acc,
    id_vehicule,
    catv
  )

usa_clean <- usa_raw %>%
  transmute(
    num_acc,
    id_vehicule,
    grav,
    catu,
    sexe,
    secu1
  )
carac_clean <- carac_clean %>%
  mutate(
    area = case_when(
      agg == 1 ~ "Rural (outside built-up)",
      agg == 2 ~ "Urban (built-up)",
      TRUE ~ NA_character_
    ),
    light = case_when(
      lum == 1 ~ "Day",
      lum == 2 ~ "Dawn/Dusk",
      lum %in% c(3,4,5) ~ "Night",
      TRUE ~ NA_character_
    ),
    # hrmn in this dataset is a fraction of the day (0-1). We'll keep it numeric.
    hrmn = as.numeric(hrmn),
    hour = if_else(!is.na(hrmn), floor(hrmn * 24), NA_real_),
    time_of_day = case_when(
      hour >= 6  & hour < 12 ~ "Morning",
      hour >= 12 & hour < 18 ~ "Afternoon",
      hour >= 18 & hour < 22 ~ "Evening",
      hour >= 22 | hour < 6  ~ "Night",
      TRUE ~ NA_character_
    )
  )

veh_clean <- veh_clean %>%
  mutate(
    catv = str_pad(as.character(catv), 2, pad = "0"),
    vehicle_group = case_when(
      catv %in% c("01", "80") ~ "Bicycle / e-bike",
      catv %in% c("02","30","31","32","33","34","41","42","43") ~ "Motorcycle / scooter",
      catv == "07" ~ "Car",
      catv == "10" ~ "Van / light truck",
      catv %in% c("13","14","15","16","17") ~ "Heavy vehicle",
      catv %in% c("37","38") ~ "Bus / coach",
      TRUE ~ "Other/Unknown"
    ),
    motorization = if_else(vehicle_group == "Bicycle / e-bike", "Non-motorized", "Motorized"),
    vehicle_mass = if_else(vehicle_group %in% c("Heavy vehicle","Bus / coach"), "Heavy", "Light")
  )

usa_clean <- usa_clean %>%
  mutate(
    severity = case_when(
      grav == 1 ~ "Unharmed",
      grav == 2 ~ "Killed",
      grav == 3 ~ "Hospitalized",
      grav == 4 ~ "Slight injury",
      TRUE ~ NA_character_
    ),
    severe = case_when(
      grav %in% c(2,3) ~ 1L,
      grav %in% c(1,4) ~ 0L,
      TRUE ~ NA_integer_
    ),
    user_role = case_when(
      catu == 1 ~ "Driver",
      catu == 2 ~ "Passenger",
      catu == 3 ~ "Pedestrian",
      TRUE ~ NA_character_
    ),
    sex = case_when(
      sexe == 1 ~ "Male",
      sexe == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    safety_equipment_1 = case_when(
      secu1 == 0 ~ "No equipment",
      secu1 == 1 ~ "Seatbelt",
      secu1 == 2 ~ "Helmet",
      secu1 == 3 ~ "Child restraint",
      secu1 == 4 ~ "Reflective vest",
      secu1 == 5 ~ "Airbag (2/3W)",
      secu1 == 6 ~ "Gloves (2/3W)",
      secu1 == 7 ~ "Gloves + Airbag",
      secu1 == 9 ~ "Other",
      TRUE ~ "Unknown/Not specified"
    )
  )

cat("\n--- Simple checks ---\n")

carac_clean %>%
  count(area, light, sort = TRUE) %>%
  print(n = 10)

veh_clean %>%
  count(vehicle_group, sort = TRUE) %>%
  print(n = 10)

usa_clean %>%
  count(severity, user_role, sort = TRUE) %>%
  print(n = 10)

#  SAVE CLEAN TABLES 
write_csv(carac_clean, "carac_clean_step1.csv")
write_csv(veh_clean,   "veh_clean_step1.csv")
write_csv(usa_clean,   "usa_clean_step1.csv")