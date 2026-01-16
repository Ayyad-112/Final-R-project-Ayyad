library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)
library(scales)
library(shiny)
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
#  NOW THE NEW CLEANED FILES! LETS HOPE IT WORKS
carac_clean <- read_csv("carac_clean_step1.csv", show_col_types = FALSE) %>% clean_names()
veh_clean   <- read_csv("veh_clean_step1.csv", show_col_types = FALSE) %>% clean_names()
usa_clean   <- read_csv("usa_clean_step1.csv", show_col_types = FALSE) %>% clean_names()
#STANDARDIZE JOIN KEYS (Joining 2 folders!!!)
carac_clean <- carac_clean %>%
  mutate(num_acc = as.character(num_acc))

veh_clean <- veh_clean %>%
  mutate(num_acc = as.character(num_acc),
         id_vehicule = as.character(id_vehicule))

usa_clean <- usa_clean %>%
  mutate(num_acc = as.character(num_acc),
         id_vehicule = as.character(id_vehicule))
# 2) Join users to crash context (ALWAYS works via num_acc)
user_context <- usa_clean %>%
  left_join(
    carac_clean %>% select(num_acc, dep, area, light, time_of_day),
    by = "num_acc"
  )

# 3) Join vehicle info WHEN available (via num_acc + id_vehicule)
# Pedestrians may have missing/odd id_vehicule -> they will stay, with NA vehicle fields.
user_level <- user_context %>%
  left_join(
    veh_clean %>% select(num_acc, id_vehicule, vehicle_group, motorization, vehicle_mass),
    by = c("num_acc", "id_vehicule")
  )

# 4) Create a flag to show whether vehicle info was successfully linked
user_level <- user_level %>%
  mutate(
    vehicle_linked = if_else(is.na(vehicle_group), "Not linked (e.g., pedestrian/unknown)", "Linked")
  )

# 5) Basic quality checks (very useful for report)
cat("\n--- Join checks ---\n")
cat("Total users:", nrow(user_level), "\n")
cat("Vehicle linked:", sum(user_level$vehicle_linked == "Linked", na.rm = TRUE), "\n")
cat("Not linked:", sum(user_level$vehicle_linked != "Linked", na.rm = TRUE), "\n\n")

# How many pedestrians are not linked?
user_level %>%
  count(user_role, vehicle_linked) %>%
  arrange(desc(n)) %>%
  print(n = 50)

# 6) Save the final joined dataset for analysis / Shiny
write_csv(user_level, "baac2019_user_level_joined.csv")

glimpse(user_level)
user_level <- read_csv("baac2019_user_level_joined.csv", show_col_types = FALSE)
user_level <- user_level %>%
  mutate(severe = as.integer(severe))
agg_vehicle <- user_level %>%
  group_by(vehicle_group, area, light) %>%
  summarise(
    n_users = n(),
    severe_n = sum(severe, na.rm = TRUE),
    severe_rate = severe_n / n_users,
    .groups = "drop"
  ) %>%
  arrange(desc(severe_rate), desc(n_users))

write_csv(agg_vehicle, "agg_vehicle_area_light.csv")

# Plot 1: vehicle vs severe rate (filtered later in Shiny)
ggplot(agg_vehicle, aes(x = vehicle_group, y = severe_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Severe injury rate by vehicle group (overall, grouped by area & light in data)",
    x = "Vehicle group",
    y = "Severe injury rate"
  )

# -------------------------
# B) User role + time of day table
# -------------------------
agg_role_time <- user_level %>%
  group_by(user_role, time_of_day, area) %>%
  summarise(
    n_users = n(),
    severe_n = sum(severe, na.rm = TRUE),
    severe_rate = severe_n / n_users,
    .groups = "drop"
  ) %>%
  arrange(desc(severe_rate), desc(n_users))

write_csv(agg_role_time, "agg_role_time_area.csv")

# Plot 2: role × time of day severe rate
ggplot(agg_role_time, aes(x = time_of_day, y = severe_rate, fill = user_role)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Severe injury rate by time of day and user role",
    x = "Time of day",
    y = "Severe injury rate",
    fill = "User role"
  )

# -------------------------
# C) Department risk table (for ranking / map later)
# -------------------------
agg_dep <- user_level %>%
  group_by(dep) %>%
  summarise(
    n_users = n(),
    severe_n = sum(severe, na.rm = TRUE),
    severe_rate = severe_n / n_users,
    .groups = "drop"
  ) %>%
  arrange(desc(severe_rate))

write_csv(agg_dep, "agg_dep_risk.csv")

# Plot 3: top 15 departments by severe rate (with minimum sample size)
agg_dep_filtered <- agg_dep %>% filter(n_users >= 500) %>% slice_head(n = 15)

ggplot(agg_dep_filtered, aes(x = reorder(dep, severe_rate), y = severe_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Top 15 departments by severe injury rate (n >= 500)",
    x = "Department",
    y = "Severe injury rate"
  )

# -------------------------
# D) Safety equipment vs severity (motorized only)
# -------------------------
agg_safety <- user_level %>%
  filter(motorization == "Motorized") %>%
  group_by(vehicle_group, safety_equipment_1) %>%
  summarise(
    n_users = n(),
    severe_n = sum(severe, na.rm = TRUE),
    severe_rate = severe_n / n_users,
    .groups = "drop"
  ) %>%
  filter(n_users >= 200) %>%
  arrange(desc(severe_rate))

write_csv(agg_safety, "agg_safety_motorized.csv")
agg_vehicle <- read_csv("agg_vehicle_area_light.csv", show_col_types = FALSE)
agg_role_time <- read_csv("agg_role_time_area.csv", show_col_types = FALSE)
agg_safety <- read_csv("agg_safety_motorized.csv", show_col_types = FALSE)

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  titlePanel("Road Traffic Injury Severity in France (2019): An Interactive visualization"),
  
  tabsetPanel(
    # =========================
    # TAB 1: Vehicle Risk
    # =========================
    tabPanel("Vehicle Risk",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "vr_area",
                   "Area:",
                   choices = sort(unique(agg_vehicle$area)),
                   selected = sort(unique(agg_vehicle$area))[1]
                 ),
                 checkboxGroupInput(
                   "vr_light",
                   "Light conditions:",
                   choices = sort(unique(agg_vehicle$light)),
                   selected = unique(agg_vehicle$light)
                 ),
                 selectInput(
                   "vr_sort",
                   "Sort bars by:",
                   choices = c("Severe injury rate" = "severe_rate", "Sample size (n)" = "n_users"),
                   selected = "severe_rate"
                 )
               ),
               mainPanel(
                 h4("Severe injury rate by vehicle group"),
                 plotOutput("plot_vehicle", height = 450),
            
               )
             )
    ),
    
    # =========================
    # TAB 2: Role & Time
    # =========================
    tabPanel("Role & Time",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "rt_area",
                   "Area:",
                   choices = sort(unique(agg_role_time$area)),
                   selected = sort(unique(agg_role_time$area))[1]
                 ),
                 checkboxGroupInput(
                   "rt_roles",
                   "User role:",
                   choices = c("Driver", "Passenger", "Pedestrian"),
                   selected = intersect(
                     c("Driver", "Passenger", "Pedestrian"),
                     unique(agg_role_time$user_role)
                   )
                 ),
                 checkboxGroupInput(
                   "rt_time",
                   "Time of day:",
                   choices = c("Morning", "Afternoon", "Evening", "Night"),
                   selected = intersect(
                     c("Morning", "Afternoon", "Evening", "Night"),
                     unique(agg_role_time$time_of_day)
                   )
                 )
               ),
               mainPanel(
                 h4("Severe injury rate by time of day and user role"),
                 plotOutput("plot_role_time", height = 450),
                
               )
             )
    ),
    
    # =========================
    # TAB 3: Safety Equipment
    # =========================
    tabPanel("Safety Equipment",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "se_vehicle_group",
                   "Vehicle group:",
                   choices = sort(unique(agg_safety$vehicle_group)),
                   selected = if ("Motorcycle / scooter" %in% agg_safety$vehicle_group)
                     "Motorcycle / scooter"
                   else sort(unique(agg_safety$vehicle_group))[1]
                 ),
                 checkboxGroupInput(
                   "se_equipment",
                   "Safety equipment:",
                   choices = sort(unique(agg_safety$safety_equipment_1)),
                   selected = unique(agg_safety$safety_equipment_1)
                 )
               ),
               mainPanel(
                 h4("Severe injury rate by safety equipment (motorized users)"),
                 plotOutput("plot_safety", height = 450),
               
               )
             )
    )
  )
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  # ---- TAB 1 ----
  vehicle_filtered <- reactive({
    agg_vehicle %>%
      filter(
        area == input$vr_area,
        light %in% input$vr_light
      )
  })
  
  output$plot_vehicle <- renderPlot({
    df <- vehicle_filtered()
    req(nrow(df) > 0)
    
    sort_col <- input$vr_sort
    df <- df %>% arrange(desc(.data[[sort_col]]))
    
    ggplot(df, aes(
      x = reorder(vehicle_group, .data[[sort_col]]),
      y = severe_rate,
      fill = vehicle_group
    )) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Vehicle group", y = "Severe injury rate") +
      guides(fill = "none") +
      theme_minimal()
  })
  
  output$table_vehicle <- renderTable({
    vehicle_filtered() %>%
      arrange(desc(severe_rate)) %>%
      mutate(severe_rate = percent(severe_rate, accuracy = 0.1))
  })
  
  # ---- TAB 2 ----
  role_time_filtered <- reactive({
    agg_role_time %>%
      filter(
        area == input$rt_area,
        user_role %in% input$rt_roles,
        time_of_day %in% input$rt_time
      ) %>%
      mutate(
        time_of_day = factor(time_of_day, levels = c("Morning", "Afternoon", "Evening", "Night")),
        user_role = factor(user_role, levels = c("Driver", "Passenger", "Pedestrian"))
      )
  })
  
  output$plot_role_time <- renderPlot({
    df <- role_time_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = time_of_day, y = severe_rate, fill = user_role)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Time of day", y = "Severe injury rate", fill = "User role") +
      theme_minimal()
  })
  
  output$table_role_time <- renderTable({
    role_time_filtered() %>%
      arrange(desc(severe_rate)) %>%
      mutate(severe_rate = percent(severe_rate, accuracy = 0.1))
  })
  
  # ---- TAB 3 ----
  safety_filtered <- reactive({
    agg_safety %>%
      filter(
        vehicle_group == input$se_vehicle_group,
        safety_equipment_1 %in% input$se_equipment
      ) %>%
      arrange(desc(severe_rate))
  })
  
  output$plot_safety <- renderPlot({
    df <- safety_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(
      x = reorder(safety_equipment_1, severe_rate),
      y = severe_rate,
      fill = safety_equipment_1
    )) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Safety equipment", y = "Severe injury rate") +
      guides(fill = "none") +
      theme_minimal()
  })
  
  output$table_safety <- renderTable({
    safety_filtered() %>%
      mutate(severe_rate = percent(severe_rate, accuracy = 0.1))
  })
}

shinyApp(ui, server)