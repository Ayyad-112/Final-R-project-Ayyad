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
# now for shiny! maybe?
install.packages("shiny")
library(shiny)
# ============================
# SHINY APP STARTS HERE
# ============================

ui <- fluidPage(
  titlePanel("Global Public Health: Road Traffic Injuries"),
  
  tabsetPanel(
    # ------------------------
    # TAB 1: Road Traffic Deaths
    # ------------------------
    tabPanel("Deaths",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "top_n",
                   "Show top N countries by deaths:",
                   min = 5,
                   max = 30,
                   value = 10,
                   step = 1
                 ),
                 sliderInput(
                   "min_deaths",
                   "Minimum deaths to include:",
                   min = 0,
                   max = max(deaths_clean$deaths, na.rm = TRUE),
                   value = 0,
                   step = 1000
                 ),
                 textInput(
                   "country_search",
                   "Filter by country name (contains):",
                   value = ""
                 )
               ),
               mainPanel(
                 h3("Countries with highest road traffic deaths"),
                 plotOutput("deaths_plot"),
                 br(),
                 tableOutput("deaths_table")
               )
             )
    ),
    
    # ------------------------
    # TAB 2: Road User Types
    # ------------------------
    tabPanel("Road User Types",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "ut_country",
                   "Select country:",
                   choices = sort(unique(user_types_clean$country)),
                   selected = sort(unique(user_types_clean$country))[1]
                 ),
                 sliderInput(
                   "ut_year",
                   "Select year:",
                   min = min(user_types_clean$year, na.rm = TRUE),
                   max = max(user_types_clean$year, na.rm = TRUE),
                   value = max(user_types_clean$year, na.rm = TRUE),
                   step = 1,
                   sep = ""
                 ),
                 checkboxGroupInput(
                   "ut_types",
                   "Select road user types:",
                   choices = sort(unique(user_types_clean$road_user_type)),
                   selected = sort(unique(user_types_clean$road_user_type))
                 )
               ),
               mainPanel(
                 h3("Distribution of road traffic deaths by road user type"),
                 plotOutput("user_types_plot"),
                 br(),
                 tableOutput("user_types_table")
               )
             )
    ),
    
    # ------------------------
    # TAB 3: Vehicle Standards
    # ------------------------
    tabPanel("Vehicle Standards",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "vs_countries",
                   "Select country/countries:",
                   choices = sort(unique(standards_clean$country)),
                   selected = sort(unique(standards_clean$country))[1],
                   multiple = TRUE
                 ),
                 checkboxGroupInput(
                   "vs_status",
                   "Include standards with status:",
                   choices = c("Yes", "No"),
                   selected = c("Yes", "No")
                 ),
                 textInput(
                   "vs_search",
                   "Filter standards by keyword (optional):",
                   value = ""
                 )
               ),
               mainPanel(
                 h3("Vehicle safety standards"),
                 plotOutput("standards_plot"),
                 br(),
                 tableOutput("standards_table")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # TAB 1: Deaths
  deaths_filtered <- reactive({
    df <- deaths_clean
    
    df <- df %>%
      filter(deaths >= input$min_deaths)
    
    if (input$country_search != "") {
      pattern <- tolower(input$country_search)
      df <- df %>%
        filter(grepl(pattern, tolower(country)))
    }
    
    df <- df %>%
      arrange(desc(deaths)) %>%
      slice_head(n = input$top_n)
    
    df
  })
  
  output$deaths_plot <- renderPlot({
    df <- deaths_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(country, deaths), y = deaths)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top countries by number of road traffic deaths",
        x = "Country",
        y = "Estimated number of deaths"
      )
  })
  
  output$deaths_table <- renderTable({
    deaths_filtered()
  })
  
  # TAB 2: Road User Types
  user_types_filtered <- reactive({
    user_types_clean %>%
      filter(
        country == input$ut_country,
        year == input$ut_year,
        road_user_type %in% input$ut_types
      )
  })
  
  output$user_types_plot <- renderPlot({
    df <- user_types_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = road_user_type, y = percent)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Road traffic deaths by road user type -",
                      input$ut_country, input$ut_year),
        x = "Road user type",
        y = "Percentage of deaths"
      )
  })
  
  output$user_types_table <- renderTable({
    user_types_filtered()
  })
  
  # TAB 3: Vehicle Standards
  standards_filtered <- reactive({
    df <- standards_clean %>%
      filter(country %in% input$vs_countries,
             status %in% input$vs_status)
    
    if (input$vs_search != "") {
      pattern <- tolower(input$vs_search)
      df <- df %>%
        filter(grepl(pattern, tolower(standard)))
    }
    
    df
  })
  
  output$standards_plot <- renderPlot({
    df <- standards_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = standard, fill = status)) +
      geom_bar() +
      coord_flip() +
      labs(
        title = "Vehicle safety standards by status",
        x = "Standard",
        y = "Count of entries",
        fill = "Status"
      )
  })
  
  output$standards_table <- renderTable({
    standards_filtered()
  })
}

shinyApp(ui, server)

