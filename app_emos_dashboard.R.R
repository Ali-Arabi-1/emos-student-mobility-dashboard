
# ============================================================
# EMOS Advanced Shiny Dashboard
# International Student Mobility in Europe
# Eurostat + World Bank + OWID/V-Dem + Gravity Model + ML
# ============================================================

# install.packages(c(
#   "shiny", "shinydashboard", "ggplot2", "plotly", "dplyr", "DT",
#   "corrplot", "eurostat", "tidyr", "countrycode", "scales",
#   "WDI", "readr", "forecast", "plm", "randomForest", "broom",
#   "maps", "geosphere"
# ))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(corrplot)
library(eurostat)
library(tidyr)
library(countrycode)
library(scales)
library(WDI)
library(readr)
library(forecast)
library(plm)
library(randomForest)
library(broom)
library(maps)
library(geosphere)

# -------------------------
# Helper functions
# -------------------------

get_year_col <- function(df) {
  possible <- c("time", "TIME_PERIOD", "Time", "TIME", "year", "Year")
  found <- possible[possible %in% names(df)][1]
  if (is.na(found)) stop("No time/year column found.")
  found
}

get_value_col <- function(df) {
  possible <- c("values", "value", "OBS_VALUE", "Value")
  found <- possible[possible %in% names(df)][1]
  if (is.na(found)) stop("No value column found.")
  found
}

get_existing_col <- function(df, possible_cols) {
  found <- possible_cols[possible_cols %in% names(df)][1]
  if (is.na(found)) return(NA)
  found
}

get_hr_col <- function(df) {
  possible <- setdiff(names(df), c("Entity", "Code", "Year"))
  possible[1]
}

weighted_avg <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

median_impute <- function(x) {
  x[!is.finite(x)] <- NA
  if (all(is.na(x))) return(rep(0, length(x)))
  med <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med
  x
}

safe_log <- function(x) {
  log(ifelse(is.na(x) | x <= 0, NA, x))
}

empty_plotly <- function(message) {
  p <- ggplot() +
    annotate("text", x = 1, y = 1, label = message, size = 5) +
    theme_void()
  ggplotly(p)
}

model_metrics <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  actual <- actual[ok]
  predicted <- predicted[ok]
  if (length(actual) < 3) {
    return(data.frame(RMSE = NA, MAE = NA, R2 = NA))
  }
  data.frame(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = cor(actual, predicted)^2
  )
}

continent_colors <- c(
  "Africa" = "#E41A1C",
  "Americas" = "#984EA3",
  "Asia" = "#377EB8",
  "Europe" = "#4DAF4A",
  "Oceania" = "#FF7F00"
)

# -------------------------
# Country coordinates
# -------------------------

country_coords <- maps::world.cities %>%
  filter(capital == 1) %>%
  group_by(country.etc) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(iso3c = countrycode(country.etc, "country.name", "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  select(iso3c, capital_name = name, lat, long)

# -------------------------
# Load data
# -------------------------

load_data <- function() {

  mobile_raw <- get_eurostat("educ_uoe_mobs02", time_format = "num")

  year_col <- get_year_col(mobile_raw)
  value_col <- get_value_col(mobile_raw)

  origin_col <- get_existing_col(
    mobile_raw,
    c("c_origin", "origin", "citizen", "partner", "geo_origin")
  )

  if (is.na(origin_col)) stop("Origin country column was not found.")

  mobile <- mobile_raw %>%
    filter(sex == "T")

  if ("isced11" %in% names(mobile)) {
    mobile <- mobile %>%
      filter(grepl("5|6|7|8|ED5|ED6|ED7|ED8|ED5-8", isced11))
  }

  mobile <- mobile %>%
    rename(origin_code = all_of(origin_col)) %>%
    mutate(
      year = as.integer(.data[[year_col]]),
      students = as.numeric(.data[[value_col]]),

      destination_country = countrycode(geo, "eurostat", "country.name"),
      destination_continent = countrycode(geo, "eurostat", "continent"),
      origin_country = countrycode(origin_code, "eurostat", "country.name"),
      origin_continent = countrycode(origin_code, "eurostat", "continent"),

      origin_iso3 = countrycode(origin_code, "eurostat", "iso3c"),
      destination_iso3 = countrycode(geo, "eurostat", "iso3c")
    ) %>%
    filter(
      destination_continent == "Europe",
      !is.na(destination_country),
      !is.na(destination_continent),
      !is.na(origin_country),
      !is.na(origin_continent),
      !is.na(origin_iso3),
      !is.na(destination_iso3),
      origin_continent %in% names(continent_colors),
      !is.na(year),
      !is.na(students)
    ) %>%
    group_by(
      year,
      geo,
      destination_country,
      destination_continent,
      destination_iso3,
      origin_continent,
      origin_country,
      origin_iso3
    ) %>%
    summarise(students = sum(students, na.rm = TRUE), .groups = "drop")

  min_year <- min(mobile$year, na.rm = TRUE)
  max_year <- max(mobile$year, na.rm = TRUE)

  wb_data <- WDI(
    indicator = c(
      gdp_per_capita = "NY.GDP.PCAP.CD",
      unemployment = "SL.UEM.TOTL.ZS",
      youth_unemployment = "SL.UEM.1524.ZS",
      education_expenditure = "SE.XPD.TOTL.GD.ZS",
      population = "SP.POP.TOTL"
    ),
    start = min_year,
    end = max_year,
    extra = FALSE
  ) %>%
    transmute(
      iso3c = iso3c,
      year = year,
      gdp_per_capita = gdp_per_capita,
      unemployment = unemployment,
      youth_unemployment = youth_unemployment,
      education_expenditure = education_expenditure,
      population = population
    )

  human_rights_raw <- read_csv(
    "https://ourworldindata.org/grapher/human-rights-index-vdem.csv",
    show_col_types = FALSE
  )

  hr_value_col <- get_hr_col(human_rights_raw)

  human_rights <- human_rights_raw %>%
    transmute(
      iso3c = Code,
      year = Year,
      human_rights_index = as.numeric(.data[[hr_value_col]])
    ) %>%
    filter(!is.na(iso3c), !is.na(year), !is.na(human_rights_index))

  mobile <- mobile %>%
    left_join(
      wb_data %>%
        rename(
          origin_gdp = gdp_per_capita,
          origin_unemployment = unemployment,
          origin_youth_unemployment = youth_unemployment,
          origin_education_expenditure = education_expenditure,
          origin_population = population
        ),
      by = c("origin_iso3" = "iso3c", "year" = "year")
    ) %>%
    left_join(
      wb_data %>%
        rename(
          destination_gdp = gdp_per_capita,
          destination_unemployment = unemployment,
          destination_youth_unemployment = youth_unemployment,
          destination_education_expenditure = education_expenditure,
          destination_population = population
        ),
      by = c("destination_iso3" = "iso3c", "year" = "year")
    ) %>%
    left_join(
      human_rights %>% rename(origin_human_rights = human_rights_index),
      by = c("origin_iso3" = "iso3c", "year" = "year")
    ) %>%
    left_join(
      human_rights %>% rename(destination_human_rights = human_rights_index),
      by = c("destination_iso3" = "iso3c", "year" = "year")
    ) %>%
    left_join(
      country_coords %>%
        rename(origin_capital = capital_name, origin_lat = lat, origin_long = long),
      by = c("origin_iso3" = "iso3c")
    ) %>%
    left_join(
      country_coords %>%
        rename(destination_capital = capital_name, destination_lat = lat, destination_long = long),
      by = c("destination_iso3" = "iso3c")
    ) %>%
    rowwise() %>%
    mutate(
      distance_km = ifelse(
        !is.na(origin_long) & !is.na(origin_lat) &
          !is.na(destination_long) & !is.na(destination_lat),
        distHaversine(
          c(origin_long, origin_lat),
          c(destination_long, destination_lat)
        ) / 1000,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    mutate(
      gdp_gap = destination_gdp - origin_gdp,
      human_rights_gap = destination_human_rights - origin_human_rights,
      unemployment_gap = destination_unemployment - origin_unemployment,
      youth_unemployment_gap = destination_youth_unemployment - origin_youth_unemployment,
      education_expenditure_gap = destination_education_expenditure - origin_education_expenditure,

      origin_mobility_rate_100k = ifelse(
        !is.na(origin_population) & origin_population > 0,
        students / origin_population * 100000,
        NA_real_
      ),

      same_continent = ifelse(origin_continent == destination_continent, 1, 0),
      intra_europe = ifelse(origin_continent == "Europe", 1, 0),

      log_students = safe_log(students),
      log_distance = safe_log(distance_km),
      log_origin_gdp = safe_log(origin_gdp),
      log_destination_gdp = safe_log(destination_gdp),
      log_origin_population = safe_log(origin_population)
    )

  enrolled_raw <- get_eurostat("educ_uoe_enrt01", time_format = "num")
  year_col <- get_year_col(enrolled_raw)
  value_col <- get_value_col(enrolled_raw)

  enrolled <- enrolled_raw %>%
    filter(sex == "T")

  if ("isced11" %in% names(enrolled)) {
    enrolled <- enrolled %>%
      filter(grepl("5|6|7|8|ED5|ED6|ED7|ED8|ED5-8", isced11))
  }

  enrolled <- enrolled %>%
    mutate(
      year = as.integer(.data[[year_col]]),
      enrolled = as.numeric(.data[[value_col]]),
      destination_country = countrycode(geo, "eurostat", "country.name"),
      destination_continent = countrycode(geo, "eurostat", "continent")
    ) %>%
    filter(destination_continent == "Europe", !is.na(destination_country)) %>%
    group_by(year, geo, destination_country) %>%
    summarise(enrolled = sum(enrolled, na.rm = TRUE), .groups = "drop")

  graduates_raw <- get_eurostat("educ_uoe_grad02", time_format = "num")
  year_col <- get_year_col(graduates_raw)
  value_col <- get_value_col(graduates_raw)

  graduates <- graduates_raw %>%
    filter(sex == "T")

  if ("isced11" %in% names(graduates)) {
    graduates <- graduates %>%
      filter(grepl("5|6|7|8|ED5|ED6|ED7|ED8|ED5-8", isced11))
  }

  graduates <- graduates %>%
    mutate(
      year = as.integer(.data[[year_col]]),
      graduates = as.numeric(.data[[value_col]]),
      destination_country = countrycode(geo, "eurostat", "country.name"),
      destination_continent = countrycode(geo, "eurostat", "continent")
    ) %>%
    filter(destination_continent == "Europe", !is.na(destination_country)) %>%
    group_by(year, geo, destination_country) %>%
    summarise(graduates = sum(graduates, na.rm = TRUE), .groups = "drop")

  early_raw <- get_eurostat("edat_lfse_14", time_format = "num")
  year_col <- get_year_col(early_raw)
  value_col <- get_value_col(early_raw)

  early <- early_raw %>% filter(sex == "T")

  if ("age" %in% names(early)) early <- early %>% filter(age == "Y18-24")
  if ("unit" %in% names(early)) early <- early %>% filter(unit == "PC")

  early <- early %>%
    mutate(
      year = as.integer(.data[[year_col]]),
      dropout_rate = as.numeric(.data[[value_col]]) / 100,
      destination_country = countrycode(geo, "eurostat", "country.name"),
      destination_continent = countrycode(geo, "eurostat", "continent")
    ) %>%
    filter(destination_continent == "Europe", !is.na(destination_country)) %>%
    group_by(year, geo, destination_country) %>%
    summarise(dropout_rate = mean(dropout_rate, na.rm = TRUE), .groups = "drop")

  dropout_data <- enrolled %>%
    full_join(graduates, by = c("year", "geo", "destination_country")) %>%
    full_join(early, by = c("year", "geo", "destination_country")) %>%
    arrange(destination_country, year) %>%
    mutate(
      completion_rate = graduates / enrolled,
      estimated_dropouts = enrolled * dropout_rate
    )

  list(students = mobile, dropout = dropout_data)
}

euro_data <- load_data()

student_data <- euro_data$students
dropout_data <- euro_data$dropout

years <- sort(unique(student_data$year))

country_choices <- student_data %>%
  distinct(geo, destination_country) %>%
  arrange(destination_country)

country_vector <- setNames(country_choices$geo, country_choices$destination_country)

continents <- names(continent_colors)
continents <- continents[continents %in% sort(unique(student_data$origin_continent))]

# -------------------------
# UI
# -------------------------

ui <- dashboardPage(

  dashboardHeader(title = "International Student Mobility in Europe"),

  dashboardSidebar(

    selectInput(
      "destinationCountry",
      "Select European Destination Country",
      choices = country_vector,
      selected = "IT"
    ),

    sliderInput(
      "yearRange",
      "Select Year Range",
      min = min(years),
      max = max(years),
      value = c(min(years), max(years)),
      step = 1,
      sep = ""
    ),

    checkboxGroupInput(
      "continentInput",
      "Select Origin Continents",
      choices = continents,
      selected = continents
    ),

    selectizeInput(
      "originCountries",
      "Select Origin Countries",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Leave empty to include all selected continents",
        plugins = list("remove_button")
      )
    ),

    hr(),
    helpText("Research question: Which socio-economic and geographic factors explain international student mobility toward European universities?"),
    helpText("Data: Eurostat, World Bank, OWID/V-Dem")
  ),

  dashboardBody(

    tabsetPanel(

      tabPanel(
        "Overview",

        fluidRow(
          box(
            width = 12,
            title = "Research Question",
            h4("Which socio-economic and geographic factors explain international student mobility toward European universities?"),
            p("This project combines official statistics with socio-economic indicators, geographic distance, mobility rate, gravity-style modelling, machine learning, forecasting and country profiling.")
          )
        ),

        fluidRow(
          valueBoxOutput("totalStudents"),
          valueBoxOutput("rangeStudents"),
          valueBoxOutput("topOriginCountry")
        ),

        fluidRow(
          box(width = 12, title = "Student Mobility Trend", plotlyOutput("timePlot", height = 350))
        ),

        fluidRow(
          box(width = 6, title = "Origin Continent Distribution", plotlyOutput("piePlot")),
          box(width = 6, title = "Origin Continent Comparison", plotlyOutput("barPlot"))
        )
      ),

      tabPanel(
        "Migration Rate",

        fluidRow(
          valueBoxOutput("avgMobilityRate"),
          valueBoxOutput("topMobilityRateCountry"),
          valueBoxOutput("avgOriginPopulation")
        ),

        fluidRow(
          box(width = 12, title = "Origin Mobility Rate Trend per 100,000 Population", plotlyOutput("mobilityRateTrend", height = 400))
        ),

        fluidRow(
          box(width = 6, title = "Mobility Rate by Origin Country", plotlyOutput("mobilityRateCountryPlot", height = 400)),
          box(width = 6, title = "Students vs Origin Population", plotlyOutput("populationMobilityPlot", height = 400))
        ),

        fluidRow(
          box(width = 12, title = "Origin Mobility Rate Table", DTOutput("mobilityRateTable"))
        )
      ),

      tabPanel(
        "European Map",

        fluidRow(
          box(width = 12, title = "Incoming International Students by Destination Country", plotlyOutput("europeMap", height = 550))
        ),

        fluidRow(
          box(width = 12, title = "Destination Country Ranking", DTOutput("destinationRankingTable"))
        )
      ),

      tabPanel(
        "Migration Drivers",

        fluidRow(
          valueBoxOutput("avgGDPGap"),
          valueBoxOutput("avgRightsGap"),
          valueBoxOutput("avgDistance")
        ),

        fluidRow(
          box(width = 6, title = "GDP Gap Trend and Mobility", plotlyOutput("gdpGapPlot", height = 350)),
          box(width = 6, title = "Human Rights Gap Trend and Mobility", plotlyOutput("rightsGapPlot", height = 350))
        ),

        fluidRow(
          box(width = 6, title = "Distance vs Mobility", plotlyOutput("distancePlot", height = 350)),
          box(width = 6, title = "Youth Unemployment Gap vs Mobility", plotlyOutput("youthUnemploymentPlot", height = 350))
        ),

        fluidRow(
          box(width = 12, title = "Migration Drivers Table", DTOutput("driversTable"))
        )
      ),

      tabPanel(
        "Gravity Model",

        fluidRow(
          box(
            width = 12,
            title = "Gravity Model",
            p("Dependent variable: log(students)."),
            p("Explanatory variables: log(origin GDP), log(destination GDP), log(distance), log(origin population), human-rights gap, youth unemployment gap and education expenditure gap."),
            verbatimTextOutput("gravityModel")
          )
        ),

        fluidRow(
          box(width = 12, title = "Gravity Model Coefficients", DTOutput("gravityCoefTable"))
        )
      ),

      tabPanel(
        "Panel Regression",

        fluidRow(
          box(
            width = 12,
            title = "Fixed-effects Panel Regression",
            p("Dependent variable: log international students."),
            p("Fixed effects: origin country and year."),
            verbatimTextOutput("panelRegression")
          )
        ),

        fluidRow(
          box(width = 12, title = "Panel Dataset", DTOutput("panelDataTable"))
        )
      ),

      tabPanel(
        "Machine Learning",

        fluidRow(
          box(width = 12, title = "Random Forest Variable Importance", plotOutput("rfImportancePlot", height = 400))
        ),

        fluidRow(
          box(width = 6, title = "Actual vs Predicted", plotlyOutput("actualPredictedPlot", height = 350)),
          box(width = 6, title = "Model Performance", DTOutput("modelPerformanceTable"))
        )
      ),

      tabPanel(
        "Country Profiles",

        fluidRow(
          box(
            width = 12,
            title = "Origin Country Profiles",
            p("Countries are profiled using PCA and hierarchical clustering based on mobility volume, mobility rate, GDP gap, human-rights gap, youth unemployment gap and distance."),
            plotlyOutput("profilePlot", height = 450)
          )
        ),

        fluidRow(
          box(width = 12, title = "Country Profile Groups", DTOutput("profileTable"))
        )
      ),

      tabPanel(
        "Forecasting",

        fluidRow(
          box(width = 12, title = "ARIMA Forecast of Student Mobility", plotlyOutput("forecastPlot", height = 400))
        ),

        fluidRow(
          box(width = 12, title = "Forecast Table", DTOutput("forecastTable"))
        )
      ),

      tabPanel(
        "Dropout / Completion",

        fluidRow(
          valueBoxOutput("dropoutRate"),
          valueBoxOutput("completionRate"),
          valueBoxOutput("totalDropouts")
        ),

        fluidRow(
          box(width = 12, title = "Dropout Proxy and Completion Trend", plotlyOutput("dropoutTrend", height = 350))
        ),

        fluidRow(
          box(width = 6, title = "Graduates vs Estimated Early Leavers", plotlyOutput("comparisonBar")),
          box(width = 6, title = "Distribution", plotlyOutput("dropoutPie"))
        )
      ),

      tabPanel(
        "Policy Insights",

        fluidRow(
          box(width = 12, title = "Automatically Generated Policy Interpretation", htmlOutput("policyInsight"))
        ),

        fluidRow(
          box(width = 12, title = "Top Origin Countries", DTOutput("originCountryTable"))
        )
      ),

      tabPanel(
        "Methodology",

        fluidRow(
          box(
            width = 12,
            title = "Methodology",
            h4("Project title"),
            p("Explaining International Student Mobility in Europe: An Official Statistics and Socio-Economic Dashboard"),

            h4("Data sources"),
            tags$ul(
              tags$li("Eurostat: international student mobility, enrolment, graduates and early leavers."),
              tags$li("World Bank: GDP per capita, unemployment, youth unemployment, education expenditure and population."),
              tags$li("Our World in Data / V-Dem: Human Rights Index."),
              tags$li("Maps/geographic coordinates: capital-city coordinates for distance approximation.")
            ),

            h4("Migration-rate indicator"),
            p("Origin Mobility Rate per 100,000 population = international students from the origin country / origin population × 100,000."),

            h4("Country profiles"),
            p("Country profiles are created with PCA plus hierarchical clustering. This is more stable and interpretable than simple k-means on raw variables."),

            h4("Methods"),
            tags$ul(
              tags$li("Descriptive visual analytics."),
              tags$li("Origin mobility-rate analysis."),
              tags$li("Gravity-style regression model."),
              tags$li("Fixed-effects panel regression."),
              tags$li("Random Forest prediction and variable importance."),
              tags$li("PCA-based origin-country profiling."),
              tags$li("ARIMA forecasting.")
            ),

            h4("Limitations"),
            tags$ul(
              tags$li("The analysis identifies associations, not causal effects."),
              tags$li("Distance is approximated using capital-city coordinates."),
              tags$li("Early leavers are a proxy indicator, not direct university dropout."),
              tags$li("Missing macro indicators may affect some countries and years."),
              tags$li("Macro-level indicators do not capture individual student-level motivations.")
            )
          )
        )
      )
    )
  )
)

# -------------------------
# SERVER
# -------------------------

server <- function(input, output, session) {

  observe({
    available_countries <- student_data %>%
      filter(
        geo == input$destinationCountry,
        origin_continent %in% input$continentInput
      ) %>%
      distinct(origin_country) %>%
      arrange(origin_country) %>%
      pull(origin_country)

    updateSelectizeInput(
      session,
      "originCountries",
      choices = available_countries,
      selected = character(0),
      server = TRUE
    )
  })

  filtered_students <- reactive({
    data <- student_data %>%
      filter(
        geo == input$destinationCountry,
        year >= input$yearRange[1],
        year <= input$yearRange[2],
        origin_continent %in% input$continentInput
      )

    if (!is.null(input$originCountries) && length(input$originCountries) > 0) {
      data <- data %>% filter(origin_country %in% input$originCountries)
    }

    data
  })

  yearly_students <- reactive({
    filtered_students() %>%
      group_by(year, origin_continent) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop")
  })

  range_students <- reactive({
    filtered_students() %>%
      group_by(origin_continent) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      mutate(origin_continent = factor(origin_continent, levels = names(continent_colors)))
  })

  filtered_dropout <- reactive({
    dropout_data %>%
      filter(
        geo == input$destinationCountry,
        year >= input$yearRange[1],
        year <= input$yearRange[2]
      )
  })

  model_data <- reactive({
    filtered_students() %>%
      group_by(origin_country, origin_iso3, origin_continent, year) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        origin_population = weighted_avg(origin_population, students),
        origin_mobility_rate_100k = weighted_avg(origin_mobility_rate_100k, students),
        gdp_gap = weighted_avg(gdp_gap, students),
        human_rights_gap = weighted_avg(human_rights_gap, students),
        unemployment_gap = weighted_avg(unemployment_gap, students),
        youth_unemployment_gap = weighted_avg(youth_unemployment_gap, students),
        education_expenditure_gap = weighted_avg(education_expenditure_gap, students),
        distance_km = weighted_avg(distance_km, students),
        log_distance = weighted_avg(log_distance, students),
        log_origin_gdp = weighted_avg(log_origin_gdp, students),
        log_destination_gdp = weighted_avg(log_destination_gdp, students),
        log_origin_population = weighted_avg(log_origin_population, students),
        same_continent = weighted_avg(same_continent, students),
        intra_europe = weighted_avg(intra_europe, students),
        .groups = "drop"
      ) %>%
      mutate(log_students = safe_log(students)) %>%
      filter(
        !is.na(origin_country),
        !is.na(year),
        !is.na(students),
        students > 0
      )
  })

  profile_data <- reactive({
    data <- model_data() %>%
      group_by(origin_country, origin_continent) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        mobility_rate = weighted_avg(origin_mobility_rate_100k, students),
        gdp_gap = weighted_avg(gdp_gap, students),
        human_rights_gap = weighted_avg(human_rights_gap, students),
        youth_unemployment_gap = weighted_avg(youth_unemployment_gap, students),
        distance_km = weighted_avg(distance_km, students),
        .groups = "drop"
      ) %>%
      filter(students > 0)

    if (nrow(data) < 3) return(NULL)

    data <- data %>%
      mutate(
        log_students = log(students),
        mobility_rate = median_impute(mobility_rate),
        gdp_gap = median_impute(gdp_gap),
        human_rights_gap = median_impute(human_rights_gap),
        youth_unemployment_gap = median_impute(youth_unemployment_gap),
        distance_km = median_impute(distance_km)
      )

    x <- data %>%
      select(
        log_students,
        mobility_rate,
        gdp_gap,
        human_rights_gap,
        youth_unemployment_gap,
        distance_km
      )

    x <- x[, sapply(x, function(col) {
      col <- col[is.finite(col)]
      length(unique(col)) > 1
    }), drop = FALSE]

    if (ncol(x) < 2) return(NULL)

    x_scaled <- scale(x)

    if (any(!is.finite(x_scaled))) return(NULL)

    pca <- prcomp(x_scaled, center = TRUE, scale. = FALSE)

    pca_scores <- as.data.frame(pca$x[, 1:2, drop = FALSE])
    names(pca_scores) <- c("PC1", "PC2")

    hc <- hclust(dist(x_scaled), method = "ward.D2")

    k <- min(4, nrow(data) - 1)
    if (k < 2) return(NULL)

    data$profile_group <- as.factor(cutree(hc, k = k))
    data$PC1 <- pca_scores$PC1
    data$PC2 <- pca_scores$PC2

    data
  })

  # -------------------------
  # Overview
  # -------------------------

  output$totalStudents <- renderValueBox({
    total <- sum(filtered_students()$students, na.rm = TRUE)

    valueBox(
      format(round(total), big.mark = ","),
      "Total Selected Students",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$rangeStudents <- renderValueBox({
    valueBox(
      paste(input$yearRange[1], "-", input$yearRange[2]),
      "Selected Year Range",
      icon = icon("calendar"),
      color = "purple"
    )
  })

  output$topOriginCountry <- renderValueBox({
    top <- filtered_students() %>%
      group_by(origin_country) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(students)) %>%
      slice(1)

    valueBox(
      ifelse(nrow(top) == 0, "No data", top$origin_country),
      "Top Origin Country",
      icon = icon("flag"),
      color = "green"
    )
  })

  output$timePlot <- renderPlotly({
    p <- ggplot(
      yearly_students(),
      aes(year, students, color = origin_continent)
    ) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      scale_color_manual(values = continent_colors, drop = FALSE) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(x = "Year", y = "Students", color = "Origin Continent")

    ggplotly(p)
  })

  output$piePlot <- renderPlotly({
    data <- range_students() %>%
      arrange(origin_continent)

    plot_ly(
      data,
      labels = ~origin_continent,
      values = ~students,
      type = "pie",
      marker = list(colors = continent_colors[as.character(data$origin_continent)])
    )
  })

  output$barPlot <- renderPlotly({
    data <- range_students() %>%
      arrange(origin_continent)

    p <- ggplot(
      data,
      aes(origin_continent, students, fill = origin_continent)
    ) +
      geom_col() +
      scale_fill_manual(values = continent_colors, drop = FALSE) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(x = "Origin Continent", y = "Students", fill = "Origin Continent")

    ggplotly(p)
  })

  # -------------------------
  # Migration Rate
  # -------------------------

  output$avgMobilityRate <- renderValueBox({
    rate <- weighted_avg(filtered_students()$origin_mobility_rate_100k, filtered_students()$students)

    valueBox(
      round(rate, 2),
      "Weighted Origin Mobility Rate per 100k",
      icon = icon("people-arrows"),
      color = "blue"
    )
  })

  output$topMobilityRateCountry <- renderValueBox({
    top <- model_data() %>%
      group_by(origin_country) %>%
      summarise(
        mobility_rate = weighted_avg(origin_mobility_rate_100k, students),
        .groups = "drop"
      ) %>%
      filter(!is.na(mobility_rate), is.finite(mobility_rate)) %>%
      arrange(desc(mobility_rate)) %>%
      slice(1)

    valueBox(
      ifelse(nrow(top) == 0, "No data", top$origin_country),
      "Highest Origin Mobility Rate",
      icon = icon("ranking-star"),
      color = "green"
    )
  })

  output$avgOriginPopulation <- renderValueBox({
    pop <- weighted_avg(filtered_students()$origin_population, filtered_students()$students)

    valueBox(
      comma(round(pop)),
      "Weighted Origin Population",
      icon = icon("earth-americas"),
      color = "purple"
    )
  })

  output$mobilityRateTrend <- renderPlotly({
    data <- model_data() %>%
      group_by(year, origin_country) %>%
      summarise(
        mobility_rate = weighted_avg(origin_mobility_rate_100k, students),
        students = sum(students, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(mobility_rate), is.finite(mobility_rate))

    if (nrow(data) == 0) {
      return(empty_plotly("No mobility-rate data for the selected filters."))
    }

    top_countries <- data %>%
      group_by(origin_country) %>%
      summarise(total_students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_students)) %>%
      slice_head(n = 10) %>%
      pull(origin_country)

    data <- data %>%
      filter(origin_country %in% top_countries)

    p <- ggplot(
      data,
      aes(
        x = year,
        y = mobility_rate,
        color = origin_country,
        group = origin_country,
        text = paste(
          "Year:", year,
          "<br>Origin country:", origin_country,
          "<br>Students:", comma(students),
          "<br>Mobility rate per 100k:", round(mobility_rate, 2)
        )
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Origin Mobility Rate per 100,000 Population",
        color = "Origin Country"
      )

    ggplotly(p, tooltip = "text")
  })

  output$mobilityRateCountryPlot <- renderPlotly({
    data <- model_data() %>%
      group_by(origin_country, origin_continent) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        mobility_rate = weighted_avg(origin_mobility_rate_100k, students),
        .groups = "drop"
      ) %>%
      filter(!is.na(mobility_rate), is.finite(mobility_rate)) %>%
      arrange(desc(mobility_rate)) %>%
      slice_head(n = 25) %>%
      mutate(origin_continent = factor(origin_continent, levels = names(continent_colors)))

    if (nrow(data) == 0) {
      return(empty_plotly("No mobility-rate data for selected filters."))
    }

    p <- ggplot(
      data,
      aes(
        x = reorder(origin_country, mobility_rate),
        y = mobility_rate,
        fill = origin_continent,
        text = paste(
          "Country:", origin_country,
          "<br>Students:", comma(students),
          "<br>Mobility rate per 100k:", round(mobility_rate, 2)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = continent_colors, drop = FALSE) +
      theme_minimal() +
      labs(
        x = "Origin Country",
        y = "Mobility Rate per 100,000 Population",
        fill = "Origin Continent"
      )

    ggplotly(p, tooltip = "text")
  })

  output$populationMobilityPlot <- renderPlotly({
    data <- model_data() %>%
      group_by(origin_country, origin_continent) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        origin_population = weighted_avg(origin_population, students),
        mobility_rate = weighted_avg(origin_mobility_rate_100k, students),
        .groups = "drop"
      ) %>%
      filter(
        !is.na(origin_population),
        !is.na(students),
        origin_population > 0,
        students > 0
      ) %>%
      mutate(origin_continent = factor(origin_continent, levels = names(continent_colors)))

    if (nrow(data) == 0) {
      return(empty_plotly("No population data for selected filters."))
    }

    p <- ggplot(
      data,
      aes(
        x = origin_population,
        y = students,
        color = origin_continent,
        size = mobility_rate,
        text = paste(
          "Country:", origin_country,
          "<br>Population:", comma(round(origin_population)),
          "<br>Students:", comma(round(students)),
          "<br>Mobility rate per 100k:", round(mobility_rate, 2)
        )
      )
    ) +
      geom_point(alpha = 0.8) +
      scale_x_log10(labels = comma) +
      scale_y_log10(labels = comma) +
      scale_color_manual(values = continent_colors, drop = FALSE) +
      theme_minimal() +
      labs(
        x = "Origin Population, log scale",
        y = "Students, log scale",
        color = "Origin Continent",
        size = "Mobility Rate"
      )

    ggplotly(p, tooltip = "text")
  })

  output$mobilityRateTable <- renderDT({
    data <- model_data() %>%
      group_by(origin_continent, origin_country) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        avg_origin_population = weighted_avg(origin_population, students),
        avg_mobility_rate_100k = weighted_avg(origin_mobility_rate_100k, students),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_mobility_rate_100k)) %>%
      mutate(
        students = round(students),
        avg_origin_population = round(avg_origin_population),
        avg_mobility_rate_100k = round(avg_mobility_rate_100k, 2)
      )

    datatable(data, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -------------------------
  # Map
  # -------------------------

  output$europeMap <- renderPlotly({
    data <- student_data %>%
      filter(
        year >= input$yearRange[1],
        year <= input$yearRange[2],
        origin_continent %in% input$continentInput
      )

    if (!is.null(input$originCountries) && length(input$originCountries) > 0) {
      data <- data %>% filter(origin_country %in% input$originCountries)
    }

    data <- data %>%
      group_by(destination_country, destination_iso3) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(destination_iso3), students > 0)

    plot_ly(
      data = data,
      type = "choropleth",
      locations = ~destination_iso3,
      z = ~students,
      text = ~paste(destination_country, "<br>Students:", comma(students)),
      hoverinfo = "text",
      colorscale = "Blues",
      marker = list(line = list(color = "white", width = 0.5))
    ) %>%
      layout(
        geo = list(
          scope = "europe",
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth")
        )
      )
  })

  output$destinationRankingTable <- renderDT({
    data <- student_data %>%
      filter(
        year >= input$yearRange[1],
        year <= input$yearRange[2],
        origin_continent %in% input$continentInput
      )

    if (!is.null(input$originCountries) && length(input$originCountries) > 0) {
      data <- data %>% filter(origin_country %in% input$originCountries)
    }

    data <- data %>%
      group_by(destination_country) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(students)) %>%
      mutate(students = round(students))

    datatable(data, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -------------------------
  # Migration Drivers
  # -------------------------

  output$avgGDPGap <- renderValueBox({
    gap <- weighted_avg(filtered_students()$gdp_gap, filtered_students()$students)

    valueBox(
      dollar(round(gap, 0)),
      "Weighted GDP Gap",
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  output$avgRightsGap <- renderValueBox({
    gap <- weighted_avg(filtered_students()$human_rights_gap, filtered_students()$students)

    valueBox(
      round(gap, 3),
      "Weighted Human Rights Gap",
      icon = icon("scale-balanced"),
      color = "green"
    )
  })

  output$avgDistance <- renderValueBox({
    dist <- weighted_avg(filtered_students()$distance_km, filtered_students()$students)

    valueBox(
      paste0(comma(round(dist)), " km"),
      "Weighted Distance",
      icon = icon("route"),
      color = "yellow"
    )
  })

  output$gdpGapPlot <- renderPlotly({
    data <- model_data() %>%
      group_by(year, origin_continent) %>%
      summarise(
        total_students = sum(students, na.rm = TRUE),
        gdp_gap_avg = weighted_avg(gdp_gap, students),
        .groups = "drop"
      ) %>%
      mutate(
        gdp_gap_avg = median_impute(gdp_gap_avg),
        origin_continent = factor(origin_continent, levels = names(continent_colors))
      )

    p <- ggplot(
      data,
      aes(
        x = year,
        y = gdp_gap_avg,
        color = origin_continent,
        group = origin_continent,
        text = paste(
          "Year:", year,
          "<br>Continent:", origin_continent,
          "<br>Students:", comma(total_students),
          "<br>GDP Gap:", dollar(round(gdp_gap_avg, 0))
        )
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = continent_colors, drop = FALSE) +
      scale_y_continuous(labels = dollar) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Weighted GDP Gap",
        color = "Origin Continent"
      )

    ggplotly(p, tooltip = "text")
  })

  output$rightsGapPlot <- renderPlotly({
    data <- model_data() %>%
      group_by(year, origin_continent) %>%
      summarise(
        total_students = sum(students, na.rm = TRUE),
        rights_gap_avg = weighted_avg(human_rights_gap, students),
        .groups = "drop"
      ) %>%
      mutate(
        rights_gap_avg = median_impute(rights_gap_avg),
        origin_continent = factor(origin_continent, levels = names(continent_colors))
      )

    p <- ggplot(
      data,
      aes(
        x = year,
        y = rights_gap_avg,
        color = origin_continent,
        group = origin_continent,
        text = paste(
          "Year:", year,
          "<br>Continent:", origin_continent,
          "<br>Students:", comma(total_students),
          "<br>Human Rights Gap:", round(rights_gap_avg, 3)
        )
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = continent_colors, drop = FALSE) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Weighted Human Rights Gap",
        color = "Origin Continent"
      )

    ggplotly(p, tooltip = "text")
  })

  output$distancePlot <- renderPlotly({
    data <- model_data() %>%
      group_by(origin_country, origin_continent) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        distance_km = weighted_avg(distance_km, students),
        .groups = "drop"
      ) %>%
      mutate(
        distance_km = median_impute(distance_km),
        origin_continent = factor(origin_continent, levels = names(continent_colors))
      )

    p <- ggplot(
      data,
      aes(
        distance_km,
        students,
        color = origin_continent,
        text = origin_country
      )
    ) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = continent_colors, drop = FALSE) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(x = "Distance between capitals (km)", y = "Students")

    ggplotly(p, tooltip = c("text", "x", "y"))
  })

  output$youthUnemploymentPlot <- renderPlotly({
    data <- model_data() %>%
      group_by(origin_country, origin_continent) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        youth_unemployment_gap = weighted_avg(youth_unemployment_gap, students),
        .groups = "drop"
      ) %>%
      mutate(
        youth_unemployment_gap = median_impute(youth_unemployment_gap),
        origin_continent = factor(origin_continent, levels = names(continent_colors))
      )

    p <- ggplot(
      data,
      aes(
        youth_unemployment_gap,
        students,
        color = origin_continent,
        text = origin_country
      )
    ) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = continent_colors, drop = FALSE) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(x = "Youth Unemployment Gap", y = "Students")

    ggplotly(p, tooltip = c("text", "x", "y"))
  })

  output$driversTable <- renderDT({
    data <- model_data() %>%
      group_by(origin_continent, origin_country) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        avg_mobility_rate_100k = weighted_avg(origin_mobility_rate_100k, students),
        avg_gdp_gap = weighted_avg(gdp_gap, students),
        avg_human_rights_gap = weighted_avg(human_rights_gap, students),
        avg_youth_unemployment_gap = weighted_avg(youth_unemployment_gap, students),
        avg_distance_km = weighted_avg(distance_km, students),
        .groups = "drop"
      ) %>%
      arrange(desc(students)) %>%
      mutate(
        students = round(students),
        avg_mobility_rate_100k = round(avg_mobility_rate_100k, 2),
        avg_gdp_gap = round(avg_gdp_gap, 0),
        avg_human_rights_gap = round(avg_human_rights_gap, 3),
        avg_youth_unemployment_gap = round(avg_youth_unemployment_gap, 2),
        avg_distance_km = round(avg_distance_km)
      )

    datatable(data, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -------------------------
  # Gravity Model
  # -------------------------

  gravity_fit <- reactive({
    data <- model_data() %>%
      mutate(
        gdp_gap = median_impute(gdp_gap),
        human_rights_gap = median_impute(human_rights_gap),
        youth_unemployment_gap = median_impute(youth_unemployment_gap),
        education_expenditure_gap = median_impute(education_expenditure_gap),
        log_distance = median_impute(log_distance),
        log_origin_gdp = median_impute(log_origin_gdp),
        log_destination_gdp = median_impute(log_destination_gdp),
        log_origin_population = median_impute(log_origin_population),
        intra_europe = median_impute(intra_europe)
      )

    if (nrow(data) < 15) return(NULL)

    lm(
      log_students ~ log_origin_gdp +
        log_destination_gdp +
        log_origin_population +
        log_distance +
        human_rights_gap +
        youth_unemployment_gap +
        education_expenditure_gap +
        intra_europe,
      data = data
    )
  })

  output$gravityModel <- renderPrint({
    fit <- gravity_fit()

    if (is.null(fit)) {
      cat("Not enough observations for Gravity Model.")
    } else {
      summary(fit)
    }
  })

  output$gravityCoefTable <- renderDT({
    fit <- gravity_fit()

    if (is.null(fit)) {
      datatable(data.frame(Message = "Not enough observations."))
    } else {
      data <- broom::tidy(fit) %>%
        mutate(
          estimate = round(estimate, 4),
          std.error = round(std.error, 4),
          statistic = round(statistic, 3),
          p.value = round(p.value, 4)
        )

      datatable(data, options = list(pageLength = 10, scrollX = TRUE))
    }
  })

  # -------------------------
  # Panel Regression
  # -------------------------

  output$panelRegression <- renderPrint({
    data <- model_data() %>%
      mutate(
        gdp_gap = median_impute(gdp_gap),
        human_rights_gap = median_impute(human_rights_gap),
        youth_unemployment_gap = median_impute(youth_unemployment_gap),
        education_expenditure_gap = median_impute(education_expenditure_gap),
        log_distance = median_impute(log_distance),
        log_origin_population = median_impute(log_origin_population)
      )

    if (nrow(data) < 20 || length(unique(data$origin_country)) < 3) {
      cat("Not enough observations for panel regression.")
    } else {
      pdata <- pdata.frame(data, index = c("origin_country", "year"))

      model <- tryCatch(
        plm(
          log_students ~ gdp_gap +
            human_rights_gap +
            youth_unemployment_gap +
            education_expenditure_gap +
            log_distance +
            log_origin_population,
          data = pdata,
          model = "within"
        ),
        error = function(e) e
      )

      if (inherits(model, "error")) {
        cat("Panel regression could not be estimated:\n")
        cat(model$message)
      } else {
        print(summary(model))
      }
    }
  })

  output$panelDataTable <- renderDT({
    data <- model_data() %>%
      mutate(
        students = round(students),
        origin_population = round(origin_population),
        origin_mobility_rate_100k = round(origin_mobility_rate_100k, 2),
        gdp_gap = round(gdp_gap, 0),
        human_rights_gap = round(human_rights_gap, 3),
        youth_unemployment_gap = round(youth_unemployment_gap, 2),
        education_expenditure_gap = round(education_expenditure_gap, 2),
        distance_km = round(distance_km),
        log_students = round(log_students, 3)
      )

    datatable(data, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -------------------------
  # Machine Learning
  # -------------------------

  rf_result <- reactive({
    data <- model_data() %>%
      select(
        log_students,
        origin_mobility_rate_100k,
        gdp_gap,
        human_rights_gap,
        youth_unemployment_gap,
        education_expenditure_gap,
        log_distance,
        log_origin_gdp,
        log_destination_gdp,
        log_origin_population,
        intra_europe
      )

    if (nrow(data) < 20) return(NULL)

    data <- data %>%
      mutate(
        origin_mobility_rate_100k = median_impute(origin_mobility_rate_100k),
        gdp_gap = median_impute(gdp_gap),
        human_rights_gap = median_impute(human_rights_gap),
        youth_unemployment_gap = median_impute(youth_unemployment_gap),
        education_expenditure_gap = median_impute(education_expenditure_gap),
        log_distance = median_impute(log_distance),
        log_origin_gdp = median_impute(log_origin_gdp),
        log_destination_gdp = median_impute(log_destination_gdp),
        log_origin_population = median_impute(log_origin_population),
        intra_europe = median_impute(intra_europe)
      ) %>%
      filter(!is.na(log_students))

    if (nrow(data) < 20) return(NULL)

    set.seed(123)
    train_id <- sample(seq_len(nrow(data)), size = floor(0.75 * nrow(data)))

    train <- data[train_id, ]
    test <- data[-train_id, ]

    model <- randomForest(
      log_students ~ .,
      data = train,
      ntree = 500,
      importance = TRUE
    )

    pred <- predict(model, newdata = test)

    list(model = model, train = train, test = test, pred = pred)
  })

  output$rfImportancePlot <- renderPlot({
    result <- rf_result()

    if (is.null(result)) {
      plot.new()
      text(0.5, 0.5, "Not enough observations for Random Forest.")
    } else {
      importance_df <- data.frame(
        variable = rownames(importance(result$model)),
        importance = importance(result$model)[, "%IncMSE"]
      ) %>%
        arrange(importance)

      ggplot(importance_df, aes(reorder(variable, importance), importance)) +
        geom_col() +
        coord_flip() +
        theme_minimal() +
        labs(x = "Variable", y = "Importance (%IncMSE)")
    }
  })

  output$actualPredictedPlot <- renderPlotly({
    result <- rf_result()

    if (is.null(result)) {
      return(empty_plotly("Not enough observations for Random Forest."))
    } else {
      df <- data.frame(
        actual = result$test$log_students,
        predicted = as.numeric(result$pred)
      )

      p <- ggplot(df, aes(actual, predicted)) +
        geom_point(alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        theme_minimal() +
        labs(x = "Actual log(students)", y = "Predicted log(students)")

      ggplotly(p)
    }
  })

  output$modelPerformanceTable <- renderDT({
    result <- rf_result()

    if (is.null(result)) {
      datatable(data.frame(Message = "Not enough observations."))
    } else {
      metrics <- model_metrics(result$test$log_students, as.numeric(result$pred)) %>%
        mutate(
          RMSE = round(RMSE, 3),
          MAE = round(MAE, 3),
          R2 = round(R2, 3)
        )

      datatable(metrics, options = list(dom = "t"))
    }
  })

  # -------------------------
  # Country Profiles: PCA + Hierarchical Clustering
  # -------------------------

  output$profilePlot <- renderPlotly({
    data <- profile_data()

    if (is.null(data)) {
      return(empty_plotly("Not enough valid data for country profiling. Try selecting more origin countries or a wider year range."))
    }

    p <- ggplot(
      data,
      aes(
        x = PC1,
        y = PC2,
        color = profile_group,
        size = mobility_rate,
        text = paste(
          "Origin country:", origin_country,
          "<br>Profile group:", profile_group,
          "<br>Students:", comma(round(students)),
          "<br>Mobility rate per 100k:", round(mobility_rate, 2),
          "<br>GDP Gap:", dollar(round(gdp_gap, 0)),
          "<br>Human Rights Gap:", round(human_rights_gap, 3),
          "<br>Distance:", comma(round(distance_km)), "km"
        )
      )
    ) +
      geom_point(alpha = 0.85) +
      theme_minimal() +
      labs(
        x = "PCA Dimension 1",
        y = "PCA Dimension 2",
        color = "Profile Group",
        size = "Mobility Rate"
      )

    ggplotly(p, tooltip = "text")
  })

  output$profileTable <- renderDT({
    data <- profile_data()

    if (is.null(data)) {
      datatable(data.frame(Message = "Not enough valid data for country profiling. Try selecting more origin countries or a wider year range."))
    } else {
      table <- data %>%
        group_by(profile_group) %>%
        summarise(
          countries = n(),
          total_students = sum(students, na.rm = TRUE),
          avg_mobility_rate = mean(mobility_rate, na.rm = TRUE),
          avg_gdp_gap = mean(gdp_gap, na.rm = TRUE),
          avg_human_rights_gap = mean(human_rights_gap, na.rm = TRUE),
          avg_youth_unemployment_gap = mean(youth_unemployment_gap, na.rm = TRUE),
          avg_distance_km = mean(distance_km, na.rm = TRUE),
          example_countries = paste(head(origin_country, 5), collapse = ", "),
          .groups = "drop"
        ) %>%
        mutate(
          total_students = round(total_students),
          avg_mobility_rate = round(avg_mobility_rate, 2),
          avg_gdp_gap = round(avg_gdp_gap, 0),
          avg_human_rights_gap = round(avg_human_rights_gap, 3),
          avg_youth_unemployment_gap = round(avg_youth_unemployment_gap, 2),
          avg_distance_km = round(avg_distance_km)
        )

      datatable(table, options = list(pageLength = 10, scrollX = TRUE))
    }
  })

  # -------------------------
  # Forecasting
  # -------------------------

  forecast_data <- reactive({
    filtered_students() %>%
      group_by(year) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)
  })

  output$forecastPlot <- renderPlotly({
    data <- forecast_data()

    if (nrow(data) < 6) {
      return(empty_plotly("Not enough data for forecasting"))
    } else {
      ts_data <- ts(data$students, start = min(data$year), frequency = 1)
      fit <- auto.arima(ts_data)
      fc <- forecast(fit, h = 5)

      hist_df <- data.frame(
        year = data$year,
        students = as.numeric(data$students),
        type = "Historical"
      )

      fc_df <- data.frame(
        year = (max(data$year) + 1):(max(data$year) + 5),
        students = as.numeric(fc$mean),
        lower = as.numeric(fc$lower[, 2]),
        upper = as.numeric(fc$upper[, 2]),
        type = "Forecast"
      )

      p <- ggplot() +
        geom_line(data = hist_df, aes(year, students, color = type), linewidth = 1.2) +
        geom_point(data = hist_df, aes(year, students, color = type)) +
        geom_line(data = fc_df, aes(year, students, color = type), linewidth = 1.2) +
        geom_point(data = fc_df, aes(year, students, color = type)) +
        geom_ribbon(data = fc_df, aes(year, ymin = lower, ymax = upper), alpha = 0.2) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(x = "Year", y = "Students", color = "")

      ggplotly(p)
    }
  })

  output$forecastTable <- renderDT({
    data <- forecast_data()

    if (nrow(data) < 6) {
      datatable(data.frame(Message = "Not enough data for forecasting."))
    } else {
      ts_data <- ts(data$students, start = min(data$year), frequency = 1)
      fit <- auto.arima(ts_data)
      fc <- forecast(fit, h = 5)

      table <- data.frame(
        year = (max(data$year) + 1):(max(data$year) + 5),
        forecast_students = round(as.numeric(fc$mean)),
        lower_95 = round(as.numeric(fc$lower[, 2])),
        upper_95 = round(as.numeric(fc$upper[, 2]))
      )

      datatable(table, options = list(pageLength = 5, scrollX = TRUE))
    }
  })

  # -------------------------
  # Dropout / Completion
  # -------------------------

  output$dropoutRate <- renderValueBox({
    rate <- mean(filtered_dropout()$dropout_rate, na.rm = TRUE)

    valueBox(
      paste0(round(rate * 100, 1), "%"),
      "Average Early Leaver Rate",
      icon = icon("user-times"),
      color = "red"
    )
  })

  output$completionRate <- renderValueBox({
    rate <- mean(filtered_dropout()$completion_rate, na.rm = TRUE)

    valueBox(
      paste0(round(rate * 100, 1), "%"),
      "Average Completion Rate",
      icon = icon("user-check"),
      color = "green"
    )
  })

  output$totalDropouts <- renderValueBox({
    total <- sum(filtered_dropout()$estimated_dropouts, na.rm = TRUE)

    valueBox(
      format(round(total), big.mark = ","),
      "Estimated Early Leavers",
      icon = icon("exclamation"),
      color = "yellow"
    )
  })

  output$dropoutTrend <- renderPlotly({
    plot_data <- filtered_dropout() %>%
      select(year, dropout_rate, completion_rate) %>%
      pivot_longer(
        cols = c(dropout_rate, completion_rate),
        names_to = "indicator",
        values_to = "rate"
      )

    p <- ggplot(plot_data, aes(year, rate, color = indicator)) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      scale_y_continuous(labels = percent) +
      theme_minimal() +
      labs(x = "Year", y = "Rate", color = "Indicator")

    ggplotly(p)
  })

  output$dropoutPie <- renderPlotly({
    row <- filtered_dropout() %>%
      summarise(
        graduates = sum(graduates, na.rm = TRUE),
        estimated_dropouts = sum(estimated_dropouts, na.rm = TRUE)
      )

    plot_ly(
      labels = c("Graduates", "Estimated Early Leavers"),
      values = c(row$graduates, row$estimated_dropouts),
      type = "pie"
    )
  })

  output$comparisonBar <- renderPlotly({
    row <- filtered_dropout() %>%
      summarise(
        graduates = sum(graduates, na.rm = TRUE),
        estimated_dropouts = sum(estimated_dropouts, na.rm = TRUE)
      )

    df <- data.frame(
      category = c("Graduates", "Estimated Early Leavers"),
      values = c(row$graduates, row$estimated_dropouts)
    )

    p <- ggplot(df, aes(category, values, fill = category)) +
      geom_col() +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(x = "", y = "Count")

    ggplotly(p)
  })

  # -------------------------
  # Policy Insights
  # -------------------------

  output$policyInsight <- renderUI({
    data <- filtered_students()

    top_origin <- data %>%
      group_by(origin_country) %>%
      summarise(students = sum(students, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(students)) %>%
      slice(1)

    top_rate <- model_data() %>%
      group_by(origin_country) %>%
      summarise(
        mobility_rate = weighted_avg(origin_mobility_rate_100k, students),
        .groups = "drop"
      ) %>%
      arrange(desc(mobility_rate)) %>%
      slice(1)

    gdp_gap <- weighted_avg(data$gdp_gap, data$students)
    rights_gap <- weighted_avg(data$human_rights_gap, data$students)
    distance <- weighted_avg(data$distance_km, data$students)
    youth_gap <- weighted_avg(data$youth_unemployment_gap, data$students)
    mobility_rate <- weighted_avg(data$origin_mobility_rate_100k, data$students)

    HTML(paste0(
      "<p><b>Main interpretation:</b> For the selected destination country and period, the largest origin country by student count is <b>",
      ifelse(nrow(top_origin) == 0, "not available", top_origin$origin_country),
      "</b>.</p>",
      "<p>The highest origin mobility rate is observed for <b>",
      ifelse(nrow(top_rate) == 0, "not available", top_rate$origin_country),
      "</b>.</p>",
      "<p>The weighted origin mobility rate is <b>", round(mobility_rate, 2),
      " students per 100,000 origin population</b>.</p>",
      "<p>The weighted GDP gap is <b>", dollar(round(gdp_gap, 0)),
      "</b>, the weighted human-rights gap is <b>", round(rights_gap, 3),
      "</b>, and the average distance is about <b>", comma(round(distance)), " km</b>.</p>",
      "<p>The weighted youth-unemployment gap is <b>", round(youth_gap, 2),
      " percentage points</b>.</p>",
      "<p><b>Policy relevance:</b> The mobility-rate indicator helps distinguish countries that send many students because they are populous from countries with a high relative tendency toward student migration.</p>",
      "<p><b>Important caution:</b> These results are descriptive and predictive, not causal.</p>"
    ))
  })

  output$originCountryTable <- renderDT({
    data <- model_data() %>%
      group_by(origin_continent, origin_country) %>%
      summarise(
        students = sum(students, na.rm = TRUE),
        avg_mobility_rate_100k = weighted_avg(origin_mobility_rate_100k, students),
        avg_gdp_gap = weighted_avg(gdp_gap, students),
        avg_human_rights_gap = weighted_avg(human_rights_gap, students),
        avg_youth_unemployment_gap = weighted_avg(youth_unemployment_gap, students),
        avg_distance_km = weighted_avg(distance_km, students),
        .groups = "drop"
      ) %>%
      arrange(desc(students)) %>%
      mutate(
        students = round(students),
        avg_mobility_rate_100k = round(avg_mobility_rate_100k, 2),
        avg_gdp_gap = round(avg_gdp_gap, 0),
        avg_human_rights_gap = round(avg_human_rights_gap, 3),
        avg_youth_unemployment_gap = round(avg_youth_unemployment_gap, 2),
        avg_distance_km = round(avg_distance_km)
      )

    datatable(data, options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)
