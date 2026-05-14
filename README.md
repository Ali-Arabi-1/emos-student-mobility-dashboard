# Explaining International Student Mobility in Europe

## An Official Statistics and Socio-Economic Dashboard built with R Shiny

This project is an interactive **R Shiny dashboard** designed to analyse international student mobility in Europe using official statistics and socio-economic indicators. It was developed as an EMOS-related project and is suitable for presentation in contexts focused on **official statistics, reproducible R workflows, policy analysis, and statistical communication**.

The dashboard combines data from **Eurostat**, the **World Bank**, and **Our World in Data / V-Dem** to explore how international student mobility is associated with economic, demographic, geographic, and institutional factors.

---

## Project Aim

The aim of this project is to provide a reproducible and interactive framework for understanding patterns of international student mobility in Europe.

The dashboard helps answer questions such as:

- Which countries send the largest number of international students to European destinations?
- How does student mobility change over time?
- How do mobility patterns differ by region or continent of origin?
- Are socio-economic indicators such as GDP, unemployment, education expenditure, and human-rights conditions associated with mobility patterns?
- Can statistical models help explain and forecast student mobility trends?

---

## Key Features

- Interactive R Shiny dashboard
- Eurostat-based analysis of international student mobility
- Origin-country and destination-country filtering
- Time-series visualisation of mobility trends
- Mobility-rate indicator adjusted by origin-country population
- Socio-economic comparison using World Bank indicators
- Human-rights indicator integration using OWID / V-Dem data
- Gravity-style regression model
- Fixed-effects panel regression
- Random Forest prediction and variable-importance analysis
- PCA-based country profiling
- ARIMA forecasting
- Policy-oriented interpretation section
- Methodology and limitations section

---

## Data Sources

This project uses open and official data sources:

### Eurostat

Used for:

- International student mobility
- Enrolment statistics
- Graduates
- Early leavers / dropout proxy indicators

### World Bank

Used for macro-level indicators:

- GDP per capita
- Total population
- Unemployment rate
- Youth unemployment rate
- Education expenditure

### Our World in Data / V-Dem

Used for:

- Human Rights Index

### Geographic Data

Used for:

- Approximate distance between origin and destination countries
- Capital-city coordinate-based distance calculation

---

## Methodology

The dashboard applies several descriptive, statistical, and predictive methods.

### 1. Descriptive Visual Analytics

The dashboard summarises student mobility flows by:

- destination country
- origin country
- continent of origin
- year
- selected period

### 2. Mobility-Rate Indicator

A central indicator in the project is the **origin mobility rate per 100,000 population**:

```text
Mobility Rate = International students from origin country / Origin population × 100,000
```

This indicator helps distinguish countries that send many students because they are highly populated from countries that have a high relative tendency toward international student mobility.

### 3. Gravity-Style Regression

A gravity-style model is used to analyse associations between student mobility and macro-level factors such as:

- geographic distance
- GDP differences
- population size
- unemployment differences
- youth unemployment differences
- education expenditure differences
- human-rights differences

### 4. Fixed-Effects Panel Regression

Panel regression is used to account for repeated observations across countries and years. This helps capture country-level and time-level heterogeneity.

### 5. Random Forest Prediction

Random Forest is used as a supplementary predictive method to explore non-linear associations and variable importance.

This component is intended as an exploratory tool, not as a causal model.

### 6. PCA-Based Country Profiling

Principal Component Analysis and clustering are used to group origin countries with similar socio-economic and mobility characteristics.

### 7. ARIMA Forecasting

Time-series forecasting is used to explore possible future trends in student mobility.

---

## Important Interpretation Note

The results should be interpreted as **descriptive and predictive**, not causal.

The dashboard identifies associations between international student mobility and macro-level indicators. It does not claim that one factor directly causes student mobility.

---

## Limitations

- The analysis is based on aggregate macro-level data.
- Individual student motivations are not directly observed.
- Distance is approximated using capital-city coordinates.
- Early leavers are used as a proxy indicator and may not perfectly represent university dropout.
- Missing macro indicators may affect some countries and years.
- Results are sensitive to data availability and statistical assumptions.
- Predictive models should not be interpreted as causal evidence.

---

## R Packages Used

The project uses the following main R packages:

```r
shiny
shinydashboard
ggplot2
plotly
dplyr
DT
corrplot
eurostat
tidyr
countrycode
scales
WDI
readr
forecast
plm
randomForest
broom
maps
geosphere
```

---

## How to Run the Dashboard

### 1. Install Required Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "ggplot2", "plotly", "dplyr", "DT",
  "corrplot", "eurostat", "tidyr", "countrycode", "scales",
  "WDI", "readr", "forecast", "plm", "randomForest", "broom",
  "maps", "geosphere"
))
```

### 2. Open the Project in RStudio

Open the project folder and make sure the main Shiny file is named:

```text
app.R
```

### 3. Run the Application

In RStudio, click:

```text
Run App
```

Or run:

```r
shiny::runApp()
```

---

## Suggested Repository Structure

```text
emos-student-mobility-dashboard/
│
├── app.R
├── README.md
├── LICENSE
├── .gitignore
│
├── data/
│   └── README.md
│
├── figures/
│   └── dashboard_screenshot.png
│
└── docs/
    └── methodology.md
```

For a first version, it is acceptable to keep the project simple with:

```text
app.R
README.md
LICENSE
.gitignore
```

---

## Reproducibility

This project is designed as a reproducible R workflow using open data sources and documented statistical methods.

For stronger reproducibility, future versions may include:

- `renv` for package version control
- saved metadata files
- data download scripts
- session information
- automated preprocessing scripts

Recommended command:

```r
sessionInfo()
```

---

## Policy Relevance

International student mobility is an important topic for European higher education, labour-market integration, and cross-border educational policy.

This dashboard supports policy-oriented analysis by combining:

- official statistics
- socio-economic indicators
- interpretable modelling
- interactive visual communication

The project can help users explore how mobility patterns vary across countries and how these patterns are associated with broader economic and institutional conditions.

---

## Conference Relevance

This project is relevant to the uRos / EMOS context because it demonstrates:

- the use of R in official statistics
- reproducible statistical workflows
- interactive dashboards for policy communication
- integration of Eurostat data with external socio-economic indicators
- transparent and interpretable modelling

---

## Suggested Citation

If you use or refer to this project, please cite it as:

```text
Arabi, A. Explaining International Student Mobility in Europe: An Official Statistics and Socio-Economic Dashboard. R Shiny project.
```

---

## Author

**Ali Arabi**  
EMOS Student  
GitHub: `@arabi6000`

---

## License

This project is released under the MIT License.
