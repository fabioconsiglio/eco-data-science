# Project for ECON4170 - 24H Data science for economists
### Contributors: 
Student 1: Candidate No. 17425

Student 2: Candidate No. 17422

### Project Description:

This project explores descriptively the relationship between income inequality and various social and economic variables, such as crime rates and financial market trends, to provide insights with important policy implications.

### Repo Structure:

#### ggplot_project (folder): Contains code, data and figures from the data visualization process. 

`code`:

- inequ_viz: Explores different inequality measures (for G7 countries)
- inequ_finance: Explores the relationship between income inequality and financial market trends.
- inequ_vs_crimes: Explores the relationship between income inequality and homicide rates.

- data_preprocessing_shiny: Data preprocessing for R Shiny.

`data`: 

- **raw**:
  - homicide-rate-unodc: Homicide rates from 'Our World in Data'
  - inequality-dataset: Income inequality data from 'Our World in Data'
  - INDICES_DATA: Financial market data from 'kaggle'
  
- **preprocessed**:
  - inequ_homicide_data: Preprocessed income inequality and homicide data
  - stock-data: financial market data (long-format)
  
`figures`: contains all figures that were relevant for our report. 

#### shiny
  - app.R: R Shiny app for interactive data visualization.
  - description.html: Short description that is included in the dashboard to explain the functions in the dashboard.
  - stock_data.csv / inequ_homicide_data.csv: Data for the dashboard (For publishing, storing data in the same folder was necessary).


### Data Sources:
- [Inequality measures](https://ourworldindata.org/economic-inequality#explore-data-on-economic-inequality)
- [Homicide rates](https://ourworldindata.org/grapher/homicide-rate-unodc?time=earliest)
- [Stock market indices](https://www.kaggle.com/datasets/mukhazarahmad/worldwide-stock-market-indices-data)
