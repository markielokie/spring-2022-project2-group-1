# Project 2: Shiny App Development

### Data folder

The data directory contains data used in the analysis. This is treated as read only; in particular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

Files in this folder:

+ *nyc_zip_borough_neighborhoods_pop.csv*: Data is taken from [data.betaNYC](https://data.beta.nyc/dataset/pediacities-nyc-neighborhoods/resource/7caac650-d082-4aea-9f9b-3681d568e8a5).
+ *zip_code_040114.geojson*: Data is taken from [data.betaNYC](https://data.beta.nyc/en/dataset/nyc-zip-code-tabulation-areas/resource/894e9162-871c-4552-a09c-c6915d8783fb). 

Files not in this folder (too huge):

+ *311 Call Center Inquiry*: Data is downloaded one year at a time starting from October 2019 to present from [NYC OpenData](https://data.cityofnewyork.us/City-Government/311-Call-Center-Inquiry/wewp-mm3p).
+ *311 Service Requests*: Data is downloaded one year at a time starting from October 2019 to present from [NYC OpenData](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9).
+ *NYC Covid Cases & Deaths*: Data is downloaded from the GitHub of [New York Times](https://github.com/nytimes/covid-19-data).