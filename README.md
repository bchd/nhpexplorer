# Neighborhood Health Profiles (NHP) Explorer for useR Conference
For educational purposes only. For useR 2022 Conference

# Background
Exploratory analysis of geographic health indicators and outcomes is frequently performed. For example, examining correlations between Social Determinants of Health (SDOH) and the homicide rate may provide insights into risk factors, protective factors and potential solutions. Examples of social determinants of health include median household income, percent of students absent from high or middle school and the lead paint violation rate. 

# Purpose and Methods
A Shiny App was created to explore correlations at the neighborhood-level in Baltimore City, Maryland. Neighborhood Health Profile (NHP) data for Baltimore City’s 55 Community Statistical Areas was used.  The NHP 2017 dataset contains 102 continuous variables on SDOH and health outcomes.  The Shiny App allows users to select an explanatory variable and outcome variable for analysis of correlations and related statistics.   In addition, the app contains maps for explanatory and outcome variables, and an overlay map combining both using the leaflet package. Lastly, basic machine learning components were added for K-means clustering and Principal Component Analysis—using factoextra.  

# Conclusion
This Shiny App can be used to explore any geographic dataset containing lots of continuous variables. The rgeoda package will also be briefly discussed
