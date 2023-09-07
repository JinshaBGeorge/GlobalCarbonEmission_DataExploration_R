## Overview

This project focuses on the analysis of global carbon emissions data sourced from opendata-jrc and World Bank data. The primary objectives of this project are data preprocessing, cleaning, visualization, and deriving meaningful insights from the datasets.


 **Data**: From opendata-jrc and World Bank data.

## Data Pre-processing and Cleaning

The available data is in various formats and spread across three different datasets. The following data pre-processing and cleaning steps were undertaken:

- Skipping irrelevant columns and initial rows in CSV files containing data descriptions.
- Tidying the raw data by using the `pivot_longer()` function to transform the Year variable spread across multiple columns.
- Filtering data from the land area and population datasets to match the carbon emissions data, focusing on the years 1970 to 2021.
- Merging the three data frames to create a single table.
- Calculating new columns such as population density and carbon emission per capita.

## Conclusion

After analyzing the global carbon emissions data from 1970 to 2021 in approximately 200 countries, the following key findings emerged:

- Top contributors to global carbon emissions are China, USA, India, and Russia.
- There is a linear and proportional relationship between global population and global carbon emissions, both showing an increase over the years.
- China and India have seen exponential growth in carbon emissions, while the USA and Russia have maintained steadier emissions.
- No conclusive relationship was found between a country's area and its carbon emissions.
- Several outlier countries exhibit high carbon emissions per capita, though most countries remain below 0.02 carbon emissions per capita.


