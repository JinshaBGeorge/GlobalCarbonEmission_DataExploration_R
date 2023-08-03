install.packages('ggmap')

#Calling libraries
library(tidyverse)
library(formattable)
library(ggmap)
library(ggpubr)
library(directlabels)
options(scipen = 999)

#setting current working directory
setwd("C:/Users/jinsh/Documents/R") #Please change to the work directory
getwd()

##################################################
#Section 2: Data pre-processing and data cleaning
#################################################
#Creating dataframe for carbon emissions data
carbon_rawdata <- read.csv("CO2_1970_2021.csv", header = TRUE, 
                           sep = ",", stringsAsFactors = FALSE, skip = 9)
carbon_rawdata <- carbon_rawdata[, -c(58:72)]
carbon_rawdata <- carbon_rawdata[, -c(1:2)]
carbon_rawdata <- select(carbon_rawdata,-Substance)
head(carbon_rawdata)
str(carbon_rawdata)

#Creating a dataframe for populations data
population_rawdata <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv", 
                               header = TRUE, sep = ",", stringsAsFactors = FALSE, 
                               skip = 4)
population_rawdata <- population_rawdata[, -67]
population_rawdata <- select(population_rawdata,-Indicator.Name,-Indicator.Code)
head(population_rawdata)
str(population_rawdata)

#Creating a dataframe for land area data
area_rawdata <- read.csv("API_AG.LND.TOTL.K2_DS2_en_csv_v2_4701206.csv", 
                         header = TRUE, sep = ",", stringsAsFactors = FALSE, 
                         skip = 4)
area_rawdata <- area_rawdata[, -c(66:67)]
area_rawdata$X2021 = area_rawdata$X2020
area_rawdata <- select(area_rawdata,-Indicator.Name,-Indicator.Code)
head(area_rawdata)
str(area_rawdata)

#Renaming columns 
colnames(carbon_rawdata)<-gsub("Y_","",colnames(carbon_rawdata))
carbon_rawdata <- carbon_rawdata %>% 
  rename("CountryCode" = "Country_code_A3") %>%
  rename("CountryName" = "Name")

#Tiding columns of Year to single column
carbon_rawdata <- carbon_rawdata %>%
  pivot_longer(c('1970','1971','1972','1973','1974','1975',
                 '1976','1977','1978','1979','1980','1981',
                 '1982','1983','1984','1985','1986','1987',
                 '1988','1989','1990','1991','1992','1993',
                 '1994','1995','1996','1997','1998','1999',
                 '2000','2001','2002','2003','2004','2005',
                 '2006','2007','2008','2009','2010','2011',
                 '2012','2013','2014','2015','2016','2017',
                 '2018','2019','2020','2021'), 
               names_to = "Year", 
               values_to = "CarbonEmissions(Kt)")

carbon_rawdata$`CarbonEmissions(Kt)`<-formattable(carbon_rawdata$`CarbonEmissions(Kt)`,format="f",digits=5)                            

population_rawdata <- population_rawdata %>%
  pivot_longer(c('X1960','X1961','X1962','X1963','X1964','X1965',
                 'X1966','X1967','X1968','X1969',
                 'X1970','X1971','X1972','X1973','X1974','X1975',
                 'X1976','X1977','X1978','X1979','X1980','X1981',
                 'X1982','X1983','X1984','X1985','X1986','X1987',
                 'X1988','X1989','X1990','X1991','X1992','X1993',
                 'X1994','X1995','X1996','X1997','X1998','X1999',
                 'X2000','X2001','X2002','X2003','X2004','X2005',
                 'X2006','X2007','X2008','X2009','X2010','X2011',
                 'X2012','X2013','X2014','X2015','X2016','X2017',
                 'X2018','X2019','X2020','X2021'), 
               names_to = "Year", 
               values_to = "Population")

population_rawdata <- population_rawdata %>% 
  rename("CountryCode" = "Country.Code") %>%
  rename("CountryName" = "Country.Name")
population_rawdata <- population_rawdata %>% mutate(Year=str_replace(Year,"X",""))
#filtering data from 1970-2021
population_rawdata <- population_rawdata %>% filter(Year>=1970)

area_rawdata <- area_rawdata %>%
  pivot_longer(c('X1960','X1961','X1962','X1963','X1964','X1965',
                 'X1966','X1967','X1968','X1969',
                 'X1970','X1971','X1972','X1973','X1974','X1975',
                 'X1976','X1977','X1978','X1979','X1980','X1981',
                 'X1982','X1983','X1984','X1985','X1986','X1987',
                 'X1988','X1989','X1990','X1991','X1992','X1993',
                 'X1994','X1995','X1996','X1997','X1998','X1999',
                 'X2000','X2001','X2002','X2003','X2004','X2005',
                 'X2006','X2007','X2008','X2009','X2010','X2011',
                 'X2012','X2013','X2014','X2015','X2016','X2017',
                 'X2018','X2019','X2020','X2021'), 
               names_to = "Year", 
               values_to = "Area(sq.km)")

area_rawdata <- area_rawdata %>% 
  rename("CountryCode" = "Country.Code") %>%
  rename("CountryName" = "Country.Name")
area_rawdata <- area_rawdata %>% mutate(Year = str_replace(Year, "X", ""))
#filtering data from 1970-2021
area_rawdata <- area_rawdata %>% filter(Year>=1970)

#Merging dataframes together into a single dataframe
country_area_population <- inner_join(population_rawdata,area_rawdata, by = c("CountryName","CountryCode","Year"))
country_area_population_carbon <- inner_join(carbon_rawdata,country_area_population,by = c("CountryName","CountryCode","Year"))

#Deriving new columns for analysis
country_area_population_carbon <- country_area_population_carbon %>%
                                    mutate(`PopulationDensity(PPKM)` = Population/`Area(sq.km)`)
country_area_population_carbon$`PopulationDensity(PPKM)`<-formattable(country_area_population_carbon$`PopulationDensity(PPKM)`,format="f",digits=3)
country_area_population_carbon <- country_area_population_carbon %>%
                                    mutate(`CarbonEmissionsPerCapita` = `CarbonEmissions(Kt)`/Population)
#reordering columns
country_area_population_carbon <- country_area_population_carbon[,c(1,2,3,6,5,7,4,8)]
#Final Tidy Data
head(country_area_population_carbon)
str(country_area_population_carbon)

##########################################
#Section 3.1: World Map of average carbon emissions from 1970-2021
##########################################

#Calculating average carbon emissions grouped by country
mapData <- country_area_population_carbon %>%
  group_by(CountryName) %>%
  summarise(AverageEmissions = mean(`CarbonEmissions(Kt)`))

mapData <- mapData %>%
  mutate(across('CountryName',str_replace, 'United States','USA')) %>%
  mutate(across('CountryName',str_replace, 'Russian Federation','Russia')) %>%
  mutate(across('CountryName',str_replace, 'United Kingdom','UK'))

worldData=map_data("world")  #to take the world data

combined_country <- worldData[mapData$CountryName %in% mapData$CountryName, ]
combined_country$value <- mapData$AverageEmissions[match(combined_country$region, mapData$CountryName)]

#Figure 3.1.1: Average Carbon Emissions by Countries from 1970 to 2021
ggplot(combined_country, aes(x=long,y=lat, group = group, fill = value)) +
  geom_polygon(colour =  "white") +
  scale_fill_continuous(low = "lightblue", high = "orange", guide = "colorbar") +
  theme_bw() +
  labs(fill = "Average Carbon Emissions", title = "Average Carbon Emissions by Countries from 1970 to 2021", x="", y="") +
  scale_y_continuous(breaks = c()) +
  scale_x_continuous(breaks = c()) +
  theme(panel.border = element_blank())

##################################################
##Section 3.2: Correlation between population and Carbon Emissions over the years 
#################################################
coeff <- 100 #setting a variable for scaling the graph

#Grouping year to find trends of total population and total carbon emissions
year_population_emission <- country_area_population_carbon %>%
  filter(!is.na(Population) & !is.na(`PopulationDensity(PPKM)`)) %>%
  group_by(Year) %>%
  summarise(TotalPopulation = sum(Population), 
            TotalEmissions = sum(`CarbonEmissions(Kt)`)
            )

#Figure 3.2.1: Correlation between population and carbon emissions
year_population_emission %>%
  ggplot(aes(x=as.integer(Year))) +
  geom_bar(aes(y= TotalPopulation/coeff),stat="identity",fill = "#24495c") +
  geom_line(aes(y= TotalEmissions, color = "Total Emissions")) +
  labs(title = "Correlation between population and Carbon Emissions",
       x= "Year", y="Total Carbon Emissions in world(kT), Total Population in the world", color = "Legend")

###############################################
## Section 3.3: Top 10 countries with highest carbon emissions 
##############################################

#Finding the top 10 carbon emitting countries in each year 
ranked_countries <- country_area_population_carbon %>%
  arrange(Year,CountryName,`CarbonEmissions(Kt)`) %>%
  group_by(Year) %>%
  mutate(rank = rank(desc(`CarbonEmissions(Kt)`))) %>%
  filter(rank<=10)

#Figure 3.3.1: Top 10 highest carbon emitting carbon and its emission across years
ranked_countries %>%
  ggplot(aes(x= as.integer(Year), y = `CarbonEmissions(Kt)`, colour = CountryName)) +
  geom_line() +
  labs(title = "Top 10 countries with highest carbon emission",x= "Year", y="Carbon Emissions (kT)")

#Figure 3.3.2: Stats of Top 10 countries for 2021: Land area vs. Carbon Emissions
ranked_countries %>%
  filter(Year ==2021) %>%
  ggplot(aes(x = `Area(sq.km)`, y = `CarbonEmissions(Kt)`, colour = CountryName, label = CountryName)) +
  geom_point() +
  geom_text(size = 2.5,hjust=0.5, vjust=1) +
  labs(title = "Stats of Top 10 countries for 2021: Land area vs. Carbon Emissions", x = "Area(sq.km)", y = "Carbon Emissions(Kt)")

#Figure 3.3.3: Stats of Top 10 countries for 2021: Population vs. Carbon Emissions
ranked_countries %>%
  filter(Year ==2021) %>%
  ggplot(aes(x = Population, y = `CarbonEmissions(Kt)`, colour = CountryName,label = CountryName)) +
  geom_point() +
  geom_text(size = 2.5,hjust=0.5, vjust=1) +
  labs(title = "Stats of Top 10 countries for 2021: Population vs. Carbon Emissions", x = "Population", y = "Carbon Emissions(Kt)")



################################################
## Section 3.4: Correlation between population density and Carbon Emissions per capita
###############################################

#Finding outlier countries and filtering 2021 data
PopDen_Carbon_countries <- country_area_population_carbon %>%
  filter(Year == 2021) %>%
  mutate(Outlier_name = ifelse((CarbonEmissionsPerCapita> 0.03), CountryName, NA))

PopDen_Carbon_countries <- PopDen_Carbon_countries[,c("CountryName", "PopulationDensity(PPKM)", "CarbonEmissionsPerCapita", "Outlier_name")] 

#Figure 3.4.1: Population Density vs Carbon Emissions Per Capita in 2021
PopDen_Carbon_countries %>%
  ggplot(aes(x = `PopulationDensity(PPKM)`,y = CarbonEmissionsPerCapita, label = Outlier_name )) +
  geom_boxplot(alpha = 0.5, color = "blue") +
  geom_text(size = 2.5, hjust=-8.5, vjust=1) +
  labs(title = "Population Density vs CarbonEmissionsPerCapita in 2021")


