library(tidyverse)
library(plotly)

#Setting the path to the current directory of the R Script
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

'
STEP: Data Import
'
data = read.csv(file = "./Dataset/Air_Pollution_Required_Data.csv")


'
STEP: Data Claening 
'
#Converting the dataframe in tibble format.
data = as_tibble(data)

#Creating a new column Date and Time using Measure.Date and Measure.Time columns.
#data = data %>% mutate(Date = as.Date(Measure.Date, '%d-%m-%Y'))
#data = data %>% mutate(Time = lubridate::hms(Measure.Time))

#Certain rows in Pollutants contains rows with values as 0, which is incorrect entries and affects the anlysis hence,
#they need to be cleaned.
#data = filter(data, Pollutant.Avg!=0)

#Creating a datetime column
#data$Date_Time = (paste(data$Date, data$Measure.Time))
#data$Date_Time = lubridate::ymd_hms(data$Date_Time)

#The analysis needs to be done on City Level or State Level, So we dont need data on Station Level.
#data = data %>% group_by(Country, State, City, Pollution.ID, Date_Time) %>% summarise(Country, State, City, Pollution.ID, Date, Time, Date_Time, Pollutant.Avg = mean(Pollutant.Avg), Pollutant.Max = max(Pollutant.Max), Pollutant.Min = min(Pollutant.Min))


#Making Required Changes
data$Date_Time = lubridate::ymd_hms(data$Date_Time)


#Creating Groups based on AQI Breakpoint
data = data %>% mutate(AQI.Category = ifelse(data$Pollutant.Avg <= 50, "Good", 
                                             ifelse(data$Pollutant.Avg <= 100, "Moderate",
                                                    ifelse(data$Pollutant.Avg <= 150, "Unhealthy For Sensetive Groups", 
                                                           ifelse(data$Pollutant.Avg <= 200, "Unhealthy", 
                                                                  ifelse(data$Pollutant.Avg <= 300, "Very Unhealthy", "Hazardours"))))))



#Date Segmentation:
# Diwali Week - 25 Oct to 2 Nov
# Stubble Burning Period - 15 Oct to 15 Nov (Taking from 3 Nov to 30 Nov)
data = data %>% mutate(Period = ifelse(data$Date_Time >= '2019-10-25 00:00:00' & data$Date_Time <= '2019-11-02 23:59:59', "Diwali", 
                                             ifelse(data$Date_Time >= '2019-11-03 00:00:00' & data$Date_Time <= '2019-12-03 23:59:59', "Stubble Burning",
                                                    ifelse(data$Date_Time >= '2020-03-25 00:00:00' & data$Date_Time <= '2020-04-14 23:59:59', "Lockdown Phase 1",
                                                                  ifelse(data$Date_Time >= '2020-04-15 00:00:00' & data$Date_Time <= '2020-05-03 23:59:59', "Lockdown Phase 2",
                                                                         ifelse(data$Date_Time >= '2020-05-04 00:00:00' & data$Date_Time <= '2020-05-17 23:59:59', "Lockdown Phase 3",
                                                                                ifelse(data$Date_Time >= '2020-05-18 00:00:00' & data$Date_Time <= '2020-05-31 23:59:59', "Lockdown Phase 4",
                                                                                       ifelse(data$Date_Time >= '2020-06-01 00:00:00' & data$Date_Time <= '2020-06-30 23:59:59', "Unlock 1.0",
                                                                                              ifelse(data$Date_Time >= '2020-07-01 00:00:00' & data$Date_Time <= '2020-07-31 23:59:59', "Unlock 2.0",
                                                                                                     ifelse(data$Date_Time >= '2020-08-01 00:00:00' & data$Date_Time <= '2020-08-31 23:59:59', "Unlock 3.0",
                                                                                                            ifelse(data$Date_Time >= '2020-09-01 00:00:00' & data$Date_Time <= '2020-09-30 23:59:59', "Unlock 4.0", "Normal Period")))))))))))
                                                                                


'
Important Links
'
#https://www.nytimes.com/2020/12/04/world/asia/india-farmers-protest-pollution-coronavirus.html#:~:text=Farmers%20traditionally%20play%20a%20role,air%20pollution%20during%20the%20period.
#https://www.bbc.com/news/world-asia-india-54930380

'
STEP: Data Analysis
'
#Analysis on Categorical Variable.
#Checking the Pollutant Distribution of 5 metro cities
metro_cities = filter(data, (City=='Delhi'|City=='Mumbai'|City=='Kolkata'|City=='Bengaluru'|City=='Hyderabad'))

box_plot <- metro_cities %>%
  plot_ly(
    type = 'box', 
    x = ~City,
    y = ~Pollutant.Avg, 
    customdata = ~Pollution.ID,
    color = ~City,
    transforms = list(
      list(
        type = 'filter',
        target = 'customdata',
        operation = '=',
        value = unique(metro_cities$Pollution.ID)[1]
      )
    )
  ) %>% layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[1]),
               label = unique(metro_cities$Pollution.ID)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[2]),
               label = unique(metro_cities$Pollution.ID)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[3]),
               label = unique(metro_cities$Pollution.ID)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[4]),
               label = unique(metro_cities$Pollution.ID)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[5]),
               label = unique(metro_cities$Pollution.ID)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[6]),
               label = unique(metro_cities$Pollution.ID)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities$Pollution.ID)[7]),
               label = unique(metro_cities$Pollution.ID)[7])
        )
      )
    )
  )

box_plot


#Data is provided on Station level and each cities contains multiple stations, so I grouped the station level data 
#to city level by taking mean of all the rows for that city and that specific Date time.
metro_cities_filter = filter(data, (City=='Delhi'|City=='Mumbai'|City=='Kolkata'|City=='Bengaluru'|City=='Hyderabad') & Pollution.ID=='PM2.5')
#metro_cities_filter = metro_cities_filter %>% group_by(City, Date_Time) %>% summarise(Country, State, City, Pollution.ID, Date_Time, AQI.Category, Pollutant.Avg = mean(Pollutant.Avg))

line_plot <- metro_cities_filter %>%
  plot_ly(
    type = 'scatter',
    mode = 'line',
    x = ~Date_Time,
    y = ~Pollutant.Avg, 
    customdata = ~City,
    customdata1 = ~Period,
    color = ~Pollution.ID,
    transforms = list(
      list(
        type = 'filter',
        target = 'customdata',
        operation = '=',
        value = unique(metro_cities_filter$City)[1]
      ),
      list(
        type = 'filter',
        target = 'customdata1',
        operation = '=',
        value = unique(metro_cities_filter$Period)[1]
      )
    )
    
  ) %>% layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        x = -0.1,
        y = 1,
        label = 'Cities',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities_filter$City)[1]),
               label = unique(metro_cities_filter$City)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities_filter$City)[2]),
               label = unique(metro_cities_filter$City)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities_filter$City)[3]),
               label = unique(metro_cities_filter$City)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities_filter$City)[4]),
               label = unique(metro_cities_filter$City)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(metro_cities_filter$City)[5]),
               label = unique(metro_cities_filter$City)[5])
        )
      )
    ),
    list(
      type = 'buttons',
      x = -0.1,
      y = 0.7,
      label = 'Period',
      active = 0,
      buttons = list(
        list(method = "restyle",
             args = list("transforms[1].value1", unique(metro_cities_filter$Period)[1]),
             label = unique(metro_cities_filter$Period)[1]),
        list(method = "restyle",
             args = list("transforms[1].value1", unique(metro_cities_filter$Period)[2]),
             label = unique(metro_cities_filter$Period)[2]),
        list(method = "restyle",
             args = list("transforms[1].value1", unique(metro_cities_filter$Period)[3]),
             label = unique(metro_cities_filter$Period)[3]),
        list(method = "restyle",
             args = list("transforms[1].value1", unique(metro_cities_filter$Period)[4]),
             label = unique(metro_cities_filter$Period)[4]),
        list(method = "restyle",
             args = list("transforms[1].value1", unique(metro_cities_filter$Period)[5]),
             label = unique(metro_cities_filter$Period)[5])
      )
    )
  )
    

line_plot
