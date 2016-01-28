library(gdata)
library(dplyr)
library(maps)
library(ggplot2)

# data set of US superfund sites can be downloaded from http://www.epa.gov/superfund/superfund-data-and-reports
# search for foia-004_-_all_final_npl_sites_0.xlsx
superfund <- read.csv("superfund.csv") %>%
  mutate(
    Latitude = as.numeric(gsub("[^0-9.-]", "", Latitude)), 
    Longitude = as.numeric(gsub("[^0-9.-]", "", Longitude)) 
  ) 

# download census data from https://www.census.gov/hhes/www/income/data/statemedian/
# 2014 American Community Survey 1-Year Estimates
# MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (IN 2014 INFLATION-ADJUSTED DOLLARS) BY NUMBER OF EARNERS IN FAMILY - Universe: Families
income.by.state <- read.xls("medincearnersandstate14.xls", sheet = 1, header = FALSE) %>%
        transmute(column3 = gsub(",", "", V3))

# seperate state and income into two columns
income.by.state <- data.frame(
  region = tolower(income.by.state$column3[seq(3, by = 7, length.out = 51)]),
  med_income = as.numeric(income.by.state$column3[seq(5, by = 7, length.out = 51)]) # selecting "Total" for use in map
  ) 

income.by.state$region <- as.character(income.by.state$region) 

choro <- map_data("state") %>%  # retrieve state shape data from built in data set map_data
  inner_join(income.by.state, by = "region") %>%  # merge state shape and state income data
  arrange(order)

# draw map
ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = med_income)) +
  labs(x="longitude", y="latitude", 
       title = "United States Map of Superfund Sites and Median State Income") +
  scale_fill_gradientn("Median Income $", 
                       colours =c("#4CFE7C","#3DCD62","#2F9E49","#207132","#12471D","#042108")) + # makes fill color light-dark green
  coord_map("albers",  at0 = 45.5, lat1 = 29.5, 
            xlim = c(-125, -68) , # changes framing of map
            ylim = c(24, 51)) +
  geom_point(data=superfund, aes(Longitude, Latitude,  # plot superfund sites
             color ="Superfund"),  size = .5, 
             na.rm = TRUE, show.legend = TRUE) + 
  scale_colour_manual(name="", values = ("Superfund"= "yellow")) + 
  theme(legend.key=element_rect(fill="black")) # change background superfund legend to black



