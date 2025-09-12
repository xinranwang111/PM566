# Preset ----
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))

dir <- "/Users/xinranwang/Documents/Course/25Fall/PM566"
idir <- "/Users/xinranwang/Documents/Course/25Fall/PM566/Data"
sdir <- "/Users/xinranwang/Documents/Course/25Fall/PM566/Scripts"

filetype <- "Class3"
#source(paste0(sdir, "/", filetype, ".func.R"))

odir <- paste0(dir, "/Output")
invisible(suppressMessages(suppressWarnings((dir.create(odir, recursive = TRUE)))))

infile <- "met_all.gz"

# Start ----
library(tidytuesdayR)
met <- fread(paste0(idir, "/", infile))

#. Formulate a Question
#  It is a good idea to first have a question such as:
#  Which weather stations reported the hottest and coldest daily temperatures?
#  What day of the month was on average the hottest?
#  Is there correlation between temperature and humidity in my dataset?

dim(met)
nrow(met)
ncol(met)

str(met)
summary(met[,8:13])

# integer
table(met$hour)
table(met$month)
barplot(table(met$hour))
table(met$hour)
table(met$month)
barplot(table(met$hour))

# numerical
table(met$year)
summary(met$lat)
summary(met$lon)

layout(matrix(1:2, nrow=1))
hist(met$lat)
hist(met$lon)

layout(1)

#
summary(met$temp)
hist(met$temp)

# percent missing
mean(is.na(met$temp))

#
met_ss <- met[met$temp == -40.00, c('hour','lat','lon','elev','wind.sp')]
dim(met_ss)
summary(met_ss)

#
library(dplyr)
met_ss <- filter(met, temp == -40.00) |> 
  select(USAFID, day, hour, lat, lon, elev, wind.sp)

dim(met_ss)
summary(met_ss)

# clean and sort
met <- met[met$temp > -40, ]
met <- met[!is.na(met$temp), ]
met <- met[order(met$temp), ]
head(met)[,c(1,8:10,24)]
tail(met)[,c(1,8:10,24)]

# summary statistics: temp, lat and lon (by site and day)
library(dplyr)
met_daily <- summarize(met,
                       temp = mean(temp),
                       lat = mean(lat),
                       lon = mean(lon),
                       elev = mean(elev),
                       .by = c(USAFID, day))

summarize(met,
          temp = mean(temp),
          .by = c(USAFID, day))

# new df: average daily temperature
met_daily <- met_daily[order(met_daily$temp), ]
head(met_daily)
tail(met_daily)

# exploratory graphs
hist(met$temp)
hist(met_daily$temp)

boxplot(met$temp, col = "blue")
boxplot(met_daily$temp, col = "blue")

plot(met_daily$lon, met_daily$lat, asp=1)

# 
met_stations <- (unique(met[,c("lat","lon")]))  
dim(met_stations)

# where the weather stations are located
library(leaflet)
leaflet(met_stations) |> 
  addProviderTiles('CartoDB.Positron') |> 
  addCircles(lat = ~lat, lng = ~lon,
             opacity = 1, fillOpacity = 1, radius = 400)

# max and min daily temperatures
min <- met_daily[1, ]               # First observation
max <- met_daily[nrow(met_daily), ] # Last observation

leaflet() |> 
  addProviderTiles('CartoDB.Positron') |> 
  addCircles(
    data = min,
    lat = ~lat, lng = ~lon, popup = "Min temp.",
    opacity = 1, fillOpacity = 1, radius = 400, color = "blue"
  ) |>
  addCircles(
    data = max,
    lat = ~lat, lng = ~lon, popup = "Max temp.",
    opacity=1, fillOpacity=1, radius = 400, color = "red"
  )

# scatter plot: pairwise relationships (lat vs temp)
plot(met_daily$lat, met_daily$temp, pch=16, cex=0.5)

# scatter plot: add a simple linear regression line
mod <- lm(temp ~ lat, data = met_daily)
met_daily[, plot(
  lat, temp, pch=19, cex=0.5, 
  main = "Temperature and Latitude", 
  xlab = "Latitude", ylab = "Temperature (deg C)")
]
abline(mod, lwd=2, col="red")

library(ggplot2)
ggplot(data = met_daily, mapping = aes(x = lat, y = temp)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature and Latitute", xlab  = "Latitute", y = "Temperature (deg C)")

rm(list = ls())

###############################################################

tuesdata <- tidytuesdayR::tt_load(2025, week = 36) 

countnry_lists <- tuesdata$countnry_lists
rank_by_year <- tuesdata$rank_by_year

table(rank_by_year$year)







