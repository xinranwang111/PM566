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

infile <- ""


#
world_map <- map_data("world")
ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "darkgray", color = "white")

us_map <- map_data("state")
ggplot(data = us_map, aes(x = long, y = lat, fill = region)) +
  geom_polygon(color = "white")

#
library(tidytuesdayR)
library(ggplot2)
library(ggwordcloud)

tuesdata <- tidytuesdayR::tt_load(2025, week = 37) 

cuisines <- tuesdata$cuisines
rm(tuesdata)

avg_cuisines <- cuisines %>%
  group_by(country) %>%
  mutate(mean.rating = mean(avg_rating, na.rm = T),
         mean.calories = mean(calories, na.rm = T)) %>% 
  
country_cuisines <- cuisines %>%
  count(country, name = "num_entry")

p <- ggplot(country_cuisines, aes(label = country, size = num_entry)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 30) +
  labs(title = "Count of Cuisines by Country", size = 20) +
  theme_minimal() 
ggsave(paste0(odir, "/extra_credit4.png"), p, width = 8, height = 5)
  


