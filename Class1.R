# PRESET ----
DIR <- "/Users/xinranwang/Documents/Course/25Fall/PM566"
IDIR <- "/Users/xinranwang/Documents/Course/25Fall/PM566/Data"
SDIR <- "/Users/xinranwang/Documents/Course/25Fall/PM566/Scripts"
LOCATION <- "inclass"

WEEK <- "1"
INPUT <- "Billboard Hot 100 Number Ones Database.xlsx"

ODIR <- paste0(DIR, "/", LOCATION, "/", WEEK, "/output")
invisible(suppressMessages(suppressWarnings((dir.create(ODIR, recursive = TRUE)))))

source(paste0(SDIR, "/", LOCATION, "/", WEEK, ".func.R"))

# START ----
df <- fread(paste0(IDIR, "/", INPUT), header=T, sep=',')

#
library(tidytuesdayR)

# Billboard Hot 100 Number Ones
tuesdata <- tidytuesdayR::tt_load(2025, week = 34) 

#
billboard <- tuesdata$billboard
topics <- tuesdata$topics
rm(tuesdata)

View(billboard)

hist(billboard$bpm)
min(billboard$bpm, na.rm = T)

hist(billboard$danceability)

plot(billboard$bpm, billboard$danceability)
plot(billboard$bpm, billboard$energy)
plot(billboard$energy, billboard$danceability)

pairs(billboard[,c('energy', 'happiness', 'danceability')])

table(billboard$cdr_genre)
barplot(table(billboard$cdr_genre))

tab <- table(billboard$cdr_genre)
names(tab)[tab >= 10]

genres2keep <- names(tab)[tab >= 10]
billboard <- billboard[billboard$cdr_genre %in% genres2keep,]

#avg_dance <- sapply(genres2keep, function(gen){ mean(billboard$danceability[billboard$cdr_genre]) })
#barplot(avg_dance, las=2)

# Billboard Hot 100 Number Ones
tuesdata <- tidytuesdayR::tt_load(2025, week = 34) 

#

library(lubridate)
library(tidyverse)
library(lattice)
library(tidytuesdayR)

# Billboard Hot 100 Number Ones
tuesdata <- tidytuesdayR::tt_load(2025, week = 34) 
df <- tuesdata$billboard
df <- df %>% mutate(year = year(date))

major_genres <- c("Pop", "Rock", "Blues", "Electronic/Dance", 
                  "Folk/Country", "Funk/Soul", "Hip Hop", 
                  "Jazz", "Latin", "Reggae", "March", "Polka")

df_clean <- df %>%
  mutate(
    major_genre = sapply(cdr_genre, function(x) {
      mg <- major_genres[str_detect(x, major_genres)]
      if (length(mg) == 0) NA else mg[1] 
    })
  ) %>%
  group_by(year, major_genre) %>%
  mutate(mean_overall_rating = mean(overall_rating)) %>%
  filter(major_genre %in% c("Electronic/Dance", "Rock", "Pop", "Hip Hop"))

png(paste0("./extra_credit1.png"), width = 10, height = 7, res=300, unit = "in")
xyplot(overall_rating ~ year,
       data = df_clean,
       groups = major_genre, type = "l",
       main = "Trends in Mean Overall Rating by Genre (1950s–2020s)",
       ylab = "Mean Overall Rating",
       xlab = "Year",
       auto.key=list(space="bottom", columns = 4, 
                     title="Genre", cex.title = 1.2),
       par.settings = list(
         superpose.line = list(col = c("blue", "red", "skyblue", "pink"))
       )
)
dev.off()



