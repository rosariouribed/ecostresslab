

## LOAD LIBRARIES ##
library(tidyverse)
library(chron)
library(lubridate)
library(RColorBrewer)
library(purrr)

## LOAD DATA ##
path <- "C:/Users/np658/Dropbox (YSE)/Nathalia/Eddy Flux/WeeklyEddyCo/"

allfiles = list.files(path = "C:/Users/np658/Dropbox (YSE)/Nathalia/Eddy Flux/WeeklyEddyCo/", recursive = TRUE)
allfiledates = data.frame(year = as.numeric(substr(allfiles, 2, 5)),
                          day = as.numeric(substr(allfiles, 6, 8)),
                          hour = as.numeric(substr(allfiles, 9, 10)),
                          minutes = substr(allfiles, 11, 12))

eddyco_files <- paste0(path,list.files(path, pattern = "*.csv"))
eddyco_lists <- lapply(eddyco_files, read_csv)
eddyco <- do.call(rbind, eddyco_lists)
eddyco[eddyco == -9999] = NA

# Repeat rows of 'allfiledates' to match the size of 'eddyco'
n <- nrow(eddyco)
allfiledates_repeated <- allfiledates[rep(seq_len(nrow(allfiledates)), each = n), ]

# Add date and time components to 'eddyco'
eddyco$year <- allfiledates_repeated$year
eddyco$day <- allfiledates_repeated$day
eddyco$hour <- allfiledates_repeated$hour
eddyco$minutes <- allfiledates_repeated$minutes

## CHANGE DATE ##

date_info <- str_match(eddyco_files, "C(\\d{4})(\\d{3})(\\d{2}).csv")


allfiledates <- data.frame(
  year = as.numeric(substr(eddyco_lists, 7, 10)),
  day = as.numeric(substr(eddyco_lists, 11, 13)),
  hour = as.numeric(substr(eddyco_lists, 14, 15)),
  minutes = as.numeric(substr(eddyco_lists, 16, 17))
)


## CREATE DATAFRAME WITH ALL FILES FOR NEEDED DATES ##
alldates = seq.Date(from = as.Date("2023-05-01"), to = as.Date("2023-05-31"), by = "day")
alltimes = chron(time = paste(c(0:23), ':', 0, ':', 0))
alldatetime <- merge(alldates, alltimes) %>%
  dplyr::rename(date = x, time = y) %>%
  mutate(year = year(date), hour = chron::hours(time), week = week(date))

allavaildata = alldatetime %>%
  select(-date, -time) %>%
  left_join(., filedatescount, by=c("year", "week", "hour")) %>%
  mutate(count2 = as.factor(ifelse(is.na(n), "0", 
                                   ifelse(n > 7, ">7", as.character(n))))) %>%
  mutate(count2 = ordered(count2, levels = c("0", "1", "2", "3", "4", "5", "6", "7", ">7")))

## PLOT HEAT MAP ##
mypal = rev(brewer.pal(name = "YlOrRd", n = 9))
dataheatmap = ggplot(allavaildata,aes(x= week, y= hour, fill= count2)) +
  #add border white colour of line thickness 0.25
  geom_tile(colour= "white",linewidth= 0.25) +
  #remove extra space
  scale_y_continuous(expand=c(0,0), breaks=seq(0,23,2), labels=seq(0,23,2)) +
  #define new breaks on x-axis
  scale_x_continuous(expand=c(0,0), breaks=seq(0,52,4), labels=seq(0,52,4)) +
  scale_fill_manual(values = mypal, name = "Number of available files") +
  #theme options
  theme(
    legend.position = "top",
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank()) +
  facet_wrap(~year)

plot(dataheatmap)







