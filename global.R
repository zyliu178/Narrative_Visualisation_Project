library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(plotly)
library(stringr)
library(scales)
library(leaflet.extras)

# Read both data set 
Crashes_Last_Five_Years <- read_csv("Crashes_Last_Five_Years.csv")
Traffic_Volume <- read_csv("Traffic_Volume.csv")

# Tidy two data set 
data_a  <- Crashes_Last_Five_Years %>%
  mutate(ACCIDENT_DATE = dmy(Crashes_Last_Five_Years$ACCIDENT_DATE))
data_a <- data_a %>%
  mutate(year = year(data_a$ACCIDENT_DATE),
         month = month(data_a$ACCIDENT_DATE),
         day = day(data_a$ACCIDENT_DATE),
         weekday = wday(data_a$ACCIDENT_DATE)) %>%
  dplyr::select(year:weekday, ACCIDENT_TYPE, LONGITUDE:LATITUDE, SPEED_ZONE, LGA_NAME, REGION_NAME, SEVERITY)

data_b <- Traffic_Volume %>% select(LGA_SHORT_NM, RGN_LONG_NM, ALLVEHS_AADT)
data_b <- data_b %>% group_by(LGA_SHORT_NM) %>% summarise(AVERAGE_CAR = sum(ALLVEHS_AADT))

# Join data set
data_ab <- data_a %>%
  inner_join(data_b, by = c("LGA_NAME" = "LGA_SHORT_NM")) 

# Remove unknow speed limit
data_all <- data_ab %>% dplyr::filter(SPEED_ZONE != "Not known", 
                                     SPEED_ZONE != "Camping grounds or off road", 
                                     SPEED_ZONE != "Other speed limit") 
data_all <- data_all %>%
  mutate(SPEED_ZONE =  as.numeric(str_remove(data_all$SPEED_ZONE, "km/hr"))) %>%
  na.omit() %>%
  filter(year != 2019)

# variables
LGA <- unique(data_all$LGA_NAME)
Sevr <- c("Serious injury accident", "Fatal accident", "Other injury accident")
Year <- unique(data_all$year) %>% 
  as.numeric()
Speed <- unique(data_all$SPEED_ZONE) %>%
  sort()

LGAAll <- c('All', LGA)
YearAll <- c('All', Year)

theme_hua <- function (base_size = 12, base_family = "",
                       
                       plot.title = element_text(hjust=0.5, size = 18, face = "bold", 
                                                 margin=margin(t = 20, b = 20, unit = "pt")),
                       legend.text = element_text(size = 10),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14, face = "bold"),
                       
                       axis.line = element_line(color = 'black'), 
                       panel.border = element_blank(),
                       
                       #'gray96', i.e. #F4F4F4, is a comfortable color for background.
                       panel.background = element_rect(fill="gray96", colour=NA),
                       plot.background = element_rect(fill="gray96", colour=NA), 
                       legend.background = element_rect(fill="transparent", colour=NA),
                       
                       legend.key = element_rect(fill="transparent", colour=NA), ...
) { 
  
  theme_bw(base_size, base_family) + 
    # "+" is to update target theme_bw w/ new specified changes
    # "%+replace%"  is to only use the following specified changes, 
    # all other unspecified params in the original theme are completely overriden.
    theme(
      # new changes 
      plot.title = plot.title,
      legend.text = legend.text,
      axis.text = axis.text,
      axis.title = axis.title,
      
      axis.line = axis.line, 
      panel.border = panel.border,
      
      panel.background = panel.background,
      plot.background = plot.background, 
      legend.background = legend.background,
      
      legend.key = legend.key, ...
    )
}

# fine control:
#"expand" param: default: data is placed some distance away from the axes

scale_y_continuous_hua <- function (expand = c(0, 0), 
                                    labels = scales::comma, ...) {
  scale_y_continuous(expand = expand, labels = labels, ...)
}

scale_x_continuous_hua <- function (expand = c(0, 0), 
                                    labels = scales::comma, ...) {
  scale_x_continuous(expand = expand, labels = labels, ...)
}

