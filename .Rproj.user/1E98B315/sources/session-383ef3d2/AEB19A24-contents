# Mempersiapkan libraries

library(shiny)
library(shinydashboard)
library(DT)

options(scipen = 99) # me-non-aktifkan scientific notation
library(dplyr) # data prep
library(lubridate) # date data prep
library(ggplot2) # visualisasi statis
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot

# read data
data_monitoring <- read.csv("dataset/data_tree_monitoring_2024.csv")

# change datatype
monitoring_clean <- data_monitoring %>% 
  filter(total_tree > 0) %>% 
  mutate_at(.vars = c("monitoring_id", "species_name", "tree_planting_system"), 
            .funs = as.factor) %>% 
  mutate(tree_planting_date = ymd(tree_planting_date),
         date_monitoring = ymd(date_monitoring))