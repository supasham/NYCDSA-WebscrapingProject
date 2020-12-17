################################################################################
# Shameer SUkha  - Webscraping project
# global.R
################################################################################

library(shinydashboard)
library(shiny)
library(tidyverse)
library(googleVis)
library(DT)

# Would normally load data from .csv files but for project 
# use saved workspace as example
setwd("~/NYCDSA/Python/NYCDSA-WebscrapingProject")
df = read.csv('final_redwine.csv', header = TRUE, encoding = 'UTF-8')


################################################################################
# Global R data wrangling and cleanups
################################################################################


factortbl <- df %>%
  filter(., viv_name != "missed") %>% 
  filter(., score != 0) %>% 
  select(., lcbo_name, lcbo_region, lcbo_country, vintage, container, size.mL, price, unit_price , score, num_reviews, critic, critic_score)


max_reviews = round(max(df$num_reviews),-3)
max_price = round(max(df$price),-3)

################################################################################