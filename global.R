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

# Join today and yesterday's data to be able to calculate changes
# avgdata <- inner_join(avgdata, avgyest,
#                       by = c("CMA.Group.Id", 
#                              "Sector", 
#                              "Rating", 
#                              "Region", 
#                              "Tenor"), 
#                       suffix = c(".t",".y"))
# 
# # Replace NAs with zeros for numeric columns
# avgdata <- avgdata %>%
#   replace_na(list(GlobalSpread.t = 0, SectorFactor.t = 0, 
#                   RatingFactor.t = 0, RegionFactor.t = 0,
#                   GlobalSpread.y = 0, SectorFactor.y = 0, 
#                   RatingFactor.y = 0, RegionFactor.y = 0))
# 
# # Replace blanks with better label
# avgdata$Region[avgdata$Region == ""] = "No Region"
# avgdata$Rating[avgdata$Rating == ""] = "No Rating"
# 
# # Calculate changes in CDS spread levels and factors
# avgdata <- avgdata %>%
#   mutate(., sprdchanges = (ParSpreadMid.t - ParSpreadMid.y),
#          tenorfactorchng = (GlobalSpread.t / GlobalSpread.y - 1),
#          sectorfactorchng = (SectorFactor.t / SectorFactor.y - 1),
#          ratingfactorchng = (RatingFactor.t / RatingFactor.y - 1),
#          regionfactorchng = (RegionFactor.t / RegionFactor.y - 1)
#          )
# 
# # Truncate long names for better display without loss of meaning
# avgdata$Sector[avgdata$Sector == "Consumer Cyclical"] <- "Consumer Cycl."
# avgdata$Sector[avgdata$Sector == "Consumer Non-Cyclical"] <- "Consumer Non-Cycl."
# avgdata$Sector[avgdata$Sector == "Sovereigns/States/Agencies"] <- "SSAs"
# 
# # Create choices for selector widgets
# sectorchoice <- unique(avgdata$Sector)
# ratingchoice <- unique(avgdata$Rating)
# regionchoice <- unique(avgdata$Region)
# tenorchoice <- unique(avgdata$Tenor)
# 
# 
# ################################################################################
# # Create factor dataframes for heatmaps and data table
# ################################################################################
# tmpdf <- avgdata %>%
#   filter(., (Region != "No Region") & 
#            (Rating != "No Rating") & 
#            (Sector != "All")) %>%
#   select(., Sector, Rating, Region, Tenor, 
#          tenorfactorchng, sectorfactorchng, ratingfactorchng, regionfactorchng)
# 
# sectordf <- tmpdf %>%
#   mutate(., Class = "Sector") %>%
#   select(., Category = Sector, Class, Tenor, Factor = sectorfactorchng) %>%
#   unique(.,)
# 
# ratingdf <- tmpdf %>%
#   mutate(., Class = "Rating") %>%
#   select(., Category = Rating, Class, Tenor, Factor = ratingfactorchng) %>%
#   unique(.,)
# 
# regiondf <- tmpdf %>%
#   mutate(., Class = "Region") %>%
#   select(., Category = Region, Class, Tenor, Factor = regionfactorchng) %>%
#   unique(.,)
# 
# tenordf <- tmpdf %>%
#   mutate(., Class = "SYSTEMIC") %>%
#   mutate(., Category = "SYSTEMIC") %>%
#   select(., Category, Class, Tenor, Factor = tenorfactorchng) %>%
#   unique(.,)
# 
# classdf <- bind_rows(regiondf, sectordf, ratingdf, tenordf)
# classdf <- classdf %>%
#   filter(., Tenor <= 10) %>%
#   group_by(., Class)
# 
# maxfactor = ceiling(100*max(classdf$Factor))
# minfactor = floor(100*min(classdf$Factor))

factortbl <- df %>%
  filter(., viv_name != "missed") %>% 
  filter(., score != 0) %>% 
  select(., lcbo_name, lcbo_region, lcbo_country, vintage, container, size.mL, price, unit_price , score, num_reviews, critic, critic_score)

################################################################################