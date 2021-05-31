##Starting project. 
##Installing packages & libraries
  install.packages("tidyverse")
  install.packages("devtools")
  install.packages("here")
  install.packages("ggplot2")
  install.packages("skimr")
  install.packages("janitor")
  install.packages("gridExtra")
  install.packages("readxl")
  install.packages("lubridate")
  library(tidyverse)
  library(devtools)
  library(here)
  library(ggplot2)
  library(skimr)
  library(janitor)
  library(gridExtra)
  library(readxl)
  library(lubridate)

##Importing first data set
  dailiy_Activity <- read_excel("dailiy_Activity.xlsx")
  
##Cleaning data
  da <- clean_names(dailiy_Activity)
  
##Analyzing data
  head(da)
  da %>%
    select(total_steps,
           total_distance,
           very_active_distance,
           moderately_active_distance,
           light_active_distance,
           very_active_minutes,lightly_active_minutes,
           fairly_active_minutes,
           calories) %>%
    summary()
  
##Graphs
  ##We can see a trend in light distance, is bigger than the other ones. This means that people use to do more exercise in a lightly way
  ggplot(data = da) +
    geom_point(mapping = aes(x = very_active_distance, y = total_distance), color = "red") +
    labs(title = "Total Distance VS Very Active Distance")
  
  ggplot(data = da) +
    geom_point(mapping = aes(x = light_active_distance, y = total_distance), color = "red") +
    labs(title = "Lightly Distance VS Very Active Distance")
  
  ##But in the nexts graphs, we can see the trend that when you spend more time doing active exercise than light, more calories you burn
  ggplot(data = da, aes(x = calories, y = total_activity_minutes)) +
    geom_point(aes(alpha = lightly_active_minutes), color = "red") +
    geom_point(aes(alpha = very_active_minutes), color = "blue") +
    geom_point(aes(alpha = fairly_active_minutes), color = "green") +
    theme(legend.position='none')
  
  ##Total time doing activity vs the light and very active
  ##We can see, that if you do a light activity u need more time to burn calories. But if you do an extensive activity, u need less time.
  ggplot(data = da) +
    geom_point(mapping = aes(x = calories, y = total_activity_minutes)) +
    geom_smooth(mapping = aes (x = calories, y = very_active_minutes), color = "red") +
    geom_smooth(mapping = aes (x = calories, y = lightly_active_minutes), color = "green")
  
  ##Calories vs total minutes // Total minutes = sedentary + active
  ##We can see that human spend more time in a sedentary way than an active way 
  ggplot(data = da) +
    geom_point(mapping = aes(x = calories, y = total_minutes)) +
    geom_smooth(mapping = aes (x = calories, y = sedentary_minutes), color = "red") + 
    geom_smooth(mapping = aes (x = calories, y = total_activity_minutes), color = "yellow")
  
  
  ##We've done a couple of trends on this sheet, so now we are goin to merge it with other ones, se we can keep searching for new trends. 