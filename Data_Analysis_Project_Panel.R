##Starting project. 
##Installing packages & libraries
  install.packages("tidyverse")
  install.packages("here")
  install.packages("ggplot2")
  install.packages("skimr")
  install.packages("janitor")
  install.packages("readxl")
  library(tidyverse)
  library(here)
  library(ggplot2)
  library(skimr)
  library(janitor)
  library(readxl)

##Importing first data set
  dailiy_Activity <- read_excel("dailiy_Activity.xlsx")
  
##Cleaning data
  da <- clean_names(dailiy_Activity)
  
##The day has 1440 minutes, according to the data sets, if u sum the sedentary minutes, plus active minutes (light, very and fairly)
## plus the time in bed, u will have the 1440 minutes
  
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
      geom_point(mapping = aes(x = moderately_active_distance, y = total_distance), color = "green") +
      labs(title = "Lightly Distance VS Very Active Distance")
    
    ggplot(data = da) +
      geom_point(mapping = aes(x = light_active_distance, y = total_distance), color = "purple") +
      labs(title = "Lightly Distance VS Very Active Distance")
##Conclusion:
    ##If we check these 3 graphs, we will see a trend that people used to do more exercise distances in a lightly way. 
    ##Active distances are the shortest. 
  
  ##But in the nexts graphs, we can see the trend that when you spend more time doing active exercise than light, more calories you burn
    ggplot(data = da, aes(x = calories, y = total_activity_minutes)) +
      geom_point(aes(alpha = lightly_active_minutes), color = "red") +
      geom_point(aes(alpha = very_active_minutes), color = "blue") +
      geom_point(aes(alpha = fairly_active_minutes), color = "green") +
      theme(axis.title.x = element_text()) +
      labs(x = "Calories", y = "Total activity (mins)") +
      annotate("text", x = 680, y = 530, label = "Red = Lightly active mins", color = "red") +
      annotate("text", x = 645, y = 500, label = "Blue = Very active mins", color = "blue") +
      annotate("text", x = 705, y = 470, label = "Green = Fairly active mins", color = "green") +
      theme(legend.position='none') 
    
##Conclusion: doing very active exercises will burn more calories. We can found a balance if you do a moderately exercise,
    ##and finally if you do a light exercise you will burn less calories. 
  
  ##We can see, that if you do light activity u need more time to burn calories. But if you do an extensive activity, u need less time.
    ggplot(data = da) +
      geom_point(mapping = aes(x = calories, y = total_activity_minutes)) +
      geom_smooth(mapping = aes (x = calories, y = very_active_minutes), color = "blue") +
      geom_smooth(mapping = aes (x = calories, y = lightly_active_minutes), color = "red") +
      labs(x = "Calories", y = "Total activity (mins)") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
      annotate("text", x = 500, y = 530, label = "Blue = Very active mins", color = "blue") +
      annotate("text", x = 550, y = 500, label = "Red = Lightly active mins", color = "red") 
  
  ##Calories vs total minutes // Total minutes = sedentary + active
  ##We can see that human spend more time in a sedentary way than an active way 
    ggplot(data = da) +
      geom_point(mapping = aes(x = calories, y = total_minutes)) +
      geom_smooth(mapping = aes (x = calories, y = sedentary_minutes), color = "red") + 
      geom_smooth(mapping = aes (x = calories, y = total_activity_minutes), color = "yellow") +
      labs(x = "Calories", y = "Total minutes") +
      annotate("text", x = 3500, y = 100, label = "Red = Sedentary active mins", color = "blue") +
      annotate("text", x = 3435, y = 40, label = "Yellow = Total active mins", color = "blue") 
  
  ##We've done a couple of trends on this sheet, so now we are going to merge it with other ones and keep searching for new trends.
  ##Importing data set 
    sleep_Day <- read_excel("sleep_Day.xlsx")
  
  #Only for future datasets merges & analysis, I will change the column named as SleepDay for activity_date
  ##2) Cleaning the data
    sl <- clean_names(sleep_Day) %>%
      select(-id_len)
    sld <- rename(sl, activity_date = sleep_day)
  
  ##Analyzing data 
    head(sld)
    sld %>%
      select(hour_and_minutes_asleep,
             hours_and_minutos_in_bed,
             total_minutes_asleep,
             total_time_in_bed) %>%
      summary()
  
  ##4) Graphs 
    ggplot(data = sld) +
      geom_point(mapping = aes(x = total_time_in_bed, y = total_minutes_asleep), color = "purple") +
      labs(title = "Minutes asleep vs in bed")
  
  ##We are going to merge with Inner Join, this means only the information that match, so we can analyze the data daily activity with the sleep days. 
  ## da = daily Activity dataframe; sl = sleep Day dataframe
    da_sl <- merge(da, sld, by = c("id","activity_date"))
    
    glimpse(da_sl)
  
    ##I want to chech how many people sleep 8 or more hours per day from my dataset. 
      filter_8hours <- da_sl %>%
        filter(total_minutes_asleep >= 480)
      View(filter_8hours)
    ##Conclusion: I had 413 rows in the data set and 117 people slept more than 8 hours. 
      filter_less8hourse <- da_sl %>%
        filter(total_minutes_asleep < 480)
      View(filter_less8hourse)
    ##Conclusion_ From 413 rows data, 296 slept less than 8 hours. 
      
    filter_hours_pie <- c(117, 296)
    piepercent<- round(100*filter_hours_pie/sum(filter_hours_pie), 1)
    pie(filter_hours_pie, labels)
    pie(filter_hours_pie, labels = piepercent,col = rainbow(length(filter_hours_pie)))
    legend("topright", c("8 hourse or more", "Less than 8 hours"), cex = 1,
           fill = rainbow(length(filter_hours_pie)))
    
    
    ##Analyzing data
      
      filter_less8hourse %>%
        summary()
      filter_8hours %>%
        summary()
      
      ##fl8 = filter_less8hours
      select_fl8 <- filter_less8hourse %>%
        select(total_steps,
               total_distance,
               very_active_distance,
               moderately_active_distance,
               light_active_distance,
               very_active_minutes,
               fairly_active_minutes,
               lightly_active_minutes,
               total_activity_minutes,
               sedentary_minutes, 
               calories,
               total_minutes_asleep,
               total_time_in_bed)
      View(select_fl8)
      
      ##8h = 8hours
      select_8h <- filter_8hours %>%
        select(
          total_steps,
          total_distance,
          very_active_distance,
          moderately_active_distance,
          light_active_distance,
          very_active_minutes,
          fairly_active_minutes,
          lightly_active_minutes,
          total_activity_minutes,
          sedentary_minutes, 
          calories,
          total_minutes_asleep,
          total_time_in_bed)
      View(select_8h)
      
    ## Random size so we can compare means values in both data sets. 
       random_selectfl8 <- select_fl8 %>%
        sample_n(117, replace = FALSE)
       View(random_selectfl8)
      
      
    ##I will merge both 117 rows data sets, creating a new one only of 234
        merge_8h_fl8 <- merge(select_8h, random_selectfl8, all = TRUE)
        View(merge_8h_fl8)
      
        eight_hours <- merge_8h_fl8 %>%
          mutate(eight_hours_or_not = if_else(total_minutes_asleep >= 480, 'TRUE', 'FALSE'))
        View(eight_hours)
          
      
     
    
    ##Courious data: 
      ##8 >= hours means: 
        ##Total time in bed: 583,2
        ##Total minues asleep: 543
        ##Calories: 2284
        ##Total activity minutes: 251,9
        ##Sedentary minutes: 610
      
  ##Graphs:
  ##Total steps vs total time in bed 
    ## 1 trend
    label_filter_change = as_labeller(c(`FALSE` = "Less than 8 hours", `TRUE` = "8 hours or more"))
        
    ggplot(data = eight_hours) +
      geom_point(mapping = aes(x = total_activity_minutes, y = sedentary_minutes, color = calories)) +
      facet_wrap(~eight_hours_or_not, labeller = label_filter_change) +
      labs(x = "Total Activity Minutes", y = "Sedentary Minutes")
    ##2 trend
    ggplot(data = eight_hours) +
      geom_point(mapping = aes(x = total_activity_minutes, y = calories, color = very_active_minutes)) +
      facet_wrap(~eight_hours_or_not, labeller = label_filter_change)
    ##3 trend
    ggplot(data = eight_hours, aes(x = total_distance, y = calories)) +
      geom_smooth(mapping = aes(alpha = very_active_distance), color = "red") +
      facet_wrap(~eight_hours_or_not, labeller = label_filter_change) +
      theme(legend.position='none') 
    
    ggplot(data = eight_hours, aes(x = total_distance, y = calories)) +
      geom_smooth(mapping = aes(x = very_active_distance, y = calories), se = FALSE) +
      geom_smooth(mapping = aes(x = light_active_distance, y = calories), color = "red", se = FALSE) +
      geom_smooth(mapping = aes(x = moderately_active_distance, y = calories), color = "green", se = FALSE) +
      facet_wrap(~eight_hours_or_not, labeller = label_filter_change) 

  
  ##Now, I am going to import the weight_Info data set, clean it, analyze it, graph some trends and then merge with other data sets. 
  ##1) Importing data set 
    weight_Log_Info <- read_excel("weight_Log_Info.xlsx")
  
  ##2) Cleaning and analyzing 
  ## weight = weight_Log_Info
    head(weight_Log_Info)
    colnames(weight_Log_Info)
  
  ##Columns did not have names 
    
    weightt <- weight_Log_Info %>%
      clean_names() %>%
      rename(activity_date = x3) 
    
    
    weight <- weightt %>%
      select(id,
             activity_date,
             weight_kg,
             fat,
             bmi,)
    
    View(weight)
    
  ##Now, we are going to merge with da_Sl
    da_sl_we <- merge(da_sl, weight, by = c("id", "activity_date"))
    View(da_sl_we)
    
  ## Graphs 
    
    
  ## There is no big difference between  the minutes in bed, wake up and the weight 
    ggplot(data = da_sl_we) +
      geom_point(mapping = aes(x = total_minutes_asleep, y = total_time_in_bed,color = weight_kg))
    
  ## We can see a trend when weight increment, the bmi also increment. 
    ggplot(data = da_sl_we) +
      geom_line(mapping = aes(x = weight_kg, y = bmi, ), color = "blue") +
      labs(x = "Weight (kg)", y = "BMI")
    
    ggplot(data = da_sl_we) +
      geom_point(mapping = aes(x = weight_kg , y = total_time_in_bed))
    
    ggplot( data = da_sl_we, aes(x=total_distance, y = total_steps)) +
      geom_line() +
      geom_point(mapping = aes(x = moderately_active_distance, y = total_steps), color = "purple") +
      geom_point(mapping = aes(x = very_active_distance, y = total_steps), color = "green") +
      geom_point(mapping = aes(x = light_active_distance, y = total_steps), color = "red") 
    
    
    
  
  ##I want to analyze the data sets per hour and merge them for trends findings 
  ##We are going to import data sets, clean it, analyze it and graph trends. 
    hourly_Intensities <- read_excel("hourly_Intensities.xlsx")
    hourly_Calories <- read_excel("hourly_Calories.xlsx")
    hourly_Steps <- read_excel("hourly_Steps.xlsx") 
    
    ##Variables for understanding 
      ## hi =hourly_Itennsities
      ## hc = hourly_Calories
      ## hs = hourly_Steps
    
    ##The time was inconsistent in data sets, so with excel I clean it, so all the columns have the same format in date & time. 
    
    ##2) Cleaning 
      hi <- hourly_Intensities %>%
        clean_names()
      
      hc <- hourly_Calories %>%
        clean_names()
      
      hs <- hourly_Steps %>%
        clean_names()
    
    ##3) Merging data 

      
      hi_hc <- merge(hi, hc, by = c("id", "activity_date"))
      View(hi_hc)
      
        ## hi_hc es un data set muy grande, voy a agarrar una sample según la población. Accorging to poblation, trust level y error margin
        ##254979, 95%, 3% = 1063
      
          random_hi_hc <- hi_hc %>%
            sample_n(1063, replace = FALSE)
          View(random_hi_hc)
          
      hi_hc_hs <- merge(random_hi_hc, hs, by = c("id", "activity_date"))
      View(hi_hc_hs)
    
    ##I could find that in these merges were duplicates, so we will fix it.
      hi_hc_hs_nodup <- hi_hc_hs %>%
        distinct(id, activity_date, .keep_all = TRUE)
      View(hi_hc_hs_nodup)
    
    ##4) Graphs 
      
      ggplot(data = hi_hc) +
        geom_point(mapping = aes(x = total_intensity, y = calories))
      
      ggplot(data = hi_hc, aes(x=total_intensity, y=calories) ) +
        geom_hex() +
        theme_bw()
      
      ggplot(data = hi_hc_hs_nodup) +
        geom_point(mapping = aes(x = step_total, y = calories, color = total_intensity))
      
      ggplot(data = hi_hc_hs_nodup) +
        geom_point(mapping = aes(x = step_total, y = calories, color = average_intensity))
      
      ggplot(data = hi_hc_hs_nodup) +
        geom_point(mapping = aes(x = calories, y = step_total, color = total_intensity))
      
      ggplot(data = hi_hc_hs_nodup) +
        geom_point(mapping = aes(x = total_intensity, y = step_total, color = calories))
    
  ##I am going to add the heartrate data set and merge it with hc_hi_hs_nodup
    ##Importing data set
      heartrate <- read_excel("heartrate.xlsx")
        
    ##Cleaning it 
      hr <- heartrate %>% 
        select(-Time) %>%
        clean_names()
      View(hr)
          
    ##Analyzing data
      head(hr)
      
    ##Merge data sets
      hi_hc_hs_hr <- merge(hi_hc_hs_nodup, hr, by = c("id", "activity_date"))
      View(hi_hc_hs_hr)
        
    ##Graph data 
        ggplot(data = hi_hc_hs_hr) +
          geom_point(mapping = aes(x = total_intensity, y = heartrate_value))
        ##Here, we can see a trend that when more steps u do, more control u have on the heartrate value  
        ggplot(data = hi_hc_hs_hr) +
          geom_point(mapping = aes(x = heartrate_value, y = calories, color = step_total))
    ##The cases are almost all good, but u can see some exceptions when the intensity is lower, the calories too and the hearthrate increment. 
        ggplot(data = hi_hc_hs_hr) +
          geom_point(mapping = aes(x = heartrate_value, y = calories, color = total_intensity))
    
      random_minw_mmn <- minw_mmn %>%
      sample_n(385, replace = FALSE)
    
      View(random_minw_mmn)
    
    ggplot(data = random_minw_mmn) +
      geom_bar(mapping = aes(x= me_ts))
    
    
