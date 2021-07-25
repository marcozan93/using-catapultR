#### AUTOMATE DATA COLLECTION CATAPULTR FOR SQUAD ####
#### Download the GPS data from Catapult API

#The aim of this code is to use catapultR package to download Catapult GPS data
#from Catapult API and store the data in a tidy data frame that could be used for further analysis.
#
#Pre-requisutes:
#This code would work if tagging is performed in Openfield Console and Openfield Cloud.
#
#In the Openfield Console, I tag the participation (i.e., Full, Modified) of players 
#in periods and activity, the tag for each period (i.e., Full session, Extras), and the
#activity (i.e., Day code, Field, Match, Extras). This is performed partially live during
#the training session and post download.
#
#In the Openfield Cloud, I tag the activity (i.e., Week, Opposition, Season) and I have to
#tag at least one player in the activity with the tag Squad (i.e., Non-23, Team, NA). This is
#performed post Full Sync from the Openfield Console.
#
#The script below can be run after the tagging is complete.
#
#Once the script is finished running, you can choose whether to save the dataframe
#in a .CSV file or in a table in MySQL.

# Load libraries
library(catapultR)
library(tidyverse)
library(lubridate)
options(digits = 12)
library(janitor)
library(anytime)

# Activate token for API
token <- ofCloudGetToken(sRegion = "EMEA", 
                         sName = "<YOUR OWN CREDENTIALS>", 
                         sPwd = "<YOUR OWN CREDENTIALS>",
                         sClientID = "<YOUR OWN CREDENTIALS>", 
                         sClientSecret = "<YOUR OWN CREDENTIALS>"
)


# Get Activities from cloud: 
activities <- ofCloudGetActivities(token)


#### 1. Get all Stats for activities and periods ####
#these are the metrics used at Bath Rugby 2021/2022 season
#Velocity bands are defined as:
#0-60%Vmax
#60-75%Vmax
#75-90%Vmax
#>90%Vmax
data <- ofCloudGetStatistics(
  token, 
  params = c("date", 
             "athlete_name", "athlete_id", 
             "position_name", 
             "activity_name", "activity_id", 
             "period_name", "period_id", 
             "day_name",
             "total_duration", "total_distance", 
             "velocity2_band2_total_distance", 
             "velocity2_band1_total_distance",
             "velocity2_band3_total_distance",
             "velocity2_band4_total_distance",
             "velocity2_band3_total_effort_count",
             "velocity2_band4_total_effort_count",
             "max_vel",
             "percentage_max_velocity",
             "gen2_acceleration_band6plus_total_effort_count",
             "gen2_acceleration_band3plus_total_effort_count",
             "meterage_per_minute",
             "acceleration_density",
             "total_player_load",
             "max_heart_rate",
             "mean_heart_rate",
             "total_running_(>2m/s)_(m)"), 
  groupby = c("athlete", "period", "activity"), 
  filters = list(name = "date",
                 comparison = "=",
                 #values = "21/07/2021")                  # I) select the date you are interested in!
                 values = format(Sys.Date(), "%d/%m/%Y")) # or II) today's activities.
) %>% mutate(date = as.Date(date, "%d/%m/%Y"), 
             position_name = as.factor(position_name)) %>%
  rename(total_running_2m_s_m = "total_running_(>2m/s)_(m)")


## I) Select my activities for the day (if downloading on same day of activity)
my_activities <- activities %>% filter(anydate(activities$start_time) == Sys.Date())

## II) Select my activities from a different day (need to enter the day " ")
#my_activities <- activities %>% filter(anydate(activities$start_time) == "<ENTER DATE YOU WANT>")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 2. Get Activity Tags: ####

## 2.1. functions to add prefix to nested period column names
fix_names_periods <- function(x) {
  ifelse(x %in% c("id", "name", "start_time", "end_time"), paste0("period_", x), x)
}
# add prefix to nested tag_list column names
fix_names_tags <- function(x) {
  ifelse(x %in% c("id", "name", "start_time", "end_time"), paste0("tag_", x), x)
}

## 2.2. unnest periods & tags
activities_periods_tags <- my_activities %>% 
  rename(activity_id = id,
         activity_name = name,
         activity_start_time = start_time,
         activity_end_time = end_time) %>% 
  unnest(periods, 
         names_repair = fix_names_periods) %>%
  select(-tags) %>%
  unnest(tag_list, 
         names_repair = fix_names_tags)

activities_periods_tags <- activities_periods_tags[ ,-which(duplicated(colnames(activities_periods_tags)))]

tags <- activities_periods_tags %>% 
  select(activity_id, period_id, starts_with("tag"))
activities_tags <- tags %>% 
  pivot_wider(names_from = tag_type_name, values_from = tag_name, values_fn = unique) %>% 
  select(-tag_id, -tag_type_id) %>%
  group_by(activity_id, period_id) %>% 
  summarise_all(list(~first(na.omit(.))))

## 2.2.1 Safely remove columns with "Device" in their title
activities_tags <-  if (any(str_detect(colnames(activities_tags), "Device"))) {
    activities_tags %>% select(-contains("Device"))
  } else { activities_tags }

## 2.3. Combine data and activity tags
data_activities.tags <- data %>% 
  left_join(activities_tags) %>% 
  clean_names() %>%
  rename(activity_tag = activity) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 3. GET PERIODS TAGS ####

## 3.1. Get all Periods available from Cloud
periods_list <- list()
for (i in 1:length(my_activities$id)) { 
  periods_list[[i]] <- ofCloudGetPeriods(token, my_activities$id[i])
}


## 3.2 Unnest all detaframe within the list and bind them together into a single data frame
periods_list_tidy <- list()
for (i in seq_along(periods_list)) {
  if (is_empty(periods_list[[i]])) {next
  } else { periods_list_tidy[[i]] <- unnest(periods_list[[i]], cols = c())}
}
periods_list_tidy <- Filter(is.data.frame, periods_list_tidy) %>% bind_rows()

## 3.3 Create a data frame with activity_id, period_id, period_name, period_tag
periods_tags <- periods_list_tidy %>% 
  dplyr::select(activity_id, id, name, tags) %>% 
  rename(period_id = id,
         period_name = name,
         period_tags = tags) %>%
  unnest(period_tags, names_repair = "universal") %>%
  dplyr::select(-id, -tag_type_id, -name) %>%
  pivot_wider(names_from = tag_type_name, values_from = tag_name, values_fn = unique) %>%
  dplyr::select(-starts_with("Device")) %>%
  rename(period_tag = Period) 

## 3.4. Combine data, activities_tags, periods_tags
data_activities.tags_periods.tags <- data_activities.tags %>% left_join(periods_tags)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 4. GET ATHLETES IN PERIODS TAGS ####

## 4.1. get all period_id in a single data frame
periods_ids_list <- list()
for (i in seq_along(periods_list)) {
  periods_ids_list[[i]] <- periods_list[[i]][["id"]] 
}                                                      
periods_ids_df <- tibble(id = unlist(periods_ids_list))    

## 4.2. get athletes in periods and period_ids
athletes_in_periods_list <- list() 
for (i in 1:length(periods_ids_df$id)) {                  
  if(!is.null(ofCloudGetAthletesInPeriod(token, periods_ids_df$id[[i]]))) {
    athletes_in_periods_list[[i]] <- ofCloudGetAthletesInPeriod(token, periods_ids_df$id[[i]]) %>%
      mutate(period_id = periods_ids_df$id[[i]])
  } else {
    athletes_in_periods_list[[i]] <- NA
  }
}

## 4.3. delete NAs from athletes_in_periods_list
athletes_in_periods_no_NAs <- Filter(is.data.frame, athletes_in_periods_list)

## 4.4 Unnest athletes in periods and create data frame
athletes_in_periods_list_tidy <- list()
for (i in seq_along(athletes_in_periods_no_NAs)) {
  if (is_empty(athletes_in_periods_no_NAs[[i]])) {next
  } else { athletes_in_periods_list_tidy[[i]] <- unnest(athletes_in_periods_no_NAs[[i]], cols = c())}
}
athletes_tags_df <- Filter(is.data.frame, athletes_in_periods_list_tidy) %>% bind_rows()

## 4.5 Create a data frame with period_id, athlete_name, participation_specific
athletes_tags_df_interim <- athletes_tags_df %>% 
  dplyr::select(id, first_name, last_name, tags, period_id) %>%
  unite(athlete_name, c("first_name", "last_name"), sep = " ") %>%
  rename(athlete_id = id) %>% unnest(tags, names_repair = "universal") %>%
  dplyr::select(-id, -tag_type_id, -name) 

# check presence of Flagged Region tag
athletes_tags_df <- if(any(athletes_tags_df_interim$tag_type_name == "Flagged Region")) {
  athletes_tags_df_interim %>% filter(tag_type_name != "Flagged Region") %>% 
    pivot_wider(names_from = tag_type_name, values_from = tag_name) %>%
  rename(participation_specific = Participation) 
} else {athletes_tags_df_interim %>%
    pivot_wider(names_from = tag_type_name, values_from = tag_name) %>%
    rename(participation_specific = Participation)}

## 4.6. combine data, activities_tags, periods_tags, athletes_tags
data_act.tags_per.tags_ath.tags <- data_activities.tags_periods.tags %>% 
  left_join(athletes_tags_df) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### GET ATHELTES IN ACTIVITY TO GET PARTICIPATION_WHOLE & SQUAD ####

## 5.1. Create a "safe" version of the function using the safely() function from the purrr package:
#this is done because if no athletes are found in activity I get an error 404
safe_ofCloudGetAthletesInActivity <- safely(ofCloudGetAthletesInActivity)

#For an even more thorough approach, you could use the following function as a wrapper around this "safe" function.
ofCloudGetAthletesInActivityWrapper <- function(credentials, id)
{
  result <- safe_ofCloudGetAthletesInActivity(credentials, id)
  if (!is.null(result$error))
  {
    err <- ofCloudParseError(result$error)
    if (!is.null(err$status_code) && (err$status_code == 404))  
      return(NULL)    
    stop(result$error)
  }
  return(result$result)
}

## 5.2. get athlete in activity tag: squad and participation_whole
athletes_in_activity_list <- list()
for (i in seq_along(my_activities$id)) {
  if (is.null(ofCloudGetAthletesInActivityWrapper(token, my_activities$id[i]))) { next }
  athletes_in_activity_list[[i]] <- ofCloudGetAthletesInActivityWrapper(token, my_activities$id[i]) %>%
    mutate(activity_id = my_activities$id[i])
}

## 5.3. delete NULLs from athletes_in_activity_list
athletes_in_activity_no_NAs <- Filter(is.data.frame, athletes_in_activity_list)

## 5.4. tidy all tables 
athletes_in_activity_list_tidy <- list()
for (i in seq_along(athletes_in_activity_no_NAs)) {
  if (!is_empty(athletes_in_activity_no_NAs[[i]]$tags[[1]])) {
    athletes_in_activity_list_tidy[[i]] <- athletes_in_activity_no_NAs[[i]] %>%
      dplyr::select(activity_id, id, first_name, last_name, tags) %>%
      rename(athlete_id = id) %>%
      unite(athlete_name, c("first_name", "last_name"), sep = " ") %>%
      unnest(tags, names_repair = "universal") %>%
      dplyr::select(-name, -id, -tag_type_id) %>% distinct() %>%
      pivot_wider(names_from = tag_type_name, values_from = tag_name) #%>%
    #rename(participation_whole = Participation, squad = Squad)
  } else {
    athletes_in_activity_list_tidy[[i]] <- NA
  }
}

## 5.5 delete NAs from athletes_in_activity_list_tidy
athletes_in_activity_df <- Filter(is.data.frame, athletes_in_activity_list_tidy) %>% 
  bind_rows() %>%
  rename(participation_whole = Participation, 
         squad = Squad)

## 5.6. combine data, activities_tags, periods_tags, athletes_tags, athlete_in_activity tags
data_base <- data_act.tags_per.tags_ath.tags %>% 
  left_join(athletes_in_activity_df) 
# THIS IS MY FINAL DATA FRAME !


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### ADD VARIABLES OF INTEREST: FORWARD_BACK, COMPETITIVE_SEASON, SENIOR_ACADEMY ####

## 6.0 Add variables forward_back, competitive_season, senior_academy:
data_base_final <- data_base %>% 
  mutate(
    forward_back = case_when(
      (position_name == "Prop" |
         position_name == "Hooker" |
         position_name == "Back Row" |
         position_name == "Second Row")  ~ "Forward",
      (position_name == "Fly Half" |
         position_name == "Centre" |
         position_name == "Wing" |
         position_name == "Scrum Half" |
         position_name == "Full Back") ~ "Back"
    ),
    competitive_season = case_when(
      date < "2020-11-01" ~ "2019-2020",
      (date >= "2020-11-01") & (date < "2021-07-15") ~ "2020-2021",
      (date >= "2021-07-15") & (date < "2022-07-15") ~ "2021-2022"
    ),
    senior_acad = 
      case_when(
        (athlete_name == "<ADD YOUR PLAYERS NAME>" | 
           athlete_name == "<ADD YOUR PLAYERS NAME>" | 
           athlete_name == "<ADD YOUR PLAYERS NAME>" | 
           athlete_name == "<ADD YOUR PLAYERS NAME>" | 
           athlete_name == "<ADD YOUR PLAYERS NAME>") &  # add more names if needed
          (competitive_season == "2020-2021" |
             competitive_season == "2021-2022") ~ "senior",
        (athlete_name == "<ADD YOUR PLAYERS NAME>" |
           athlete_name == "<ADD YOUR PLAYERS NAME>" |
           athlete_name == "<ADD YOUR PLAYERS NAME>" |
           athlete_name == "<ADD YOUR PLAYERS NAME>" |
           athlete_name == "<ADD YOUR PLAYERS NAME>") &  # add more names if needed
          (competitive_season == "2020-2021" |
             competitive_season == "2021-2022") ~ "academy"
      )) %>%
  mutate(senior_acad = as_factor(senior_acad),
         forward_back = as_factor(forward_back),
         competitive_season = as_factor(competitive_season))
  

#### 6.1 ENSURE COLUMNS ARE IN THE RIGHT ORDER ####
#Here you can choose the order you want. 
#This is just an example.
data_base_final <- data_base_final[ , c("athlete_id", "athlete_name",
                                        "activity_id", "activity_name",
                                        "period_id", "period_name",
                                        "position_name", "day_name",
                                        "total_distance", "total_duration",
                                        "total_player_load", "max_heart_rate",
                                        "max_vel", "velocity2_band1_total_distance",
                                        "velocity2_band2_total_distance",
                                        "velocity2_band3_total_distance",
                                        "velocity2_band3_total_effort_count",
                                        "velocity2_band4_total_distance",
                                        "velocity2_band4_total_effort_count",
                                        "mean_heart_rate",
                                        "meterage_per_minute",
                                        "percentage_max_velocity",
                                        "gen2_acceleration_band6plus_total_effort_count",
                                        "gen2_acceleration_band3plus_total_effort_count",
                                        "acceleration_density",
                                        "date", "week", "activity_tag",
                                        "season", "day_code", "opposition",
                                        "period_tag", "participation_specific",
                                        "participation_whole", "squad",
                                        "forward_back", "competitive_season",
                                        "senior_acad", 
                                        "total_running_2m_s_m")]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 7. WRITE AN EXTERNAL DATA BASE FROM 02-11-2020 ####
#To create a new .CSV file if this is the first time you run the script.
#write_csv(data_base_final, "C:/Users/SportsScience/Desktop/GPS_data_base.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 8. ADD NEW ROWS TO EXISTING FILE ####
#This can be used to append the new data frame to the existing .CSV file you created in STEP 7.
#write.table(data_base_final, file = "C:/Users/SportsScience/Desktop/GPS_data_base_Trial.csv", 
#            sep = ",", append = T, col.names = F, row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 8.1 MySQL & R ####

#In order to use MySQL and R, you will have to create a table in 
#MySQL with the same columns as your final data frame.
#
#After you have created the table in MySQL, you can link R with MySQL and 
#append the latest data frame you have collected to the MySQL table.
#
#A good resource to get started is:
#https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r

#### WRITE THE DATA FRAME IN MYSQL USING R ####

#library(RMariaDB)

## 1. Create a password and connection to MySQL:
#localuserpassword <- "<YOUR PASSWORD>"
# The connection method below uses a password stored in a variable.
#gps_data <- dbConnect(RMariaDB::MariaDB(), user='<YOUR USER NAME', 
#                      password=localuserpassword, dbname='gps_data_base', host='localhost')
#dbListTables(gps_data)

## 2. Write the data frame in the table inside MySQL:
#dbWriteTable(gps_data, value = data_base_final, row.names = FALSE, 
#             name = "<NAME OF YOUR TABLE IN MYSQL", append = TRUE )

#disconnect from MySQL
#dbDisconnect(gps_data)
# Now you have the data frame into your MySQL Workbench.
# You should not repeated this operation, otherwise you will get a copy of the data, an exact duplicate.
