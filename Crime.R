library(data.table) #faster way to read large dataset
library(tidyverse) #load dplyr, tidyr and ggplot
library(ggmap) #use to read map
library(maps) #map tools kits
library(mapdata) #read the map data
library(lubridate) #date manuplation
library(ggrepel) #better label
library(varhandle) #load the function unfactor

crime_la <- as.data.frame(fread("Crime_Data_from_2010_to_Present.csv", na.strings = c("NA")))
glimpse(crime_la)

#select relevant variables
crime_la_selected <- select(crime_la, `Date Occurred`, `Time Occurred`, `Area Name`, `Crime Code Description`, `Victim Age`, `Victim Sex`, `Victim Descent`, `Premise Description`, `Weapon Description`, `Status Description`, Location)

#convert the date into date type
crime_la_selected$`Date Occurred` <- mdy(crime_la_selected$`Date Occurred`) 

#Separate latitude and longitude
location <- crime_la_selected$Location %>% # take coord as string
  str_replace_all("[()]", "") %>% # replace parantheses
  str_split_fixed(", ", n=2) %>% # split up based on comma and space after
  as.data.frame %>% # turn this to a data frame
  transmute(lat=V1, long=V2) # rename the variables 

#combine the lat and long then remove the location
crime_la_selected <- cbind(crime_la_selected, location)

crime_la_selected <- subset(crime_la_selected, select = -c(Location))

#select only 2017 and 2018
crime_selected_years <- filter(crime_la_selected, `Date Occurred` >= as_date("2017-01-01"), `Date Occurred` <= as_date("2017-12-30"))

#remove these data frames to same memory
rm(crime_la, crime_la_selected, location) #remove these data frames to same memory

#separate date into year, month and day.
crime_selected_years$year <- year(crime_selected_years$`Date Occurred`)
crime_selected_years$month <- month(crime_selected_years$`Date Occurred`)
crime_selected_years$days <- day(crime_selected_years$`Date Occurred`)

#Recode the variable into readable format
crime_selected_years$`Victim Sex` <- recode(crime_selected_years$`Victim Sex`, 'F' = 'Female', 'M' = 'Male', 'X' = 'Unknown')

crime_selected_years$`Victim Descent` <- recode(crime_selected_years$`Victim Descent`, "A" = "Other Asian", "B" = "Black", "C" = "Chinese", "D" = "Cambodian", "F" = "Filipino", "G" = "Guamanian", "H" = "Hispanci/Latin/Mexican", 'I' = "American Indian/Alaskan Native", "J" = "Japanese", "K" = "Korean", "L" = "Laotian", "O" = "Other", "P" = "Pacific Islander", "S" = "Somoan", "U" = "Hawaiian", "V" = "Vietnamese", "W" = "White", "X" = "Unknown", "Z" = "Asian Indian")

#convert the character into factor
character_vars <- lapply(crime_selected_years, class) == "character"
crime_selected_years[, character_vars] <- lapply(crime_selected_years[, character_vars], as.factor)

glimpse(crime_selected_years)


Total Crime in 2017
Let’s look at the top 20 of crime that has been committed in 2017.

year_2017 <- crime_selected_years %>%
  filter(year == "2017")

group <- year_2017 %>%
  group_by(`Crime Code Description`) %>%
  summarise(total = n()) %>%
  distinct() %>%
  top_n(20)

group %>%
  ggplot(aes(reorder(`Crime Code Description`, total), y = total)) +
  geom_col(fill = "red") +
  geom_label_repel(aes(label = total), size = 2.5) +
  coord_flip() +
  labs(title = "Top 20 Crime Commited in 2017", 
       x = "Crime Description", 
       y = "Total")
       
       
 Age group
Next I'm going to examine the age group most likely to become victim of crime.

age <- year_2017 %>%
  group_by(`Victim Age`) %>%
  summarise(total = n()) %>%
  na.omit()

age %>%
  ggplot(aes(x = `Victim Age`, y = total)) +
  geom_line(group = 1) +
  geom_point(size = 0.5) +
  labs(title = "Age Most Likely To Become Crime Victim", 
       x = "Victim Age", 
       y = "Total")


year_2017$age_group <- cut(year_2017$`Victim Age`, breaks = c(-Inf, 19, 35, 55, Inf), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly"))

age.group <- year_2017 %>%
  group_by(age_group, `Crime Code Description`) %>%
  summarise(total = n()) %>%
  top_n(20) %>%
  na.omit()

age.group %>%
  ggplot(aes(reorder(x = `Crime Code Description`, total), y = total)) +
  geom_col(fill = 'red') +
  geom_text(aes(label=total), color='black', hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~ age_group) +
  labs(x = 'Total', 
       y = "Crime Description")
 
 
 Gender
In this section, I'm going to examine type of crime targeted to different gender.

gender <- year_2017 %>%
  group_by(`Victim Sex`, `Crime Code Description`) %>%
  summarise(total = n()) %>%
  filter(`Victim Sex` != "Unknown", `Victim Sex` != "H") %>%
  na.omit() %>%
  top_n(20) 

gender <- gender[-c(1:30),]

gender %>%
  ggplot(aes(reorder(x = `Crime Code Description`, total), y = total)) +
  geom_col(fill = 'green') +
  geom_text(aes(label=total), color='black', hjust = 0.8, size = 3) +
  coord_flip() +
  facet_wrap(~ `Victim Sex`) +
  labs(x = 'Total', 
       y = "Crime Description")

Map The Crime
Next we are going to map the crime. For the illustrate purpose, I'm going to map only the the highest crime commited in 2017 which were assualt vehicle stolen and burgarly from vehicle.

#get the map of LA
LA_map <- qmap(location = "Los Angeles", zoom = 12)

#unfactor variable
year_2017$lat <- unfactor(year_2017$lat)
year_2017$long <- unfactor(year_2017$long)

#select relevant variables
mapping <- year_2017 %>%
  select(`Crime Code Description`, long, lat) %>%
  filter(`Crime Code Description` == 'BATTERY - SIMPLE ASSAULT') %>%
  na.omit()

#mapping
LA_map + geom_density_2d(aes(x = long, y = lat), data = mapping) +
  stat_density2d(data = mapping, 
    aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
    bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
    guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)
    
 

mapping <- year_2017 %>%
  select(`Crime Code Description`, long, lat) %>%
  filter(`Crime Code Description` == 'VEHICLE - STOLEN') %>%
  na.omit()

LA_map + geom_density_2d(aes(x = long, y = lat), data = mapping) +
  stat_density2d(data = mapping, 
    aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
    bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
    guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)
    


Interestingly, most vehicle are more likely to be stolen on South Los Angeles.

mapping <- year_2017 %>%
  select(`Crime Code Description`, long, lat) %>%
  filter(`Crime Code Description` == 'BURGLARY FROM VEHICLE') %>%
  na.omit()

LA_map + geom_density_2d(aes(x = long, y = lat), data = mapping) +
  stat_density2d(data = mapping, 
    aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
    bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
    guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)
    
