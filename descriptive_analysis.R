# Descriptive analysis
rm(list=ls())
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(readxl)
library(jsonlite)
library(gridExtra)
library(gt)
library(networkD3)
library(scales)
setwd("C:/Users/aalaz/Desktop/Internship_data/database_created")

# database
data<-read.csv("bicimad_users.csv")
bicimad_data<-read.csv("bicimad_data.csv")
bicimad_abonos<-read.csv("bicimad_abonos.csv")

# District shapefile mutation
district<-readOGR("C:/Users/aalaz/Desktop/Internship_data/shapefile/DISTRITOS.shp")


xy<- bicimad_data[,c("longitude","latitude")]
pdf <- SpatialPointsDataFrame(coords = xy, data = bicimad_data,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(pdf)

district <- spTransform(district,
                        CRS = proj4string(pdf))
plot(district)  
proj4string(district)
district_df <- fortify(district)
district@data<-district_df


# Table1: stations per neighbouroud

bicimad_data %>%
  group_by(neighbours)%>%
  summarise(Bases_per_district=sum(total_bases),
            Stations_per_district=n()) %>%
  rename(Neighbourhood=neighbours)%>%
  arrange(desc(Stations_per_district)) %>%
  gt()%>%
  tab_header(
    title = md("Table 1: Stations and bases per neigbourhood"))%>%
  tab_source_note(md("Source: EMT de Madrid"))

# the map

##distinguish the historic center

bicimad_data$palcolour <- ifelse(bicimad_data$neighbours =="Centro", 
                                 "center", 
                                 "ne_center")
# set the .CSS colors
pal<-colorFactor(palette = c("#DC143C","#0000FF"), domain = c("center","ne_center"))


leaflet(bicimad_data) %>% addTiles() %>%
  addCircleMarkers(lng= ~as.numeric(longitude), lat= ~as.numeric(latitude), fillOpacity = 0.4,
                   radius = 3, color = ~pal(palcolour)) %>%
  addScaleBar(position = "bottomleft")%>%
  addPolygons(data=district)



## about age range
data%>%
  filter(ageRange!=0 & user_type!=3)%>%
  group_by(ageRange)%>%
  summarise(Total=n())%>%
  mutate(Percent = Total / sum(Total)*100)%>%
  gt()%>%
  tab_header(
    title = md("Table 2: Movements per age range"))%>%
  tab_source_note(md("Source: EMT de Madrid"))

## about travel_time

data%>%
  filter(user_type!=3 & travel_time!=0 & travel_time<36000)%>%
  group_by(user_type)%>%
  summarise(Mean=mean(travel_time/60))%>%
  gt()%>%
  tab_header(
    title = md("Table 3: Average travel time per user (min)"))%>%
  tab_source_note(md("Source: EMT de Madrid"))

##about user_type
data%>%
  filter(user_type!=0)%>%
  group_by(user_type)%>%
  summarise(Total=n())%>%
  mutate(Percent = Total / sum(Total)*100)%>%
  gt()%>%
  tab_header(
    title = md("Table 4: Movements per user"))%>%
  tab_source_note(md("Source: EMT de Madrid"))


# movements per month 
data%>%
  filter(user_type!=3)%>%
  mutate(month = factor(month, levels = c("décembre", "février", "mars",
                                   "avril", "juin"))) %>%
  group_by(user_type, month)%>%
  summarise(Total=n())%>%
  gt()%>%
  tab_header(
    title = md("Table 5: Movements per month by user type"))%>%
  tab_source_note(md("Source: EMT de Madrid"))
  
  


# Weekday movements per user

#number of trip per weekday by users

nbr_trip1<-data%>%
  filter(user_type==1)%>%
  group_by(weekday)%>%
  summarise(count1=n())
nbr_trip2<-data%>%
  filter(user_type==2)%>%
  group_by(weekday)%>%
  summarise(count2=n())
nbr_trip3<-data%>%
  filter(user_type==3)%>%
  group_by(weekday)%>%
  summarise(count3=n())

nbr_trip<-nbr_trip1%>%
  group_by(weekday)%>%
  left_join(nbr_trip2, by = "weekday")%>%
  left_join(nbr_trip3, by = "weekday")%>%
  select(weekday, count1, count2, count3)%>%
  mutate(count1=as.numeric(count1),
         count2=as.numeric(count2),
         count3=as.numeric(count3))%>%
  rename("Annual user"= count1,
         "Occasional user"= count2,
         "BiciMAD employee"=count3)

nbr_trip<-nbr_trip%>%
  mutate(grouping=weekday,
         grouping = factor(grouping, levels = c("lundi", "mardi",
                                              "mercredi", "jeudi",
                                              "vendredi", "samedi",
                                              "dimanche"))) 

plot1<-nbr_trip%>%
  gather(key = "Legend", value = "value", -weekday, -grouping) %>%
  ggplot(aes(x = as.factor(grouping), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#00AFBB", "#E7B800","#DC143C")) +
  theme_minimal() + ggtitle("Graph 1: Number of trips per user type") +
  labs(x = "Weekday", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))
plot2<-nbr_trip%>%
  gather(key = "Legend", value = "value", -weekday,-grouping, -"Occasional user", -"BiciMAD employee") %>%
  ggplot(aes(x = as.factor(grouping), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + ggtitle("Graph 2: Number of trips of annual user") +
  labs(x = "Weekday", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))
plot3<-nbr_trip%>%
  gather(key = "Legend", value = "value", -weekday,-grouping, -"Annual user", -"BiciMAD employee") %>%
  ggplot(aes(x = as.factor(grouping), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#DC143C")) +
  theme_minimal() + ggtitle("Graph 3 : Number of trips of ocasional user") +
  labs(x = "Weekday", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))
plot4<-nbr_trip%>%
  gather(key = "Legend", value = "value", -weekday,-grouping, -"Annual user", -"Occasional user") %>%
  ggplot(aes(x = as.factor(grouping), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#E7B800")) +
  theme_minimal() + ggtitle("Graph 4: Number of trips of BiciMAD employee") +
  labs(x = "Weekday", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

##### hourly trip per user

trip_day1<-data%>%
  filter(user_type==1)%>%
  group_by(hour)%>%
  summarise(count1=n())
trip_day2<-data%>%
  filter(user_type==2)%>%
  group_by(hour)%>%
  summarise(count2=n())
trip_day3<-data%>%
  filter(user_type==3)%>%
  group_by(hour)%>%
  summarise(count3=n())

trip_day<-trip_day1%>%
  group_by(hour)%>%
  left_join(trip_day2, by = "hour")%>%
  left_join(trip_day3, by = "hour")%>%
  select(hour, count1, count2, count3)%>%
  mutate(count1=as.numeric(count1),
         count2=as.numeric(count2),
         count3=as.numeric(count3))%>%
  rename("Annual user"= count1,
         "Occasional user"= count2,
         "BiciMAD employee"=count3)


# hourly movements per user

plot5<-trip_day%>%
  gather(key = "Legend", value = "value", -hour) %>%
  ggplot(aes(x = hour, y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#DC143C")) +
  theme_minimal() + ggtitle("Graph 5: Hourly number of trips per user type") +
  labs(x = "Hour", y = "Trips")

plot6<-trip_day%>%
  gather(key = "Legend", value = "value", -hour, -"Occasional user", -"BiciMAD employee") %>%
  ggplot(aes(x = hour, y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + ggtitle("Graph 6: Hourly number of trips with annual user") +
  labs(x = "Hour", y = "Trips")

plot7<-trip_day%>%
  gather(key = "Legend", value = "value", -hour, -"Annual user", -"BiciMAD employee") %>%
  ggplot(aes(x = hour, y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) + 
  scale_color_manual(values = c("#DC143C")) +
  theme_minimal() + ggtitle("Graph 7 : Hourly number of trips with ocasional user") +
  labs(x = "Hour", y = "Trips") 

plot8<-trip_day%>%
  gather(key = "Legend", value = "value", -hour, -"Annual user", -"Occasional user") %>%
  ggplot(aes(x = hour, y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  scale_color_manual(values = c("#E7B800")) +
  theme_minimal() + ggtitle("Graph 7: Hourly number of trips with BiciMAD employee") +
  labs(x = "Hour", y = "Trips") 

grid.arrange(plot5, plot6, plot7, plot8, ncol=2)


## daily use month 

daily_use<-bicimad_abonos%>%
  select(DIA, annual_membership_use, occas_membership_use, total_use, month, year, weekday)%>%
  mutate(date=ymd(DIA),
         month = factor(month, levels = c("janvier", "février", "mars",
                                                 "avril", "mai", "juin",
                                                 "juillet", "août",
                                                 "septembre", "octobre", "novembre",
                                                 "décembre")))

datetime<-format(as.POSIXct(strptime(daily_use$date,"%Y-%m-%d")) ,format = "%Y-%m")
daily_use$datetime<-datetime

daily_use %>%
  group_by(year, month, datetime)%>%
  summarise("Bicycle use with annual membership"=sum(annual_membership_use),
            "Bicycle use with occasional membership"=sum(occas_membership_use))%>%
  select(datetime, "Bicycle use with annual membership", "Bicycle use with occasional membership")%>%
  gather(key = "Legend", value = "value", -datetime, -"Bicycle use with occasional membership", -month, -year) %>%
  ggplot(aes(x = datetime, y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) + 
  scale_color_manual(values = c("#DC143C")) +
  theme_minimal() + ggtitle("Graph 9 : Monthly use of bicycle (annual membership)") +
  labs(x = "Month", y = "Bicycle use") + theme(axis.text.x = element_text(angle=90))

daily_use %>%
  group_by(year, month, datetime)%>%
  summarise("Bicycle use with annual membership"=sum(annual_membership_use),
            "Bicycle use with occasional membership"=sum(occas_membership_use))%>%
  select(datetime, "Bicycle use with annual membership", "Bicycle use with occasional membership")%>%
  gather(key = "Legend", value = "value", -datetime, -"Bicycle use with annual membership", -month, -year) %>%
  ggplot(aes(x = datetime, y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  scale_color_manual(values = c("#E7B800")) +
  theme_minimal() + ggtitle("Graph 10: Monthly use of bicycle (occaional membership)") +
  labs(x = "Month", y = "Bicycle use") + theme(axis.text.x = element_text(angle=90))





# the 10 most popular stations, the ones that generate the most traffic

bicimad_data%>%
  group_by(id)%>%
  summarise(Bike_flow=departure1+departure2+arrival1+arrival2,
            name=name,
            neighbours=neighbours)%>%
  arrange(desc(Bike_flow))%>%
  slice(1:10)%>%
  gt()%>%
  tab_header(
    title = md("Table 6: 10 most popular stations"))%>%
  tab_source_note(md("Source: EMT de Madrid")) 

## map associated

icons <- awesomeIcons(
  library = 'fa',
  icon = 'bicycle',
  markerColor = "blue")
bicimad_data%>%
  group_by(id)%>%
  summarise(Bike_flow=departure1+departure2+arrival1+arrival2,
            name=name,
            neighbours=neighbours,
            longitude=longitude,
            latitude=latitude)%>%
  arrange(desc(Bike_flow))%>%
  slice(1:10)%>%
  leaflet() %>% 
  setView(lng = -3.690965 , lat = 40.42270 , zoom = 12) %>%
  addTiles() %>%
  addAwesomeMarkers(lng= ~as.numeric(longitude), lat= ~as.numeric(latitude),
                    icon = icons) %>%
  addPolygons(data=district)












##The Sankey Diagram


df1<-data%>%
  select(user_type, idunplug_station, idplug_station)%>%
  filter(user_type==2)%>%
  mutate(id=idunplug_station)%>%
  group_by(id)%>%
  left_join(bicimad_data)%>%
  select(idunplug_station, idplug_station, neighbours)%>%
  rename(neighbours_source=neighbours)
df2<-data%>%
  select(user_type, idplug_station, idunplug_station)%>%
  filter(user_type==2)%>%
  mutate(id=idplug_station)%>%
  group_by(id)%>%
  left_join(bicimad_data)%>%
  select(id, idplug_station, idunplug_station, neighbours)%>%
  rename(neighbours_target=neighbours,
         idplug_station2=idplug_station,
         idunplug_station2=idunplug_station,
         id2=id)


df<-cbind.data.frame(df1,df2)

df<-df%>%
  select(idunplug_station, idplug_station, neighbours_source, neighbours_target)

links<-df %>%
  group_by(neighbours_source, neighbours_target)%>%
  summarise(value=n())%>%
  mutate(value=(value))%>%
  arrange(neighbours_target)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(links$neighbours_source), 
                           as.character(links$neighbours_target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource=match(links$neighbours_source, nodes$name)-1 
links$IDtarget=match(links$neighbours_target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF"])'

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, 
              nodeWidth=20, fontSize=15, nodePadding=80)

# table associated

links%>%
  rename(neighbourhood=neighbours_source)%>%
  group_by(neighbourhood)%>%
  summarise(Total_trip=sum(value))%>%
  arrange(desc(Total_trip))%>%
    gt()%>%
    tab_header(
      title = md("Table: Neighbourhoods movements (Occasional users)"))%>%
    tab_source_note(md("Source: EMT de Madrid"))

p<-links%>%
  select(neighbours_source, neighbours_target, value)
p%>%
  arrange(desc(value))%>%
  formattable()

##################################################################################








bicimad_abonos<-read.csv("C:/Users/aalaz/Desktop/Internship_data/database_created/bicimad_abonos.csv")

### BiciMAD abonos database

#annual subscribers

#the target is too have the total for each month for each year
bicimad_abonos%>%
  mutate(month = factor(month, levels = c("janvier", "février", "mars",
                                          "avril", "mai", "juin",
                                          "juillet", "août",
                                          "septembre", "octobre", "novembre",
                                          "décembre"))) %>%
  group_by(year, month)%>%
  summarise(total_membership_no_ptc=sum(membership_no_ptc),
            total_membership_ptc=sum(membership_ptc),
            total_membership=sum(total_membership))%>%
  mutate(day = 1)%>%
  unite(Date,day, month, year, sep=" ")%>%
  mutate(Date= dmy(Date))%>%
  gather(key = "Legend", value = "value", -Date,-total_membership_no_ptc,-total_membership_ptc) %>%
  ggplot(aes(x = Date, y = value)) + 
  geom_line(aes(color = Legend), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#DC143C")) +
  theme_minimal() + ggtitle("Registration of new active annual users per month") +
  labs(x = "Year", y = "New annual memberships")

##### Now let's try for each year

sum_abonos_by_year<-bicimad_abonos%>%
  group_by(year)%>%
  summarise(total_membership_no_ptc=sum(membership_no_ptc),
            total_membership_ptc=sum(membership_ptc),
            total_membership=sum(total_membership))%>%
  na.omit()

##ocasional subscribers

##month by month
sum_ocas_by_month<-bicimad_abonos%>%
  mutate(month = factor(month, levels = c("janvier", "février", "mars",
                                          "avril", "mai", "juin",
                                          "juillet", "août",
                                          "septembre", "octobre", "novembre",
                                          "décembre"))) %>%
  group_by(year, month)%>%
  summarise("1 day subscription"=sum(membership_1day),
            "3 days subscription"=sum(membership_3days),
            "5 days subscription"=sum(membership_5days),
            "Total occasional subscription"=sum(membership_occas_total))%>%
  mutate(day = 1)%>%
  unite(Date,day, month, year, sep=" ")%>%
  mutate(Date= dmy(Date))%>%
    gather(key = "Legend", value = "value", -Date) %>%
    ggplot(aes(x = Date, y = value)) + 
    geom_line(aes(color = Legend), size = 1) +
    scale_color_manual(values = c("#00AFBB", "#E7B800","#DC143C","#228B22")) +
    theme_minimal() + ggtitle("Registration of new occasional subscribers to the BiciMAD service") +
    labs(x = "Year", y = "New ocasional membership")+
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%m/%Y")) +
  theme(axis.text.x = element_text(angle=90))

##Now, year by year

sum_ocas_by_year<-bicimad_abonos%>%
  group_by(year)%>%
  summarise(occas_1day=sum(membership_1day),
            occas_3days=sum(membership_3days),
            occas_5days=sum(membership_5days),
            occas_total=sum(membership_occas_total))%>%
  na.omit()

## daily use by month
sum_usos_month<-bicimad_abonos%>%
  group_by(year, month)%>%
  summarise(annual_use=sum(annual_membership_use),
            occas_use=sum(occas_membership_use),
            total_use=sum(total_use))%>%
  na.omit()

##Now, year by year

sum_usos_year<-bicimad_abonos%>%
  filter(year!=2020)%>%
  group_by(year)%>%
  summarise("Annual users"=sum(annual_membership_use),
            "Occasional users"=sum(occas_membership_use),
            total_use=sum(total_use))

sum_usos_year%>%
  gather(key = "Legend", value = "value", -year, -"Occasional users", -total_use) %>%
  ggplot(aes(x = as.factor(year), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + ggtitle("Graph: Number of trips per year (annual users)") +
  labs(x = "Year", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))

sum_usos_year%>%
  gather(key = "Legend", value = "value", -year, -"Annual users", -total_use) %>%
  ggplot(aes(x = as.factor(year), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + ggtitle("Graph: Number of trips per year (annual users)") +
  labs(x = "Year", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))

## per weekdays

sum_usos_weekday<-bicimad_abonos%>%
  group_by(weekday)%>%
  summarise("Annual users"=sum(annual_membership_use),
            occas_use=sum(occas_membership_use),
            total_use=sum(total_use))%>%
  mutate(grouping=weekday,
       grouping = factor(grouping, levels = c("lundi", "mardi",
                                              "mercredi", "jeudi",
                                              "vendredi", "samedi",
                                              "dimanche")))
sum_usos_weekday%>%
  gather(key = "Legend", value = "value", -weekday, -grouping, -occas_use, -total_use) %>%
  ggplot(aes(x = as.factor(grouping), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + ggtitle("Graph: Number of trips per weekdays (annual users)") +
  labs(x = "Weekday", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))

sum_usos_weekday2<-bicimad_abonos%>%
  group_by(weekday)%>%
  summarise(annual_use=sum(annual_membership_use),
            "Occasional users"=sum(occas_membership_use),
            total_use=sum(total_use))%>%
  mutate(grouping=weekday,
         grouping = factor(grouping, levels = c("lundi", "mardi",
                                                "mercredi", "jeudi",
                                                "vendredi", "samedi",
                                                "dimanche")))
sum_usos_weekday2%>%
  gather(key = "Legend", value = "value", -weekday, -grouping, -annual_use, -total_use) %>%
  ggplot(aes(x = as.factor(grouping), y = value, group = Legend)) + 
  geom_line(aes(color = Legend), size = 1) +
  geom_point(aes(color = Legend))+
  scale_color_manual(values = c("#00AFBB")) +
  theme_minimal() + ggtitle("Graph: Number of trips per weekdays (occasional users)") +
  labs(x = "Weekday", y = "Trips") +
  theme(axis.text.x = element_text(angle = 45))
