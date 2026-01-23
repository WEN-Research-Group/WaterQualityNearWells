#Upload all necessary datasets first, then do distance analysis

### WELL INFO ####

### MARGINAL
# Marginal well lists
NY_marginal_wells <- read.csv(file="./input/MERPMCWs.csv",header = TRUE, na.strings = c(""),
                              stringsAsFactors = F,
                              colClasses = "character")
#Change null to NA
NY_marginal_wells[NY_marginal_wells == "NULL"] = NA

#Need to remove wells that have Na Spud dates
NY_marginal_wells = filter(NY_marginal_wells, !is.na(DateSpud))

#There are two types of date formats - need to fit that
NY_marginal_wells$DateSpud = parse_date_time(NY_marginal_wells$DateSpud,c("mdY","Ymd HMS"))

#Convert date format just to be safe
NY_marginal_wells$DateSpud = as.POSIXct(NY_marginal_wells$DateSpud,format="%Y-%M-%d")

#Renaming
NY_marginal = NY_marginal_wells

#Rename lat long columns in PA_marginal dataset so that it would be "Longitude" and "Latitude"
#Also coordinates need to be numeric
#converting Lat/Long to numeric values
#Add negative sign to longitute in marginal well list
NY_marginal$Longitude = as.numeric(NY_marginal$Longitude)*(-1)
NY_marginal$Latitude = as.numeric(NY_marginal$Latitude)

names(NY_marginal)[names(NY_marginal) == 'DateSpud'] <- 'SpudDate'

### UNCONVENTIONAL

#There are no unconventional wells


### ORPHANED

orphaned_well_raw_new <- read.csv(file = "20240901_USOrphanWells_Dataset.csv")
#Clean up data
#Remove samples that don't have SpudDate
orphaned_well_raw_new_c = filter(orphaned_well_raw_new,!SpudDate=="")
#A lot of samples don't have spud date, so a lot of wells are eliminated after this step
#Texas_orphaned_all = filter(orphaned_well_raw_new, State=="Texas")
NY_orphaned_clean = filter(orphaned_well_raw_new_c, State=="New York")
#Some sites don't have lat/long
NY_orphaned_clean = filter(NY_orphaned_clean,!is.na(Latitude))
#Date format
NY_orphaned_clean = NY_orphaned_clean %>%
  mutate(SpudDate = 
           floor_date(as.POSIXct(SpudDate, format="%Y-%m-%d"), "day"))


### WATER QUALITY

### SURFACE - only data from surface water quality portal

#Water Quality Portal
#Raw dataset was cleaned when I analyzed Texas data
#Upload cleaned data and extract NY state
SW_WQP_Cl = readRDS("sw_wqp_wq_all_curated_Cl.rds")

#Separate Texas samples
SW_WQP_Cl_State = filter(SW_WQP_Cl, State=="New York")

#Calculate daily values
SW_WQP_Cl_NY_daily = SW_WQP_Cl_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(SW_WQP_Cl_NY_daily)
#No outliers
SW_WQP_Cl_NY = SW_WQP_Cl_NY_daily

#Mapping 
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(SW_WQP_Cl_NY$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(SW_WQP_Cl_NY, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(SW_WQP_Cl_NY)
Q_75

### GROUND WATER

#Cleaned WQP data
#saveRDS(GW_WQP_Na_clean, file="gw_wqp_wq_all_curated_Na.rds", compress = FALSE)
GW_WQP_Cl = readRDS(file="gw_wqp_wq_all_curated_Cl.rds")

#Separate Texas samples
GW_WQP_Cl_State = filter(GW_WQP_Cl, State=="New York")

#Calculate daily values
GW_WQP_Cl_NY_daily = GW_WQP_Cl_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(GW_WQP_Cl_NY_daily)

#Remove largest values
GW_WQP_Cl_NY = filter(GW_WQP_Cl_NY_daily, daily_mean < 100000000)


#Mapping 
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(GW_WQP_Cl_NY$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(GW_WQP_Cl_NY, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(GW_WQP_Cl_NY)
Q_75



# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = NY_marginal
#unconv_well_data = enverus_og_well_curated
orphan_well_data = NY_orphaned_clean
surface_water_data = SW_WQP_Cl_NY
groundwater_data = GW_WQP_Cl_NY

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "Cl"
meas = "ug/L" 
state = "New York" 

#SURFACE

#MARGINAL
sw.wq.distance = Well_spatial_analysis_compl_0127(marginal_well_data,surface_water_data)

type_w = "SW"
well_type = "marginal"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

sw.wq.distance = Well_spatial_analysis_compl_0127(orphan_well_data,surface_water_data)

well_type = "orphaned"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)



#GROUNDWATER

#MARGINAL
gw.wq.distance = Well_spatial_analysis_compl_0127(marginal_well_data,groundwater_data)


type_w = "GW"
well_type = "marginal"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

gw.wq.distance = Well_spatial_analysis_compl_0127(orphan_well_data,groundwater_data)

well_type = "orphaned"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)





################################
##################

#Compile all values into one file
#define these for every distance calculation
anal = "Cl" #already should be defined above
meas = "ug/L" 
state = "New York" 

#SURFACE

#MARGINAL

type_w = "SW"
well_type = "marginal"

#Read analysis results
SW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"

plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
cor(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))


### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"

plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))
#plot((SW_orph$nearest_distance_m), log(SW_orph$daily_mean))
cor(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))



#GROUNDWATER

#MARGINAL
type_w = "GW"
well_type = "marginal"

GW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_marg$type_w = "GW"
GW_marg$well_type = "marginal"

plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
#GW_marg = filter(GW_marg, nearest_distance_m < 6283100)
#plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
cor(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))


### ORPHANED

well_type = "orphaned"

GW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_orph$type_w = "GW"
GW_orph$well_type = "orphaned"

#GW_orph = filter(GW_orph, nearest_distance_m < 6283100)
plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))
cor(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))


########### SAVE ALL RESULTS
#Combine all 6 into 1 and save
Geospatial_result_NY_Cl = rbind(SW_marg,SW_orph,GW_marg,GW_orph)

saveRDS(Geospatial_result_NY_Cl,file = "Geospatial_results_NY_Cl.rds", compress = FALSE)




