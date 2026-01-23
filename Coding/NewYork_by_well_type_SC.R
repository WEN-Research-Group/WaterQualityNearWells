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

#There are two types of date formats - need to fit that
NY_marginal_wells$DateSpud = parse_date_time(NY_marginal_wells$DateSpud,c("mdY","Ymd HMS"))


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
SW_WQP_SC = readRDS("./input/SC_sw_wqp_daily_samples_summary.rds")

#Rename some columns
names(SW_WQP_SC)[names(SW_WQP_SC) == 'daily_sc'] <- 'daily_mean'
names(SW_WQP_SC)[names(SW_WQP_SC) == 'min_sc'] <- 'min_v'
names(SW_WQP_SC)[names(SW_WQP_SC) == 'max_sc'] <- 'max_v'

# Get the state samples 
SW_WQP_SC_NY = filter(SW_WQP_SC,State=="New York" & daily_mean >1) 

#Need to remove some columns 
SW_WQP_SC_NY = SW_WQP_SC_NY[,-1]


#Mapping 
View(SW_WQP_SC_NY) #Bronx county is fucked, lat/long are switched and lat has - sign
ind = which(SW_WQP_SC_NY$Latitude < 0)
#temp hold
lat_v = SW_WQP_SC_NY$Latitude
long_v = SW_WQP_SC_NY$Longitude

#switching values
SW_WQP_SC_NY$Latitude[ind] = long_v[ind]*(-1)
SW_WQP_SC_NY$Longitude[ind] = lat_v[ind]

ind2 = which(SW_WQP_SC_NY$Longitude > 0)
SW_WQP_SC_NY$Longitude[ind2] = SW_WQP_SC_NY$Longitude[ind2] * (-1)

# Now it's clean
SW_WQP_SC_NY = SW_WQP_SC_NY %>%
  arrange(daily_mean)

View(SW_WQP_SC_NY)

#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(SW_WQP_SC_NY$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(SW_WQP_SC_NY, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(SW_WQP_SC_NY)
Q_75

### GROUND WATER

#Cleaned WQP data
#NY_GW_SC1 = readRDS(file="gw.wqp_data_NY_all_ord_corr_SC.rds")
NY_GW_SC = readRDS(file="gw.wqp_data_NY_daily_spring_well_ord_corr_SC.rds")

NY_GW_WQP_SC = NY_GW_SC[,!names(NY_GW_SC) %in% c("State","MonitoringLocationTypeName","Category")] 

names(NY_GW_WQP_SC)[names(NY_GW_WQP_SC) == 'mean_sc'] <- 'daily_mean'
names(NY_GW_WQP_SC)[names(NY_GW_WQP_SC) == 'min_sc'] <- 'min_v'
names(NY_GW_WQP_SC)[names(NY_GW_WQP_SC) == 'max_sc'] <- 'max_v'

NY_GW_WQP_SC$Latitude = as.numeric(NY_GW_WQP_SC$Latitude)
NY_GW_WQP_SC$Longitude = as.numeric(NY_GW_WQP_SC$Longitude)


#Mapping 
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(NY_GW_WQP_SC$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(NY_GW_WQP_SC, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(NY_GW_WQP_SC)
Q_75


# NOW ANALYZE
#Use function
#source("Well_spatial_analysis_compl.R")
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = NY_marginal
#unconv_well_data = enverus_og_well_curated
orphan_well_data = NY_orphaned_clean
surface_water_data = SW_WQP_SC_NY
groundwater_data = NY_GW_WQP_SC

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "SC"
meas = "uS/cm" 
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
anal = "SC" #already should be defined above
meas = "uS/cm" 
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
Geospatial_result_NY_SC = rbind(SW_marg,SW_orph,GW_marg,GW_orph)

saveRDS(Geospatial_result_NY_SC,file = "Geospatial_results_NY_SC.rds", compress = FALSE)


