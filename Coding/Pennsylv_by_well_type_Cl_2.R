#Upload all necessary datasets first, then do distance analysis

### WELL INFO ####

### MARGINAL
PA_marginal_list <- read.csv(file="./input/Pen_marginal.csv",header = FALSE)[,1]
#All wells from enverus database
PA_all_wells <- read.csv(file="./input/PA_wells_enverus.csv",header = TRUE, na.strings = c(""),
                         stringsAsFactors = F,
                         colClasses = "character")
#Keep only marginal wells from enverus database
PA_list = PA_all_wells$API_UWI_14_Unformatted %in% as.character(PA_marginal_list)
summary(PA_list)
PA_marginal_data = PA_all_wells[PA_list,]

#Rename lat long columns in PA_marginal dataset so that it would be "Longitude" and "Latitude"
#Also coordinates need to be numeric
#converting Lat/Long to numeric values
PA_marginal_data$Latitude = as.numeric(PA_marginal_data$Latitude)
PA_marginal_data$Longitude = as.numeric(PA_marginal_data$Longitude)

colnames(PA_marginal_data)
#Need to remove wells that have Na Spud dates
PA_marginal_data = filter(PA_marginal_data, !is.na(SpudDate))

#Convert date format just to be safe
PA_marginal_data$SpudDate = as.POSIXct(PA_marginal_data$SpudDate,format="%Y-%M-%d")
#Don't need time
PA_marginal_data$SpudDate = floor_date(as.POSIXct(PA_marginal_data$SpudDate, format="%Y-%M-%d"), "day")

#Keep only cenrtain columns
PA_marginal = data.frame(PA_marginal_data[,c(1:6)],PA_marginal_data[,c("Latitude","Longitude","SpudDate")])


### UNCONVENTIONAL

#In OG directory
padep_uncon_og_well_curated <- readRDS("./input/PADEP_OG/padep_uncon_og_well_curated.rds")
#Rename some columns
names(padep_uncon_og_well_curated)[names(padep_uncon_og_well_curated) == 'SPUD_DATE'] <- 'SpudDate'
names(padep_uncon_og_well_curated)[names(padep_uncon_og_well_curated) == 'LATITUDE'] <- 'Latitude'
names(padep_uncon_og_well_curated)[names(padep_uncon_og_well_curated) == 'LONGITUDE'] <- 'Longitude'

#Convert date format just to be safe
padep_uncon_og_well_curated$SpudDate = as.POSIXct(padep_uncon_og_well_curated$SpudDate,format="%Y-%M-%d")


### ORPHANED

orphaned_well_raw_new <- read.csv(file = "20240901_USOrphanWells_Dataset.csv")
#Clean up data
#Remove samples that don't have SpudDate
orphaned_well_raw_new_c = filter(orphaned_well_raw_new,!SpudDate=="")
#A lot of samples don't have spud date, so a lot of wells are eliminated after this step
#Texas_orphaned_all = filter(orphaned_well_raw_new, State=="Texas")
PA_orphaned_clean = filter(orphaned_well_raw_new_c, State=="Pennsylvania")
#Some sites don't have lat/long
PA_orphaned_clean = filter(PA_orphaned_clean,!is.na(Latitude))
#Date format
PA_orphaned_clean = PA_orphaned_clean %>%
  mutate(SpudDate = 
           floor_date(as.POSIXct(SpudDate, format="%Y-%m-%d"), "day"))


### WATER QUALITY

anal = "Cl"
### SURFACE

#Water Quality Portal
#Raw dataset was cleaned when I analyzed Texas data
#Upload cleaned data and extract NY state
SW_WQP_anal = readRDS(file=paste("sw_wqp_wq_all_curated_",anal,".rds",sep=""))


#Separate Texas samples
SW_WQP_anal_State = filter(SW_WQP_anal, State=="Pennsylvania")

#Calculate daily values
SW_WQP_anal_daily = SW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(SW_WQP_anal_daily)
SW_WQP_anal_daily_Cl_copy = SW_WQP_anal_daily
View(SW_WQP_anal_daily_Cl_copy)

#Remove a few largest values
SW_WQP_Cl_PA = filter(SW_WQP_anal_daily, daily_mean < 2000000)


#SHALE NETWORK

#### Shale Network Data ####
Cl_Shale_raw = read.csv(file="./input/NewShaleDownload/Cl_0115.csv", na.strings = "", stringsAsFactors = FALSE)

unique(Cl_Shale_raw$State)
unique(Cl_Shale_raw$UnitsAbbreviation)
unique(Cl_Shale_raw$VariableName) 
unique(Cl_Shale_raw$SampleMedium)

Cl_Shale_curated <- Cl_Shale_raw %>%
  filter(SampleMedium %in% c("Surface Water", "Surface water", "Groundwater"),
         !CensorCode == "lt",
         Latitude > 0,
         Longitude < -70,
         State %in% c("Pennsylvania", "PA", "Pennsylvania ","Pa", "PENNSYLVANIA", "Pennsylvannia"),
         DataValue != -9999,
         DataValue > 0) %>%
  filter(!(UnitsAbbreviation %in% c("-"))) %>%
  ## convert mM to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mM"),
                            DataValue * 35453, DataValue)) %>%
  #convert mg/L to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mg/Kg", "mg/L", 
                                                     "ppm", "ug/mL"),
                            DataValue * 1000, DataValue)) %>%
  mutate(UnitsAbbreviation = "ug/L") %>%
  ## remove columns not needed
  # format sampling date
  mutate(LocalDateTime = as.POSIXct(LocalDateTime,
                                    format="%m/%d/%y")) %>%
  # create a column for sampling DAY
  mutate(Samplingdate = floor_date(LocalDateTime, "day")) %>%
  filter(DataValue > 1) %>%
  arrange(DataValue)

View(Cl_Shale_curated)

#Remove some of the categories
unique(Cl_Shale_curated$SiteType)
unique(Cl_Shale_curated$SampleMedium)

# extract groundwater and surface water samples

sw.sn_curated_all <- Cl_Shale_curated %>%
  filter(SampleMedium %in% c("Surface Water","Surface water"),
         SiteType %in% c("Stream")) 
View(sw.sn_curated_all)


#Calculate daily averages and rename columns
SW_Shale_anal_daily = sw.sn_curated_all %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(SW_Shale_anal_daily)

#Need to remove the largest value
SW_Shale_Cl_PA = filter(SW_Shale_anal_daily, daily_mean < 200000)


#Merge two datasets together and save
SW_Na_Cl_merged = rbind(SW_Shale_Cl_PA, SW_WQP_Cl_PA)

SW_Na_Cl_merged = SW_Na_Cl_merged %>%
  arrange(daily_mean)
View(SW_Na_Cl_merged)

#Filter if needed 
#Remove values higher than 1260000
SW_Na_Cl_merged = filter(SW_Na_Cl_merged, daily_mean < 1260000)

#Save this for later
saveRDS(SW_Na_Cl_merged,file = "SW_all_Cl_Penn_merged_0115.rds", compress = FALSE)



# Now it's clean
SW_Na_Cl_merged_org = SW_Na_Cl_merged %>%
  arrange(daily_mean)
View(SW_Na_Cl_merged_org)
#ind = which(SW_Na_PA_merged_org$Longitude > 0)
#SW_Na_PA_merged_org$Longitude[ind] = SW_Na_PA_merged_org$Longitude[ind] *(-1)
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(SW_Na_Cl_merged_org$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(SW_Na_Cl_merged_org, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(SW_Na_Cl_merged_org)
Q_75




### GROUND WATER

# SHALE NETWORK
gw.sn_curated_all <- Cl_Shale_curated %>%
  filter(SampleMedium == "Groundwater",
         SiteType %in% c("Well", "Spring"))
#View(gw.sn_sc_curated_all)

#Calculate daily averages and rename columns
GW_Shale_anal_daily = gw.sn_curated_all %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(GW_Shale_anal_daily)

#Need to remove the largest values
GW_Shale_Cl_PA = filter(GW_Shale_anal_daily, daily_mean < 2000000)


#WATER QUALITY PORTAL
#Cleaned WQP data
GW_WQP_anal = readRDS(file=paste("gw_wqp_wq_all_curated_",anal,".rds",sep=""))

#Separate Texas samples
GW_WQP_anal_State = filter(GW_WQP_anal, State=="Pennsylvania")

#Calculate daily values
GW_WQP_anal_daily = GW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(GW_WQP_anal_daily)

#Remove a few largest values
GW_WQP_Cl_PA = filter(GW_WQP_anal_daily, daily_mean < 4000000)

#Merge two datasets together and save
GW_Cl_PA_merged = rbind(GW_Shale_Cl_PA, GW_WQP_Cl_PA)

GW_Cl_PA_merged = GW_Cl_PA_merged %>%
  arrange(daily_mean)
View(GW_Cl_PA_merged)

#No need to do further filtering

#Save this for later
saveRDS(GW_Cl_PA_merged,file = "GW_all_Cl_Penn_merged_0115.rds", compress = FALSE)


# Now it's clean
GW_Cl_PA_merged_org = GW_Cl_PA_merged %>%
  arrange(daily_mean)
View(GW_Cl_PA_merged_org)
#ind = which(SW_Na_PA_merged_org$Longitude > 0)
#SW_Na_PA_merged_org$Longitude[ind] = SW_Na_PA_merged_org$Longitude[ind] *(-1)
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(GW_Cl_PA_merged_org$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(GW_Cl_PA_merged_org, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(GW_Cl_PA_merged_org)
Q_75


#Upload SW and GW data
SW_Cl_PA_merged = readRDS("SW_all_Cl_Penn_merged_0115.rds")
GW_Cl_PA_merged = readRDS("GW_all_Cl_Penn_merged_0115.rds")



# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = PA_marginal
unconv_well_data = padep_uncon_og_well_curated
orphan_well_data = PA_orphaned_clean
#surface_water_data = SW_Na_Cl_merged
surface_water_data = SW_Cl_PA_merged
groundwater_data = GW_Cl_PA_merged

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "Cl" #already should be defined above
meas = "ug/L" 
state = "Pennsylvania" 

#SURFACE

#MARGINAL
sw.wq.distance = Well_spatial_analysis_compl_0127(marginal_well_data,surface_water_data)

type_w = "SW"
well_type = "marginal"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL

sw.wq.distance = Well_spatial_analysis_compl_0127(unconv_well_data,surface_water_data)

well_type = "unconventional"
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


### UNCONVENTIONAL

gw.wq.distance = Well_spatial_analysis_compl_0127(unconv_well_data,groundwater_data)

well_type = "unconventional"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)



### ORPHANED

gw.wq.distance = Well_spatial_analysis_compl_0127(orphan_well_data,groundwater_data)

well_type = "orphaned"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)





##################

#Compile all values into one file
#define these for every distance calculation
anal = "Cl" #already should be defined above
meas = "ug/L" 
state = "Pennsylvania" 

#SURFACE

#MARGINAL

type_w = "SW"
well_type = "marginal"

#Read analysis results
SW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"
#32000 values

plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
#Remove values above 1000
#SW_marg = filter(SW_marg, daily_mean < 1000) #31940 values

### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
#Remove values above 1000
#SW_unconv = filter(SW_unconv, daily_mean < 1000) #31940 values

### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"

#Remove values above 1000
#SW_orph = filter(SW_orph, daily_mean < 1000) #31940 values
plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))

#Would be good to explore how many sites and if correlation would be stronger if we used site averages
SW_orph_sit_nrs = SW_orph %>%
  group_by(SiteCode, Latitude, Longitude) %>%
  summarise(n_per_site = n(),site_mean = mean(daily_mean), near_dist_site = mean(nearest_distance_m))

plot(log(SW_orph_sit_nrs$near_dist_site), log(SW_orph_sit_nrs$site_mean))
cor(log(SW_orph_sit_nrs$near_dist_site), log(SW_orph_sit_nrs$site_mean))

#GROUNDWATER

#MARGINAL
type_w = "GW"
well_type = "marginal"

GW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_marg$type_w = "GW"
GW_marg$well_type = "marginal"

plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))

GW_marg = filter(GW_marg, nearest_distance_m > 1)
plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))

GW_marg_val_sum = GW_marg %>%
  group_by(daily_mean) %>%
  summarise(n_val = n()) %>%
  arrange(n_val)


GW_marg_nrs = merge(GW_marg, GW_marg_val_sum, by = "daily_mean")
GW_marg_nrs_f = filter(GW_marg_nrs, n_val < 100)
plot(log(GW_marg_nrs_f$nearest_distance_m), log(GW_marg_nrs_f$daily_mean))
GW_marg = GW_marg_nrs_f[,c(1:16)]

### UNCONVENTIONAL

well_type = "unconventional"

GW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_unconv$type_w = "GW"
GW_unconv$well_type = "unconventional"

plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))

GW_unconv_val_sum = GW_unconv %>%
  group_by(daily_mean) %>%
  summarise(n_val = n()) %>%
  arrange(n_val)

GW_unconv_nrs = merge(GW_unconv, GW_unconv_val_sum, by = "daily_mean")
GW_unconv_nrs_f = filter(GW_unconv_nrs, n_val < 100)
GW_unconv = GW_unconv_nrs_f[,c(1:16)]
plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))


### ORPHANED

well_type = "orphaned"

GW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_orph$type_w = "GW"
GW_orph$well_type = "orphaned"

plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))

GW_orph_val_sum = GW_orph %>%
  group_by(daily_mean) %>%
  summarise(n_val = n()) %>%
  arrange(n_val)

GW_orph_nrs = merge(GW_orph, GW_orph_val_sum, by = "daily_mean")
GW_orph_nrs_f = filter(GW_orph_nrs, n_val < 100)
GW_orph = GW_orph_nrs_f[,c(1:16)]
plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))

#Make sure all outliers are removed before combining into 1 dataset


#Combine all 6 into 1 and save
Geospatial_result_PA_Cl = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_PA_Cl,file = "Geospatial_results_PA_Cl.rds", compress = FALSE)

