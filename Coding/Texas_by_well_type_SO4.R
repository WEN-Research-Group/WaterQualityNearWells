#Upload all necessary datasets first, then do distance analysis

### WELL INFO ####

### MARGINAL
Texas_marginal = readRDS("./input/Texas_marginal_wells_curated.rds")

Texas_marginal_short = Texas_marginal %>%
  filter(!is.na(SpudDate)) %>%
  mutate(SpudDate = 
           floor_date(as.POSIXct(SpudDate, format="%Y-%m-%d"), "day")) %>%
  select(c(1:6,"County","Latitude","Longitude","SpudDate"))

#Unconventional wells
#In OG directory
enverus_og_well_curated <- readRDS("./input/Enverus_OG/enverus_og_well_curated.rds")
#Rename some columns
names(enverus_og_well_curated)[names(enverus_og_well_curated) == 'spud_date'] <- 'SpudDate'
names(enverus_og_well_curated)[names(enverus_og_well_curated) == 'surface_latitude_wgs84'] <- 'Latitude'
names(enverus_og_well_curated)[names(enverus_og_well_curated) == 'surface_longitude_wgs84'] <- 'Longitude'

#Orphaned wells
#From 2024
orphaned_well_raw_new <- read.csv(file = "20240901_USOrphanWells_Dataset.csv")
#Clean up data
#Remove samples that don't have SpudDate
orphaned_well_raw_new_c = filter(orphaned_well_raw_new,!SpudDate=="")
Texas_orphaned_all = filter(orphaned_well_raw_new, State=="Texas")
Texas_orphaned_clean = filter(orphaned_well_raw_new_c, State=="Texas")
#Some sites don't have lat/long
Texas_orphaned_clean = filter(Texas_orphaned_clean,!is.na(Latitude))
#Date format
Texas_orphaned_clean = Texas_orphaned_clean %>%
  mutate(SpudDate = 
           floor_date(as.POSIXct(SpudDate, format="%Y-%m-%d"), "day"))


### WATER QUALITY

#SURFACE

#State level samples
TX_SW_SO4 = read.csv("./input/Texas_surface_WQ/Sulfate/TX_SW_Sulfate.csv", header = TRUE)

#Analyze units
unique(TX_SW_SO4$Parameter.Description)
TX_SW_SO4 = filter(TX_SW_SO4, Parameter.Description == "SULFATE (MG/L AS SO4)")

#Change units if needed 
TX_SW_SO4$Value = TX_SW_SO4$Value *1000

#Use function to do further cleaning and USGS station name change
source("./input/Texas_surface_WQ/Cleaning_TX_SW_fun_daily.R")

#Lat/long data and other site info
TX_SW_sites = read.csv("./input/Texas_surface_WQ/SWQM_Stations.csv", header = TRUE)

#data_daily_clean_short = Cleaning_TX_SW_fun_daily(TX_SW_Na,TX_SW_sites)
Texas_state_SO4 = Cleaning_TX_SW_fun_daily(TX_SW_SO4,TX_SW_sites)
Texas_state_SO4_non_USGS = filter(Texas_state_SO4,USGS_GAUGE == "")

#Check for outliers
Texas_state_SO4_non_USGS_org = Texas_state_SO4_non_USGS %>%
  arrange(daily_mean)
View(Texas_state_SO4_non_USGS_org)

#Remove abnormally large values - above 11 656 000
Texas_state_SO4_non_USGS = filter(Texas_state_SO4_non_USGS, daily_mean < 10000000)


#Water quality portal
#All data - need to clean it still
SW_WQP_SO4 = readRDS("./input/sw_results_all_info_sulfate.rds")

#Look up these to include in the filter
unique(SW_WQP_SO4$ResultDetectionConditionText)
unique(SW_WQP_SO4$MonitoringLocationTypeName)
#unique(SW_WQP_SO4$ResultStatusIdentifier)
unique(SW_WQP_SO4$ResultMeasure.MeasureUnitCode)

#Cleaning Na data for all the states
SW_WQP_SO4_clean = SW_WQP_SO4 %>%
  rename(Latitude = LatitudeMeasure, Longitude = LongitudeMeasure, DataValue = ResultMeasureValue,
         Samplingdate = ActivityStartDate, State = StateName, County = COUNTY_NAME,
         SiteCode = MonitoringLocationIdentifier) %>%
  mutate(DataValue = as.numeric(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Samplingdate = as.POSIXct(Samplingdate,format="%Y-%M-%d")) %>%
  filter(!ResultStatusIdentifier=="Rejected",
         MonitoringLocationTypeName %in% c("River/Stream","Stream","Stream: Canal","Channelized Stream"),
         !is.na(DataValue),
         DataValue > 0,
         !is.na(Latitude),
         !is.na(Longitude),
         !is.na(Samplingdate),
         ResultDetectionConditionText %in% c(NA,"Present Above Quantification Limit"),
         ResultMeasure.MeasureUnitCode %in% c("mg/l","ug/l","mg/L","ppm","ug/L","ppb"))


unique(SW_WQP_SO4_clean$ResultDetectionConditionText)
unique(SW_WQP_SO4_clean$MonitoringLocationTypeName)
unique(SW_WQP_SO4_clean$ResultStatusIdentifier)
unique(SW_WQP_SO4_clean$ResultMeasure.MeasureUnitCode)

#Do unit conversions 
ind = which(SW_WQP_SO4_clean$ResultMeasure.MeasureUnitCode == "mg/L"|
              SW_WQP_SO4_clean$ResultMeasure.MeasureUnitCode == "mg/l"|
              SW_WQP_SO4_clean$ResultMeasure.MeasureUnitCode == "ppm")

SW_WQP_SO4_clean[ind,"DataValue"] = SW_WQP_SO4_clean[ind,"DataValue"]*1000
#All values are converted and now ug/L
SW_WQP_SO4_clean$ResultMeasure.MeasureUnitCode = "ug/L"

#Save file to use with other states
saveRDS(SW_WQP_SO4_clean, file="sw_wqp_wq_all_curated_SO4.rds", compress = FALSE)

#Now analyze for a state
#Separate Texas samples
SW_WQP_SO4_Texas = filter(SW_WQP_SO4_clean, State=="Texas")

#Calculate daily values
SW_WQP_SO4_Texas_daily = SW_WQP_SO4_Texas %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check for outliers and remove if necessary
View(SW_WQP_SO4_Texas_daily)
#Remove values above 10000000
SW_WQP_SO4_Texas_daily = filter(SW_WQP_SO4_Texas_daily, daily_mean < 10000000)

#Merge data from WQP and Texas state database

#Change data types as needed
Texas_state_SO4_non_USGS$SiteCode = as.character(Texas_state_SO4_non_USGS$SiteCode)
#Remove some columns 
Texas_state_SO4_non_USGS = Texas_state_SO4_non_USGS[,-c(3,6)]

#Now merge
SW_SO4_Texas_merged = rbind(Texas_state_SO4_non_USGS,SW_WQP_SO4_Texas_daily)

Q_75 = quantile(SW_SO4_Texas_merged$daily_mean, probs = c(0.75)) # that would be 32,000

dim(SW_SO4_Texas_merged)
Q_75

#Save this for later
saveRDS(SW_SO4_Texas_merged,file = "SW_daily_SO4_Texas_merged.rds", compress = FALSE)



#GROUNDWATER
#### Water quality portal
#Check if this has the same samples as when I clean it
gw_all_results_SO4 = readRDS("./input/water_chemistry/gw_results_curated_so4_us.rds")

#Need to clean GW dataset and separate by analyte
#Upload downloaded results instead
gw.all.results = readRDS("./input/water_chemistry/gw_results_ba_sr_so4_us.rds")
gw.all.sites = readRDS("./input/water_chemistry/gw_sites_ba_sr_so4_us.rds")
gw.all.detection <- read.csv(file = "./input/water_chemistry/gw_ba_sr_so4_us_resdetectqntlmt.csv",
                             stringsAsFactors = FALSE, na.strings = "")

#Clean the whole dataset, them separate it into Na and Cl datasets
gw.wqp_data <- merge(x = gw.all.results, y = gw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

gw.wqp_data_long <- merge(x = gw.wqp_data, y = gw.all.detection,
                     by = "ResultIdentifier", all.x = TRUE)

# Look up these to include in the filter
unique(gw.wqp_data$ResultDetectionConditionText) #not working
#table(gw.wqp_data$ResultDetectionConditionText)
unique(gw.wqp_data$MonitoringLocationTypeName)
unique(gw.wqp_data$ResultStatusIdentifier)
unique(gw.wqp_data$ResultMeasure.MeasureUnitCode)

#gw.wqp_data_backup = gw.wqp_data
#gw.wqp_data$ResultDetectionConditionText = as.character(gw.wqp_data$ResultDetectionConditionText)
#unique(gw.wqp_data$ResultDetectionConditionText)

#Cleaning data for all the states
GW_WQP_Ba_Sr_SO4_clean = gw.wqp_data %>%
  rename(Latitude = LatitudeMeasure, Longitude = LongitudeMeasure, DataValue = ResultMeasureValue,
         Samplingdate = ActivityStartDate, State = StateName, County = COUNTY_NAME,
         SiteCode = MonitoringLocationIdentifier) %>%
  mutate(DataValue = as.numeric(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Samplingdate = as.POSIXct(Samplingdate,format="%Y-%M-%d")) %>%
  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  filter(!ResultStatusIdentifier=="Rejected",
         MonitoringLocationTypeName %in% c("Well","Spring","Seep"),
         !is.na(DataValue),
         DataValue > 0,
         !is.na(Latitude),
         !is.na(Longitude),
         !is.na(Samplingdate),
         ResultDetectionConditionText %in% c(NA,"Present Above Quantification Limit","*Present >QL","*Present"),
         ResultMeasure.MeasureUnitCode %in% c("mg/l","ug/l","mg/L","ppm","ug/L","ppb"))



unique(GW_WQP_Ba_Sr_SO4_clean$ResultDetectionConditionText)
unique(GW_WQP_Ba_Sr_SO4_clean$MonitoringLocationTypeName)
unique(GW_WQP_Ba_Sr_SO4_clean$ResultStatusIdentifier)
unique(GW_WQP_Ba_Sr_SO4_clean$ResultMeasure.MeasureUnitCode)

#GW_WQP_Na_Cl_clean_backup = GW_WQP_Na_Cl_clean

#SW_WQP_Na_clean = data.frame(SW_WQP_Na_clean_backup)

#Do unit conversions 
ind = which(GW_WQP_Ba_Sr_SO4_clean$ResultMeasure.MeasureUnitCode == "mg/L"|
              GW_WQP_Ba_Sr_SO4_clean$ResultMeasure.MeasureUnitCode == "mg/l"|
              GW_WQP_Ba_Sr_SO4_clean$ResultMeasure.MeasureUnitCode == "ppm")

GW_WQP_Ba_Sr_SO4_clean[ind,"DataValue"] = GW_WQP_Ba_Sr_SO4_clean[ind,"DataValue"]*1000
#All values are converted and now ug/L
GW_WQP_Ba_Sr_SO4_clean$ResultMeasure.MeasureUnitCode = "ug/L"

GW_WQP_Ba_Sr_SO4_clean = GW_WQP_Ba_Sr_SO4_clean %>%
  mutate(Samplingdate = floor_date(Samplingdate, "day"))

#Separate into CL nd NA dataset and save this for future use
#For each state, I will have to separate samples from that state and double check that there are no outliers
#As well as calculate daily mean values

unique(GW_WQP_Ba_Sr_SO4_clean$CharacteristicName)

#Separating analytes
GW_WQP_Ba_clean = filter(GW_WQP_Ba_Sr_SO4_clean, CharacteristicName == "Barium")
GW_WQP_Sr_clean = filter(GW_WQP_Ba_Sr_SO4_clean, CharacteristicName == "Strontium")
GW_WQP_SO4_clean = filter(GW_WQP_Ba_Sr_SO4_clean, CharacteristicName %in% c("Sulfate as SO4","Sulfate as S",
                                                                            "Sulfate","Sulfur Sulfate"))
dim(gw_all_results_SO4) #Mine has more samples - not daily values

GW_WQP_SO4_clean_daily = GW_WQP_SO4_clean %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(GW_WQP_SO4_clean_daily) #- it has some outliers that will have to be removed
dim(GW_WQP_SO4_clean_daily) #mine has less, since I removed non-detects

#Save files
saveRDS(GW_WQP_Ba_clean, file="gw_wqp_wq_all_curated_Ba.rds", compress = FALSE)
saveRDS(GW_WQP_Sr_clean, file="gw_wqp_wq_all_curated_Sr.rds", compress = FALSE)
saveRDS(GW_WQP_SO4_clean, file="gw_wqp_wq_all_curated_SO4.rds", compress = FALSE)


#Focus on Texas
GW_WQP_SO4_clean_TX = filter(GW_WQP_SO4_clean, State == "Texas")

GW_WQP_SO4_clean_TX_daily = GW_WQP_SO4_clean_TX %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check for outliers
View(GW_WQP_SO4_clean_TX_daily)
#Remove values above 10000000
GW_WQP_SO4_clean_TX_daily = filter(GW_WQP_SO4_clean_TX_daily, daily_mean < 10000000)


#### TWDB Groundwater Quality Data ####
wqdetail <-  readRDS(file="./input/TWDB/gw_twdb_curated_v1.rds")
lu_param <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/LU_WaterQualityParameters.txt", sep="|", dec=".", header = TRUE, fill = TRUE)

#extract analyte data from the master table by code
gw.twdb.so4_long_clean <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(82, 83),#look up these values for each analyte
         is.na(Description),
         ParameterValue> 1) %>%
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate, DataValue = ParameterValue) %>%
  arrange(DataValue)
View(gw.twdb.so4_long_clean)

#Check units
unique(gw.twdb.so4_long_clean$ParameterLongDescription)

#Change units
gw.twdb.so4_long_clean$DataValue = gw.twdb.so4_long_clean$DataValue * 1000

#Change date type - already done
# Reorder columns and drop some
gw_twdb_so4_clean = gw.twdb.so4_long_clean %>%
  select(c(County, SiteCode, Latitude, Longitude, Samplingdate, DataValue))

#Calculate daily averages 
TX_GW_TWDB_SO4_part1 = gw_twdb_so4_clean %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  mutate(SiteCode = as.character(SiteCode)) %>%
  arrange(daily_mean)
#Check for outliers
View(TX_GW_TWDB_SO4_part1)

#Remove largest values - clearly an outliers  (keep values below 113138) 113138000
TX_GW_TWDB_SO4 = filter(TX_GW_TWDB_SO4_part1, daily_mean < 100000000)


dim(TX_GW_TWDB_SO4)
dim(GW_WQP_SO4_clean_TX_daily)
#Now merge two datasets
GW_SO4_Texas_merged = rbind(TX_GW_TWDB_SO4,GW_WQP_SO4_clean_TX_daily)

Q_75 = quantile(GW_SO4_Texas_merged$daily_mean, probs = c(0.75)) # that would be 32,000

dim(GW_SO4_Texas_merged)
Q_75

#Save this for later
saveRDS(GW_SO4_Texas_merged,file = "GW_all_SO4_Texas_merged_corr.rds", compress = FALSE)




# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = Texas_marginal_short
unconv_well_data = enverus_og_well_curated
orphan_well_data = Texas_orphaned_clean
surface_water_data = SW_SO4_Texas_merged
groundwater_data = GW_SO4_Texas_merged


#SURFACE

#MARGINAL
#sw.wq.distance = Well_spatial_analysis_compl(Texas_marginal_short,SW_SO4_Texas_merged)
sw.wq.distance = Well_spatial_analysis_compl_0127(marginal_well_data,surface_water_data)

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "SO4"
meas = "ug/L" 
state = "Texas" 
type_w = "SW"
well_type = "marginal"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(sw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_marginal.rds", compress = FALSE)
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL

#sw.wq.distance = Well_spatial_analysis_compl(enverus_og_well_curated,SW_Cl_Texas_merged)
sw.wq.distance = Well_spatial_analysis_compl_0127(unconv_well_data,surface_water_data)

well_type = "unconventional"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(sw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_unconv.rds", compress = FALSE)
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

#sw.wq.distance = Well_spatial_analysis_compl(Texas_orphaned_clean,SW_Na_Texas_merged)
sw.wq.distance = Well_spatial_analysis_compl_0127(orphan_well_data,surface_water_data)

well_type = "orphaned"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(sw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_orphaned.rds", compress = FALSE)
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


#GROUNDWATER

#MARGINAL
#gw.wq.distance = Well_spatial_analysis_compl(Texas_marginal_short,GW_Cl_Texas_merged)
gw.wq.distance = Well_spatial_analysis_compl_0127(marginal_well_data,groundwater_data)

type_w = "GW"
well_type = "marginal"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(gw.wq.distance,file = "./output/AGU/Texas/Texas_GW_Cl_marginal.rds", compress = FALSE)
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL

#gw.wq.distance = Well_spatial_analysis_compl(enverus_og_well_curated,GW_Cl_Texas_merged)
gw.wq.distance = Well_spatial_analysis_compl_0127(unconv_well_data,groundwater_data)

well_type = "unconventional"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(gw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_unconv.rds", compress = FALSE)
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

#gw.wq.distance = Well_spatial_analysis_compl(Texas_orphaned_clean,GW_Na_Texas_merged)
gw.wq.distance = Well_spatial_analysis_compl_0127(orphan_well_data,groundwater_data)

well_type = "orphaned"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(gw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_orphaned.rds", compress = FALSE)
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)






#### GEOSPATIAL RESULTS ####
############################

#Compile all values into one file
#define these for every distance calculation
anal = "SO4" #already should be defined above
meas = "ug/L" 
state = "Texas" 

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


# #Double check if duplicated site number samples are removed
SW_marg = SW_marg[-which(SW_marg$max_v - SW_marg$min_v > 1000),]
#SW_unconv = filter(SW_unconv, daily_mean < 50000)
plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))

SW_marg_fx = SW_marg %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

SW_marg_fx_comb <- SW_marg[!duplicated(SW_marg[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
SW_marg_fx_comb_sort2 = SW_marg_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)
SW_marg_fx_f2 = cbind(SW_marg_fx[,1:2],SW_marg_fx_comb_sort2[,3:4],SW_marg_fx[,3:12])

#backup
SW_marg_backup = SW_marg
SW_marg = SW_marg_fx_f2
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"
plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
cor(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))


#See how many repeated values I have 
## #Try remove that value that occurs a lot
SW_marg_val_sum = SW_marg %>%
  group_by(daily_mean) %>%
  summarise(n_val = n()) %>%
  arrange(n_val)

# SW_marg_nrs = merge(SW_marg, SW_marg_val_sum, by = "daily_mean")
# SW_marg_nrs_f = filter(SW_marg_nrs, n_val < 180) #removed 500, 1000, 5000, 10000, 110000
# plot(log(SW_marg_nrs_f$nearest_distance_m), log(SW_marg_nrs_f$daily_mean))
# cor(log(SW_marg_nrs_f$nearest_distance_m), log(SW_marg_nrs_f$daily_mean)) #worse than with outlies
# SW_marg = GW_orph_nrs_f[,c(1:16)]



### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
cor(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))

# #Try remove that value that occurs a lot
# SW_unconv_val_sum = SW_unconv %>%
#   group_by(daily_mean) %>%
#   summarise(n_val = n()) %>%
#   arrange(n_val)
# 
# SW_unconv_nrs = merge(SW_unconv, SW_unconv_val_sum, by = "daily_mean")
# SW_unconv_nrs_f = filter(SW_unconv_nrs, n_val < 100)
# plot(log(SW_unconv_nrs_f$nearest_distance_m), log(SW_unconv_nrs_f$daily_mean))
# cor(log(SW_unconv_nrs_f$nearest_distance_m), log(SW_unconv_nrs_f$daily_mean)) #worse than with outlies
# #SW_unconv = SW_unconv_nrs_f[,c(1:16)]

#Double check if duplicated site number samples are removed
SW_unconv = SW_unconv[-which(SW_unconv$max_v - SW_unconv$min_v > 1000),] 
#SW_unconv = filter(SW_unconv, daily_mean < 50000)
plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))

SW_unconv_fx = SW_unconv %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

SW_unconv_fx_comb <- SW_unconv[!duplicated(SW_unconv[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
SW_unconv_fx_comb_sort2 = SW_unconv_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)
SW_unconv_fx_f2 = cbind(SW_unconv_fx[,1:2],SW_unconv_fx_comb_sort2[,3:4],SW_unconv_fx[,3:12])

#backup
SW_unconv_backup = SW_unconv
SW_unconv = SW_unconv_fx_f2
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"
plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
cor(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))


### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"

plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))
#plot((SW_orph$nearest_distance_m), log(SW_orph$daily_mean))
cor(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))

#Should Remove values with nearest distances above 161796

SW_orph = SW_orph[-which(SW_orph$max_v - SW_orph$min_v > 1000),] 
#SW_orph = filter(SW_orph, daily_mean < 50000)
plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))

SW_orph_fx = SW_orph %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

SW_orph_fx_comb <- SW_orph[!duplicated(SW_orph[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
SW_orph_fx_comb_sort2 = SW_orph_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)
SW_orph_fx_f2 = cbind(SW_orph_fx[,1:2],SW_orph_fx_comb_sort2[,3:4],SW_orph_fx[,3:12])

#backup
SW_orph_backup = SW_orph
SW_orph = SW_orph_fx_f2
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"
plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))
cor(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))


#GROUNDWATER

#MARGINAL
type_w = "GW"
well_type = "marginal"

GW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_marg$type_w = "GW"
GW_marg$well_type = "marginal"

plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
cor(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))

# GW_marg = filter(GW_marg, nearest_distance_m < 6283100)
# plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
# cor(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
# 
# # #Try remove that value that occurs a lot
# GW_marg_val_sum = GW_marg %>%
#   group_by(daily_mean) %>%
#   summarise(n_val = n()) %>%
#   arrange(n_val)
# 
# GW_marg_nrs = merge(GW_marg, GW_marg_val_sum, by = "daily_mean")
# GW_marg_nrs_f = filter(GW_marg_nrs, n_val < 180) #removed 500, 1000, 5000, 10000, 110000
# plot(log(GW_marg_nrs_f$nearest_distance_m), log(GW_marg_nrs_f$daily_mean))
# cor(log(GW_marg_nrs_f$nearest_distance_m), log(GW_marg_nrs_f$daily_mean)) #worse than with outlies
# GW_marg = GW_marg_nrs_f[,c(1:16)]

# GW_marg = filter(GW_marg, nearest_distance_m > 1)
# plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
# 
# dim(unique(GW_marg[,c("Latitude","Longitude", "Samplingdate")])) #38544 in dataset but 36571 unique combos
GW_marg = GW_marg[-which(GW_marg$max_v - GW_marg$min_v > 1000),] #38517
plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))

GW_marg_fx = GW_marg %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

GW_marg_fx_comb <- GW_marg[!duplicated(GW_marg[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
GW_marg_fx_comb_sort2 = GW_marg_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)
GW_marg_fx_f2 = cbind(GW_marg_fx[,1:2],GW_marg_fx_comb_sort2[,3:4],GW_marg_fx[,3:12])

#backup
GW_marg_backup = GW_marg
GW_marg = GW_marg_fx_f2
GW_marg$type_w = "GW"
GW_marg$well_type = "marginal"
plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
cor(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))


### UNCONVENTIONAL

well_type = "unconventional"

GW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_unconv$type_w = "GW"
GW_unconv$well_type = "unconventional"

#GW_unconv = filter(GW_unconv, nearest_distance_m < 6283100)
plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))
cor(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))

GW_unconv = GW_unconv[-which(GW_unconv$max_v - GW_unconv$min_v > 1000),] 

# ## #Try remove that value that occurs a lot
# GW_unconv_val_sum = GW_unconv %>%
#   group_by(daily_mean) %>%
#   summarise(n_val = n()) %>%
#   arrange(n_val)
# 
# GW_unconv_nrs = merge(GW_unconv, GW_unconv_val_sum, by = "daily_mean")
# GW_unconv_nrs_f = filter(GW_unconv_nrs, n_val < 180) #removed 500, 1000, 5000, 10000, 110000
# plot(log(GW_unconv_nrs_f$nearest_distance_m), log(GW_unconv_nrs_f$daily_mean))
# cor(log(GW_unconv_nrs_f$nearest_distance_m), log(GW_unconv_nrs_f$daily_mean)) #worse than with outlies
# GW_unconv = GW_unconv_nrs_f[,c(1:16)]

# GW_unconv_val_sum = GW_unconv %>%
#   group_by(daily_mean) %>%
#   summarise(n_val = n()) %>%
#   arrange(n_val)
# 
# GW_unconv_nrs = merge(GW_unconv, GW_unconv_val_sum, by = "daily_mean")
# GW_unconv_nrs_f = filter(GW_unconv_nrs, n_val < 100)
# GW_unconv = GW_unconv_nrs_f[,c(1:16)]
# plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))

GW_unconv_fx = GW_unconv %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

GW_unconv_fx_comb <- GW_unconv[!duplicated(GW_unconv[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
GW_unconv_fx_comb_sort2 = GW_unconv_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)
GW_unconv_fx_f2 = cbind(GW_unconv_fx[,1:2],GW_unconv_fx_comb_sort2[,3:4],GW_unconv_fx[,3:12])

#backup
GW_unconv_backup = GW_unconv
GW_unconv = GW_unconv_fx_f2
GW_unconv$type_w = "GW"
GW_unconv$well_type = "unconventional"
plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))
cor(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))



### ORPHANED

well_type = "orphaned"

GW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_orph$type_w = "GW"
GW_orph$well_type = "orphaned"

#GW_orph = filter(GW_orph, nearest_distance_m < 6283100)
plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))
cor(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))

GW_orph = GW_orph[-which(GW_orph$max_v - GW_orph$min_v > 1000),] 

# ## #Try remove that value that occurs a lot
# GW_orph_val_sum = GW_orph %>%
#   group_by(daily_mean) %>%
#   summarise(n_val = n()) %>%
#   arrange(n_val)
# 
# GW_orph_nrs = merge(GW_orph, GW_orph_val_sum, by = "daily_mean")
# GW_orph_nrs_f = filter(GW_orph_nrs, n_val < 180) #removed 500, 1000, 5000, 10000, 110000
# plot(log(GW_orph_nrs_f$nearest_distance_m), log(GW_orph_nrs_f$daily_mean))
# cor(log(GW_orph_nrs_f$nearest_distance_m), log(GW_orph_nrs_f$daily_mean)) #worse than with outlies
# GW_orph = GW_orph_nrs_f[,c(1:16)]

# GW_orph_val_sum = GW_orph %>%
#   group_by(daily_mean) %>%
#   summarise(n_val = n()) %>%
#   arrange(n_val)
# 
# GW_orph_nrs = merge(GW_orph, GW_orph_val_sum, by = "daily_mean")
# GW_orph_nrs_f = filter(GW_orph_nrs, n_val < 100)
# GW_orph = GW_orph_nrs_f[,c(1:16)]
# plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))

GW_orph_fx = GW_orph %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

GW_orph_fx_comb <- GW_orph[!duplicated(GW_orph[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
GW_orph_fx_comb_sort2 = GW_orph_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)
GW_orph_fx_f2 = cbind(GW_orph_fx[,1:2],GW_orph_fx_comb_sort2[,3:4],GW_orph_fx[,3:12])

#backup
GW_orph_backup = GW_orph
GW_orph = GW_orph_fx_f2
GW_orph$type_w = "GW"
GW_orph$well_type = "orphaned"
plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))
cor(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))

plot((GW_orph$nearest_distance_m), log(GW_orph$daily_mean))
cor((GW_orph$nearest_distance_m), log(GW_orph$daily_mean))


########### SAVE ALL RESULTS
#Combine all 6 into 1 and save
Geospatial_result_TX_SO4 = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_TX_SO4,file = "Geospatial_results_TX_SO4.rds", compress = FALSE)










