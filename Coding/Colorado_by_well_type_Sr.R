### WELL INFO ####

### MARGINAL
# Marginal well lists
CO_marginal_list <- read.csv(file="./input/CO_marginal.csv",header = FALSE)[,1]
# Add a zero before each well number since excel removed it
CO_marginal_list_ch = paste('0',as.character(CO_marginal_list),sep = "")
#All wells from enverus database
CO_all_wells <- read.csv(file="./input/CO_wells_enverus.csv",header = TRUE, na.strings = c(""),
                         stringsAsFactors = F,
                         colClasses = "character")
#Keep only marginal wells from enverus database
CO_list = CO_all_wells$API_UWI_14_Unformatted %in% CO_marginal_list_ch
summary(CO_list)
CO_marginal_data = CO_all_wells[CO_list,]

colnames(CO_marginal_data)

#Also coordinates need to be numeric
#converting Lat/Long to numeric values
CO_marginal_data$Latitude = as.numeric(CO_marginal_data$Latitude)
CO_marginal_data$Longitude = as.numeric(CO_marginal_data$Longitude)

#Convert date format just to be safe
#Don't need time
CO_marginal_data$SpudDate = floor_date(as.POSIXct(CO_marginal_data$SpudDate, format="%Y-%M-%d"), "day")

#Need to remove wells that have NA Spud dates
CO_marginal_data = filter(CO_marginal_data, !is.na(SpudDate))

#Keep only certain columns
CO_marginal = data.frame(CO_marginal_data[,c(1:6)],CO_marginal_data[,c("Latitude","Longitude","SpudDate")])


### UNCONVENTIONAL

cogcc_uncon_og_well_curated <- readRDS("./input/COGCC_OG/cogcc_uncon_og_well_curated.rds")
#Rename some columns
names(cogcc_uncon_og_well_curated)[names(cogcc_uncon_og_well_curated) == 'Spud_Date'] <- 'SpudDate'

#Convert date format just to be safe
cogcc_uncon_og_well_curated$SpudDate = as.POSIXct(cogcc_uncon_og_well_curated$SpudDate,format="%Y-%M-%d")


### ORPHANED

orphaned_well_raw_new <- read.csv(file = "20240901_USOrphanWells_Dataset.csv")
#Clean up data
#Remove samples that don't have SpudDate
orphaned_well_raw_new_c = filter(orphaned_well_raw_new,!SpudDate=="")
#A lot of samples don't have spud date, so a lot of wells are eliminated after this step
CO_orphaned_clean = filter(orphaned_well_raw_new_c, State=="Colorado")
#Some sites don't have lat/long
CO_orphaned_clean = filter(CO_orphaned_clean,!is.na(Latitude))
#Date format
CO_orphaned_clean = CO_orphaned_clean %>%
  mutate(SpudDate = 
           floor_date(as.POSIXct(SpudDate, format="%Y-%m-%d"), "day"))




### WATER QUALITY

anal = "Sr"

### SURFACE

#Water Quality Portal
#Raw dataset was cleaned when I analyzed Texas data
#Upload cleaned data and extract NY state
SW_WQP_anal = readRDS(file=paste("sw_wqp_wq_all_curated_",anal,".rds",sep=""))

#Separate Texas samples
SW_WQP_anal_State = filter(SW_WQP_anal, State=="Colorado")

#Calculate daily values
SW_WQP_anal_daily = SW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(SW_WQP_anal_daily)

#Remove a few largest values
SW_WQP_Sr_CO = filter(SW_WQP_anal_daily, daily_mean < 7000 & daily_mean > 1 & max_v/min_v < 100)
View(SW_WQP_Sr_CO)

### COGCC
#Remove GARFIELD data

cogcc_wq_raw <- readxl::read_xlsx(path="./input/COGCC_WQ/COGCC_BasicQuery.xlsx",
                                  na="") %>% janitor::clean_names()

cogcc_wq_curated <- cogcc_wq_raw %>%
  # select only non-NA SC data from relevant surface water and
  # groundwater samples
  filter(param_description %in%
           c("STRONTIUM"),
         facility_type %in%
           c("Creek","Domestic Well", "Ground Water", "Groundwater",
             "Monitoring Well", "Pond", "River", "Seep", "Spring",
             "Surface Water"),
         !matrix %in% c("GAS", "LIQUID", "SOIL"),
         !is.na(result_value),
         !is.na(latitude83),
         !units == "mg/L as CaCO3") %>%
  # convert all units to ug/L
  mutate(result_value = ifelse(units %in% c("mg/Kg","mg/l","mg/L", "MG/L"),
                               result_value * 1000, result_value)) %>%
  mutate(units = "ug/L") %>%
  # add a column 'State'
  mutate(State="Colorado") %>%
  # select only needed columns and rename them in a way consistent with others
  #  select(c(facility_id, facility_type, latitude83, longitude83, State,
  #           county,sample_date,result_value, param_description)) %>%
  rename(SiteCode = facility_id, Latitude = latitude83, Longitude = longitude83,
         County = county, Samplingdate = sample_date, Analyte=param_description,
         SampleMedium = facility_type, DataValue = result_value) %>%
  # derive the day of sampling date
  mutate(Samplingdate = floor_date(as.POSIXct(Samplingdate, format="%Y-%M-%d"), "day")) %>%
  mutate(SiteCode = as.character(SiteCode)) %>%
  filter(!County == "GARFIELD") 

sw.cogcc_wq_curated <- cogcc_wq_curated %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water"))

#calculate daily means
cogcc_wq_curated_daily = sw.cogcc_wq_curated %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean) %>%
  filter(daily_mean > 1)

#Analyze outliers
View(cogcc_wq_curated_daily)
SW_COGCC_Sr_CO = filter(cogcc_wq_curated_daily, max_v/min_v < 100)


#Merge two datasets together and save
SW_Sr_CO_merged = rbind(SW_COGCC_Sr_CO, SW_WQP_Sr_CO)
View(SW_Sr_CO_merged)

#Save this for later
saveRDS(SW_Sr_CO_merged,file = "SW_all_Sr_CO_merged.rds", compress = FALSE)



### GROUNDWATER

### COGCC
gw.cogcc_wq_curated <- cogcc_wq_curated %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring"))

#calculate daily means
cogcc_wq_curated_daily = gw.cogcc_wq_curated %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean) %>%
  filter(daily_mean > 1)

#Analyze outliers
View(cogcc_wq_curated_daily)
GW_COGCC_Sr_CO = filter(cogcc_wq_curated_daily, daily_mean < 11000 & max_v/min_v < 100)



#WATER QUALITY PORTAL
#Cleaned WQP data
GW_WQP_anal = readRDS(file=paste("gw_wqp_wq_all_curated_",anal,".rds",sep=""))

#Separate Texas samples
GW_WQP_anal_State = filter(GW_WQP_anal, State=="Colorado")

#Calculate daily values
GW_WQP_anal_daily = GW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(GW_WQP_anal_daily)
#Again, there are a lot of samples that have huge differenes between min and max - likely unit issue
#Remove all samples that max_v/man_v > 100
GW_WQP_anal_daily_e = filter(GW_WQP_anal_daily, max_v/min_v > 100)
View(GW_WQP_anal_daily_e)

#Remove a few largest values
GW_WQP_Sr_CO = filter(GW_WQP_anal_daily, daily_mean < 20000 & daily_mean > 1 & max_v/min_v < 100)

#Merge two datasets together and save
GW_Sr_CO_merged = rbind(GW_COGCC_Sr_CO, GW_WQP_Sr_CO)

#Save this for later
saveRDS(GW_Sr_CO_merged,file = "GW_all_Sr_CO_merged.rds", compress = FALSE)


# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = CO_marginal
unconv_well_data = cogcc_uncon_og_well_curated
orphan_well_data = CO_orphaned_clean
surface_water_data = SW_Sr_CO_merged
groundwater_data = GW_Sr_CO_merged

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "Sr" #already should be defined above
meas = "ug/L" 
state = "Colorado" 

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









################################
##################

#Compile all values into one file
#define these for every distance calculation
anal = "Sr" #already should be defined above
meas = "ug/L" 
state = "Colorado" 

#SURFACE

#MARGINAL

type_w = "SW"
well_type = "marginal"

#Read analysis results
SW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"
#97637 values

plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
cor(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))


### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
cor(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))


### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,881:884)]
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

plot(log(GW_marg$num_well_1km[which(GW_marg$num_well_1km>0)]), log(GW_marg$daily_mean[which(GW_marg$num_well_1km>0)]))
cor(log(GW_marg$num_well_1km[which(GW_marg$num_well_1km>0)]), log(GW_marg$daily_mean[which(GW_marg$num_well_1km>0)]))

plot(log(GW_marg$num_well_3km[which(GW_marg$num_well_3km>0)]), log(GW_marg$daily_mean[which(GW_marg$num_well_3km>0)]))
cor(log(GW_marg$num_well_3km[which(GW_marg$num_well_3km>0)]), log(GW_marg$daily_mean[which(GW_marg$num_well_3km>0)]))


### UNCONVENTIONAL

well_type = "unconventional"

GW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_unconv$type_w = "GW"
GW_unconv$well_type = "unconventional"

#GW_unconv = filter(GW_unconv, nearest_distance_m < 6283100)
plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))
cor(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))


plot(log(GW_unconv$num_well_1km[which(GW_unconv$num_well_1km>0)]), log(GW_unconv$daily_mean[which(GW_unconv$num_well_1km>0)]))
cor(log(GW_unconv$num_well_1km[which(GW_unconv$num_well_1km>0)]), log(GW_unconv$daily_mean[which(GW_unconv$num_well_1km>0)]))




### ORPHANED

well_type = "orphaned"

GW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,881:884)]
GW_orph$type_w = "GW"
GW_orph$well_type = "orphaned"

#GW_orph = filter(GW_orph, nearest_distance_m < 6283100)
plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))
cor(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))


plot(log(GW_orph$num_well_1km[which(GW_orph$num_well_1km>0)]), log(GW_orph$daily_mean[which(GW_orph$num_well_1km>0)]))
cor(log(GW_orph$num_well_1km[which(GW_orph$num_well_1km>0)]), log(GW_orph$daily_mean[which(GW_orph$num_well_1km>0)]))

plot(log(GW_orph$num_well_3km[which(GW_orph$num_well_3km>0)]), log(GW_orph$daily_mean[which(GW_orph$num_well_3km>0)]))
cor(log(GW_orph$num_well_3km[which(GW_orph$num_well_3km>0)]), log(GW_orph$daily_mean[which(GW_orph$num_well_3km>0)]))




########### SAVE ALL RESULTS
#Combine all 6 into 1 and save
Geospatial_result_CO_Sr = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_CO_Sr,file = "Geospatial_results_CO_Sr.rds", compress = FALSE)
saveRDS(Geospatial_result_CO_Sr,file = "Geospatial_results_CO_Sr_all.rds", compress = FALSE)



