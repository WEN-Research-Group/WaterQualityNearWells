#Need to get plots Texas, each well type, for groundwater and surface water, for specific conductance
#Analyze only distance to the nearest well


### WELL INFO ####

#Marginal wells 
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

#Surface water Sodium
#### SODIUM - SW ####
TX_SW_Na = read.csv("./input/Texas_surface_WQ/Sodium/TX_SW_Na.csv", header = TRUE)

#Analyze units
unique(TX_SW_Na$Parameter.Description)
#No abnormalities

#Change units if needed - no need for SC
TX_SW_Na$Value = TX_SW_Na$Value *1000

#Use function to do further cleaning and USGS station name change
source("./input/Texas_surface_WQ/Cleaning_TX_SW_fun_daily.R")

#Lat/long data and other site info
TX_SW_sites = read.csv("./input/Texas_surface_WQ/SWQM_Stations.csv", header = TRUE)

#data_daily_clean_short = Cleaning_TX_SW_fun_daily(TX_SW_Na,TX_SW_sites)
Texas_state_Na = Cleaning_TX_SW_fun_daily(TX_SW_Na,TX_SW_sites)
Texas_state_Na_non_USGS = filter(Texas_state_Na,USGS_GAUGE == "")



#Water Quality Portal
#All data - need to clean it still
SW_WQP_Na = readRDS("./input/sw_results_all_info_sodium.rds")

#Look up these to include in the filter
unique(SW_WQP_Na$ResultDetectionConditionText)
unique(SW_WQP_Na$MonitoringLocationTypeName)
unique(SW_WQP_Na$ResultStatusIdentifier)
unique(SW_WQP_Na$ResultMeasure.MeasureUnitCode)

#Cleaning Na data for all the states
SW_WQP_Na_clean = SW_WQP_Na %>%
  rename(Latitude = LatitudeMeasure, Longitude = LongitudeMeasure, DataValue = ResultMeasureValue,
         Samplingdate = ActivityStartDate, State = StateName, County = COUNTY_NAME,
         SiteCode = MonitoringLocationIdentifier) %>%
  mutate(DataValue = as.numeric(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude)) %>%
  mutate(Samplingdate = as.POSIXct(Samplingdate,format="%Y-%M-%d")) %>%
  filter(!ResultStatusIdentifier=="Rejected",
         MonitoringLocationTypeName %in% c("River/Stream","Stream","Stream: Canal"),
         !is.na(DataValue),
         DataValue > 0,
         !is.na(Latitude),
         !is.na(Longitude),
         !is.na(Samplingdate),
         !ResultDetectionConditionText %in% c("*Non-detect","*Present <QL","Not Detected","Not Reported",
                                              "Present Below Quantification Limit","Not Detected at Detection Limit",
                                              "Below Reporting Limit","Detected Not Quantified"),
         ResultMeasure.MeasureUnitCode %in% c("mg/l","ug/l","mg/L","ppm","ug/L"))


  
unique(SW_WQP_Na_clean$ResultDetectionConditionText)
unique(SW_WQP_Na_clean$MonitoringLocationTypeName)
unique(SW_WQP_Na_clean$ResultStatusIdentifier)
unique(SW_WQP_Na_clean$ResultMeasure.MeasureUnitCode)

SW_WQP_Na_clean_backup = SW_WQP_Na_clean

SW_WQP_Na_clean = data.frame(SW_WQP_Na_clean_backup)

#Do unit conversions 
ind = which(SW_WQP_Na_clean$ResultMeasure.MeasureUnitCode == "mg/L"|
       SW_WQP_Na_clean$ResultMeasure.MeasureUnitCode == "mg/l"|
       SW_WQP_Na_clean$ResultMeasure.MeasureUnitCode == "ppm")

SW_WQP_Na_clean[ind,"DataValue"] = SW_WQP_Na_clean[ind,"DataValue"]*1000
#All values are converted and now ug/L
SW_WQP_Na_clean$ResultMeasure.MeasureUnitCode = "ug/L"

#Save this to use for future use
#For each state, separate samples from that state and double check that there are no outliers
#As well as calculate daily mean values

#Save file
saveRDS(SW_WQP_Na_clean, file="sw_wqp_wq_all_curated_Na.rds", compress = FALSE)

#Separate Texas samples
SW_WQP_Na_Texas = filter(SW_WQP_Na_clean, State=="Texas")

#Calculate daily values
SW_WQP_Na_Texas_daily = SW_WQP_Na_Texas %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)


#Merge data from WQP and Texas state database

#Change data types as needed
Texas_state_Na_non_USGS$SiteCode = as.character(Texas_state_Na_non_USGS$SiteCode)
#Remove some columns 
Texas_state_Na_non_USGS = Texas_state_Na_non_USGS[,-c(3,6)]
  
#Now merge
SW_Na_Texas_merged = rbind(Texas_state_Na_non_USGS,SW_WQP_Na_Texas_daily)
#Very few samples that come from non-USGS stations

Q_75 = quantile(SW_Na_Texas_merged$daily_mean, probs = c(0.75)) # that would be 32,000

dim(SW_Na_Texas_merged)
Q_75

#Save this for later
saveRDS(SW_Na_Texas_merged,file = "SW_daily_Na_Texas_merged.rds", compress = FALSE)



# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#MARGINAL
sw.wq.distance = Well_spatial_analysis_compl_0127(Texas_marginal_short,SW_Na_Texas_merged)

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "Na"
meas = "ug/L" 
state = "Texas" 
type_w = "SW"
well_type = "marginal"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL
#source("Well_spatial_analysis_compl.R")

sw.wq.distance = Well_spatial_analysis_compl_0127(enverus_og_well_curated,SW_Na_Texas_merged)

well_type = "unconventional"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)

### ORPHANED

sw.wq.distance = Well_spatial_analysis_compl_0127(Texas_orphaned_clean,SW_Na_Texas_merged)

well_type = "orphaned"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


########################
#GROUNDWATER

#Ground water SC
#### TWDB Groundwater Quality SC Data ####
# read curated file (curated in Ba and Sr source codes)
wqdetail <-  readRDS(file="./input/TWDB/gw_twdb_curated_v1.rds")
lu_param <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/LU_WaterQualityParameters.txt", sep="|", dec=".", header = TRUE, fill = TRUE)

#extract analyte data from the master table by code
gw.twdb.na_long <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(73, 74)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  #remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  #  filter(!Description == "not detected" | is.na(Description)) %>%
  # drop columns not needed
  #  select(-c("WaterQualityDetailId", "WaterQualityHeaderId", 
  #            "WaterQualityParameterId", "ParameterValueFlagId",
  #            "Description", "ParameterUnitOfMeasure",
  #            "ParameterLongDescription")) %>%
  #calculate daily average
  #  group_by(StateWellId, CoordDDLat, CoordDDLong, 
  #           CountyNameMixed, State, SampleDate) %>%
  #  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate, DataValue = ParameterValue)
View(gw.twdb.na_long)

gw.twdb.na_long_clean = gw.twdb.na_long %>%
  filter(is.na(Description) & DataValue > 1) %>%
  arrange(DataValue)
#All values looked reasonable 

#Check units
unique(gw.twdb.na_long_clean$ParameterLongDescription)

#Change units
gw.twdb.na_long_clean$DataValue = gw.twdb.na_long_clean$DataValue * 1000

#Change date type - already done
# Reorder columns and drop some
gw_twdb_na_long_clean = gw.twdb.na_long_clean %>%
  select(c(County, SiteCode, Latitude, Longitude, Samplingdate, DataValue))

#Calculate daily averages 
data_all_short_daily_summary = gw_twdb_na_long_clean %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Probably will have to change site number type to be as character
data_all_short_daily_summary$SiteCode = as.character(data_all_short_daily_summary$SiteCode)
TX_GW_TWDB_Na_part1 = data_all_short_daily_summary



#### Water quality portal
#Need to clean GW Na dataset
#Upload downloaded results instead
gw.all.results = readRDS("./input/water_chemistry/gw_results_na_cl_us.rds")
gw.all.sites = readRDS("./input/water_chemistry/gw_sites_na_cl_us.rds")

#Clean the whole dataset, them separate it into Na and Cl datasets
gw.wqp_data <- merge(x = gw.all.results, y = gw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

# Look up these to include in the filter
#unique(gw.wqp_data$ResultDetectionConditionText) #not working
#table(gw.wqp_data$ResultDetectionConditionText)
unique(gw.wqp_data$MonitoringLocationTypeName)
unique(gw.wqp_data$ResultStatusIdentifier)
unique(gw.wqp_data$ResultMeasure.MeasureUnitCode)

gw.wqp_data_backup = gw.wqp_data
gw.wqp_data$ResultDetectionConditionText = as.character(gw.wqp_data$ResultDetectionConditionText)
unique(gw.wqp_data$ResultDetectionConditionText)

#Cleaning Na data for all the states
GW_WQP_Na_Cl_clean = gw.wqp_data %>%
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



unique(GW_WQP_Na_Cl_clean$ResultDetectionConditionText)
unique(GW_WQP_Na_Cl_clean$MonitoringLocationTypeName)
unique(GW_WQP_Na_Cl_clean$ResultStatusIdentifier)
unique(GW_WQP_Na_Cl_clean$ResultMeasure.MeasureUnitCode)

GW_WQP_Na_Cl_clean_backup = GW_WQP_Na_Cl_clean

#SW_WQP_Na_clean = data.frame(SW_WQP_Na_clean_backup)

#Do unit conversions 
ind = which(GW_WQP_Na_Cl_clean$ResultMeasure.MeasureUnitCode == "mg/L"|
              GW_WQP_Na_Cl_clean$ResultMeasure.MeasureUnitCode == "mg/l"|
              GW_WQP_Na_Cl_clean$ResultMeasure.MeasureUnitCode == "ppm")

GW_WQP_Na_Cl_clean[ind,"DataValue"] = GW_WQP_Na_Cl_clean[ind,"DataValue"]*1000
#All values are converted and now ug/L
GW_WQP_Na_Cl_clean$ResultMeasure.MeasureUnitCode = "ug/L"

GW_WQP_Na_Cl_clean = GW_WQP_Na_Cl_clean %>%
  mutate(Samplingdate = floor_date(Samplingdate, "day"))

#Separate into CL nd NA dataset and save this for future use
#For each state, I will have to separate samples from that state and double check that there are no outliers
#As well as calculate daily mean values

unique(GW_WQP_Na_Cl_clean$CharacteristicName)
GW_WQP_Na_clean = filter(GW_WQP_Na_Cl_clean, CharacteristicName == "Sodium")
GW_WQP_Cl_clean = filter(GW_WQP_Na_Cl_clean, CharacteristicName == "Chloride")

#Save files
saveRDS(GW_WQP_Na_clean, file="gw_wqp_wq_all_curated_Na.rds", compress = FALSE)
saveRDS(GW_WQP_Cl_clean, file="gw_wqp_wq_all_curated_Cl.rds", compress = FALSE)
  
# Part 1 - TX_GW_TWDB_Na_part1
#Part 2
GW_WQP_Na_clean_TX = filter(GW_WQP_Na_clean, State == "Texas")
  
GW_WQP_Na_clean_TX_daily = GW_WQP_Na_clean_TX %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)


#Now merge two datasets
GW_Na_Texas_merged = rbind(TX_GW_TWDB_Na_part1,GW_WQP_Na_clean_TX_daily)

Q_75 = quantile(GW_Na_Texas_merged$daily_mean, probs = c(0.75)) # that would be 32,000

dim(GW_Na_Texas_merged)
Q_75

#Save this for later
saveRDS(GW_Na_Texas_merged,file = "GW_all_Na_Texas_merged.rds", compress = FALSE)


#NOW ANALYZE
#GROUNDWATER

#MARGINAL
gw.wq.distance = Well_spatial_analysis_compl_0127(Texas_marginal_short,GW_Na_Texas_merged)

type_w = "GW"
well_type = "marginal"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(gw.wq.distance,file = "./output/AGU/Texas/Texas_GW_Cl_marginal.rds", compress = FALSE)
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL
#source("Well_spatial_analysis_compl.R")

gw.wq.distance = Well_spatial_analysis_compl_0127(enverus_og_well_curated,GW_Na_Texas_merged)

well_type = "unconventional"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(gw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_unconv.rds", compress = FALSE)
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

gw.wq.distance = Well_spatial_analysis_compl_0127(Texas_orphaned_clean,GW_Na_Texas_merged)

well_type = "orphaned"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
#saveRDS(gw.wq.distance,file = "./output/AGU/Texas/Texas_SW_Cl_orphaned.rds", compress = FALSE)
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


###MARGINAL
#Define
well.all.coords = marg.all.coords
Well_data = Texas_marginal_short
water_data = GW_Na_Texas_merged

sw.nearest.well.all = get.knnx(coordinates(well.all.coords), coordinates(sw.all.coords),k=1000)

# Relationship between distance / density and water chemistry for groundwater samples
# distance vs. water chemistry
sw.nearest.well.all.distance <- as.data.frame(sw.nearest.well.all$nn.dist)
sw.nearest.well.all.index <- as.data.frame(sw.nearest.well.all$nn.index)

sw.nearest.well.all.distance$index <- rownames(sw.nearest.well.all.distance)
sw.nearest.well.all.index$index <- rownames(sw.nearest.well.all.index)
sw.wq.sites$index <- rownames(sw.wq.sites)

#sw.wq.distance <- merge(SW_SC_Texas_merged, sw.wq.sites[,c(2,3,8)],
sw.wq.distance <- merge(water_data, sw.wq.sites[,c("Latitude","Longitude","index")],                        
                        by=c("Latitude","Longitude"),
                        all.x=TRUE)

sw.nearest.well.all.date <- data.frame(matrix(0,nrow = dim(sw.nearest.well.all.index)[1],ncol = dim(sw.nearest.well.all.index)[2]))
for (i in 1:(ncol(sw.nearest.well.all.index)-1)) {
  sw.nearest.well.all.date[,i] <- Well_data[sw.nearest.well.all.index[,i],"SpudDate"]
}

sw.nearest.well.all.date$index <- sw.nearest.well.all.distance$index

sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.date,
                        by="index", all.x=TRUE)
sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.distance,
                        by="index", all.x=TRUE)

a1=which(colnames(sw.wq.distance)=="X1")
a2=which(colnames(sw.wq.distance)=="X1000")
sw.nearest.well.all.boolean <- as.data.frame(sw.wq.distance[,rep("Samplingdate",1000)] > sw.wq.distance[,a1:a2])

sw.nearest.well.all.boolean$nearest_well_index<-as.data.frame(apply(sw.nearest.well.all.boolean, 1, which.max))

# extract the distance to the nearest oil and gas well that is drilled prior to gw sampling
for (i in 1:nrow(sw.wq.distance)) {
  sw.wq.distance$nearest_distance_m[i] = 
    sw.wq.distance[i,as.integer(sw.nearest.well.all.boolean[i, "nearest_well_index"])+a2]
}

# remove rows with nearest_distance_m >10000 m
sw.wq.distance_10km <- sw.wq.distance %>%
  filter(!nearest_distance_m > 10000)

#plot distance vs. mean concentration
plot_gw_dist_nearest_ow_all <- ggscatter(data=sw.wq.distance_10km,
                                         x="nearest_distance_m",
                                         y="daily_mean",
                                         add = "reg.line",
                                         add.params = list(color="blue",fill="lightgray"),
                                         conf.int = TRUE) +
  xscale("log10", .format = TRUE) +
  yscale("log10", .format = TRUE) +
  stat_cor(method = "pearson",label.y = 6, p.accuracy = 0.001,r.accuracy = 0.01) +
  xlab("Log[Distance to the Nearest Well (m)]") +
  ylab("Log[Daily-aggregated Mean SC (uS/cm)]") +
  #  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10') +
  ggtitle("TX Ground Water Samples") +
  theme_bw() +  theme(text = element_text(size = 16))
plot_gw_dist_nearest_ow_all
#ggsave("./output/figures/co/sc_gw_cogcc_wqp_cogcc_dist_nearest_uncon_ow.png",
#       plot = plot_gw_dist_nearest_ow_all,
#       width = 8, height = 6, units = "in")

saveRDS(sw.wq.distance,file = "./output/Texas_GW_SC_nearest_well_marginal.rds", compress = FALSE)



#UNCONVENTIONAL
#Define
well.all.coords = unconv.all.coords
Well_data = enverus_og_well_curated

sw.nearest.well.all = get.knnx(coordinates(well.all.coords), coordinates(sw.all.coords),k=1000)

# Relationship between distance / density and water chemistry for groundwater samples
# distance vs. water chemistry
sw.nearest.well.all.distance <- as.data.frame(sw.nearest.well.all$nn.dist)
sw.nearest.well.all.index <- as.data.frame(sw.nearest.well.all$nn.index)

sw.nearest.well.all.distance$index <- rownames(sw.nearest.well.all.distance)
sw.nearest.well.all.index$index <- rownames(sw.nearest.well.all.index)
sw.wq.sites$index <- rownames(sw.wq.sites)

#sw.wq.distance <- merge(SW_SC_Texas_merged, sw.wq.sites[,c(2,3,8)],
sw.wq.distance <- merge(water_data, sw.wq.sites[,c("Latitude","Longitude","index")],                        
                        by=c("Latitude","Longitude"),
                        all.x=TRUE)

sw.nearest.well.all.date <- data.frame(matrix(0,nrow = dim(sw.nearest.well.all.index)[1],ncol = dim(sw.nearest.well.all.index)[2]))
for (i in 1:(ncol(sw.nearest.well.all.index)-1)) {
  sw.nearest.well.all.date[,i] <- Well_data[sw.nearest.well.all.index[,i],"SpudDate"]
}

sw.nearest.well.all.date$index <- sw.nearest.well.all.distance$index

sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.date,
                        by="index", all.x=TRUE)
sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.distance,
                        by="index", all.x=TRUE)

a1=which(colnames(sw.wq.distance)=="X1")
a2=which(colnames(sw.wq.distance)=="X1000")
sw.nearest.well.all.boolean <- as.data.frame(sw.wq.distance[,rep("Samplingdate",1000)] > sw.wq.distance[,a1:a2])

sw.nearest.well.all.boolean$nearest_well_index<-as.data.frame(apply(sw.nearest.well.all.boolean, 1, which.max))

# extract the distance to the nearest oil and gas well that is drilled prior to gw sampling
for (i in 1:nrow(sw.wq.distance)) {
  sw.wq.distance$nearest_distance_m[i] = 
    sw.wq.distance[i,as.integer(sw.nearest.well.all.boolean[i, "nearest_well_index"])+a2]
}

# remove rows with nearest_distance_m >10000 m
sw.wq.distance_10km <- sw.wq.distance %>%
  filter(!nearest_distance_m > 10000)

#plot distance vs. mean concentration
plot_gw_dist_nearest_ow_all <- ggscatter(data=sw.wq.distance_10km,
                                         x="nearest_distance_m",
                                         y="daily_mean",
                                         add = "reg.line",
                                         add.params = list(color="blue",fill="lightgray"),
                                         conf.int = TRUE) +
  xscale("log10", .format = TRUE) +
  yscale("log10", .format = TRUE) +
  stat_cor(method = "pearson",label.y = 6, p.accuracy = 0.001,r.accuracy = 0.01) +
  xlab("Log[Distance to the Nearest Well (m)]") +
  ylab("Log[Daily-aggregated Mean SC (uS/cm)]") +
  #  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10') +
  ggtitle("TX Ground Water Samples") +
  theme_bw() +  theme(text = element_text(size = 16))
plot_gw_dist_nearest_ow_all
#ggsave("./output/figures/co/sc_gw_cogcc_wqp_cogcc_dist_nearest_uncon_ow.png",
#       plot = plot_gw_dist_nearest_ow_all,
#       width = 8, height = 6, units = "in")

saveRDS(sw.wq.distance,file = "./output/Texas_GW_SC_nearest_well_unconv.rds", compress = FALSE)


#ORHANED
#Define
well.all.coords = orph.all.coords
Well_data = Texas_orphaned_clean

sw.nearest.well.all = get.knnx(coordinates(well.all.coords), coordinates(sw.all.coords),k=1000)

# Relationship between distance / density and water chemistry for groundwater samples
# distance vs. water chemistry
sw.nearest.well.all.distance <- as.data.frame(sw.nearest.well.all$nn.dist)
sw.nearest.well.all.index <- as.data.frame(sw.nearest.well.all$nn.index)

sw.nearest.well.all.distance$index <- rownames(sw.nearest.well.all.distance)
sw.nearest.well.all.index$index <- rownames(sw.nearest.well.all.index)
sw.wq.sites$index <- rownames(sw.wq.sites)

#sw.wq.distance <- merge(SW_SC_Texas_merged, sw.wq.sites[,c(2,3,8)],
sw.wq.distance <- merge(water_data, sw.wq.sites[,c("Latitude","Longitude","index")],                        
                        by=c("Latitude","Longitude"),
                        all.x=TRUE)

sw.nearest.well.all.date <- data.frame(matrix(0,nrow = dim(sw.nearest.well.all.index)[1],ncol = dim(sw.nearest.well.all.index)[2]))
for (i in 1:(ncol(sw.nearest.well.all.index)-1)) {
  sw.nearest.well.all.date[,i] <- Well_data[sw.nearest.well.all.index[,i],"SpudDate"]
}

sw.nearest.well.all.date$index <- sw.nearest.well.all.distance$index

sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.date,
                        by="index", all.x=TRUE)
sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.distance,
                        by="index", all.x=TRUE)

a1=which(colnames(sw.wq.distance)=="X1")
a2=which(colnames(sw.wq.distance)=="X1000")
sw.nearest.well.all.boolean <- as.data.frame(sw.wq.distance[,rep("Samplingdate",1000)] > sw.wq.distance[,a1:a2])

sw.nearest.well.all.boolean$nearest_well_index<-as.data.frame(apply(sw.nearest.well.all.boolean, 1, which.max))

# extract the distance to the nearest oil and gas well that is drilled prior to gw sampling
for (i in 1:nrow(sw.wq.distance)) {
  sw.wq.distance$nearest_distance_m[i] = 
    sw.wq.distance[i,as.integer(sw.nearest.well.all.boolean[i, "nearest_well_index"])+a2]
}

# remove rows with nearest_distance_m >10000 m
sw.wq.distance_10km <- sw.wq.distance %>%
  filter(!nearest_distance_m > 10000)

#plot distance vs. mean concentration
plot_gw_dist_nearest_ow_all <- ggscatter(data=sw.wq.distance_10km,
                                         x="nearest_distance_m",
                                         y="daily_mean",
                                         add = "reg.line",
                                         add.params = list(color="blue",fill="lightgray"),
                                         conf.int = TRUE) +
  xscale("log10", .format = TRUE) +
  yscale("log10", .format = TRUE) +
  stat_cor(method = "pearson",label.y = 6, p.accuracy = 0.001,r.accuracy = 0.01) +
  xlab("Log[Distance to the Nearest Well (m)]") +
  ylab("Log[Daily-aggregated Mean SC (uS/cm)]") +
  #  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10') +
  ggtitle("TX Ground Water Samples") +
  theme_bw() +  theme(text = element_text(size = 16))
plot_gw_dist_nearest_ow_all
#ggsave("./output/figures/co/sc_gw_cogcc_wqp_cogcc_dist_nearest_uncon_ow.png",
#       plot = plot_gw_dist_nearest_ow_all,
#       width = 8, height = 6, units = "in")

saveRDS(sw.wq.distance,file = "./output/Texas_GW_SC_nearest_well_orph.rds", compress = FALSE)





#### GEOSPATIAL RESULTS ####
############################

#Compile all values into one file
#define these for every distance calculation
anal = "Na" #already should be defined above
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


### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
cor(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))

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



########### SAVE ALL RESULTS
#Combine all 6 into 1 and save
Geospatial_result_TX_Na = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_TX_Na,file = "Geospatial_results_TX_Na.rds", compress = FALSE)
