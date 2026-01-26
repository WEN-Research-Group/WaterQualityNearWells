#### Coding Environment Set-up ####
library(dataRetrieval)
library(data.table)
library(reshape2)
library(leaflet)
library(DT)
library(usmap)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(sp)
#library(rgdal)
library(FNN)
library(htmlwidgets)
library(ggpubr)
library(rstudioapi)
library(janitor)

#Need to get plots Texas, each well type, for groundwater and surface water, for specific conductance
#Analyze only distance to the nearest well

#Marginal wells 
Texas_marginal = readRDS("./input/Texas_marginal_wells_curated.rds")

Texas_marginal_short = Texas_marginal %>%
  filter(!is.na(SpudDate)) %>%
  mutate(SpudDate = 
           floor_date(as.POSIXct(SpudDate, format="%Y-%m-%d"), "day")) %>%
  select(c(1:6,"County","Latitude","Longitude","SpudDate"))

#Unconventional wells
#In OG directore
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

#Surface water SC
#### SC - SW ####
###Texas surface water - from state level database
TX_SW_SC_1 = read.csv("./input/Texas_surface_WQ/SC/TX_SW_SC_1.csv", header = TRUE)
TX_SW_SC_2 = read.csv("./input/Texas_surface_WQ/SC/TX_SW_SC_2.csv", header = TRUE)
TX_SW_SC_3 = read.csv("./input/Texas_surface_WQ/SC/TX_SW_SC_3.csv", header = TRUE)

#Combine all 3 files into one - remove empty columns that got added with csv
TX_SW_SC = rbind(TX_SW_SC_1, TX_SW_SC_2[,-21], TX_SW_SC_3[,-c(21,22)])

#Analyze units
unique(TX_SW_SC$Parameter.Description)
#Three samples are missing first few column, remove them
TX_SW_SC = filter(TX_SW_SC, Parameter.Description == "SPECIFIC CONDUCTANCE, US/CM, FIELD, 24HR AVG" | 
                    Parameter.Description == "SPECIFIC CONDUCTANCE,FIELD (US/CM @ 25C)")

#Change units if needed - no need for SC
#TX_SW_Sulfate$Value = TX_SW_Sulfate$Value *1000

#Use function to do further cleaning and USGS station name change
source("./input/Texas_surface_WQ/Cleaning_TX_SW_fun_daily.R")

#Lat/long data and other site info
TX_SW_sites = read.csv("./input/Texas_surface_WQ/SWQM_Stations.csv", header = TRUE)

Texas_state_SC = Cleaning_TX_SW_fun_daily(TX_SW_SC,TX_SW_sites)
Texas_state_SC_non_USGS = filter(Texas_state_SC,USGS_GAUGE == "")

#Water Quality Portal
SW_WQP_SC = readRDS("./input/SC_sw_wqp_daily_samples_summary.rds")
SW_WQP_SC_Texas = filter(SW_WQP_SC,State=="Texas" & daily_sc >1) 

#Need to merge two datasets
#Need to remove some columns before merging
SW_WQP_SC_Texas_part1 = SW_WQP_SC_Texas[,-1]
Texas_state_SC_non_USGS_part2 = Texas_state_SC_non_USGS[,-c(3,6)]
#Rename some columns
names(SW_WQP_SC_Texas_part1)[names(SW_WQP_SC_Texas_part1) == 'daily_sc'] <- 'daily_mean'
names(SW_WQP_SC_Texas_part1)[names(SW_WQP_SC_Texas_part1) == 'min_sc'] <- 'min_v'
names(SW_WQP_SC_Texas_part1)[names(SW_WQP_SC_Texas_part1) == 'max_sc'] <- 'max_v'

#Change data types as needed
Texas_state_SC_non_USGS_part2$SiteCode = as.character(Texas_state_SC_non_USGS_part2$SiteCode)

#Now merge
SW_SC_Texas_merged = rbind(SW_WQP_SC_Texas_part1,Texas_state_SC_non_USGS_part2)

Q_75 = quantile(SW_SC_Texas_merged$daily_mean, probs = c(0.75)) # that would be 32,000

dim(SW_SC_Texas_merged)
Q_75

#Save this for later
saveRDS(SW_SC_Texas_merged,file = "SW_all_SC_Texas_merged.rds", compress = FALSE)

#Or read in data
SW_SC_Texas_merged = readRDS("SW_all_SC_Texas_merged.rds")


#NOW ANALYZE
#Surface water

#Use function
source("Well_spatial_analysis_comp_0127.R")
#Make sure spud name is "SpudDate"

#MARGINAL
sw.wq.distance = Well_spatial_analysis_compl_0127(Texas_marginal_short,SW_SC_Texas_merged)

source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "SC"
meas = "uS/cm" 
state = "Texas" 
type_w = "SW"
well_type = "marginal"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL
#source("Well_spatial_analysis_compl.R")

sw.wq.distance = Well_spatial_analysis_compl_0127(enverus_og_well_curated,SW_SC_Texas_merged)

well_type = "unconventional"
Saving_figures_fun_exp(sw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(sw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

sw.wq.distance = Well_spatial_analysis_compl_0127(Texas_orphaned_clean,SW_SC_Texas_merged)

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

#extract Ba and Sr data from the master table
# SC parameters codes are 94 and 95. All values are in UMHOS/CM = uS/cm
gw.twdb.sc_long <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(4, 5)) %>%
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
View(gw.twdb.sc_long)


gw.twdb.sc_long_clean = gw.twdb.sc_long %>%
  filter(is.na(Description) & DataValue > 1) %>%
  arrange(DataValue)
#All values looked reasonable 

#Change date type - already done
# Reorder columns and drop some
gw_twdb_sc_long_clean = gw.twdb.sc_long_clean %>%
  select(c(County, SiteCode, Latitude, Longitude, Samplingdate, DataValue))

#Calculate daily averages 
data_all_short_daily_summary = gw_twdb_sc_long_clean %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Probably will have to change site number type to be as character
data_all_short_daily_summary$SiteCode = as.character(data_all_short_daily_summary$SiteCode)
TX_GW_TWDB_SC_part1 = data_all_short_daily_summary

#Cleaned WQP data
TX_GW_SC = readRDS(file="gw.wqp_data_TX_daily_categ_ord_corr_SC.rds")
TX_GW_WQP_SC_part2 = TX_GW_SC[,!names(TX_GW_SC) %in% c("State","MonitoringLocationTypeName","Category")] 

names(TX_GW_WQP_SC_part2)[names(TX_GW_WQP_SC_part2) == 'mean_sc'] <- 'daily_mean'
names(TX_GW_WQP_SC_part2)[names(TX_GW_WQP_SC_part2) == 'min_sc'] <- 'min_v'
names(TX_GW_WQP_SC_part2)[names(TX_GW_WQP_SC_part2) == 'max_sc'] <- 'max_v'

TX_GW_WQP_SC_part2$Latitude = as.numeric(TX_GW_WQP_SC_part2$Latitude)
TX_GW_WQP_SC_part2$Longitude = as.numeric(TX_GW_WQP_SC_part2$Longitude)

#Now merge two datasets
GW_SC_Texas_merged = rbind(TX_GW_WQP_SC_part2,TX_GW_TWDB_SC_part1)

Q_75 = quantile(GW_SC_Texas_merged$daily_mean, probs = c(0.75)) # that would be 32,000

dim(GW_SC_Texas_merged)
Q_75

#Save this for later
saveRDS(GW_SC_Texas_merged,file = "GW_all_SC_Texas_merged.rds", compress = FALSE)

#Or load
GW_SC_Texas_merged = readRDS(file="GW_all_SC_Texas_merged.rds")


#NOW ANALYZE
#GROUNDWATER

## Using function

#MARGINAL
gw.wq.distance = Well_spatial_analysis_compl_0127(Texas_marginal_short,GW_SC_Texas_merged)

type_w = "GW"
well_type = "marginal"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### UNCONVENTIONAL
#source("Well_spatial_analysis_compl.R")

gw.wq.distance = Well_spatial_analysis_compl_0127(enverus_og_well_curated,GW_SC_Texas_merged)

well_type = "unconventional"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


### ORPHANED

gw.wq.distance = Well_spatial_analysis_compl_0127(Texas_orphaned_clean,GW_SC_Texas_merged)

well_type = "orphaned"
Saving_figures_fun_exp(gw.wq.distance,anal, meas, state, type_w, well_type)

#Save analysis results
saveRDS(gw.wq.distance,file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""), compress = FALSE)


#EXTRA
# Step by Step

# extract unique lat/long combinations
#Fow convenience sake, I will keep sw in the name
sw.wq.sites <- GW_SC_Texas_merged[!duplicated(GW_SC_Texas_merged[c("Longitude","Latitude")]),]

# convert to spatial dataset
sw.all.coords <- SpatialPoints(sw.wq.sites[,c("Longitude","Latitude")])

crs.geo<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(sw.all.coords) <-crs.geo

# project all spatial layers to NAD83 for distance calculation
sw.all.coords <- spTransform(sw.all.coords, CRS("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))



###MARGINAL
#Define
well.all.coords = marg.all.coords
Well_data = Texas_marginal_short

sw.nearest.well.all = get.knnx(coordinates(well.all.coords), coordinates(sw.all.coords),k=1000)

# Relationship between distance / density and water chemistry for groundwater samples
# distance vs. water chemistry
sw.nearest.well.all.distance <- as.data.frame(sw.nearest.well.all$nn.dist)
sw.nearest.well.all.index <- as.data.frame(sw.nearest.well.all$nn.index)

sw.nearest.well.all.distance$index <- rownames(sw.nearest.well.all.distance)
sw.nearest.well.all.index$index <- rownames(sw.nearest.well.all.index)
sw.wq.sites$index <- rownames(sw.wq.sites)

#sw.wq.distance <- merge(SW_SC_Texas_merged, sw.wq.sites[,c(2,3,8)],
sw.wq.distance <- merge(GW_SC_Texas_merged, sw.wq.sites[,c("Latitude","Longitude","index")],                        
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
sw.wq.distance <- merge(GW_SC_Texas_merged, sw.wq.sites[,c("Latitude","Longitude","index")],                        
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
sw.wq.distance <- merge(GW_SC_Texas_merged, sw.wq.sites[,c("Latitude","Longitude","index")],                        
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
anal = "SC" #already should be defined above
meas = "uS/cm" 
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
Geospatial_result_TX_SC = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_TX_SC,file = "Geospatial_results_TX_SC.rds", compress = FALSE)
