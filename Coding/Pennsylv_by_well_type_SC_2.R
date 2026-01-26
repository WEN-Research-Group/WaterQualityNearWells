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

### SURFACE - only data from surface water quality portal

#Water Quality Portal
SW_WQP_SC = readRDS("./input/SC_sw_wqp_daily_samples_summary.rds")

#Rename some columns
names(SW_WQP_SC)[names(SW_WQP_SC) == 'daily_sc'] <- 'daily_mean'
names(SW_WQP_SC)[names(SW_WQP_SC) == 'min_sc'] <- 'min_v'
names(SW_WQP_SC)[names(SW_WQP_SC) == 'max_sc'] <- 'max_v'

# Get the state samples 
SW_WQP_SC_PA = filter(SW_WQP_SC,State=="Pennsylvania" & daily_mean >1) 

#Need to remove some columns 
SW_WQP_SC_PA = SW_WQP_SC_PA[,-1]


#SHALE NETWORK

#### Shale Network SC Data ####
SC_Shale_raw = read.csv(file="./input/NewShaleDownload/SC_0115.csv", na.strings = "", stringsAsFactors = FALSE)

unique(SC_Shale_raw$State)
unique(SC_Shale_raw$UnitsAbbreviation)
unique(SC_Shale_raw$VariableName) 
unique(SC_Shale_raw$SampleMedium)

SC_Shale_curated <- SC_Shale_raw %>%
  filter(SampleMedium %in% c("Surface Water", "Surface water", "Groundwater"),
         !CensorCode == "lt",
         Latitude > 0,
         Longitude < -70,
         State %in% c("Pennsylvania", "PA", "Pennsylvania ","Pa", "PENNSYLVANIA", "Pennsylvannia"),
         DataValue != -9999,
         DataValue > 0) %>%
  filter(!(UnitsAbbreviation %in% c("-"))) %>%
  ## convert ms/cm to uS/cm
  mutate(DataValue = ifelse(UnitsAbbreviation == "mS/cm",
                            DataValue * 1000, DataValue)) %>%
  mutate(UnitsAbbreviation = replace(UnitsAbbreviation,
                                     UnitsAbbreviation == "mS/cm",
                                     "uS/cm")) %>%
  # format sampling date
  mutate(LocalDateTime = as.POSIXct(LocalDateTime,
                                    format="%m/%d/%y")) %>%
  # create a column for sampling DAY
  mutate(Samplingdate = floor_date(LocalDateTime, "day")) %>%
  filter(DataValue > 1) %>%
  arrange(DataValue)

View(SC_Shale_curated)

#Remove some of the categories
unique(SC_Shale_curated$SiteType)
unique(SC_Shale_curated$SampleMedium)


# extract groundwater and surface water samples

sw.sn_sc_curated_all <- SC_Shale_curated %>%
  filter(SampleMedium %in% c("Surface Water","Surface water"),
         SiteType %in% c("Stream")) 
View(sw.sn_sc_curated_all)


#Calculate daily averages and rename columns
SW_Shale_SC_PA_daily = sw.sn_sc_curated_all %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(SW_Shale_SC_PA_daily)

#Need to remove the largest value
SW_Shale_SC_PA = filter(SW_Shale_SC_PA_daily, daily_mean < 2000)

#Merge two datasets together and save
SW_SC_PA_merged = rbind(SW_Shale_SC_PA, SW_WQP_SC_PA)

SW_SC_PA_merged = SW_SC_PA_merged %>%
  arrange(daily_mean)
View(SW_SC_PA_merged)

#Remove outliers
SW_SC_PA_merged = filter(SW_SC_PA_merged, daily_mean < 1000000)

#Save this for later
saveRDS(SW_SC_PA_merged,file = "SW_all_SC_Penn_merged_0115.rds", compress = FALSE)



# Now it's clean
SW_SC_PA_merged_org = SW_SC_PA_merged %>%
  arrange(daily_mean)
View(SW_SC_PA_merged_org)
ind = which(SW_SC_PA_merged_org$Longitude > 0)
SW_SC_PA_merged_org$Longitude[ind] = SW_SC_PA_merged_org$Longitude[ind] *(-1)
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(SW_SC_PA_merged_org$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(SW_SC_PA_merged_org, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(SW_SC_PA_merged_org)
Q_75



### GROUND WATER

# SHALE NETWORK
gw.sn_sc_curated_all <- SC_Shale_curated %>%
  filter(SampleMedium == "Groundwater",
         SiteType %in% c("Well", "Spring"))
#View(gw.sn_sc_curated_all)

#Calculate daily averages and rename columns
GW_Shale_SC_PA_daily = gw.sn_sc_curated_all %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(GW_Shale_SC_PA_daily)

#Need to remove the largest values
GW_Shale_SC_PA = filter(GW_Shale_SC_PA_daily, daily_mean < 10000)


#WATER QUALITY PORTAL
#Cleaned WQP data
PA_GW_SC = readRDS(file="gw.wqp_data_PA_daily_categ_ord_corr_SC.rds")

PA_GW_WQP_SC = PA_GW_SC[,!names(PA_GW_SC) %in% c("State","MonitoringLocationTypeName","Category")] 

names(PA_GW_WQP_SC)[names(PA_GW_WQP_SC) == 'mean_sc'] <- 'daily_mean'
names(PA_GW_WQP_SC)[names(PA_GW_WQP_SC) == 'min_sc'] <- 'min_v'
names(PA_GW_WQP_SC)[names(PA_GW_WQP_SC) == 'max_sc'] <- 'max_v'

PA_GW_WQP_SC$Latitude = as.numeric(PA_GW_WQP_SC$Latitude)
PA_GW_WQP_SC$Longitude = as.numeric(PA_GW_WQP_SC$Longitude)

View(PA_GW_WQP_SC)
PA_GW_WQP_SC = filter(PA_GW_WQP_SC, daily_mean < 15001)


#Merge two datasets together and save
GW_SC_PA_merged = rbind(GW_Shale_SC_PA, PA_GW_WQP_SC)

GW_SC_PA_merged = GW_SC_PA_merged %>%
  arrange(daily_mean)
View(GW_SC_PA_merged)

GW_SC_PA_merged = filter(GW_SC_PA_merged, daily_mean < 15000)

#Save this for later
saveRDS(GW_SC_PA_merged,file = "GW_all_SC_Penn_merged_0115.rds", compress = FALSE)




# Now it's clean
GW_SC_PA_merged_org = GW_SC_PA_merged %>%
  arrange(daily_mean)
View(GW_SC_PA_merged_org)
#ind = which(SW_SC_PA_merged_org$Longitude > 0)
#SW_SC_PA_merged_org$Longitude[ind] = SW_SC_PA_merged_org$Longitude[ind] *(-1)
#Find 75th percentile? Then plot values above that value?
Q_75 = quantile(GW_SC_PA_merged_org$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(GW_SC_PA_merged_org, daily_mean > Q_75)
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

dim(GW_SC_PA_merged_org)
Q_75




#Upload SW and GW data
SW_SC_PA_merged = readRDS("SW_all_SC_Penn_merged_0115.rds")
GW_SC_PA_merged = readRDS("GW_all_SC_Penn_merged_0115.rds")



# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = PA_marginal
unconv_well_data = padep_uncon_og_well_curated
orphan_well_data = PA_orphaned_clean
surface_water_data = SW_SC_PA_merged
groundwater_data = GW_SC_PA_merged

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "SC" #already should be defined above
meas = "uS/cm" 
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
anal = "SC" #already should be defined above
#meas = "ug/L" 
state = "Pennsylvania" 

#SURFACE

#MARGINAL

type_w = "SW"
well_type = "marginal"

#Read analysis results
SW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"
#143347 values 

plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
#Remove values above 1000
#SW_marg = filter(SW_marg, daily_mean < 1000) #31940 values

#Delete high values that are caused likely because of wrong unit specification
#compare min and max values and remove those with difference above 1000

length(which(SW_marg$max_v - SW_marg$min_v > 100))# 
SW_marg = SW_marg[-which(SW_marg$max_v - SW_marg$min_v > 100),] #140257
plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))

#Remove values above 58400 (conc based on site nrs - keep highest values from USGS only)
SW_marg = filter(SW_marg, daily_mean < 50000)
plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))

#Deal with double counted samples from the same day but different site codes and same lat/long
SW_marg_fx = SW_marg %>%
  group_by(Latitude, Longitude, Samplingdate) %>%
  summarise(n = sum(n), daily_mean=mean(daily_mean), daily_median = mean(daily_median), min_v=min(min_v), max_v=max(max_v),
            nearest_distance_m = min(nearest_distance_m),num_well_1km=mean(num_well_1km), num_well_3km=mean(num_well_3km),
            closest_well_dist_sum=mean(closest_well_dist_sum))

# SW_marg_fx_comb = merge(SW_marg_fx, SW_marg[,1:4], by = "Latitude", all.y = FALSE) #wrong
# SW_marg_fx_comb = merge(SW_marg[,1:4],SW_marg_fx, by = "Latitude")

dim(unique(SW_marg[,c("Latitude","Longitude", "Samplingdate")])) #Unique combinations, making sure no duplicates are there
SW_marg_fx_comb <- SW_marg[!duplicated(SW_marg[c("Longitude","Latitude","Samplingdate")]),] #a lot of columns
W_marg_fx_comb_sort = SW_marg_fx_comb %>%
  arrange(Latitude)
SW_marg_fx_comb_sort2 = SW_marg_fx_comb %>%
  arrange(Latitude,Longitude,Samplingdate)

SW_marg_fx_sort = SW_marg_fx %>%
  arrange(Latitude)

length(SW_marg_fx_comb_sort$Latitude == SW_marg_fx$Latitude)

#Add sites nrs into true daily aver dataset
#SW_marg_fx_f = cbind(SW_marg_fx[,1:2],SW_marg_fx_comb_sort[,3:4],SW_marg_fx[,3:12])
SW_marg_fx_f2 = cbind(SW_marg_fx[,1:2],SW_marg_fx_comb_sort2[,3:4],SW_marg_fx[,3:12])

#backup
SW_marg_backup = SW_marg
SW_marg = SW_marg_fx_f2
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"
plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))


### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
#Remove weird values

SW_unconv = SW_unconv[-which(SW_unconv$max_v - SW_unconv$min_v > 100),] 
SW_unconv = filter(SW_unconv, daily_mean < 50000)
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



### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"

#Remove values above 1000
#SW_orph = filter(SW_orph, daily_mean < 1000) #31940 values
plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))


SW_orph = SW_orph[-which(SW_orph$max_v - SW_orph$min_v > 100),] 
SW_orph = filter(SW_orph, daily_mean < 50000)
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

dim(unique(GW_marg[,c("Latitude","Longitude", "Samplingdate")])) #38544 in dataset but 36571 unique combos
GW_marg = GW_marg[-which(GW_marg$max_v - GW_marg$min_v > 1000),] #38517


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


### UNCONVENTIONAL

well_type = "unconventional"

GW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_unconv$type_w = "GW"
GW_unconv$well_type = "unconventional"

plot(log(GW_unconv$nearest_distance_m), log(GW_unconv$daily_mean))

GW_unconv = GW_unconv[-which(GW_unconv$max_v - GW_unconv$min_v > 1000),] #38517


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


### ORPHANED

well_type = "orphaned"

GW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
GW_orph$type_w = "GW"
GW_orph$well_type = "orphaned"

plot(log(GW_orph$nearest_distance_m), log(GW_orph$daily_mean))

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


#Make sure all outliers are removed before combining into 1 dataset


#Combine all 6 into 1 and save
Geospatial_result_PA_SC = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_PA_SC,file = "Geospatial_results_PA_SC.rds", compress = FALSE)


