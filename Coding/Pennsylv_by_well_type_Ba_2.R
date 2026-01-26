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

#Keep only certain columns
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

anal = "Ba"
### SURFACE

#Water Quality Portal
#Raw dataset was cleaned when I analyzed Texas data
#Upload cleaned data and extract NY state
SW_WQP_anal = readRDS(file=paste("sw_wqp_wq_all_curated_",anal,".rds",sep=""))

#Separate Texas samples
SW_WQP_anal_State = filter(SW_WQP_anal, State=="Pennsylvania")
View(SW_WQP_anal_State)

#Calculate daily values
SW_WQP_anal_daily = SW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(SW_WQP_anal_daily)

#Remove a few largest values
SW_WQP_Ba_PA = filter(SW_WQP_anal_daily, daily_mean < 1000 & daily_mean > 1)



#SHALE NETWORK

#### Shale Network Data ####
#New data files
Ba_Shale_raw = read.csv(file="./input/NewShaleDownload/Ba_0115.csv", na.strings = "", stringsAsFactors = FALSE)

unique(Ba_Shale_raw$State)
unique(Ba_Shale_raw$UnitsAbbreviation)
unique(Ba_Shale_raw$VariableName) 
unique(Ba_Shale_raw$SampleMedium)

Ba_Shale_curated <- Ba_Shale_raw %>%
  filter(SampleMedium %in% c("Surface Water", "Surface water", "Groundwater"),
         !CensorCode == "lt",
         Latitude > 0,
         Longitude < -70,
         State %in% c("Pennsylvania", "PA", "Pennsylvania ","Pa", "PENNSYLVANIA"),
         DataValue != -9999,
         DataValue > 0) %>%
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
View(Ba_Shale_curated)


Ba_Shale_curated2 <- Ba_Shale_raw %>%
  filter(SampleMedium %in% c("Surface Water", "Surface water", "Groundwater"),
         !CensorCode == "lt",
         Latitude > 0,
         Longitude < -70,
         State %in% c("Pennsylvania", "PA", "Pennsylvania ","Pa", "PENNSYLVANIA"),
         DataValue != -9999,
         DataValue > 0) %>%
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
  #filter(DataValue > 1) %>%
  arrange(DataValue)
View(Ba_Shale_curated2)

#Remove some of the categories
unique(Ba_Shale_curated$VariableName) 
unique(Ba_Shale_curated$SiteType)
unique(Ba_Shale_curated$SampleMedium)

# Extract groundwater and surface water samples

sw.sn_curated_all <- Ba_Shale_curated %>%
  filter(SampleMedium %in% c("Surface Water","Surface water"),
         SiteType %in% c("Stream")) 
#View(sw.sn_curated_all)

#Calculate daily averages and rename columns
SW_Shale_anal_daily = sw.sn_curated_all %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(SW_Shale_anal_daily)

#Need to remove the largest value
SW_Shale_Ba_PA = filter(SW_Shale_anal_daily, daily_mean < 10000)


#Merge two datasets together and save
SW_Ba_PA_merged = rbind(SW_Shale_Ba_PA, SW_WQP_Ba_PA)

SW_Ba_PA_merged = SW_Ba_PA_merged %>%
  arrange(daily_mean)

SW_Ba_PA_merged = filter(SW_Ba_PA_merged, daily_mean < 5000)

#Save this for later
saveRDS(SW_Ba_PA_merged,file = "SW_all_Ba_Penn_merged_0115.rds", compress = FALSE)



### GROUND WATER

# SHALE NETWORK
gw.sn_curated_all <- Ba_Shale_curated %>%
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
GW_Shale_Ba_PA = filter(GW_Shale_anal_daily, daily_mean < 30000) 


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
GW_WQP_Ba_PA = filter(GW_WQP_anal_daily, daily_mean < 5001 & daily_mean > 1)

#Merge two datasets together and save
GW_Ba_PA_merged = rbind(GW_Shale_Ba_PA, GW_WQP_Ba_PA)

GW_Ba_PA_merged = GW_Ba_PA_merged %>%
  arrange(daily_mean)
View(GW_Ba_PA_merged)

#Filter if needed

#Save this for later
saveRDS(GW_Ba_PA_merged,file = "GW_all_Ba_Penn_merged_0115.rds", compress = FALSE)


#Read in values
SW_Ba_PA_merged = readRDS("SW_all_Ba_Penn_merged_0115.rds")
GW_Ba_PA_merged = readRDS("GW_all_Ba_Penn_merged_0115.rds")



# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = PA_marginal
unconv_well_data = padep_uncon_og_well_curated
orphan_well_data = PA_orphaned_clean
surface_water_data = SW_Ba_PA_merged
groundwater_data = GW_Ba_PA_merged

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "Ba" #already should be defined above
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

#One point is very close to samplong location and affects results, need to remove it
sw.wq.distance_x <- gw.wq.distance %>%
  filter(nearest_distance_m < 10000,
         nearest_distance_m>10)#,
#num_well_1km<40)

plot_gw_well_all <- ggscatter(data=sw.wq.distance_x,
                                          #x="count_1km",
                                          x="nearest_distance_m",
                                          y="daily_mean",
                                          alpha = 0.5,
                                          add = "reg.line",
                                          #add = "loess",
                                          add.params = list(color="blue",fill="lightgray"),
                                          conf.int = TRUE) +
  xscale("log10", .format = TRUE) +
  yscale("log10", .format = TRUE) +
  stat_cor(method = "pearson", p.accuracy = 0.001,r.accuracy = 0.01) +
  xlab("# of Wells within 1KM") +
  #ylab("Log[Daily-aggregated Mean Na (ug/L))]") +
  #ggtitle("TX Ground Water Samples") +
  ylab(paste("Log[Daily-aggregated Mean",anal,meas,"]")) +
  ggtitle( paste(state, type_w, "Water Samples")) +
  theme_bw() +  theme(text = element_text(size = 16))
plot_gw_well_all

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
anal = "Ba" #already should be defined above
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
SW_marg = filter(SW_marg, daily_mean < 1000) #31940 values

### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
#Remove values above 1000
SW_unconv = filter(SW_unconv, daily_mean < 1000) #31940 values

### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"

#Remove values above 1000
SW_orph = filter(SW_orph, daily_mean < 1000) #31940 values
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
Geospatial_result_PA_Ba = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_PA_Ba,file = "Geospatial_results_PA_Ba.rds", compress = FALSE)


##########################################

all_wells_PA_nrs = all_wells_PA %>%
  group_by(W_type) %>%
  summarise(n = n())


