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
anal = "Ba"
### SURFACE - only data from surface water quality portal

#Water Quality Portal
#Raw dataset was cleaned when I analyzed Texas data
#Upload cleaned data and extract NY state
#SW_WQP_SO4 = readRDS("sw_wqp_wq_all_curated_SO4.rds")
SW_WQP_anal = readRDS(file=paste("sw_wqp_wq_all_curated_",anal,".rds",sep=""))

#Separate Texas samples
SW_WQP_anal_State = filter(SW_WQP_anal, State=="New York")

#Calculate daily values
SW_WQP_anal_daily = SW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(SW_WQP_anal_daily)

#Remove a few largest values
SW_WQP_Ba_NY = filter(SW_WQP_anal_daily, daily_mean < 250)



### GROUND WATER

#Cleaned WQP data
GW_WQP_anal = readRDS(file=paste("gw_wqp_wq_all_curated_",anal,".rds",sep=""))

#Separate Texas samples
GW_WQP_anal_State = filter(GW_WQP_anal, State=="New York")

#Calculate daily values
GW_WQP_anal_daily = GW_WQP_anal_State %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Check if there are any outliers
View(GW_WQP_anal_daily)

#Remove largest values, also some values are very small
GW_WQP_Ba_NY = filter(GW_WQP_anal_daily, daily_mean < 11000 & daily_mean > 1)


#Or Upload data
#SW_WQP_Ba_NY = 
#GW_WQP_Ba_NY = 


# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = NY_marginal
#unconv_well_data = enverus_og_well_curated
orphan_well_data = NY_orphaned_clean
surface_water_data = SW_WQP_Ba_NY
groundwater_data = GW_WQP_Ba_NY

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "Ba" #already should be defined above
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


#Open sw.wq.distance file to plot new figures and calculate skewness
Marg_SW = readRDS("./output/AGU/New York/New York_SW_Ba_marginal.rds")
source("Saving_figures_fun_exp_surf_no_limit.R")
sw.wq.distance_no_0_1km = filter(Marg_SW,num_well_1km>0)
plot_gw_1km_density_well_all <- ggscatter(data=sw.wq.distance_no_0_1km,
                                          x="num_well_1km",
                                          #x="num_og_1km",
                                          y="daily_mean",
                                          alpha = 0.5,
                                          add = "reg.line",
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
plot_gw_1km_density_well_all

sw.wq.distance_no_0_3km = filter(Marg_SW,num_well_3km>0)
sw.wq.distance_no_0_3km$num_well_3km = log(sw.wq.distance_no_0_3km$num_well_3km)
plot_gw_3km_density_well_all <- ggscatter(data=sw.wq.distance_no_0_3km,
                                          x="num_well_3km",
                                          #x="num_og_1km",
                                          y="daily_mean",
                                          alpha = 0.5,
                                          add = "reg.line",
                                          add.params = list(color="blue",fill="lightgray"),
                                          conf.int = TRUE) +
  #xscale("log10", .format = FALSE) +
  yscale("log10", .format = FALSE) +
  stat_cor(method = "pearson", p.accuracy = 0.001,r.accuracy = 0.01) +
  xlab("# of Wells within 3KM") +
  #ylab("Log[Daily-aggregated Mean Na (ug/L))]") +
  #ggtitle("TX Ground Water Samples") +
  ylab(paste("Log[Daily-aggregated Mean",anal,meas,"]")) +
  ggtitle( paste(state, type_w, "Water Samples")) +
  theme_bw() +  theme(text = element_text(size = 16))
plot_gw_3km_density_well_all

hist(sw.wq.distance_no_0_3km$num_well_3km)
install.packages(c("e1071"))
library(e1071)
skewness(sw.wq.distance_no_0_3km$num_well_3km)
hist(log(sw.wq.distance_no_0_3km$num_well_3km))
x = sw.wq.distance_no_0_3km$num_well_3km
b <- boxcox(lm(x ~ 1))
# Exact lambda
lambda <- b$x[which.max(b$y)]
lambda
new_x_exact <- (x ^ lambda - 1) / lambda
hist(new_x_exact)


plot_sw_dist_nearest_ow_all <- ggscatter(data=Marg_SW,
                                         x="nearest_distance_m",
                                         y="daily_mean",
                                         alpha = 0.5,
                                         add = "reg.line",
                                         add.params = list(color="blue",fill="lightgray"),
                                         conf.int = TRUE) +
  #xscale("log10", .format = TRUE) +
  yscale("log10", .format = TRUE) +
  stat_cor(method = "pearson", p.accuracy = 0.001,r.accuracy = 0.01) +
  xlab("Log[Distance to the Nearest Well (m)]") +
  #ylab("Log[Daily-aggregated Mean Na (ug/L)]") +
  ylab(paste("Log[Daily-aggregated Mean",anal,meas,"]")) +
  ggtitle( paste(state, type_w, "Water Samples")) +
  theme_bw() +  theme(text = element_text(size = 16))
plot_sw_dist_nearest_ow_all



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





#Retrieve results to get TAO numbers - how many wells have sampling sites next to them?
#Check distances from sampling site to well - count if it's within 1 or 3 km radius
#After identifying those wells, count how many total samples are from there

Marg_GW = readRDS("./output/AGU/New York/New York_GW_Ba_marginal.rds")
length(which(Marg_GW$num_well_1km > 0))
length(which(Marg_GW$num_well_1km > 0))/dim(Marg_GW)[1]*100
length(which(Marg_GW$num_well_3km > 0))
length(which(Marg_GW$num_well_3km > 0))/dim(Marg_GW)[1]*100

#Get SiteCodes for those wells 
Marg_GW$SiteCode[which(Marg_GW$num_well_1km > 0)]
Samples_near_count_w = Marg_GW %>%
     filter(SiteCode %in% Marg_GW$SiteCode[which(Marg_GW$num_well_1km > 0)])
length(unique(Samples_near_count_w$SiteCode))

Samples_near_count = groundwater_data %>%
  filter(SiteCode %in% c(Marg_GW$SiteCode[which(Marg_GW$num_well_1km > 0)]))

Samples_near_count_summ = Samples_near_count %>%
  group_by(SiteCode) %>%
  summarise(n = n()) 

length(unique(Marg_GW$SiteCode)) #only 350 sites have more than 1 sample
length(unique(groundwater_data$SiteCode))

dim(Marg_GW)[1]
summar = Marg_GW %>%
  group_by(SiteCode) %>%
  summarise(n = n()) %>%
  arrange(n)

dim(groundwater_data)
summar_ = groundwater_data %>%
  #group_by(SiteCode) %>%
  #summarise(n = n()) %>%
  #arrange(n) %>%
  filter(SiteCode %in% Marg_GW$SiteCode[which(Marg_GW$num_well_1km > 0)])


summar_y = groundwater_data %>%
  #group_by(SiteCode) %>%
  #summarise(n = n()) %>%
  #arrange(n) %>%
  filter(groundwater_data,!duplicated(groundwater_data[c("Longitude","Latitude")]))

groundwater_data_unique = groundwater_data[!duplicated(groundwater_data[c("Longitude","Latitude")]),]
groundwater_data_unique_site = groundwater_data[!duplicated(groundwater_data$SiteCode),]
groundwater_data_unique2 = groundwater_data %>%
  distinct(Latitude, Longitude, .keep_all = TRUE)
groundwater_data_unique_site2 = groundwater_data %>%
  distinct(SiteCode)


grouped_samp = groundwater_data %>%
  group_by(SiteCode, Latitude, Longitude) %>%
  summarise(n = n())


#Gives different values - why?
diff_samp = groundwater_data_unique_site %>%
  filter(!SiteCode %in% groundwater_data_unique$SiteCode)

diff_samp_sites = groundwater_data %>%
  filter(SiteCode %in% diff_samp$SiteCode) %>%
  arrange(Latitude)





################################
##################

#Compile all values into one file
#define these for every distance calculation
anal = "Ba" #already should be defined above
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
Geospatial_result_NY_Ba = rbind(SW_marg,SW_orph,GW_marg,GW_orph)

saveRDS(Geospatial_result_NY_Ba,file = "Geospatial_results_NY_Ba.rds", compress = FALSE)



