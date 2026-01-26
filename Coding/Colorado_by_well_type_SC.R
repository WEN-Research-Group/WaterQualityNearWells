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

### SURFACE

#Water Quality Portal
SW_WQP_SC = readRDS("./input/SC_sw_wqp_daily_samples_summary.rds")

#Rename some columns
names(SW_WQP_SC)[names(SW_WQP_SC) == 'daily_sc'] <- 'daily_mean'
names(SW_WQP_SC)[names(SW_WQP_SC) == 'min_sc'] <- 'min_v'
names(SW_WQP_SC)[names(SW_WQP_SC) == 'max_sc'] <- 'max_v'

# Get the state samples 
SW_WQP_SC_CO = filter(SW_WQP_SC,State=="Colorado" & daily_mean >1) 

#Need to remove some columns 
SW_WQP_SC_CO = SW_WQP_SC_CO[,-1]

View(SW_WQP_SC_CO)
#Remove values above 80000
SW_WQP_SC_CO = filter(SW_WQP_SC_CO, daily_mean < 80000)

#COGCC

SW_COGCC_SC = readRDS("sw_cogcc_wq_daily_curated_clean_SC.rds")
SW_COGCC_SC$SiteCode = as.character(SW_COGCC_SC$SiteCode)

#Change some column names
names(SW_COGCC_SC)[names(SW_COGCC_SC) == 'mean_sc'] <- 'daily_mean'
names(SW_COGCC_SC)[names(SW_COGCC_SC) == 'min_sc'] <- 'min_v'
names(SW_COGCC_SC)[names(SW_COGCC_SC) == 'max_sc'] <- 'max_v'

#Remove 2 columns and two largest values
SW_COGCC_SC = SW_COGCC_SC %>%
  filter(daily_mean < 10000) 
  #select(-c("State","SampleMedium"))
SW_COGCC_SC = SW_COGCC_SC [,-c(1,7)]

#Merge two datasets together and save
SW_SC_CO_merged = rbind(SW_COGCC_SC, SW_WQP_SC_CO)

#Save this for later
saveRDS(SW_SC_CO_merged,file = "SW_all_SC_Colorado_merged.rds", compress = FALSE)

#Check if I can exclude GARFIELD
SW_SC_CO_merged_sum = SW_SC_CO_merged%>%
  group_by(County) %>%
  summarise(n=n())

#EXCLUDE GARFIELD from COGCC SW and GW data
SW_COGCC_SC_no_G = filter(SW_COGCC_SC, !County == "GARFIELD")

#Merge two datasets together and save
SW_SC_CO_merged_no_G = rbind(SW_COGCC_SC_no_G, SW_WQP_SC_CO)
View(SW_SC_CO_merged_no_G)

#Save this for later
saveRDS(SW_SC_CO_merged_no_G,file = "SW_all_SC_Colorado_merged_no_G_fr_COGCC.rds", compress = FALSE)


#Map 75% percentile
SW_SC_CO_merged_no_G_org = SW_SC_CO_merged_no_G %>%
  arrange(daily_mean)
Q_75 = quantile(SW_SC_CO_merged_no_G_org$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(SW_SC_CO_merged_no_G_org, daily_mean > Q_75 & Longitude < -20)
data_perc_75_low = filter(SW_SC_CO_merged_no_G_org, daily_mean < Q_75 & Longitude < -20)
#data_perc_75 = filter(GW_WQP_SO4_NY, daily_mean > quantile(GW_WQP_SO4_NY$daily_mean, probs = c(0.75)))
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#Should I include all the sampling points to show all areas it was sampled? - takes too much time to plot
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75_low$Longitude,
             lat = data_perc_75_low$Latitude,
             radius = 1,
             color = "grey") %>% 
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")


### GROUNDWATER

#Water quality portal
#Cleaned WQP data
CO_GW_SC = readRDS(file="gw.wqp_data_CO_daily_categ_ord_corr_SC.rds")
View(CO_GW_SC)

CO_GW_SC = CO_GW_SC[,!names(CO_GW_SC) %in% c("State","MonitoringLocationTypeName","Category")] 

names(CO_GW_SC)[names(CO_GW_SC) == 'mean_sc'] <- 'daily_mean'
names(CO_GW_SC)[names(CO_GW_SC) == 'min_sc'] <- 'min_v'
names(CO_GW_SC)[names(CO_GW_SC) == 'max_sc'] <- 'max_v'

CO_GW_SC$Latitude = as.numeric(CO_GW_SC$Latitude)
CO_GW_SC$Longitude = as.numeric(CO_GW_SC$Longitude)

View(CO_GW_SC)
CO_GW_SC = filter(CO_GW_SC, daily_mean < 50000 & daily_mean > 1)

CO_GW_SC_sum = CO_GW_SC %>%
  group_by(County) %>%
  summarise(n=n())

#GARFIELD data from WQP looks fine 

#COGCC

GW_COGCC_SC = readRDS("gw_cogcc_wq_all_curated_clean_SC.rds")
View(GW_COGCC_SC)
GW_COGCC_SC$SiteCode = as.character(GW_COGCC_SC$SiteCode)

#Change some column names
names(GW_COGCC_SC)[names(GW_COGCC_SC) == 'mean_sc'] <- 'daily_mean'
names(GW_COGCC_SC)[names(GW_COGCC_SC) == 'min_sc'] <- 'min_v'
names(GW_COGCC_SC)[names(GW_COGCC_SC) == 'max_sc'] <- 'max_v'

View(GW_COGCC_SC)
#GARFIELD COUNTY IS WEIRD AGAIN

#Remove 2 columns and two largest values
GW_COGCC_SC = GW_COGCC_SC %>%
  filter(daily_mean > 1) 
#select(-c("State","SampleMedium"))
GW_COGCC_SC = GW_COGCC_SC [,-c(1,7)]

#Check if I can exclude GARFIELD
GW_COGCC_SC_sum = GW_COGCC_SC%>%
  group_by(County) %>%
  summarise(n=n())
#It's the 3rd largest sample size by county

#Merge two datasets together and save
GW_SC_CO_merged = rbind(GW_COGCC_SC, CO_GW_SC)

#Save this for later
saveRDS(GW_SC_CO_merged,file = "GW_all_SC_Colorado_merged.rds", compress = FALSE)

GW_COGCC_SC_no_G = filter(GW_COGCC_SC, !County == "GARFIELD")

GW_COGCC_SC_no_G = filter(GW_COGCC_SC_no_G, daily_mean < 40000)

#Merge two datasets together and save
GW_SC_CO_merged_no_G = rbind(GW_COGCC_SC_no_G, CO_GW_SC)

GW_SC_CO_merged_no_G = filter(GW_SC_CO_merged_no_G, daily_mean < 40000)

#Save this for later
saveRDS(GW_SC_CO_merged_no_G,file = "GW_all_SC_Colorado_merged_no_G_fr_COGCC.rds", compress = FALSE)

View(GW_SC_CO_merged_no_G)


#Map 75% percentile
GW_SC_CO_merged_no_G_org = GW_SC_CO_merged_no_G %>%
  arrange(daily_mean)
Q_75 = quantile(GW_SC_CO_merged_no_G_org$daily_mean, probs = c(0.75)) # that would be 32,000
data_perc_75 = filter(GW_SC_CO_merged_no_G_org, daily_mean > Q_75 & Longitude < -20)
#data_perc_75 = filter(GW_WQP_SO4_NY, daily_mean > quantile(GW_WQP_SO4_NY$daily_mean, probs = c(0.75)))
pal <- colorNumeric(palette = "viridis", domain = data_perc_75$daily_mean)
#map_75 = 
leaflet(data_perc_75) %>% addTiles() %>%
  addCircles(lng = data_perc_75$Longitude,
             lat = data_perc_75$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")


# NOW ANALYZE
#Use function
source("Well_spatial_analysis_compl_0127.R")
#Make sure spud name is "SpudDate"

#Define these variables for function
marginal_well_data = CO_marginal
unconv_well_data = cogcc_uncon_og_well_curated
orphan_well_data = CO_orphaned_clean
surface_water_data = SW_SC_CO_merged_no_G
groundwater_data = GW_SC_CO_merged_no_G

#Plots
#Create a function that would save all 3 plots
source("Saving_figures_fun_exp.R")

#define these for every distance calculation
anal = "SC" #already should be defined above
meas = "uS/cm" 
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
anal = "SC" #already should be defined above
meas = "uS/cm" 
state = "Colorado" 

#SURFACE

#MARGINAL

type_w = "SW"
well_type = "marginal"

#Read analysis results
SW_marg = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_marg$type_w = "SW"
SW_marg$well_type = "marginal"
#232341 values

plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
SW_marg = filter(SW_marg, nearest_distance_m < 6283100)
plot(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))
cor(log(SW_marg$nearest_distance_m), log(SW_marg$daily_mean))


### UNCONVENTIONAL

well_type = "unconventional"

SW_unconv = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,2013:2016)]
SW_unconv$type_w = "SW"
SW_unconv$well_type = "unconventional"

plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
SW_unconv = filter(SW_unconv, nearest_distance_m < 6283100)
plot(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))
cor(log(SW_unconv$nearest_distance_m), log(SW_unconv$daily_mean))


### ORPHANED

well_type = "orphaned"

SW_orph = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))[,c(2:11,881:884)]
SW_orph$type_w = "SW"
SW_orph$well_type = "orphaned"

plot(log(SW_orph$nearest_distance_m), log(SW_orph$daily_mean))
SW_orph = filter(SW_orph, nearest_distance_m < 6283100)
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
#GW_marg = filter(GW_marg, nearest_distance_m < 6283100)
#plot(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))
cor(log(GW_marg$nearest_distance_m), log(GW_marg$daily_mean))

plot(log(GW_marg$num_well_1km[which(GW_marg$num_well_1km>0)]), log(GW_marg$daily_mean[which(GW_marg$num_well_1km>0)]))
cor(log(GW_marg$num_well_1km[which(GW_marg$num_well_1km>0)]), log(GW_marg$daily_mean[which(GW_marg$num_well_1km>0)]))


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


########### SAVE ALL RESULTS
#Combine all 6 into 1 and save
Geospatial_result_CO_SC = rbind(SW_marg,SW_unconv,SW_orph,GW_marg,GW_unconv,GW_orph)

saveRDS(Geospatial_result_CO_SC,file = "Geospatial_results_CO_SC.rds", compress = FALSE)



