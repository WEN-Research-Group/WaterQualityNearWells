#Texas data - compare surface water and ground water sample data

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

data_daily_clean_short = Cleaning_TX_SW_fun_daily(TX_SW_SC,TX_SW_sites)

#Plot all the samples instead of site averages
data_daily_clean_short_org = data_daily_clean_short %>%
  arrange(daily_mean)

pal <- colorNumeric(palette = "viridis", domain = data_daily_clean_short_org$daily_mean)#, reverse = TRUE)
leaflet(data_daily_clean_short_org) %>% addTiles() %>%
  addCircles(lng = data_daily_clean_short_org$Longitude,
             lat = data_daily_clean_short_org$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")
  #addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")



#Analyze surface water 
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

data_daily_clean_short = Cleaning_TX_SW_fun_daily(TX_SW_Na,TX_SW_sites)

#Plot all the samples instead of site averages
data_daily_clean_short_org = data_daily_clean_short %>%
  arrange(daily_mean)
View(data_daily_clean_short_org)

pal <- colorNumeric(palette = "viridis", domain = data_daily_clean_short_org$daily_mean)
leaflet(data_daily_clean_short_org) %>% addTiles() %>%
  addCircles(lng = data_daily_clean_short_org$Longitude,
             lat = data_daily_clean_short_org$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#above 1000000
data_all_short_daily_summary_ab_aver = filter(data_daily_clean_short_org, daily_mean > 1000000)
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")


#### CL - SW ####
TX_SW_Cl = read.csv("./input/Texas_surface_WQ/Chloride/TX_SW_Chloride.csv", header = TRUE)

#Analyze units
unique(TX_SW_Cl$Parameter.Description)
#Some samples are missing first few column, remove them
TX_SW_Cl = filter(TX_SW_Cl, Parameter.Description == "CHLORIDE (MG/L AS CL)") 

#Change units if needed - no need for SC
TX_SW_Cl$Value = TX_SW_Cl$Value *1000

data_daily_clean_short = Cleaning_TX_SW_fun_daily(TX_SW_Cl,TX_SW_sites)

#Plot all the samples instead of site averages
data_daily_clean_short_org = data_daily_clean_short %>%
  arrange(daily_mean)
View(data_daily_clean_short_org)

#Remove 4 largest values
data_daily_clean_short_org = filter(data_daily_clean_short_org, daily_mean < 98141001)

pal <- colorNumeric(palette = "viridis", domain = data_daily_clean_short_org$daily_mean)
leaflet(data_daily_clean_short_org) %>% addTiles() %>%
  addCircles(lng = data_daily_clean_short_org$Longitude,
             lat = data_daily_clean_short_org$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#above 1000000
data_all_short_daily_summary_ab_aver = filter(data_daily_clean_short_org, daily_mean > 1000000)
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")



#### Sulfate - SW ####
TX_SW_Sulf = read.csv("./input/Texas_surface_WQ/Sulfate/TX_SW_Sulfate.csv", header = TRUE)

#Analyze units
unique(TX_SW_Sulf$Parameter.Description)
#Some samples are missing first few column, remove them
TX_SW_Sulf = filter(TX_SW_Sulf, Parameter.Description == "SULFATE (MG/L AS SO4)")

#Change units if needed - no need for SC
TX_SW_Sulf$Value = TX_SW_Sulf$Value *1000

data_daily_clean_short = Cleaning_TX_SW_fun_daily(TX_SW_Sulf,TX_SW_sites)

#Plot all the samples instead of site averages
data_daily_clean_short_org = data_daily_clean_short %>%
  arrange(daily_mean)
View(data_daily_clean_short_org)

pal <- colorNumeric(palette = "viridis", domain = data_daily_clean_short_org$daily_mean)
leaflet(data_daily_clean_short_org) %>% addTiles() %>%
  addCircles(lng = data_daily_clean_short_org$Longitude,
             lat = data_daily_clean_short_org$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#above 1000000
data_all_short_daily_summary_ab_aver = filter(data_daily_clean_short_org, daily_mean > 1000000)
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")





#Compare with GW data


#### Analyze raw TWDB file (my download)####
wq_det = read.table(file="./input/GWDBDownloadSQL/GWDBDownloadSQL/DB_WaterQualityDetail.txt", sep="|", dec=".", header = TRUE, fill = TRUE)

#remove NA values
wq_det_cor = filter(wq_det,!is.na(ParameterValue))

#get summary
wq_det_sum = wq_det_cor %>%
  group_by(WaterQualityParameterId) %>%
  summarise(n =n(),min = min(ParameterValue),max = max(ParameterValue))

#where those disturbingly high values are coming from??
wq_det_cor_org = wq_det_cor %>%
  arrange(WaterQualityParameterId, ParameterValue)

#Just Na samples
wq_det_cor_org_just_na = wq_det_cor_org %>%
  filter(WaterQualityParameterId == 73 | WaterQualityParameterId == 74)

#Those high values are also in raw files. WTF


#### TWDB Groundwater Quality Data####
# load SQL txt files
wqheader <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/DB_WaterQualityHeader.txt", sep="|", dec=".", header = TRUE, fill = TRUE)
wqdetail <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/DB_WaterQualityDetail.txt", sep="|", dec=".", header = TRUE, fill = TRUE)
welllocation <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/DB_WellLocation.txt", sep="|", dec=".", header = TRUE, fill = TRUE)
lu_valueflag <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/LU_ParameterValueFlag.txt", sep="|", dec=".", header = TRUE, fill = TRUE)
lu_param <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/LU_WaterQualityParameters.txt", sep="|", dec=".", header = TRUE, fill = TRUE)
lu_county <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/LU_County.txt", sep="|", dec=".", header = TRUE, fill = TRUE)


# keep only the essential columns
lu_county <- lu_county %>%
  select(c("CountyId", "CountyNameMixed"))

welllocation <- welllocation %>%
  select(c("StateWellId", "CountyId", "CoordDDLat", "CoordDDLong")) %>%
  left_join(y=lu_county, by = "CountyId") %>%
  select(-CountyId)

wqheader <- wqheader %>%
  select(c("WaterQualityHeaderId", "StateWellId", "SampleDate")) %>%
  #remove rows with empty sample date
  filter(!wqheader$SampleDate == "") %>%
  left_join(y=welllocation, by = "StateWellId") %>%
  mutate(State = "Texas")

lu_valueflag <- lu_valueflag %>%
  select(c("ParameterValueFlagId", "Description"))

lu_param <- lu_param %>%
  select(c("WaterQualityParameterId", "ParameterLongDescription", 
           "ParameterUnitOfMeasure"))

wqdetail_all <- wqdetail %>%
#  select(-c("ParameterValuePlusMinus", "CreatedDate", "LastUpdateDate")) %>%
  left_join(y=lu_valueflag, by = "ParameterValueFlagId") %>%
  left_join(y=lu_param, by = "WaterQualityParameterId") %>%
  left_join(y=wqheader, by = "WaterQualityHeaderId") #%>%
  # remove rows with NA values in statewellID, coordinates, and sampling dates
#  filter(!is.na(StateWellId),
#         !is.na(CoordDDLat),
#         !is.na(CoordDDLong),
#         !is.na(SampleDate))

#rm(lu_county, welllocation, wqheader)

#saveRDS(wqdetail, file="./input/TWDB/gw_twdb_curated_v1.rds", compress = FALSE)



#### SC - GW ####
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


#Remove not detected values
#gw.twdb.sc_long_clean = filter(gw.twdb.sc_long, !Description == "not detected")

gw.twdb.sc_long_clean = gw.twdb.sc_long %>%
  filter(is.na(Description) & DataValue > 1) %>%
  arrange(DataValue)
View(gw.twdb.sc_long_clean)  

#Calculate daily averages 
data_all_short_daily_summary = gw.twdb.sc_long_clean %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

#Map
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary$daily_mean)
leaflet(data_all_short_daily_summary) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary$Longitude,
             lat = data_all_short_daily_summary$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")




#### SODIUM - GW ####

# Na parameterId are 73 and 74 while Cl are 80 and 81. All values are in mg/l
wq_na <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(73, 74)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  # convert unit from mg/L to ug/L
  mutate(ParameterValue = ParameterValue * 1000) %>%
  # remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  filter(is.na(Description)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate, DataValue = ParameterValue) %>%
  arrange(DataValue)
View(wq_na)  

#Calculate site averages
data_all_short_daily_summary_by_site = wq_na %>%
  group_by(County, SiteCode, Latitude, Longitude) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(data_all_short_daily_summary_by_site)

data_all_short_daily_summary = wq_na %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(data_all_short_daily_summary)

#Map
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary$daily_mean)
leaflet(data_all_short_daily_summary) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary$Longitude,
             lat = data_all_short_daily_summary$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#plot just values above average
data_all_short_daily_summary_ab_aver = filter(data_all_short_daily_summary, daily_mean > mean(daily_mean))
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#above 1000000
data_all_short_daily_summary_ab_aver = filter(data_all_short_daily_summary, daily_mean > 1000000)
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")


#Plot these high sample values with all wells  
source("Well_map.R")
texas_marg_w = readRDS(file="Texas_marginal_wells_curated.rds")
data_all_short_daily_summary_ab_1m = filter(data_all_short_daily_summary, daily_mean > 1000000)
data_all_short_daily_summary_bel_1m = filter(data_all_short_daily_summary, daily_mean <= 1000000)
TX_high_gw_marg_map = Well_map(texas_marg_w, data_all_short_daily_summary_ab_aver,"Marginal wells","High Groundwater Na")
TX_high_gw_marg_map
source("Well_map_3sp.R")
TX_high_gw_marg_map_3sp = Well_map_3sp(texas_marg_w, data_all_short_daily_summary_ab_1m, data_all_short_daily_summary_bel_1m,"Marginal wells","High Groundwater Na","Normal Na")
TX_high_gw_marg_map_3sp

#Pull all wells from enverus database that Greg gave us
texas_all_wells = read.csv(file="./input/TX_wells_enverus.csv",header = TRUE)
View(texas_all_wells)
#Number of unique wells
length(unique(texas_all_wells$API_UWI))
#Keep only some of the columns to save memory
colnames(texas_all_wells)
k = c("API_UWI","API_UWI_Unformatted","Latitude","Longitude")
texas_all_wells_short = texas_all_wells[,k]
#Map
TX_high_gw_all_w_map_3sp = Well_map_3sp(texas_all_wells_short, data_all_short_daily_summary_ab_1m, data_all_short_daily_summary_bel_1m,"All wells","High Groundwater Na","Normal Na")
TX_high_gw_all_w_map_3sp
saveWidget(widget = TX_high_gw_all_w_map_3sp, file = "./output/maps/TX_high_gw_na_all_w_map.html")



library(raster)
View(data_all_short_daily_summary)
# convert to spatial dataset
sp.coords <- SpatialPoints(data_all_short_daily_summary[,c("Longitude","Latitude")])

crs.geo<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(sp.coords) <-crs.geo

# project all spatial layers to NAD83 for distance calculation
sp.coords <- spTransform(sp.coords, CRS("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

sp.daily <- spTransform(data_all_short_daily_summary[-c(1:4,40899),], CRS("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

data_all_short_daily_summary = data_all_short_daily_summary[-c(1:4,40899),]
wgs84 = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sp.vals = SpatialPointsDataFrame(data_all_short_daily_summary[,c("Longitude","Latitude")], data_all_short_daily_summary, proj4string = wgs84)
sp.daily <- spTransform(sp.vals, CRS("+proj=lcc +lat_0=31.1666666666667 +lon_0=-100 +lat_1=27.4166666666667 +lat_2=34.9166666666667 +x_0=914400 +y_0=914400 +datum=NAD27 +units=ft +no_defs +type=crs"))

#Texas-106.66, 25.83, -93.5, 36.5
neighborhoods = readOGR(dsn='.', layer='statistical_neighborhoods', stringsAsFactors = F)

pixelsize = 100
box = round(extent(sp.daily) / pixelsize) * pixelsize
template = raster(box, crs = CRS("+proj=lcc +lat_0=31.1666666666667 +lon_0=-100 +lat_1=27.4166666666667 +lat_2=34.9166666666667 +x_0=914400 +y_0=914400 +datum=NAD27 +units=ft +no_defs +type=crs"),
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

raster_gw = rasterize(sp.daily, template, field = 'daily_mean', fun = mean)
plot(raster_gw, xlim=c(-8185112, -7239212), ylim=c(7598328, 8798228))
plot(neighborhoods, border='#00000040', add=T)


pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary$daily_mean)
Sta_den <- st_kde(as.data.frame(data_all_short_daily_summary), gridsize = c(100, 100))
contours <- eks::st_get_contour(Sta_den, cont = c(20,40,60,80)) %>% 
  mutate(value=as.numeric(levels(contlabel)))

leaflet(data_all_short_daily_summary) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary$Longitude,
             lat = data_all_short_daily_summary$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addPolygons(fillColor = ~pal_fun(as.numeric(contlabel)),
              popup = p_popup, weight=2, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

  
  
  
#### CL - GW ####
# Na parameterId are 73 and 74 while Cl are 80 and 81. All values are in mg/l
wq_cl <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(80,81)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  # convert unit from mg/L to ug/L
  mutate(ParameterValue = ParameterValue * 1000) %>%
  # remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  filter(is.na(Description) & ParameterValue > 1) %>%
  #filter(is.na(Description)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate, DataValue = ParameterValue) %>%
  arrange(DataValue)
View(wq_cl)  

#Remove the largest value since it's too big compared to the rest
wq_cl = wq_cl[-dim(wq_cl)[1],]

data_all_short_daily_summary = wq_cl %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)

pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary$daily_mean)
leaflet(data_all_short_daily_summary) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary$Longitude,
             lat = data_all_short_daily_summary$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")


#Map - plot just values above average
mean(data_all_short_daily_summary$daily_mean)
data_all_short_daily_summary_ab_aver = filter(data_all_short_daily_summary, daily_mean > mean(daily_mean))
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#above 1000000
data_all_short_daily_summary_ab_aver = filter(data_all_short_daily_summary, daily_mean > 1000000)
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")



### SULFATE GW ####

#82, and 83 are sulfate. All in mg/L. 82 and 83 are as SO4 
wq_sulf <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(82, 83)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  # convert unit from mg/L to ug/L
  mutate(ParameterValue = ParameterValue * 1000) %>%
  # remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  filter(is.na(Description) & ParameterValue> 1) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate, DataValue = ParameterValue) %>%
  arrange(DataValue)
View(wq_sulf)  

#Calculate site averages
data_all_short_daily_summary_by_site = wq_na %>%
  group_by(County, SiteCode, Latitude, Longitude) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(data_all_short_daily_summary_by_site)

data_all_short_daily_summary = wq_sulf %>%
  group_by(County, SiteCode, Latitude, Longitude, Samplingdate) %>%
  summarise(n = n(), daily_mean = mean(DataValue), daily_median = median(DataValue), 
            min_v = min(DataValue),max_v = max(DataValue)) %>%
  arrange(daily_mean)
View(data_all_short_daily_summary_by_site)

#Map
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary$daily_mean)
leaflet(data_all_short_daily_summary) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary$Longitude,
             lat = data_all_short_daily_summary$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")


#above 1000000
data_all_short_daily_summary_ab_aver = filter(data_all_short_daily_summary, daily_mean > 1000000)
pal <- colorNumeric(palette = "viridis", domain = data_all_short_daily_summary_ab_aver$daily_mean)
leaflet(data_all_short_daily_summary_ab_aver) %>% addTiles() %>%
  addCircles(lng = data_all_short_daily_summary_ab_aver$Longitude,
             lat = data_all_short_daily_summary_ab_aver$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")
