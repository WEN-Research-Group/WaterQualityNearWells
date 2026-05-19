#-------------------------------------------------------------------#

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
library(rgdal)
library(FNN)
library(htmlwidgets)
library(ggpubr)
library(rstudioapi)
library(janitor)

wd_folder_path <- getActiveDocumentContext()$path
setwd(dirname(wd_folder_path))

if (!dir.exists(file.path(getwd(),"input","water_chemistry"))) {
  dir.create(file.path(wd_folder_path,"input","water_chemistry"),recursive = TRUE)
}

#### WQP Groundwater Chemistry Downloading ####
# Code Block for Downloading Na and Cl Data #
# I modified the flag of Zip from yes to no #
# Site info
#https://www.waterqualitydata.us/data/Station/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Chloride&characteristicName=Sodium&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET
# Narrow results
#https://www.waterqualitydata.us/data/Result/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Chloride&characteristicName=Sodium&mimeType=csv&zip=no&dataProfile=narrowResult&providers=NWIS&providers=STEWARDS&providers=STORET
#Result detection quantitation limit
#https://www.waterqualitydata.us/data/ResultDetectionQuantitationLimit/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Chloride&characteristicName=Sodium&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET
#Activity
#https://www.waterqualitydata.us/data/Activity/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Chloride&characteristicName=Sodium&mimeType=csv&zip=no&dataProfile=activityAll&providers=NWIS&providers=STEWARDS&providers=STORET

# generate the list of states FIPS on the continental U.S.
state.fips <- data.frame(name=state.name, fips=fips(state.name))
state.fips <- state.fips[!(state.fips$name %in% c("Hawaii", "Alaska")),]
state.fips$US_fips <- paste("US:",state.fips$fips,sep = "")

# read in the list of sites downloaded from WQP website
gw.all.sites <- read.csv(file = "./input/water_chemistry/gw_na_cl_us_station.csv",
                         stringsAsFactors = FALSE, na.strings = "")

gw.all.sites$StateCode <- as.character(formatC(gw.all.sites$StateCode,
                                               width = 2, format = 'd',
                                               flag = '0'))
gw.all.sites$CountyCode <- formatC(gw.all.sites$CountyCode, width = 3, format='d',
                                   flag = '0')
gw.all.sites <- merge(x = gw.all.sites, y = state.fips,
                      by.x="StateCode", by.y="fips", all.x = TRUE)
gw.all.sites <- merge(x = gw.all.sites, y=countyCd,
                      by.x=c("StateCode","CountyCode"),
                      by.y=c("STATE","COUNTY"),all.x = TRUE)
colnames(gw.all.sites)[38] <- "StateName"

# save the downloaded list of sites into RDS file
saveRDS(gw.all.sites, file="./input/water_chemistry/gw_sites_na_cl_us.rds", compress = FALSE)

# Download data from the above monitoring sites
gw.all.results <- read.csv(file = "./input/water_chemistry/gw_na_cl_us_narrowresult.csv",
                           stringsAsFactors = FALSE, na.strings = "")

# save downloaded results into RDS file
saveRDS(gw.all.results, file="./input/water_chemistry/gw_results_na_cl_us.rds", compress = FALSE)

# further subset by ActivityMediaSubdivisionName to keep only "NA", "Groundwater"
# "Water", and "Ground Water". Then save to csv and rds file
gw.all.activity <- read.csv(file = "./input/water_chemistry/gw_na_cl_us_activityall.csv",
                            stringsAsFactors = FALSE, na.strings = "")
gw.all.activity <- gw.all.activity %>% 
  group_by(ActivityIdentifier, ActivityStartDate, 
           MonitoringLocationIdentifier, ActivityMediaSubdivisionName) %>% 
  summarise(COUNT = n()) %>%
  select(-c("COUNT"))

gw.all.results <- merge(x = gw.all.results, y = gw.all.activity,
                        by=c("ActivityIdentifier", "ActivityStartDate", 
                             "MonitoringLocationIdentifier"), all.x = TRUE)
gw.all.results <- gw.all.results[gw.all.results$ActivityMediaSubdivisionName %in% c(NA,"Ground Water", 
                                                                                    "Groundwater", "Water"),]

saveRDS(gw.all.results, file="./input/water_chemistry/gw_results_na_cl_us_ActivityMediaSubdivision.rds", compress = FALSE)


# merge all.sites, all.results, and detection limit information
gw.all.detection <- read.csv(file = "./input/water_chemistry/gw_na_cl_us_resdetectqntlmt.csv",
                             stringsAsFactors = FALSE, na.strings = "")
gw.all.detection <- gw.all.detection %>% 
  arrange(ResultIdentifier, DetectionQuantitationLimitMeasure.MeasureValue) %>%
  distinct(ResultIdentifier, .keep_all = TRUE) %>%
  select(c("ResultIdentifier","DetectionQuantitationLimitMeasure.MeasureValue",
           "DetectionQuantitationLimitMeasure.MeasureUnitCode"))

gw.all.sites.reduced <- gw.all.sites[, c("MonitoringLocationIdentifier",
                                         "MonitoringLocationTypeName",
                                         "LatitudeMeasure", "LongitudeMeasure",
                                         "StateName", "COUNTY_NAME")]
gw.wqp_data <- merge(x = gw.all.results, y = gw.all.sites.reduced,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)
gw.wqp_data <- merge(x = gw.wqp_data, y = gw.all.detection,
                     by = "ResultIdentifier", all.x = TRUE)


## Clean the merged dataset
colnames(gw.wqp_data)

gw.wqp_data$censorcode <- "nc"
gw.wqp_data$samplingdate <- as.POSIXct(gw.wqp_data$ActivityStartDate,
                                       format="%Y-%m-%d")

ggplot(gw.wqp_data, aes(samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  scale_x_datetime(breaks = date_breaks("5 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))

gw.wqp_data <- gw.wqp_data[,!names(gw.wqp_data) %in% c("ActivityStartDate","ActivityStartTime.Time", "ActivityStartDateTime", "ActivityEndDateTime")]
gw.wqp_data <- gw.wqp_data[!is.na(gw.wqp_data$samplingdate),]

# work on censor code and detection limits
index <- (gw.wqp_data$ResultDetectionConditionText == "Detected Not Quantified" |
            gw.wqp_data$ResultDetectionConditionText == "Not Detected" |
            gw.wqp_data$ResultDetectionConditionText == "Not Reported" |
            gw.wqp_data$ResultDetectionConditionText == "Present Below Quantification Limit" |
            gw.wqp_data$ResultDetectionConditionText == "*Non-detect" |
            gw.wqp_data$ResultDetectionConditionText == "*Not detected" |
            gw.wqp_data$ResultDetectionConditionText == "*Not Detected" |
            gw.wqp_data$ResultDetectionConditionText == "*Not Reported" |
            gw.wqp_data$ResultDetectionConditionText == "Not Present" |
            gw.wqp_data$ResultDetectionConditionText == "*Present <QL" |
            gw.wqp_data$ResultDetectionConditionText == "Between Inst Detect and Quant Limit" |
            gw.wqp_data$ResultDetectionConditionText == "Below Method Detection Limit" |
            gw.wqp_data$ResultDetectionConditionText == "Below Detection Limit" |
            gw.wqp_data$ResultDetectionConditionText == "Below Reporting Limit" |
            gw.wqp_data$ResultDetectionConditionText == "Below Sample-specific Detect Limit" |
            gw.wqp_data$ResultDetectionConditionText == "Not Detected at Detection Limit" |
            gw.wqp_data$ResultDetectionConditionText == "Not Detected at Reporting Limit")
index[is.na(index)] <- FALSE
gw.wqp_data[index,"censorcode"] = "lt"
gw.wqp_data[index,"ResultMeasureValue"] = gw.wqp_data[index,"DetectionQuantitationLimitMeasure.MeasureValue"]
gw.wqp_data[index,"ResultMeasure.MeasureUnitCode"] = gw.wqp_data[index,"DetectionQuantitationLimitMeasure.MeasureUnitCode"]
gw.wqp_data[index,"ResultDetectionConditionText"] = "FIXED"

# remove "Systematic Contamination" samples
index <- (gw.wqp_data$ResultDetectionConditionText == "Systematic Contamination")
index[is.na(index)] <- FALSE
gw.wqp_data <- gw.wqp_data[!index,]

# remove non-sense rows
gw.wqp_data$ResultMeasureValue <- as.numeric(gw.wqp_data$ResultMeasureValue)
gw.wqp_data <- gw.wqp_data[!is.na(gw.wqp_data$ResultMeasureValue),]

index = (gw.wqp_data$ResultMeasure.MeasureUnitCode == "None" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "tons/day" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "ueq/L" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "%")

index[is.na(index)] <- FALSE
gw.wqp_data <- gw.wqp_data[!index,]

# convert unit
index = gw.wqp_data$ResultMeasure.MeasureUnitCode %in% c("ug/l", "ppb", "ug/L")
index[is.na(index)] <- FALSE
gw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "ug/L"

index = gw.wqp_data$ResultMeasure.MeasureUnitCode %in% c("mg/l", "mg/L",
                                                         "ppm", "mg/kg")
index[is.na(index)] <- FALSE
gw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "ug/L"
gw.wqp_data[index, "ResultMeasureValue"] <- gw.wqp_data[index, "ResultMeasureValue"] * 1000

# replacing nc with 0 and lt with 1 in censorcode column
index = gw.wqp_data$censorcode == "nc"
gw.wqp_data[index, "censorcode"] <- 0

index = gw.wqp_data$censorcode == "lt"
gw.wqp_data[index, "censorcode"] <- 1

gw.wqp_data$censorcode <- as.numeric(gw.wqp_data$censorcode)


# final cleaning and save clean results to rds file
colnames(gw.wqp_data)
gw.wqp_data <- gw.wqp_data %>%
  select(c("MonitoringLocationIdentifier",
           "LatitudeMeasure", "LongitudeMeasure",
           "StateName", "COUNTY_NAME",
           "samplingdate", "CharacteristicName", "ResultMeasureValue")) %>%
  rename(SiteCode = MonitoringLocationIdentifier, Latitude = LatitudeMeasure,
         Longitude = LongitudeMeasure, State = StateName, County = COUNTY_NAME,
         Samplingdate = samplingdate, Analyte = CharacteristicName,
         DataValue = ResultMeasureValue)

gw.wqp_data.na <- gw.wqp_data[gw.wqp_data$Analyte == "Sodium",]
gw.wqp_data.cl <- gw.wqp_data[gw.wqp_data$Analyte == "Chloride",]

# derive the day of sampling date
gw.wqp_data.na <- gw.wqp_data.na %>%  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  select(-Analyte) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))

gw.wqp_data.cl <- gw.wqp_data.cl %>%  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  select(-Analyte) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))


saveRDS(gw.wqp_data.na, file="./input/water_chemistry/gw_results_curated_na_us.rds", compress = FALSE)
saveRDS(gw.wqp_data.cl, file="./input/water_chemistry/gw_results_curated_cl_us.rds", compress = FALSE)

# Plotting temporal histogram
gw_na_hist <- ggplot(gw.wqp_data.na, aes(Samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("GW Sodium") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
gw_na_hist
ggsave("./output/figures/gw_na_hist.png")

gw_cl_hist <- ggplot(gw.wqp_data.cl, aes(Samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("GW Chloride") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
gw_cl_hist
ggsave("./output/figures/gw_cl_hist.png")



# Generate list of sites reporting data
gw.all.sites.na <- gw.wqp_data.na[!duplicated(gw.wqp_data.na$SiteCode)
                                  ,c("SiteCode", "Latitude", "Longitude")]
gw.all.sites.cl <- gw.wqp_data.cl[!duplicated(gw.wqp_data.cl$SiteCode)
                                  ,c("SiteCode", "Latitude", "Longitude")]




#### Shale Network WQ Data ####
##### Sodium data ####
sn_na_datavalue <- read.csv(file="./input/SN/SN_Sodium_101123.csv",
                            na.strings = "", stringsAsFactors = FALSE)

sn_unit <- read.csv(file="./input/SN/SN_SC_unit_071022.csv",
                       na.strings = "", stringsAsFactors = FALSE)

sn_na_raw <- merge(x=sn_na_datavalue, y=sn_unit, all.x=TRUE,
                   by.x="VariableUnitsID", by.y="UnitsID")

sn_na_curated <- sn_na_raw %>%
  # remove production, flowback water, wastewater, and unknown
  filter(!(SampleMedium %in% c("Flowback water", "Flowback Water",
                               "Injection Water", "Production water",
                               "Wastewater effluent", "Unknown"))) %>%
  ## remove units of -
  filter(!(UnitsAbbreviation %in% c("-"))) %>%
  ## convert mM to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mM"),
                            DataValue * 23000, DataValue)) %>%
  mutate(UnitsAbbreviation = "ug/L") %>%
  ## remove columns not needed
  select(c(SiteCode, Latitude, Longitude, State, County, SampleMedium,
           LocalDateTime, DataValue, UnitsAbbreviation, CensorCode)) %>%
  # remove -9999 data values
  filter(DataValue != -9999) %>%
  # format sampling date
  mutate(LocalDateTime = as.POSIXct(LocalDateTime,
                                    format="%Y-%m-%d %H:%M:%OS")) %>%
  # create a column for sampling DAY
  mutate(Samplingdate = floor_date(LocalDateTime, "day")) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
  summarise(daily_sc = mean(DataValue))

# extract groundwater samples
gw.sn_na_curated <- sn_na_curated %>%
  filter(SampleMedium == "Groundwater") %>%
  select(-SampleMedium)

saveRDS(gw.sn_na_curated, file="./input/SN/gw_sn_na_curated.rds",
        compress = FALSE)

# extract groundwater samples
sw.sn_na_curated <- sn_na_curated %>%
  filter(SampleMedium == "Surface Water") %>%
  select(-SampleMedium)

saveRDS(sw.sn_na_curated, file="./input/SN/sw_sn_na_curated.rds",
        compress = FALSE)


rm(list=ls())
gc()



##### Chloride data ####
sn_cl_datavalue <- read.csv(file="./input/SN/SN_Chloride_101123.csv",
                            na.strings = "", stringsAsFactors = FALSE)

sn_unit <- read.csv(file="./input/SN/SN_SC_unit_071022.csv",
                    na.strings = "", stringsAsFactors = FALSE)

sn_cl_raw <- merge(x=sn_cl_datavalue, y=sn_unit, all.x=TRUE,
                   by.x="VariableUnitsID", by.y="UnitsID")

sn_cl_curated<- sn_cl_raw %>%
  # remove production, flowback water, wastewater, and unknown
  filter(!(SampleMedium %in% c("Flowback water", "Flowback Water",
                               "Injection Water", "Production water",
                               "Wastewater effluent", "Unknown"))) %>%
  ## convert mM to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mM"),
                            DataValue * 35453, DataValue)) %>%
  mutate(UnitsAbbreviation = "ug/L") %>%
  ## convert ms/cm to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mg/Kg", "mg/L", 
                                                     "ppm", "ug/mL"),
                            DataValue * 1000, DataValue)) %>%
  mutate(UnitsAbbreviation = "ug/L") %>%
  ## remove columns not needed
  select(c(SiteCode, Latitude, Longitude, State, County, SampleMedium,
           LocalDateTime, DataValue, UnitsAbbreviation, CensorCode)) %>%
  # remove -9999 data values
  filter(DataValue != -9999) %>%
  # format sampling date
  mutate(LocalDateTime = as.POSIXct(LocalDateTime,
                                    format="%Y-%m-%d %H:%M:%OS")) %>%
  # create a column for sampling DAY
  mutate(Samplingdate = floor_date(LocalDateTime, "day")) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
  summarise(daily_sc = mean(DataValue))

# extract groundwater samples
gw.sn_cl_curated <- sn_cl_curated %>%
  filter(SampleMedium == "Groundwater") %>%
  select(-SampleMedium)

saveRDS(gw.sn_cl_curated, file="./input/SN/gw_sn_cl_curated.rds",
        compress = FALSE)

rm(list=ls())
gc()






#### COGCC Water Quality Data ####
cogcc_wq_raw <- readxl::read_xlsx(path="./input/COGCC_WQ/COGCC_BasicQuery.xlsx",
                                  na="") %>% janitor::clean_names()
test <- data.frame(table(cogcc_wq_raw$param_description))
rm(test)

cogcc_wq_curated <- cogcc_wq_raw %>%
  # select only non-NA SC data from relevant surface water and
  # groundwater samples
  filter(param_description %in%
           c("CHLORIDE", "SODIUM"),
         facility_type %in%
           c("Creek","Domestic Well", "Ground Water", "Groundwater",
             "Monitoring Well", "Pond", "River", "Seep", "Spring",
             "Surface Water"),
         !matrix %in% c("GAS", "LIQUID", "SOIL"),
         !is.na(result_value),
         !units == "mg/L as CaCO3") %>%
  # convert all units to ug/L
  mutate(result_value = ifelse(units %in% c("mg/Kg","mg/l","mg/L", "MG/L"),
                               result_value * 1000, result_value)) %>%
  mutate(units = "ug/L") %>%
  # add a column 'State'
  mutate(State="Colorado") %>%
  # select only needed columns and rename them in a way consistent with others
  select(c(facility_id, facility_type, latitude83, longitude83, State,
           county,sample_date,result_value, param_description)) %>%
  rename(SiteCode = facility_id, Latitude = latitude83, Longitude = longitude83,
         County = county, Samplingdate = sample_date, Analyte=param_description,
         SampleMedium = facility_type, DataValue = result_value) %>%
  # derive the day of sampling date
  mutate(Samplingdate = floor_date(Samplingdate, "day"))

# separate data into na and cl datasets
cogcc_wq_curated.na <- cogcc_wq_curated[cogcc_wq_curated$Analyte == "SODIUM",]
cogcc_wq_curated.cl <- cogcc_wq_curated[cogcc_wq_curated$Analyte == "CHLORIDE",]


# calculate site-level daily average
cogcc_wq_curated.na <- cogcc_wq_curated.na %>%
  select(-Analyte) %>%
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
  summarise(daily_sc = mean(DataValue))

cogcc_wq_curated.cl <- cogcc_wq_curated.cl %>%
  select(-Analyte) %>%
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
    summarise(daily_sc = mean(DataValue))
  
  
# extract groundwater and surface water samples
gw.cogcc_wq_curated.na <- cogcc_wq_curated.na %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring")) %>%
  select(-SampleMedium)

sw.cogcc_wq_curated.na <- cogcc_wq_curated.na %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water")) %>%
  select(-SampleMedium)

saveRDS(gw.cogcc_wq_curated.na, file="./input/COGCC_WQ/gw_cogcc_na_curated.rds",
        compress = FALSE)
saveRDS(sw.cogcc_wq_curated.na, file="./input/COGCC_WQ/sw_cogcc_na_curated.rds",
        compress = FALSE)


gw.cogcc_wq_curated.cl <- cogcc_wq_curated.cl %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring")) %>%
  select(-SampleMedium)

sw.cogcc_wq_curated.cl <- cogcc_wq_curated.cl %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water")) %>%
  select(-SampleMedium)

saveRDS(gw.cogcc_wq_curated.cl, file="./input/COGCC_WQ/gw_cogcc_cl_curated.rds",
        compress = FALSE)
saveRDS(sw.cogcc_wq_curated.cl, file="./input/COGCC_WQ/sw_cogcc_cl_curated.rds",
        compress = FALSE)

rm(list=ls())
gc()





#### TWDB Groundwater Quality Data ####
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

wqdetail <- wqdetail %>%
  select(-c("ParameterValuePlusMinus", "CreatedDate", "LastUpdateDate")) %>%
  left_join(y=lu_valueflag, by = "ParameterValueFlagId") %>%
  left_join(y=lu_param, by = "WaterQualityParameterId") %>%
  left_join(y=wqheader, by = "WaterQualityHeaderId") %>%
  # remove rows with NA values in statewellID, coordinates, and sampling dates
  filter(!is.na(StateWellId),
         !is.na(CoordDDLat),
         !is.na(CoordDDLong),
         !is.na(SampleDate))

rm(lu_county, welllocation, wqheader)
saveRDS(wqdetail, file="./input/TWDB/gw_twdb_curated_v1.rds", compress = FALSE)


#extract Ba, Sr, and sulfate data from the master table
# Na parameterId are 73 and 74 while Cl are 80 and 81. All values are in mg/l
wq_na_cl <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(73, 74, 80, 81)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  # convert unit from mg/L to ug/L
  mutate(ParameterValue = ParameterValue * 1000) %>%
  # remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  filter(!Description == "not detected" | is.na(Description)) %>%
  # drop columns not needed
  select(-c("WaterQualityDetailId", "WaterQualityHeaderId", 
            "WaterQualityParameterId", "ParameterValueFlagId",
            "Description", "ParameterUnitOfMeasure"))

gw.twdb.na <- wq_na_cl %>% filter(ParameterLongDescription %in%
                                    c("SODIUM, TOTAL (MG/L AS NA)",
                                      "SODIUM, DISSOLVED (MG/L AS NA)")) %>%
  select(-ParameterLongDescription)

gw.twdb.cl <- wq_na_cl %>% filter(ParameterLongDescription %in%
                                    c("CHLORIDE, TOTAL (MG/L AS CL)",
                                      "CHLORIDE, DISSOLVED (MG/L AS CL)")) %>%
  select(-ParameterLongDescription)



#calculate daily average
gw.twdb.na <- gw.twdb.na %>% 
  group_by(StateWellId, CoordDDLat, CoordDDLong, 
           CountyNameMixed, State, SampleDate) %>%
  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate)

gw.twdb.cl <- gw.twdb.cl %>% 
  group_by(StateWellId, CoordDDLat, CoordDDLong, 
           CountyNameMixed, State, SampleDate) %>%
  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate)


#save to rds file
saveRDS(gw.twdb.na, file="./input/TWDB/gw_twdb_curated_na.rds", compress = FALSE)
saveRDS(gw.twdb.cl, file="./input/TWDB/gw_twdb_curated_cl.rds", compress = FALSE)

rm(list=ls())
gc()



