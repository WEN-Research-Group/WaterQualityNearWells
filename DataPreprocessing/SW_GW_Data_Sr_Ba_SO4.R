#-------------------------------------------------------------------#
#
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
# Code Block for Downloading Ba, Sr, and SO4 Data #
# I modified the flag of Zip from yes to no #
# Site info
#https://www.waterqualitydata.us/data/Station/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Barium&characteristicName=Strontium&characteristicName=Sulfate&characteristicName=Sulfate%20as%20S&characteristicName=Total%20Sulfate&characteristicName=Sulfate%20as%20SO4&characteristicName=Sulfur%20Sulfate&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET
# Narrow results
#https://www.waterqualitydata.us/data/Result/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Barium&characteristicName=Strontium&characteristicName=Sulfate&characteristicName=Sulfate%20as%20S&characteristicName=Total%20Sulfate&characteristicName=Sulfate%20as%20SO4&characteristicName=Sulfur%20Sulfate&mimeType=csv&zip=no&dataProfile=narrowResult&providers=NWIS&providers=STEWARDS&providers=STORET
#Result detection quantitation limit
#https://www.waterqualitydata.us/data/ResultDetectionQuantitationLimit/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Barium&characteristicName=Strontium&characteristicName=Sulfate&characteristicName=Sulfate%20as%20S&characteristicName=Total%20Sulfate&characteristicName=Sulfate%20as%20SO4&characteristicName=Sulfur%20Sulfate&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET
#Activity
#https://www.waterqualitydata.us/data/Activity/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Barium&characteristicName=Strontium&characteristicName=Sulfate&characteristicName=Sulfate%20as%20S&characteristicName=Total%20Sulfate&characteristicName=Sulfate%20as%20SO4&characteristicName=Sulfur%20Sulfate&mimeType=csv&zip=no&dataProfile=activityAll&providers=NWIS&providers=STEWARDS&providers=STORET

# generate the list of states FIPS on the continental U.S.
state.fips <- data.frame(name=state.name, fips=fips(state.name))
state.fips <- state.fips[!(state.fips$name %in% c("Hawaii", "Alaska")),]
state.fips$US_fips <- paste("US:",state.fips$fips,sep = "")

# read in the list of sites downloaded from WQP website
gw.all.sites <- read.csv(file = "./input/water_chemistry/gw_ba_sr_so4_us_station.csv",
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
saveRDS(gw.all.sites, file="./input/water_chemistry/gw_sites_ba_sr_so4_us.rds", compress = FALSE)

# Download data from the above monitoring sites
gw.all.results <- read.csv(file = "./input/water_chemistry/gw_ba_sr_so4_us_narrowresult.csv",
                           stringsAsFactors = FALSE, na.strings = "")

# save downloaded results into RDS file
saveRDS(gw.all.results, file="./input/water_chemistry/gw_results_ba_sr_so4_us.rds", compress = FALSE)

# further subset by ActivityMediaSubdivisionName to keep only "NA", "Groundwater"
# "Water", and "Ground Water". Then save to csv and rds file
gw.all.activity <- read.csv(file = "./input/water_chemistry/gw_ba_sr_so4_us_activityall.csv",
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

saveRDS(gw.all.results, file="./input/water_chemistry/gw_results_ba_sr_so4_us_ActivityMediaSubdivision.rds", compress = FALSE)


# merge all.sites, all.results, and detection limit information
gw.all.detection <- read.csv(file = "./input/water_chemistry/gw_ba_sr_so4_us_resdetectqntlmt.csv",
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
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mg/l CaCO3**" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "pCi/L" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "ueq/L" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mm")

index[is.na(index)] <- FALSE
gw.wqp_data <- gw.wqp_data[!index,]

# convert unit
index = gw.wqp_data$ResultMeasure.MeasureUnitCode %in% c("ug/l", "ppb")
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

# Drop CharacteristicName == Sulfur Sulfate as it is likely organic species
# Unify sulfate names to Sulfate (as sulfate)
index <- (gw.wqp_data$CharacteristicName == "Sulfur Sulfate")
index[is.na(index)] <- FALSE
gw.wqp_data <- gw.wqp_data[!index,]

index = gw.wqp_data$CharacteristicName == "Sulfate as SO4"
gw.wqp_data[index, "CharacteristicName"] <- "Sulfate"

index = gw.wqp_data$CharacteristicName == "Sulfate as S"
gw.wqp_data[index, "CharacteristicName"] <- "Sulfate"
gw.wqp_data[index, "ResultMeasureValue"] <- gw.wqp_data[index, "ResultMeasureValue"] * 3


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

gw.wqp_data.ba <- gw.wqp_data[gw.wqp_data$Analyte == "Barium",]
gw.wqp_data.sr <- gw.wqp_data[gw.wqp_data$Analyte == "Strontium",]
gw.wqp_data.so4 <- gw.wqp_data[gw.wqp_data$Analyte == "Sulfate",]

# derive the day of sampling date
gw.wqp_data.ba <- gw.wqp_data.ba %>%  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  select(-Analyte) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))

gw.wqp_data.sr <- gw.wqp_data.sr %>%  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  select(-Analyte) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))

gw.wqp_data.so4 <- gw.wqp_data.so4 %>%  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  select(-Analyte) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))


saveRDS(gw.wqp_data.ba, file="./input/water_chemistry/gw_results_curated_ba_us.rds", compress = FALSE)
saveRDS(gw.wqp_data.sr, file="./input/water_chemistry/gw_results_curated_sr_us.rds", compress = FALSE)
saveRDS(gw.wqp_data.so4, file="./input/water_chemistry/gw_results_curated_so4_us.rds", compress = FALSE)

# Plotting temporal histogram
gw_ba_hist <- ggplot(gw.wqp_data.ba, aes(Samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("GW Barium") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
gw_ba_hist
ggsave("./output/figures/gw_ba_hist.png")

gw_sr_hist <- ggplot(gw.wqp_data.sr, aes(Samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("GW Strontium") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
gw_sr_hist
ggsave("./output/figures/gw_sr_hist.png")

gw_so4_hist <- ggplot(gw.wqp_data.so4, aes(Samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("GW Sulfate") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
gw_so4_hist
ggsave("./output/figures/gw_so4_hist.png")



# Generate list of sites reporting data
gw.all.sites.ba <- gw.wqp_data.ba[!duplicated(gw.wqp_data.ba$SiteCode)
                                  ,c("SiteCode", "Latitude", "Longitude")]
gw.all.sites.sr <- gw.wqp_data.sr[!duplicated(gw.wqp_data.sr$SiteCode)
                                  ,c("SiteCode", "Latitude", "Longitude")]
gw.all.sites.so4 <- gw.wqp_data.so4[!duplicated(gw.wqp_data.so4$SiteCode)
                                  ,c("SiteCode", "Latitude", "Longitude")]





#### Shale Network WQ Data ####
##### Barium data ####
sn_ba_datavalue <- read.csv(file="./input/SN/SN_Barium_062723.csv",
                            na.strings = "", stringsAsFactors = FALSE)

sn_unit <- read.csv(file="./input/SN/SN_SC_unit_071022.csv",
                       na.strings = "", stringsAsFactors = FALSE)

sn_ba_raw <- merge(x=sn_ba_datavalue, y=sn_unit, all.x=TRUE,
                   by.x="VariableUnitsID", by.y="UnitsID")

sn_ba_curated <- sn_ba_raw %>%
  # remove production, flowback water, wastewater, and unknown
  filter(!(SampleMedium %in% c("Flowback water", "Flowback Water",
                               "Injection Water", "Production water",
                               "Wastewater effluent", "Unknown"))) %>%
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
gw.sn_ba_curated <- sn_ba_curated %>%
  filter(SampleMedium == "Groundwater") %>%
  select(-SampleMedium)

saveRDS(gw.sn_ba_curated, file="./input/SN/gw_sn_ba_curated.rds",
        compress = FALSE)

sw.sn_ba_curated <- sn_ba_curated %>%
  filter(SampleMedium == "Surface Water") %>%
  select(-SampleMedium)

saveRDS(sw.sn_ba_curated, file="./input/SN/sw_sn_ba_curated.rds",
        compress = FALSE)

#rm(list=ls())
gc()



##### Strontium data ####
sn_sr_datavalue <- read.csv(file="./input/SN/SN_Strontium_062723.csv",
                            na.strings = "", stringsAsFactors = FALSE)

sn_unit <- read.csv(file="./input/SN/SN_SC_unit_071022.csv",
                    na.strings = "", stringsAsFactors = FALSE)

sn_sr_raw <- merge(x=sn_sr_datavalue, y=sn_unit, all.x=TRUE,
                   by.x="VariableUnitsID", by.y="UnitsID")

sn_sr_curated<- sn_sr_raw %>%
  # remove production, flowback water, wastewater, and unknown
  filter(!(SampleMedium %in% c("Flowback water", "Flowback Water",
                               "Injection Water", "Production water",
                               "Wastewater effluent", "Unknown"))) %>%
  ## remove units of %
  filter(!(UnitsAbbreviation %in% c("%"))) %>%
  ## convert mM to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mM"),
                            DataValue * 87620, DataValue)) %>%
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
gw.sn_sr_curated <- sn_sr_curated %>%
  filter(SampleMedium == "Groundwater") %>%
  select(-SampleMedium)

saveRDS(gw.sn_sr_curated, file="./input/SN/gw_sn_sr_curated.rds",
        compress = FALSE)

sw.sn_sr_curated <- sn_sr_curated %>%
  filter(SampleMedium == "Surface Water") %>%
  select(-SampleMedium)

saveRDS(sw.sn_sr_curated, file="./input/SN/sw_sn_sr_curated.rds",
        compress = FALSE)

#rm(list=ls())
gc()



##### Sulfate data ####
sn_so4_datavalue <- read.csv(file="./input/SN/SN_Sulfate_101123.csv",
                            na.strings = "", stringsAsFactors = FALSE)

sn_unit <- read.csv(file="./input/SN/SN_SC_unit_071022.csv",
                    na.strings = "", stringsAsFactors = FALSE)

sn_so4_raw <- merge(x=sn_so4_datavalue, y=sn_unit, all.x=TRUE,
                   by.x="VariableUnitsID", by.y="UnitsID")

sn_so4_curated<- sn_so4_raw %>%
  # remove production, flowback water, wastewater, and unknown
  filter(!(SampleMedium %in% c("Flowback water", "Flowback Water",
                               "Injection Water", "Production water",
                               "Wastewater effluent", "Unknown"))) %>%
  ## remove units of - and lb/d
  filter(!(UnitsAbbreviation %in% c("-", "lb/d"))) %>%
  ## convert mM to ug/L
  mutate(DataValue = ifelse(UnitsAbbreviation %in% c("mM"),
                            DataValue * 96000, DataValue)) %>%
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
gw.sn_so4_curated <- sn_so4_curated %>%
  filter(SampleMedium == "Groundwater") %>%
  select(-SampleMedium)

saveRDS(gw.sn_so4_curated, file="./input/SN/gw_sn_so4_curated.rds",
        compress = FALSE)

#rm(list=ls())
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
           c("BARIUM", "STRONTIUM", "SULFATE"),
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

# separate data into ba and sr datasets
cogcc_wq_curated.ba <- cogcc_wq_curated[cogcc_wq_curated$Analyte == "BARIUM",]
cogcc_wq_curated.sr <- cogcc_wq_curated[cogcc_wq_curated$Analyte == "STRONTIUM",]
cogcc_wq_curated.so4 <- cogcc_wq_curated[cogcc_wq_curated$Analyte == "SULFATE",]


# calculate site-level daily average
cogcc_wq_curated.ba <- cogcc_wq_curated.ba %>%
  select(-Analyte) %>%
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
  summarise(daily_sc = mean(DataValue))

cogcc_wq_curated.sr <- cogcc_wq_curated.sr %>%
  select(-Analyte) %>%
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
    summarise(daily_sc = mean(DataValue))

cogcc_wq_curated.so4 <- cogcc_wq_curated.so4 %>%
  select(-Analyte) %>%
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
  summarise(daily_sc = mean(DataValue))

  
  
# extract groundwater and surface water samples
gw.cogcc_wq_curated.ba <- cogcc_wq_curated.ba %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring")) %>%
  select(-SampleMedium)

sw.cogcc_wq_curated.ba <- cogcc_wq_curated.ba %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water")) %>%
  select(-SampleMedium)

saveRDS(gw.cogcc_wq_curated.ba, file="./input/COGCC_WQ/gw_cogcc_ba_curated.rds",
        compress = FALSE)
saveRDS(sw.cogcc_wq_curated.ba, file="./input/COGCC_WQ/sw_cogcc_ba_curated.rds",
        compress = FALSE)


gw.cogcc_wq_curated.sr <- cogcc_wq_curated.sr %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring")) %>%
  select(-SampleMedium)

sw.cogcc_wq_curated.sr <- cogcc_wq_curated.sr %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water")) %>%
  select(-SampleMedium)

saveRDS(gw.cogcc_wq_curated.sr, file="./input/COGCC_WQ/gw_cogcc_sr_curated.rds",
        compress = FALSE)
saveRDS(sw.cogcc_wq_curated.sr, file="./input/COGCC_WQ/sw_cogcc_sr_curated.rds",
        compress = FALSE)


gw.cogcc_wq_curated.so4 <- cogcc_wq_curated.so4 %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring")) %>%
  select(-SampleMedium)

sw.cogcc_wq_curated.so4 <- cogcc_wq_curated.so4 %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water")) %>%
  select(-SampleMedium)

saveRDS(gw.cogcc_wq_curated.so4, file="./input/COGCC_WQ/gw_cogcc_so4_curated.rds",
        compress = FALSE)
saveRDS(sw.cogcc_wq_curated.so4, file="./input/COGCC_WQ/sw_cogcc_so4_curated.rds",
        compress = FALSE)


#rm(list=ls())
gc()

# load curated dataset




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
# Sr parameterId are 122 and 123 while Ba are 91 and 92. All values are in ug/l
# 82, and 83 are sulfate. All in mg/L. 82 and 83 are as SO4 
# Although 7 is also sulfate related but it is as sulfure and has only two measurements
wq_ba_sr_so4 <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(122, 123, 91, 92, 82, 83)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  # remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  filter(!Description == "not detected" | is.na(Description)) %>%
  # drop columns not needed
  select(-c("WaterQualityDetailId", "WaterQualityHeaderId", 
            "WaterQualityParameterId", "ParameterValueFlagId",
            "Description", "ParameterUnitOfMeasure"))

gw.twdb.ba <- wq_ba_sr_so4 %>% filter(ParameterLongDescription %in%
                                    c("BARIUM, DISSOLVED (UG/L AS BA)",
                                      "BARIUM, TOTAL (UG/L AS BA)")) %>%
  select(-ParameterLongDescription)

gw.twdb.sr <- wq_ba_sr_so4 %>% filter(ParameterLongDescription %in%
                                    c("STRONTIUM, DISSOLVED (UG/L AS SR)",
                                      "STRONTIUM, TOTAL (UG/L AS SR)")) %>%
  select(-ParameterLongDescription)

gw.twdb.so4 <- wq_ba_sr_so4 %>% filter(ParameterLongDescription %in%
                                         c("SULFATE, TOTAL  (MG/L AS SO4)",
                                           "SULFATE, DISSOLVED (MG/L AS SO4)")) %>%
  # convert unit from mg/l to ug/l
  mutate(ParameterValue = ParameterValue * 1000) %>%
  select(-ParameterLongDescription)


#calculate daily average
gw.twdb.ba <- gw.twdb.ba %>% 
  group_by(StateWellId, CoordDDLat, CoordDDLong, 
           CountyNameMixed, State, SampleDate) %>%
  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate)

gw.twdb.sr <- gw.twdb.sr %>% 
  group_by(StateWellId, CoordDDLat, CoordDDLong, 
           CountyNameMixed, State, SampleDate) %>%
  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate)

gw.twdb.so4 <- gw.twdb.so4 %>% 
  group_by(StateWellId, CoordDDLat, CoordDDLong, 
           CountyNameMixed, State, SampleDate) %>%
  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate)


#save to rds file
saveRDS(gw.twdb.ba, file="./input/TWDB/gw_twdb_curated_ba.rds", compress = FALSE)
saveRDS(gw.twdb.sr, file="./input/TWDB/gw_twdb_curated_sr.rds", compress = FALSE)
saveRDS(gw.twdb.so4, file="./input/TWDB/gw_twdb_curated_so4.rds", compress = FALSE)


