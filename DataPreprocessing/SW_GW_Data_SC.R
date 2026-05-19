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
library(rgdal) #not available for this version of R (too recent?)
library(FNN)
library(htmlwidgets)
library(ggpubr)
library(rstudioapi)
library(janitor)

#wd_folder_path <- getActiveDocumentContext()$path
#setwd(dirname(wd_folder_path))

wd_folder_path <- getwd()

if (!dir.exists(file.path(getwd(),"input","water_chemistry"))) {
  dir.create(file.path(wd_folder_path,"input","water_chemistry"),recursive = TRUE)
}

######################################################################
#Water Quality portal

#### WQP Surface Water Chemistry Downloading ####
# generate the list of states FIPS on the continental U.S.
state.fips <- data.frame(name=state.name, fips=fips(state.name))
state.fips <- state.fips[!(state.fips$name %in% c("Hawaii", "Alaska")),]
state.fips$US_fips <- paste("US:",state.fips$fips,sep = "")


##### Downloading data ####
# download the list of sites reporting SC data state by state
analyte.names = c("Specific conductance","Specific conductivity")
sampleMedia = c("water","Water")
siteType = c("Aggregate surface-water-use","Stream", "Wetland",
             "Lake, Reservoir, Impoundment")
sw.all.sites <- data.frame()
for (state.code in state.fips$US_fips) {
  sites <- whatWQPsites(characteristicName = analyte.names,
                        sampleMedia = sampleMedia,
                        siteType = siteType,
                        statecode = state.code)
  sw.all.sites <- rbind(sw.all.sites, sites)
}

sw.all.sites$StateCode <- as.character(sw.all.sites$StateCode)
sw.all.sites$CountyCode <- formatC(sw.all.sites$CountyCode, width = 3, format='d',
                                flag = '0')
sw.all.sites <- merge(x = sw.all.sites, y = state.fips,
                   by.x="StateCode", by.y="fips", all.x = TRUE)
sw.all.sites <- merge(x = sw.all.sites, y=countyCd,
                   by.x=c("StateCode","CountyCode"),
                   by.y=c("STATE","COUNTY"),all.x = TRUE)
colnames(sw.all.sites)[37] <- "StateName"

# save the downloaded list of sites into RDS file
saveRDS(sw.all.sites, file="./input/water_chemistry/sw_sites_sc_us.rds", compress = FALSE)

# Download SC data from the above monitoring sites
sw.all.results <- data.frame()
for (state.code in state.fips$US_fips) {
  results <- readWQPdata(characteristicName = analyte.names,
                         sampleMedia = sampleMedia,
                         siteType = siteType,
                         statecode = state.code,
                         Zip = "no")
  sw.all.results <- rbind(sw.all.results, results)
}


# save downloaded SC results into RDS file
saveRDS(sw.all.results, file="./input/water_chemistry/sw_results_sc_us.rds", compress = FALSE)

# further subset by ActivityMediaSubdivisionName to keep only "NA", "Surface Water"
# and "Water". Then save to csv and rds file
sw.all.results <- sw.all.results[sw.all.results$ActivityMediaSubdivisionName %in% c(NA,"Surface Water", "Water"),]

saveRDS(sw.all.results, file="./input/water_chemistry/sw_results_sc_us_ActivityMediaSubdivision.rds", compress = FALSE)

#####
#Read data
sw.all.results = readRDS("./input/water_chemistry/sw_results_sc_us_ActivityMediaSubdivision.rds")
sw.all.sites = readRDS("./input/water_chemistry/sw_sites_sc_us.rds")


# merge all.sites and all.results
sw.all.sites.reduced <- sw.all.sites[, c("MonitoringLocationIdentifier",
                                   "MonitoringLocationTypeName",
                                   "LatitudeMeasure", "LongitudeMeasure",
                                   "StateName", "COUNTY_NAME")]
sw.wqp_data <- merge(x = sw.all.results, y = sw.all.sites.reduced,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

## Clean the merged dataset
colnames(sw.wqp_data)

sw.wqp_data$censorcode <- "nc"
sw.wqp_data$samplingdate <- as.POSIXct(sw.wqp_data$ActivityStartDate,
                                       format="%Y-%m-%d")

ggplot(sw.wqp_data, aes(samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  scale_x_datetime(breaks = date_breaks("5 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))

##### Cleaning surface data #####

sw.wqp_data <- sw.wqp_data[,!names(sw.wqp_data) %in% c("ActivityStartDate","ActivityStartTime.Time", "ActivityStartDateTime", "ActivityEndDateTime")]
sw.wqp_data <- sw.wqp_data[!is.na(sw.wqp_data$samplingdate),]

# work on censor code and detection limits
index <- (sw.wqp_data$ResultDetectionConditionText == "Detected Not Quantified" |
            sw.wqp_data$ResultDetectionConditionText == "Not Detected" |
            sw.wqp_data$ResultDetectionConditionText == "Not Reported" |
            sw.wqp_data$ResultDetectionConditionText == "Present Below Quantification Limit" |
            sw.wqp_data$ResultDetectionConditionText == "*Non-detect" |
            sw.wqp_data$ResultDetectionConditionText == "*Not Reported" |
            sw.wqp_data$ResultDetectionConditionText == "Not Present" |
            sw.wqp_data$ResultDetectionConditionText == "*Present <QL" |
            sw.wqp_data$ResultDetectionConditionText == "Between Inst Detect and Quant Limit")
index[is.na(index)] <- FALSE
sw.wqp_data[index,"censorcode"] = "lt"
sw.wqp_data[index,"ResultMeasureValue"] = sw.wqp_data[index,"DetectionQuantitationLimitMeasure.MeasureValue"]
sw.wqp_data[index,"ResultMeasure.MeasureUnitCode"] = sw.wqp_data[index,"DetectionQuantitationLimitMeasure.MeasureUnitCode"]
sw.wqp_data[index,"ResultDetectionConditionText"] = "FIXED"

# remove "Systematic Contamination" samples
index <- (sw.wqp_data$ResultDetectionConditionText == "Systematic Contamination")
index[is.na(index)] <- FALSE
sw.wqp_data <- sw.wqp_data[!index,]

# remove non-sense rows
sw.wqp_data$ResultMeasureValue <- as.numeric(sw.wqp_data$ResultMeasureValue)
sw.wqp_data <- sw.wqp_data[!is.na(sw.wqp_data$ResultMeasureValue),]

index = (sw.wqp_data$ResultMeasure.MeasureUnitCode == "%" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "cfs" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "deg C" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "in" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "mg/l" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "mg/sec" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "mmhos/cm" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "mosm/kg" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "mV" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "None" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "NTU" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "nu" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "ppm" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "ppth" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "ug/l" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "umho" |
           sw.wqp_data$ResultMeasure.MeasureUnitCode == "volts")

index[is.na(index)] <- FALSE
sw.wqp_data <- sw.wqp_data[!index,]

# convert unit
index = sw.wqp_data$ResultMeasure.MeasureUnitCode == "mS/cm"
index[is.na(index)] <- FALSE
sw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"
sw.wqp_data[index, "ResultMeasureValue"] <- sw.wqp_data[index, "ResultMeasureValue"] * 1000

index = sw.wqp_data$ResultMeasure.MeasureUnitCode %in% c("umho/cm","uS/cm @25C")
index[is.na(index)] <- FALSE
sw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"

index = sw.wqp_data$ResultMeasure.MeasureUnitCode == "mho/cm"
index[is.na(index)] <- FALSE
sw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"
sw.wqp_data[index, "ResultMeasureValue"] <- sw.wqp_data[index, "ResultMeasureValue"] * 1000000

index = sw.wqp_data$ResultMeasure.MeasureUnitCode == "S/m"
index[is.na(index)] <- FALSE
sw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"
sw.wqp_data[index, "ResultMeasureValue"] <- sw.wqp_data[index, "ResultMeasureValue"] * 10000

# replacing nc with 0 and lt with 1 in censorcode column
index = sw.wqp_data$censorcode == "nc"
sw.wqp_data[index, "censorcode"] <- 0

index = sw.wqp_data$censorcode == "lt"
sw.wqp_data[index, "censorcode"] <- 1

sw.wqp_data$censorcode <- as.numeric(sw.wqp_data$censorcode)

# save clean results to rds file
colnames(sw.wqp_data)
sw.wqp_data <- sw.wqp_data %>%
  # filter out Canal or floodwater or ditch type sites
  filter(!MonitoringLocationTypeName %in% c("Canal Drainage","Canal Irrigation",
                                            "Canal Transport","Floodwater non-Urban",
                                            "Floodwater Urban",
                                            "Pipe, Unspecified Source",
                                            "Stream: Canal",
                                            "Stream: Ditch")) %>%
  select(c("MonitoringLocationIdentifier",
           "LatitudeMeasure", "LongitudeMeasure",
           "StateName", "COUNTY_NAME",
           "samplingdate", "ResultMeasureValue")) %>%
  rename(SiteCode = MonitoringLocationIdentifier, Latitude = LatitudeMeasure,
         Longitude = LongitudeMeasure, State = StateName, County = COUNTY_NAME,
         Samplingdate = samplingdate, DataValue = ResultMeasureValue) %>%
  # derive the day of sampling date
  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))


saveRDS(sw.wqp_data, file="./input/water_chemistry/sw_results_curated_sc_us.rds", compress = FALSE)

#####
#Load data
sw.wqp_data = readRDS("./input/water_chemistry/sw_results_curated_sc_us.rds")

# Plotting temporal histogram
sw_sc_hist <- ggplot(sw.wqp_data, aes(samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("SW Specific Conductance") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
sw_sc_hist
ggsave("./output/figures/sw_sc_hist.png")

# Generate list of sites reporting SC data
sw.all.sites <- sw.wqp_data[!duplicated(sw.wqp_data$MonitoringLocationIdentifier)
                            ,c("MonitoringLocationIdentifier", "LatitudeMeasure",
                               "LongitudeMeasure")]



#### WQP Groundwater Chemistry Downloading and cleaning ####
# generate the list of states FIPS on the continental U.S.
state.fips <- data.frame(name=state.name, fips=fips(state.name))
state.fips <- state.fips[!(state.fips$name %in% c("Hawaii", "Alaska")),]
state.fips$US_fips <- paste("US:",state.fips$fips,sep = "")

# download the list of sites reporting SC data state by state
analyte.names = c("Specific conductance","Specific conductivity")
sampleMedia = c("water","Water")
gw.siteType = c("Aggregate groundwater use","Aggregate groundwater use ",
                "Spring","Subsurface","Well")
gw.all.sites <- data.frame()
for (state.code in state.fips$US_fips) {
  gw.sites <- whatWQPsites(characteristicName = analyte.names,
                           sampleMedia = sampleMedia,
                           siteType = gw.siteType,
                           statecode = state.code)
  gw.all.sites <- rbind(gw.all.sites, gw.sites)
}

gw.all.sites$StateCode <- as.character(gw.all.sites$StateCode)
gw.all.sites$CountyCode <- formatC(gw.all.sites$CountyCode, width = 3,
                                   format='d', flag = '0')
gw.all.sites <- merge(x = gw.all.sites, y = state.fips,
                      by.x="StateCode", by.y="fips", all.x = TRUE)
gw.all.sites <- merge(x = gw.all.sites, y=countyCd,
                      by.x=c("StateCode","CountyCode"),
                      by.y=c("STATE","COUNTY"),all.x = TRUE)
colnames(gw.all.sites)[37] <- "StateName"

# save the downloaded list of sites into rds file
saveRDS(gw.all.sites, file="./input/water_chemistry/gw_sites_sc_us.rds", compress = FALSE)

# Download SC data from the above monitoring sites
# Water Quality Portal website using this link:
# https://www.waterqualitydata.us/data/Result/search?countrycode=US&siteType=Aggregate%20groundwater%20use&siteType=Aggregate%20groundwater%20use%20&siteType=Spring&siteType=Subsurface&siteType=Well&sampleMedia=water&sampleMedia=Water&characteristicName=Specific%20conductance&characteristicName=Specific%20Conductance%2C%20Calculated%2FMeasured%20Ratio&characteristicName=Specific%20conductivity&characteristicName=Specific%20conductivity***retired***use%20Specific%20conductance&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET

####  Download SC data from the above monitoring sites
gw.all.results <- data.frame()
for (state.code in state.fips$US_fips) {
  gw.results <- readWQPdata(characteristicName = analyte.names,
                            sampleMedia = sampleMedia,
                            siteType = gw.siteType,
                            statecode = state.code,
                            Zip = "no")
  gw.all.results <- rbind(gw.all.results, gw.results)
}

# load data downloaded from WQP website via the link above
gw.all.results <- read.csv(file = "./input/water_chemistry/gw_results_sc_us.csv", stringsAsFactors = FALSE, na.strings = "")
gw.all.results$ActivityStartDateTime <- NA
gw.all.results$ActivityEndDateTime <- NA

# save downloaded SC results into rds file
saveRDS(gw.all.results, file="./input/water_chemistry/gw_results_sc_us.rds", compress = FALSE)

# further subset by ActivityMediaSubdivisionName to keep only "NA", "Groundwater"
# "Water", and "Ground Water". Then save to a rds file
gw.all.results <- gw.all.results[gw.all.results$ActivityMediaSubdivisionName %in% c(NA,"Groundwater", "Water", "Ground Water"),]

saveRDS(gw.all.results, file="./input/water_chemistry/gw_results_sc_us_ActivityMediaSubdivision.rds", compress = FALSE)

# merge all.sites and all.results
gw.all.sites.reduced <- gw.all.sites[, c("MonitoringLocationIdentifier",
                                         "MonitoringLocationTypeName",
                                         "LatitudeMeasure", "LongitudeMeasure",
                                         "StateName", "COUNTY_NAME")]
gw.wqp_data <- merge(x = gw.all.results, y = gw.all.sites.reduced,
                  by = "MonitoringLocationIdentifier", all.x = TRUE)



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
            gw.wqp_data$ResultDetectionConditionText == "*Not Reported" |
            gw.wqp_data$ResultDetectionConditionText == "Not Present" |
            gw.wqp_data$ResultDetectionConditionText == "*Present <QL" |
            gw.wqp_data$ResultDetectionConditionText == "Between Inst Detect and Quant Limit")
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

index = (gw.wqp_data$ResultMeasure.MeasureUnitCode == "ft" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "cfs" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "deg C" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "in" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mg/l" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mg/sec" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mmhos/cm" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mosm/kg" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "mV" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "None" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "NTU" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "nu" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "ppm" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "ppth" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "ug/l" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "umho" |
           gw.wqp_data$ResultMeasure.MeasureUnitCode == "volts")

index[is.na(index)] <- FALSE
gw.wqp_data <- gw.wqp_data[!index,]

# convert unit
index = gw.wqp_data$ResultMeasure.MeasureUnitCode == "mS/cm"
index[is.na(index)] <- FALSE
gw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"
gw.wqp_data[index, "ResultMeasureValue"] <- gw.wqp_data[index, "ResultMeasureValue"] * 1000

index = gw.wqp_data$ResultMeasure.MeasureUnitCode %in% c("umho/cm","uS/cm @25C")
index[is.na(index)] <- FALSE
gw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"

index = gw.wqp_data$ResultMeasure.MeasureUnitCode == "mho/cm"
index[is.na(index)] <- FALSE
gw.wqp_data[index, "ResultMeasure.MeasureUnitCode"] = "uS/cm"
gw.wqp_data[index, "ResultMeasureValue"] <- gw.wqp_data[index, "ResultMeasureValue"] * 1000000

# replacing nc with 0 and lt with 1 in censorcode column
index = gw.wqp_data$censorcode == "nc"
gw.wqp_data[index, "censorcode"] <- 0

index = gw.wqp_data$censorcode == "lt"
gw.wqp_data[index, "censorcode"] <- 1

gw.wqp_data$censorcode <- as.numeric(gw.wqp_data$censorcode)


# save clean results to rds file
colnames(gw.wqp_data)
gw.wqp_data.test <- gw.wqp_data %>%
  select(c("MonitoringLocationIdentifier",
           "LatitudeMeasure", "LongitudeMeasure",
           "StateName", "COUNTY_NAME",
           "samplingdate", "ResultMeasureValue")) %>%
  rename(SiteCode = MonitoringLocationIdentifier, Latitude = LatitudeMeasure,
         Longitude = LongitudeMeasure, State = StateName, County = COUNTY_NAME,
         Samplingdate = samplingdate, DataValue = ResultMeasureValue) %>%
  # derive the day of sampling date
  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate) %>%
  summarise(daily_sc = mean(DataValue)) %>%
  mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))

saveRDS(gw.wqp_data, file="./input/water_chemistry/gw_results_curated_sc_us.rds", compress = FALSE)

# Plotting temporal histogram
gw_sc_hist <- ggplot(gw.wqp_data, aes(samplingdate)) +
  geom_histogram() + theme_bw() + xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2000-01-01")),
             linetype = 4, colour="red") +
  ggtitle("GW Specific Conductance") +
  theme(text = element_text(size = 20)) +
  scale_x_datetime(breaks = date_breaks("10 years"),
                   labels = date_format("%Y"),
                   limits = c(as.POSIXct("1920-01-01"),
                              as.POSIXct("2022-12-31")))
gw_sc_hist
#ggsave("./output/figures/gw_sc_hist.png")

#####
#Load cleaned groundwater data
gw.wqp_data = readRDS("./input/water_chemistry/gw_results_curated_sc_us.rds")
gw.all.sites = readRDS("./input/water_chemistry/gw_sites_sc_us.rds")

gw.wqp_data$samplingdate <- as.POSIXct(gw.wqp_data$ActivityStartDate,
                                       format="%Y-%m-%d")


# Generate list of sites reporting SC data
#gw.all.sites <- gw.wqp_data[!duplicated(gw.wqp_data$MonitoringLocationIdentifier)
#                            ,c("MonitoringLocationIdentifier", "LatitudeMeasure",
#                               "LongitudeMeasure")]

#rm(list = ls())
#gc()

# Load curated datasets
#gw.wqp_data <- readRDS("./input/water_chemistry/gw_results_curated_sc_us.rds")
sw.wqp_data <- readRDS("./input/water_chemistry/sw_results_curated_sc_us.rds")



######################################################################################

#### Shale Network SC Data ####
sn_sc_datavalue <- read.csv(file="./input/SN/SN_SC_071022.csv",
                            na.strings = "", stringsAsFactors = FALSE)

sn_sc_unit <- read.csv(file="./input/SN/SN_SC_unit_071022.csv",
                       na.strings = "", stringsAsFactors = FALSE)

sn_sc_raw <- merge(x=sn_sc_datavalue, y=sn_sc_unit, all.x=TRUE,
                   by.x="VariableUnitsID", by.y="UnitsID")

sn_sc_curated<- sn_sc_raw %>%
  # remove production and flowback water
  # assume all "Unknown" == "Surface Water"
  filter(!(SampleMedium %in% c("Flowback water",
                                           "Flowback Water",
                                           "Injection Water",
                                           "Production water"))) %>%
  mutate(SampleMedium = replace(SampleMedium, SampleMedium == "Unknown",
                                "Surface Water")) %>%
  ## convert ms/cm to uS/cm
  mutate(DataValue = ifelse(UnitsAbbreviation == "mS/cm",
                            DataValue * 1000, DataValue)) %>%
  mutate(UnitsAbbreviation = replace(UnitsAbbreviation,
                                     UnitsAbbreviation == "mS/cm",
                                     "uS/cm")) %>%
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

# extract groundwater and surface water samples
gw.sn_sc_curated <- sn_sc_curated %>%
  filter(SampleMedium == "Groundwater") %>%
  select(-SampleMedium)

sw.sn_sc_curated <- sn_sc_curated %>%
  filter(!SampleMedium == "Surface Water") %>%
  select(-SampleMedium)

saveRDS(gw.sn_sc_curated, file="./input/SN/gw_sn_sc_curated.rds",
        compress = FALSE)
saveRDS(sw.sn_sc_curated, file="./input/SN/sw_sn_sc_curated.rds",
        compress = FALSE)

#rm(list=ls())
gc()

#####
gw.sn_sc_curated = readRDS("./input/SN/gw_sn_sc_curated.rds")
sw.sn_sc_curated = readRDS("./input/SN/sw_sn_sc_curated.rds")

######################################################################################
#### COGCC SC Data ####
cogcc_wq_raw <- readxl::read_xlsx(path="./input/COGCC_WQ/COGCC_BasicQuery.xlsx",
                                  na="") %>% janitor::clean_names()

cogcc_wq_curated <- cogcc_wq_raw %>%
  # select only non-NA SC data from relevant surface water and
  # groundwater samples
  filter(param_description %in%
           c("SPECIFIC CONDUCTIVITY", "SPECIFIC CONDUCTIVITY, FIELD"),
         facility_type %in%
           c("Creek","Domestic Well", "Ground Water", "Groundwater",
             "Monitoring Well", "Pond", "River", "Seep", "Spring",
             "Surface Water"),
         !matrix %in% c("GAS", "LIQUID", "SOIL"),
         !is.na(result_value),
         !units %in% c("mg/L","SU")) %>%
  # convert all units to us/cm
  mutate(result_value = ifelse(units %in% c("mS/cm","dS/m","mmhos/cm"),
                               result_value * 1000, result_value)) %>%
  mutate(units = "uS/cm") %>%
  # add a column 'State'
  mutate(State="Colorado") %>%
  # select only needed columns and rename them in a way consistent with others
  select(c(facility_id, facility_type, latitude83, longitude83, State,
           county,sample_date,result_value)) %>%
  rename(SiteCode = facility_id, Latitude = latitude83, Longitude = longitude83,
         County = county, Samplingdate = sample_date,
         SampleMedium = facility_type, DataValue = result_value) %>%
  # derive the day of sampling date
  mutate(Samplingdate = floor_date(Samplingdate, "day")) %>%
  # calculate site-level daily average
  group_by(SiteCode, Latitude, Longitude, State,
           County, Samplingdate, SampleMedium) %>%
  summarise(daily_sc = mean(DataValue))

# extract groundwater and surface water samples
gw.cogcc_wq_curated <- cogcc_wq_curated %>%
  filter(SampleMedium %in% c("Domestic Well","Ground Water",
                             "Groundwater","Monitoring Well",
                             "Seep","Spring")) %>%
  select(-SampleMedium)

sw.cogcc_wq_curated <- cogcc_wq_curated %>%
  filter(SampleMedium %in% c("Creek","Pond","River","Surface Water")) %>%
  select(-SampleMedium)

saveRDS(gw.cogcc_wq_curated, file="./input/COGCC_WQ/gw_cogcc_wq_curated.rds",
        compress = FALSE)
saveRDS(sw.cogcc_wq_curated, file="./input/COGCC_WQ/sw_cogcc_wq_curated.rds",
        compress = FALSE)

#rm(list=ls())
gc()
#####
gw.cogcc_wq_curated = readRDS("./input/COGCC_WQ/gw_cogcc_wq_curated.rds")
sw.cogcc_wq_curated = readRDS("./input/COGCC_WQ/sw_cogcc_wq_curated.rds")


###########################################################################
#### TWDB Groundwater Quality SC Data ####
# read curated file (curated in Ba and Sr source codes)
wqdetail <-  readRDS(file="./input/TWDB/gw_twdb_curated_v1.rds")
lu_param <- read.table(file="./input/TWDB/GWDBDownloadSQL/GWDBDownloadSQL/LU_WaterQualityParameters.txt", sep="|", dec=".", header = TRUE, fill = TRUE)

#extract Ba and Sr data from the master table
# SC parameters codes are 94 and 95. All values are in UMHOS/CM = uS/cm
gw.twdb.sc <- wqdetail %>%
  filter(WaterQualityParameterId %in% c(94, 95)) %>%
  #format sampledate
  mutate(SampleDate = 
           floor_date(as.POSIXct(SampleDate, format="%Y-%m-%d"), "day")) %>%
  #remove not-detected value while retaining less than values as no DL
  # available for not-detected value
  filter(!Description == "not detected" | is.na(Description)) %>%
  # drop columns not needed
  select(-c("WaterQualityDetailId", "WaterQualityHeaderId", 
            "WaterQualityParameterId", "ParameterValueFlagId",
            "Description", "ParameterUnitOfMeasure",
            "ParameterLongDescription")) %>%
  #calculate daily average
  group_by(StateWellId, CoordDDLat, CoordDDLong, 
           CountyNameMixed, State, SampleDate) %>%
  summarise(daily_sc = mean(ParameterValue)) %>%
  rename(SiteCode = StateWellId, Latitude = CoordDDLat, Longitude = CoordDDLong,
         County = CountyNameMixed, Samplingdate = SampleDate)

#save to rds file
saveRDS(gw.twdb.sc, file="./input/TWDB/gw_twdb_curated_sc.rds", compress = FALSE)

rm(list=ls())
gc()
#####
gw.twdb.sc = readRDS("./input/TWDB/gw_twdb_curated_sc.rds")




