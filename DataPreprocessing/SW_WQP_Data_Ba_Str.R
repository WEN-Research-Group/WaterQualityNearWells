#USGS WQP data download 

#### BARIUM
# download the list of sites reporting SC data state by state
analyte.names = c("Barium")
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
#Double check which column it is
colnames(sw.all.sites)[38] <- "StateName"

# save the downloaded list of sites into RDS file
saveRDS(sw.all.sites, file="./input/sw_sites_barium_WQP.rds", compress = FALSE)

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
saveRDS(sw.all.results, file="./input/sw_results_barium_WQP.rds", compress = FALSE)

# merge all.sites and all.results
sw.wqp_data_bar <- merge(x = sw.all.results, y = sw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

# save downloaded SC results into RDS file
saveRDS(sw.wqp_data_bar, file="./input/sw_results_all_info_barium.rds", compress = FALSE)



#### STRONTIUM
# download the list of sites reporting SC data state by state
analyte.names = c("Strontium")
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
#Double check which column it is
colnames(sw.all.sites)[38] <- "StateName"

# save the downloaded list of sites into RDS file
saveRDS(sw.all.sites, file="./input/sw_sites_strontium_WQP.rds", compress = FALSE)

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
saveRDS(sw.all.results, file="./input/sw_results_strontium_WQP.rds", compress = FALSE)

# merge all.sites and all.results
sw.wqp_data_str <- merge(x = sw.all.results, y = sw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

# save downloaded SC results into RDS file
saveRDS(sw.wqp_data_str, file="./input/sw_results_all_info_strontium.rds", compress = FALSE)


#### CLEAN DATA
##### Barium ####
SW_dataset = sw.wqp_data_bar

SW_dataset_cat = SW_dataset %>%
  group_by(ActivityMediaSubdivisionName) %>%
  summarise(n =n())

#Remove all samples hat come from non-surface water
SW_dataset_subd = filter(SW_dataset, ActivityMediaSubdivisionName == "Surface Water")

#Remove NAs
SW_dataset_subd = filter(SW_dataset_subd, !is.na(ResultMeasureValue))
SW_dataset_subd = filter(SW_dataset_subd, !is.na(ResultMeasure.MeasureUnitCode))
SW_dataset_subd$samplingdate <- as.POSIXct(SW_dataset_subd$ActivityStartDate, format="%Y-%m-%d")
SW_dataset_subd <- SW_dataset_subd[!is.na(SW_dataset_subd$samplingdate),]
SW_dataset_subd = filter(SW_dataset_subd, ResultStatusIdentifier != "Rejected")

#Remove systematic contamination samples
SW_dataset_subd = SW_dataset_subd[-which(SW_dataset_subd$ResultDetectionConditionText == "Systematic Contamination"),]

SW_dataset_subd$ResultMeasureValue = as.numeric(SW_dataset_subd$ResultMeasureValue)
SW_dataset_subd = filter(SW_dataset_subd, !is.na(ResultMeasureValue))
SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureValue = as.numeric(SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureValue)

ind = which(SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureUnitCode == SW_dataset_subd$ResultMeasure.MeasureUnitCode &
               SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureValue > SW_dataset_subd$ResultMeasureValue)
SW_dataset_subd_below_dec = SW_dataset_subd[ind,]
SW_dataset_subd = SW_dataset_subd[-ind,]

#Fix units
unique(SW_dataset_subd$ResultMeasure.MeasureUnitCode)
SW_dataset_subd = SW_dataset_subd[-which(SW_dataset_subd$ResultMeasure.MeasureUnitCode == "mg/kg"),]
ind = which(SW_dataset_subd$ResultMeasure.MeasureUnitCode %in% c("mg/L","mg/l","ppm"))
#SW_dataset_subd$ResultMeasureValue = as.numeric(SW_dataset_subd$ResultMeasureValue)
SW_dataset_subd[ind,"ResultMeasureValue"] = SW_dataset_subd[ind,"ResultMeasureValue"]*1000
#change units
SW_dataset_subd[which(SW_dataset_subd$ResultMeasure.MeasureUnitCode %in% c("mg/L","mg/l","ppm","ug/l")),"ResultMeasure.MeasureUnitCode"] = "ug/L"
#SW_dataset_subd = filter(SW_dataset_subd, !is.na(ResultMeasureValue))

#Save cleaned data
saveRDS(SW_dataset_subd, file="./input/sw_clean_results_all_info_barium.rds", compress = FALSE)

SW_dataset_subd_cat = SW_dataset_subd %>%
  group_by(MonitoringLocationTypeName) %>%
  summarise(n =n())

#Filter samples from river/steams
SW_dataset_subd_streams = SW_dataset_subd %>%
  filter(MonitoringLocationTypeName %in% c("Stream","Channelized Stream", "River/Stream", "Stream: Canal"))

#Save cleaned data
saveRDS(SW_dataset_subd_streams, file="./input/sw_clean_results_stream_all_info_barium.rds", compress = FALSE)


##### Strontium ####
SW_dataset = sw.wqp_data_str

#Remove all samples hat come from non-surface water
SW_dataset_subd = filter(SW_dataset, ActivityMediaSubdivisionName == "Surface Water")

#Remove NAs
SW_dataset_subd = filter(SW_dataset_subd, !is.na(ResultMeasure.MeasureUnitCode))
SW_dataset_subd$samplingdate <- as.POSIXct(SW_dataset_subd$ActivityStartDate, format="%Y-%m-%d")
SW_dataset_subd <- SW_dataset_subd[!is.na(SW_dataset_subd$samplingdate),]
SW_dataset_subd = filter(SW_dataset_subd, ResultStatusIdentifier != "Rejected")

#Remove flagged values that are below detection limit
SW_dataset_subd = SW_dataset_subd[-which(SW_dataset_subd$ResultDetectionConditionText == "Not Detected" |
                                           SW_dataset_subd$ResultDetectionConditionText == "Between Inst Detect and Quant Limit"),]

SW_dataset_subd$ResultMeasureValue = as.numeric(SW_dataset_subd$ResultMeasureValue)
SW_dataset_subd = filter(SW_dataset_subd, !is.na(ResultMeasureValue))
SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureValue = as.numeric(SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureValue)

ind = which(SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureUnitCode == SW_dataset_subd$ResultMeasure.MeasureUnitCode &
              SW_dataset_subd$DetectionQuantitationLimitMeasure.MeasureValue > SW_dataset_subd$ResultMeasureValue)
SW_dataset_subd_below_dec = SW_dataset_subd[ind,]
SW_dataset_subd = SW_dataset_subd[-ind,]

#Fix units
unique(SW_dataset_subd$ResultMeasure.MeasureUnitCode)
SW_dataset_subd = SW_dataset_subd[-which(SW_dataset_subd$ResultMeasure.MeasureUnitCode == "mg/kg"),]
ind = which(SW_dataset_subd$ResultMeasure.MeasureUnitCode %in% c("mg/L","mg/l"))
SW_dataset_subd[ind,"ResultMeasureValue"] = SW_dataset_subd[ind,"ResultMeasureValue"]*1000
#change units
SW_dataset_subd[which(SW_dataset_subd$ResultMeasure.MeasureUnitCode %in% c("mg/L","mg/l","ug/l")),"ResultMeasure.MeasureUnitCode"] = "ug/L"

#Save cleaned data
saveRDS(SW_dataset_subd, file="./input/sw_clean_results_all_info_strontium.rds", compress = FALSE)

SW_dataset_subd_cat = SW_dataset_subd %>%
  group_by(MonitoringLocationTypeName) %>%
  summarise(n =n())

#Filter samples from river/steams
SW_dataset_subd_streams = SW_dataset_subd %>%
  filter(MonitoringLocationTypeName %in% c("Stream","Channelized Stream", "River/Stream", "Stream: Canal"))

#Save cleaned data
saveRDS(SW_dataset_subd_streams, file="./input/sw_clean_results_stream_all_info_strontium.rds", compress = FALSE)
