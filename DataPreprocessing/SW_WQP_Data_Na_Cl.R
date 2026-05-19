#USGS WQP data download 

library(usmap)

# generate the list of states FIPS on the continental U.S.
state.fips <- data.frame(name=state.name, fips=fips(state.name))
state.fips <- state.fips[!(state.fips$name %in% c("Hawaii", "Alaska")),]
state.fips$US_fips <- paste("US:",state.fips$fips,sep = "")

#### CHLORIDE
# download the list of sites reporting SC data state by state
analyte.names = c("Chloride")
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
saveRDS(sw.all.sites, file="./input/sw_sites_chloride_WQP.rds", compress = FALSE)

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
saveRDS(sw.all.results, file="./input/sw_results_chloride_WQP.rds", compress = FALSE)

# merge all.sites and all.results
sw.wqp_data_cl <- merge(x = sw.all.results, y = sw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

# save downloaded SC results into RDS file
saveRDS(sw.wqp_data_cl, file="./input/sw_results_all_info_chloride.rds", compress = FALSE)



#### SODIUM
# download the list of sites reporting SC data state by state
analyte.names = c("Sodium")
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
saveRDS(sw.all.sites, file="./input/sw_sites_sodium_WQP.rds", compress = FALSE)

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
saveRDS(sw.all.results, file="./input/sw_results_sodium_WQP.rds", compress = FALSE)

# merge all.sites and all.results
sw.wqp_data_na <- merge(x = sw.all.results, y = sw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

# save downloaded SC results into RDS file
saveRDS(sw.wqp_data_na, file="./input/sw_results_all_info_sodium.rds", compress = FALSE)

### Try downloading state by state
#st = "US:08" # Colorado
# New York US:36
# Pennsylvania US:42
# Texas US:48
sw.all.sites <- data.frame()
states_needed = c("US:08","US:36","US:42","US:48")
for (state.co in states_needed){
  sites <- whatWQPsites(characteristicName = analyte.names,
                           sampleMedia = sampleMedia,
                           siteType = siteType,
                           statecode = state.co)
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

#Find which column needs to be changed
colnames(sw.all.sites)[which(colnames(sw.all.sites) == 'name')] <- "StateName"

# save the downloaded list of sites into RDS file
saveRDS(sw.all.sites, file="./input/sw_sites_sodium_WQP.rds", compress = FALSE)


# Download  data from the above monitoring sites
sw.all.results <- data.frame()
for (state.code in states_needed) {
  results <- readWQPdata(characteristicName = analyte.names,
                         sampleMedia = sampleMedia,
                         siteType = siteType,
                         statecode = state.code,
                         Zip = "no")
  sw.all.results <- rbind(sw.all.results, results)
}
# save downloaded SC results into RDS file
saveRDS(sw.all.results, file="./input/sw_results_sodium_WQP.rds", compress = FALSE)

# merge all.sites and all.results
sw.wqp_data_na <- merge(x = sw.all.results, y = sw.all.sites,
                        by = "MonitoringLocationIdentifier", all.x = TRUE)

# save downloaded SC results into RDS file
saveRDS(sw.wqp_data_na, file="./input/sw_results_all_info_sodium.rds", compress = FALSE)
