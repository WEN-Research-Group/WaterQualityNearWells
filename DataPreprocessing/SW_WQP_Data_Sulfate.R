#WATER QUALITY PORTAL
# download the list of sites reporting SC data state by state
analyte.names = c("Sulfate","Total Sulfate","Sulfate as S", "Sulfate as SO4", "Sulfur Sulfate")
sampleMedia = c("water","Water")
siteType = c("Aggregate surface-water-use","Stream", "Wetland",
             "Lake, Reservoir, Impoundment")
sw.all.sites_sulf <- data.frame()
for (state.code in state.fips$US_fips) {
  sites <- whatWQPsites(characteristicName = analyte.names,
                        sampleMedia = sampleMedia,
                        siteType = siteType,
                        statecode = state.code)
  sw.all.sites_sulf <- rbind(sw.all.sites_sulf, sites)
}

sw.all.sites = sw.all.sites_sulf
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
saveRDS(sw.all.sites, file="./input/sw_sites_sulfate_WQP.rds", compress = FALSE)

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
saveRDS(sw.all.results, file="./input/sw_results_sulfate_WQP.rds", compress = FALSE)

# merge all.sites and all.results
sw.wqp_data <- merge(x = sw.all.results, y = sw.all.sites,
                     by = "MonitoringLocationIdentifier", all.x = TRUE)

# save downloaded SC results into RDS file
saveRDS(sw.wqp_data, file="./input/sw_results_all_info_sulfate.rds", compress = FALSE)



############################################
#TEXAS STATE LEVEL DATABASE
TX_SW_Sulfate = read.csv("./input/Texas_surface_WQ/Sulfate/TX_SW_Sulfate.csv", header = TRUE)
#Find lat/long after I cleaned data

#Analyze units
unique(TX_SW_Sulfate$Parameter.Description)
#Two samples are missing first few column, remove them
TX_SW_Sulfate = filter(TX_SW_Sulfate, Parameter.Description == "SULFATE (MG/L AS SO4)")
#Identify values below detection limit 
unique(TX_SW_Sulfate$Greater.Than.Less.Than)
#Remove all values below detection limit 
TX_SW_Sulfate = filter(TX_SW_Sulfate, !Greater.Than.Less.Than == "<")
#Remove 0 values
TX_SW_Sulfate = filter(TX_SW_Sulfate, Value > 0)
 
#Change date format
TX_SW_Sulfate$End.Date = as.POSIXct(TX_SW_Sulfate$End.Date,format="%d/%m/%y")

#Save file
saveRDS(TX_SW_Sulfate, file="./input/Texas_surface_WQ/Sulfate/TX_SW_Sulfate_all_samples_no_cord.rds", compress = FALSE)

#Change units to ug/L from mg/L
TX_SW_Sulfate$Value = TX_SW_Sulfate$Value *1000

#Calculate daily values
TX_SW_Sulfate_daily = TX_SW_Sulfate %>%
  group_by(Segment, Station.ID, Station.Description, End.Date) %>%
  summarise(n = n(), daily_mean = mean(Value), daily_median = median(Value), 
            min_v = min(Value),max_v = max(Value))

TX_SW_Sulfate_daily = TX_SW_Sulfate_daily %>%
  rename(Samplingdate = End.Date, SiteCode = Station.ID, Station_name = Station.Description)


#Lat/long data
TX_SW_sites = read.csv("./input/Texas_surface_WQ/SWQM_Stations.csv", header = TRUE)

TX_SW_sites = TX_SW_sites %>%
  rename(SiteCode = STATION_ID, Latitude = LAT_DD, Longitude = LONG_DD)

TX_SW_Sulf_data = merge(TX_SW_Sulfate_daily, TX_SW_sites, by = "SiteCode")
TX_SW_Sulf_data_org = TX_SW_Sulf_data %>%
  arrange(daily_mean)

#Plot data
#Map 
pal <- colorNumeric(palette = "viridis", domain = TX_SW_Sulf_data_org$daily_mean)
leaflet(TX_SW_Sulf_data_org) %>% addTiles() %>%
  addCircles(lng = TX_SW_Sulf_data_org$Longitude,
             lat = TX_SW_Sulf_data_org$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#Identify sample types
TX_SW_Sulf_data_org_cat = TX_SW_Sulf_data_org %>%
  group_by(EPA_TYPE1) %>%
  summarise(n = n())
View(TX_SW_Sulf_data_org_cat)

#Keep only Stream samples
TX_SW_Sulf_data_org_streams = filter(TX_SW_Sulf_data_org, EPA_TYPE1 == "STREAM")

pal <- colorNumeric(palette = "viridis", domain = TX_SW_Sulf_data_org_streams$daily_mean)
leaflet(TX_SW_Sulf_data_org_streams) %>% addTiles() %>%
  addCircles(lng = TX_SW_Sulf_data_org_streams$Longitude,
             lat = TX_SW_Sulf_data_org_streams$Latitude,
             radius = 2,
             color = ~pal(daily_mean)) %>% #, popup = ~name) %>%
  addLegend(pal = pal, values = ~daily_mean, position = "bottomleft")

#Save daily sample values with coordinates 
saveRDS(TX_SW_Sulf_data_org, file="./input/Texas_surface_WQ/Sulfate/TX_SW_Sulfate_daily_all_categ.rds", compress = FALSE)
saveRDS(TX_SW_Sulf_data_org_streams, file="./input/Texas_surface_WQ/Sulfate/TX_SW_Sulfate_daily_streams.rds", compress = FALSE)

