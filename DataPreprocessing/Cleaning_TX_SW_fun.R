Cleaning_TX_SW_fun = function(Data_f,Station_info){
  
  #1) Values below detection limit
  #Remove Values that are flagged "<"
  #Make sure it has that value, or else matrix will become empty
  N = which(Data_f$Greater.Than.Less.Than == "<")
  if (length(N)>0){
    Data_f = Data_f[-which(Data_f$Greater.Than.Less.Than == "<"),]
  }
  
  #2) Remove NA sample values if there are any
  Data_f = filter(Data_f,!is.na(as.numeric(Value)))
  
  #3) Convert date format
  Data_f$Samplingdate = as.POSIXct(Data_f$End.Date,  format="%m/%d/%y")
  
  #4) Merge with site info
  data_all = data.frame()
  data_all = merge(Data_f,Station_info, by.x = "Station.ID", by.y = "STATION_ID")
  data_all = merge(Data_f,TX_SW_sites, by.x = "Station.ID", by.y = "STATION_ID")
  
  #5) Check all site type
  #Keep only stream values
  data_all_stream = filter(data_all,EPA_TYPE1 == "STREAM")
  
  #6) Keeping only some of the columns
  data_all_short = data_all_stream %>%
    select(Station.ID, Station.Description, Parameter.Description, Value, Samplingdate, LAT_DD, LONG_DD,
           USGS_GAUGE, EPA_TYPE1, EPA_TYPE2, CNTY_NME) %>%
    mutate(LAT_DD = as.numeric(LAT_DD)) %>%
    mutate(LONG_DD = as.numeric(LONG_DD)) %>%
    mutate(Value = as.numeric(Value)) %>%
    rename(SiteCode = Station.ID, Latitude = LAT_DD,Longitude = LONG_DD, County = CNTY_NME, DataValue = Value,
           Station_name = Station.Description) 
  
    
  #7) Change USGS site number to match monitoring location ID format from USGS WQP
  for (i in seq(1:dim(data_all_short)[1])){
    if (nchar(data_all_short$USGS_GAUGE[i])>0){
      while (nchar(data_all_short$USGS_GAUGE[i])<8){
        data_all_short$USGS_GAUGE[i] = paste("0",data_all_stream$USGS_GAUGE[i],sep="")
      }
      data_all_short$USGS_GAUGE[i] = paste("USGS-",data_all_short$USGS_GAUGE[i],sep="")
    }
  }
  
  return(data_all_short)
  
}
