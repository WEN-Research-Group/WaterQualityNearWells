Well_spatial_analysis_compl_0127 = function(Texas_marginal_short,SW_Na_Texas_merged){
  
  #Texas_marginal_short - well dataset: unique wells and their location info
  #SW_Na_Texas_merged - water quality dataset: sampling locations and detected concentrations
  
  # extract unique lat/long combinations
  marginal.sites <- Texas_marginal_short[!duplicated(Texas_marginal_short[c("Longitude","Latitude")]),] #a lot of columns
  sw.wq.sites <- SW_Na_Texas_merged[!duplicated(SW_Na_Texas_merged[c("Longitude","Latitude")]),]
  
  # convert to spatial dataset
  sw.all.coords <- SpatialPoints(sw.wq.sites[,c("Longitude","Latitude")])
  marg.all.coords <- SpatialPoints(marginal.sites[,c("Longitude","Latitude")])
  
  crs.geo<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  proj4string(sw.all.coords) <-crs.geo
  proj4string(marg.all.coords) <-crs.geo
  
  # project all spatial layers to NAD83 for distance calculation
  sw.all.coords <- spTransform(sw.all.coords, CRS("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  marg.all.coords <- spTransform(marg.all.coords, CRS("+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  #Define new variables
  well.all.coords = marg.all.coords
  Well_data = Texas_marginal_short
  water_data = SW_Na_Texas_merged
  
  sw.nearest.well.all = get.knnx(coordinates(well.all.coords), coordinates(sw.all.coords),k=1000)
  
  # Relationship between distance / density and water chemistry for groundwater samples
  # distance vs. water chemistry
  sw.nearest.well.all.distance <- as.data.frame(sw.nearest.well.all$nn.dist)
  sw.nearest.well.all.index <- as.data.frame(sw.nearest.well.all$nn.index)
  
  sw.nearest.well.all.distance$index <- rownames(sw.nearest.well.all.distance)
  sw.nearest.well.all.index$index <- rownames(sw.nearest.well.all.index)
  sw.wq.sites$index <- rownames(sw.wq.sites)
  
  sw.wq.distance <- merge(water_data, sw.wq.sites[,c("Latitude","Longitude","index")],                        
                          by=c("Latitude","Longitude"),
                          all.x=TRUE)
  
  sw.wq.distance2 <- merge(water_data, sw.wq.sites[,c("Latitude","Longitude","index")],                        
                          by=c("Latitude","Longitude"),
                          all.x=TRUE)
  
  sw.nearest.well.all.date <- data.frame(matrix(0,nrow = dim(sw.nearest.well.all.index)[1],ncol = dim(sw.nearest.well.all.index)[2]))
  for (i in 1:(ncol(sw.nearest.well.all.index)-1)) {
    sw.nearest.well.all.date[,i] <- Well_data[sw.nearest.well.all.index[,i],"SpudDate"]
  }
  
  sw.nearest.well.all.date$index <- sw.nearest.well.all.distance$index
  
  sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.date,
                          by="index", all.x=TRUE)
  sw.wq.distance <- merge(sw.wq.distance,sw.nearest.well.all.distance,
                          by="index", all.x=TRUE)
  
  
  a1=which(colnames(sw.wq.distance)=="X1")
  a2=which(colnames(sw.wq.distance)=="X1000")
  
  #creating a matrix that finds first FALSE, which is the closest well that was built before sampling event
  sw.nearest.well.all.boolean <- as.data.frame(sw.wq.distance[,rep("Samplingdate",1000)] < sw.wq.distance[,a1:a2])
  sw.nearest.well.all.boolean2 <- as.data.frame(sw.wq.distance[,rep("Samplingdate",1000)] > sw.wq.distance[,a1:a2])
  sw.nearest.well.all.boolean$nearest_well_index = as.data.frame(apply(sw.nearest.well.all.boolean, 1, which.min))
  
  a3=which(colnames(sw.wq.distance)=="V1") -1
  # extract the distance to the nearest oil and gas well that is drilled prior to gw sampling
  for (i in 1:nrow(sw.wq.distance)) {
    sw.wq.distance$nearest_distance_m[i] = 
      sw.wq.distance[i,as.integer(sw.nearest.well.all.boolean[i, "nearest_well_index"])+a3]
  }
  
  #Keep only distances from wells that were built before sampling date
  #  sw.nearest.well.all.distance_w_NA = sw.nearest.well.all.distance
  b1 = which(colnames(sw.wq.distance)=="V1")
  b2 = which(colnames(sw.wq.distance)=="V1000")
  sw.nearest.well.all.distance_w_NA = sw.wq.distance[,b1:b2]
  for (i in seq(1:1000)){
    #TF_ = sw.nearest.well.all.boolean2[,i]
    TF_ = sw.nearest.well.all.boolean[,i]
    sw.nearest.well.all.distance_w_NA[TF_==TRUE,i] = NA
  }
  
  # 1km well density
  sw.nearest.well.all.1km.boolean <- as.data.frame(sw.nearest.well.all.distance[,1:1000] < 1000)
  
  sw.nearest.well.all.1km.boolean$index <- rownames(sw.nearest.well.all.1km.boolean)
  
  sw.nearest.well.all.1km.boolean <- merge(data.frame(index=sw.wq.distance[,1]),
                                           sw.nearest.well.all.1km.boolean,
                                           by="index",
                                           all.x=TRUE)
  
  sw.nearest.well.all.1km.boolean2 <- 
    select(sw.nearest.well.all.1km.boolean,-index) * sw.nearest.well.all.boolean2
  
  # 3km well density
  sw.nearest.well.all.3km.boolean <- as.data.frame(sw.nearest.well.all.distance[,1:1000] < 3000)
  
  sw.nearest.well.all.3km.boolean$index <- rownames(sw.nearest.well.all.3km.boolean)
  
  sw.nearest.well.all.3km.boolean <- merge(data.frame(index=sw.wq.distance[,1]),
                                           sw.nearest.well.all.3km.boolean,
                                           by="index",
                                           all.x=TRUE)
  sw.nearest.well.all.3km.boolean2 <- 
    select(sw.nearest.well.all.3km.boolean,-index) * sw.nearest.well.all.boolean2

  
  # calculate # of OG wels within buffer distance
  sw.wq.distance$num_well_1km <- rowSums(sw.nearest.well.all.1km.boolean2)
  sw.wq.distance$num_well_3km <- rowSums(sw.nearest.well.all.3km.boolean2)
  
  #Sum distance of #k closest wells
  k = 10
  
  #Keep only distances from wells that were built before sampling date
  #  sw.nearest.well.all.distance_w_NA = sw.nearest.well.all.distance
  b1 = which(colnames(sw.wq.distance)=="V1")
  b2 = which(colnames(sw.wq.distance)=="V1000")
  sw.nearest.well.all.distance_w_NA = sw.wq.distance[,b1:b2]
  for (i in seq(1:1000)){
    TF_ = sw.nearest.well.all.boolean[,i]
    sw.nearest.well.all.distance_w_NA[TF_==TRUE,i] = NA
  }
  
  c_fun = function(sw.nearest.well.all.distance_w_NA,output){
    return(sw.nearest.well.all.distance_w_NA[!is.na(sw.nearest.well.all.distance_w_NA)][1:k])
  }
  sw.nearest.well.all.distance_w_NA_10 = t(apply(sw.nearest.well.all.distance_w_NA, 1, c_fun))
  
  sw.wq.distance$closest_well_dist_sum = apply(sw.nearest.well.all.distance_w_NA_10, 1, sum)
  
  
  return(sw.wq.distance)
}
