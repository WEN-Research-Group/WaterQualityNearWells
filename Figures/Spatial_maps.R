#######################################
### PA water quality ###########
#Variables pulled from "Violin_plots.R"
#Geospatial variables pulled from Pennsylv_by_well_type_Ba_2.R
########################################

# Ba
P1_SW_long_corr = filter(P1_SW, Longitude > -80.5)
P1_GW_long_corr = filter(P1_GW, Longitude > -80.5)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap without Labels") %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap with Labels") %>%
  #addCircleMarkers(data = Geospatial_result_PA_Ba, lat = ~Latitude, lng = ~Longitude,
  addCircleMarkers(data = P1_SW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Ba SW") %>%
  addCircleMarkers(data = P1_GW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Ba GW") %>%
  # addCircleMarkers(data = another, lat = ~Latitude, lng = ~Longitude,
  #                  weight = 0.1, radius = 1, color="grey",stroke = TRUE,
  #                  opacity = 0.5, fill = TRUE,
  #                  group = group3) %>%
  addLayersControl(
    #baseGroups = c("Basemap without Labels", "Basemap with Labels"),
    overlayGroups = c("Ba SW", "Ba GW"),
    options = layersControlOptions(collapsed = FALSE))


#Cl 
P2_SW_long_corr = filter(P2_SW, Longitude > -80.5)
P2_GW_long_corr = filter(P2_GW, Longitude > -80.5)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap without Labels") %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap with Labels") %>%
  #addCircleMarkers(data = Geospatial_result_PA_Ba, lat = ~Latitude, lng = ~Longitude,
  addCircleMarkers(data = P2_SW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Cl SW") %>%
  addCircleMarkers(data = P2_GW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Cl GW") %>%
  # addCircleMarkers(data = another, lat = ~Latitude, lng = ~Longitude,
  #                  weight = 0.1, radius = 1, color="grey",stroke = TRUE,
  #                  opacity = 0.5, fill = TRUE,
  #                  group = group3) %>%
  addLayersControl(
    #baseGroups = c("Basemap without Labels", "Basemap with Labels"),
    overlayGroups = c("Cl SW", "Cl GW"),
    options = layersControlOptions(collapsed = FALSE))


#Na
P3_SW_long_corr = filter(P3_SW, Longitude > -80.5)
P3_GW_long_corr = filter(P3_GW, Longitude > -80.5)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap without Labels") %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap with Labels") %>%
  #addCircleMarkers(data = Geospatial_result_PA_Ba, lat = ~Latitude, lng = ~Longitude,
  addCircleMarkers(data = P3_SW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Na SW") %>%
  addCircleMarkers(data = P3_GW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Na GW") %>%
  # addCircleMarkers(data = another, lat = ~Latitude, lng = ~Longitude,
  #                  weight = 0.1, radius = 1, color="grey",stroke = TRUE,
  #                  opacity = 0.5, fill = TRUE,
  #                  group = group3) %>%
  addLayersControl(
    #baseGroups = c("Basemap without Labels", "Basemap with Labels"),
    overlayGroups = c("Na SW", "Na GW"),
    options = layersControlOptions(collapsed = FALSE))


## SC
P4_SW_long_corr = filter(P4_SW, Longitude > -80.5)
P4_GW_long_corr = filter(P4_GW, Longitude > -80.5)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap without Labels") %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap with Labels") %>%
  #addCircleMarkers(data = Geospatial_result_PA_Ba, lat = ~Latitude, lng = ~Longitude,
  addCircleMarkers(data = P4_SW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "SC SW") %>%
  addCircleMarkers(data = P4_GW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "SC GW") %>%
  # addCircleMarkers(data = another, lat = ~Latitude, lng = ~Longitude,
  #                  weight = 0.1, radius = 1, color="grey",stroke = TRUE,
  #                  opacity = 0.5, fill = TRUE,
  #                  group = group3) %>%
  addLayersControl(
    #baseGroups = c("Basemap without Labels", "Basemap with Labels"),
    overlayGroups = c("SC SW", "SC GW"),
    options = layersControlOptions(collapsed = FALSE))


### SO4
P5_SW_long_corr = filter(P5_SW, Longitude > -80.5)
P5_GW_long_corr = filter(P5_GW, Longitude > -80.5)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap without Labels") %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap with Labels") %>%
  #addCircleMarkers(data = Geospatial_result_PA_Ba, lat = ~Latitude, lng = ~Longitude,
  addCircleMarkers(data = P5_SW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "SO4 SW") %>%
  addCircleMarkers(data = P5_GW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "SO4 GW") %>%
  # addCircleMarkers(data = another, lat = ~Latitude, lng = ~Longitude,
  #                  weight = 0.1, radius = 1, color="grey",stroke = TRUE,
  #                  opacity = 0.5, fill = TRUE,
  #                  group = group3) %>%
  addLayersControl(
    #baseGroups = c("Basemap without Labels", "Basemap with Labels"),
    overlayGroups = c("SO4 SW", "SO4 GW"),
    options = layersControlOptions(collapsed = FALSE))


### Sr
P6_SW_long_corr = filter(P6_SW, Longitude > -80.5)
P6_GW_long_corr = filter(P6_GW, Longitude > -80.5)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap without Labels") %>%
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = FALSE
  ),
  group = "Basemap with Labels") %>%
  #addCircleMarkers(data = Geospatial_result_PA_Ba, lat = ~Latitude, lng = ~Longitude,
  addCircleMarkers(data = P6_SW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Sr SW") %>%
  addCircleMarkers(data = P6_GW_long_corr, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Sr GW") %>%
  # addCircleMarkers(data = another, lat = ~Latitude, lng = ~Longitude,
  #                  weight = 0.1, radius = 1, color="grey",stroke = TRUE,
  #                  opacity = 0.5, fill = TRUE,
  #                  group = group3) %>%
  addLayersControl(
    #baseGroups = c("Basemap without Labels", "Basemap with Labels"),
    overlayGroups = c("Sr SW", "Sr GW"),
    options = layersControlOptions(collapsed = FALSE))


leaflet() %>%
  addTiles() %>%
  setView(lng = -77.4, lat = 41, zoom = 7)

#Pennsylvania shapefile from https://www.pasda.psu.edu/tutorials/pasda_R.html
dep_regions <- sf::st_read('https://mapservices.pasda.psu.edu/server/rest/services/pasda/DEP2/MapServer/7/query?returnGeometry=true&where=1=1&outFields=*&f=geojson')
plot(dep_regions$geometry)

library("maps") #How to use county map https://quantdev.ssri.psu.edu/sites/qdev/files/Tutorial_Maps_WeR.html
PAcounties <- map_data("county", region = "pennsylvania")


############ Wells Locations ############

#all_wells_PA
all_wells_PA_marg = filter(all_wells_PA, W_type == "Marginal")
all_wells_PA_unconv = filter(all_wells_PA, W_type == "Unconventional")
all_wells_PA_orph = filter(all_wells_PA, W_type == "Orphaned")


# Create custom colors for markers
pal <- leaflet::colorFactor(c('blue', '#2ca25f', 'black'),
                            domain = c('Marginal','Unconventional', 'Orphaned'),
                            ordered = TRUE)
#leaflet() %>%
leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  # addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = dep_regions, color = "grey", stroke = 0.5, opacity = 0.2) %>%
  addCircleMarkers(data = all_wells_PA_marg, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="blue",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Marginal") %>%
  addCircleMarkers(data = all_wells_PA_unconv, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="#2ca25f",stroke = TRUE,
                   opacity = 0.9, fill = TRUE,
                   group = "Unconv.") %>%
  addCircleMarkers(data = all_wells_PA_orph, lat = ~Latitude, lng = ~Longitude,
                   weight = 0.1, radius = 1.5, color="black",stroke = TRUE,
                   opacity = 0.5, fill = TRUE,
                   group = "Orph") %>%
  addLegend('topright', pal = pal, values = all_wells_PA$W_type, title = 'Well', opacity = 1)


#Matching color pallete in figures
pal2 <- leaflet::colorFactor(c('#d8b365', '#999999', '#5ab4ac'),
                            domain = c('Marginal','Unconventional', 'Orphaned'),
                            ordered = TRUE)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = dep_regions, color = "black", stroke = 0.8, weight = 3, opacity = 0.6, fillOpacity = 0) %>%
  addCircleMarkers( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                    weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
  addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)


library("rgdal")
library("maptools")

PA_polygon1 = st_combine(dep_regions)
PA_polygon = st_union(dep_regions)
PA_polygon2 = st_union(PA_polygon1)

PA_polygon = st_union(dep_regions, by_feature = FALSE, is_coverage = TRUE)

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = PA_polygon, color = "black", stroke = 0.8, weight = 8, opacity = 0.3, fillOpacity = 0.1) %>%
  addCircleMarkers( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                    weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
  addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)


##############################
#DOWLOAD STATE COUNTY POLYGONS
counties <- tigris::counties(cb = T)
PA_counties <- subset(counties, counties$STATEFP == "42")
PA_counties.dfFORT <- fortify(PA_counties, region = "GEOID")
##############################

ggplot(PA_counties.dfFORT, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = NA, color = "blue") +
  coord_map()

plot(PA_counties.dfFORT)

counties_sf <- st_as_sf(counties)
pa_counties_sf <- counties_sf %>%
  filter(STATEFP == "42")
ggplot(pa_counties_sf) +
  geom_sf(fill = NA, color = "blue")


leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = pa_counties_sf, color = "black", stroke = 0.8, weight = 3, opacity = 0.6, fillOpacity = 0.01) %>%
  addCircleMarkers( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                    weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
  addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)



PA_polygon_2 = st_union(pa_counties_sf)
plot(PA_polygon_2)

#Just PA boarder
leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = PA_polygon_2, color = "black", stroke = 0.8, weight = 3, opacity = 0.6, fillOpacity = 0.01) %>%
  addCircleMarkers( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                    weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
 addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)


leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = pa_counties_sf, color = "black", stroke = 0.8, weight = 1, opacity = 0.4, fillOpacity = 0) %>%
  addPolygons(data = PA_polygon_2, color = "black", stroke = 0.8, weight = 3, opacity = 0.6, fillOpacity = 0.01) %>%
  addCircleMarkers( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                    weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
 addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)


leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addTiles() %>%
  setView(lng = -77.6, lat = 41, zoom = 7) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(data = pa_counties_sf, color = "black", stroke = 0.8, weight = 1, opacity = 0.4, fillOpacity = 0) %>%
  addPolygons(data = PA_polygon_2, color = "black", stroke = 0.8, weight = 3, opacity = 0.6, fillOpacity = 0.01) %>%
  addCircleMarkers( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                    weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
  addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)



ggplot(pa_counties_sf) +
  geom_sf(fill = NA, color = "black") +
  theme_ipsum() +
  geom_point( data=all_wells_PA, lat = ~Latitude, lng = ~Longitude, 
                   weight = 0.1, radius = 1.5, color=pal2(all_wells_PA$W_type),stroke = TRUE,  opacity = 0.9, fill = pal2(all_wells_PA$W_type),) %>%
  
  
  addLegend('topright', pal = pal2, values = all_wells_PA$W_type, title = 'Well', opacity = 1)

#THE winner
all_wells_PA_c = filter(all_wells_PA, Longitude < -10)
clrs = c('#d8b365', '#5ab4ac','#999999')
ggplot() + 
  geom_sf(data = PA_polygon_2, lwd = 1, fill = NA) + 
  geom_sf(data = pa_counties_sf, colour = "black", fill = NA) + 
  #geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = clrs[factor(W_type)]),size =1.5, alpha = 0.5) + 
  geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = W_type),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  #theme_ipsum() +
  scale_color_manual(values = clrs)

#Water sample map
clrs_water = c( "#2ca25f","#304092")
ggplot() + 
  geom_sf(data = PA_polygon_2, lwd = 1, fill = NA) + 
  geom_sf(data = pa_counties_sf, colour = "black", fill = NA) + 
  #geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = clrs[factor(W_type)]),size =1.5, alpha = 0.5) + 
  geom_point(data = P_SW_GW[which(P_SW_GW$well_type=="marginal"),], mapping = aes(x = Longitude, y = Latitude, color = type_w),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  #theme_ipsum() +
  scale_color_manual(values = clrs_water)



#Separate GW and SW?
anal_color = c( "#a6cee3", "#1f78b4", "#b2df8a","#33a02c","#fdbf6f","#ff7f00" )
ggplot() + 
  geom_sf(data = PA_polygon_2, lwd = 1, fill = NA) + 
  geom_sf(data = pa_counties_sf, colour = "black", fill = NA) + 
  geom_point(data = P_SW_GW[which(P_SW_GW$well_type=="marginal" & P_SW_GW$type_w =="SW"),], mapping = aes(x = Longitude, y = Latitude, color = Analyte),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  scale_color_manual(values = anal_color)


ggplot() + 
  geom_sf(data = PA_polygon_2, lwd = 1, fill = NA) + 
  geom_sf(data = pa_counties_sf, colour = "black", fill = NA) + 
  geom_point(data = P_SW_GW[which(P_SW_GW$well_type=="marginal" & P_SW_GW$type_w =="GW"),], mapping = aes(x = Longitude, y = Latitude, color = Analyte),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  scale_color_manual(values = anal_color)


D_V_min = min(log(P_SW_GW[which(P_SW_GW$well_type=="marginal" & P_SW_GW$type_w =="GW" & P_SW_GW$Analyte =="Ba"),"daily_mean"]))
D_V_max = max(log(P_SW_GW[which(P_SW_GW$well_type=="marginal" & P_SW_GW$type_w =="GW"& P_SW_GW$Analyte =="Ba"),"daily_mean"]))


ggplot() + 
  geom_sf(data = PA_polygon_2, lwd = 1, fill = NA) + 
  geom_sf(data = pa_counties_sf, colour = "black", fill = NA) + 
  geom_point(data = P_SW_GW[which(P_SW_GW$well_type=="marginal" & P_SW_GW$type_w =="GW" & P_SW_GW$Analyte =="Ba"),], mapping = aes(x = Longitude, y = Latitude, color = Analyte, size = log(daily_mean)*1.5), alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  scale_color_manual(values = anal_color)


#############################################
############## COLORADO ####################

#counties_sf <- st_as_sf(counties)
co_counties_sf <- counties_sf %>%
  filter(STATEFP == "08")
CO_polygon = st_union(co_counties_sf)

#clrs = c('#d8b365', '#5ab4ac','#999999') #should be set in an earlier code
ggplot() + 
  geom_sf(data = CO_polygon, lwd = 1, fill = NA) + 
  geom_sf(data = co_counties_sf, colour = "black", fill = NA) + 
  geom_point(data = all_wells_CO, mapping = aes(x = Longitude, y = Latitude, color = W_type),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  scale_color_manual(values = clrs)


#water sampling locations
ggplot() + 
  geom_sf(data = CO_polygon, lwd = 1, fill = NA) + 
  geom_sf(data = co_counties_sf, colour = "black", fill = NA) + 
  geom_point(data = P_SW_GW_CO[which(P_SW_GW$well_type=="marginal"),], mapping = aes(x = Longitude, y = Latitude, color = type_w),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  scale_color_manual(values = clrs_water)



############################################
############## TEXAS #######################
tx_counties_sf <- counties_sf %>%
  filter(STATEFP == "48")
TX_polygon = st_union(tx_counties_sf)

ggplot() + 
  geom_sf(data = TX_polygon, lwd = 1, fill = NA) + 
  geom_sf(data = tx_counties_sf, colour = "black", fill = NA) + 
  #geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = clrs[factor(W_type)]),size =1.5, alpha = 0.5) + 
  geom_point(data = all_wells_TX, mapping = aes(x = Longitude, y = Latitude, color = W_type),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  #theme_ipsum() +
  scale_color_manual(values = clrs)


#water sampling locations
ggplot() + 
  geom_sf(data = TX_polygon, lwd = 1, fill = NA) + 
  geom_sf(data = tx_counties_sf, colour = "black", fill = NA) + 
  #geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = clrs[factor(W_type)]),size =1.5, alpha = 0.5) + 
  geom_point(data = P_SW_GW_TX[which(P_SW_GW$well_type=="marginal"),], mapping = aes(x = Longitude, y = Latitude, color = type_w),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  #theme_ipsum() +
  scale_color_manual(values = clrs_water)



###########################################
############# NEW YORK ####################

min(all_wells_NY$Latitude)
max(all_wells_NY$Latitude)
min(all_wells_NY$Longitude)
max(all_wells_NY$Longitude)

all_wells_NY_c = filter(all_wells_NY, Longitude < 0)
all_wells_NY_c = filter(all_wells_NY_c, Latitude > 0)

ny_counties_sf <- counties_sf %>%
  filter(STATEFP == "36")
NY_polygon = st_union(ny_counties_sf)

ggplot() + 
  geom_sf(data = NY_polygon, lwd = 1, fill = NA) + 
  geom_sf(data = ny_counties_sf, colour = "black", fill = NA) + 
  #geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = clrs[factor(W_type)]),size =1.5, alpha = 0.5) + 
  geom_point(data = all_wells_NY_c, mapping = aes(x = Longitude, y = Latitude, color = W_type), size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  #theme_ipsum() +
  scale_color_manual(values = clrs)


#water sampling locations
ggplot() + 
  geom_sf(data = NY_polygon, lwd = 1, fill = NA) + 
  geom_sf(data = ny_counties_sf, colour = "black", fill = NA) + 
  #geom_point(data = all_wells_PA_c, mapping = aes(x = Longitude, y = Latitude, color = clrs[factor(W_type)]),size =1.5, alpha = 0.5) + 
  geom_point(data = P_SW_GW_NY[which(P_SW_GW$well_type=="marginal"),], mapping = aes(x = Longitude, y = Latitude, color = type_w),size =1.3, alpha = 0.4) + 
  coord_sf() + 
  theme_bw() +
  #theme_ipsum() +
  scale_color_manual(values = clrs_water)




######
## SAVE all four well variables as one file

all_wells_PA_c$State = "PA"
all_wells_CO$State = "CO"
all_wells_TX$State = "TX"
all_wells_NY_c$State = "NY"

All_wells_comb = rbind(all_wells_PA_c, all_wells_CO, all_wells_TX, all_wells_NY_c)
#Save it
saveRDS(All_wells_comb,file = "All_states_all_wells_list.rds", compress = FALSE)


#United states map?
US_map = st_union(counties_sf)
plot(US_map)
unique(counties_sf)


states <- states(cb = TRUE)
states_cont = states[-c(18,28,29,35,49,54,55),]
plot(states)

states_cont_sf <- st_as_sf(states_cont)

plot(states_cont_sf)
ggplot(states_cont_sf) +
  geom_sf(fill = NA, color = "blue")


ggplot() + 
  geom_sf(data = states_cont_sf, lwd = 1, fill = NA) + 
  geom_sf(data = TX_polygon, lwd = 1, fill = "red") + 
  geom_sf(data = NY_polygon, lwd = 1, fill = "red") + 
  geom_sf(data = CO_polygon, lwd = 1, fill = "red") + 
  geom_sf(data = PA_polygon_2, lwd = 1, fill = "red") + 
  coord_sf() + 
  theme_bw()


####################################
# WATER QUALITY MAPS

#TEXAS
#p_v = which(P_SW_GW_TX$well_type=="marginal" & P_SW_GW_TX$type_w == "SW" & P_SW_GW_TX$Analyte == "Cl")
P_SW_GW_TX_org = P_SW_GW_TX %>%
  arrange(daily_mean)
P_SW_GW_TX_org = filter(P_SW_GW_TX_org, Longitude > -110)
p_v = which(P_SW_GW_TX_org$well_type=="marginal" & P_SW_GW_TX_org$type_w == "GW" & P_SW_GW_TX_org$Analyte == "Sr")
#water sampling - daily_mean magnitudes based on location
ggplot(P_SW_GW_TX_org[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")


##########PA
P_SW_GW_PA_org = P_SW_GW %>%
  arrange(daily_mean)
p_v = which(P_SW_GW_PA_org$well_type=="marginal" & P_SW_GW_PA_org$type_w == "GW" & P_SW_GW_PA_org$Analyte == "Sr")
#water sampling - daily_mean magnitudes based on location
ggplot(P_SW_GW_PA_org[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")


st_as_sf(P_SW_GW_PA_org, crs = crs.geo, coords = 
           c("Longitude", "Latitude")) |>
  st_transform(crs.geo) -> P_SW_GW_PA_org.sf
p_v = which(P_SW_GW_PA_org$well_type=="marginal" & P_SW_GW_PA_org$type_w == "SW" & P_SW_GW_PA_org$Analyte == "Cl")
ggplot() + geom_sf(data = pa_counties_sf) + 
  geom_sf(data = P_SW_GW_PA_org.sf[p_v,], mapping = aes(col = log(daily_mean)))



###### New York
P_SW_GW_NY_org = P_SW_GW_NY %>%
  arrange(daily_mean)

st_as_sf(P_SW_GW_NY_org, crs = crs.geo, coords = 
           c("Longitude", "Latitude")) |>
  st_transform(crs.geo) -> P_SW_GW_NY_org.sf
p_v = which(P_SW_GW_NY_org$well_type=="marginal" & P_SW_GW_NY_org$type_w == "SW" & P_SW_GW_NY_org$Analyte == "SC")
ggplot() + geom_sf(data = ny_counties_sf) + 
  geom_sf(data = P_SW_GW_NY_org.sf[p_v,], mapping = aes(col = log(daily_mean))) +
  theme_bw()





