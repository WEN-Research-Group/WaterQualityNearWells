########################################
#Texas counties
Ogallala = c("Dallam","Sherman", "Hansford","Ochiltree","Lipscomb","Hartley","Moore","Hutchinson","Roberts","Hemphill","Oldham","Potter","Carson","Gray","Wheeler",
  "Deaf Smith","Randall","Armstrong","Donley","Parmer","Castro","Swisher","Briscoe","Bailey","Lamb","Hale","Floyd","Cochran","Hockley","Lubbock","Crosby",
  "Yoakum","Terry","Lynn","Garza","Gaines","Dawson","Borden","Andrews","Martin","Howard",
  "Dallam County","Sherman County", "Hansford County","Ochiltree County","Lipscomb County","Hartley County","Moore County","Hutchinson County",
  "Roberts County","Hemphill County","Oldham County","Potter County","Carson County","Gray County","Wheeler County",
  "Deaf Smith County","Randall County","Armstrong County","Donley County","Parmer County","Castro County","Swisher County","Briscoe County",
  "Bailey County","Lamb County","Hale County","Floyd County","Cochran County","Hockley County","Lubbock County","Crosby County",
  "Yoakum County","Terry County","Lynn County","Garza County","Gaines County","Dawson County","Borden County","Andrews County","Martin County","Howard County")


TX_Ogallala = Geospatial_result_TX_all %>%
  filter(County %in% Ogallala)

p_v = which(TX_Ogallala$well_type=="marginal" & TX_Ogallala$type_w == "GW" & TX_Ogallala$Analyte == "SC")
ggplot(TX_Ogallala[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_Ogallala$nearest_distance_m[p_v]),log(TX_Ogallala$daily_mean[p_v]))
cor(log(TX_Ogallala$nearest_distance_m[p_v]),log(TX_Ogallala$daily_mean[p_v]))

p_v = which(Geospatial_result_TX_all$well_type=="marginal" & Geospatial_result_TX_all$type_w == "GW" & Geospatial_result_TX_all$Analyte == "SC")
plot(log(Geospatial_result_TX_all$nearest_distance_m[p_v]),log(Geospatial_result_TX_all$daily_mean[p_v]))
cor(log(Geospatial_result_TX_all$nearest_distance_m[p_v]),log(Geospatial_result_TX_all$daily_mean[p_v]))




p_v = which(TX_Ogallala$well_type=="marginal" & TX_Ogallala$type_w == "SW" & TX_Ogallala$Analyte == "SC")

plot(log(TX_Ogallala$nearest_distance_m[p_v]),log(TX_Ogallala$daily_mean[p_v]))
cor(log(TX_Ogallala$nearest_distance_m[p_v]),log(TX_Ogallala$daily_mean[p_v]))

p_v = which(Geospatial_result_TX_all$well_type=="marginal" & Geospatial_result_TX_all$type_w == "SW" & Geospatial_result_TX_all$Analyte == "SC")
plot(log(Geospatial_result_TX_all$nearest_distance_m[p_v]),log(Geospatial_result_TX_all$daily_mean[p_v]))
cor(log(Geospatial_result_TX_all$nearest_distance_m[p_v]),log(Geospatial_result_TX_all$daily_mean[p_v]))



#Correlation matrix
TX_Ogallala_corr = TX_Ogallala %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

corrplot(as.matrix(TX_Ogallala_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_Ogallala_corr[1:18,4:7]), method = 'color',is.corr = FALSE) #GW


#Summary - how many data points?
TX_Ogallala_summ = TX_Ogallala %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())



#SW marginal Sr causes error - remove
p_v = which(TX_Ogallala$well_type=="marginal" & TX_Ogallala$type_w == "SW" & TX_Ogallala$Analyte == "Sr")
TX_Ogallala_r = TX_Ogallala[-p_v,]
p_v = which(TX_Ogallala_r$well_type=="orphaned" & TX_Ogallala_r$type_w == "SW" ) # & TX_Ogallala_r$Analyte == "Ba")
TX_Ogallala_r2 = TX_Ogallala_r[-p_v,]

#Calculate just for GW
p_v = which(TX_Ogallala$type_w == "GW")
TX_Ogallala_r = TX_Ogallala[p_v,]


TX_Ogallala_corr_p = TX_Ogallala_r %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_Ogallala_corr_vals = TX_Ogallala_corr[1:18,4:7]
TX_Ogallala_corr_p_vals = TX_Ogallala_corr_p[,4:7]


cnt = 0
TX_Ogallala_corr_vals_only_sig = TX_Ogallala_corr_vals
for (i in seq(1:dim(TX_Ogallala_corr_vals)[1])){
  for (j in seq(1:dim(TX_Ogallala_corr_vals)[2])){
    if (!is.na(as.numeric(TX_Ogallala_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_Ogallala_corr_p_vals[i,j]) > 0.005){
        TX_Ogallala_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}

corrplot(as.matrix(TX_Ogallala_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_Ogallala_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW



############################################

Gulf = c("Newton","Orange","Jasper","Tyler","Hardin","Jefferson","Polk","San Jacinto","Liberty","Chambers","Walker","Montgomery","Harris","Brazoria",
         "Grimes","Waller","Fort Bend","Galveson","Washington","Austin","Fayette","Colorado","Wharton","Matagorcia","Lavaca","Jackson","Dewitt","Victoria",
         "Calhoun","Kames","Goliad","Refugio","Bee","Live Oak","San Patricio","Duval","Jim Wells","Nueces","Kleberg","Jim Hogg","Brooks","Kenedy",
         "Starr","Hidalgo", 
         "Newton County","Orange County","Jasper County","Tyler County","Hardin County","Jefferson County","Polk County","San Jacinto County",
         "Liberty County","Chambers County","Walker County","Montgomery County","Harris County","Brazoria County",
         "Grimes County","Waller County","Fort Bend County","Galveson County","Washington County","Austin County","Fayette County","Colorado County",
         "Wharton County","Matagorcia County","Lavaca County","Jackson County","Dewitt County","Victoria County",
         "Calhoun County","Kames County","Goliad County","Refugio County","Bee County","Live Oak County","San Patricio County","Duval County",
         "Jim Wells County","Nueces County","Kleberg County","Jim Hogg County","Brooks County","Kenedy County",
         "Starr County","Hidalgo County")


TX_Gulf = Geospatial_result_TX_all %>%
  filter(County %in% Gulf,
         type_w == "GW")

p_v = which(TX_Gulf$well_type=="marginal" & TX_Gulf$type_w == "GW" & TX_Gulf$Analyte == "SC")
ggplot(TX_Gulf[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_Gulf$nearest_distance_m[p_v]),log(TX_Gulf$daily_mean[p_v]))
cor(log(TX_Gulf$nearest_distance_m[p_v]),log(TX_Gulf$daily_mean[p_v]))


#Summary - how many data points?
TX_Gulf_summ = TX_Gulf %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())


#Correlation matrix
TX_Gulf_corr = TX_Gulf %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

#corrplot(as.matrix(TX_Gulf_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_Gulf_corr[1:18,4:7]), method = 'color',is.corr = FALSE) #GW


TX_Gulf_corr_p = TX_Gulf %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_Gulf_corr_vals = TX_Gulf_corr[,4:7]
TX_Gulf_corr_p_vals = TX_Gulf_corr_p[,4:7]


cnt = 0
TX_Gulf_corr_vals_only_sig = TX_Gulf_corr_vals
for (i in seq(1:dim(TX_Gulf_corr_vals)[1])){
  for (j in seq(1:dim(TX_Gulf_corr_vals)[2])){
    if (!is.na(as.numeric(TX_Gulf_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_Gulf_corr_p_vals[i,j]) > 0.005){
        TX_Gulf_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}

corrplot(as.matrix(TX_Gulf_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_Gulf_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW





#############

E_Trinity = c("Glasscock","Sterling","Reagan","Upion","Irion","Pecos","Terrell","Crockett","Schleicher","Menard","Sutton","Kimble","Val Verde",
              "Edwards","Real","Glasscock County","Sterling County","Reagan County","Upion County","Irion County","Pecos County","Terrell County",
              "Crockett County","Schleicher County","Menard County","Sutton County","Kimble County","Val Verde County",
              "Edwards County","Real County")

TX_E_Trinity = Geospatial_result_TX_all %>%
  filter(County %in% E_Trinity,
         type_w == "GW")

p_v = which(TX_E_Trinity$well_type=="marginal" & TX_E_Trinity$type_w == "GW" & TX_E_Trinity$Analyte == "Na")
ggplot(TX_E_Trinity[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_E_Trinity$nearest_distance_m[p_v]),log(TX_E_Trinity$daily_mean[p_v]))
cor(log(TX_E_Trinity$nearest_distance_m[p_v]),log(TX_E_Trinity$daily_mean[p_v]),use = "complete.obs")

p_v = which(TX_E_Trinity$well_type=="orphaned" & TX_E_Trinity$type_w == "GW" & TX_E_Trinity$Analyte == "Na" & TX_E_Trinity$num_well_1km >0)
plot(log(TX_E_Trinity$num_well_1km[p_v]),log(TX_E_Trinity$daily_mean[p_v]))
cor(log(TX_E_Trinity$num_well_1km[p_v]),log(TX_E_Trinity$daily_mean[p_v]), use = "complete.obs")


#Summary - how many data points?
TX_E_Trinity_summ = TX_E_Trinity %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())


#Correlation matrix
TX_E_Trinity_corr = TX_E_Trinity %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m), use = "complete.obs"), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

#corrplot(as.matrix(TX_Gulf_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_E_Trinity_corr[1:18,4:7]), method = 'color',is.corr = FALSE) #GW


TX_E_Trinity_corr_p = TX_E_Trinity %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_E_Trinity_corr_vals = TX_E_Trinity_corr[,4:7]
TX_E_Trinity_corr_p_vals = TX_E_Trinity_corr_p[,4:7]


cnt = 0
TX_E_Trinity_corr_vals_only_sig = TX_E_Trinity_corr_vals
for (i in seq(1:dim(TX_E_Trinity_corr_vals)[1])){
  for (j in seq(1:dim(TX_E_Trinity_corr_vals)[2])){
    if (!is.na(as.numeric(TX_E_Trinity_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_E_Trinity_corr_p_vals[i,j]) > 0.005){
        TX_E_Trinity_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}

corrplot(as.matrix(TX_E_Trinity_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_E_Trinity_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW






##############################################


C_Wilcox = c("Cass","Marion","Harrison","Panola","Shelby","Titus","Morris","Upshur","Rusk","Gregg","Nacogdoches","San Augustine","Camp","Wood",
             "Smith","Cherokee","Houston","Anderson","Henderson","Van Zandt","Rains","Freestone","Leon","Madison","Robertson","Milam","Burleson",
             "Lee","Bastrop","Caldwell","Guadalupe","Gonzales","Wilson","Atascosa","Frio","La Salle","McMullen","Zavala","Dimmit",
             "Cass County","Marion County","Harrison County","Panola County","Shelby County","Titus County","Morris County","Upshur County",
             "Rusk County","Gregg County","Nacogdoches County","San Augustine County","Camp County","Wood County",
             "Smith County","Cherokee County","Houston County","Anderson County","Henderson County","Van Zandt County","Rains County",
             "Freestone County","Leon County","Madison County","Robertson County","Milam County","Burleson County",
             "Lee County","Bastrop County","Caldwell County","Guadalupe County","Gonzales County","Wilson County","Atascosa County",
             "Frio County","La Salle County","McMullen County","Zavala County","Dimmit County")



TX_C_Wilcox = Geospatial_result_TX_all %>%
  filter(County %in% C_Wilcox,
         type_w == "GW")

p_v = which(TX_C_Wilcox$well_type=="marginal" & TX_C_Wilcox$type_w == "GW" & TX_C_Wilcox$Analyte == "Na")
ggplot(TX_C_Wilcox[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_C_Wilcox$nearest_distance_m[p_v]),log(TX_C_Wilcox$daily_mean[p_v]))
cor(log(TX_C_Wilcox$nearest_distance_m[p_v]),log(TX_C_Wilcox$daily_mean[p_v]),use = "complete.obs")

p_v = which(TX_C_Wilcox$well_type=="orphaned" & TX_C_Wilcox$type_w == "GW" & TX_C_Wilcox$Analyte == "Na" & TX_C_Wilcox$num_well_1km >0)
plot(log(TX_C_Wilcox$num_well_1km[p_v]),log(TX_C_Wilcox$daily_mean[p_v]))
cor(log(TX_C_Wilcox$num_well_1km[p_v]),log(TX_C_Wilcox$daily_mean[p_v]), use = "complete.obs")


#Summary - how many data points?
TX_C_Wilcox_summ = TX_C_Wilcox %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())


#Correlation matrix
TX_C_Wilcox_corr = TX_C_Wilcox %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m), use = "complete.obs"), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

#corrplot(as.matrix(TX_Gulf_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_C_Wilcox_corr[,4:7]), method = 'color',is.corr = FALSE) #GW


TX_C_Wilcox_corr_p = TX_C_Wilcox %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_C_Wilcox_corr_vals = TX_C_Wilcox_corr[,4:7]
TX_C_Wilcox_corr_p_vals = TX_C_Wilcox_corr_p[,4:7]


cnt = 0
TX_C_Wilcox_corr_vals_only_sig = TX_C_Wilcox_corr_vals
for (i in seq(1:dim(TX_C_Wilcox_corr_vals)[1])){
  for (j in seq(1:dim(TX_C_Wilcox_corr_vals)[2])){
    if (!is.na(as.numeric(TX_C_Wilcox_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_C_Wilcox_corr_p_vals[i,j]) > 0.005){
        TX_C_Wilcox_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}

corrplot(as.matrix(TX_C_Wilcox_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_C_Wilcox_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW


#High influence from unconventional wells - in this aquifer most of the well are unconventional




##################################################

Trinity = c("Lamar","Fannin","Grayson","Cooke","Montague","Wise","Denton","Collin","Parker","Tarrant","Dallas","Ellis","Johnson","Hood","Somervell",
            "Erath","Comanche","Mills","Hamilton","Bosque","Hill","Lampasas","Coryell","McLennan","Burnet","Williamson","Bell","Travis","Hays","Bianco",
            "Kendall","Comal","Bandera","Lamar County","Fannin County","Grayson County","Cooke County","Montague County","Wise County","Denton County",
            "Collin County","Parker County","Tarrant County","Dallas County","Ellis County","Johnson County","Hood County","Somervell County",
            "Erath County","Comanche County","Mills County","Hamilton County","Bosque County","Hill County","Lampasas County","Coryell County",
            "McLennan County","Burnet County","Williamson County","Bell County","Travis County","Hays County","Bianco County",
            "Kendall County","Comal County","Bandera County")


TX_Trinity = Geospatial_result_TX_all %>%
  filter(County %in% Trinity,
         type_w == "GW")

p_v = which(TX_Trinity$well_type=="marginal" & TX_Trinity$type_w == "GW" & TX_Trinity$Analyte == "Na")
ggplot(TX_Trinity[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_Trinity$nearest_distance_m[p_v]),log(TX_Trinity$daily_mean[p_v]))
cor(log(TX_Trinity$nearest_distance_m[p_v]),log(TX_Trinity$daily_mean[p_v]),use = "complete.obs")

p_v = which(TX_Trinity$well_type=="orphaned" & TX_Trinity$type_w == "GW" & TX_Trinity$Analyte == "Na" & TX_Trinity$num_well_1km >0)
plot(log(TX_Trinity$num_well_1km[p_v]),log(TX_Trinity$daily_mean[p_v]))
cor(log(TX_Trinity$num_well_1km[p_v]),log(TX_Trinity$daily_mean[p_v]), use = "complete.obs")


#Summary - how many data points?
TX_Trinity_summ = TX_Trinity %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())


#Correlation matrix
TX_Trinity_corr = TX_Trinity %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m), use = "complete.obs"), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

#corrplot(as.matrix(TX_Gulf_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_Trinity_corr[,4:7]), method = 'color',is.corr = FALSE) #GW


TX_Trinity_corr_p = TX_Trinity %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_Trinity_corr_vals = TX_Trinity_corr[,4:7]
TX_Trinity_corr_p_vals = TX_Trinity_corr_p[,4:7]


cnt = 0
TX_Trinity_corr_vals_only_sig = TX_Trinity_corr_vals
for (i in seq(1:dim(TX_Trinity_corr_vals)[1])){
  for (j in seq(1:dim(TX_Trinity_corr_vals)[2])){
    if (!is.na(as.numeric(TX_Trinity_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_Trinity_corr_p_vals[i,j]) > 0.005){
        TX_Trinity_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}

corrplot(as.matrix(TX_Trinity_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_Trinity_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW




######################################

Edwards = c("Uvalde","Medina","Bexar")

TX_Edwards = Geospatial_result_TX_all %>%
  filter(County %in% Edwards,
         type_w == "GW")

p_v = which(TX_Edwards$well_type=="marginal" & TX_Edwards$type_w == "GW" & TX_Edwards$Analyte == "Na")
ggplot(TX_Edwards[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_Edwards$nearest_distance_m[p_v]),log(TX_Edwards$daily_mean[p_v]))
cor(log(TX_Edwards$nearest_distance_m[p_v]),log(TX_Edwards$daily_mean[p_v]),use = "complete.obs")


#Summary - how many data points?
TX_Edwards_summ = TX_Edwards %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())

#Remove GW orphaned Cl
p_v = which(TX_Edwards$well_type=="orphaned" & TX_Edwards$type_w == "GW" )
TX_Edwards_r = TX_Edwards[-p_v,]

p_v = which(TX_Edwards$well_type=="orphaned" & TX_Edwards$type_w == "GW" & TX_Edwards$Analyte == "SC" & TX_Edwards$num_well_1km >0)
plot(log(TX_Edwards$num_well_1km[p_v]),log(TX_Edwards$daily_mean[p_v]))
cor(log(TX_Edwards$num_well_1km[p_v]),log(TX_Edwards$daily_mean[p_v]), use = "complete.obs")



#Correlation matrix
TX_Edwards_corr = TX_Edwards %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m), use = "complete.obs"), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"))#,
           # den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
          #  den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

#corrplot(as.matrix(TX_Gulf_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_Edwards_corr[,4:5]), method = 'color',is.corr = FALSE) #GW


TX_Edwards_corr_p = TX_Edwards %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value))#,
           # den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
           # den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_Edwards_corr_vals = TX_Edwards_corr[,4:5]
TX_Edwards_corr_p_vals = TX_Edwards_corr_p[,4:5]


cnt = 0
TX_Edwards_corr_vals_only_sig = TX_Edwards_corr_vals
for (i in seq(1:dim(TX_Edwards_corr_vals)[1])){
  for (j in seq(1:dim(TX_Edwards_corr_vals)[2])){
    if (!is.na(as.numeric(TX_Edwards_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_Edwards_corr_p_vals[i,j]) > 0.005){
        TX_Edwards_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 
      }
    }
  }
}

corrplot(as.matrix(TX_Edwards_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_Edwards_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW



##########

#Pecos Valley Aquifer


Pecos = c("Loving","Winkler","Ward","Crane","Reeves","Pecos",
          "Culberson","Jeff Davis","Andrews","Ector","Upton","Crockett",
          "Loving County","Winkler County","Ward County","Crane County","Reeves County","Pecos County",
          "Culberson County","Jeff Davis County","Andrews County","Ector County","Upton County","Crockett County")

TX_Pecos = Geospatial_result_TX_all %>%
  filter(County %in% Pecos,
         type_w == "GW")

p_v = which(TX_Pecos$well_type=="marginal" & TX_Pecos$type_w == "GW" & TX_Pecos$Analyte == "Na")
ggplot(TX_Pecos[p_v,], aes(x = Longitude, y = Latitude)) +
  geom_point(aes(colour = log(daily_mean))) + 
  scale_colour_gradient(low = "white", high = "black")

plot(log(TX_Pecos$nearest_distance_m[p_v]),log(TX_Pecos$daily_mean[p_v]))
cor(log(TX_Pecos$nearest_distance_m[p_v]),log(TX_Pecos$daily_mean[p_v]),use = "complete.obs")

#Summary - how many data points?
TX_Pecos_summ = TX_Pecos %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(n = n())

#Correlation matrix
TX_Pecos_corr = TX_Pecos %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m), use = "complete.obs"), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"))#,
# den_1km = -cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
#  den_3km = -cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

#corrplot(as.matrix(TX_Gulf_corr[19:36,4:7]), method = 'color',is.corr = FALSE) #SW
corrplot(as.matrix(TX_Pecos_corr[,4:5]), method = 'color',is.corr = FALSE) #GW


TX_Pecos_corr_p = TX_Pecos %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m), use = "complete.obs")$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value))#,
# den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
# den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


TX_Pecos_corr_vals = TX_Pecos_corr[,4:5]
TX_Pecos_corr_p_vals = TX_Pecos_corr_p[,4:5]


cnt = 0
TX_Pecos_corr_vals_only_sig = TX_Pecos_corr_vals
for (i in seq(1:dim(TX_Pecos_corr_vals)[1])){
  for (j in seq(1:dim(TX_Pecos_corr_vals)[2])){
    if (!is.na(as.numeric(TX_Pecos_corr_p_vals[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(TX_Pecos_corr_p_vals[i,j]) > 0.005){
        TX_Pecos_corr_vals_only_sig[i,j] = NA
        cnt= cnt + 1 
      }
    }
  }
}

corrplot(as.matrix(TX_Pecos_corr_vals_only_sig), method = 'color',is.corr = FALSE) #GW
corrplot(as.matrix(TX_Pecos_corr_vals_only_sig[,1:2]), method = 'color',is.corr = FALSE) #GW







###########
Texas_corr_all_aq = cbind(TX_Ogallala_corr_vals_only_sig[,1:2], TX_Pecos_corr_vals_only_sig[,1:2], TX_E_Trinity_corr_vals_only_sig[,1:2],
                          TX_Trinity_corr_vals_only_sig[,1:2], TX_Edwards_corr_vals_only_sig[,1:2],
                          TX_C_Wilcox_corr_vals_only_sig[,1:2], TX_Gulf_corr_vals_only_sig[,1:2])


corrplot(as.matrix(Texas_corr_all_aq), method = 'color',is.corr = FALSE) 

#Get some values

#Averages for each aquifer

#Ogallala
mean(Texas_corr_all_aq[,1:2], na.rm = TRUE)
min(Texas_corr_all_aq[,1:2], na.rm = TRUE)
max(Texas_corr_all_aq[,1:2], na.rm = TRUE)

#Pecos
mean(Texas_corr_all_aq[,3:4], na.rm = TRUE)
min(Texas_corr_all_aq[,3:4], na.rm = TRUE)
max(Texas_corr_all_aq[,3:4], na.rm = TRUE)

mean(Texas_corr_all_aq[c(2,8,14),3:4], na.rm = TRUE) #Cl
mean(Texas_corr_all_aq[c(5,11,17),3:4], na.rm = TRUE) #SO4

#E Trinity
mean(Texas_corr_all_aq[,5:6], na.rm = TRUE)
min(Texas_corr_all_aq[,5:6], na.rm = TRUE)
max(Texas_corr_all_aq[,5:6], na.rm = TRUE)

#Trinity
mean(Texas_corr_all_aq[,7:8], na.rm = TRUE)
min(Texas_corr_all_aq[,7:8], na.rm = TRUE)
max(Texas_corr_all_aq[,7:8], na.rm = TRUE)

#Edwards
mean(Texas_corr_all_aq[,9:10], na.rm = TRUE)
min(Texas_corr_all_aq[,9:10], na.rm = TRUE)
max(Texas_corr_all_aq[,9:10], na.rm = TRUE)

#C Wilcox
mean(Texas_corr_all_aq[,11:12], na.rm = TRUE)
min(Texas_corr_all_aq[,11:12], na.rm = TRUE)
max(Texas_corr_all_aq[,11:12], na.rm = TRUE)
mean(Texas_corr_all_aq[13:18,11:12], na.rm = TRUE)

#Gulf
mean(Texas_corr_all_aq[,13:14], na.rm = TRUE)
min(Texas_corr_all_aq[,13:14], na.rm = TRUE)
max(Texas_corr_all_aq[,13:14], na.rm = TRUE)


###########
Texas_corr_all_aq = cbind(TX_Ogallala_corr_vals_only_sig[,1:2], TX_E_Trinity_corr_vals_only_sig[,1:2],
                          TX_Trinity_corr_vals_only_sig[,1:2], TX_Edwards_corr_vals_only_sig[,1:2],
                          TX_C_Wilcox_corr_vals_only_sig[,1:2], TX_Gulf_corr_vals_only_sig[,1:2])


corrplot(as.matrix(Texas_corr_all_aq), method = 'color',is.corr = FALSE) 


#################

Orph_Texas = read_csv("Texas Orphan Well List September 2025.csv")
Orph_Texas = filter(Orph_Texas, !is.na(Orph_Texas$latitude))

ggplot(Orph_Texas, aes(x = longitude, y = latitude)) +
  geom_point(aes(colour = longitude)) + 
  scale_colour_gradient(low = "white", high = "black")

plot(Orph_Texas$longitude, Orph_Texas$latitude)

