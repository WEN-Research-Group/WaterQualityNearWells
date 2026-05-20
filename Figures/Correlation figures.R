#CORRELATION FIGURES


#######################################
#######################################
## FIGURE 2
########################################
######################################
#Uploading Geospatial results

#OPEN CSVs
Geospatial_result_TX_all = rbind(Geospatial_results_TX_Ba, Geospatial_results_TX_Cl, Geospatial_results_TX_Na, Geospatial_results_TX_SC,
                                 Geospatial_results_TX_SO4, Geospatial_results_TX_Sr)
Geospatial_result_TX_all$Analyte = c(rep("Ba",dim(Geospatial_results_TX_Ba)[1]),rep("Cl",dim(Geospatial_results_TX_Cl)[1]),
                                     rep("Na",dim(Geospatial_results_TX_Na)[1]),rep("SC",dim(Geospatial_results_TX_SC)[1]),
                                     rep("SO4",dim(Geospatial_results_TX_SO4)[1]),rep("Sr",dim(Geospatial_results_TX_Sr)[1]))


F_P_SW_GW_CO = Geospatial_result_CO_all
F_P_SW_GW_TX = Geospatial_result_TX_all
F_P_SW_GW_PA = Geospatial_result_PA_all
F_P_SW_GW_NY = Geospatial_result_NY_all


########################################
# Calculating correlation values

F_P_SW_GW_CO_corr = F_P_SW_GW_CO %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_TX_corr = F_P_SW_GW_TX %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_PA_corr = F_P_SW_GW_PA %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean[which(nearest_distance_m > 1)]), log(nearest_distance_m[which(nearest_distance_m > 1)])), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_NY_corr = F_P_SW_GW_NY %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_NY_corr_unc = rbind(as.matrix(F_P_SW_GW_NY_corr[1:12,4:7]), 
                            (matrix(NA, nrow = 6, ncol=4)),
                            as.matrix(F_P_SW_GW_NY_corr[13:24,4:7]), 
                            (matrix(NA, nrow = 6, ncol=4)))

F_corr_matrix_TX_CO_PA_NY = as.matrix(cbind(F_P_SW_GW_TX_corr[,4:7], F_P_SW_GW_CO_corr[,4:7], F_P_SW_GW_PA_corr[,4:7],F_P_SW_GW_NY_corr_unc))
F_corr_matrix_TX_CO_PA_NY_dir = F_corr_matrix_TX_CO_PA_NY
F_corr_matrix_TX_CO_PA_NY_dir[,c(3,4,7,8,11,12,15,16)] = F_corr_matrix_TX_CO_PA_NY[,c(3,4,7,8,11,12,15,16)] * (-1)

corrplot(F_corr_matrix_TX_CO_PA_NY_dir, method = 'color',is.corr = FALSE)


##############################################
#P-values

F_P_SW_GW_TX_corr_p = F_P_SW_GW_TX %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))

F_P_SW_GW_CO_corr_p = F_P_SW_GW_CO %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))

F_P_SW_GW_PA_corr_p = F_P_SW_GW_PA %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean[which(nearest_distance_m > 1)]), log(nearest_distance_m[which(nearest_distance_m > 1)]))$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))


F_P_SW_GW_NY_corr_p = F_P_SW_GW_NY %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$p.value), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$p.value),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$p.value), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$p.value))

F_P_SW_GW_NY_corr_unc_p = rbind(as.matrix(F_P_SW_GW_NY_corr_p[1:12,4:7]), 
                              (matrix(NA, nrow = 6, ncol=4)),
                              as.matrix(F_P_SW_GW_NY_corr_p[13:24,4:7]), 
                              (matrix(NA, nrow = 6, ncol=4)))


F_corr_matrix_TX_CO_PA_NY_p = as.matrix(cbind(F_P_SW_GW_TX_corr_p[,4:7], F_P_SW_GW_CO_corr_p[,4:7], F_P_SW_GW_PA_corr_p[,4:7],F_P_SW_GW_NY_corr_unc_p))


##################################################
#Keeping only significant values 

cnt = 0
F_corr_matrix_TX_CO_PA_NY_only_sig_dir = F_corr_matrix_TX_CO_PA_NY_dir
for (i in seq(1:dim(F_corr_matrix_TX_CO_PA_NY_dir)[1])){
  for (j in seq(1:dim(F_corr_matrix_TX_CO_PA_NY_dir)[2])){
    if (!is.na(as.numeric(F_corr_matrix_TX_CO_PA_NY_p[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(F_corr_matrix_TX_CO_PA_NY_p[i,j]) > 0.005){
        F_corr_matrix_TX_CO_PA_NY_only_sig_dir[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}
corrplot(F_corr_matrix_TX_CO_PA_NY_only_sig_dir, method = 'color',is.corr = FALSE)



###############################################################
# DISTANCE FILTER
#Do the same as above for distance-filtered data
##############################################################



#Need to make a loop so that it would be easier to create matrixes and report numbers
state = c("Colorado","New York", "Pennsylvania", "Texas")
type_w = c("GW","SW")
anal = c("Ba", "Cl", "Na", "SC", "SO4", "Sr")
well_type = c("marginal","orphaned","unconventional")
well_type_NY = c("marginal", "orphaned")


#Correlation matrix
corr_mat = (array(NA, c(36,4,6)))
well_ct = 0
anal_ct = 0
stat_cnt = 0

state = "New York"

#calculating values
for (i in state){
  stat_cnt = stat_cnt + 1
  for (j in type_w){
    #for (a_t in anal){
    if (i != "New York") {
      for (w_t in well_type){
        well_ct = well_ct +1
        for (a_t in anal){
          anal_ct = anal_ct + 1
          geospatial_results_1 = readRDS(file = paste("./output/AGU/",i,"/",i,"_",j,"_",a_t,"_",w_t,".rds",sep=""))
          #geospatial_results_1_test = readRDS(file = paste("./output/AGU/",i,"/",i,"_","GW","_","Ba","_","marginal",".rds",sep=""))
          
          
          #geospatial_results_1 = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))
          geospatial_results_1_short = geospatial_results_1[,c("daily_mean","nearest_distance_m","closest_well_dist_sum","num_well_1km","num_well_3km")]
          
          wq.distance_100km <- geospatial_results_1_short %>%
            filter(closest_well_dist_sum < 100000)
          
          wq.distance_10km <- geospatial_results_1_short %>%
            filter(nearest_distance_m < 10000 & nearest_distance_m > 1)
          
          wq.distance_1km_d <- geospatial_results_1_short %>%
            filter(num_well_1km >0)
          
          wq.distance_3km_d <- geospatial_results_1_short %>%
            filter(num_well_3km >0)
          
          #rows = analyte
          #columns = proxy type
          #3rd dimension = metric
          
          #For nearest_distance_m proxy
          corr_mat[anal_ct, 1*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m))$estimate) #Pearson
          corr_mat[anal_ct, 1*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m))$p.value) #Pearson
          corr_mat[anal_ct, 1*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 1*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 1*stat_cnt, 5] = length(wq.distance_10km$daily_mean) #Nr of samples
          corr_mat[anal_ct, 1*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_10km$nearest_distance_m)))
          
          #For distance_sum proxy
          corr_mat[anal_ct, 2*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum))$estimate) #Pearson
          corr_mat[anal_ct, 2*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum))$p.value) #Pearson
          corr_mat[anal_ct, 2*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 2*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 2*stat_cnt, 5] = length(wq.distance_100km$closest_well_dist_sum) #Nr of samples
          corr_mat[anal_ct, 2*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_100km$closest_well_dist_sum)))
          
          #For 1km density proxy
          corr_mat[anal_ct, 3*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km))$estimate) #Pearson
          corr_mat[anal_ct, 3*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km))$p.value) #Pearson
          corr_mat[anal_ct, 3*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 3*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 3*stat_cnt, 5] = length(wq.distance_1km_d$num_well_1km) #Nr of samples
          corr_mat[anal_ct, 3*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_1km_d$num_well_1km)))
          
          #For 3km density proxy
          corr_mat[anal_ct, 4*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km))$estimate) #Pearson
          corr_mat[anal_ct, 4*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km))$p.value) #Pearson
          corr_mat[anal_ct, 4*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 4*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 4*stat_cnt, 5] = length(wq.distance_3km_d$num_well_3km) #Nr of samples
          corr_mat[anal_ct, 4*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_3km_d$num_well_3km)))
          
          
        }}
    } else{
      for (w_t in well_type_NY){
        for (a_t in anal){
          anal_ct = anal_ct + 1
          geospatial_results_1 = readRDS(file = paste("./output/AGU/",i,"/",i,"_",j,"_",a_t,"_",w_t,".rds",sep=""))
          #geospatial_results_1_test = readRDS(file = paste("./output/AGU/",i,"/",i,"_","GW","_","Ba","_","marginal",".rds",sep=""))
          
          
          #geospatial_results_1 = readRDS(file = paste("./output/AGU/",state,"/",state,"_",type_w,"_",anal,"_",well_type,".rds",sep=""))
          geospatial_results_1_short = geospatial_results_1[,c("daily_mean","nearest_distance_m","closest_well_dist_sum","num_well_1km","num_well_3km")]
          
          wq.distance_100km <- geospatial_results_1_short %>%
            filter(closest_well_dist_sum < 100000)
          
          wq.distance_10km <- geospatial_results_1_short %>%
            filter(nearest_distance_m < 10000 & nearest_distance_m > 1)
          
          wq.distance_1km_d <- geospatial_results_1_short %>%
            filter(num_well_1km >0)
          
          wq.distance_3km_d <- geospatial_results_1_short %>%
            filter(num_well_3km >0)
          
          #rows = analyte
          #columns = proxy type
          #3rd dimension = metric
          
          #For nearest_distance_m proxy
          corr_mat[anal_ct, 1*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m))$estimate) #Pearson
          corr_mat[anal_ct, 1*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m))$p.value) #Pearson
          corr_mat[anal_ct, 1*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 1*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 1*stat_cnt, 5] = length(wq.distance_10km$daily_mean) #Nr of samples
          corr_mat[anal_ct, 1*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_10km$nearest_distance_m)))
          
          #For distance_sum proxy
          corr_mat[anal_ct, 2*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum))$estimate) #Pearson
          corr_mat[anal_ct, 2*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum))$p.value) #Pearson
          corr_mat[anal_ct, 2*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 2*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_100km$daily_mean), log(wq.distance_100km$closest_well_dist_sum),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 2*stat_cnt, 5] = length(wq.distance_100km$closest_well_dist_sum) #Nr of samples
          corr_mat[anal_ct, 2*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_100km$closest_well_dist_sum)))
          
          #For 1km density proxy
          corr_mat[anal_ct, 3*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km))$estimate) #Pearson
          corr_mat[anal_ct, 3*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km))$p.value) #Pearson
          corr_mat[anal_ct, 3*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 3*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_1km_d$daily_mean), log(wq.distance_1km_d$num_well_1km),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 3*stat_cnt, 5] = length(wq.distance_1km_d$num_well_1km) #Nr of samples
          corr_mat[anal_ct, 3*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_1km_d$num_well_1km)))
          
          #For 3km density proxy
          corr_mat[anal_ct, 4*stat_cnt, 1] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km))$estimate) #Pearson
          corr_mat[anal_ct, 4*stat_cnt, 2] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km))$p.value) #Pearson
          corr_mat[anal_ct, 4*stat_cnt, 3] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km),method = "spearman")$estimate) #Spearman
          corr_mat[anal_ct, 4*stat_cnt, 4] = as.numeric(cor.test(log(wq.distance_3km_d$daily_mean), log(wq.distance_3km_d$num_well_3km),method = "spearman")$p.value) #Spearman
          corr_mat[anal_ct, 4*stat_cnt, 5] = length(wq.distance_3km_d$num_well_3km) #Nr of samples
          corr_mat[anal_ct, 4*stat_cnt, 6] = as.numeric(skewness(log(wq.distance_3km_d$num_well_3km)))
          
          
        }}
      #}
    }
  }
}


#corr_NewYork = corr_mat
corrplot(corr_NewYork[,,1], method = 'color',is.corr = FALSE)
corrplot(corr_NewYork[,,3], method = 'color',is.corr = FALSE)

corr_NewYork_e = corr_NewYork#[1:24,,]
for (i in seq(1,6)){
  corr_NewYork_e[,,i] = rbind(as.matrix(corr_NewYork[1:12,,i]), 
                         (matrix(NA, nrow = 6, ncol=4)),
                         as.matrix(corr_NewYork[13:24,,i]), 
                         (matrix(NA, nrow = 6, ncol=4)))
}

corrplot(corr_NewYork_e[,,1], method = 'color',is.corr = FALSE)


#corr_Colorado = corr_mat
corrplot(corr_Colorado[,,1], method = 'color',is.corr = FALSE)
corrplot(corr_Colorado[,,3], method = 'color',is.corr = FALSE)


#corr_Pennsylvania = corr_mat
corrplot(corr_Pennsylvania[,,1], method = 'color',is.corr = FALSE)
corrplot(corr_Pennsylvania[,,3], method = 'color',is.corr = FALSE)


#corr_Texas = corr_mat
corrplot(corr_Texas[,,1], method = 'color',is.corr = FALSE)
corrplot(corr_Texas[,,3], method = 'color',is.corr = FALSE)


F_corr_matrix_TX_CO_PA_NY_filt = as.matrix(cbind(corr_Texas[,,1], corr_Colorado[,,1], corr_Pennsylvania[,,1],corr_NewYork_e[,,1]))
F_corr_matrix_TX_CO_PA_NY_filt_dir = F_corr_matrix_TX_CO_PA_NY_filt
F_corr_matrix_TX_CO_PA_NY_filt_dir[,c(3,4,7,8,11,12,15,16)] = F_corr_matrix_TX_CO_PA_NY_filt[,c(3,4,7,8,11,12,15,16)] * (-1)

corrplot(F_corr_matrix_TX_CO_PA_NY_filt_dir, method = 'color',is.corr = FALSE)



#Creating a matrix with all 4 states
corr_matrix_all4 = as.matrix(cbind(corr_Texas[,,1], corr_Colorado[,,1], corr_Pennsylvania[,,1],corr_NewYork_e[,,1]))

#P-values (for Pearson)
F_corr_matrix_TX_CO_PA_NY_filt_p = as.matrix(cbind(corr_Texas[,,2], corr_Colorado[,,2], corr_Pennsylvania[,,2],corr_NewYork_e[,,2]))


#Keeping only significant values 
cnt = 0
F_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir = F_corr_matrix_TX_CO_PA_NY_filt_dir
for (i in seq(1:dim(F_corr_matrix_TX_CO_PA_NY_filt_dir)[1])){
  for (j in seq(1:dim(F_corr_matrix_TX_CO_PA_NY_filt_dir)[2])){
    if (!is.na(as.numeric(F_corr_matrix_TX_CO_PA_NY_filt_p[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(F_corr_matrix_TX_CO_PA_NY_filt_p[i,j]) > 0.005){
        F_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}
corrplot(F_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir, method = 'color',is.corr = FALSE)



#Creating a matrix with all 4 states
corr_matrix_all4 = as.matrix(rbind(F_corr_matrix_TX_CO_PA_NY_only_sig_dir[,1:4], 
                                   F_corr_matrix_TX_CO_PA_NY_only_sig_dir[,5:8],
                                   F_corr_matrix_TX_CO_PA_NY_only_sig_dir[,9:12],
                                   F_corr_matrix_TX_CO_PA_NY_only_sig_dir[,13:16]))
A = rep(rep(c("Ba", "Cl","Na", "SC", "SO4", "Sr"),6),4)
B = rep(c(rep("marginal",6),rep("orphaned",6),rep("unconventional",6)),4*2)
C = rep(c(rep("GW",18),rep("SW",18)),4)
S = c(rep("TX",36),rep("CO",36),rep("PA",36),rep("NY",36))
                                                       
corr_matrix_all4_DF = data.frame(S,C,B,A,corr_matrix_all4)
colnames(corr_matrix_all4_DF) = c("State","Water","Well","Analyte","near_d","sum_d","den_1km","den_3km")

#Effects by well type
corr_matrix_all4_DF_well_effects = corr_matrix_all4_DF %>%
  group_by(State,Water,Well) %>%
  summarise(mean_R = mean(sum(near_d,sum_d,den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d,den_1km,den_3km,NA)))),na.rm = TRUE))
  #summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))

corr_matrix_all4_DF_well_effects_2 = corr_matrix_all4_DF %>%
  group_by(State,Water,Well) %>%
  summarise(mean_R_dist = mean(sum(near_d,sum_d,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d)))),na.rm = TRUE),
            mean_R_dens = mean(sum(den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_well_effects_2[,4:5]), method = 'color',is.corr = FALSE)


corr_matrix_all4_DF_well_effects_3 = corr_matrix_all4_DF %>%
  group_by(State,Well) %>%
  summarise(mean_R_dist = mean(sum(near_d,sum_d,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d)))),na.rm = TRUE),
            mean_R_dens = mean(sum(den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_well_effects_3[,3:4]), method = 'color',is.corr = FALSE)

corr_matrix_all4_DF_well_effects_4 = corr_matrix_all4_DF %>%
  group_by(State,Water,Well) %>%
  summarise(mean_R = mean(sum(near_d,sum_d,den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d,den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_well_effects_4[,4]), method = 'color',is.corr = FALSE)

corr_matrix_all4_DF_well_effects_5 = corr_matrix_all4_DF %>%
  group_by(Well,State,Water) %>%
  summarise(mean_R = mean(sum(near_d,sum_d,den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d,den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_well_effects_5[,4]), method = 'color',is.corr = FALSE)

corr_matrix_all4_DF_well_effects_6 = corr_matrix_all4_DF %>%
  group_by(Well,State,Water) %>%
  summarise(mean_R_dist = mean(sum(near_d,sum_d,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d)))),na.rm = TRUE),
            mean_R_dens = mean(sum(den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_well_effects_6[,4:5]), method = 'color',is.corr = FALSE)

#By analyte
corr_matrix_all4_DF_analyte_effects_1 = corr_matrix_all4_DF %>%
  group_by(Analyte,State,Water) %>%
  summarise(mean_R_dist = mean(sum(near_d,sum_d,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d)))),na.rm = TRUE),
            mean_R_dens = mean(sum(den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_analyte_effects_1[,4:5]), method = 'color',is.corr = FALSE)

corr_matrix_all4_DF_analyte_effects_2 = corr_matrix_all4_DF %>%
  group_by(Analyte) %>%
  summarise(min_R = min(near_d,sum_d,den_1km,den_3km,na.rm = TRUE),
            max_R = max(near_d,sum_d,den_1km,den_3km,na.rm = TRUE))
corr_matrix_all4_DF_analyte_effects_2$range = corr_matrix_all4_DF_analyte_effects_2$max_R - corr_matrix_all4_DF_analyte_effects_2$min_R 
corrplot(as.matrix(corr_matrix_all4_DF_analyte_effects_2[,2:3]), method = 'color',is.corr = FALSE)


#By water
corr_matrix_all4_DF_water_effects_1 = corr_matrix_all4_DF %>%
  group_by(Water,State) %>%
  summarise(min_R = min(near_d,sum_d,den_1km,den_3km,na.rm = TRUE),
            max_R = max(near_d,sum_d,den_1km,den_3km,na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_water_effects_1[,3:4]), method = 'color',is.corr = FALSE)

corr_matrix_all4_DF_water_effects_2 = corr_matrix_all4_DF %>%
  group_by(Water,State) %>%
  summarise(mean_R_dist = mean(sum(near_d,sum_d,na.rm = TRUE)/length(which(!is.na(c(near_d,sum_d)))),na.rm = TRUE),
            mean_R_dens = mean(sum(den_1km,den_3km,na.rm = TRUE)/length(which(!is.na(c(den_1km,den_3km)))),na.rm = TRUE))
#summarise(mean_R = mean((near_d+sum_d+den_1km+den_3km)/4))
corrplot(as.matrix(corr_matrix_all4_DF_water_effects_2[,3:4]), method = 'color',is.corr = FALSE)




########
#SPEARMAN

S_corr_matrix_TX_CO_PA_NY_filt = as.matrix(cbind(corr_Texas[,,3], corr_Colorado[,,3], corr_Pennsylvania[,,3],corr_NewYork_e[,,3]))
S_corr_matrix_TX_CO_PA_NY_filt_dir = S_corr_matrix_TX_CO_PA_NY_filt
S_corr_matrix_TX_CO_PA_NY_filt_dir[,c(3,4,7,8,11,12,15,16)] = S_corr_matrix_TX_CO_PA_NY_filt[,c(3,4,7,8,11,12,15,16)] * (-1)

corrplot(S_corr_matrix_TX_CO_PA_NY_filt_dir, method = 'color',is.corr = FALSE)


#P-values (for Pearson)
S_corr_matrix_TX_CO_PA_NY_filt_p = as.matrix(cbind(corr_Texas[,,4], corr_Colorado[,,4], corr_Pennsylvania[,,4],corr_NewYork_e[,,4]))


#Keeping only significant values 

cnt = 0
S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir = S_corr_matrix_TX_CO_PA_NY_filt_dir
for (i in seq(1:dim(S_corr_matrix_TX_CO_PA_NY_filt_dir)[1])){
  for (j in seq(1:dim(S_corr_matrix_TX_CO_PA_NY_filt_dir)[2])){
    if (!is.na(as.numeric(S_corr_matrix_TX_CO_PA_NY_filt_p[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(S_corr_matrix_TX_CO_PA_NY_filt_p[i,j]) > 0.005){
        S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}
corrplot(S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir, method = 'color',is.corr = FALSE)

S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir_GW = S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir[1:18,] ##GW
100 - ((sum(apply(S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir_GW, 2, count_NAs)) -24)/ Total_nr *100) #Percent significant
S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir_SW = S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir[19:36,] ##SW
100 - ((sum(apply(S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir_SW, 2, count_NAs)) -24)/ Total_nr *100) #Percent significant

#For NY
100 - ((sum(apply(S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir_GW[c(1:12),13:16], 2, count_NAs))/ (4*12)))*100
100 - ((sum(apply(S_corr_matrix_TX_CO_PA_NY_filt_only_sig_dir_SW[c(1:12),13:16], 2, count_NAs))/ (4*12)))*100



#Comparing impact from different well types

#######################################
#######################################
## FIGURE 3
########################################
######################################

F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW = F_corr_matrix_TX_CO_PA_NY_only_sig_dir[1:18,] ##GW
corrplot(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, method = 'color',is.corr = FALSE)
#How many significant?
count_NAs = function(x) {
  return(length(which(is.na(x))))
}
#How many NA values per state and proxy? 
Total_nr = 18*16 - 6*4 #Total Nr
sum(apply(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, 2, count_NAs)) 
(sum(apply(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, 2, count_NAs)) -24)/ Total_nr *100 #percent of NAs
100 - ((sum(apply(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, 2, count_NAs)) -24)/ Total_nr *100) #Percent significant
#For NY
100 - ((sum(apply(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW[c(1:12),13:16], 2, count_NAs))/ (4*12)))*100

F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW = F_corr_matrix_TX_CO_PA_NY_only_sig_dir[19:36,] ##SW
corrplot(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, method = 'color',is.corr = FALSE)
100 - ((sum(apply(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, 2, count_NAs)) -24)/ Total_nr *100) #Percent significant
#For NY
100 - ((sum(apply(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[c(1:12),13:16], 2, count_NAs))/ (4*12)))*100

library(ramify)

# SURFACE
SW_all_corr_values = resize(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, 288, 1, across = "columns", byrow=TRUE)
#corrplot(corr_matrix_TX_CO_PA_only_sig_SW, method = 'color')
X_vals = c(rep(0.8, 18),rep(0.9, 18),rep(1, 18),rep(1.1, 18),
           rep(1.8, 18),rep(1.9, 18),rep(2, 18),rep(2.1, 18),
           rep(2.8, 18),rep(2.9, 18),rep(3, 18),rep(3.1, 18),
           rep(3.8, 18),rep(3.9, 18),rep(4, 18),rep(4.1, 18))

SW_matrix = data.frame(cbind(X_vals,SW_all_corr_values))
colnames(SW_matrix) = c("X_cord", "Corr_coef")
SW_matrix$Analyte = rep(c("Ba","Cl","Na","SC","SO4","Sr"),48)
SW_matrix$Well_type = rep(c(rep("Marginal",6), rep("Orphaned",6), rep("Unconv",6)),16)

#GGplot
ggplot(SW_matrix, aes(x = X_cord, y = Corr_coef, shape = Well_type, fill = Analyte)) +
  geom_point(size = 3, color = "black",alpha = 0.7) + 
  scale_shape_manual(values = c(22, 21, 24)) + 
  scale_fill_manual(values = c( "#a6cee3", "#1f78b4", "#b2df8a","#33a02c","#fdbf6f","#ff7f00"  )) +
  ylim(-0.65, 0.65) +
  theme_bw() 
  

length(which(SW_matrix[,2]>0)) #62
length(which(SW_matrix[,2]<0)) #156
length(which(SW_matrix[,2]< -0.3)) #43
length(which(is.na(SW_matrix[,2]))) #70
288 #Total
156/(288-70) #72%
43/(288-70)

length(which(SW_matrix[,2]<0)) #156

18*4 #Texas 1-72; Colorado 73-144; Pennsylvania 145-216; NY 217-288
length(which(SW_matrix[,2]< -0.3)) 
42/(288-24)*100
length(which(SW_matrix[1:72,2]< -0.3))
length(which(SW_matrix[73:144,2]< -0.3))
length(which(SW_matrix[145:216,2]< -0.3))
length(which(SW_matrix[217:288,2]< -0.3))

100 - (length(which(is.na(SW_matrix[1:72,2])))/(18*4))*100 #83.3%
100 - (length(which(is.na(SW_matrix[73:144,2])))/(18*4))*100 #84.7
100 - (length(which(is.na(SW_matrix[145:216,2])))/(18*4))*100 #86.1
100 - ((length(which(is.na(SW_matrix[217:288,2])))-24)/(18*4))*100 #84.7

#PA
100 - (length(which(GW_matrix[145:216,2]< -0.3))/(18*4))*100 #83.3
(length(which(SW_matrix[145:216,2]< 0))/(72))*100 #83.3

#Comparing proxies
SW_all_corr_values_dist_proxy = resize(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[,c(1,2,5,6,9,10,13,14)], 144, 1, across = "columns", byrow=TRUE)
SW_all_corr_values_dens_proxy = resize(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[,c(3,4,7,8,11,12,15,16)], 144, 1, across = "columns", byrow=TRUE)

length(which(is.na(SW_all_corr_values_dist_proxy))) #83
length(which(is.na(SW_all_corr_values_dens_proxy))) 




#GROUND
GW_all_corr_values = resize(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, 288, 1, across = "columns", byrow=TRUE)
# X_vals = c(rep(0.8, 18),rep(0.9, 18),rep(1, 18),rep(1.1, 18),
#            rep(1.8, 18),rep(1.9, 18),rep(2, 18),rep(2.1, 18),
#            rep(2.8, 18),rep(2.9, 18),rep(3, 18),rep(3.1, 18),
#            rep(3.8, 18),rep(3.9, 18),rep(4, 18),rep(4.1, 18))

GW_matrix = data.frame(cbind(X_vals,GW_all_corr_values))
colnames(GW_matrix) = c("X_cord", "Corr_coef")
GW_matrix$Analyte = rep(c("Ba","Cl","Na","SC","SO4","Sr"),48)
GW_matrix$Well_type = rep(c(rep("Marginal",6), rep("Orphaned",6), rep("Unconv",6)),16)

#GGplot
ggplot(GW_matrix, aes(x = X_cord, y = Corr_coef, shape = Well_type, fill = Analyte)) +
  geom_point(size = 3, color = "black",alpha = 0.7) + 
  scale_shape_manual(values = c(22, 21, 24)) + 
  scale_fill_manual(values = c( "#a6cee3", "#1f78b4", "#b2df8a","#33a02c","#fdbf6f","#ff7f00"  )) +
  ylim(-0.65, 0.65) +
  theme_bw() 

length(which(GW_matrix[,2]>0)) #65
length(which(GW_matrix[,2]<0)) #140
length(which(is.na(GW_matrix[,2]))) #83
140/(288-83) #72%
28/(288-83)

length(which(GW_matrix[,2]< -0.3)) 
31/(288-24)*100
length(which(GW_matrix[1:72,2]< -0.3))
length(which(GW_matrix[73:144,2]< -0.3))
length(which(GW_matrix[145:216,2]< -0.3))
length(which(GW_matrix[217:288,2]< -0.3))

100 - (length(which(is.na(GW_matrix[1:72,2])))/(18*4))*100 #66.7%
100 - (length(which(is.na(GW_matrix[73:144,2])))/(18*4))*100 #91.7
100 - (length(which(is.na(GW_matrix[145:216,2])))/(18*4))*100 #83.3
100 - ((length(which(is.na(GW_matrix[217:288,2])))-24)/(18*4))*100 #73.6

#PA
100 - (length(which(GW_matrix[145:216,2]< -0.3))/(18*4))*100 #83.3
(length(which(GW_matrix[145:216,2]< 0))/(72))*100 #83.3
5/72*100


#Comparing proxies
GW_all_corr_values_dist_proxy = resize(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW[,c(1,2,5,6,9,10,13,14)], 144, 1, across = "columns", byrow=TRUE)
GW_all_corr_values_dens_proxy = resize(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW[,c(3,4,7,8,11,12,15,16)], 144, 1, across = "columns", byrow=TRUE)

length(which(is.na(GW_all_corr_values_dist_proxy))) #83
length(which(is.na(GW_all_corr_values_dens_proxy))) 

#for dist proxy - surface + ground
100 - (24+15)/(288-24)*100
(length(which(GW_all_corr_values_dist_proxy<0)))/(144-12)*100
(length(which(SW_all_corr_values_dist_proxy<0)))/(144-12)*100
#for dens proxy - surface + ground
100 - (61+53)/(288-24)*100


####
#Checking which values should be removed 

# GROUND 

F_P_SW_GW_CO_t1 = F_P_SW_GW_CO %>%
  filter(well_type == "orphaned",
         type_w == "GW",
         Analyte == "Sr",
         num_well_1km > 0)
plot(log(F_P_SW_GW_CO_t1$num_well_1km), log(F_P_SW_GW_CO_t1$daily_mean))


F_P_SW_GW_CO_t1 = F_P_SW_GW_CO %>%
  filter(well_type == "orphaned",
         type_w == "GW",
         Analyte == "SO4",
         num_well_1km > 0)
plot(log(F_P_SW_GW_CO_t1$num_well_1km), log(F_P_SW_GW_CO_t1$daily_mean))
unique(F_P_SW_GW_CO_t1$num_well_1km)
cor(log(F_P_SW_GW_CO_t1$num_well_1km), log(F_P_SW_GW_CO_t1$daily_mean))

F_P_SW_GW_CO_t1 = F_P_SW_GW_CO %>%
  filter(well_type == "orphaned",
         type_w == "GW",
         Analyte == "SO4",
         num_well_1km > 0,
         num_well_1km < 7)
plot(log(F_P_SW_GW_CO_t1$num_well_1km), log(F_P_SW_GW_CO_t1$daily_mean))
cor(log(F_P_SW_GW_CO_t1$num_well_1km), log(F_P_SW_GW_CO_t1$daily_mean))


F_P_SW_GW_CO_t1 = F_P_SW_GW_CO %>%
  filter(well_type == "orphaned",
         type_w == "GW",
         Analyte == "Sr",
         num_well_3km > 0)
plot(log(F_P_SW_GW_CO_t1$num_well_3km), log(F_P_SW_GW_CO_t1$daily_mean))

F_P_SW_GW_CO_t1 = F_P_SW_GW_CO %>%
  filter(well_type == "orphaned",
         type_w == "GW",
         Analyte == "SO4",
         num_well_3km > 0)
plot(log(F_P_SW_GW_CO_t1$num_well_3km), log(F_P_SW_GW_CO_t1$daily_mean))
cor(log(F_P_SW_GW_CO_t1$num_well_3km), log(F_P_SW_GW_CO_t1$daily_mean))

F_P_SW_GW_CO_t1 = F_P_SW_GW_CO %>%
  filter(well_type == "marginal",
         type_w == "GW",
         Analyte == "Sr",
         num_well_3km > 0)
plot(log(F_P_SW_GW_CO_t1$num_well_3km), log(F_P_SW_GW_CO_t1$daily_mean))



F_P_SW_GW_PA_t1 = F_P_SW_GW_PA %>%
  filter(well_type == "orphaned",
         type_w == "GW",
         Analyte == "Ba",
         num_well_3km > 0)
plot(log(F_P_SW_GW_PA_t1$num_well_3km), log(F_P_SW_GW_PA_t1$daily_mean))
cor(log(F_P_SW_GW_PA_t1$num_well_3km), log(F_P_SW_GW_PA_t1$daily_mean))



#SURFACE
F_P_SW_GW_TX_t1 = F_P_SW_GW_TX %>%
  filter(well_type == "orphaned",
         type_w == "SW",
         Analyte == "Ba",
         num_well_1km > 0)
plot(log(F_P_SW_GW_TX_t1$num_well_1km), log(F_P_SW_GW_TX_t1$daily_mean))
cor(log(F_P_SW_GW_TX_t1$num_well_3km), log(F_P_SW_GW_TX_t1$daily_mean))

F_P_SW_GW_TX_t1 = F_P_SW_GW_TX %>%
  filter(well_type == "unconventional",
         type_w == "SW",
         Analyte == "Sr",
         num_well_1km > 0)
plot(log(F_P_SW_GW_TX_t1$num_well_1km), log(F_P_SW_GW_TX_t1$daily_mean))

F_P_SW_GW_TX_t1 = F_P_SW_GW_TX %>%
  filter(well_type == "marginal",
         type_w == "SW",
         Analyte == "Na",
         num_well_1km > 0)
plot(log(F_P_SW_GW_TX_t1$num_well_1km), log(F_P_SW_GW_TX_t1$daily_mean))

F_P_SW_GW_NY_t1 = F_P_SW_GW_NY %>%
  filter(well_type == "orphaned",
         type_w == "SW",
         Analyte == "Sr",
         num_well_3km > 0)
plot(log(F_P_SW_GW_NY_t1$num_well_3km), log(F_P_SW_GW_NY_t1$daily_mean))




#######################################
#######################################
## FIGURE 6
########################################
######################################

#CONTINUE ANALYZING correlations without distance filter - F_corr_matrix_TX_CO_PA_NY_only_sig_dir


#Add labels
#Analyzing GW and SW separately, apply clustering
#Color by marginal vs unconventional vs orphaned
well_col = c( "#d8b365", "#d8b365","#d8b365","#d8b365","#d8b365","#d8b365",
              "#999999","#999999","#999999","#999999","#999999","#999999",
              "#5ab4ac","#5ab4ac","#5ab4ac","#5ab4ac","#5ab4ac","#5ab4ac")

#F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW = F_corr_matrix_TX_CO_PA_NY_only_sig_dir[1:18,] ##GW
#F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW = F_corr_matrix_TX_CO_PA_NY_only_sig_dir[19:36,] ##SW

rownames(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW) = c("Ba", "Cl","Na", "SC", "SO4", "Sr",
                                                        "Ba", "Cl","Na", "SC", "SO4", "Sr",
                                                        "Ba", "Cl","Na", "SC", "SO4", "Sr")
rownames(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW) = c("Ba", "Cl","Na", "SC", "SO4", "Sr",
                                                        "Ba", "Cl","Na", "SC", "SO4", "Sr",
                                                        "Ba", "Cl","Na", "SC", "SO4", "Sr")


#heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, col=col2, scale = "none",RowSideColors=well_col ) #SW
#heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, col=col2, scale = "none",RowSideColors=well_col ) #GW

#Maybe do not cluster on columns?
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, Colv = NA, col=col2, scale = "none",RowSideColors=well_col ) #SW
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, Colv = NA,, col=col2, scale = "none",RowSideColors=well_col ) #GW

state_col = c("#ff7f00", "#ff7f00","#ff7f00","#ff7f00",#Texas
              "#4daf4a","#4daf4a","#4daf4a","#4daf4a", #CO
              "#984ea3", "#984ea3","#984ea3","#984ea3",#Pennsylvania
              "#e7298a","#e7298a","#e7298a","#e7298a") #NY

#heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, col=col2, scale = "none",ColSideColors = state_col, RowSideColors=well_col ) #SW
#heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, col=col2, scale = "none", ColSideColors = state_col, RowSideColors=well_col ) #GW

heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, Colv = NA, col=col2, scale = "none",ColSideColors = state_col, RowSideColors=well_col ) #SW
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW, Colv = NA,, col=col2, scale = "none", ColSideColors = state_col, RowSideColors=well_col ) #GW

#Without NY
#What if we removed NY? It seems like it's affecting clustering in SW results
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[,1:12], Colv = NA, col=col2, scale = "none",ColSideColors = state_col[1:12], RowSideColors=well_col2 ) #SW
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW[,1:12], Colv = NA,, col=col2, scale = "none", ColSideColors = state_col[1:12], RowSideColors=well_col2 ) #GW

#Get color legend/ colorbar
corrplot(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[,1:12], method = 'color',is.corr = FALSE)
corrplot(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW[,1:12], method = 'color',is.corr = FALSE)


corrplot(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, order = 'hclust', addrect = 2,is.corr = FALSE)

color_palette_test <- colorRampPalette(c("blue", "white", "red"))(100)
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[,1:12], col = color_palette_test)
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, Colv = NA, col=col2, scale = "none",ColSideColors = state_col, RowSideColors=well_col ) #SW
col2_test <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(50)
heatmap(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW, Colv = NA, col=col2_test, scale = "none",ColSideColors = state_col, RowSideColors=well_col ) #SW

#Artificially add 1 and -1 to expand color palette to include the whole range
test_color = F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW
test_color[1,1] = 1
test_color[2,1] = -1

corrplot(test_color, method = 'color',is.corr = FALSE)

test_color = F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW
test_color[6,5] = -0.63
test_color[18,3] = 0.63
heatmap(test_color[,1:12], Colv = NA, col=col2, scale = "none",ColSideColors = state_col[1:12], RowSideColors=well_col2 ) #SW
corrplot(test_color, method = 'color',is.corr = FALSE)
F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW[18,3]

test_color = F_corr_matrix_TX_CO_PA_NY_only_sig_dir_SW
test_color[6,5] = -0.63
test_color[18,3] = 0.55
heatmap(test_color[,1:12], Colv = NA, col=col2, scale = "none",ColSideColors = state_col[1:12], RowSideColors=well_col2 ) #SW
corrplot(test_color, method = 'color',is.corr = FALSE)


View(F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW)
test_color2 = F_corr_matrix_TX_CO_PA_NY_only_sig_dir_GW
test_color2[10,12] = -0.63
test_color2[10,7] = 0.63
heatmap(test_color2[,1:12], Colv = NA, col=col2, scale = "none",ColSideColors = state_col[1:12], RowSideColors=well_col2 ) #GW
corrplot(test_color2, method = 'color',is.corr = FALSE)




##############################################
#PARKING LOT

# wq.distance_100km <- geospatial_results_1_short %>%
#   filter(closest_well_dist_sum < 100000)
# 
# wq.distance_10km <- geospatial_results_1_short %>%
#   filter(nearest_distance_m < 10000 & nearest_distance_m > 1)
# 
# wq.distance_1km_d <- geospatial_results_1_short %>%
#   filter(num_well_1km >0)
# 
# wq.distance_3km_d <- geospatial_results_1_short %>%
#   filter(num_well_3km >0)


F_P_SW_GW_CO_filt = Geospatial_result_CO_all %>%
  filter(closest_well_dist_sum < 100000,
         nearest_distance_m < 10000, nearest_distance_m > 1,
         num_well_1km >0,
         num_well_3km >0)
F_P_SW_GW_TX_filt = Geospatial_result_TX_all %>%
  filter(closest_well_dist_sum < 100000,
         nearest_distance_m < 10000, nearest_distance_m > 1,
         num_well_1km >0,
         num_well_3km >0)
F_P_SW_GW_PA_filt = Geospatial_result_PA_all %>%
  filter(closest_well_dist_sum < 100000,
         nearest_distance_m < 10000, nearest_distance_m > 1,
         num_well_1km >0,
         num_well_3km >0)
F_P_SW_GW_NY_filt = Geospatial_result_NY_all %>%
  filter(closest_well_dist_sum < 100000,
         nearest_distance_m < 10000, nearest_distance_m > 1,
         num_well_1km >0,
         num_well_3km >0)


########################################
# Calculating correlation values

F_P_SW_GW_CO_filt_corr = F_P_SW_GW_CO_filt %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_TX_filt_corr = F_P_SW_GW_TX_filt %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_PA_filt_corr = F_P_SW_GW_PA_filt %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean[which(nearest_distance_m > 1)]), log(nearest_distance_m[which(nearest_distance_m > 1)])), 
            sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_NY_filt_corr = F_P_SW_GW_NY_filt %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))

F_P_SW_GW_NY_filt_corr_unc = rbind(as.matrix(F_P_SW_GW_NY_filt_corr[1:12,4:7]), 
                              (matrix(NA, nrow = 6, ncol=4)),
                              as.matrix(F_P_SW_GW_NY_filt_corr[13:24,4:7]), 
                              (matrix(NA, nrow = 6, ncol=4)))

F_corr_matrix_TX_CO_PA_NY_filt = as.matrix(cbind(F_P_SW_GW_TX_filt_corr[,4:7], F_P_SW_GW_CO_filt_corr[,4:7], F_P_SW_GW_PA_filt_corr[,4:7],F_P_SW_GW_NY_filt_corr_unc))
F_corr_matrix_TX_CO_PA_NY_filt_dir = F_corr_matrix_TX_CO_PA_NY_filt
F_corr_matrix_TX_CO_PA_NY_filt_dir[,c(3,4,7,8,11,12,15,16)] = F_corr_matrix_TX_CO_PA_NY_filt[,c(3,4,7,8,11,12,15,16)] * (-1)

corrplot(F_corr_matrix_TX_CO_PA_NY_filt_dir, method = 'color',is.corr = FALSE)















