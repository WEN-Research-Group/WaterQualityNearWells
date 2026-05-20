#CONFIDENCE INTERVAL FIGURE

#######################################
#######################################
## FIGURE S4
########################################
######################################
#Uploading Geospatial results

#OPEN CSVs
Geospatial_result_TX_all = rbind(Geospatial_results_TX_Ba, Geospatial_results_TX_Cl, Geospatial_results_TX_Na, Geospatial_results_TX_SC,
                                 Geospatial_results_TX_SO4, Geospatial_results_TX_Sr)
Geospatial_result_TX_all$Analyte = c(rep("Ba",dim(Geospatial_results_TX_Ba)[1]),rep("Cl",dim(Geospatial_results_TX_Cl)[1]),
                                     rep("Na",dim(Geospatial_results_TX_Na)[1]),rep("SC",dim(Geospatial_results_TX_SC)[1]),
                                     rep("SO4",dim(Geospatial_results_TX_SO4)[1]),rep("Sr",dim(Geospatial_results_TX_Sr)[1]))

Geospatial_result_TX_all_marginal = Geospatial_result_TX_all %>%
  filter(well_type == "marginal")

Geospatial_result_CO_all = rbind(Geospatial_results_CO_Ba, Geospatial_results_CO_Cl, Geospatial_results_CO_Na, Geospatial_results_CO_SC,
                                 Geospatial_results_CO_SO4, Geospatial_results_CO_Sr)
Geospatial_result_CO_all$Analyte = c(rep("Ba",dim(Geospatial_results_CO_Ba)[1]),rep("Cl",dim(Geospatial_results_CO_Cl)[1]),
                                     rep("Na",dim(Geospatial_results_CO_Na)[1]),rep("SC",dim(Geospatial_results_CO_SC)[1]),
                                     rep("SO4",dim(Geospatial_results_CO_SO4)[1]),rep("Sr",dim(Geospatial_results_CO_Sr)[1]))

Geospatial_result_PA_all = rbind(Geospatial_results_PA_Ba, Geospatial_results_PA_Cl, Geospatial_results_PA_Na, Geospatial_results_PA_SC,
                                 Geospatial_results_PA_SO4, Geospatial_results_PA_Sr)
Geospatial_result_PA_all$Analyte = c(rep("Ba",dim(Geospatial_results_PA_Ba)[1]),rep("Cl",dim(Geospatial_results_PA_Cl)[1]),
                                     rep("Na",dim(Geospatial_results_PA_Na)[1]),rep("SC",dim(Geospatial_results_PA_SC)[1]),
                                     rep("SO4",dim(Geospatial_results_PA_SO4)[1]),rep("Sr",dim(Geospatial_results_PA_Sr)[1]))

Geospatial_result_NY_all = rbind(Geospatial_results_NY_Ba, Geospatial_results_NY_Cl, Geospatial_results_NY_Na, Geospatial_results_NY_SC,
                                 Geospatial_results_NY_SO4, Geospatial_results_NY_Sr)
Geospatial_result_NY_all$Analyte = c(rep("Ba",dim(Geospatial_results_NY_Ba)[1]),rep("Cl",dim(Geospatial_results_NY_Cl)[1]),
                                     rep("Na",dim(Geospatial_results_NY_Na)[1]),rep("SC",dim(Geospatial_results_NY_SC)[1]),
                                     rep("SO4",dim(Geospatial_results_NY_SO4)[1]),rep("Sr",dim(Geospatial_results_NY_Sr)[1]))


F_P_SW_GW_CO = Geospatial_result_CO_all
F_P_SW_GW_TX = Geospatial_result_TX_all
F_P_SW_GW_PA = Geospatial_result_PA_all
F_P_SW_GW_NY = Geospatial_result_NY_all

#F_P_SW_GW_TX = Geospatial_result_TX_all_marginal

#backup
F_P_SW_GW_CO_no_filt =  F_P_SW_GW_CO
F_P_SW_GW_TX_no_filt =  F_P_SW_GW_TX
F_P_SW_GW_PA_no_filt =  F_P_SW_GW_PA
F_P_SW_GW_NY_no_filt =  F_P_SW_GW_NY

#With distance filter
F_P_SW_GW_CO = F_P_SW_GW_CO %>%
  filter(nearest_distance_m < 10000,
         closest_well_dist_sum < 100000)

F_P_SW_GW_TX = F_P_SW_GW_TX %>%
  filter(nearest_distance_m < 10000,
         closest_well_dist_sum < 100000)

F_P_SW_GW_PA = F_P_SW_GW_PA %>%
  filter(nearest_distance_m < 10000,
         closest_well_dist_sum < 100000)

F_P_SW_GW_NY = F_P_SW_GW_NY %>%
  filter(nearest_distance_m < 10000,
         closest_well_dist_sum < 100000)


F_P_SW_GW_CO = F_P_SW_GW_CO_no_filt
F_P_SW_GW_TX = F_P_SW_GW_TX_no_filt
F_P_SW_GW_PA = F_P_SW_GW_PA_no_filt
F_P_SW_GW_NY = F_P_SW_GW_NY_no_filt

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



##########################
#Confidense intervals
# corr_mat[anal_ct, 1*stat_cnt, 7] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m))$conf.int[1]) 
# corr_mat[anal_ct, 1*stat_cnt, 8] = as.numeric(cor.test(log(wq.distance_10km$daily_mean), log(wq.distance_10km$nearest_distance_m))$conf.int[2]) 


F_P_SW_GW_TX_conf_min = F_P_SW_GW_TX %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[1]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[1]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[1]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[1]))

F_P_SW_GW_TX_conf_max = F_P_SW_GW_TX %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[2]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[2]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[2]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[2]))

F_P_SW_GW_TX_conf_int = F_P_SW_GW_TX_conf_max[,4:7] - F_P_SW_GW_TX_conf_min[,4:7]


#CO
F_P_SW_GW_CO_conf_min = F_P_SW_GW_CO %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[1]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[1]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[1]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[1]))

F_P_SW_GW_CO_conf_max = F_P_SW_GW_CO %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[2]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[2]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[2]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[2]))

F_P_SW_GW_CO_conf_int = F_P_SW_GW_CO_conf_max[,4:7] - F_P_SW_GW_CO_conf_min[,4:7]


#PA
F_P_SW_GW_PA_conf_min = F_P_SW_GW_PA %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[1]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[1]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[1]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[1]))

F_P_SW_GW_PA_conf_max = F_P_SW_GW_PA %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[2]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[2]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[2]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[2]))

F_P_SW_GW_PA_conf_int = F_P_SW_GW_PA_conf_max[,4:7] - F_P_SW_GW_PA_conf_min[,4:7]


#NY
F_P_SW_GW_NY_conf_min = F_P_SW_GW_NY %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[1]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[1]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[1]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[1]))

F_P_SW_GW_NY_conf_max = F_P_SW_GW_NY %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = as.numeric(cor.test(log(daily_mean), log(nearest_distance_m))$conf.int[2]), 
            sum_d = as.numeric(cor.test(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs")$conf.int[2]),
            den_1km = as.numeric(cor.test(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)]))$conf.int[2]), 
            den_3km = as.numeric(cor.test(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)]))$conf.int[2]))

F_P_SW_GW_NY_conf_int = F_P_SW_GW_NY_conf_max[,4:7] - F_P_SW_GW_NY_conf_min[,4:7]



F_P_SW_GW_NY_conf_int_s = rbind(as.matrix(F_P_SW_GW_NY_conf_int[1:12,]), 
                                (matrix(NA, nrow = 6, ncol=4)),
                                as.matrix(F_P_SW_GW_NY_conf_int[13:24,]), 
                                (matrix(NA, nrow = 6, ncol=4)))


F_corr_matrix_TX_CO_PA_NY_conf = as.matrix(cbind(F_P_SW_GW_TX_conf_int, F_P_SW_GW_CO_conf_int, F_P_SW_GW_PA_conf_int,F_P_SW_GW_NY_conf_int_s))

corrplot(F_corr_matrix_TX_CO_PA_NY_conf, method = 'color',is.corr = FALSE)










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

sum(F_corr_matrix_TX_CO_PA_NY_only_sig_dir < -0.3, na.rm = TRUE)

sum(F_corr_matrix_TX_CO_PA_NY_only_sig_dir[1:6,13:16] < -0.3, na.rm = TRUE)
sum(F_corr_matrix_TX_CO_PA_NY_only_sig_dir[19:24,13:16] < -0.3, na.rm = TRUE)

##################################################
#Keeping only significant values 
#for confidense intervals

cnt = 0
F_corr_matrix_TX_CO_PA_NY_conf_only_sig_dir = F_corr_matrix_TX_CO_PA_NY_conf
for (i in seq(1:dim(F_corr_matrix_TX_CO_PA_NY_conf)[1])){
  for (j in seq(1:dim(F_corr_matrix_TX_CO_PA_NY_conf)[2])){
    if (!is.na(as.numeric(F_corr_matrix_TX_CO_PA_NY_p[i,j]))){
      #cnt= cnt + 1
      if (as.numeric(F_corr_matrix_TX_CO_PA_NY_p[i,j]) > 0.005){
        F_corr_matrix_TX_CO_PA_NY_conf_only_sig_dir[i,j] = NA
        cnt= cnt + 1 #105 values
      }
    }
  }
}
corrplot(F_corr_matrix_TX_CO_PA_NY_conf_only_sig_dir, method = 'color',is.corr = FALSE)

F_corr_matrix_TX_CO_PA_NY_conf_only_sig_dir_faking = F_corr_matrix_TX_CO_PA_NY_conf_only_sig_dir



