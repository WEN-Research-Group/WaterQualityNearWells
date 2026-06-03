#Spatial Permutation Test

# Picking state/water type/analyte/well type
S_t = "PA"
well_t = "marginal"
water_t = "GW"
analyte = "Sr"

analyte_data <- readRDS(paste("Geospatial_results_",S_t,"_",analyte,".rds",sep=""))

#Isolate SW and GW data separately, keep only one set of wells since concentrations will be same
analyte_data_W = analyte_data %>%
  filter(type_w == water_t, 
         well_type == well_t)

full_data = analyte_data_W

permutation_test <- function(analyte_vals, dist_vals, n_permutations = 999) {
  
  # Observed correlation
  complete  <- !is.na(analyte_vals) & !is.na(dist_vals)
  observed_r <- cor(dist_vals[complete], analyte_vals[complete])
  
  # Permutation distribution — shuffle well distances
  perm_r <- replicate(n_permutations, {
    shuffled_dist <- sample(dist_vals[complete])
    cor(shuffled_dist, analyte_vals[complete])
  })
  
  # Permutation p-value
  p_perm <- mean(abs(perm_r) >= abs(observed_r))
  
  
  list(
    observed_r     = observed_r,
    perm_r         = perm_r,         # full null distribution
    p_permutation  = p_perm,
    n_permutations = n_permutations
  )
}

#Test one analyte at a time
result <- permutation_test(log(full_data$daily_mean), log(full_data$nearest_distance_m))
#For PA
result <- permutation_test(log(full_data$daily_mean[which(full_data$nearest_distance_m > 1)]), log(full_data$nearest_distance_m[which(full_data$nearest_distance_m > 1)]))

result$observed_r
min(result$perm_r)
max(result$perm_r)
result$p_permutation

#After 999 shuffles you have 999 correlations that represent what the relationship 
#looks like when well locations are random. You can visualize this:

# Plot null distribution with observed r marked
hist(result$perm_r, breaks = 40, 
     main = "Permutation null distribution — Cl vs. marginal well distance",
     xlab = "Pearson r (permuted)",
     col  = "lightgrey", border = "white")
abline(v = result$observed_r, col = "red", lwd = 2)
legend("topright", legend = paste("Observed r =", round(result$observed_r, 3)),
       col = "red", lwd = 2)


###########################################################
#Loop
well_t = "unconventional"
water_t = "GW"
S_t_l = c("TX","CO","PA") # c("TX","CO","PA","NY")
analyte_l = c("Ba","Cl","Na","SC","SO4","Sr")
Perm_matrix = data.frame(matrix(NA, nrow = length(S_t_l)*length(analyte_l), ncol = 7))
colnames(Perm_matrix) = c("State","Analyte","Pearson R","Min","Max","p-permut.","N")
cnt = 0

#Rerun PA because of NAs
#S_t_l = "PA"

for (i in S_t_l){
  for(j in analyte_l){
    cnt = cnt + 1
    
    #i = "NY"
    #j = "Cl"
    
    analyte_data <- readRDS(paste("Geospatial_results_",i,"_",j,".rds",sep=""))
    
    #Isolate SW and GW data separately, keep only one set of wells since concentrations will be same
    analyte_data_W = analyte_data %>%
      filter(type_w == water_t, 
             well_type == well_t)
    
    full_data = analyte_data_W
    
    result <- permutation_test(log(full_data$daily_mean), log(full_data$nearest_distance_m))
    #result <- permutation_test(log(full_data$daily_mean[which(full_data$nearest_distance_m > 1)]), log(full_data$nearest_distance_m[which(full_data$nearest_distance_m > 1)]))
    
    Perm_matrix[cnt,1] = i
    Perm_matrix[cnt,2] = j
    Perm_matrix[cnt,3] = result$observed_r
    Perm_matrix[cnt,4] = min(result$perm_r)
    Perm_matrix[cnt,5] = max(result$perm_r)
    Perm_matrix[cnt,6] = result$p_permutation
    Perm_matrix[cnt,7] = dim(analyte_data)[1]
  }
}

#Surf_results = Perm_matrix
#Ground_results = Perm_matrix

Surf_results$W_type = "SW"
Ground_results$W_type = "GW"

#Merge
Permutation_SW_GW = rbind(Ground_results, Surf_results)

#Save results
#write.csv(Permutation_SW_GW, "Permutation_SW_GW_unconv.csv")
