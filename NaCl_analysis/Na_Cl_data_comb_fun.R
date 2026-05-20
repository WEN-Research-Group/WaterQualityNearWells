#Create a function that combines Na and Cl into one dataset based on
#their Lat/Long and sampling date
#Assume those measurements came from the same sample

Na_Cl_data_comb_fun <- function(S_t, well_t, water_t) {
  
  #Function inputs:
  #1) State
  #2) Well type
  #3) Water type
  
  # ── 1. Load your data ─────────────────────────────────────────────────────────
  #na_data <- readRDS("Geospatial_results_NY_Na.rds")
  #cl_data <- readRDS("Geospatial_results_NY_Cl.rds")
  na_data <- readRDS(paste("Geospatial_results_",S_t,"_Na.rds",sep=""))
  cl_data <- readRDS(paste("Geospatial_results_",S_t,"_Cl.rds",sep=""))
  
  #Isolate SW and GW data separately, keep only one set of wells since concentrations will be same
  na_data_SW = na_data %>%
    filter(type_w == water_t, 
           well_type == well_t)
  
  cl_data_SW = cl_data %>%
    filter(type_w == water_t, 
           well_type == well_t)
  
  # ── 2. Join on location + date ────────────────────────────────────────────────
  cl_na_SW <- inner_join(
    na_data_SW  %>% select(Latitude, Longitude, SiteCode, Samplingdate, na_conc  = daily_mean),
    cl_data_SW  %>% select(Latitude, Longitude, SiteCode, Samplingdate, cl_conc  = daily_mean),
    by = c("Latitude", "Longitude","SiteCode", "Samplingdate")
  )
  
  # ── 3. Compute molar Na/Cl ratio ──────────────────────────────────────────────
  cl_na_SW <- cl_na_SW %>%
    mutate(
      na_mmol = na_conc / 22.99,
      cl_mmol = cl_conc / 35.45,
      na_cl_molar = na_mmol / cl_mmol
    )
  
  # ── 4. Add distances ──────────────────────────────────────────────
  cl_na_SW_dist <- cl_na_SW %>%
    left_join(
      na_data_SW %>% select(Latitude, Longitude, SiteCode, Samplingdate, nearest_distance_m),
      by = c("Latitude", "Longitude","SiteCode", "Samplingdate")
    ) %>%
    select(Latitude, Longitude, SiteCode, Samplingdate, na_conc, cl_conc, na_mmol, cl_mmol, na_cl_molar, nearest_distance_m)
  
  #return(cl_na_SW_dist)
  
  
  # ── 5. Filter based on distance ──────────────────────────────────────────────
  #Some values are abnormally small so the plot looks weird
  cl_na_SW_dist_cor = cl_na_SW_dist %>%
    filter(na_cl_molar > 1E-02,
           nearest_distance_m < 10000)
  
  return(cl_na_SW_dist_cor)
  
}