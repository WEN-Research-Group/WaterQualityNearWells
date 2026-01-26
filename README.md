# Oil and gas wells leave measurable impacts on U.S. surface and groundwater quality


This repository contains all data and code for the manuscript:

**Basijokaite, R., Kang, M., & Wen, T. (2026). Oil and gas wells leave measurable impacts on U.S. surface and groundwater quality. *Science Advances*, under review.**


## Project Overview

This project analyzes the impact of oil and gas wells on water quality across four U.S. states (New York, Pennsylvania, Colorado, and Texas). Using approximately 3 million water-quality records, we quantify 
relationships between well proximity/density and concentrations of production-associated analytes (barium, chloride, sodium, sulfate, strontium, and specific conductivity) in surface and ground water.
Our analysis reveals statistically significant water-quality degradation in all four states, with stronger signals in surface water (83.3% of correlations significant at p<0.005) than groundwater (76.9%). 
Regional analyses in Texas demonstrate that statewide assessments can obscure localized contamination patterns driven by hydrogeology and aquifer connectivity.

These findings establish that oil and gas infrastructure—including marginal and orphaned wells—leaves a measurable geochemical signature on water systems, emphasizing the importance of hydrogeology-informed 
monitoring strategies for both legacy and active sites.


## Repository Structure
```
├── Data/        # Cleaned and processed water quality data. Each file contains analyte sampling information and associated spatial proxy metrics 
│   ├── CO/                 
│   ├── NY/          
|   ├── PA/  
│   └── TX/         
├── Coding/
│   ├── Well_spatial_analysis_compl_0127.R   # Main function used in R scripts below to calculate 4 spatial proxies used in the manuscript
│   ├── [State]_by_well_type_[analyte].R     # Analysis was conducted separately for each state, analyte, water source and well type
└── README.md
```

## Data

### Raw Data 

Authors don't have permission to share raw data associated with oil and gas wells used in this analysis.

### Processed Data 

Water quality data underwent rigorous quality control before analysis. We retained only samples from natural surface water (rivers, streams) and groundwater (seeps, springs, wells) sources, excluding anthropogenic or potentially stagnant sources. Samples flagged as rejected or contaminated, measurements below detection limits, and outliers identified through manual inspection were removed to ensure data reliability. Data quality control steps are detailed in the manuscript Methods section.

Combined and quality-controlled data ready for analysis can be found in *Data* folder.

### Analysis

1. **Calculating Spatial Proxies**: 
```bash
   Rscript Well_spatial_analysis_compl_0127.R 
```
   Generates a matrix with four spatial proxies associated with each daily analyte sample:
   1) distance to the closest well (in meters)
   2) summed distance of 10 closest wells (in meters)
   3) number of oil and gas wells within 1 kilometer radius from water sampling location
   4) number of oil and gas wells within 3-kilometer radius from water sampling location

2. **Correlation analysis**:
```bash
#Obtaining correlation coefficient values (example):
corr_matrix = Data %>%
  group_by(type_w, well_type, Analyte) %>%
  summarise(near_d = cor(log(daily_mean), log(nearest_distance_m)), sum_d = cor(log(daily_mean), log(closest_well_dist_sum), use = "complete.obs"),
            den_1km = cor(log(daily_mean[-which(num_well_1km==0)]), log(num_well_1km[-which(num_well_1km==0)])), 
            den_3km = cor(log(daily_mean[-which(num_well_3km==0)]), log(num_well_3km[-which(num_well_3km==0)])))
```
   We calculated Pearson correlation coefficients (R) between log-transformed daily analyte concentrations and spatial proxies (well proximity/density), with statistical significance defined as p < 0.005. 
   Correlation results are reported separately for each analyte, spatial proxy, state, well type, and water source.

3. **Hierarchical clustering**:
```bash
   Visualization:  heatmap(corr_matrix, Colv = NA, col = color_t, scale = "none", ColSideColors = state_colors, RowSideColors = well_colors)  #define color themes prior to using this
```
 We performed hierarchical clustering on correlation results using the heatmap function in R's stats package. This function applies agglomerative clustering using Euclidean distance, iteratively merging the most similar clusters and displaying results via dendrograms. Rows in the correlation matrix were reordered based on clustering to group similar correlation patterns together.

## Requirements

- R version 4.4.0 or higher
- Libraries needed: *dataRetrieval, data.table, reshape2, leaflet, DT, usmap, ggplot2, scales, dplyr, lubridate, sp, FNN, htmlwidgets, ggpubr, rstudioapi, janitor*

## Citation

If you use this code or data, please cite:
```
Basijokaite, R., Kang, M., & Wen, T. (2026). Oil and gas wells leave measurable impacts on U.S.
surface and groundwater quality. Science Advances [under review]
```

## Contact

For questions, please contact Ruta Basijokaite at rbasijok@syr.edu.

## Acknowledgments

This work was funded by the National Science Foundation grant OAC-2209864.

