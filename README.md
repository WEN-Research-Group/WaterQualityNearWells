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

Combined and quality-controlled data ready for analysis can be found in *Data* folder.

Data collection methods and quality control procedures are detailed in the manuscript Methods section.

Water quality data underwent rigorous quality control before analysis. We retained only samples from natural surface water (rivers, streams) and groundwater (seeps, springs, wells) sources, excluding anthropogenic or potentially stagnant sources. Samples flagged as rejected or contaminated, measurements below detection limits, and outliers identified through manual inspection were removed to ensure data reliability.

## Reproducing the Analysis

1. **Data Cleaning**: 
```bash
   Rscript code/01_data_cleaning.R
```
   Generates `processed/merged_dataset.csv`

2. **Exploratory Analysis**:
```bash
   Rscript code/02_exploratory.R
```
   Creates Figure 1 and Figure 2

3. **Model Training**:
```bash
   python code/03_model_training.py
```
   Trains random forest and gradient boosting models, saves to `results/models/`

4. **Validation**:
```bash
   python code/04_validation.py
```
   Generates Figure 3, Figure 4, and Table 2

Expected runtime: approximately 15 minutes on a standard desktop computer.


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

