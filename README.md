# uas_canopy_fuels
R code from the analyses presented in "Unoccupied aerial system (UAS) Structure-from-Motion canopy fuel parameters: Multisite area-based modelling across forests in California, USA"

## Contents

### UAS-SfM data preprocessing
[**als_dtm_generation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/als_dtm_generation.R): Generates DTM from ALS las files and converts the DTM to a LAS file format for use in ground icp registration of UAS files

### Field data
[**biomass_computation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/biomass_computation.R): Computes tree and plot level biomass

[**biomass_equations.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/biomass_equations.R): Function for CARB biomass equations


### Area-based metric extraction
[**als_metric_function.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/als_metric_function.R): Function for generating height metrics from ALS


