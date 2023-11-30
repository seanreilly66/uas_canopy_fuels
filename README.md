# uas_canopy_fuels
R code from the analyses presented in "Unoccupied aerial system (UAS) Structure-from-Motion canopy fuel parameters: Multisite area-based modelling across forests in California, USA"

## Contents

### LAS data preprocessing
[**als_dtm_generation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/als_dtm_generation.R): Generates DTM from ALS las files and converts the DTM to a LAS file format for use in ground icp registration of UAS files

[**las_ground_classification.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_ground_classification.R): Classifies ground points within LAS point clouds using CSF algorithm and the set of optimized parameters identified during the 2019 Pepperwood UAS study. Clips to given box boundary and merges spectral data.

[**las_plot_clip.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_plot_clip.R): Clips las files to given plot extent

[**las_reg_and_hnorm.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_reg_and_hnorm.R): Applies las_transformation.R to register a given LAS file using a transformation matrix obtained from Lidar360 and height normalizes the result using ALS dtm

[**las_transformation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_transformation.R): Function to Performs a spatial transformation on a las point cloud based on given transformation matrix

### Field data
[**biomass_computation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/biomass_computation.R): Computes tree and plot level biomass

[**biomass_equations.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/biomass_equations.R): Function for CARB biomass equations


### Area-based metric extraction
[**als_metric_function.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/als_metric_function.R): Function for generating height metrics from ALS

### Plots
[**field_correlation_plot.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/field_correlation_plot.R): Generates correlelogram for relationship between field measured fuel parameters

[**field_data_plots.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/field_data_plots.R): Generates stacked boxplot with scatter overlay for field measured fuel parameters


