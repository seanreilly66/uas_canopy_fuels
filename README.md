# uas_canopy_fuels
R code from the analyses presented in "Unoccupied aerial system (UAS) Structure-from-Motion canopy fuel parameters: Multisite area-based modelling across forests in California, USA"

## Contents

### Data processing


#### LAS
[**als_dtm_generation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/als_dtm_generation.R): Generates DTM from ALS las files and converts the DTM to a LAS file format for use in ground icp registration of UAS files

[**las_ground_classification.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_ground_classification.R): Classifies ground points within LAS point clouds using CSF algorithm and the set of optimized parameters identified during the 2019 Pepperwood UAS study. Clips to given box boundary and merges spectral data.

[**las_reg_and_hnorm.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_reg_and_hnorm.R): Applies las_transformation.R to register a given LAS file using a transformation matrix obtained from Lidar360 and height normalizes the result using ALS dtm

[**las_transformation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_transformation.R): Function to Performs a spatial transformation on a las point cloud based on given transformation matrix

[**las_plot_clip.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/las_plot_clip.R): Clips las files to given plot extent


#### Field data
[**biomass_computation.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/biomass_computation.R): Computes tree and plot level biomass

[**biomass_equations.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/biomass_equations.R): Function for CARB biomass equations


#### Area-based metric extraction

[**plot_metrics_calc.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/plot_metrics_calc.R): Parallel wrapper for metric functions to extract metrics from many plot level las files and merge into one dataset

[**als_metric_function.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/als_metric_function.R): Function for generating height metrics from ALS

[**uas_metric_function.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/uas_metric_function.R): Function for generating height and spectral metrics from UAS las

[**plot_spec_raster_metrics.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/plot_spec_raster_metrics.R): Extract spectral metrics from raster


### Modelling

[**lm_spatial_fold_regression.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/lm_spatial_fold_regression.R): Linear modelling regression using multiple selection algorithms (RFE, none, backwards, forwards, sequential) and spatial fold cross validation

[**rf_spatial_fold_regression.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/rf_spatial_fold_regression.R): Random forest model with RFE variable selection and spatial cross validation


[**svm_spatial_fold_regression.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/svm_spatial_fold_regression.R): SVM model with radial kernel, RFE variable selection, and spatial cross validation

[**rf_novel_site.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/rf_novel_site.R): Model performance testing against novel site, generates random forest model with RFE variable selection and spatial cross validation holding out one study site then tests the final model performance against the hold out site

[**rf_spatial_fold_regression.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/rf_spatial_fold_regression.R): 


### Plots

#### Manuscript

[**field_correlation_plot.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/field_correlation_plot.R): Generates correlelogram for relationship between field measured fuel parameters

[**field_data_plots.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/field_data_plots.R): Generates stacked boxplot with scatter overlay for field measured fuel parameters

[**ml_result_comparison_plots.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/ml_result_comparison_plots.R): R2 comparison plots between modelling methods for full model and site specific

[**ml_data_comparison_plot.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/ml_data_comparison_plot.R): R2 comparison figure of model performance using different data subtypes as predictors (e.g., structural, spectral, height spectral)

[**uas_als_comparison_plot.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/uas_als_comparison_plot.R): R2 comparison figure between individual site UAS and ALS RF models

[**prediction_rasters.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/prediction_rasters.R): Generates prediction rasters by applying model to larger dataset

[**sdlmtn_scatter.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/sdlmtn_scatter.R): Scatter plot of predictions versus observed results for novel site model testing

[**variable_importance_plots.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/variable_importance_plots.R): Variable importance plots for top 5 variables from RF model


[**variable_importance_plots_data_subset.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/variable_importance_plots_data_subsets.R): Variable importance plots for top 5 variables from RF model for each of the predictor data subset models

#### Additional plots

[**ml_prediction_plot.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/ml_prediction_plot.R):Generates variable specific predicted versus observed regression plots

[**partial_dependence_plots.R**](https://github.com/seanreilly66/uas_canopy_fuels/blob/main/R/partial_dependence_plots.R): Generates variable partial dependence plots from ml model result

