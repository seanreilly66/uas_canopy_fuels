# =================================== Libraries ================================
library(randomForest)
library(caret)
library(leaps)
library(foreach)
library(tidyverse)
library(glue)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

rf_file <- 'data/ml_output/rf_spatial_cluster_model_20230419_1631.Rdata'
svm_file <- 'data/ml_output/svm_spatial_cluster_model_20230416_1536.Rdata'
lm_file <- 'data/ml_output/lm_spatial_cluster_model_20230419_1852.Rdata'


response_csv <- 'data/field/plot_field_metrics.csv'

# ==============================================================================
# ============================== Data preparation ==============================
# ==============================================================================

# Generate model predictions

predobs_func <- function(mdl_file) {
  
  mdl <- readRDS(mdl_file)
  
  predobs <- foreach(
    i = 1:length(mdl),
    .combine = 'rbind'
  ) %do% {
    
    mdl_i <- mdl[i]
    predobs_i <- extractPrediction(models = mdl_i)
    
  }
  
  return(predobs)
  
}

predobs_df <- foreach(
  i = c(rf_file, svm_file, lm_file),
  .combine = 'rbind'
) %do% {
  
  predobs_i <- predobs_func(mdl_file = i)
  
}

predobs_df <- predobs_df %>%
  mutate(object = str_extract(object, '^[:alpha:]+') %>%
           str_replace('densiometer', 'CC') %>%
           str_replace('^h$', 'Mean height') %>%
           str_replace('cbh', 'CBH') %>%
           str_replace('cbd', 'CBD') %>%
           str_replace('lai', 'LAI') %>%
           str_replace('biomass', 'Biomass')) %>%
  mutate(model = str_replace(model, 'rf', 'RF') %>%
           str_replace('svmRadial', 'SVM') %>%
           str_replace('leapForward', 'OLS')) %>%
  filter(model != 'leapSeq',
         model != 'leapBackward')


ols_labels <- tibble(
  object = c('Biomass', 'Mean height', 'CBH', 'CC', 'CBD', 'LAI'),
  R2 = c(0.62, 0.72, 0.69, 0.57, 0.54, 0.38),
  RMSE = c(12.36, 5.05, 3.01, 12.11, 0.02, 1.51),
  pRMSE = c(65.8, 25.7, 39.1, 16.1, 10.5, 42.7),
  MAPE = c(128.0, 24.9, 51.3, 71.2, 12.8, 62.1),
  model = 'OLS'
  )

svm_labels <- tibble(
  object = c('Biomass', 'Mean height', 'CBH', 'CC', 'CBD', 'LAI'),
  R2 = c(0.69, 0.71, 0.69, 0.55, 0.46, 0.45),
  RMSE = c(11.36, 5.19, 2.78, 11.05, 0.02, 1.35),
  pRMSE = c(60.5, 26.4, 36.2, 14.6, 10.5, 38.1),
  MAPE = c(139.0, 22.4, 40.2, 62.5, 14.3, 69.2),
  model = 'SVM'
)

rf_labels <- tibble(
  object = c('Biomass', 'Mean height', 'CBH', 'CC', 'CBD', 'LAI'),
  R2 = c(0.75, 0.73, 0.70, 0.56, 0.56, 0.59),
  RMSE = c(9.47, 4.85, 2.57, 11.56, 0.02, 1.13),
  pRMSE = c(50.4, 24.7, 33.4, 15.3, 10.5, 31.9),
  MAPE = c(82.3, 24.2, 41.9, 63.6, 12.6, 51.3),
  model = 'RF'
)

label_xy = predobs_df %>%
  group_by(object, model) %>%
  summarize(
    x = min(pred),
    y = max(obs)
  )

label_xy[which(label_xy$object == 'CBD'), 4] = label_xy[which(label_xy$object == 'CBD'),4] + 0.05
label_xy[which(label_xy$object == 'CBD'), 3] = label_xy[which(label_xy$object == 'CBD'),3] - 0.01

labels <- rbind(ols_labels, svm_labels, rf_labels) %>%
  mutate(lab = glue(
    'Model = {model}
R^2 = {R2}
RMSE = {RMSE}
RMSE% = {pRMSE}%
MAPE = {MAPE}%'
  )) %>%
  left_join(label_xy)




# ==============================================================================
# ================================== Plotting ==================================
# ==============================================================================

# ================================ ggplot theme ================================ 

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(linewidth = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)

# facet_label <- function(variable, value) {
#   
#   lab <- plot_labels %>%
#     filter(model == value)
#   
#   return(glue(
#     'Model = {lab$model}
# R^2 = {lab$R2}
# RMSE = {lab$RMSE}
# RMSE"% = {lab$pRMSE}\%
# MAPE = {lab$MAPE}\%'
#   ))
#   
#   
# }

# ============================= Plotting function ============================== 

for (field_var in c('Biomass', 'Mean height', 'CBH', 'CC', 'CBD', 'LAI')) {
  
  plot_df = predobs_df %>%
    filter(object == field_var)
  
  ggplot(data = plot_df,
         mapping = aes(x = pred,
                       y = obs,
                       color = model)) +
    geom_abline(linetype = 'dashed',
                linewidth = 0.6,
                color = 'grey60') +
    geom_smooth(method = 'lm',
                se = FALSE) +
    geom_point(size = 2) +
    geom_label(
      data = labels %>%
        filter(object == field_var),
      aes(x = x,
          y = y,
          label = lab),
      color = 'black',
      label.size = NA,
      hjust = 'inward',
      vjust = 'inward',
      family = 'serif', fontface = 'plain', size = 3
    ) +
    scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
                       name = NULL,
                       guide = NULL) +
    labs(title = plot_df$object[1],
         x = 'Predicted',
         y = 'Observed') +
    facet_grid(cols = vars(model)) +
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing = unit(0.25, "cm"))
  
  ggsave(
    filename = glue('figures/predobs/{field_var}_predobs.png'),
    height = 5,
    width = 8,
    dpi = 700
  )
  
}







# ==============================================================================
# ================================== CV Plots ================================== 
# ==============================================================================

rf <- readRDS(rf_file)

bm <- rf$biomass_sum_rf_rfe_spatial_folds

x = bm$pred
