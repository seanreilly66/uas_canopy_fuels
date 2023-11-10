library(tidyverse)
library(caret)
library(randomForest)
library(ggpubr)
library(glue)

ml <- readRDS('data/ml_output/rf_structural_metric_model_20230604_1017.RData')
subset <- 'Vertical distribution'
file_name <- 'vert_dist'


varimp_plot <- function(.data, plot_label, slice_n = 5) {
  
  ml_varimp <- .data %>%
    varImp() %>%
    .$importance %>%
    rownames_to_column() %>%
    tibble() %>%
    rename(Predictor = rowname,
           Importance = Overall) %>%
    slice_max(order_by = Importance, n = slice_n)

  ml_varimp <- ml_varimp %>%
    mutate(
      var_label = str_replace(Predictor, 'gndvi_mean', 'bar(GNDVI)') %>%
        str_replace('z_mean', 'bar(Z)') %>%
        str_replace('cc_1m', 'CC[1][m]') %>%
        str_replace('ndvi_mean', 'bar(NDVI)') %>%
        str_replace('ndre_mean', 'bar(NDRE)') %>%
        str_replace('z', 'P') %>%
        str_replace('_p', '[') %>%
        str_replace('red_mean', 'bar(Red)') %>%
        str_replace('blue_mean', 'bar(Blue)') %>%
        str_replace('rededge_mean', 'bar(Red~Edge)') %>%
        str_replace('ndre_sd', 'NDRE[S][D]') %>%
        str_replace('rededge_sd', 'Red~Edge[S][D]') %>%
        str_replace('green_sd', 'Green[S][D]') %>%
        str_replace('ndvi_sd', 'NDVI[S][D]') %>%
        str_replace('ndre02_sd', 'NDRE[H][2]~SD') %>%
        str_replace('ndre03_sd', 'NDRE[H][3]~SD') %>%
        str_replace('ndre04_sd', 'NDRE[H][4]~SD') %>%
        str_replace('ndre05_sd', 'NDRE[H][5]~SD') %>%
        str_replace('ndre06_sd', 'NDRE[H][6]~SD') %>%
        str_replace('ndre07_sd', 'NDRE[H][7]~SD') %>%
        str_replace('ndre08_sd', 'NDRE[H][8]~SD') %>%
        str_replace('ndre09_sd', 'NDRE[H][9]~SD') %>%
        str_replace('ndre10_sd', 'NDRE[H][10]~SD') %>%
        str_replace('ndvi02_sd', 'NDVI[H][2]~SD') %>%
        str_replace('ndvi03_sd', 'NDVI[H][3]~SD') %>%
        str_replace('ndvi04_sd', 'NDVI[H][4]~SD') %>%
        str_replace('ndvi05_sd', 'NDVI[H][5]~SD') %>%
        str_replace('ndvi06_sd', 'NDVI[H][6]~SD') %>%
        str_replace('ndvi07_sd', 'NDVI[H][7]~SD') %>%
        str_replace('ndvi08_sd', 'NDVI[H][8]~SD') %>%
        str_replace('ndvi09_sd', 'NDVI[H][9]~SD') %>%
        str_replace('ndvi10_sd', 'NDVI[H][10]~SD') %>%
        str_replace('r08_mean', 'bar(Red)[H][8]') %>%
        str_replace('b10_sd', 'Blue[H][10]~SD') %>%
        str_replace('g07_sd', 'Green[H][7]~SD') %>%
        str_replace('ndvi04_mean', 'bar(NDVI)[H][4]') %>%
        str_replace('ndvi05_mean', 'bar(NDVI)[H][5]') %>%
        str_replace('ndvi06_mean', 'bar(NDVI)[H][6]') %>%
        str_replace('ndvi07_mean', 'bar(NDVI)[H][7]') %>%
        str_replace('ndvi08_mean', 'bar(NDVI)[H][8]') %>%
        str_replace('ndvi09_mean', 'bar(NDVI)[H][9]') %>%
        str_replace('ndvi10_mean', 'bar(NDVI)[H][10]') %>%
      str_replace('re10_sd', 'Red~Edge[H][10]~SD') %>%
        str_replace('(?<=[:digit:])$', ']')
    ) %>%
    arrange(desc(Importance)) %>%
    mutate(var_label = factor(var_label, levels = rev(.$var_label)))

  # ml_varimp <- ml_varimp %>%
  #   mutate(var_label = Predictor) %>%
  #   arrange(desc(Importance)) %>%
  #   mutate(var_label = factor(var_label, levels = rev(.$var_label)))
  
  var_cat <- tibble(
    val = c('firebrick', '#40B0A6', '#DDCC77'),
    lev = c('Vertical distribution', 'Raster-based spectral', 'Height band spectral')) %>%
    filter(lev == subset) %>%
    pull(val)

  vimp_plot = ggplot(
    data = ml_varimp
  ) +
    geom_point(
      mapping = aes(
        x = Importance,
        y = var_label),
      size = 3,
      color = var_cat
    ) + 
    geom_segment(
      mapping = aes(
        x = 0,
        xend = Importance,
        y = var_label,
        yend = var_label
      ),
      linetype = 'dashed',
      linewidth = 0.6,
      color = var_cat
    ) +
    labs(x = NULL, 
         y = NULL,
         title = plot_label) +
    scale_y_discrete(labels = parse(text = rev(as.character(ml_varimp$var_label)))) +
    theme(legend.position = 'bottom')
  
  return(vimp_plot)
  
}


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
    legend.margin = ggplot2::margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)



lai <- ml$lai_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'f) LAI')

cbd <- ml$cbd_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'e) CBD')

cbh <- ml$cbh_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'c) CBH') +
  scale_x_continuous(labels = NULL)

cc <- ml$densiometer_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'd) CC')

h <- ml$h_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'b) Mean height') +
  scale_x_continuous(labels = NULL)

biomass <- ml$biomass_sum_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'a) Biomass') +
  scale_x_continuous(labels = NULL)



varimp_fig <- ggarrange(
  biomass, h, cbh, cc, cbd, lai,
  nrow = 2,
  ncol = 3,
  widths = c(1, 1, 1),
  legend = 'none',
  align = "hv") %>%
  annotate_figure(
    bottom = text_grob('                   Scaled variable importance', family = 'serif', size = 16)) %>%
  annotate_figure(
    left = text_grob('Top variables', family = 'serif', size = 16, rot = 90),
    top = text_grob(subset, family = 'serif', size = 18))

varimp_fig

ggsave(
  filename = glue('figures/supplement/rf_{file_name}_variable_importance.png'),
  width = 9,
  height = 5,
  units = 'in',
  dpi = 700
)
