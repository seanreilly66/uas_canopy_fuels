library(tidyverse)
library(caret)
library(randomForest)
library(ggpubr)

ml <- readRDS('data/ml_output/rf_spatial_cluster_model_20230227_1528.RData')

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
      var_label = str_replace(Predictor, 'gndvi', 'bar(GNDVI)') %>%
        str_replace('ndvi', 'bar(NDVI)') %>%
        str_replace('ndre', 'bar(NDRE)') %>%
        str_replace('(?<=\\))0', '[H') %>%
        str_replace('(?<=[:digit:])_', ']_') %>%
        str_replace('_mean', '') %>%
        str_replace('z', 'P') %>%
        str_replace('_p', '[') %>%
        str_replace('(?<=[:digit:])$', ']')
    ) %>%
  arrange(desc(Importance)) %>%
  mutate(var_label = factor(var_label, levels = rev(.$var_label)))
  
  ml_varimp <- ml_varimp %>%
    mutate(
      var_cat = ifelse(
        test = str_detect(Predictor, 'z_p'), 
        yes = 'Vertical distribution',
        no = ifelse(
          test = str_detect(Predictor, '[:digit:]_'),
          yes = 'Height band spectral',
          no = 'Raster-based spectral')))
  
  ml_varimp$var_cat <- factor(
    ml_varimp$var_cat, 
    levels = c('Vertical distribution', 'Raster-based spectral', 'Height band spectral'))
  
  col_val <- tibble(
    val = c('firebrick', '#40B0A6', '#DDCC77'),
    var_cat = c('Vertical distribution', 'Raster-based spectral', 'Height band spectral')
  ) %>%
    filter(var_cat %in% ml_varimp$var_cat)

  vimp_plot = ggplot(
    data = ml_varimp
  ) +
    geom_point(
      mapping = aes(
        x = Importance,
        y = var_label,
        color = var_cat),
      size = 3
    ) + 
    geom_segment(
      mapping = aes(
        x = 0,
        xend = Importance,
        y = var_label,
        yend = var_label,
        color = var_cat
      ),
      linetype = 'dashed',
      linewidth = 0.6
    ) +
    labs(x = NULL, 
         y = NULL,
         title = plot_label) +
    scale_y_discrete(labels = parse(text = rev(as.character(ml_varimp$var_label)))) +
    scale_color_manual(values = col_val$val,
                         name = NULL) +
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


legend_df <- tibble(
  x = c(1, 2, 3),
  y = c(1, 2, 3),
  val = c('firebrick', '#40B0A6', '#DDCC77'),
  lev = c('Vertical distribution', 'Raster-based spectral', 'Height band spectral')) %>%
  mutate(
    lev = lev %>%
      as_factor() %>%
      fct_relevel(
        'Vertical distribution', 'Raster-based spectral', 'Height band spectral')
  )

legend_plot <- ggplot(
  data = legend_df,
  mapping = aes(
    x = x,
    y = y,
    color = lev
  )
) +
  geom_point(
    size = 5,
    shape = 15
  ) +
  scale_color_manual(values = legend_df$val,
                     name = NULL) +
  theme(legend.position = 'bottom')


com_legend <- get_legend(legend_plot)



varimp_fig <- ggarrange(
  biomass, h, cbh, cc, cbd, lai,
  nrow = 2,
  ncol = 3,
  widths = c(1, 1, 1),
  legend = 'none',
  align = "hv") %>%
  annotate_figure(
    bottom = text_grob('                   Scaled variable importance', family = 'serif', size = 16)) %>%
  ggarrange(
    legend.grob = com_legend,
    legend = 'bottom'
  ) %>%
  annotate_figure(
    left = text_grob('Top variables', family = 'serif', size = 16, rot = 90))
  
varimp_fig

ggsave(
  filename = 'figures/manuscript/rf_variable_importance.png',
  width = 9,
  height = 5,
  units = 'in',
  dpi = 700
)
