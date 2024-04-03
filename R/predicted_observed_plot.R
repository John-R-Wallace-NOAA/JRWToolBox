
predicted_observed_plot <- function(observed_val, predicted_val, model_predicted_line = NULL, model_line_col = 'dodgerblue', model_name = "", ...) {

  require(ggplot2)
  
  residual_val <- observed_val - predicted_val
  R_squared <- round(cor(observed_val, predicted_val)^2, 4)
  
  ggplot(mapping = aes(x = observed_val, y = predicted_val, col = abs(residual_val))) +
  geom_point(alpha = 0.9, size = 2) +
  geom_abline(intercept = 0, slope = 1) +
  { if(!is.null(model_predicted_line))
       geom_abline(intercept = model_predicted_line[1], slope = model_predicted_line[2], col = model_line_col, size = 1.25) } +
    # facet_wrap(~) +
    labs(title = paste0(model_name, "\nPredicted vs Observed: Test Set"),
         subtitle = paste0("R-squared: ", R_squared),
         x = "Observed",
         y = "Predicted",
         col = "Absolute Deviation") +
  theme_bw() +
  theme(aspect.ratio = 0.9, panel.grid.minor.x = element_blank(), legend.title = element_text(size = 10, face="bold"), 
          legend.text = element_text(size = 9), plot.title = element_text(size=12, face="bold"), axis.title=element_text(size=10, face="bold"), 
          axis.text.x = element_text(angle = 0), legend.position = "none") +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) + 
  coord_equal() + scale_color_viridis_c(direction = -1)
}
