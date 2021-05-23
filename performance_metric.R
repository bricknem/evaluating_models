#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @return winter_mean_cor

# --------------
# begin function
# --------------

winter_flow <- function(m, o, month, year, wy) {
  
  df = cbind.data.frame(m, o, month, year, wy)
  
  winter <- df %>% 
    filter(month %in% c("11", "12", "1", "2")) %>% 
    group_by(year) %>% 
    summarize(
      model_average_flow = mean(m),
      obs_average_flow = mean(o)
    )
  
  winter_mean_cor = cor(winter$model_average_flow, winter$obs_average_flow)
  
  return(winter_mean_cor)
  
}
