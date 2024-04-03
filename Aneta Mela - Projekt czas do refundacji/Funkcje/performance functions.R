
# Performance assessment functions ----------------------------------------



# Function for calculating marginal and conditional r-squared -------------


r2_marginal <- function(mod) {
  
  r <- MuMIn::r.squaredGLMM(mod)
  
  return(r[[1]])
  
}

r2_conditional <- function(mod) {
  
  r <- MuMIn::r.squaredGLMM(mod) 
  
  return(r[[2]])
  
}




# Function for filtering outliers  ----------------------------------------

filter_residuals <- function(data, model, cutoff = 3) {
  
  data %>% 
    mutate(resid = residuals(model) %>% scale() %>% as.double()) %>% 
    filter(abs(resid) <= cutoff)
} 




# Extract functions for quick assumptions check ---------------------------


hist_resid <- function(model) {
  p <- ggplot(mapping = aes(residuals(model))) +
    geom_histogram()
  return(p)
}

homoscedasticity_plot <- function(model) {
  
  p <- ggplot(mapping = aes(predict(model), residuals(model))) +
    geom_point() +
    geom_smooth(se = F)
  
  return(p)
}
