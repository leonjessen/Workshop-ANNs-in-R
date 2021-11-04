set.seed(299985)
data_generator <- function(n){
  x <- seq(from = 0, to = 1, length.out = n)
  y <- 3*x + 2 + rnorm(n, 3)
  return(data.frame(x = x, y = y, stringsAsFactors = FALSE))
}
mse <- function(x, y){
  return( mean((x-y)**2) )
}
train_model_complex <- function(x, y, alpha, data){
  return( loess(y ~ x, span = alpha, data = data) )
}
train_model_simple <- function(x, y, data){
  return( lm(y ~ x, data = data) )
}
get_metrics <- function(data = .){
  perf <- data %>%
    select(-x) %>%
    pivot_longer(-y,
                 names_to = "type",
                 values_to = "value") %>%
    group_by(type) %>%
    summarise(pcc = cor(y, value, method = "pearson"),
              scc = cor(y, value, method = "spearman"),
              mse = mse(y, value)) %>%
    ungroup %>% 
    pivot_longer(-type,
                 names_to = "metric",
                 values_to = "value")
  return(perf)
}
case_data <- data_generator(50)
mdl_simple <- train_model_simple(y ~ x, data = case_data)
mdl_complex <- train_model_complex(y ~ x, alpha = 0.1, data = case_data)
save(data_generator,
     mse,
     train_model_complex,
     train_model_simple,
     get_metrics,
     case_data,
     mdl_simple,
     mdl_complex,
     file = "data/06_case_story.rds")
