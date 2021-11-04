# Load libraries ----------------------------------------------------------
library("tidyverse")
library("keras")


# Define functions --------------------------------------------------------

# Example of custom metric, here pearson's correlation coefficient, i.e.
# equivalent to cor(x, y, method = "pearson"), but note how we need to use the
# keras (tensorflow) methods "k_*"
metric_pcc <- custom_metric("pcc", function(y_true, y_pred){
  mu_y_true <- k_mean(y_true)
  mu_y_pred <- k_mean(y_pred)
  r <- k_sum( (y_true - mu_y_true) * (y_pred - mu_y_pred) ) /
    ( k_sqrt(k_sum( k_square(y_true - mu_y_true) )) *
        k_sqrt(k_sum( k_square(y_pred - mu_y_pred) )) )
  return(r)
})


# Prepare data ------------------------------------------------------------

# Set nn data and test/training partitions
test_f <- 0.20
nn_dat <- diamonds %>%
  mutate_if(is.ordered, as.numeric) %>% 
  mutate(partition = sample(x = c("test", "train"),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(test_f, 1 - test_f)))

# Set training data
X_train <- nn_dat %>%
  filter(partition == "train") %>%
  select(-price, -partition) %>%
  as.matrix
y_train <- nn_dat %>%
  filter(partition == "train") %>%
  pull(price)

# Set test data
X_test <- nn_dat %>%
  filter(partition == "test") %>%
  select(-price, -partition) %>%
  as.matrix
y_test <- nn_dat %>%
  filter(partition == "test") %>%
  pull(price)

# Define ANN model --------------------------------------------------------

# Set hyperparameters for regression predicting price as continuous number 
n_epochs <- 10               # Number of runs through entire data set
batch_size <- 200            # Number of examples used for one error estimation
loss <- "mean_squared_error" # Quantify difference between target and pred
learning_rate <- 0.1         # How big a step should we take during each update
optimzer <- optimizer_adam(learning_rate = learning_rate) # Optimisation algorithm
h1_activation <- "relu"      # Non-linear function for 1st hidden layer
h1_n_hidden <- 2             # Number of hidden units in 1st hidden layer
h2_activation <-"relu"       # Non-linear function for 2nd hidden layer
h2_n_hidden <- 2             # Number of hidden units in 2nd hidden layer
h3_activation <- "relu"      # Non-linear function for 3rd hidden layer
h3_n_hidden <- 2             # Number of hidden units in 3rd hidden layer
o_activation <- "sigmoid"    # Non-linear function for output layer, [0;1]
                             # Think about what you are predicting?

# Set architecture
model <- keras_model_sequential() %>% 
  layer_dense(units = h1_n_hidden,
              activation = h1_activation,
              input_shape = ncol(X_train)) %>%
  layer_dense(units = h2_n_hidden,
              activation = h2_activation) %>% 
  layer_dense(units = h3_n_hidden,
              activation = h3_activation) %>% 
  layer_dense(units = 1,
              activation = o_activation)

# Compile model
model %>%
  compile(
    loss = loss,
    optimizer = optimzer,
    metrics = metric_pcc # Note the custom metric here
)

# View model
model %>%
  summary %>%
  print


# Train model -------------------------------------------------------------

# Fit model on training data
history <- model %>%
  fit(x = X_train,
      y = y_train,
      epochs = n_epochs,
      batch_size = batch_size,
      validation_split = 0
)

# Note, if you get "pcc: nan", try running the following in the console
# cor(x = rep(0.8, 10), y = 1:10, method = "pearson")
# 

# Evaluate model ----------------------------------------------------------

# Calculate performance on test data
y_test_true <- y_test
y_test_pred <- model %>% predict(X_test) %>% as.vector
pcc_test <- round(cor(y_test_pred, y_test_true, method = "pearson"), 3)

# Calculate performance on training data
y_train_true <- y_train
y_train_pred <- model %>% predict(X_train) %>% as.vector
pcc_train <- round(cor(y_train_pred, y_train_true, method = "pearson"), 3)

# Compile data for plotting
d_perf <- bind_rows(
  tibble(y_pred = y_test_pred,
         y_true = y_test_true,
         partition = "test"),
  tibble(y_pred = y_train_pred,
         y_true = y_train_true,
         partition = "train"))


# Visualise performance ---------------------------------------------------
d_perf %>%
  ggplot(aes(x = y_pred,
             y = y_true)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              colour = "red") +
  labs(title = "Performance of ANN Regression model on Diamonds data set",
       subtitle = str_c("Test PCC = ", pcc_test,
                        ", training PCC = ", pcc_train, "."),
       x = "Predicted Price of Diamond",
       y = "Actual Price of Diamond") +
  facet_wrap(~partition) +
  theme_bw()


# Save model --------------------------------------------------------------

# We can save trained models for later use. Models can be easily loaded using
# the load_model_hdf5() function
save_model_hdf5(object = model,
                filepath = "Models/04_diamond_model.h5")

# Load the model (Note the use of the custom_objects argument)
loaded_model <- load_model_hdf5(filepath = "Models/04_diamond_model.h5",
                                custom_objects = list("pcc" = metric_pcc))
