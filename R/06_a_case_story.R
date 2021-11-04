# This is a read and execute exercise, the details of the code are not
# super important. The content of the exercises is extrapolatable to any
# scenario involving mathematical modeling. Read and execute and play around

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")
set.seed(239334)


# Load data ---------------------------------------------------------------
load(file = "data/06_case_story.rds")


# wrangle data ------------------------------------------------------------

# First let us add some predictions to the case data:
case_data_w_pred <- case_data %>% 
  as_tibble %>% 
  mutate(y_pred_simple = predict(mdl_simple, newdata = data.frame(x = x)),
         y_pred_complex = predict(mdl_complex, newdata = data.frame(x = x)))

# and then you can use the get_metrics function to see the numbers from the
# performance plot
case_data_perf <- case_data_w_pred %>% 
  get_metrics
case_data_perf

# Great! All is good and it has been decided to put the complex model into
# production.
#
# Time goes by and people using the model for prioritisation
# in the wet lab start complaining, that the despite prioritising targets
# using the model, only very few of the targets actually validate.
#
# Now, you re-read the fine prints of the documentation for the models and
# apparently the complex model was trained using a hyperparameter alpha,
# which was tuned for optimal performance. The tuned value is 0.1. The details
# on how this parameter was tuned are scarce.
#
# You are worried and you communicate your worries to your boss, who authorises
# using a large sum of money on generating 50 new data points:
new_data <- data_generator(n = 50)

# Great! Finally you can see if the wetlab scientist are right or if something
# has been misunderstood.

# You add predictions to the new data
new_data_w_pred <- new_data %>% 
  as_tibble %>% 
  mutate(y_pred_simple = predict(mdl_simple, newdata = data.frame(x = x)),
         y_pred_complex = predict(mdl_complex, newdata = data.frame(x = x)))

# and then you again use the get_metrics function to get the performance
new_data_perf <- new_data_w_pred %>% 
  get_metrics

# Seeing is believing, let's look at the numbers
case_data_perf_plot <- case_data_perf %>% 
  ggplot(aes(x = type, y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.5) +
  theme_bw(base_size = 16)
new_data_perf_plot <- new_data_perf %>% 
  ggplot(aes(x = type, y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.5) +
  theme_bw(base_size = 16)
print(case_data_perf_plot + new_data_perf_plot)

# Q1: Discuss with your breakout room buddy: What in the world is going on?
#     Same data source, same models!?

# You decide to take a closer look at the hyperparameter alpha and using the
# old (case) data, you retrain 3 models, one for each value of alpha, that you
# have randomly chosen for exploratory reasons:
mdl_complex_a_010 <- train_model_complex(y ~ x, alpha = 0.1, data = case_data)
mdl_complex_a_025 <- train_model_complex(y ~ x, alpha = 0.25, data = case_data)
mdl_complex_a_050 <- train_model_complex(y ~ x, alpha = 0.50, data = case_data)

# Now, you use your 3 new models trained on the case data to predict on the
# case data and the new data:
case_data_new_models <- case_data %>% 
  as_tibble %>% 
  mutate(y_complex_a_010 = predict(mdl_complex_a_010, newdata = data.frame(x = x)),
         y_complex_a_025 = predict(mdl_complex_a_025, newdata = data.frame(x = x)),
         y_complex_a_050 = predict(mdl_complex_a_050, newdata = data.frame(x = x)))
new_data_new_models <- new_data %>% 
  as_tibble %>% 
  mutate(y_complex_a_010 = predict(mdl_complex_a_010, newdata = data.frame(x = x)),
         y_complex_a_025 = predict(mdl_complex_a_025, newdata = data.frame(x = x)),
         y_complex_a_050 = predict(mdl_complex_a_050, newdata = data.frame(x = x)))

# and then you again use the get_metrics function to get the performances
case_data_new_models_perf <- case_data_new_models %>% 
  get_metrics
new_data_new_models_perf <- new_data_new_models %>% 
  get_metrics

# Seeing is believing, let's look at the numbers
case_data_new_models_plot <- case_data_new_models_perf %>% 
  ggplot(aes(x = type, y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.5) +
  theme_bw(base_size = 16) +
  labs(title = "Case data new models")
new_data_new_models_plot <- new_data_new_models_perf %>% 
  ggplot(aes(x = type, y = value, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.5) +
  theme_bw(base_size = 16) +
  labs(title = "New data new models")
print(case_data_new_models_plot + new_data_new_models_plot)

# Q2: Discuss with your breakout room buddy: What in the world is going on?
#     In the "Case data new models" the performance is going down with higher
#     values of alpha and in the "New data new models", performance is going up
#     with higher values of alpha!?

# At the coffee machine you discuss with a colleague who talks about data and 
# hyperparameter optimisation using cross validation. You get the gist and
# decide to try it out. You decide to forget about the new data for now and
# return to the old case to take a look at this elusive alpha:

# Set the number of folds in your k-fold cross-validation
k_fold = 5

# Add partitions (randomly in this case) to your old case data:
case_data_partitioned <- case_data %>% 
  as_tibble %>% 
  mutate(partition = sample(rep(1:5, each = nrow(.)/k_fold)))

# Set the range of alpha you want to test out. The original documentation is
# again fuzzy, so you decide to test out:
a_min = 0.1
a_max = 10
alphas <- seq(a_min, a_max, length.out = 100)

# Run the cross-validation (Don't worry about the details here)
results <- list()
for( a in alphas ){
  tmp = list()
  for( i in 1:k_fold ){
    
    # Define test set
    test_set = i
    
    # Define train set
    train_sets = (1:5)[-i]
    
    # Get current data for training and testing
    training_data <- case_data_partitioned %>%
      filter(partition %in% train_sets)
    testing_data <- case_data_partitioned %>%
      filter(partition %in% test_set)
    
    # Train model
    current_model <- train_model_complex(y ~ x, alpha = a,
                                         data = training_data)
    
    # Add predictions to training/test data
    training_data_w_pred <- training_data %>%
      mutate(y_pred = predict(current_model,
                              newdata = data.frame(x = x))) %>% 
      drop_na
    testing_data_w_pred <- testing_data %>%
      mutate(y_pred = predict(current_model,
                              newdata = data.frame(x = x))) %>% 
      drop_na
    
    # Get performance on training/test data
    training_data_w_pred_perf <- training_data_w_pred %>%
      select(-partition) %>% 
      get_metrics %>%
      mutate(eval_set = "train", test_set = test_set, alpha = a)
    testing_data_w_pred_perf <- testing_data_w_pred %>%
      select(-partition) %>% 
      get_metrics %>%
      mutate(eval_set = "test", test_set = test_set, alpha = a)
    
    # Store current results
    tmp[[i]] = bind_rows(training_data_w_pred_perf,
                         testing_data_w_pred_perf)
  }
  # Store current tmp-results
  results <- rbind(results, do.call(rbind, tmp))
}

# Visualise the results of your hyper-parameter scan:
results %>%
  group_by(eval_set, metric, alpha) %>%
  summarise(mu = mean(value), se = sd(value) / sqrt(n())) %>%
  ungroup %>%
  ggplot(aes(x = alpha, y = mu, colour = eval_set,
             ymin = mu - se, ymax = mu + se)) +
  geom_errorbar(colour = "grey90") +
  geom_line() +
  scale_x_continuous(breaks = seq(0, a_max, by = 1)) +
  theme_bw() +
  labs(y = "mean +/- se") +
  facet_wrap(vars(metric), nrow = 1, scales = "free_y")

# Ok, that's interesting, so it seems that your original tuned hyper-parameter
# alpha at value 0.1, was a pretty poor choice. Let us have a closer look at
# that. From your cross validation, it seems that a good choice of alpha could
# be ?. You choose that and re-train a new complex model using the new alpha:

alpha = 0.1 # ===> ? Look at the plot you just made and decide on an alpha
mdl_complex_new_alpha <- train_model_complex(y ~ x, alpha = alpha,
                                             data = case_data)

# Now, add predictions from the old simple model, the old complex model and your
# new complex model with your own tuning of alpha:
case_data_w_new_preds <- case_data %>% 
  as_tibble %>% 
  mutate(y_pred_simple = predict(mdl_simple,
                                 newdata = data.frame(x = x)),
         y_pred_complex = predict(mdl_complex,
                                  newdata = data.frame(x = x)),
         y_pred_complex_new_alpha = predict(mdl_complex_new_alpha,
                                            newdata = data.frame(x = x)))

# Again, seeing is believing, so let us have a look at the models you created:
case_data_w_new_preds_metric_plot <- case_data_w_new_preds %>%
  get_metrics %>%
  ggplot(aes(x = metric, y = value, fill = type, label = round(value,2))) +
  geom_col(position = "dodge", alpha = 0.5) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.25, size = 4) +
  scale_y_continuous(limits = c(0,2.2)) +
  theme_bw() +
  labs(title = "case data")
case_data_w_new_preds_plot <- case_data_w_new_preds %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = case_data_w_new_preds %>%
              select(-y) %>%
              pivot_longer(cols = -x,
                           names_to = "type",
                           values_to = "value"),
            aes(x = x, y = value, colour = type), inherit.aes = FALSE) +
  scale_y_continuous(limits = c(3,9)) +
  theme_bw() +
  labs(x = "explanatory", y = "response", title = "case data")
print(case_data_w_new_preds_plot + case_data_w_new_preds_metric_plot)

# Q3: Again, discuss with your break-out room partner:
#     Interpret and explain what is going on here

# With this visualisation in mind, you did end up spending quite a bit of money
# on getting new data. This data immediately revealed, that something was quite
# fishy. Perhaps you should revisit that with the models you now have and see
# what is what?

# Add predictions from the old simple model, the old complex model and your
# new complex model with your own tuning of alpha:
new_data_w_new_preds <- new_data %>% 
  as_tibble %>% 
  mutate(y_pred_simple = predict(mdl_simple,
                                 newdata = data.frame(x = x)),
         y_pred_complex = predict(mdl_complex,
                                  newdata = data.frame(x = x)),
         y_pred_complex_new_alpha = predict(mdl_complex_new_alpha,
                                            newdata = data.frame(x = x)))

# Again, seeing is believing, so let us have a look at the models you created:
new_data_w_new_preds_metric_plot <- new_data_w_new_preds %>%
  get_metrics %>%
  ggplot(aes(x = metric, y = value, fill = type, label = round(value,2))) +
  geom_col(position = "dodge", alpha = 0.5) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.25, size = 4) +
  scale_y_continuous(limits = c(0,2.2)) +
  theme_bw() +
  labs(title = "new data")
new_data_w_new_preds_plot <- new_data_w_new_preds %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = case_data_w_new_preds %>%
              select(-y) %>%
              pivot_longer(cols = -x,
                           names_to = "type",
                           values_to = "value"),
            aes(x = x, y = value, colour = type), inherit.aes = FALSE) +
  scale_y_continuous(limits = c(3,9)) +
  theme_bw() +
  labs(x = "explanatory", y = "response", title = "new data")
print(new_data_w_new_preds_plot + new_data_w_new_preds_metric_plot)

# Q4: Again, discuss with your break-out room partner:
#     Interpret and explain what is going on here

# Perhaps it can be useful to view both
# of the last plots you made simultaneously?
print((case_data_w_new_preds_plot + case_data_w_new_preds_metric_plot) /
        (new_data_w_new_preds_plot + new_data_w_new_preds_metric_plot))

