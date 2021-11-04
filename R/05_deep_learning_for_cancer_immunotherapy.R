# Scaled down version of my invited post on the RStudio AI Blog
# https://blogs.rstudio.com/ai/posts/2018-01-29-dl-for-cancer-immunotherapy/


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("keras")
library("ggseqlogo")


# Define functions --------------------------------------------------------
source(file = "R/99_proj_func.R")


# Load data ---------------------------------------------------------------
file <- "data/ran_peps_netMHCpan40_predicted_A0201_reduced_cleaned_balanced.tsv"
pep_dat <- read_tsv(file = file)


# About the data ----------------------------------------------------------
#
# The data set consists of a total of 23,760 data points with 3 balanced classes
# each with 7,920 data points. Epitope identification is paramount in cancer
# immunotherapy. A peptide may be an epitope, if it is a strong-binder (SB),
# less likely if it is a weak-binder (WB) and not if it is a non-binder (NB).
#
# The data was created in silico using the NetMHCpan 4.0 Server.
# NetMHCpan is the state-of-the-art binding predictor and available at:
# https://services.healthtech.dtu.dk/service.php?NetMHCpan-4.1
#
# Aim: Create a predictive model capable of predicting if a given peptide is
#      non-binder, weak binder or SB = strong binder. I.e. a 3-class classifier
#
# Variables:
#
# - peptide
#       The amino acid sequence of the 9-mer peptide
# - label_chr
#       The class, NB = non-binder, WB = weak binder, SB = strong binder
# - label_num
#       Numeric class label 0 = NB, 1 = WB, 2 = SB
# - data_type
#       Data partitioned into a test (~10%) and training set (~90%)

# View the data
pep_dat %>%
  print

# View class distribution
pep_dat %>%
  count(label_chr, data_type) %>%
  print

# Prepare data ------------------------------------------------------------

# We will use the BLOSUM62 matrix to convert the peptides to a numeric 
# representation
# Reference: https://en.wikipedia.org/wiki/BLOSUM
# Data source: https://www.ncbi.nlm.nih.gov/Class/FieldGuide/BLOSUM62.txt
bl62 <- read.table(file = "data/BLOSUM62.txt") %>% as.matrix

# Define training and test feature matrices
X_train <- pep_dat %>%
  filter(data_type == "train") %>%
  pull(peptide) %>% 
  encode_peptide(m = bl62)
X_test <- pep_dat %>%
  filter(data_type == "test") %>%
  pull(peptide) %>% 
  encode_peptide(m = bl62)

# Define known target classes for trainng and test data
y_train <- pep_dat %>%
  filter(data_type == "train") %>%
  pull(label_num) %>% 
  to_categorical
y_test <- pep_dat %>%
  filter(data_type == "test") %>%
  pull(label_num) %>% 
  to_categorical

# Define ANN model --------------------------------------------------------

# Set hyperparameters
n_hidden_1  <- 360
h1_activate <- "relu"
drop_out_1  <- 0
n_hidden_2  <- 360
h2_activate <- "relu"
drop_out_2  <- 0
n_hidden_3  <- 180
h3_activate <- "relu"
drop_out_3  <- 0
n_hidden_4  <- 90
h4_activate <- "relu"
drop_out_4  <- 0
n_hidden_5  <- 45
h5_activate <- "relu"
drop_out_5  <- 0
n_output    <- 3
o_ativate   <- "softmax"
n_epochs    <- 100
batch_size  <- 128
loss_func   <- "categorical_crossentropy"
learn_rate  <- 0.001

# Set architecture
model <- keras_model_sequential() %>% 
  layer_dense(units = n_hidden_1, activation = h1_activate, input_shape = 180) %>% 
  layer_dropout(rate = drop_out_1) %>% 
  layer_dense(units = n_hidden_2, activation = h2_activate) %>%
  layer_dropout(rate = drop_out_2) %>%
  layer_dense(units = n_hidden_3, activation = h3_activate) %>%
  layer_dropout(rate = drop_out_3) %>%
  layer_dense(units = n_hidden_4, activation = h4_activate) %>%
  layer_dropout(rate = drop_out_4) %>%
  layer_dense(units = n_hidden_5, activation = h5_activate) %>%
  layer_dropout(rate = drop_out_5) %>%
  layer_dense(units = n_output, activation = o_ativate)

# Compile model
model %>%
  compile(loss = loss_func,
          optimizer = optimizer_adam(learning_rate = learn_rate),
          metrics = c("accuracy")
)

# View model
model %>%
  summary %>%
  print

# How many parameters are in the model?

# Train model -------------------------------------------------------------
history <- model %>%
  fit(x = X_train,
      y = y_train,
      epochs = n_epochs,
      batch_size = batch_size,
      validation_split = 0
)

# Evaluate model ----------------------------------------------------------

# Get the test performance
perf_test <- model %>%
  evaluate(X_test, y_test) %>% 
  pluck("accuracy")
acc_test <- round(perf_test, 3)*100

# Get the training performance
perf_train <- model %>%
  evaluate(X_train, y_train) %>% 
  pluck("accuracy")
acc_train <- round(perf_train, 3)*100

# Create combined results
results <- bind_rows(
  tibble(y_true = y_test %>%
           get_class %>%
           as.factor,
         y_pred = predict_classes(model, X_test) %>%
           as.factor,
         Correct = ifelse(y_true == y_pred ,"yes", "no") %>%
           factor,
         data_type = "test")
  ,
  tibble(y_true = y_train %>%
           get_class %>%
           as.factor,
         y_pred = predict_classes(model, X_train) %>%
           as.factor,
         Correct = ifelse(y_true == y_pred ,"yes", "no") %>%
           factor,
         data_type = "train"))
my_counts <- results %>%
  count(y_pred, y_true, data_type)


# Visualise model performance ---------------------------------------------
plot_title = str_c(
  "Performance of Deep Feed Forward Neural Network (",
  "Total number of model parameters = ", count_params(model), ").")
plot_subtitle = str_c(
  "Test Accuracy = ", acc_test, "%, n = ", nrow(X_test), ". ",
  "Training Accuracy = ", acc_train, "%, n = ", nrow(X_train), ".")
plot_x_lab = "Predicted (Class assigned by Keras/TensorFlow deep FFN)"
plot_y_lab = "Measured (Real class, as predicted by netMHCpan-4.0)"
results %>%
  ggplot(mapping = aes(x = y_pred,
                       y = y_true,
                       fill = Correct)) +
  geom_jitter(pch = 21,
              size = 4,
              alpha = 0.4,
              colour = "black") +
  geom_text(data = my_counts,
            mapping = aes(x = y_pred,
                          y = y_true,
                          label = n),
            size = 20,
            inherit.aes = FALSE) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = plot_x_lab,
    y = plot_y_lab
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(labels = c("No", "Yes"),
                     values = c("tomato","cornflowerblue")) +
  facet_wrap(facets = vars(data_type),
             nrow = 1)


# What did the model learn? (posthoc analysis) ----------------------------

# From the test set, retrieve 100 peptides predicted to be class SB
test_pep_SB <- pep_dat %>%
  filter(data_type == "test") %>%
  mutate(pred_class = predict_classes(model,
                                      x = encode_peptide(peptide, bl62))) %>%
  filter(pred_class == 2) %>%
  sample_n(100) %>%
  pull(peptide)

# Now, perform an alanine scan, i.e. mutate each peptide position to an
# alanine (ala/A) and use the model to predict the impact of the mutation
mutational_scan = alanine_scan(test_pep_SB) %>%
  as_tibble %>% 
  pivot_longer(cols = everything(),
               names_to = "wildtype",
               values_to = "mutant") %>%
  arrange(wildtype) %>%
  mutate(mut_pos = find_mut_pos(wildtype, mutant),
         wildtype_class = predict_classes(model, encode_peptide(wildtype, bl62)),
         mutant_class = predict_classes(model, encode_peptide(mutant, bl62)))

# Then we can analyse the positional impact of the mutation by looking at
# differences between the class of the wildtype and the mutant
mutational_scan %>%
  drop_na(mut_pos) %>%
  group_by(mut_pos) %>%
  summarise(mean_wildtype_class = mean(wildtype_class),
            mean_mutant_class = mean(mutant_class)) %>% 
  ungroup() %>% 
  mutate(minus_delta_class = -(mean_mutant_class - mean_wildtype_class)) %>% 
  ggplot(aes(x = mut_pos, y = minus_delta_class)) +
  geom_col() +
  theme_bw()

# But what if you wanted to see exactly which amino acid residues are
# important at what positions in the peptide? Let us try to create a
# sequence logo, see https://en.wikipedia.org/wiki/Sequence_logo 
ggseqlogo(test_pep_SB)

# Save model --------------------------------------------------------------
save_model_hdf5(object = model,
                filepath = "Models/05_peptide_model.h5")
