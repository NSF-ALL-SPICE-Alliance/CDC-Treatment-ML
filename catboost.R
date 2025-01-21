# Libraries ----
library(catboost)
library(tidyverse)
library(arrow)
library(here)
library(naniar)


### Read feather 
teds_d <- arrow::read_feather(here("teds_d_15_19.feather"))

### Check
glimpse(teds_d)

# Missing Data ----

missing_summary <- miss_var_summary(teds_d)


## Filter for completed treatments only

table(teds_d$reason)

teds_d_completed <- teds_d %>% 
  filter(reason == 1)


missing_summary_completed <- miss_var_summary(teds_d_completed)


## Drop columns where more than 55% of data is missing
columns_to_drop <- missing_summary_completed %>%
  filter(pct_miss > 55) %>%
  pull(variable)  # Extract the column names

columns_to_drop

# Drop columns with more than 55% missing data
teds_d_completed_clean <- teds_d_completed %>%
  select(-all_of(columns_to_drop))

# Print the updated dataset
glimpse(teds_d_completed_clean)


### Bin Length of Stay

# Custom binning for los
teds_d_completed_clean <- teds_d_completed_clean %>%
  mutate(
    los_binned = case_when(
      los == 1 ~ "1 day",                 # 1 day treatments
      los >= 2 & los <= 10 ~ "2-10 days", # 2 to 10 days
      los >= 11 & los <= 21 ~ "11-21 days", # 12 to 21 days
      los >= 22 & los <= 30 ~ "22-30 days", # 22 to 30 days
      los == 31 ~ "31-45 days",           # Existing codebook bins
      los == 32 ~ "46-60 days",
      los == 33 ~ "61-90 days",
      los == 34 ~ "91-120 days",
      los == 35 ~ "121-180 days",
      los == 36 ~ "181-365 days",
      los == 37 ~ ">365 days",
      TRUE ~ NA_character_                # Handle unexpected values
    ),
    los_binned = factor(
      los_binned,
      levels = c("1 day", "2-10 days", "12-21 days", "22-30 days",
                 "31-45 days", "46-60 days", "61-90 days", 
                 "91-120 days", "121-180 days", "181-365 days", ">365 days"),
      ordered = TRUE
    )
  )

# Verify the new bins
table(teds_d_completed_clean$los_binned)


# Check the frequency table of the new variable
teds_d_completed_clean %>% 
  select(los, los_binned)

### Ensure factor
class(teds_d_completed_clean$los_binned)

### Drop los

teds_d_completed_clean <- teds_d_completed_clean %>% 
  select(-los)

### Check freq1_d

table(teds_d_completed_clean$freq1_d)

sum(is.na(teds_d_completed_clean$freq1_d))

teds_d_completed_clean_no_freq1_na <- teds_d_completed_clean %>%
  filter(!is.na(freq1_d))

teds_d_completed_clean_no_freq1_na <- teds_d_completed_clean_no_freq1_na %>%
  mutate(
    freq1_d = case_when(
      freq1_d == 1 ~ "no use",
      freq1_d %in% c(2, 3) ~ "some/daily use",
      TRUE ~ as.character(freq1_d)  # Preserve other values as they are
    )
  )

table(teds_d_completed_clean_no_freq1_na$freq1_d)

### Mutate all to factor

teds_d_completed_clean_no_freq1_na <- teds_d_completed_clean_no_freq1_na %>%
  mutate(across(everything(), as.factor)) 

table(teds_d_completed_clean_no_freq1_na$freq1_d)

glimpse(teds_d_completed_clean_no_freq1_na)


### Prep freq1_d

# Convert target variable to numeric (starting from 0)
teds_d_completed_clean_no_freq1_na$freq1_d <- as.integer(as.factor(teds_d_completed_clean_no_freq1_na$freq1_d)) - 1

### Check
table(teds_d_completed_clean_no_freq1_na$freq1_d)

# Model ----

freq_use_rf_data <- teds_d_completed_clean_no_freq1_na

glimpse(freq_use_rf_data)

# Split Data into Training and Testing Sets

set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(freq_use_rf_data), 0.8 * nrow(freq_use_rf_data))
train_data <- freq_use_rf_data[train_indices, ]
test_data <- freq_use_rf_data[-train_indices, ]



# Create CatBoost pools
train_pool <- catboost.load_pool(
  data = train_data %>% select(-freq1_d),
  label = train_data$freq1_d
)

test_pool <- catboost.load_pool(
  data = test_data %>% select(-freq1_d),
  label = test_data$freq1_d
)

## Train the CatBoost Model ----

# Define parameters
params <- list(
  loss_function = "Logloss",       # Binary classification
  eval_metric = "AUC",            # Metric for evaluation
  iterations = 500,               # Number of boosting iterations
  depth = 6,                      # Depth of trees
  learning_rate = 0.1,            # Learning rate
  verbose = 100                   # Log every 100 iterations
)

# # # Train the model
model <- catboost.train(
  learn_pool = train_pool,
  params = params
)
#
# # Save the model
# # Define the file path for saving the model
model_path <- here("models", "catboost_model.bin")
# # 
# # 
# # # Save the model
catboost.save_model(
  model = model,
  model_path = model_path
)

# Load Model
#model <- catboost.load_model(model_path)

## Evaluate the Model ----

# Predict probabilities for the test set
predictions <- catboost.predict(model, test_pool, prediction_type = "Probability")

# Convert probabilities to binary predictions (threshold 0.5)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Compute confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$freq1_d)
print(confusion_matrix)

# Compute AUC
library(pROC)
auc <- roc(test_data$freq1_d, predictions)
print(paste("AUC:", auc$auc))


## Feature Importance ----
# Get feature importance
feature_importance <- catboost.get_feature_importance(model)

# Create a data frame for visualization
importance_df <- data.frame(
  Feature = colnames(train_data %>% select(-freq1_d)),
  Importance = feature_importance
)

# Plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")


## Partial dependence  ----
# Get SHAP values for the test data
shap_values <- catboost.get_feature_importance(
  model,
  pool = test_pool,
  type = "ShapValues"
)

### STFIPS ----

# Extract SHAP values for a specific feature (e.g., "stfips")
shap_feature_stfips <- shap_values[, which(colnames(train_data) == "stfips")]

# Combine SHAP values with test data for visualization
shap_df_stfips <- data.frame(
  FeatureValue = test_data$stfips,
  ShapValue = shap_feature_stfips
)

shap_df_stfips <- shap_df_stfips %>%
  group_by(FeatureValue) %>%
  mutate(median = median(ShapValue)) %>%
  ungroup()

shap_df_stfips <- shap_df_stfips %>%
  mutate(FeatureValue = case_when(
    FeatureValue == "1" ~ "Alabama",
    FeatureValue == "2" ~ "Alaska",
    FeatureValue == "4" ~ "Arizona",
    FeatureValue == "5" ~ "Arkansas",
    FeatureValue == "6" ~ "California",
    FeatureValue == "8" ~ "Colorado",
    FeatureValue == "9" ~ "Connecticut",
    FeatureValue == "10" ~ "Delaware",
    FeatureValue == "11" ~ "District of Columbia",
    FeatureValue == "12" ~ "Florida",
    FeatureValue == "13" ~ "Georgia",
    FeatureValue == "15" ~ "Hawaii",
    FeatureValue == "16" ~ "Idaho",
    FeatureValue == "17" ~ "Illinois",
    FeatureValue == "18" ~ "Indiana",
    FeatureValue == "19" ~ "Iowa",
    FeatureValue == "20" ~ "Kansas",
    FeatureValue == "21" ~ "Kentucky",
    FeatureValue == "22" ~ "Louisiana",
    FeatureValue == "23" ~ "Maine",
    FeatureValue == "24" ~ "Maryland",
    FeatureValue == "25" ~ "Massachusetts",
    FeatureValue == "26" ~ "Michigan",
    FeatureValue == "27" ~ "Minnesota",
    FeatureValue == "28" ~ "Mississippi",
    FeatureValue == "29" ~ "Missouri",
    FeatureValue == "30" ~ "Montana",
    FeatureValue == "31" ~ "Nebraska",
    FeatureValue == "32" ~ "Nevada",
    FeatureValue == "33" ~ "New Hampshire",
    FeatureValue == "34" ~ "New Jersey",
    FeatureValue == "35" ~ "New Mexico",
    FeatureValue == "36" ~ "New York",
    FeatureValue == "37" ~ "North Carolina",
    FeatureValue == "38" ~ "North Dakota",
    FeatureValue == "39" ~ "Ohio",
    FeatureValue == "40" ~ "Oklahoma",
    FeatureValue == "42" ~ "Pennsylvania",
    FeatureValue == "44" ~ "Rhode Island",
    FeatureValue == "45" ~ "South Carolina",
    FeatureValue == "46" ~ "South Dakota",
    FeatureValue == "47" ~ "Tennessee",
    FeatureValue == "48" ~ "Texas",
    FeatureValue == "49" ~ "Utah",
    FeatureValue == "50" ~ "Vermont",
    FeatureValue == "51" ~ "Virginia",
    FeatureValue == "55" ~ "Wisconsin",
    FeatureValue == "56" ~ "Wyoming",
    FeatureValue == "72" ~ "Puerto Rico",
    TRUE ~ FeatureValue  # Keep as is for any unmatched values
  ))

# Plot partial dependence
#library(ggplot2)
ggplot(shap_df_stfips, aes(x = reorder(FeatureValue, median), y = ShapValue)) +
  geom_boxplot() +
  labs(title = "Partial Dependence for stfips",
       x = "Feature Value (stfips)",
       y = "SHAP Value (Impact on Prediction)") +
  coord_flip() +
  theme_bw()

### Freq1 ----

# Extract SHAP values for a specific feature (e.g., "freq1")
shap_feature_freq1 <- shap_values[, which(colnames(train_data) == "freq1")]

# Combine SHAP values with test data for visualization
shap_df_freq1 <- data.frame(
  FeatureValue = test_data$freq1,
  ShapValue = shap_feature_freq1
)

shap_df_freq1 <- shap_df_freq1 %>%
  group_by(FeatureValue) %>%
  mutate(median = median(ShapValue)) %>%
  ungroup()

shap_df_freq1 <- shap_df_freq1 %>%
  mutate(FeatureValue = case_when(
    FeatureValue == "1" ~ "No use in the past month",
    FeatureValue == "2" ~ "Some use",
    FeatureValue == "3" ~ "Daily use",
    TRUE ~ FeatureValue  # Keep as is for any unmatched values
  ))

# Plot partial dependence
#library(ggplot2)
ggplot(shap_df_freq1, aes(x = reorder(FeatureValue, median), y = ShapValue)) +
  geom_boxplot() +
  labs(title = "Partial Dependence for frequency of use at admission",
       x = "Feature Value (freq1)",
       y = "SHAP Value (Impact on Prediction)") +
  coord_flip() +
  theme_bw()


### LOS Binned ----
# Extract SHAP values for a specific feature (e.g., "los_binned")
shap_feature_los_binned <- shap_values[, which(colnames(train_data) == "los_binned")]

# Combine SHAP values with test data for visualization
shap_df_los_binned <- data.frame(
  FeatureValue = test_data$los_binned,
  ShapValue = shap_feature_los_binned
)

shap_df_los_binned <- shap_df_los_binned %>%
  group_by(FeatureValue) %>%
  mutate(median = median(ShapValue)) %>%
  ungroup()

ggplot(shap_df_los_binned, aes(x = reorder(FeatureValue, median), y = ShapValue)) +
  geom_boxplot() +
  labs(title = "Partial Dependence for binned length of stay",
       x = "Feature Value (los_binned)",
       y = "SHAP Value (Impact on Prediction)") +
  coord_flip() +
  theme_bw()

