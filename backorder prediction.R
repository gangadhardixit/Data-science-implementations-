debug(utils:::unpackPkgZip)
install.packages("tidyquant")

#Install necessary librarieis

install.packages("dplyr")
library(ROSE)
library(magrittr)
library(tidyr)
library(unbalanced)
library(h2o)
library(tidyquant)
library(sqldf)
library(dplyr)
library(h2o)
library(ggplot2)
library(tidyquant)
install.packages("tidyverse")
install.packages("tidyquant")
#Read Backorder Data from Local store 
Data =read.csv("C://Users/gandixit/Desktop/R implementations/backorder predict/Kaggle_Training_Dataset_v2.csv",header = T)

#Balace imbalanced before performing preprocessing 
Data_balanaced=ROSE(went_on_backorder ~ ., data = Data, seed = 1)$data
table(Data_balanaced$went_on_backorder)

#Remove SKU unique nos
Data_balanaced=Data_balanaced[-1,]
str(Data_balanaced)

#Split data into Training & Test sets 

rownumbers=sample(1:nrow(Data_balanaced),0.7*nrow(Data_balanaced))
train_data=Data_balanaced[rownumbers,]
test_data=Data_balanaced[-rownumbers,]


#initialize H2O 

h2o::h2o.init()
preprocess_raw_data <- function(data) {
# data = data frame of backorder data
#Function to preprocess data 
data %>%
select(-sku) %>%
drop_na(national_inv) %>%
mutate(lead_time = ifelse(is.na(lead_time), -99, lead_time)) %>%
mutate_if(is.character, .funs = function(x) ifelse(x == "Yes", 1, 0)) %>%
mutate(went_on_backorder = as.factor(went_on_backorder))
}

# Apply the preprocessing steps
train_df <- preprocess_raw_data(train_data) 
valid_df <- preprocess_raw_data(valid_raw_df) 
test_df  <- preprocess_raw_data(test_data)
str(train_df)

#Convert data frames to H2o frames 
train_h2o <- as.h2o(train_data)
test_h2o  <- as.h2o(test_data)


y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 200
)


automl_leader <- automl_models_h2o@leader

automl_leader1 <- automl_models_h2o@project_name

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
as.tibble(pred_h2o)
tibble::is.tibble(a)
#Test performance on test data 
perf_h2o <- h2o.performance(automl_leader, newdata = test_h2o) 


# Plot ROC Curve
left_join(h2o.tpr(perf_h2o), h2o.fpr(perf_h2o)) %>%
  mutate(random_guess = fpr) %>%
  select(-threshold) %>%
  ggplot(aes(x = fpr)) +
  geom_area(aes(y = tpr, fill = "AUC"), alpha = 0.5) +
  geom_point(aes(y = tpr, color = "TPR"), alpha = 0.25) +
  geom_line(aes(y = random_guess, color = "Random Guess"), size = 1, linetype = 2) +
  theme_tq() +
  scale_color_manual(
    name = "Key", 
    values = c("TPR" = palette_dark()[[1]],
               "Random Guess" = palette_dark()[[2]])
  ) +
  scale_fill_manual(name = "Fill", values = c("AUC" = palette_dark()[[5]])) +
  labs(title = "ROC Curve", 
       subtitle = "Model is performing much better than random guessing") +
  annotate("text", x = 0.25, y = 0.65, label = "Better than guessing") +
  annotate("text", x = 0.75, y = 0.25, label = "Worse than guessing")


h2o.auc(perf_h2o)


perf_h2o@metrics$max_criteria_and_metric_scores



# Plot recall and precision vs threshold, visualize inventory strategy effect
left_join(h2o.recall(perf_h2o), h2o.precision(perf_h2o)) %>%
  rename(recall = tpr) %>%
  gather(key = key, value = value, -threshold) %>%
  ggplot(aes(x = threshold, y = value, color = key)) +
  geom_point(alpha = 0.5) +
  scale_color_tq() +
  theme_tq() +
  labs(title = 'Precision and Recall vs Cutoff ("Yes" Threshold)',
       subtitle = "As the cutoff increases from zero, inventory strategy becomes more conservative",
       caption = "Cut off decision",
       x = 'Cutoff (Probability above which we predict went_on_backorder = "Yes")',
       y = "Precision and Recall Values"
  ) +
  # p>=0
  geom_vline(xintercept = 0, color = palette_light()[[3]], size = 1) +
  annotate("text", x = 0.12, y = 0.75, size = 3,
           label = 'p1 >= 0: "Yes"\nInventory\nEverything') +
  geom_segment(x = 0, y = 0.7, xend = 0.02, yend= 0.72, color = palette_light()[[3]], size = 1) +
  # p>=0.25
  geom_vline(xintercept = 0.25, color = palette_light()[[3]], size = 1) +
  annotate("text", x = 0.37, y = 0.35, size = 3,
           label = 'p1 >= 0.25: "Yes"\nInventory Anything\nWith Chance\nof Backorder') +
  geom_segment(x = 0.25, y = 0.30, xend = 0.27, yend= 0.32, color = palette_light()[[3]], size = 1) +
  # p>=0.5
  geom_vline(xintercept = 0.5, color = palette_light()[[3]], size = 1) +
  annotate("text", x = 0.62, y = 0.75, size = 3,
           label = 'p1 >= 0.50: "Yes"\nInventory\nProbability\nSplit 50/50') +
  geom_segment(x = 0.5, y = 0.70, xend = 0.52, yend= 0.72, color = palette_light()[[3]], size = 1) +
  # p>=0.75
  geom_vline(xintercept = 0.75, color = palette_light()[[3]], size = 1) +
  annotate("text", x = 0.87, y = 0.75, size = 3,
           label = 'p1 >= 0.75: "Yes"\nInventory Very\nConservatively\n(Most Likely Backorder)') +
  geom_segment(x = 0.75, y = 0.70, xend = 0.77, yend= 0.72, color = palette_light()[[3]], size = 1) +
  # p>=1
  geom_vline(xintercept = 1, color = palette_light()[[3]], size = 1) +
  annotate("text", x = 0.87, y = 0.22, size = 3,
           label = 'p1 >= 1.00: "Yes"\nInventory Nothing') +
  geom_segment(x = 1.00, y = 0.23, xend = 0.98, yend= 0.21, color = palette_light()[[3]], size = 1)





