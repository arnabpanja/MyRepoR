# Dataset Courtesy :- Kaggle
# Data Set Link :- https://www.kaggle.com/anmolkumar/health-insurance-cross-sell-prediction

library(tidyverse)
library(janitor)
library(xgboost)
library(ROSE)

train.data <- read_csv(file = "data/insurance_cross_sell/train.csv") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty(which = "rows")

dim(train.data)
names(train.data)

train.data %>% count(gender, sort = TRUE)
train.data %>% count(vehicle_age, sort = TRUE)
train.data %>% count(response, sort = TRUE)

set.seed(1234)

# sample the training rows by 50% for training and test set
train.index <- sample(1:nrow(train.data), 
                      round(0.5 * nrow(train.data), digits = 0))

# subset the training data frame
train.data.train <- train.data[train.index, ]
# subset the testing data frame 
train.data.test <- train.data[-train.index, ]

# all factor predictors converted to integers for modeling -----
train.data.train[, c("gender", 
                     "vehicle_age", 
                     "vehicle_damage")] <- 
  apply(X = train.data.train[, c("gender", 
                                "vehicle_age", 
                                "vehicle_damage")], 
       MARGIN = 2, 
       FUN = function(x) as.integer(as.factor(x)))



# handling imbalanced data ----------------------
# train.data.rose <- ROSE::ROSE(formula = response ~ ., 
#                               data = train.data.train[, -1], 
#                               seed = 1)$data

train.data.rose <- ROSE::ovun.sample(formula = response ~ ., 
                                     data = train.data.train[, -1], 
                                     method = "under", 
                                     N = min(train.data.train %>% 
                                               count(response) %>% 
                                               select(n)) * 2, 
                                     seed = 1)$data


train.rose.mat <- as.matrix(train.data.rose)


# train the model
set.seed(1234)

# param list for Xgboost 
param_list <- list(eta = 0.15, 
                   max_depth = 6, 
                   min_child_weight = 1, 
                   subsample = 0.8, 
                   colsample_bytree = 0.8,
                   objective = "binary:logistic", 
                   eval_metric = "auc")

# Apply cross validation to find the best iteration ---------
xgb_cv <- xgb.cv(params = param_list, 
                 data = train.rose.mat[, -11], 
                 nrounds = 150, 
                 nfold = 10, 
                 label = as.matrix(train.rose.mat[, 11], drop = FALSE),  
                 verbose = 1, 
                 print_every_n = 25)


# best iteration 
best_iter <- min(xgb_cv$evaluation_log[(xgb_cv$evaluation_log)$test_auc_mean == max((xgb_cv$evaluation_log)$test_auc_mean), ]$iter)


auc_log_train <- rbind(cbind("type" = rep("train_auc", nrow(xgb_cv$evaluation_log)), 
                               xgb_cv$evaluation_log[, c("iter", "train_auc_mean")]), 
                         cbind("type" = rep("test_auc", nrow(xgb_cv$evaluation_log)), 
                               xgb_cv$evaluation_log[, c("iter", "test_auc_mean")]), 
                         use.names = FALSE) %>% 
  setNames(c("auc_type", "iter", "auc_value"))

p_plot_auc_train <- ggplot(data = auc_log_train) + 
  geom_point(mapping = aes(x = iter, 
                           y = auc_value, 
                           color = auc_type), 
             show.legend = TRUE, size = 1, position = "jitter") +
  scale_color_manual(values = c("red", "blue")) + 
  theme(text = element_text(size = 10), 
        plot.title = element_text(face = "bold")) + 
  labs(x = "Iterations", 
       y = "Mean AUC", 
       title = "Xgboost - AUC Variations (Training Data)", 
       subtitle = paste0("Test AUC is highest at Iteration ", 
                         best_iter, "\nTraining AUC keeps on increasing"))


p_plot_auc_train

# save the plot -----
ggsave(filename = "plots/insurance_cross_sell/p_plot_auc_train.png", 
       plot = p_plot_auc_train)


# train the model using the best iteration number
set.seed(1234)


xgboost_model <- xgboost(data = train.rose.mat[, -11], 
                         label = as.matrix(train.rose.mat[, 11], drop = FALSE), 
                         params = param_list, 
                         nrounds = best_iter, 
                         print_every_n = 25)



# observe the model importance
xgb_model_imp_train <- xgb.importance(model = xgboost_model)

p_feature_importances_train <- ggplot(data = xgb_model_imp_train) + 
  geom_col(mapping = aes(x = Gain, 
                         y = reorder(Feature, Gain), 
                         fill = Feature), 
           show.legend = FALSE) + 
  scale_fill_brewer(palette = "Paired") + 
  theme(text = element_text(size = 10), 
        plot.title = element_text(face = "bold")) + 
  labs(x = "Gain", 
       y = "Features", 
       title = "Xgboost - Feature Importance Visualization (Training Data)", 
       subtitle = paste0("Vehicle Damage, Previously Insured, Age are key features"))


p_feature_importances_train


# save the plot -----
ggsave(filename = "plots/insurance_cross_sell/p_feature_importances_train.png", 
       plot = p_feature_importances_train)




train.data.test[, c("gender", 
                     "vehicle_age", 
                     "vehicle_damage")] <- 
  apply(X = train.data.test[, c("gender", 
                                 "vehicle_age", 
                                 "vehicle_damage")], 
        MARGIN = 2, 
        FUN = function(x) as.integer(as.factor(x)))

# use the model to make predictions 
set.seed(1234)

pred_xgboost <- predict(object = xgboost_model, 
                        newdata = as.matrix(train.data.test)[, -c(1, 12)])


# Max and Min Probabilities 
max(pred_xgboost)
min(pred_xgboost)

# error rate 
mean(ifelse(pred_xgboost > 0.49, 1, 0) == as.matrix(train.data.test)[, 12])



# correlation matrix 
table("preds" = ifelse(pred_xgboost > 0.49, 1, 0), 
      "actuals" = as.matrix(train.data.test)[, 12])



# correlation table 
cbind("preds" = ifelse(pred_xgboost > 0.49, 1, 0), 
      "actuals" = as.matrix(train.data.test)[, 12]
) %>% as_tibble() %>% 
  group_by(preds, actuals) %>% 
  summarise(cnt = n(), .groups = "drop_last")




# THIRD and FINAL ATTEMPT --------------------------------------
# now let us apply the whole logic to the actual training data ---- 
# including cross validation to select the best iteration

set.seed(1234)

# all factor predictors converted to integers for modeling -----
train.data[, c("gender", 
                     "vehicle_age", 
                     "vehicle_damage")] <- 
  apply(X = train.data[, c("gender", 
                                 "vehicle_age", 
                                 "vehicle_damage")], 
        MARGIN = 2, 
        FUN = function(x) as.integer(as.factor(x)))



# handling imbalanced data ----------------------


train.rose <- ROSE::ovun.sample(formula = response ~ ., 
                                     data = train.data[, -1], 
                                     method = "under", 
                                     N = min(train.data %>% 
                                               count(response) %>% 
                                               select(n)) * 2, 
                                     seed = 1)$data


train.rose.matrix <- as.matrix(train.rose)



param_list <- list(eta = 0.15, 
                   max_depth = 6, 
                   min_child_weight = 1, 
                   subsample = 0.8, 
                   colsample_bytree = 0.8,
                   objective = "binary:logistic", 
                   eval_metric = "auc")

# Apply cross validation to find the best iteration ---------
xgb_cv_1 <- xgb.cv(params = param_list, 
                 data = train.rose.matrix[, -11], 
                 nrounds = 150, 
                 nfold = 10, 
                 label = as.matrix(train.rose.matrix[, 11], drop = FALSE),  
                 verbose = 1, 
                 print_every_n = 25)


# best iteration 
best_iter <- min(xgb_cv_1$evaluation_log[(xgb_cv_1$evaluation_log)$test_auc_mean == max((xgb_cv_1$evaluation_log)$test_auc_mean), ]$iter)


# plotting

error_log <- rbind(cbind("type" = rep("train_auc", nrow(xgb_cv_1$evaluation_log)), 
                         xgb_cv_1$evaluation_log[, c("iter", "train_auc_mean")]), 
                   cbind("type" = rep("test_auc", nrow(xgb_cv_1$evaluation_log)), 
                         xgb_cv_1$evaluation_log[, c("iter", "test_auc_mean")]), 
                   use.names = FALSE) %>% 
  setNames(c("auc_type", "iter", "auc_value"))

p_plot_auc <- ggplot(data = error_log) + 
  geom_point(mapping = aes(x = iter, 
                           y = auc_value, 
                           color = auc_type), 
             show.legend = TRUE, size = 1, 
             position = "jitter") +
  scale_color_manual(values = c("red", "blue")) + 
  theme(text = element_text(size = 10), 
        plot.title = element_text(face = "bold")) + 
  labs(x = "Iterations", 
       y = "Mean AUC", 
       title = "Xgboost - AUC Variations", 
       subtitle = paste0("Test AUC is highest at Iteration ", 
                         best_iter, "\nTraining AUC keeps on increasing"))

p_plot_auc

# save the plot -----
ggsave(filename = "plots/insurance_cross_sell/p_plot_auc.png", 
       plot = p_plot_auc)




# create the model based on the CV best iteration number
xgboost_model_final <- xgboost(data = train.rose.matrix[, -11], 
                         label = as.matrix(train.rose.matrix[, 11], 
                                           drop = FALSE), 
                         params = param_list, 
                         nrounds = best_iter, 
                         print_every_n = 25)

# observe the model importance
xgb_model_imp <- xgb.importance(model = xgboost_model_final)


p_feature_importances <- ggplot(data = xgb_model_imp) + 
  geom_col(mapping = aes(x = Gain, 
                         y = reorder(Feature, Gain), 
                         fill = Feature), 
           show.legend = FALSE) + 
  scale_fill_brewer(palette = "Paired") + 
  theme(text = element_text(size = 10), 
        plot.title = element_text(face = "bold")) + 
  labs(x = "Gain", 
       y = "Features", 
       title = "Xgboost - Feature Importance Visualization", 
       subtitle = paste0("Vehicle Damage, Previuosly Insured, Age are key features"))


p_feature_importances

# save the plot -----
ggsave(filename = "plots/insurance_cross_sell/p_feature_importances.png", 
       plot = p_feature_importances)



# plot the model importance
# xgb.plot.importance(importance_matrix = xgb_model_imp)



# load test data 
test.data <- read_csv(file = "data/insurance_cross_sell/test.csv") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty(which = "rows")


test.data[, c("gender", 
               "vehicle_age", 
               "vehicle_damage")] <- 
  apply(X = test.data[, c("gender", 
                           "vehicle_age", 
                           "vehicle_damage")], 
        MARGIN = 2, 
        FUN = function(x) as.integer(as.factor(x)))





# use the Xgboost model to make predictions ----
set.seed(1234)

pred_xgboost_final <- predict(object = xgboost_model_final, 
                              newdata = as.matrix(test.data)[, -1])

# Max and Min Probabilities 
max(pred_xgboost_final)
min(pred_xgboost_final)

# predicted responses 
pred_df <- data.frame(response = ifelse(pred_xgboost_final > 0.49, 1, 0))

# sample predictions
pred_df %>% head()


# distributions 
pred_df %>% count(response, sort = TRUE)

# combining the whole data with responses 

test_df_wresponse <- cbind(test.data, pred_df)

# Response distributions 
test_df_wresponse %>% 
  count(response, sort = TRUE)
