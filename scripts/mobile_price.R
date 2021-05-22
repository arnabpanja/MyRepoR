# Data Set Link - https://www.kaggle.com/iabhishekofficial/mobile-price-classification

# Bob has started his own mobile company. He wants to give tough fight to 
# big companies like Apple,Samsung etc. He does not know how to estimate price 
# of mobiles his company creates. In this competitive mobile phone market you 
# cannot simply assume things. To solve this problem he collects sales data of 
# mobile phones of various companies.
# Bob wants to find out some relation between features of a mobile phone 
# (eg:- RAM,Internal Memory etc) and its selling price. But he is not so good 
# at Machine Learning. So he needs your help to solve this problem.
# 
# In this problem you do not have to predict actual price but a price range 
# indicating how high the price is


library(tidyverse)
library(janitor)
library(e1071)
library(randomForest)
library(rminer)



set.seed(1234)

# Load training data ------------
mob_train <- read_csv(file = 
                        "data/mobile_price/train.csv", 
                      col_types = cols()) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty(which = "rows")


names(mob_train)
dim(mob_train)


# Load test data ------------

mob_test <- read_csv(file = 
                       "data/mobile_price/test.csv", 
                     col_types = cols()) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty(which = "rows")


mob.test <- as.data.frame(x = mob_test, 
                          stringsAsFactors = FALSE)


# determine if there is any missing observation ------------
vec.miss <- apply(X = mob_train, 
                  MARGIN = 2, 
                  FUN = function(x) sum(is.na(x)))

# no observations are missing ------------
length(vec.miss[vec.miss > 0])


# Now we fit this into svm() cross validation ------------

mob.train <- as.data.frame(mob_train, 
                           stringsAsFactors = FALSE)

mob.train$price_range <- as.factor(mob.train$price_range)

# let us check whether we have any variable 
# that are factors apart from the response variable ------------

names(mob.train)


vec.factor <- apply(X = mob.train[, -21], 
                    MARGIN = 2, 
                    FUN = function(x) is.factor(x))

# no factor predictors ------------
length(vec.factor[vec.factor == TRUE])

# Record the starting time 
time.vec <- Sys.time()

# Radial Kernel Analysis ------------
tune.mob.radial <- tune(method = svm, 
                        train.x = mob.train[, -21], 
                        train.y = mob.train[, 21], 
                        kernel = "radial", 
                        ranges = list(cost = c(0.1, 1, 10), 
                                      gamma = c(1, 2, 3)))

# print the elapsed time of cross validation --------------
paste0("Radial Elapsed Tuning Time = ", 
       round(as.double(difftime(time1 = Sys.time(), 
                                time2 = time.vec, 
                                units = "secs")), 
             digits = 2), " Seconds")


# summary of the cross validation ------------
summary(tune.mob.radial)


# summary of the model ------------
summary(tune.mob.radial$best.model)



# Polynomial Kernel Analysis ------------

time.vec <- Sys.time()

tune.mob.poly <- tune(method = svm, 
                      train.x = mob.train[, -21], 
                      train.y = mob.train[, 21], 
                      kernel = "polynomial", 
                      ranges = list(cost = c(0.1, 1, 10), 
                                    d = c(3, 4, 5)))

# print the elapsed time of cross validation --------------
paste0("Polynomial Elapsed Tuning Time = ", 
       round(as.double(difftime(time1 = Sys.time(), 
                                time2 = time.vec, 
                                units = "secs")), 
             digits = 2), " Seconds")


# summary of the cross validation ------------
summary(tune.mob.poly)


# summary of the model ------------
summary(tune.mob.poly$best.model)



# Sigmoid Kernel Analysis ------------

time.vec <- Sys.time()

tune.mob.sigmoid <- tune(method = svm, 
                         train.x = mob.train[, -21], 
                         train.y = mob.train[, 21], 
                         kernel = "sigmoid", 
                         ranges = list(cost = c(0.1, 1, 10), 
                                       gamma = c(1, 2, 3)))

# print the elapsed time of cross validation --------------
paste0("Sigmoid Elapsed Tuning Time = ", 
       round(as.double(difftime(time1 = Sys.time(), 
                                time2 = time.vec, 
                                units = "secs")), 
             digits = 2), " Seconds")


# summary of the cross validation ------------
summary(tune.mob.sigmoid)



# summary of the model ------------
summary(tune.mob.sigmoid$best.model)


# store the cross validation best performances of each model -------
v.perf <- vector(mode = "double", length = 3)
names(v.perf) <- c("radial", "polynomial", "sigmoid")


v.perf[1] <- tune.mob.radial$best.performance

v.perf[2] <- tune.mob.poly$best.performance

v.perf[3] <- tune.mob.sigmoid$best.performance


# creating data frame for plotting -------
df_perf <- as.data.frame(cbind("kernel" = names(v.perf), 
                               "best_performance" = v.perf))

rownames(df_perf) <- 1:nrow(df_perf)

# plotting the performances --------
p_performances <- df_perf %>% 
  ggplot(mapping = aes(x = as.double(best_performance), 
                       y = reorder(kernel, as.double(best_performance)))) + 
  geom_col(mapping = aes(fill = kernel), 
           show.legend = FALSE, 
           width = 0.50) +
  geom_text(mapping = aes(label = round(as.double(best_performance), 
                                        digits = 2)), 
            show.legend = FALSE, 
            hjust = 1, 
            color = "white", 
            fontface = "bold", size = 3.5) + 
  scale_fill_brewer(palette = "Set1") + 
  labs(x = "Error", 
       y = "Kernel", 
       title = "Support Vector Machines", 
       subtitle = "Kernel Performances")

p_performances

# save the plot -----
ggsave(filename = "plots/mobile_price/p_performances.png", 
       plot = p_performances)


# So working on the polynomial kernel 
# as this model has the LOWEST cross validation error ------------

pred.price.poly <- predict(object = tune.mob.poly$best.model, 
                           newdata = mob.test[, -1])

df.pred.poly <- as.data.frame(
  cbind("id" = mob.test[, 1], 
        "pred.price" = as.integer(as.character(pred.price.poly))), 
  stringsAsFactors = FALSE)



head(df.pred.poly) 


paste0("==== Polynomial Kernel Predictions ====")

df.pred.poly %>% count(pred.price, 
                       sort = TRUE)


# Random Forest Model -----------------------

# Set the mtry parameter to sqrt(p) for ----- 
# classification tree ----
v.mtry <- floor((sqrt(length(names(mob.train)) - 1)))



time.vec <- Sys.time()

rf.mod <- randomForest(formula = price_range ~ ., 
                       data = mob.train, 
                       mtry = v.mtry, 
                       ntree = 500, 
                       importance = TRUE)


# print the elapsed time of random forest model --------------
paste0("Random Forest Elapsed Tuning Time = ", 
       round(as.double(difftime(time1 = Sys.time(), 
                                time2 = time.vec, 
                                units = "secs")), 
             digits = 2), " Seconds")


# predict the responce price range --------------------
pred.price.rf <- predict(object = rf.mod, newdata = mob.test[, -1])


# combine this with the test data frame and with the id of each record ----
df.pred.rf <- as.data.frame(
  cbind("id" = mob.test[, 1], 
        "pred.price" = as.integer(as.character(pred.price.rf))), 
  stringsAsFactors = FALSE)


# sample the first 6 observations ------
head(df.pred.rf) 


paste0("==== Random Forest Predictions ====")

df.pred.rf %>% count(pred.price, 
                     sort = TRUE)


# Study the feature importance --------

# Random Forest Model --------

df.imp1 <- as.data.frame(cbind("pred" = rownames(rf.mod$importance[, c(5, 6)]), 
                               apply(rf.mod$importance[, c(5, 6)], 
                                     MARGIN = 2, 
                                     FUN = function(x) 
                                       as.double(round(x,
                                                       digits = 5)))
))



rownames(df.imp1) <- 1:nrow(df.imp1)
colnames(df.imp1)[c(2,3)] <- c("mean_decr_accu", 
                               "mean_decr_gini")



p_rf_imp_accu <- ggplot(data = df.imp1) + 
  geom_col(mapping = aes(x = as.double(mean_decr_accu), 
                         y = reorder(as.factor(pred), 
                                     as.double(mean_decr_accu)), 
                         fill = pred), 
           show.legend = FALSE) + 
  scale_x_continuous(n.breaks = 10) + 
  labs(x = "Mean Decrease in Accuracy", 
       y = "Features", 
       title = "Random Forest", 
       subtitle = "Feature Importance - By decrease in Accuracy")

p_rf_imp_accu

# save the plot -----
ggsave(filename = "plots/mobile_price/p_rf_imp_accu.png", 
       plot = p_rf_imp_accu)


p_rf_imp_gini <- ggplot(data = df.imp1) + 
  geom_col(mapping = aes(x = as.double(mean_decr_gini), 
                         y = reorder(as.factor(pred), 
                                     as.double(mean_decr_gini)), 
                         fill = pred), 
           show.legend = FALSE) + 
  scale_x_continuous(n.breaks = 10) + 
  scale_fill_viridis_d(alpha = 0.75) + 
  labs(x = "Mean Decrease in Gini Index", 
       y = "Features", 
       title = "Random Forest", 
       subtitle = "Feature Importance - By decrease in Gini Index")

p_rf_imp_gini



# save the plot -----
ggsave(filename = "plots/mobile_price/p_rf_imp_gini.png", 
       plot = p_rf_imp_gini)


# Support Vector Machines - Polynomial Kernel -------

# this is working with fit function ---------
# svm.model <- rminer::fit(x = price_range ~ ., data = mob.train, 
#                          model = "svm", 
#                          task = "class")
# 
# poly_imp <- rminer::Importance(M = svm.model, 
#                                data = mob.train, 
#                                method = "SA")

svm.poly <- tune.mob.poly$best.model

pred.poly <- function(svm.poly, mob.train){
  return(predict(object = svm.poly, 
                 newdata = mob.train[, -21]))
}

poly.imp <- rminer::Importance(M = tune.mob.poly$best.model, 
                               data = mob.train[, -21], 
                               method = "sens", 
                               PRED = pred.poly, 
                               measure = "AAD",
                               baseline = "range",  
                               RealL = 10)


svm.imp <- as.data.frame(
  cbind(
    "var" = names(mob.train)[-21], 
    "imp" = as.double(round(poly.imp$imp, digits = 2))
  ))



p_svm_imp <- ggplot(data = svm.imp) + 
  geom_col(mapping = aes(x = as.double(imp), 
                         y = reorder(as.factor(var), 
                                     as.double(imp)), 
                         fill = var), 
           show.legend = FALSE) +
  scale_x_continuous(n.breaks = 10) + 
  scale_fill_viridis_d(alpha = 0.8) + 
  labs(x = "Importance", 
       y = "Features", 
       title = "Support Vector Machines", 
       subtitle = "Polynomial Kernel - Feature Importance")

p_svm_imp



# save the plot -----
ggsave(filename = "plots/mobile_price/p_svm_imp.png", 
       plot = p_svm_imp)
