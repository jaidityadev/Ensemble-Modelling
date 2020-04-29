library(forecast)
library(ggplot2)
library(randomForest)
library(feather) # data import
library(data.table) # data handle
library(rpart) # decision tree method
library(party) # decision tree method

data <- as.data.table(read.csv(file.choose()))

n_date <- unique(data[,Date]) 
period <- 1

theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))

data_train <- data[Date %in% n_date[1:21]]
data_test <- data[Date %in% n_date[22]]

ggplot(data_train, aes(Date,Total)) +
  geom_point() + geom_smooth()
labs(x = "Date", y = "Number of People Affected by COVID-19") +
  theme_ts

#Bagging 
data_ts <- ts(data_train$Total, freq = 95)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series
trend_part <- ts(decomp_ts[,2])
# ARIMA  
trend_fit <- auto.arima(trend_part) 
trend_for <- as.vector(forecast(trend_fit, period)$mean) # trend forecast
data_msts <- msts(data_train$Total, seasonal.periods = c(period, period*7))

K <- 0.5
# Fourier features to model (Daily and Weekly)
fuur <- fourier(data_msts, K = c(K, K)) 

N <- nrow(data_train)
window <- (N / period) - 1

new_load <- rowSums(decomp_ts[, c(1,3)]) # detrended original time series
lag_seas <- decomp_ts[1:(period*window), 1] # lag feature to model

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

# create testing data matrix
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)
N_boot <- 100 # number of bootstraps

pred_mat <- matrix(0, nrow = N_boot, ncol = period)
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period),
                                   floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)),
                                   replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- rpart(Load ~ ., data = matrixSam,
                    control = rpart.control(minsplit = sample(2:3, 1),
                                            maxdepth = sample(26:30, 1),
                                            cp = sample(seq(0.0000009, 0.00001, by = 0.0000001), 1)))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}
pred_melt_rpart <- data.table(melt(pred_mat))

pred_ave_rpart <- pred_melt_rpart[, .(value = median(value)), by = .(Var2)]
pred_ave_rpart[, Var1 := "RPART_Bagg"]

ggplot(pred_melt_rpart, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_rpart, aes(Var2, value), color = "firebrick2", alpha = 0.9, size = 2) +
  labs(x = "Date", y = "Number of People by Affected by COVID-19", title = "Bagging with RPART") +
  theme_ts
#CTREE
pred_mat <- matrix(0, nrow = N_boot, ncol = period)
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period),
                                   floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)),
                                   replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- party::ctree(Load ~ ., data = matrixSam,
                           controls = party::ctree_control(teststat = c("quad"),
                                                           testtype = c("Teststatistic"),
                                                           mincriterion = sample(seq(0.88, 0.97, by = 0.005), 1),
                                                           minsplit = 1,
                                                           minbucket = 1,
                                                           mtry = 0, maxdepth = 0))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}
pred_melt_ctree <- data.table(melt(pred_mat))

pred_ave_ctree <- pred_melt_ctree[, .(value = median(value)), by = .(Var2)]
pred_ave_ctree[, Var1 := "CTREE_Bagg"]

ggplot(pred_melt_ctree, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_ctree, aes(Var2, value), color = "firebrick2", alpha = 0.9, size = 2) +
  labs(x = "Date", y = "Number of People Affected By COVID-19", title = "Bagging with CTREE") +
  theme_ts

#Random Forest

rf_model <- randomForest(Load ~ ., data = data.frame(matrix_train),
                         ntree = 1000, mtry = 3, nodesize = 5, importance = TRUE)
varImpPlot(rf_model, main = "Variable importance")
pred_rf <- predict(rf_model, data.frame(matrix_test)) + mean(trend_for)

pred_rf <- data.table(value = pred_rf, Var2 = 1:48, Var1 = "RF")

pred_true <- data.table(value = data_test$Total, Var2 = 1:48, Var1 = "Real")

preds_all <- rbindlist(list(pred_ave_rpart, pred_ave_ctree, pred_rf, pred_true), use.names = T)

ggplot(preds_all, aes(Var2, value, color = as.factor(Var1))) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(x = "Date", y = "Number of People Affected by COVID-19", title = "Comparison of Ensemble Learning forecasts") +
  guides(color=guide_legend(title="Method")) +
  theme_ts

