# Install Packages 
install.packages("forecast")
install.packages("data.table")
install.packages("randomForest")
install.packages("rpart")
install.packages("party")
install.packages("ggplot2")
install.packages("Metrics")

# Load libraries
# library(forecast)
# library(data.table)
# library(randomForest)
# library(rpart)
# library(party)
# library(ggplot2)
# library(Metrics) # for error metrics



# Load Data
data <- as.data.table(read.csv(file.choose())) # Modify as needed
n_date <- unique(data[, Date]) # Assuming 'Date' column; modify as needed
period <- 12

# Split data into train/test
data_train <- data[Date %in% n_date[1:floor(0.8 * length(n_date))]]
data_test <- data[Date %in% n_date[(floor(0.8 * length(n_date)) + 1):length(n_date)]]

# Ensemble Forecast Components
# 1. ARIMA for trend component
data_ts <- ts(data_train$Total, frequency = period) # Modify 'Total' column as necessary
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series
trend_part <- ts(decomp_ts[, "trend"])
arima_model <- auto.arima(trend_part)
arima_forecast <- forecast(arima_model, h = nrow(data_test))$mean

# 2. Random Forest with lagged and seasonal features
data_msts <- msts(data_train$Total, seasonal.periods = c(period, period * 7))
fuur <- fourier(data_msts, K = c(4, 4))
matrix_train <- data.table(Load = tail(rowSums(decomp_ts[, c("remainder", "seasonal")]), -period), fuur)
matrix_test <- fourier(data_msts, K = c(4, 4), h = nrow(data_test))

rf_model <- randomForest(Load ~ ., data = matrix_train, ntree = 500, mtry = 3, nodesize = 5, importance = TRUE)
rf_forecast <- predict(rf_model, matrix_test) + mean(arima_forecast)

# 3. Bagging with RPART for residuals
N_boot <- 100
pred_mat <- matrix(0, nrow = N_boot, ncol = nrow(data_test))
for (i in 1:N_boot) {
  sample_data <- matrix_train[sample(1:nrow(matrix_train), size = floor(0.8 * nrow(matrix_train)), replace = TRUE)]
  tree_bag <- rpart(Load ~ ., data = sample_data, control = rpart.control(cp = 0.0005))
  pred_mat[i, ] <- predict(tree_bag, matrix_test) + mean(arima_forecast)
}
rpart_forecast <- rowMeans(pred_mat)

# Ensemble: Weighted Averaging of Forecasts
ensemble_forecast <- 0.5 * arima_forecast + 0.3 * rf_forecast + 0.2 * rpart_forecast

# Evaluation Metrics
mae_val <- mae(data_test$Total, ensemble_forecast)
rmse_val <- rmse(data_test$Total, ensemble_forecast)
mape_val <- mape(data_test$Total, ensemble_forecast)

# Print Error Metrics
cat("MAE:", mae_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("MAPE:", mape_val, "\n")

# Visualization
pred_all <- data.table(
  Date = n_date[(floor(0.8 * length(n_date)) + 1):length(n_date)],
  Real = data_test$Total,
  Ensemble = ensemble_forecast,
  ARIMA = arima_forecast,
  RandomForest = rf_forecast,
  RPART = rpart_forecast
)

ggplot(pred_all, aes(x = Date)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Ensemble, color = "Ensemble")) +
  geom_line(aes(y = ARIMA, color = "ARIMA")) +
  geom_line(aes(y = RandomForest, color = "RandomForest")) +
  geom_line(aes(y = RPART, color = "RPART")) +
  labs(title = "Ensemble Forecast Comparison", y = "Cases", x = "Date") +
  scale_color_manual(values = c("Real" = "black", "Ensemble" = "blue", "ARIMA" = "green", "RandomForest" = "red", "RPART" = "purple")) +
  theme_minimal()
