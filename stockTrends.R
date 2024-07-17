
library(ggplot2)  

# Function to fetch stock data (
fetch_stock_data <- function(stock_name, start_date, end_date) {
  # This function could fetch data from an API or a local source
  # For simplicity, we'll generate random data within the date range
  
  dates <- seq(start_date, end_date, by = "day")
  closing_prices <- cumsum(rnorm(length(dates), mean = 0.5, sd = 2)) + 100
  
  stock_data <- data.frame(Date = dates, Closing_Price = closing_prices)
  return(stock_data)
}

#Linear Regression Function for prediction of stock prices
predict_stock_prices <- function(stock_data, future_dates) {
  model <- lm(Closing_Price ~ Date, data = stock_data)
  future_data <- data.frame(Date = future_dates)
  predicted <- predict(model, newdata = future_data)
  return(predicted)
}

# Graphical output
plot_stock_prices <- function(stock_data, predicted_data, future_dates, stock_name, start_date, end_date) {
  # Format start and end date for display
  formatted_start_date <- format(start_date, "%Y-%m-%d")
  formatted_end_date <- format(end_date, "%Y-%m-%d")
  
  ggplot(stock_data, aes(x = Date, y = Closing_Price)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("Stock Prices for", stock_name),
         subtitle = paste("From", formatted_start_date, "to", formatted_end_date),
         x = "Date", y = "Closing Price") +
    geom_point(data = data.frame(Date = future_dates, Closing_Price = predicted_data), 
               aes(x = Date, y = Closing_Price), color = "green", size = 3, shape = 23, fill = "green") +
    geom_line(data = data.frame(Date = future_dates, Closing_Price = predicted_data),
              aes(x = Date, y = Closing_Price), color = "green", linetype = "dashed") +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14, face = "italic"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
}

#Recieve name of stock in 
cat("Enter the stock symbol: ")
stock_name <- readline()

cat("Enter the start date to read from in format --> (YYYY-MM-DD): ")
start_date <- as.Date(readline())

cat("Enter the end date (this will go into the calculations for prediction) -->  (YYYY-MM-DD): ")
end_date <- as.Date(readline())

# Fetch stock data
stock_data <- fetch_stock_data(stock_name, start_date, end_date)

# Predict future stock prices (10 days after the typed time)
future_dates <- seq(end_date + 1, by = "day", length.out = 10)
predicted <- predict_stock_prices(stock_data, future_dates)

# Print the predicted values
cat("\nPredicted closing prices for the next 10 days after the chosen end date:\n")
print(predicted)

# Plotting
plot_stock_prices(stock_data, predicted, future_dates, stock_name, start_date, end_date)
