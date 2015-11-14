max(transaction_data$DAY)

transaction_data$Date_Custom <- as.Date(transaction_data$DAY -1, origin = "2010-01-20")
write.csv(transaction_data, file = "transaction_custom.csv")
