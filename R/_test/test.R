raw_transaction = read.table("./Dataset/transaction_data.csv", sep=",", header=T ,na.strings = " ")
raw_transaction$Date <- as.Date(raw_transaction$DAY -1, origin = "2010-03-24")

write.csv(raw_transaction, file = "transaction_wDate.csv",row.names=FALSE)
