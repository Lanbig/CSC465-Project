library(ggplot2)

raw_transaction = read.table("~/Dropbox/CSC465/CSC465-Project/Dataset/transaction_data.csv", sep=",", header=T ,na.strings = " ")
raw_hh = read.table("~/Dropbox/CSC465/CSC465-Project/Dataset/hh_demographic.csv", sep=",", header=T ,na.strings = " ")

raw_hh_trans <- merge(raw_transaction,raw_hh,by="household_key", all.x=TRUE)

raw_hh_trans <- raw_hh_trans[complete.cases(raw_hh_trans),]

p2f <- ggplot(raw_hh_trans, aes(x=as.factor(raw_hh_trans$AGE_DESC),y=log2(raw_hh_trans$SALES_VALUE), group=as.factor(AGE_DESC), fill=AGE_DESC, stat=" summary", fun.y = "mean"))
p2f <- p2f + geom_violin(outlier.shape = 3)
#p2f <- p2f + facet_grid(. ~ AGE_DESC,scales="free")
p2f 


######################
raw_coupon_redempt = read.table("~/Dropbox/CSC465/CSC465-Project/Dataset/coupon_redempt.csv", sep=",", header=T ,na.strings = " ")
raw_hh_coupon <- merge(raw_coupon_redempt,raw_hh,by="household_key", all.x=TRUE)

raw_hh_coupon <- cbind(raw_hh_coupon,1)
colnames(raw_hh_coupon)[12] <- "nCoupon"

library(plyr)
hh_coupon_agg <- ddply(raw_hh_coupon,.(household_key),numcolwise(sum))

hh_coupon_agg <- merge(hh_coupon_agg,raw_hh,by="household_key", all.x=TRUE)

#remove NA
hh_coupon_agg <- hh_coupon_agg[complete.cases(hh_coupon_agg),]

p2f <- ggplot(hh_coupon_agg, aes(x=as.factor(hh_coupon_agg$AGE_DESC),y=hh_coupon_agg$nCoupon, group=as.factor(AGE_DESC), fill=AGE_DESC, stat=" summary", fun.y = "mean"))
p2f <- p2f + geom_violin(outlier.shape = 3) + geom_boxplot(width=.1)
p2f <- p2f + ggtitle("Number of Coupon Redemptions by the Age of Customers")
p2f <- p2f + xlab("Age of Customers") + ylab("Number of Coupon Redemptions")
p2f

dev.copy(device = png, filename = 'Violin-CouponByAge.png', width = 800, height = 600) 
dev.off()
