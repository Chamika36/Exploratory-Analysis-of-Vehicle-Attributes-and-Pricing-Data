vehicle<-`car.details.v4.(1)`

summary(vehicle$Make)
freqCol<-table(vehicle$Make)
barplot(freqCol,main = "Make", xlab = "Make", ylab = "Number of Cars")

sorted_freqCol <- sort(freqCol, decreasing = TRUE)
other_freq <- sum(sorted_freqCol[-(1:8)])
top_5_freq <- head(sorted_freqCol, 8)
modified_data <- c(top_5_freq, other_freq)
labels <- c(names(top_5_freq), "Other")
barplot(modified_data, main = "Car Makes", xlab = "Make", ylab = "Number of Cars", names.arg = labels)

summary(vehicle$Price)

price_bins <- cut(vehicle$Price, breaks = c(0, 500000, 1000000, 1500000, 2000000, Inf),
                  labels = c("0-500K", "500K-1M", "1M-1.5M", "1.5M-2M", "2M+"))
price_freq_table <- table(price_bins)
barplot(price_freq_table, main = "Car Prices", xlab = "Price", ylab = "Number of Cars")

summary(vehicle$Year)
table(vehicle$Year)
hist(vehicle$Year, main = "Manufactured Year", xlab = "Year", ylab = "Number of Cars")

summary(vehicle$Kilometer)

ggplot(vehicle, aes(x = Kilometer)) +
  geom_histogram(binwidth = 10000, fill = "orange", color = "black") +
  labs(title = "Kilometer Readings Distribution", x = "Kilometers Driven", y = "Frequency") +
  xlim(0,1000000) 
