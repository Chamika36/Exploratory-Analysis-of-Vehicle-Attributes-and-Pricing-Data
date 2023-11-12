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
  geom_histogram(binwidth = 10000) +
  labs(title = "Kilometer Readings Distribution", x = "Kilometers Driven", y = "Frequency") +
  xlim(0,1000000) 

# Create a box plot for price distribution by car model for the top 5 makes
boxplot(Price ~ Make, data = vehicle_filtered, 
        main = "Price Distribution by Car Model (Top 5 Makes)",
        xlab = "Car Model", ylab = "Price",
        col = "green")

ggplot(vehicle_filtered, aes(x = Make, y = Price)) +
  geom_boxplot(fill = "green") +
  ylim(0, 2500000) +  
  labs(title = "Price Distribution by Car Make", x = "Car Make", y = "Price")

library(ggplot2)

# Filter the 'vehicle' dataset for years from 2015 to 2022
filtered_vehicle <- subset(vehicle, Year >= 2015 & Year <= 2022)

ggplot(filtered_vehicle, aes(x = as.factor(Year), y = Price)) +
  geom_boxplot(fill = "green") +
  ylim(0, 2500000) +  
  labs(title = "Price Distribution by Car Year (2015-2022)", x = "Car Year", y = "Price")

library(ggplot2)

ggplot(vehicle, aes(x = Kilometer, y = Price)) +
  geom_point(color = "blue") +
  xlim(0, 250000) +
  labs(title = "Kilometers Driven vs. Price", x = "Kilometers Driven", y = "Price")

fuel_table <- table(vehicle$Fuel.Type)

# Create a pie chart
pie(fuel_table, main = "Fuel Type Distribution", col = rainbow(length(fuel_table)))
filtered_vehicle <- subset(vehicle, !(Fuel.Type %in% c("CNG + CNG", "Petrol + LPG", "Petrol + CNG")))
fuel_table <- table(filtered_vehicle$Fuel.Type)
pie(fuel_table, main = "Fuel Type Distribution", col = rainbow(length(fuel_table)))
legend("topright", names(fuel_table), fill = rainbow(length(fuel_table)), cex = 0.8)



transmission_table <- table(vehicle$Transmission)
pie(transmission_table, main = "Transmission Type Distribution", col = rainbow(length(transmission_table)))
legend("topright", names(transmission_table), fill = rainbow(length(transmission_table)), cex = 0.8)

location_table <- table(vehicle$Location)
location_table

color_table <-table(vehicle$Color)
color_table

# Assuming your dataset is named 'vehicle'
# Assuming 'Color' is a categorical variable in your dataset

# Create a table of frequencies for each color
color_table <- table(vehicle$Color)

# Order the colors by frequency
sorted_colors <- names(sort(color_table, decreasing = TRUE))

# Subset the data for the top 10 colors
top_colors <- sorted_colors[1:10]
filtered_vehicle <- subset(vehicle, Color %in% top_colors)

custom_colors <- c("Black", "Blue", "Bronce", "Brown", "Gold", "Gray", "Maroon", "Red", "Silver", "White")
custom_colors <- c("#000000", "#0000FF", "#D2691E", "#A52A2A", "#FFD700", "#808080", "#800000", "#FF0000", "#C0C0C0", "#FFFFFF")
# Create a bar chart
barplot(table(filtered_vehicle$Color), main = "Car Colors", xlab = "Colors", ylab = "Number of Cars", col = custom_colors, las = 2, cex.names = 0.7)

# Assuming your dataset is named 'vehicle'
# Assuming 'Owner.Type' is a categorical variable in your dataset

# Create a table of frequencies for each owner type
owner_table <- table(vehicle$Owner)
pie(owner_table, main = "Owner Type Distribution", col = rainbow(length(owner_table)))
legend("topright", names(owner_table), fill = rainbow(length(owner_table)), cex = 0.6)

seller_table <- table(vehicle$Seller.Type)
pie(seller_table, main = "Seller Type Distribution", col = rainbow(length(seller_table)))
legend("topright", names(seller_table), fill = rainbow(length(seller_table)), cex = 0.65)

ggplot(vehicle, aes(x = Drivetrain, y = Fuel.Tank.Capacity)) +
  geom_point(aes(color = Drivetrain), size = 3) +
  labs(title = "Drive Train vs Fuel Capacity", x = "Drive Train", y = "Fuel Capacity") +
  theme_minimal()

ggplot(vehicle, aes(x = Drivetrain, y = Fuel.Tank.Capacity, fill = Drivetrain)) +
  geom_boxplot() +
  labs(title = "Drive Train vs Fuel Capacity", x = "Drive Train", y = "Fuel Capacity") +
  theme_minimal()

vehicle2 <- na.omit(vehicle)

# Load ggplot2 library
library(ggplot2)

# Create a boxplot after removing missing values
ggplot(vehicle2, aes(x = Drivetrain, y = Fuel.Tank.Capacity, fill = Drivetrain)) +
  geom_boxplot() +
  labs(title = "Drive Train vs Fuel Capacity", x = "Drive Train", y = "Fuel Capacity") +
  theme_minimal()

ggplot(vehicle, aes(x = Seating.Capacity, y = Fuel.Tank.Capacity)) +
  geom_point(aes(color = factor(Seating.Capacity)), size = 3) +
  labs(title = "Seating Capacity vs Fuel Capacity", x = "Seating Capacity", y = "Fuel Capacity") +
  theme_minimal()

ggplot(vehicle, aes(x = Engine, y = Fuel.Tank.Capacity)) +
  geom_point(size = 1) +
  labs(title = "Engine vs Fuel Capacity", x = "Engine Torque", y = "Fuel Capacity") +
  theme_minimal()
library(dplyr)


vehicle2 <- mutate(vehicle, NumericEngineSize = as.numeric(gsub("[^0-9.]", "", Engine)))

# Create a scatter plot
ggplot(vehicle2, aes(x = NumericEngineSize, y = Fuel.Tank.Capacity)) +
  geom_point(size = 2) +
  labs(title = "Engine Size vs Fuel Capacity", x = "Engine Size (cc)", y = "Fuel Capacity") +
  theme_minimal()

ggplot(vehicle2, aes(x = NumericEngineSize)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Engine Sizes", x = "Engine Size (cc)", y = "Frequency") +
  theme_minimal()

summary(vehicle2$NumericEngineSize)

ggplot(vehicle, aes(x = Max.Power)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Max Power", x = "Max Power", y = "Frequency") +
  theme_minimal()

ggplot(vehicle, aes(x = Fuel.Type, y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Fuel Type vs Price", x = "Fuel Type", y = "Price") +
  ylim(0, 2500000) +
  theme_minimal()


selected_fuels <- c("Petrol", "Diesel", "CNG", "Electric", "LPG")
filtered_data <- subset(vehicle, Fuel.Type %in% selected_fuels)

# Create a boxplot
ggplot(filtered_data, aes(x = Fuel.Type, y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Fuel Type vs Price", x = "Fuel Type", y = "Price") +
  ylim(0, 2500000) +
  theme_minimal()

ggplot(vehicle, aes(x = Transmission, y = Price)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Transmission vs Price", x = "Transmission", y = "Price") +
  ylim(0, 2500000) +
  theme_minimal()

ggplot(vehicle, aes(x = Color, y = Price)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Color vs Price", x = "Color", y = "Price") +
  theme_minimal() +
  ylim(0, 2500000) 

ggplot(vehicle, aes(x = Owner, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Owner Type vs Price", x = "Owner Type", y = "Price") +
  theme_minimal() +
  ylim(0, 2500000) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

custom_order <- c("UnRegistered Car", "First", "Second","Fourth", "Third", "4 or More")

# Convert 'Owner.Type' to a factor with the custom order
vehicle$Owner.Type <- factor(vehicle$Owner, levels = custom_order)

# Create a boxplot
ggplot(vehicle, aes(x = Owner.Type, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Owner Type vs Price", x = "Owner Type", y = "Price") +
  ylim(0, 2500000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(vehicle, aes(x = Seller.Type, y = Price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Seller Type vs Price", x = "Seller Type", y = "Price") +
  ylim(0, 2500000) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(vehicle2, aes(x = NumericEngineSize, y = Price)) +
  geom_point(color = "brown") +
  labs(title = "Engine vs Price", x = "Engine", y = "Price") +
  ylim(0, 4000000) +
  theme_minimal()
