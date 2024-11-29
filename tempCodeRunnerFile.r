# Load data
NEI <- readRDS("summarySCC_PM25.rds")

# Summarize total emissions by year
total_emissions <- aggregate(Emissions ~ year, data = NEI, sum)

# Create plot and save to PNG
png("plot1.png")
plot(total_emissions$year, total_emissions$Emissions,
     type = "b", pch = 19, col = "blue",
     xlab = "Year", ylab = "Total PM2.5 Emissions (tons)",
     main = "Total PM2.5 Emissions in the United States (1999–2008)")
dev.off()
# Filter data for Baltimore City
baltimore <- NEI[NEI$fips == "24510", ]

# Summarize total emissions by year
baltimore_emissions <- aggregate(Emissions ~ year, data = baltimore, sum)

# Create plot and save to PNG
png("plot2.png")
plot(baltimore_emissions$year, baltimore_emissions$Emissions,
     type = "b", pch = 19, col = "red",
     xlab = "Year", ylab = "Total PM2.5 Emissions (tons)",
     main = "PM2.5 Emissions in Baltimore City (1999–2008)")
dev.off()
library(ggplot2)

# Summarize emissions by year and type for Baltimore
baltimore_type <- aggregate(Emissions ~ year + type, data = baltimore, sum)

# Create ggplot and save to PNG
png("plot3.png")
ggplot(baltimore_type, aes(x = year, y = Emissions, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "PM2.5 Emissions by Source Type in Baltimore (1999–2008)",
       x = "Year", y = "Emissions (tons)") +
  theme_minimal()
dev.off()
# Filter SCC for coal combustion-related sources
coal_combustion <- SCC[grepl("Coal", SCC$EI.Sector, ignore.case = TRUE), ]
coal_data <- merge(NEI, coal_combustion, by = "SCC")

# Summarize emissions by year
coal_emissions <- aggregate(Emissions ~ year, data = coal_data, sum)

# Create plot and save to PNG
png("plot4.png")
plot(coal_emissions$year, coal_emissions$Emissions,
     type = "b", pch = 19, col = "darkgreen",
     xlab = "Year", ylab = "Total PM2.5 Emissions (tons)",
     main = "Coal Combustion PM2.5 Emissions (1999–2008)")
dev.off()
# Filter SCC for motor vehicle sources
motor_vehicles <- SCC[grepl("Vehicle", SCC$EI.Sector, ignore.case = TRUE), ]
vehicle_data <- merge(baltimore, motor_vehicles, by = "SCC")

# Summarize emissions by year
vehicle_emissions <- aggregate(Emissions ~ year, data = vehicle_data, sum)

# Create plot and save to PNG
png("plot5.png")
plot(vehicle_emissions$year, vehicle_emissions$Emissions,
     type = "b", pch = 19, col = "purple",
     xlab = "Year", ylab = "Total PM2.5 Emissions (tons)",
     main = "Motor Vehicle PM2.5 Emissions in Baltimore (1999–2008)")
dev.off()
# Filter data for Baltimore and Los Angeles
baltimore_la <- NEI[NEI$fips %in% c("24510", "06037"), ]
vehicle_data_la <- merge(baltimore_la, motor_vehicles, by = "SCC")

# Summarize emissions by year and location
vehicle_emissions_la <- aggregate(Emissions ~ year + fips, data = vehicle_data_la, sum)

# Replace fips with descriptive names
vehicle_emissions_la$fips <- factor(vehicle_emissions_la$fips,
                                    levels = c("24510", "06037"),
                                    labels = c("Baltimore", "Los Angeles"))

# Create ggplot and save to PNG
png("plot6.png")
ggplot(vehicle_emissions_la, aes(x = year, y = Emissions, color = fips)) +
  geom_line() +
  geom_point() +
  labs(title = "Motor Vehicle PM2.5 Emissions: Baltimore vs Los Angeles (1999–2008)",
       x = "Year", y = "Emissions (tons)", color = "City") +
  theme_minimal()
dev.off()
