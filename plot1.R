# Exploratory Data Analysis
# Project 2
# Plot 1
# This code assumes that all raw data files are in your working directory

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for 
# each of the years 1999, 2002, 2005, and 2008.

# Process the data
totalEmissionsByYear <- aggregate(Emissions ~ year, data = NEI, FUN = sum)
totalEmissionsByYear$Emissions <- round(totalEmissionsByYear$Emissions / 1000000, 2)

# Create the plot
dev.off()
png(filename = "plot1.png", width = 480, height = 480, units = "px")
barplot(height = totalEmissionsByYear$Emissions, 
        names = c(totalEmissionsByYear$year), 
        ylab = expression("PM"[2.5] * " emitted in millions of tons"),
        xlab = "Year",
        main = expression("Total PM"[2.5] * " Emissions each Year"),
        ylim = c(0, round(max(totalEmissionsByYear$Emissions), 0) + 1))

# Save the plot to the working directory.
dev.off()
