# Exploratory Data Analysis
# Project 2
# Plot 2
# This code assumes that all raw data files are in your working directory

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 2
# Have total emissions from PM2.5 decreased in 
# the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

# Process the data
baltimoreNEI <- NEI[NEI$fips == "24510", ]
totalEmissionsBaltimore <- aggregate(Emissions ~ year, data = baltimoreNEI, FUN = sum)
totalEmissionsBaltimore$Emissions <- round(totalEmissionsBaltimore$Emissions / 1000, 2)

# Create the plot
dev.off()
png(filename = "plot2.png", width = 480, height = 480, units = "px")
barplot(height = totalEmissionsBaltimore$Emissions,
        names = c(totalEmissionsBaltimore$year),
        ylab = expression("PM"[2.5] * " emitted in thousands of tons"),
        xlab = "Year",
        main = expression("Total PM"[2.5] * " Emissions each Year for Baltimore City, Maryland"),
        ylim = c(0, round(max(totalEmissionsBaltimore$Emissions), 0) + 1))

# Save the plot to the working directory.
dev.off()
