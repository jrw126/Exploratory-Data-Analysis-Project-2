# Exploratory Data Analysis
# Project 2
# Plot 3
# This code assumes that all raw data files are in your working directory

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)

# Process the data
baltimoreNEI <- NEI[NEI$fips == "24510", ]
totalEmissionsBaltByType <- aggregate(baltimoreNEI$Emissions, list(baltimoreNEI$type, baltimoreNEI$year), FUN = sum)
colnames(totalEmissionsBaltByType) <- c("Type", "Year", "Total")
totalEmissionsBaltByType[, 1] <- as.factor(totalEmissionsBaltByType[, 1])
totalEmissionsBaltByType[, 2] <- as.factor(totalEmissionsBaltByType[, 2])

# Create the plot
dev.off()
png(filename = "plot3.png", width = 480, height = 480, units = "px")
qplot(x = Year, 
      y = Total, 
      fill = Type, 
      data = totalEmissionsBaltByType, 
      geom = "bar", 
      stat = "identity", 
      position = "dodge",
      main = "Emissions per Year by Type\n Baltimore City, Maryland",
      ylab = expression("Total PM"[2.5])) +
      theme(plot.title = element_text(face = "bold", size = "16"))

# Save the plot to the working directory.
dev.off()