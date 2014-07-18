# Coursera Exploratory Data Analysis Project 2
# Data source: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#
## NEI DATASET ############################################################################
# This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
# For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for 
# the entire year.
### VARIABLES #############################################################################
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded
###########################################################################################
#
## SCC DATASET ############################################################################
# This table provides a mapping from the SCC digit strings int he Emissions table to the actual 
# name of the PM2.5 source. The sources are categorized in a few different ways from more general 
# to more specific and you may choose to explore whatever categories you think are most useful. 
# For example, source "10100101" is known as "Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal".
###########################################################################################
#
# Load data
dir <- getwd()
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Check for missing values
nrow(NEI) - length(complete.cases(NEI)) # Number of incomplete records in the NEI dataset.
nrow(SCC) - length(complete.cases(SCC)) # Number of incomplete records in the SCC dataset.

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for 
# each of the years 1999, 2002, 2005, and 2008.

totalEmissionsByYear <- aggregate(Emissions ~ year, data = NEI, FUN = sum)
totalEmissionsByYear$Emissions <- round(totalEmissionsByYear$Emissions / 1000000, 2)

barplot(height = totalEmissionsByYear$Emissions, 
        names = c(totalEmissionsByYear$year), 
        ylab = expression("PM"[2.5] * " emitted in millions of tons"),
        xlab = "Year",
        main = expression("Total PM"[2.5] * " Emissions each Year"),
        ylim = c(0, round(max(totalEmissionsByYear$Emissions), 0) + 1))

# Have total emissions from PM2.5 decreased in 
# the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

baltimoreNEI <- NEI[NEI$fips == "24510", ]
totalEmissionsBaltimore <- aggregate(Emissions ~ year, data = baltimoreNEI, FUN = sum)
totalEmissionsBaltimore$Emissions <- round(totalEmissionsBaltimore$Emissions / 1000, 2)
barplot(height = totalEmissionsBaltimore$Emissions,
        names = c(totalEmissionsBaltimore$year),
        ylab = expression("PM"[2.5] * " emitted in thousands of tons"),
        xlab = "Year",
        main = expression("Total PM"[2.5] * " Emissions each Year for Baltimore City, Maryland"),
        ylim = c(0, round(max(totalEmissionsBaltimore$Emissions), 0) + 1))

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
