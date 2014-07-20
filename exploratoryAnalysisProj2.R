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
library(maps)
library(reshape)

totalEmissionsBaltByType <- aggregate(baltimoreNEI$Emissions, list(baltimoreNEI$type, baltimoreNEI$year), FUN = sum)
colnames(totalEmissionsBaltByType) <- c("Type", "Year", "Total")
totalEmissionsBaltByType[, 1] <- as.factor(totalEmissionsBaltByType[, 1])
totalEmissionsBaltByType[, 2] <- as.factor(totalEmissionsBaltByType[, 2])
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

# Across the United States, how have emissions from
# coal combustion-related sources changed from 1999-2008?

# Subset NEI data to only include emissions from coal combustion data
combustSCC <- SCC[grepl("[Cc]ombust", SCC$Short.Name) | 
                  grepl("[Cc]ombust", SCC$EI.Sector) | 
                  grepl("[Cc]ombust", SCC$SCC.Level.One) | 
                  grepl("[Cc]ombust", SCC$SCC.Level.Two) | 
                  grepl("[Cc]ombust", SCC$SCC.Level.Three), ]
combustSCC <- combustSCC[grepl("[Cc]oal", combustSCC$Short.Name) | 
                         grepl("[Cc]oal", combustSCC$EI.Sector) | 
                         grepl("[Cc]oal", combustSCC$SCC.Level.One) | 
                         grepl("[Cc]oal", combustSCC$SCC.Level.Two) | 
                         grepl("[Cc]oal", combustSCC$SCC.Level.Three), ]

combustNEI <- NEI[NEI$SCC %in% unique(combustSCC$SCC), ]
combustNEI$fips <- as.integer(combustNEI$fips) # Converting fips to integers have revealed some NA values, such as "   NA" or "TR207".

# Find the total combustion emissions per county
totalCombustEmiss <- aggregate(combustNEI$Emissions, list(combustNEI$fips, combustNEI$year), FUN = sum)
colnames(totalCombustEmiss) <- c("fips", "year", "emissions")

# Get the percent change of total emissions between 1999 and 2008 for each fips.
totalCombustChange <- melt(totalCombustEmiss, id = c("fips"), na.rm = TRUE)
totalCombustChange <- cast(totalCombustEmiss, fips ~ year)
totalCombustChange$change <- (totalCombustChange[, 5] - totalCombustChange[, 2]) / totalCombustChange[, 2]
totalCombustChange$change <- round(totalCombustChange$change, 4)

totalCombustChange <- merge(totalCombustChange, county.fips, by = "fips")
totalCombustChange$change[is.na(totalCombustChange$change)] <- 0
totalCombustChange$colorBuckets <- as.numeric(cut(totalCombustChange$change, c(-2, 0, 2, 4, 6, 8, 10, 100, 1000, 2000)))
colorsMatched <- totalCombustChange$colorBuckets[match(county.fips$fips, totalCombustChange$fips)]
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
map("county", col = colors[colorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")


