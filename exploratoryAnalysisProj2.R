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
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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
library(sqldf)
library(maps)
library(RColorBrewer)
# library(reshape)
# library(SDMTools)
library(plotrix)

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

# The first 2 digits of a fips code is the state code
# http://en.wikipedia.org/wiki/FIPS_county_code
# Get the state of each fips code from the maps package
data(state.fips)
states <- state.fips[!duplicated(state.fips$fips), ]
combustNEI$state <- as.integer(substr(combustNEI$fips, 1, 2))
combustNEI <- sqldf("SELECT 
                     c.fips 
                     ,c.SCC 
                     ,c.Pollutant 
                     ,c.Emissions 
                     ,c.type 
                     ,c.year 
                     ,s.abb 
                     FROM combustNEI c 
                     LEFT JOIN states s 
                     ON c.state = s.fips")

# Fill in abbreviations for states that are not in the maps database
# Alaska, Hawaii, Puerto Rico, and the US Virgin Islands
# Then remove anything that still doesn't have a state.
combustNEI$abb[is.na(combustNEI$abb) & substr(combustNEI$fips, 1, 2) == "02"] <- "AK"
combustNEI$abb[is.na(combustNEI$abb) & substr(combustNEI$fips, 1, 2) == "15"] <- "HI"
combustNEI$abb[is.na(combustNEI$abb) & substr(combustNEI$fips, 1, 2) == "72"] <- "PR"
combustNEI$abb[is.na(combustNEI$abb) & substr(combustNEI$fips, 1, 2) == "78"] <- "VI"
combustNEI <- combustNEI[!is.na(combustNEI$abb), ]

# Get % change by state
totalCombustEmiss <- aggregate(combustNEI$Emissions, list(combustNEI$abb, combustNEI$year), FUN = sum)
colnames(totalCombustEmiss) <- c("state", "year", "emissions")
totalCombustChange <- melt(totalCombustEmiss, id = c("state"), na.rm = TRUE)
totalCombustChange <- cast(totalCombustEmiss, state ~ year)
totalCombustChange$change <- (totalCombustChange[, 5] - totalCombustChange[, 2]) / totalCombustChange[, 2]
totalCombustChange$change <- round(totalCombustChange$change, 4)
totalCombustChange$change[is.na(totalCombustChange$change)] <- 0

# Create heatmap
bucketVals <- rev(seq(from = -1.2, to = 1.6, by = .01))
totalCombustChange$colorBuckets <- as.numeric(cut(totalCombustChange$change, bucketVals))
colorsMatched <- totalCombustChange$colorBuckets[match(state.fips$abb, totalCombustChange$state)]
colors <- rev(heat.colors(length(bucketVals)))
map("state", col = colors[colorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
title("Percent Change in Coal Combustion by State\n 1999 - 2008")
legCoords <- par("usr")
color.legend(xl = legCoords[1] * .85, 
             yb = legCoords[2] * 1.03, 
             xr = legCoords[3] * 1.15, 
             yt = legCoords[4] * .38, 
             legend = c("-100%", "160%"), 
             rect.col = colors, 
             cex = 1, 
             align = "lt", 
             gradient = "x")

totalCombustChange[order(totalCombustChange$change), ]

# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# Subset NEI data to include baltimore only
baltimoreNEI <- NEI[NEI$fips == "24510", ]

# Subset SCC data to include motor vehicle sources only
# http://www.epa.gov/ttn/chief/net/2011nei/2011_nei_tsdv1_draft2_june2014.pdf
motorSCC <- SCC[grepl("[Mm]otor", SCC$Short.Name) | 
                grepl("[Mm]otor", SCC$EI.Sector) | 
                grepl("[Mm]otor", SCC$SCC.Level.One) | 
                grepl("[Mm]otor", SCC$SCC.Level.Two) | 
                grepl("[Mm]otor", SCC$SCC.Level.Three), ]
motorSCC <- motorSCC[grepl("^(?!Point).*$", as.character(motorSCC$Data.Category), perl = TRUE), ]
