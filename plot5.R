# Exploratory Data Analysis
# Project 2
# Plot 5
# This code assumes that all raw data files are in your working directory

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
library(sqldf)
library(ggplot2)

# Subset NEI data to include baltimore only
baltimoreNEI <- NEI[NEI$fips == "24510", ]

# Subset SCC data to include motor vehicle sources only
# Using anything from the NEI data that is type == "ON-ROAD" or where motor vehicles
# are mentioned in the SCC description.
# http://www.epa.gov/ttn/chief/net/2011nei/2011_nei_tsdv1_draft2_june2014.pdf
motorSCC <- SCC[grepl("[Mm]otor", SCC$Short.Name) | 
                      grepl("[Mm]otor", SCC$EI.Sector) | 
                      grepl("[Mm]otor", SCC$SCC.Level.One) | 
                      grepl("[Mm]otor", SCC$SCC.Level.Two) | 
                      grepl("[Mm]otor", SCC$SCC.Level.Three), ]
motorSCC <- motorSCC[grepl("^(?!Point).*$", as.character(motorSCC$Data.Category), perl = TRUE), ]

# Get motor vehicle emissions data for baltimore
baltimoreNEI <- sqldf("SELECT
                      b.fips
                      ,b.SCC
                      ,b.Pollutant
                      ,b.Emissions
                      ,b.type
                      ,b.year
                      FROM baltimoreNEI b
                      JOIN motorSCC s ON b.SCC = s.SCC")
baltimoreNEI <- rbind(NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD", ], baltimoreNEI)
baltimoreNEI <- baltimoreNEI[!duplicated(baltimoreNEI), ]
motorEmissionsBaltimore <- round(aggregate(Emissions ~ year, data = baltimoreNEI, FUN = sum), 2)

# Create the plot
dev.off()
png(filename = "plot5.png", width = 480, height = 480, units = "px")
qplot(x = as.factor(year),
      y = Emissions,
      data = motorEmissionsBaltimore,
      geom = "bar",
      stat = "identity",
      xlab = "Year",
      ylab = expression("Total PM"[2.5]),
      main = "Total Emissions from Motor Vehicles\n Baltimore, Maryland") +
      theme(plot.title = element_text(face = "bold", size = "16"))

# Save the plot to the working directory.
dev.off()