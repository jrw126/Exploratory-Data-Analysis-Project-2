# Exploratory Data Analysis
# Project 2
# Plot 4
# This code assumes that all raw data files are in your working directory

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# QUESTION 4
# Across the United States, how have emissions from
# coal combustion-related sources changed from 1999-2008?
library(maps)
library(sqldf)
library(plotrix)

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
# The fips reference table can be found at the following link:
# http://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
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

# Set NA values to zero.
totalCombustChange$change[is.na(totalCombustChange$change)] <- 0

# Create the plot
dev.off()
png(filename = "plot4.png", width = 800, height = 800, units = "px")
bucketVals <- rev(seq(from = -1.2, to = 1.6, by = .01))
totalCombustChange$colorBuckets <- as.numeric(cut(totalCombustChange$change, bucketVals))
colorsMatched <- totalCombustChange$colorBuckets[match(state.fips$abb, totalCombustChange$state)]
colors <- rev(heat.colors(length(bucketVals)))
map("state", col = colors[colorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
title("Percent Change in Coal Combustion by State\n 1999 to 2008")
legCoords <- par("usr")
color.legend(xl = legCoords[1] * .85, 
             yb = legCoords[2] * 1.03, 
             xr = legCoords[3] * .97, 
             yt = legCoords[4] * .4, 
             legend = c("-100%", "+160%"), 
             rect.col = colors, 
             cex = 1, 
             font = 2,
             align = "lt", 
             gradient = "x")

# Save the plot to the working directory.
dev.off()
