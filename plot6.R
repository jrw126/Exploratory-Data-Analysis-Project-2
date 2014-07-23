# Exploratory Data Analysis
# Project 2
# Plot 6
# This code assumes that all raw data files are in your working directory

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
# vehicle sources in Los Angeles County, California (fips == "06037"). Which city has 
# seen greater changes over time in motor vehicle emissions?
library(sqldf)
library(ggplot2)

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

# Process the NEI data
LABaltNEI <- NEI[NEI$fips == "24510" | NEI$fips == "06037", ]
LABaltNEI <- sqldf("SELECT
                   b.fips
                   ,b.SCC
                   ,b.Pollutant
                   ,b.Emissions
                   ,b.type
                   ,b.year
                   FROM LABaltNEI b
                   JOIN motorSCC s ON b.SCC = s.SCC")
LABaltNEI <- rbind(NEI[NEI$fips == "24510" | NEI$fips == "06037" & NEI$type == "ON-ROAD", ], LABaltNEI)
LABaltNEI <- LABaltNEI[!duplicated(LABaltNEI), ]
LABaltNEI <- aggregate(Emissions ~ year + fips, data = LABaltNEI, FUN = sum)
pairs <- c(1999, 2002, 2005, 2008)
change <- round(data.frame(sapply(1:3, FUN = function(x) 
                          (LABaltNEI$Emissions[LABaltNEI$year == pairs[x + 1]] - LABaltNEI$Emissions[LABaltNEI$year == pairs[x]]) / 
                           LABaltNEI$Emissions[LABaltNEI$year == pairs[x]])) * 100, 1)
change <- rbind(t(change[1, ]),t(change[2, ]))
change <- as.data.frame(cbind(rep(c("1999 to 2002", "2002 to 2005", "2005 to 2008"), 2), c(rep("Los Angeles", 3), rep("Baltimore", 3)), change))
colnames(change) <- c("Period", "City", "Change")
change$Change <- as.numeric(as.character(change$Change))

# Create the plot
dev.off()
png(filename = "plot6.png", width = 480, height = 480, units = "px")
qplot(x = Period, 
      y = Change, 
      fill = City, 
      data = change, 
      geom = "bar", 
      stat = "identity", 
      position = "dodge",
      main = "Motor Vehicle Emissions\n Baltimore vs Los Angeles",
      ylab = expression("1 Year % Change in Total PM"[2.5])) +
      theme(plot.title = element_text(face = "bold", size = "16"))

# Save the plot to the working directory.
dev.off()