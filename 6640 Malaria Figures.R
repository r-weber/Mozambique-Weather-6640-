############################################
# Figures for Report
# Author: Rachel Weber
# Date: November 29, 2017
############################################

# this file contains all of the figures I need for the Mozambique malaria report

temp6 <- read.table(file='rainlag.txt', sep="")
##########################################################################
# Exploratory Graphs  
p <- ggplot(temp6, aes(x = Epiweek))
p <- p + geom_smooth(aes(y=cases, color = "Malaria Cases"))
p <- p + geom_smooth(aes(y=raintot*20, color = "Rainfall"))
p <- p + scale_y_continuous(sec.axis = sec_axis(~./20, name = "Total Rainfall"))
p <- p + labs(y = "Cases", x = "Epidemiological Week", colour = "Legend")
p <- p + theme_minimal() + ggtitle("Seasonal Trends") + scale_x_continuous(breaks = seq(1,52,8))
p

h <- ggplot(temp6, aes(y = cases))+
  geom_smooth(aes(x=rain2w))+
  geom_smooth(aes(x=rain4w))+
  geom_smooth(aes(x=rain8w))+
  theme_minimal()
h

# # cool but not useful
# library(tidyr)
# temp6 %>% tidyr::gather("id", "value", 20:22) %>%
#   ggplot(., aes(value, cases, color = id)) + 
#   geom_smooth(aes(y=raintot*20, color = "Rainfall")) + 
#   scale_y_continuous(sec.axis = sec_axis(~./20, name = "Total Rainfall"))
# 
# geom_smooth() +
#   facet_wrap(~id)

year_2016 <- temp6[temp6$year == 2016,]
ggplot(year_2016, aes(Epiweek, cases)) +
  geom_smooth(aes(rain2w, cases), colour="red") +
  geom_smooth(aes(rain4w, cases), colour="purple") + 
  geom_smooth(aes(rain8w, cases), colour="goldenrod")

#############################################################
# maps
all <- read.table(file='rainlag.txt', sep="")
all2 <- all[!is.na(all$DISTCODE),]
#debug(utils:::unpackPkgZip)

### MAPPING PACKAGES ###
install.packages("rgdal")
library(RColorBrewer)
library(sp)
library(maptools) 
library(lattice)
library(latticeExtra) # For layer()
library(rgdal)

# read in shape data
poly1 <- readShapePoly('C:/Users/weberra/Downloads/Moz_admin2.shp', IDvar="DISTCODE")
plot(poly1)

all5 <- subset(all2, year==2016 & Epiweek==26)
all6 <- subset(all2, year==2016 & Epiweek==2)

# set rownames same as ID variable
rownames(all5) <- all5$DISTCODE
rownames(all6) <- all6$DISTCODE

poly2 <- poly1[poly1$DISTCODE %in% all5$DISTCODE,]
# sp package #
polydat <- SpatialPolygonsDataFrame(poly2, all5)
polydat2 <- SpatialPolygonsDataFrame(poly2, all6)

# plot total rainfall
spplot(polydat2, "rain8w", main = "Rainfall 8 Weeks Prior", sub = "2016 week 2")
spplot(polydat2, "rain4w", main = "Rainfall 4 Weeks Prior", sub = "2016 week 2")
spplot(polydat2, "rain2w", main = "Rainfall 2 Weeks Prior", sub = "2016 week 2")
spplot(polydat, "incidence", main = "Incidence", sub = "2016 week 26")
spplot(polydat2, "incidence", main = "Incidence", sub = "2016 week 2")
