############################################
# Figures for Report
# Author: Rachel Weber
# Date: December 5, 2017
############################################

# this file contains all of the figures I need for the Mozambique malaria report

temp6 <- read.table(file='rainlag.txt', sep="")
library(ggplot2)
##########################################################################
# Exploratory Graph 
p <- ggplot(temp7, aes(x = Epiweek))
p <- p + geom_smooth(aes(y=cases, color = "Malaria Cases"))
p <- p + geom_smooth(aes(y=raintot*20, color = "Rainfall"))
p <- p + scale_y_continuous(sec.axis = sec_axis(~./20, name = "Total Rainfall"))
p <- p + labs(y = "Cases", x = "Epidemiological Week", colour = "Legend")
p <- p + theme_minimal() + ggtitle("Seasonal Trends") + scale_x_continuous(breaks = seq(1,52,8))
p <- p + scale_color_manual(values = c("darkorange", "blue3"))
p

# cool but not useful
library(tidyr)
temp6 %>% tidyr::gather("id", "value", 20:22) %>%
  ggplot(., aes(value, cases, color = id)) +
  #geom_smooth(aes(y=raintot*20, color = "Rainfall")) +
  #scale_y_continuous(sec.axis = sec_axis(~./20, name = "Total Rainfall")) +
  geom_smooth() +
  facet_wrap(~id)
#########################################################################
# graphs of all lag variables

# rain lag
lag_graph <- ggplot(temp7, aes(x=rain2w, y=cases, color = '2 weeks')) +
              geom_smooth(se=F, size=1.5) +
              geom_smooth(aes(x=rain4w, y=cases, color = '4 weeks'), se=F, size=1.5) +
              geom_smooth(aes(x=rain8w, y=cases, color = '8 weeks'), se=F, size=1.5) +
              labs(x="Rainfall (mm)", y = "Cases", title = "Rain Lag Predicting Reported Cases") +
              scale_color_manual(name = "Lag", values = c('blue3', 'darkorange', 'darkorchid1'))

# interventions predicting incidence
int_graph <- ggplot(temp7, aes(x = ITNprot, y = cases, color= 'ITN')) +
              geom_smooth( size=1.25) +
              geom_smooth(aes(x = IRSprot, y = cases, color = 'IRS'), size=1.25) +
              ggtitle("Intervention Predicting Reported Cases") + xlab("Coverage (0-100%)") +
              scale_color_manual(name="Intervention", values = c("blue3", "darkorange"))

# humidity lag graph
humid_lag <- ggplot(temp9, aes(x=rh2w, y=cases, color = '2 weeks')) +
  geom_smooth(se=F, size=1.5) +
  geom_smooth(aes(x=rh4w, y=cases, color = '4 weeks'), se=F, size=1.5) +
  geom_smooth(aes(x=rh8w, y=cases, color = '8 weeks'), se=F, size=1.5) +
  labs(x="Humidity (%)", y = "Cases", title = "Humidity Lag Predicting Reported Cases") +
  scale_color_manual(name = "Lag", values = c('blue3', 'darkorange', 'darkorchid1'))

# temp lag graph
temp_lag <- ggplot(temp9, aes(x=tavg2w, y=cases, color = '2 weeks')) +
  geom_smooth(se=F, size=1.5) +
  geom_smooth(aes(x=tavg4w, y=cases, color = '4 weeks'), se=F, size=1.5) +
  geom_smooth(aes(x=tavg8w, y=cases, color = '8 weeks'), se=F, size=1.5) +
  labs(x="Temperature (Celsius)", y = "Cases", title = "Temperature Lag Predicting \n Reported Cases") +
  scale_color_manual(name = "Lag", values = c('blue3', 'darkorange', 'darkorchid1'))


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

install.packages('rgeos', type="source")
install.packages('rgdal', type="source")


# read in shape data
poly1 <- readShapePoly('C:/Users/rache/Documents/6640/Final Project/Moz_admin.shp', IDvar="DISTCODE")
#poly1 <- readShapePoly('C:/Users/weberra/Downloads/Moz_admin2.shp', IDvar="DISTCODE")
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
spplot(polydat2, "incidence", main = "Incidence", sub = "2016 week 2")

