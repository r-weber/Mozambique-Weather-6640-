#######################################
### File joining
### Author: Rachel Weber
### Date: 11/27/17
#######################################

# the following code chunk reads in all 141 districts of Mozambique, assigns them an
# identifying column, and binds them all together to make the file 'Districts'
#############################################################################################

library(XML)
library(epitools)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)

setwd("C:/Users/weberra/Downloads")
#setwd("C:/Users/rache/Documents/6640/Final Project")

# import and switch factor to character using library XML
districts <- readHTMLTable("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/", 
              skip.rows=1:2)[[1]]$Name
districts <- as.character(districts)

# remove NA in final row
districts <- districts[-143]
  #have no malaria cases


# loop attaches file path, read in each file, pulls the district name, adds 'District' as a new column
# and binds each successive district to the last to make one long dataframe
all_districts <- NULL
for(i in 1:length(districts)) {
  districts2<- paste("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/", districts[i],sep="")
  dis <- read.table(districts2, header=F, sep="", skip=3)
  name <- strsplit(districts[i], '_f') [[1]] [1]
  name_vector <- rep(name, times=length(dis$V1))
  dis$District <- NA
  dis$District <- name_vector
  if (i==1){
    all_districts <- dis
  } else {
    all_districts <- rbind(all_districts, dis)
  }
}

# Change names of variables
names(all_districts) <- c("year", "month", "day", "raint(mm/d)", "tavg(C)", "rh(%)", "vapor(mmHg)", "baro(hPa)", "District")

# write to txt file
write.table(all_districts, file="Districts.txt")

# read file back in in case you went off and had a life for a little while
dri <- read.table(file="Districts.txt")
dri$District<-as.character(dri$District)

# function that removes the last n values from a character string
substrRight <- function(x, n){
  substr(x, 1, nchar(x)-n)
}

# first remove last 4 positions in character string
dri$District <- substrRight(dri$District,4)

# change underscores to spaces so it will match the incidence data...and look nicer
dri$District <- gsub("_", " ", dri$District)
dri$District <- as.factor(dri$District)

# Make a date column
dri$date <- as.Date(with(dri, paste(year, month, day,sep="-")), "%Y-%m-%d")

# Create Epiweek using a character vector of dates (library: Epitools)
dates <- as.character(dri$date)
Epi <- as.week(dates, origin = as.Date("2009-01-01"), format = "%Y-%m-%d", sunday = TRUE)

# add epiweek to districts
dri$Epiweek <- Epi$week
dri$Epiweek <- as.numeric(dri$Epiweek)

######################################################################
# read in incidence
# reformat district column and epiweek to left_join with districts file
# averaging code
weatherepi <- dri %>%
  filter(year >= 2010) %>%
  group_by(District, year, Epiweek) %>%
  summarise(tavg = mean(tavg.C.), raintot = sum(raint.mm.d.), rh = mean(rh...), sd = mean(vapor.mmHg.), psfc = mean(baro.hPa.))

incidence <- read.csv("incidence.csv", sep=",")
incidence$Epiweek <- as.numeric(incidence$Epiweek)

# left join incidence to districts by epiweek and district name (library dplyr)
dri_inc <- left_join(weatherepi, incidence, by=c('year'='Epiyear', 'Epiweek', 'District'= 'District'))

# write to file, dont want to go ruining this pretty dataframe we made
write.table(dri_inc, file="districts_incidences.txt")

#######################################################################
# now read in intervention
intervention <- read.csv("intervention.csv", sep=",")
dri_inc <- read.table(file="districts_incidences.txt", sep="")

temp2 <- merge(dri_inc, 
               intervention %>% # here we only select the IRS year first 
                 select(DISTCODE, IRSyear, IRSepiWeek) %>% # select only IRS 
                 filter(!is.na(IRSyear) & IRSyear > 2009) %>% #Select for Epiyear > 2009
                 rename(year =  IRSyear, Epiweek = IRSepiWeek) %>% # Rename to match
                 mutate(IRSprot = ifelse(!is.na(year), 1, NA)), # Create protection variable 
               by = c("DISTCODE","year","Epiweek"), all = TRUE)
temp3 <- merge(temp2,
               intervention %>% # here we only select the IRS year first 
                 select(DISTCODE, ITNyear, ITNepiWeek) %>% # select only IRS 
                 filter(!is.na(ITNyear) & ITNyear > 2009) %>% #Select for Epiyear > 2009
                 rename(year =  ITNyear, Epiweek = ITNepiWeek) %>% # Rename to match
                 mutate(ITNprot = ifelse(!is.na(year), 1, NA)), # Create protection variable 
               by = c("DISTCODE","year","Epiweek"), all = TRUE)


write.table(temp3, file="total_combo.txt")


#####################################################################################
# Create incidence Column
temp3 <- read.table(file="total_combo.txt", sep="")
temp3$incidence <- temp3$cases/temp3$u5total

rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

library(data.table)

#########################################################################################
# Create Lag in Rainfall

#debug(utils:::unpackPkgZip) should only need this piece if antivirus software won't allow install
#install.packages("Hmisc")

library(Hmisc)
temp6 <- temp3 %>%
  group_by(DISTCODE) %>%
  mutate(rain2w = Lag(raintot, +2),
         rain4w = Lag(raintot, +4),
         rain8w = Lag(raintot, +8))

write.table(temp6, file="rainlag.txt")
temp6 <- read.table(file='rainlag.txt', sep="")

###########################################################################
# Modeling

library(lme4)
m3 <- glmer(cases ~ rain2w + rain4w + rain8w + (1|District), offset=log(u5total),
            data=temp6, family = "poisson")
  # model doesn't converge...?

# are the variables correlated?
library(infotheo)
cor(temp6[,20:22])
rain_cor <- rcorr(as.matrix(temp6[,20:22])) # need Hmisc package for rcorr
rain_cor <- discretize(rain_cor)
mutinformation(rain_cor)
round(rain_cor$r, 3)
  # since there is no correlation, all lag vars belong in the model

library(car) ## For Anova
summary(m3)
Anova(m3)

pred_2 <- predict(week2, type="response")

#library(effects)
plot(allEffects(m3))
ggplot(temp6, aes(x=rain2w, y=incidence))+
  stat_smooth(method = 'glm', family = 'poisson', 
              formula = incidence ~ rain2w + (1|DISTCODE), data = temp6) +
  geom_smooth(col='red', se=F) + geom_point()

plot(temp6$rain2w, temp6$cases)
lines(pred_2, col= 'blue')

#debug(utils:::unpackPkgZip)
#install.packages('multcomp')
library(multcomp)
tmp <- as.data.frame(confint(glht(week2))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()
