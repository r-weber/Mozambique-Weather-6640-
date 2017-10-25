###############################
### District read-in from web
### Author: Rachel Weber
### Date: 10/24/17
###############################

# the following code reads in all 141 districts of Mozambique into R, assigns them an
# identifying column and binds them all together to make the file 'Districts'


# import and switch factor to character
library(XML)
districts <- readHTMLTable("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/", 
              skip.rows=1:3)[[1]]$Name
districts <- as.character(districts)

# remove NA in final row
districts <- districts[-142]


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
