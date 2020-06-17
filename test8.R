#######################################
#test 8
# by Taro Ubukawa taro.ubukawa@un.org
# 16 June 2020
#######################################
#Please store the source data in src folder.
#Please prepare a folder for graph.
######################################

#install.packages("dplyr", dependencies = TRUE)
library(dplyr)

#Parameters
loc <- "D:/09_R/"
indicators <- c("1.1.1", "2.1.1","3.2.1","4.2.2","5.5.1","6.1.1","7.1.1","8.1.1","9.5.1","10.4.1","11.1.1","12.2.2","13.1.1","14.a.1","15.1.1","16.1.1")
y_maxs <- c(100,100,100,100,1000,100,100,125,10,100,100,10,100,10,100,100)
y_mins <- c(0,0,0,0,0,0,0,-65,0,0,0,0,0,0,0,0)

joinkey <-"GeoAreaCode"
graphloc <-"graph_test8"
tableloc <-"table_test8"
summarytable <-paste(tableloc,"/","summarytable_test8.txt",sep="")

#choose one
sex_3_2_1 <- "BOTHSEX"
sex_4_2_2 <- "BOTHSEX"
area_6_1_1 <- "ALLAREA" #Location
area_7_1_1 <- "ALLAREA" #Location
TOP_12_2_2 <- "RAW"
sex_16_1_1 <- "BOTHSEX"

#source files
srcfile <- "src/data.csv"
srcisocode <- "src/iso3cd.csv" #table for join




#settings
latestvaluetable<-paste(loc,summarytable,sep="")
srcdatapath <- paste(loc,srcfile,sep="")
isocodepath <- paste(loc,srcisocode,sep="")
all.data <- read.csv(file = srcdatapath, header = TRUE) #raw
iso.data <- read.csv(file = isocodepath, header = TRUE) #table for join
all2.data <- all.data[,c(3,4,6,7,8,9,10,14,15,16,17,18,19,20,21,22,23,24)] 
all3.data <- left_join(all2.data,iso.data,by=joinkey) #need to change if necessary
codelist <- iso.data$GeoAreaCode #area code list

##loop for each 16 indicators
k <- 1
while (k <= length(indicators)){
type <- indicators[k]


test.data <- filter(all3.data,all3.data$Indicator == type)

##filter to delete non value
test.data <- filter(test.data,test.data$Value != "N")
test.data <- filter(test.data,test.data$Value != "NA") #NA deleted with "N"??
test.data <- filter(test.data,test.data$Value != "Percentage")#Percentage (8.1.1 Sudan)
##

###Specific filter for each indicator.
if(type == "3.2.1"){
  test.data <- filter(test.data,test.data$Sex == sex_3_2_1)
}else{
  taro<-1 #no measing
}
if(type == "4.2.2"){
  test.data <- filter(test.data,test.data$Sex == sex_4_2_2)
}else{
  taro<-1 #no meaning
}
if(type == "6.1.1"){
  test.data <- filter(test.data,test.data$Location == area_6_1_1)
}else{
  taro<-1 #no meaning
}
if(type == "7.1.1"){
  test.data <- filter(test.data,test.data$Location == area_7_1_1)
}else{
  taro<-1 #no meaning
}
if(type == "12.2.2"){
  test.data <- filter(test.data,test.data$Type.of.product == TOP_12_2_2) #need to confirm
}else{
  taro<-1 #no measing
}
if(type == "16.1.1"){
  test.data <- filter(test.data,test.data$Sex == sex_16_1_1)
}else{
  taro<-1 #do nothing
}
#####specific filter (end)###


###loop for countries (start)
i <- 1
while (i <= length(codelist)){ #for loop would be better
state <-test.data$GeoAreaName[test.data$GeoAreaCode == codelist[i]][1]  
state2cd <-test.data$ISO2CD[test.data$GeoAreaCode == codelist[i]][1]
state3cd <-test.data$ISO3CD[test.data$GeoAreaCode == codelist[i]][1]


filename <- paste(loc,graphloc,"/",type,"_",state3cd,".png",sep="")

if (length(test.data$TimePeriod[test.data$GeoAreaCode==codelist[i]]) == 0){
taro<-1 #do nothing 
  } else {
png(filename,width=400, height=200)
x <- test.data$TimePeriod[test.data$GeoAreaCode == codelist[i]]
y <- test.data$Value[test.data$GeoAreaCode == codelist[i]]
title <- paste("Index", type, state)

y_max <- y_maxs[k] 
y_min <- y_mins[k]

##for indivisual country## not working
#y_max <-max(test.data$Value[test.data$GeoAreaCode == codelist[i]])
#y_min <-min(test.data$Value[test.data$GeoAreaCode == codelist[i]])

y_max_state <- max(test.data$Value[test.data$GeoAreaCode == codelist[i]])
unit<-test.data$Units[test.data$GeoAreaCode == codelist[i]][1]
#plot(x,y,pch=19,col=2,xlim=c(2000,2020),ylim=c(0,100),main=title, xlab="Year", ylab=paste("Value(",unit,")",sep=""))
plot(x,y,pch=19,col=2,xlim=c(2000,2020),ylim=c(y_min,y_max),main=title, xlab="Year", ylab=paste("Value(",unit,")",sep=""))
#plot(x,y,pch=19,col=2,xlim=c(2000,2020),main=title, xlab="Year", ylab=paste("Value(",unit,")",sep=""))
#
dev.off()

##Latest check (start)################
l <- 1
year <- 10
latest_year <- 10
latest_value <- 0

while (l <= length(test.data$TimePeriod[test.data$GeoAreaCode == codelist[i]])){
 if(test.data$TimePeriod[test.data$GeoAreaCode == codelist[i]][l] > year){
   latest_year <- test.data$TimePeriod[test.data$GeoAreaCode == codelist[i]][l]
   latest_value <- test.data$Value[test.data$GeoAreaCode == codelist[i]][l] 
 }else{
   taro <- 1 #do nothing
 }

l <- l+1  
}
##Latest check (end)#################

#record
if (latest_year > 10){
export <- paste(type,state2cd,state3cd,latest_value,latest_year,unit,sep=",")
write(export,file=latestvaluetable, sep=",",append=TRUE)
}else{
  taro <- 1 #do nothing  
  }
#record (end)

}

i <- i+1
}
###loop for countries (end)

export.data <- filter(test.data,test.data$ISO3CD != "NA")
write.csv(export.data,paste(loc,tableloc,"/export_",type,".csv",sep=""))

k <- k+1
}
##loop for each 16 indicators (end) 