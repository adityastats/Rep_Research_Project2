# Loading required libraries
library(R.utils)
library(ggplot2)

#=====================
#  Data Processing
#=====================

# 1. Downloading the StormData bz2 file :

if(!file.exists("StormData.csv.bz2")){
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = "./StormData.csv.bz2")
}

# 2. Unzipping the bz2 file :
filename <- "StormData.csv.bz2"
destfile <- "StormData.csv"
if(!file.exists("StormData.csv")){
  bunzip2(filename,destfile,overwrite=TRUE,remove=FALSE)
}

# 3. Reading the storm dataset into a dataframe 
storm <- data.table(read.csv(file="StormData.csv",header=TRUE,stringsAsFactors = FALSE))
# we don't need all the columns in the dataset to achieve our objective, so, we will extract the required
# columns and store it in a new dataframe "analysisData"
names(storm)
analysisData <- storm[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP",
                         "CROPDMG","CROPDMGEXP")]
# Examining the new dataset
str(analysisData)

# Changing the BGN_DATE column(character) into class date using as.Date and extracting only the year
analysisData$BGN_DATE <- as.Date(analysisData$BGN_DATE,format="%m/%d/%Y %H:%M:%S")
analysisData$YEAR <- as.numeric(format(analysisData$BGN_DATE,"%Y"))
str(analysisData)

#plotting the histogram for total number of weather events in a year
ggplot(analysisData,aes(x=YEAR))+geom_histogram(binwidth = 1,fill="lightblue",color="black")+
  ggtitle("Histogram of Total Number of Weather Events")+
  labs(x="Year",y="Number of Weather Events")

# Creating the columns with property damage, crop damage and total damage
library(data.table)
analysisData <- data.table(analysisData)
analysisData[is.na(analysisData)] <- 0
analysisData[, `:=`("PropertyDamage", if (PROPDMGEXP %in% c("K", "k")) PROPDMG * 1000 
           else if (PROPDMGEXP %in% c("M", "m")) PROPDMG * 1e+06 
           else if (PROPDMGEXP %in% c("B","b")) PROPDMG * 1e+09 
           else if (PROPDMGEXP %in% c("H","h")) PROPDMG * 100
           else 0), by = 1:nrow(analysisData)]
analysisData[, `:=`("CropDamage", if (CROPDMGEXP %in% c("K", "k")) CROPDMG * 1000 
                    else if (CROPDMGEXP %in% c("M", "m")) CROPDMG * 1e+06 
                    else if (CROPDMGEXP %in% c("B","b")) CROPDMG * 1e+09 
                    else if (CROPDMGEXP %in% c("H","h")) CROPDMG * 100
                    else 0), by = 1:nrow(analysisData)]
analysisData[, `:=`("TotalDamage", get("PropertyDamage") + get("CropDamage")), by = 1:nrow(analysisData)]
analysisData[,`:=`("HealthImpact",get("FATALITIES")+get("INJURIES")),by=1:nrow(analysisData)]
head(analysisData,n=3)

finaldata <- analysisData[,c("EVTYPE","FATALITIES","INJURIES","PropertyDamage","CropDamage","TotalDamage",
                        "HealthImpact")]
head(finaldata)
#Renaming few EVTYPE which are same but with different names
finaldata$EVTYPE[grep("heat|drought|dry",finaldata$EVTYPE, ignore.case = T)] <- "DRAUGHT"
finaldata$EVTYPE[grep("tornado|tornadoes",finaldata$EVTYPE, ignore.case = T)] <- "TORNADO"
finaldata$EVTYPE[grep("flood|fld|rain",finaldata$EVTYPE, ignore.case = T)] <- "FLOOD"
finaldata$EVTYPE[grep("storm|blizzard|hail|cold|snow|frost|freeze",finaldata$EVTYPE,ignore.case = T)] <- "STORM"
finaldata$EVTYPE[grep("tstm|thunderstorm|lightning",finaldata$EVTYPE, ignore.case = T)] <- "THUNDERSTORM"
finaldata$EVTYPE[grep("rip",finaldata$EVTYPE, ignore.case = T)] <- "RIP CURRENTS"
finaldata$EVTYPE[grep("fire|wildfire",finaldata$EVTYPE, ignore.case = T)] <- "FIRE"
finaldata$EVTYPE[grep("wind",finaldata$EVTYPE, ignore.case = T)] <- "WIND"
finaldata$EVTYPE[grep("hurricane",finaldata$EVTYPE, ignore.case = T)] <- "HURRICANE"

#Analysis for answering first question

Fatalities <- with(finaldata,aggregate(FATALITIES~EVTYPE,FUN=sum))
Injuries <- with(finaldata,aggregate(INJURIES~EVTYPE,FUN=sum))
Health <- with(finaldata,aggregate(HealthImpact~EVTYPE,FUN=sum))
FatalitiesOrder <- Fatalities[order(Fatalities$FATALITIES,decreasing = T),]
InjuriesOrder <- Injuries[order(Injuries$INJURIES,decreasing = T),]
HealthOrder <- Health[order(Health$HealthImpact,decreasing = T),]
Top10Fatalities <- FatalitiesOrder[1:10,]
Top10Injuries <- InjuriesOrder[1:10,]
Top10Health <- HealthOrder[1:10,]


Top10Fatalities <- cbind(Top10Fatalities,rep("Fatalities",10))
names(Top10Fatalities)<-c("EVTYPE","Casualties","Type")
Top10Injuries <- cbind(Top10Injuries,rep("Injuries",10))
names(Top10Injuries)<-c("EVTYPE","Casualties","Type")
Top10Health <- cbind(Top10Health,rep("Total",10))
names(Top10Health)<-c("EVTYPE","Casualties","Type")
HealthImpact<- rbind(Top10Fatalities,Top10Injuries,Top10Health)
ggplot(HealthImpact,aes(x=EVTYPE,y=Casualties,fill=EVTYPE))+geom_bar(stat="identity")+
  facet_grid(Type~.)+theme(axis.text.x = element_text(color="#25A203",angle = 90,hjust = 1,vjust = 0.5))+
  labs(title="Most Harmful Event Type for Public Health",x="Event Type",y="Casualties")


# Analysis for answering 2nd question
Property <- with(finaldata,aggregate(PropertyDamage~EVTYPE,FUN=sum))
Crop <- with(finaldata,aggregate(CropDamage~EVTYPE,FUN=sum))
Total <- with(finaldata,aggregate(TotalDamage~EVTYPE,FUN=sum))
PropertyOrder <- Property[order(Property$PropertyDamage,decreasing = T),]
CropOrder <- Crop[order(Crop$CropDamage,decreasing = T),]
TotalOrder <- Total[order(Total$TotalDamage,decreasing = T),]
Top10Property <- PropertyOrder[1:10,]
Top10Property <- cbind(Top10Property,rep("Property Damage",10))
names(Top10Property) <- c("EVTYPE","Damage","Type")
Top10Crop <- CropOrder[1:10,]
Top10Crop <- cbind(Top10Crop,rep("Crop Damage",10))
names(Top10Crop) <- c("EVTYPE","Damage","Type")
Top10Economy <- TotalOrder[1:10,]
Top10Economy <- cbind(Top10Economy,rep("Total Damage",10))
names(Top10Economy) <- c("EVTYPE","Damage","Type")
EconomyEffect <- rbind(Top10Property,Top10Crop,Top10Economy)
ggplot(EconomyEffect,aes(x=EVTYPE,y=Damage,fill=EVTYPE))+geom_bar(stat="identity")+
  facet_grid(Type~.)+theme(axis.text.x = element_text(color="#25A203",angle = 90,hjust = 1,vjust = 0.5))+
  labs(title="Most Harmful Event Types for Economy",x="Event Type",y="Damage")


