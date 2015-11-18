##Read Data
data<-read.csv("repdata-data-StormData.csv.bz2")

##Load necessary Packages and set options
library("dplyr")
library ("ggplot2")
library("gridExtra")
options(scipen=999)

#TRANSFORM DATA
#Transform Date
data$BGN_DATE<-as.numeric(format(as.Date(data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

#Transform Property multiplier
data$propMult[data$PROPDMGEXP == "K"] <- 1000
data$propMult[data$PROPDMGEXP == "M"] <- 1000000
data$propMult[data$PROPDMGEXP == ""] <- 1
data$propMult[data$PROPDMGEXP == "B"] <- 1000000000
data$propMult[data$PROPDMGEXP == "m"] <- 1000000
data$propMult[data$PROPDMGEXP == "0"] <- 1
data$propMult[data$PROPDMGEXP == "5"] <- 100000
data$propMult[data$PROPDMGEXP == "6"] <- 1000000
data$propMult[data$PROPDMGEXP == "4"] <- 10000
data$propMult[data$PROPDMGEXP == "2"] <- 100
data$propMult[data$PROPDMGEXP == "3"] <- 1000
data$propMult[data$PROPDMGEXP == "h"] <- 100
data$propMult[data$PROPDMGEXP == "7"] <- 10000000
data$propMult[data$PROPDMGEXP == "H"] <- 100
data$propMult[data$PROPDMGEXP == "1"] <- 10
data$propMult[data$PROPDMGEXP == "8"] <- 100000000
data$propMult[data$PROPDMGEXP == "+"] <- 0
data$propMult[data$PROPDMGEXP == "-"] <- 0
data$propMult[data$PROPDMGEXP == "?"] <- 0

data$newPROPDMG<-data$PROPDMG * data$propMult

#Tranform Crop multiplier
data$cropMult[data$CROPDMGEXP == "M"] <- 1000000
data$cropMult[data$CROPDMGEXP == "K"] <- 1000
data$cropMult[data$CROPDMGEXP == "m"] <- 1000000
data$cropMult[data$CROPDMGEXP == "B"] <- 1000000000
data$cropMult[data$CROPDMGEXP == "?"] <- 0
data$cropMult[data$CROPDMGEXP == "0"] <- 0
data$cropMult[data$CROPDMGEXP == "k"] <- 1000
data$cropMult[data$CROPDMGEXP == "2"] <- 100

data$newCROPDMG<-data$CROPDMG * data$cropMult

data$newDMG<-(data$newPROPDMG + data$newCROPDMG)/1000000000

##Plot events
ggplot(data, aes(BGN_DATE))+
     geom_histogram(aes(fill=..count..))+
     labs(title=("Events Observed From 1950 to 2011"))+
     labs(x="Year", y="Number of Events")
#hist(data$BGN_DATE, breaks=30, main = "No. of events collected over the Year", xlab = "Year", ylab = "No. of events")

##Slice the data to work only with years that have large observations
highData<-data[data$BGN_DATE>=1995,]

#Summarise of Fatalities events and get the top 15 result 
deaths <- aggregate(FATALITIES ~ EVTYPE, highData, sum)
deaths <- deaths[order(-deaths$FATALITIES), ][1:15, ]
deaths <- within(deaths, EVTYPE <- factor(x = EVTYPE, levels = deaths$EVTYPE))
#Summarise of Injuries events and get the top 15 result 
injuries <- aggregate(INJURIES ~ EVTYPE, highData, sum)
injuries <- injuries[order(-injuries$INJURIES), ][1:15, ]
injuries <- within(injuries, EVTYPE <- factor(x = EVTYPE, levels = injuries$EVTYPE))

#Create Plot
plotFatalities<-qplot(EVTYPE, data=deaths, weight=FATALITIES,
     geom="bar", binwidth=1)+
     geom_bar(fill="lightgreen")+
     scale_y_continuous("FATALITIES") + 
     theme(axis.text.x = element_text(angle = 90, 
                                      hjust = 1)) + xlab("Weather Event") +
     ggtitle("Total FATALITIES by Severe Weather\n from 1995 - 2011")

plotInjuries<-qplot(EVTYPE, data=injuries, weight=INJURIES,
      geom="bar", binwidth=1)+
     geom_bar(fill="lightblue")+
     scale_y_continuous("INJURIES") + 
     theme(axis.text.x = element_text(angle = 90, 
                                      hjust = 1)) + xlab("Weather Event") +
     ggtitle("Total INJURIES by Severe Weather\n from 1995 - 2011")

grid.arrange(plotFatalities, plotInjuries, ncol = 2)

#Summarise economic damage and get the top 15 results
ecoDamage <- highData[order(-highData$newDMG),][1:15, ]
ecoDamage <- within(ecoDamage, EVTYPE <- factor(x = EVTYPE, levels = ecoDamage$EVTYPE))

##Plot Economic Damage
qplot(EVTYPE, data=ecoDamage, weight=newDMG,
                    geom="bar", binwidth=1)+
     geom_bar(fill="purple")+
     scale_y_continuous("Economic Damage (US$ Billions)") + 
     theme(axis.text.x = element_text(angle = 90, 
                                      hjust = 1)) + xlab("Weather Event") +
     ggtitle("Total Economic (Property and Crops) Damage by\n Severe Weather from 1995 - 2011")

