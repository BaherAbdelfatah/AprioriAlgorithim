#Libraries includes 
library(reader)
library(dplyr)
library(tidyr)
library(data.table)

################################################################################
ReadData <- function() {
data=read.table("ticdata2000.txt",header=FALSE)
#data=data[1:5822,10:21]
data=data[1:20,10:21]
return(data)
}
################################################################################

UniqueValues <- function(data) {
unique=c()
for (i in 1:12) {
    temp<-table(data[,i])
    unique<-append(unique,list(temp))
}
return(unique)
}
################################################################################

SupportOneItemSet<- function(unique,data) {
support = c()
for (i in 1:12) { 
    temp=c()
    for(j in unique[i]){
        temp<-append(temp,j/NROW(data[,i]))
    }
    support<-append(support,list(temp))
}
return(support)
}
################################################################################

MinSupportFilter<- function(support,MinSupport) {
supportfiltered=c()
for (i in 1:12) { 
    temp=c()
    for(j in support[i]){
        temp<-append(temp,j[which(j >= MinSupport)])
    }
    supportfiltered<-append(supportfiltered,list(temp))
}
return(supportfiltered)
}
################################################################################

GetMinSupport<-function(){
    print("Enter Min Support: ")
    MinSupport<-readLines("stdin", 1)
    MinSupport=as.numeric(MinSupport)
    return(MinSupport)
}
################################################################################

GetMinConfidence<-function(){
    print("Enter Min Confidence: ")
    MinConfidence<-readLines("stdin", 1)
    MinConfidence=as.numeric(MinConfidence)
    MinConfidence
    return(MinConfidence)
}
################################################################################



#Main Function

################################################################################

MinSupport<-GetMinSupport()

MinConfidence<-GetMinConfidence()

data<-ReadData()
data
unique<-UniqueValues(data)
unique
support<-SupportOneItemSet(unique,data)
support
supportfiltered<-MinSupportFilter(support,MinSupport)
supportfiltered

################################################################################



