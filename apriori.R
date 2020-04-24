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
AccessValueInside<-function(threeDimDataType,col_num,value){
for (i in threeDimDataType[col_num]) { 
  return(as.numeric(i[toString(value)]))
}
}
################################################################################

check<- function(combination,listofold){
    for(i in 1:NROW(listofold)){
        if(length(combination)==length(listofold[[i]])){
            if( all(combination==listofold[[i]])){
                return(TRUE)
            }
        }
    }
    return(FALSE)
}

################################################################################

GetRules<- function(listOfitem) {
    
    listofold<-list()
    
    
    for(set in 1:NROW(listOfitem)){
        
        select<-names(listOfitem[[set]])
        
        y <- permutations(n=length(select), r=length(select), v=select, set=TRUE, repeats.allowed=FALSE)
        
        t<-"->"
        rules<-list()
        
        count<-1
        size<-ncol(rules)
        
        for(col in 1:(ncol(y)-1)) {
            old<-y[1,1:col]
            # print(col)
            listofold[[length(listofold)+1]]<-old
            message(y[1,1:col],t,y[1,(col+1):ncol(y)])
            rules[[length(rules)+1]]<-c(y[1,1:col],t,y[1,(col+1):ncol(y)])
            count<-count+1
            for(row in 1:nrow(y)) {
                
                if(!check(sort(y[row,1:col]),listofold)){
                    message(y[row,1:col],t,y[row,(col+1):ncol(y)])
                    rules[[length(rules)+1]]<-c(y[row,1:col],t,y[row,(col+1):ncol(y)])
                    count<-count+1
                    old=y[row,1:col]
                    listofold[[length(listofold)+1]]<-old
                }
                
            }
        }
        print(rules)
    }
}


################################################################################

#Main Function

################################################################################

MinSupport<-GetMinSupport()

MinConfidence<-GetMinConfidence()

data<-ReadData()
#data
unique<-UniqueValues(data)
#unique
support<-SupportOneItemSet(unique,data)
support
supportfiltered<-MinSupportFilter(support,MinSupport)
#supportfiltered
#names(supportfiltered[[1]])
print(AccessValueInside(support,1,0))
################################################################################    