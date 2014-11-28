rankall <- function (outcome, num="best") {
    # Read outcome data
    setwd("/home/billy/Documents/Coursera Data Science/ProgrammingR/hospital")
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    # Check state is valid
    if (outcome=="heart attack") {
        index<-11
    }
    else if (outcome=="heart failure") {
        index<-17
    }
    else if (outcome=="pneumonia") {
        index<-23
    }
    else stop ("invalid outcome")
    
    # Construct the data file for ranking and split as list
    outcomeData<-data.frame(Name=data$Hospital.Name, State=data$State,Outcome=data[,index])
    suppressWarnings(outcomeData$Outcome<-as.numeric(as.character((outcomeData$Outcome))))
    outcomeData<-outcomeData[!is.na(outcomeData$Outcome),]
    outcomeData$State<-as.factor(outcomeData$State)
    outcomeByState<-split(outcomeData,outcomeData$State)
    # produce vectors to hold the outcomes
    hospital<-vector()
    state<-as.factor(levels(outcomeData$State))

    # traverse the list
    for (i in 1:length(outcomeByState)) {
        working<-as.data.frame(outcomeByState[i])
        names(working)<-c("Name","State","Outcome")
        working<-working[order(working$Name),]
        working<-working[order(working$Outcome),]
        if (num=="best") n<-1
        else if (num=="worst") n<-nrow(working)
        else n<-num

        hospital[i]<-as.character(working$Name[n])
       }
    o<-data.frame(hospital=hospital,state=as.data.frame(state))
    rownames(o)<-o$state
    o
}