rankhospital<-function(state,outcome,num="best") {
    #Read outcome data
    setwd("/home/billy/Documents/Coursera Data Science/ProgrammingR/hospital")
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    #Check state and outcome are valid
    if (state %in% data$State==FALSE) {
        stop ("invalid state")
    }
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
    
    # Prepare and order the data file
    stateData<-subset(data,data$State==state)[,c(2,index)]
    names(stateData)<-c("Hospital.Name","Condition")
    suppressWarnings(stateData$Condition<-as.numeric(stateData$Condition))
    stateData<-subset(stateData,!is.na(stateData$Condition))
    stateData<-stateData[order(stateData$Hospital.Name),]
    stateData<-stateData[order(stateData$Condition),]
    STATE_DATA_LENGTH<-nrow(stateData)
    
    # evaluate the ranking
    if (num=="best") num<-1
    else if (num=="worst") num<-STATE_DATA_LENGTH
    
    # return the hospital name
    stateData$Hospital.Name[num]
}