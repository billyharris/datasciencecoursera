best<-function (state,outcome) {
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
    
    #Return hospital name in state with losest 30-day death rate
    stateData<-subset(data,data$State==state)[,c(2,index)]
    suppressWarnings(stateData[,2]<-as.numeric(stateData[,2]))
    bestPerformers<-which(stateData[,2]==min(stateData[,2],na.rm=TRUE))
    min(stateData[bestPerformers,1])
}