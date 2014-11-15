# pollutantmean: This is the version required by the programming assigment
pollutantmean<-function(directory,pollutant,id) {
    rundir<-getwd()
    setwd(directory)
    values.store<-numeric()
    id<-formatC(id,width=3,format="d",flag="0")
    filenames<-paste(id,".csv",sep="")
    for (readfile in filenames) {
        data<-read.csv(readfile)
        cases<-data[[pollutant]]
        values.store<-append(values.store,cases)
    }
    setwd(rundir)
    mean(values.store,na.rm=TRUE)
}

# pollutant2: This version returns a vector with each individual monitor's mean
pollutantmean2<-function(directory,pollutant,id) {
    rundir<-getwd()
    setwd(directory)
    means.store<-numeric()
    id<-formatC(id,width=3,format="d",flag="0")
    filenames<-paste(id,".csv",sep="")
    for (i in 1:length(filenames)) {
        readfile<-filenames[i]
        data<-read.csv(readfile)
        outcome<-mean(data[[pollutant]],na.rm=TRUE)
        means.store[i]<-outcome
    }
    setwd(rundir)
    mean(means.store)
}

# complete: This is for the assignment
complete <- function (directory,id=1:332) {
    rundir<-getwd()
    setwd(directory)
    nobs<-numeric()
    new.id<-formatC(id,width=3,format="d",flag="0")
    filenames<-paste(new.id,".csv",sep="")
    for (readfile in filenames) {
        data<-read.csv(readfile)
        comp.obs<-sum(complete.cases(data))
        nobs<-append(nobs,comp.obs)     
    }
data.frame(id,nobs)    
}

# corr: This is for the assignment

corr <- function (directory, threshold=0) {
    rundir<-getwd()
    setwd(directory)
    id=1:332
    new.id<-formatC(id,width=3,format="d",flag="0")
    filenames<-paste(new.id,".csv",sep="")
    output<-numeric()
    for (f in filenames) {
        data<-read.csv(f)
        complete <- data[complete.cases(data),]
        if (nrow(complete)>=threshold) {
            c<-cor(complete$sulfate,complete$nitrate)
            output<-append(output,c)
        }
    }
    setwd(rundir)
    output[!is.na(output)]
}