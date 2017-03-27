getBounds<-function(gage,dFrame,q1,q2){ # take month out of function create loop for month
    print(q1,q2)
    # list of months for looping
    months<-c(1,2,3,4,5,6,7,8,9,10,11,12)
    # make duplicate list of months, 
    # some gages may be missing months, so we need to take that into account
    months2<-months
    
    #Empty list for upper and lower bounds
    simHigh<-c()
    simHighQ<-c()
    simLow<-c()
    simLowQ<-c()

    # Loop through Months    
    count=1
    for (month in months){
      print(gage)
      print(months2)

      
      # Build a data frame subsetting by gage and month
      gage1<-dFrame[dFrame$STAID==gage,]
      gageBymonth1<-gage1[gage1$MONTH==month,]
      print(dim(gageBymonth1))
      
      # Some gages do not have data for specific months
      # This if/else avoids errors and writing bum data to the output 
      # if data does not exist for a specific month
      # Question for Will - minimum number of observed data
      if (dim(gageBymonth1)[1]==0){
        print(month)
        months2<-months2[!months2 %in% month]
      }else{
        # Step 1- get simulated data for gage of Concern
        gageSim<-gageBymonth1$SIM
        
        # Step 2 - get the errors from all other gages in calibration region
        # for that specific month
        gage2<-dFrame[dFrame$STAID!=gage,]
        gageBymonth2<-gage2[gage2$MONTH==month,]
        
        
        # Step 3- calculate the residual
        # Because we are using Z-scores we can subtract
        # If we were using the raw magnitudes, we would need to take a ratio
        resid2<-gageBymonth2$OBSz-gageBymonth2$SIMz
        
        # Step 4 - get the distribution of errors from all other gages
        #qSIM<-quantile(resid2,probs=c(0.025,0.975))
        qSIM<-quantile(resid2,probs=c(q1,q2))
        
        # Step 5 add to list for gage/month
        simLowQ<-append(simLowQ,qSIM[1])
        simLow<-append(simLow,min(resid2))

        simHighQ<-append(simHighQ,qSIM[2])
        simHigh<-append(simHigh,max(resid2))
      }
      errorDF<-cbind(simLow,simLowQ,simHigh,simHighQ)
    }
    # final dataframe has data for all gages, for all months for a given calibration region
    head(errorDF)
    rownames(errorDF)<-paste(gage,months2,sep="_")
    errorDF<-as.data.frame(cbind(errorDF,months2))
    errorDF$GAGE<-gage
    return(errorDF)
}


