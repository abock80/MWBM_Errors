MWBM_errors<-function(gage,dFrame,qLow,qHigh){ # take month out of function create loop for month
  # list of months for looping
  months<-c(1,2,3,4,5,6,7,8,9,10,11,12)
  
  # empty data frame to bind data with
  emptyDF<-data.frame(GAGE=character(),YEAR=numeric(),MONTH=numeric(),simLow=numeric(),sim=numeric(),simHigh=numeric(),obs=numeric(),CR=numeric())
  
  count=1
  for (month in months){
    print(gage)

    # Step 1 - get the simulated errors for a specific gage for a specific month
    # Subset by gage and month
    gage1<-dFrame[dFrame$STAID==gage,]
    gageBymonth1<-gage1[gage1$MONTH==month,]
    # subset by cal/val period -> gageBymonth1<-gageBymonth1[gageBymonth1$Period==2,]
    print(dim(gageBymonth1))
    
    # Some gages do not have data for specific months
    # This if/else avoids errors and writing bum data to the output 
    # Question for Will - minimum number of observed data
    if (dim(gageBymonth1)[1]==0){
      print(month)
    }else{
      # get simulated data for gages of Concern
      gageSim<-gageBymonth1$SIMz
      
      # Step 2 get the errors from all other gages for that specific month
      gage2<-dFrame[dFrame$STAID!=gage,]
      gageBymonth3<-gage2[gage2$MONTH==month,]
      # Because we are using Z-scores we can subtract
      # If we were using the raw magnitudes, we would need to take a ratio
      resid2<-gageBymonth3$OBSz-gageBymonth3$SIMz
      # get the distribution of errors from all other gages
      #qSIM<-quantile(resid2,probs=c(0.025,.975))
      if (qLow==0){
        qSim<-c(min(resid2),max(resid2))
      }else{
        qSim<-quantile(resid2,probs=c(qLow,qHigh))
      }
      
      print(qSim)  
      
      # add errors back into SIM from gage of concern
      simLow<-gageSim-abs(qSim[1])
      simHigh<-gageSim+qSim[2]
      print(simLow)
      print(simHigh)
      
      # Find the proportion of measured values for streamgage of concern that overlap with the simualted errors
      Prop<-length(gageBymonth1$OBSz[gageBymonth1$OBSz > simLow & gageBymonth1$OBSz < simHigh])/length(gageBymonth1$OBSz)
      print(Prop)
      # Write data to dataframe
      monthDF<-data.frame(gageBymonth1$YEAR,gageBymonth1$MONTH,simLow,gageBymonth1$SIMz,simHigh,gageBymonth1$OBSz,Prop)
      colnames(monthDF)<-c("YEAR","MONTH","simLow","sim","simHigh","obs","CR")
      
      if (count==1){
        finalDF<-rbind(emptyDF,data.frame(GAGE=rep(gage,length(simLow)),YEAR=gageBymonth1$YEAR,MONTH=gageBymonth1$MONTH,simLow=simLow,
                                          sim=gageBymonth1$SIMz,simHigh=simHigh,obs=gageBymonth1$OBSz,Cprop=rep(Prop,length(simLow))))
        colnames(finalDF)<-c("GAGE","YEAR","MONTH","simLow","sim","simHigh","obs","CR")
      }else{
        finalDF<-rbind(finalDF,cbind(GAGE=rep(gage,length(simLow)),monthDF))
      }
      count<-count+1
    }
  }
  # final df has data for all gages, for all months
  return(finalDF)
}
