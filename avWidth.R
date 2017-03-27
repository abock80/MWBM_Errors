avWidth<-function(gage,dFrame,group,q1,q2,Lev){
  print(gage)
  print(Group)
  gageTS<-dFrame[dFrame$GAGE==gage,]
  
  grpBNDS<-read.csv(paste("Bounds/",group,"_",Lev,sep=""),header=T,colClasses=c(rep("numeric",4),"character","character"))
  gageBNDS<-grpBNDS[grpBNDS$GAGE==gage,]
  print(summary(gageBNDS))
  
  # empty data frame to bind data with
  emptyDF<-data.frame(GAGE=character(),MONTH=numeric(),AW=numeric(),AWclim=numeric(),AWI=numeric(),CR=numeric())
  
  months<-c(1,2,3,4,5,6,7,8,9,10,11,12)
  count<-1
  for (month in months){
    monthTS<-gageTS[gageTS$MONTH==month,]
    print(summary(monthTS))
    monthBNDS<-gageBNDS[gageBNDS$months2==month,]
    if (dim(monthBNDS)[1]==0){
      print(month)
    }else{
      monthHigh<-monthTS[,paste("sim",q2,sep="")]
      monthLow<-monthTS[,paste("sim",q1,sep="")]
      
      # Get the average width
      AW<-mean(monthTS$AW)
      print(summary(AW))
      # Get the Bounds by month for the gage
      monthBNDS<-gageBNDS[gageBNDS$months2==month,]
      
      # Get the Range of simmulated streamflow
      Quants<-quantile(gageTS$sim,probs=c(Q1,Q2))
      # Calculate the average width climate index
      AWclim<-Quants[2]-Quants[1]  
      # Calculate the Average Width Index using the two measures calculated above
      AWI=1-(AW/AWclim)
      
#********************************      
      # If the month is January start a new data frame
      if (count==1){
        finalDF<-rbind(emptyDF,data.frame(GAGE=unique(monthTS$GAGE),MONTH=month,AW=AW,AWclim=AWclim,AWI=AWI,CR=as.numeric(unique(monthTS$CR))))
        colnames(finalDF)<-c("GAGE","MONTH","AW","AWclim","AWI","CR")
      }else{
        finalDF<-rbind(finalDF,data.frame(GAGE=unique(monthTS$GAGE),MONTH=month,AW=AW,AWclim=AWclim,AWI=AWI,CR=as.numeric(unique(monthTS$CR))))
      }
      count<-count+1
    }
  }
  return(finalDF)
}
