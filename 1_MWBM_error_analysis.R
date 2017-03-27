library (moments)
library(boot)
library(dplyr)

setwd("d:/abock/Water_Balance/MWBM_errors")
source("Func/getBounds.R")
source("Func/MWBM_errors.R")

# Build a list of groups, each folder contains numerous files
Groups=list.dirs(path = "Groups", full.names = TRUE, recursive = F)
Q1 <- .05
Q2 <- .95
Level<-"Q90"

for (Group in Groups){
  print(Group)
  # Read in Group Z-score information
  Grp2<-paste("finalGRP_",unlist(strsplit(Group,"_"))[2],sep="")
  # Simulated/Observed data from the verification period
  GrpV<-read.table(paste(Group,"/verMONTHLYq",sep=""),sep="",colClasses=c("integer","character","integer","integer","integer",     
                                                                          "numeric","numeric","numeric","numeric"))
  colnames(GrpV)<-c("GageNum","STAID","Obs","YEAR","MONTH","OBSq","SIMq","OBSz","SIMz")

  # build unique list of gages for each grou[]
  gages<-unique(GrpV$STAID)
  
  # Get the Error Bounds based on validation  Period for 90/95/99 CI
  bounds<-do.call(rbind,lapply(gages,getBounds,GrpV,Q1,Q2)) #singlebound<-getBounds("01073000",GrpV)
  # write the results to the 'Bounds' Folder
  write.csv(bounds,paste("Bounds/",Grp2,"_",Level,sep=""),row.names=F,quote=F)
  
  ## Open calibration Period and append to validation data
  GrpV$Period<-2
  GrpC<-read.table(paste(Group,"/calMONTHLYq",sep=""),sep="",colClasses=c("integer","character","integer","integer","integer",
                                                                          "numeric","numeric","numeric","numeric"))
  colnames(GrpC)<-c("GageNum","STAID","Obs","YEAR","MONTH","OBSq","SIMq","OBSz","SIMz")
  GrpC$Period<-1
  GrpV<-rbind(GrpV,GrpC)
  
  # Get the Error Bounds for the specified uncertainty intervals, add to sim time series
  errorsQ<-do.call(rbind,lapply(gages,MWBM_errors,GrpV,Q1,Q2)) #singleError<-MWBM_errors("01073000",GrpV,0.025,0.975)
  colnames(errorsQ)<-c("GAGE","YEAR","MONTH",paste("sim",Q1,sep=""),"sim",paste("sim",Q2,sep=""),"obs","CR")
  # re-order for the time series formatted output.
  errorsQ<-errorsQ[order(errorsQ$GAGE,errorsQ$YEAR,errorsQ$MONTH),]
  write.csv(errorsQ,paste("Timeseries/",Grp2,"_",Level,sep=""),row.names = F,quote=F)

  CR<-errorsQ %>% group_by(GAGE,MONTH) %>% summarise(mean=mean(CR))
  
  colnames(CR)<-c("STAID","MONTH","CR")
  write.csv(CR,paste("CR/",Grp2,"_CR_",Level,sep=""),row.names = F,quote=F)
}
