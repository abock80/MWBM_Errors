setwd("d:/abock/Water_Balance/MWBM_errors")
source("Code/HruBound_Assignment.R")

# Build a list of groups, each folder contains numerous files
Groups=list.dirs(path = "Groups", full.names = TRUE, recursive = F)
Q1 <- .025
Q2 <- .975
Level<-"Q95"

# empty data frame to bind data with
emptyDF<-data.frame(CALGROUP=character(),Q1=numeric(),Q2=numeric(),Months=numeric())
#colnames(emptyDF)<-c("CalReg",paste("sim",Q1,sep=""),paste("sim",Q2,sep=""))
count<-1
for (Group in Groups){
  # Read in Group Z-score information
  Grp2<-paste("finalGRP_",unlist(strsplit(Group,"_"))[2],sep="")
  # Simulated/Observed data from the verification period
  GrpV<-read.table(paste(Group,"/verMONTHLYq",sep=""),sep="",colClasses=c("integer","character","integer","integer","integer",     
                                                                        "numeric","numeric","numeric","numeric"))
  colnames(GrpV)<-c("GageNum","STAID","Obs","YEAR","MONTH","OBSq","SIMq","OBSz","SIMz")

  # Get the Error Bounds based on validation  Period for 90/95/99 CI
  bounds<-hruBounds(GrpV,Q1,Q2) 
  #colnames(bounds)<-c("simLow",paste("sim",Q1,sep=""),"simHigh",paste("sim",Q2,sep=""),"months")
  
  
  # If the month is January start a new data frame
  if (count==1){
    finalDF<-rbind(emptyDF,data.frame(CalReg=Grp2,Q1=bounds[,2],Q2=bounds[,4],Months=bounds[,5]))
  }else{
    finalDF<-rbind(finalDF,data.frame(CalReg=Grp2,Q1=bounds[,2],Q2=bounds[,4],Months=bounds[,5]))
  }
  count<-count+1
}# write the results to the 'Bounds' Folder
write.csv(finalDF,paste("HruBounds/hruBounds_",Level,".csv",sep=""),row.names=F,quote=F)
  

