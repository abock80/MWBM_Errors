setwd("d:/abock/Water_Balance/MWBM_errors")
source("Code/HruBound_Assignment.R")

Level<-"Q90"

#NatID.csv
NatIDs<-read.csv("HruBounds/NatID.csv")

#hruBounds
hruBounds<-read.csv(paste("HruBounds/hruBounds_",Level,".csv",sep=""))

#GageBounds/Q90
gageBounds<-list.files(paste("Bounds/",Level,sep=""))

#NatErrorGroup
NatErrorGroup<-read.csv("HruBounds/Nat_ErrorGroup.csv")

# Join NatID to NatErrorGroup
ID_dFrame<-merge(x=NatIDs,y=NatErrorGroup,by="NatID",all=T)
# Remove extraneous columns
ID_dFrame<-ID_dFrame[,!names(ID_dFrame) %in% c("hru","HRU_ID","CalReg")]

# Create empty dataframe to hold bounds
emptydFrame<-data.frame(matrix(NA,nrow=109951,ncol=24))
colnames(emptydFrame)<-c("JanLow","FebLow","MarLow","AprLow","MayLow","JunLow","JulLow","AugLow","SepLow","OctLow","NovLow","DecLow",
                         "JanHigh","FebHigh","MarHigh","AprHigh","MayHigh","JunHigh","JulHigh","AugHigh","SepHigh","OctHigh",
                         "NovHigh","DecHigh")

# Bind to current dFrame
# Use global assignment to allow assignment in function
ID_dFrame<<-cbind(ID_dFrame,emptydFrame)


  
# works for one
lapply(Bounds,hruBoundAssign)
  
# Does it work for the group
lapply(gageBounds,hruBoundAssign)
 
  
  
# By Group
for (Bounds in gageBounds){   
  # Get bounds by CalRegion
  gageBounds1<-read.csv("Bounds/Q90/finalGRP_1_Q90",colClasses=c(rep("numeric",5),"character"))
  # Get list of unique gages
  gages<-unique(gageBounds1$GAGE)
}

# For a single gage
# Find rows with gages
Pos<-which(ID_dFrame$Gages %in% gages,arr.ind=TRUE)
gageBnd<-gageBounds1[which(gageBounds1$GAGE==gages[1]),]

# Calculate LowBounds for HRUs with Gage
ID_dFrame[Pos,"JanLow"]<-gageBnd[1,2]
ID_dFrame[Pos,"FebLow"]<-gageBnd[2,2]
ID_dFrame[Pos,"MarLow"]<-gageBnd[3,2]
ID_dFrame[Pos,"AprLow"]<-gageBnd[4,2]
ID_dFrame[Pos,"MayLow"]<-gageBnd[5,2]
ID_dFrame[Pos,"JunLow"]<-gageBnd[6,2]
ID_dFrame[Pos,"JulLow"]<-gageBnd[7,2]
ID_dFrame[Pos,"AugLow"]<-gageBnd[8,2]
ID_dFrame[Pos,"SepLow"]<-gageBnd[9,2]
ID_dFrame[Pos,"OctLow"]<-gageBnd[10,2]
ID_dFrame[Pos,"NovLow"]<-gageBnd[11,2]
ID_dFrame[Pos,"DecLow"]<-gageBnd[12,2]

# Calculate LowBounds for HRUs with Gage
ID_dFrame[Pos,"JanHigh"]<-gageBnd[1,4]
ID_dFrame[Pos,"FebHigh"]<-gageBnd[2,4]
ID_dFrame[Pos,"MarHigh"]<-gageBnd[3,4]
ID_dFrame[Pos,"AprHigh"]<-gageBnd[4,4]
ID_dFrame[Pos,"MayHigh"]<-gageBnd[5,4]
ID_dFrame[Pos,"JunHigh"]<-gageBnd[6,4]
ID_dFrame[Pos,"JulHigh"]<-gageBnd[7,4]
ID_dFrame[Pos,"AugHigh"]<-gageBnd[8,4]
ID_dFrame[Pos,"SepHigh"]<-gageBnd[9,4]
ID_dFrame[Pos,"OctHigh"]<-gageBnd[10,4]
ID_dFrame[Pos,"NovHigh"]<-gageBnd[11,4]
ID_dFrame[Pos,"DecHigh"]<-gageBnd[12,4]



  