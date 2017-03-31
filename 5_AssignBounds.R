setwd("d:/abock/Water_Balance/MWBM_errors")
source("MWBM_Errors/HruBound_Gages.R")
source("MWBM_Errors/HruBound_CalReg.R")

Level<-"Q99"

#NatID.csv
NatIDs<-read.csv("HruBounds/NatID.csv")

#hruBounds
hruBounds<-read.csv(paste("HruBounds/regBounds_",Level,".csv",sep=""))
calReg<-unlist(strsplit(as.character(hruBounds$CalReg),"_"))

hruBounds$Calibration_region<-as.numeric(calReg[seq(2,length(calReg),2)])
CalReg<-unique(as.numeric(calReg[seq(2,length(calReg),2)]))

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

# 1 - Assign Uncertainty Bounds for HRUs within catchments with calibration gages.
#lapply(Bounds,hruBoundAssign)
lapply(gageBounds,hruBoundGage,Level)
 
# Start Function Here 
#Reg<-CalReg[1]
#hruBoundReg(Reg,hruBNDs)
lapply(CalReg,hruBoundReg,hruBNDs)

write.csv(ID_dFrame,paste("HruBounds/HruBounds_",Level,".csv",sep=""),quote=F,row.names=F)

