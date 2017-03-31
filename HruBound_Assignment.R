hruBoundAssign<-function(Grp){
  # Get bounds by CalRegion
  gageBounds1<-read.csv(paste("Bounds/Q90/",Grp,sep=""),colClasses=c(rep("numeric",5),"character"))
  # Get list of unique gages
  gages<-unique(gageBounds1$GAGE)
  
  for (gage in gages){
    print(gage)
    # For a single gage
    # Find rows with gages
    Pos<-which(ID_dFrame$Gages %in% gage,arr.ind=T)
    print(Pos)
    #DFrame<-DFrame[,which(DFrame$Gages == gage,arr.ind=FALSE)]
    gageBnd<-gageBounds1[which(gageBounds1$GAGE==gage),]
    print (summary(gageBnd))
  
    # Calculate LowBounds for HRUs with Gage
    ID_dFrame[Pos,"JanLow"]<<-gageBnd[1,2]
    ID_dFrame[Pos,"FebLow"]<<-gageBnd[2,2]
    ID_dFrame[Pos,"MarLow"]<<-gageBnd[3,2]
    ID_dFrame[Pos,"AprLow"]<<-gageBnd[4,2]
    ID_dFrame[Pos,"MayLow"]<<-gageBnd[5,2]
    ID_dFrame[Pos,"JunLow"]<<-gageBnd[6,2]
    ID_dFrame[Pos,"JulLow"]<<-gageBnd[7,2]
    ID_dFrame[Pos,"AugLow"]<<-gageBnd[8,2]
    ID_dFrame[Pos,"SepLow"]<<-gageBnd[9,2]
    ID_dFrame[Pos,"OctLow"]<<-gageBnd[10,2]
    ID_dFrame[Pos,"NovLow"]<<-gageBnd[11,2]
    ID_dFrame[Pos,"DecLow"]<<-gageBnd[12,2]
  
    # Calculate LowBounds for HRUs with Gage
    ID_dFrame[Pos,"JanHigh"]<<-gageBnd[1,4]
    ID_dFrame[Pos,"FebHigh"]<<-gageBnd[2,4]
    ID_dFrame[Pos,"MarHigh"]<<-gageBnd[3,4]
    ID_dFrame[Pos,"AprHigh"]<<-gageBnd[4,4]
    ID_dFrame[Pos,"MayHigh"]<<-gageBnd[5,4]
    ID_dFrame[Pos,"JunHigh"]<<-gageBnd[6,4]
    ID_dFrame[Pos,"JulHigh"]<<-gageBnd[7,4]
    ID_dFrame[Pos,"AugHigh"]<<-gageBnd[8,4]
    ID_dFrame[Pos,"SepHigh"]<<-gageBnd[9,4]
    ID_dFrame[Pos,"OctHigh"]<<-gageBnd[10,4]
    ID_dFrame[Pos,"NovHigh"]<<-gageBnd[11,4]
    ID_dFrame[Pos,"DecHigh"]<<-gageBnd[12,4]
  }
  #return(DFrame)
}


