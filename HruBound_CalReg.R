hruBoundReg<-function(Grp,hruBNDs){
  print(Grp)
  
  # By region
  Pos<-which(ID_dFrame$Calibration_region == Grp,arr.ind=T)
  print(Pos)
  hruBnd<-hruBounds[which(hruBounds$Calibration_region==Grp),]
  
  # Calculate LowBounds for HRUs with Gage
  ID_dFrame[Pos,"JanLow"]<<-hruBnd[1,2]
  ID_dFrame[Pos,"FebLow"]<<-hruBnd[2,2]
  ID_dFrame[Pos,"MarLow"]<<-hruBnd[3,2]
  ID_dFrame[Pos,"AprLow"]<<-hruBnd[4,2]
  ID_dFrame[Pos,"MayLow"]<<-hruBnd[5,2]
  ID_dFrame[Pos,"JunLow"]<<-hruBnd[6,2]
  ID_dFrame[Pos,"JulLow"]<<-hruBnd[7,2]
  ID_dFrame[Pos,"AugLow"]<<-hruBnd[8,2]
  ID_dFrame[Pos,"SepLow"]<<-hruBnd[9,2]
  ID_dFrame[Pos,"OctLow"]<<-hruBnd[10,2]
  ID_dFrame[Pos,"NovLow"]<<-hruBnd[11,2]
  ID_dFrame[Pos,"DecLow"]<<-hruBnd[12,2]

  # Calculate LowBounds for HRUs with Gage
  ID_dFrame[Pos,"JanHigh"]<<-hruBnd[1,3]
  ID_dFrame[Pos,"FebHigh"]<<-hruBnd[2,3]
  ID_dFrame[Pos,"MarHigh"]<<-hruBnd[3,3]
  ID_dFrame[Pos,"AprHigh"]<<-hruBnd[4,3]
  ID_dFrame[Pos,"MayHigh"]<<-hruBnd[5,3]
  ID_dFrame[Pos,"JunHigh"]<<-hruBnd[6,3]
  ID_dFrame[Pos,"JulHigh"]<<-hruBnd[7,3]
  ID_dFrame[Pos,"AugHigh"]<<-hruBnd[8,3]
  ID_dFrame[Pos,"SepHigh"]<<-hruBnd[9,3]
  ID_dFrame[Pos,"OctHigh"]<<-hruBnd[10,3]
  ID_dFrame[Pos,"NovHigh"]<<-hruBnd[11,3]
  ID_dFrame[Pos,"DecHigh"]<<-hruBnd[12,3]

#return(ID_dFrame)
}


