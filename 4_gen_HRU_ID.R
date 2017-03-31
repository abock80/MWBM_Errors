setwd("d:/abock/Water_Balance/MWBM_errors")


regions<-c("r01","r02","r03","r04","r05","r06","r07","r08","r09","r10l",
           "r10u","r11","r12","r13","r14","r15","r16","r17","r18")

numHrus<-c(2462,4827,9899,5936,7182,2303,8205,4449,1717,8603,10299,7373,7815,1958,3879,
           3441,2664,11102,5837)

# 1 - Get info for HRUS, GF region, cal region, NatID
hruData<-read.csv("HRUdata/HESS-2015-325/dataBYhru.csv")
hruData<-hruData[,colnames(hruData) %in% c("HRU_ID","GF_Region","Calibration_region")]
hruData$NatID<-as.integer(rownames(hruData))
write.csv(hruData,"HruBounds/NatID.csv",quote=F,row.names=F)

# 2 - Get the list of gages and calibration region ID
gageSet<-read.csv("HRUdata/HESS-2015-325/dataBYgage.csv",colClasses=c(rep("character",3),c(rep("numeric",14))))
# Remove rows that have NA values
row.has.na <- apply(gageSet, 1, function(x){any(is.na(x))})
gageSet <- gageSet[!row.has.na,]
gageInfo<-gageSet[,1:3]

count=1
for (region in regions){
  print (count)
  print(region)
  # Read in hrus-> gages file
  test<-scan(paste("HRUdata/hru2gage_contrib/",region,"_hru_cont.txt",sep=""),what=character(),nlines=287)
  # For each gage find the position by greppting
  cGages<-unique(gageInfo$STAID)

  # empty data frame to bind data with
  emptyDF<-as.data.frame(matrix(0,nrow=numHrus[count],ncol=2))
  colnames(emptyDF)<-c("hru","CalReg")
  emptyDF$Gages<-"empty"
  emptyDF$hru<-rownames(emptyDF)

  for (cGage in cGages){
    if (cGage %in% test){
      pos<-grep(cGage,test)
      numHru<-as.integer(test[pos+1])
      #print(numHru)
      hruIDs<-as.integer(test[(pos+2):(pos+1+numHru)])
      emptyDF[c(hruIDs),"Gages"]=cGage
    }
    # Assign gages calibrations to HRUs
    write.csv(emptyDF,paste("HruBounds/Hru_Assign/",region,"_ErrorGroup.csv",sep=""),quote=F,row.names=F)
  }
  count<-count+1
}

# Bind data frames together to create one file for CONUS
gageFiles<-list.files("HruBounds/Hru_Assign")
gageFile1<-read.csv("HruBounds/Hru_Assign/r01_ErrorGroup.csv")
for (gageFile in gageFiles[2:length(gageFiles)]){
  gageFile<-read.csv(paste("HruBounds/Hru_Assign/",gageFile,sep=""))
  gageFile1<-rbind(gageFile1,gageFile)
}

gageFile1$NATID<-rownames(gageFile1)
write.csv(gageFile1,"HruBounds/Nat_Bounds.csv",quote=F,row.names=F)

