library (moments)
library(boot)
library(dplyr)
library(ggplot2)

library(extrafont)
loadfonts(device="win")
fonts()

setwd("d:/abock/Water_Balance/MWBM_errors")
source("Func/avWidth.R")
Q1<-0.005
Q2<-0.995
Level<-"Q99"

# Build a list of groups, each folder contains numerous files
Groups=list.dirs(path = "Groups", full.names = FALSE, recursive = F)

for (Group in Groups){
  print(Group)
  grpTS<-read.csv(paste("Timeseries/",Group,"_",Level,sep=""),header=T,colClasses=c(rep("character",3),rep("numeric",5)))
  # calculcate average width - not sure of wisdom of dividing by the sim Z-scores
  grpTS$AW<-grpTS[,paste("sim",Q2,sep="")]-grpTS[,paste("sim",Q1,sep="")] #grpTS$AW<-(grpTS$sim0975-grpTS$sim025)#/grpTS$sim
  # Get unique list of gages
  gages<-unique(grpTS$GAGE)

  #Calculate the average width and write output
  AWDframe<-do.call(rbind,lapply(gages,avWidth,grpTS,Group,Q1,Q2,Level)) #singlebound<-avWidth("01073000",grpTS,Group,Q1,Q2)
  write.csv(AWDframe,paste("CW/",Group,"_",Level,sep=""),quote=F,row.names=F)
  
  AWIMedian<-AWDframe %>% group_by(MONTH) %>% summarise (AWI=median(AWI))
  CRMedian<-AWDframe %>% group_by(MONTH) %>% summarise (CR=median(CR))
  
  MnMonthlyZ<-read.table(paste("Groups/",Group,"/verMN_MONTHLYq",sep=""),colClasses=c(rep("character",2),rep("numeric",5)))
  colnames(MnMonthlyZ)<-c("gageNum","STAID","Month","OBS","SIM","OBSz","SIMz")
  Median<-MnMonthlyZ %>% group_by(Month) %>% summarise (median=median(OBSz))
  Median$AWI<-AWIMedian$AWI
  Median$CR<-CRMedian$CR
  write.csv(Median,paste("CW/",Group,"_Median_",Level,sep=""),quote=F)
  
  png(paste("CW/",Group,"_",Level,".png",sep=""),w=500,h=500)
  p<-ggplot(AWDframe, aes(x=factor(AWDframe$MONTH), y=AWI)) + geom_boxplot() + labs(x="Month",y="AWI")
  p<-p+stat_summary(fun.y=mean,shape=1,col='red',geom='point')
  print(p)
  dev.off()

  ## set up some fake test data
  cMonths<-c(1,2,3,4,5,6,7,8,9,10,11,12)
  
  png(paste("CW/",Group,"_AWI_",Level,".png",sep=""),w=5,h=5,units="in",res=150)
  ## add extra space to right margin of plot within frame
  par(mar=c(5, 4, 4, 6) + 0.1)
  par(family="Arial Narrow")
  
  ## Plot first set of data and draw its axis
  boxplot(AWDframe$AWI~AWDframe$MONTH, pch=16, axes=FALSE, ylim=c(0.2,0.8), ylab="",
      main="Average Width Index and Streamflow",xlim=c(0.5,12.5))
  axis(2, ylim=c(0.2,0.8),col="black",las=1)  ## las=1 makes horizontal labels
  #axis(2, ylim=c(-0.2,0.8),col="black",las=1)  ## las=1 makes horizontal labels
  mtext("Average Width Index",side=2,line=2.5)
  ## Draw the time axis
  axis(1,month.abb[cMonths],at=cMonths,las=2)
  mtext("Month",side=1,col="black",line=2.5)
  box()
  
  ## Allow a second plot on the same graph
  par(new=TRUE)
  
  ## Plot first set of data and draw its axis
  plot(cMonths, Median$median, axes=FALSE, xlab="", ylab="",col="blue",type="l",lty=1,lwd=1.5,xlim=c(0.5,12.5))
  axis(4,col="blue",col.axis="blue",las=1)
  mtext("Streamflow (Z-score)",side=4,line=2.5,col="blue")## las=1 makes horizontal labels

  
  ## Add Legend
  legend("bottomleft",legend=c("Streamflow","AWI"),
         text.col=c("blue","black"),lwd=c(2,2),col=c("blue","black"),cex=0.75)
  dev.off()
}


#http://stackoverflow.com/questions/15514255/add-a-line-from-different-result-to-boxplot-graph-in-ggplot2
#http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
#http://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right
#http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/
