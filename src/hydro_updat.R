## 
## North Biscayune Bay
##
## Code was compiled by Paul Julian
## contact info: pau.julian@floridadep.gov/pauljulianphd@gmail.com

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
# Data Wrangling
library(AnalystHelper);
library(plyr)
library(dataRetrieval)
library(reshape)

# GIS libraries 
# library(rgdal)
# library(rgeos)
# library(raster)

# analysis 
library(zoo)

# Paths
wd="C:/Julian_LaCie/Work/BiscayneBay"
paths=c(paste0(wd,"/Exports/"),paste0(wd,"/Plots/"),paste0(wd,"/Data/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]

GIS.path="C:/Julian_LaCie/_GISData"

# Helper variables
# nad83.pro=CRS("+init=epsg:4269")
# utm17=CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# -------------------------------------------------------------------------
# ogrListLayers(paste0(GIS.path,"/AHED_release/AHED_20171102.gdb"))
# 
# structs=spTransform(readOGR(paste0(GIS.path,"/AHED_release/AHED_20171102.gdb"),"STRUCTURE"),utm17)
# wmd.mon=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),utm17)
# subset(wmd.mon,SITE%in%c("S26","S27","S28","S29")&ACTIVITY_S=="Flow"&STATUS=="Active")@data

# Data --------------------------------------------------------------------
dates=date.fun(c("2000-01-01",as.character(Sys.Date())))

q.sites=data.frame(SITE=c(paste0("S",26:29),"S31"),DBKEY=c(91469:91472,91477))
q.dat=data.frame()
for(i in 1:nrow(q.sites)){
tmp=DBHYDRO_daily(dates[1],dates[2],q.sites$DBKEY[i])
tmp$DBKEY=as.character(q.sites$DBKEY[i])
q.dat=rbind(q.dat,tmp)
print(i)
}
q.dat=merge(q.dat,q.sites,"DBKEY")
q.dat$Date.EST=date.fun(q.dat$Date)
q.dat$WY=WY(q.dat$Date.EST)
q.dat$DOWY=hydro.day(q.dat$Date.EST)
q.dat$Data.Value=with(q.dat,ifelse(Data.Value<0,NA,Data.Value))
# q.dat$Data.Value=cfs.to.acftd(q.dat$Data.Value)

q.dat$cum.Q=with(q.dat,ave(Data.Value,paste(SITE,WY),FUN=function(x)cumsum(ifelse(is.na(x)==T,0,x))))

rf.sites=data.frame(SITE=paste0("S",26:29),DBKEY=c("K8672","K8673","K8619","K8620"))
rf.dat=data.frame()
for(i in 1:nrow(rf.sites)){
  tmp=DBHYDRO_daily(dates[1],dates[2],rf.sites$DBKEY[i])
  tmp$DBKEY=as.character(rf.sites$DBKEY[i])
  rf.dat=rbind(rf.dat,tmp)
  print(i)
}
rf.dat=merge(rf.dat,rf.sites,"DBKEY")
rf.dat$Date.EST=date.fun(rf.dat$Date)
rf.dat$WY=WY(rf.dat$Date.EST)
rf.dat$DOWY=hydro.day(rf.dat$Date.EST)

rf.dat$cum.Q=with(rf.dat,ave(Data.Value,paste(SITE,WY),FUN=function(x)cumsum(ifelse(is.na(x)==T,0,x))))

plot(cum.Q~DOWY,subset(q.dat,SITE=="S26"&WY==2020))
plot(cum.Q~DOWY,subset(q.dat,SITE=="S26"&WY==2021))

plot(cum.Q~DOWY,subset(rf.dat,SITE=="S26"&WY==2020))
plot(cum.Q~DOWY,subset(rf.dat,SITE=="S26"&WY==2021))

range(subset(q.dat,WY==2021)$Data.Value,na.rm=T)
#tiff(filename=paste0(plot.path,"NBB_hydro.tiff"),width=6.5,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1.5,3,0.25,0.25),oma=c(3,3,1.5,1));
layout(matrix(1:8,2,4),heights=c(0.5,1))

for(i in 1:nrow(q.sites)){
xlim.val=date.fun(c("2020-05-01","2021-05-01"));xmaj=seq(xlim.val[1],xlim.val[2],"6 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Data.Value~Date.EST,rf.dat,ylim=rev(ylim.val),xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=rev(ymaj),v=xmaj,lty=3,col="grey")
with(subset(rf.dat,SITE==rf.sites$SITE[i]),segments(Date.EST,0,Date.EST,Data.Value,col="dodgerblue1",lwd=2))
# axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,rev(ymaj),rev(ymin),rev(ymaj));box(lwd=1)
if(i==1){mtext(side=2,line=2.5,"Rainfall\n(inches)")}
mtext(side=3,rf.sites$SITE[i])

max.yval=round(ceiling(max(subset(q.dat,WY==2021)$Data.Value)+max(subset(q.dat,WY==2021)$Data.Value)*0.2),-3)
ylim.val=c(0,max.yval);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Data.Value~Date.EST,q.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(q.dat,SITE==q.sites$SITE[i]),shaded.range(Date.EST,rep(0,length(Date.EST)),ifelse(is.na(Data.Value)==T,0,Data.Value),"indianred1",lty=1,lwd=0.5))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
if(i==1){mtext(side=2,line=3.5,"Discharge (ft\u00B3s\u207B\u00B9)")}
}
mtext(side=1,line=1,outer=T,"Date (Month-Year)")
dev.off()

# USGS data
upstream.S28=readNWISdata(sites="02286328",startDate=dates[1],endDate=dates[2])
upstream.S28=renameNWISColumns(upstream.S28)
attributes(upstream.S28)
# 00480 salinity parameter code
upstream.S28$dateTime=date.fun(upstream.S28$dateTime)
upstream.S28$WY=WY(upstream.S28$dateTime)

plot(Wtemp~dateTime,upstream.S28,xlim=xlim.val)
plot(SpecCond~dateTime,upstream.S28,xlim=xlim.val,ylim=c(0,2000))
plot(X_00480~dateTime,upstream.S28,ylim=c(0,2))

#tiff(filename=paste0(plot.path,"S28_Spc.tiff"),width=6.5,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1.5,3,0.25,3.5),oma=c(2,1,2,0.5));

xlim.val=date.fun(c("2020-05-01","2021-05-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(25,35);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(Wtemp~dateTime,upstream.S28,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(upstream.S28,lines(dateTime,Wtemp,col="indianred1",lwd=2))
axis_fun(2,ymaj,ymin,ymaj)
mtext(side=2,line=2.5,"Water Temp (\u2103)")

max.yval=with(subset(upstream.S28,WY==2021),round(ceiling(max(SpecCond,na.rm=T)+max(SpecCond,na.rm=T)*0.2),-3))
ylim.val=c(0,max.yval);by.y=max.yval/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(new=T);plot(SpecCond~dateTime,upstream.S28,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
with(upstream.S28,lines(dateTime,SpecCond,col="dodgerblue1",lwd=2))
axis_fun(4,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%y"));box(lwd=1)
mtext(side=4,line=2.5,"Specific Conductivity (\u03BCS cm\u207B\u00B9)")
mtext(side=1,line=2,"Date (Month-Year)")
mtext(side=3,adj=0,"USGS Site: 02286328\n (Upstream of S-28)")
legend("topright",legend=c("Water Temp.","Spec. Cond."),
       lty=c(1,1),lwd=c(2,2),
       col=c("indianred","dodgerblue1"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

#tiff(filename=paste0(plot.path,"S26_quickexample.tiff"),width=6.5,height=6,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1.5,3,0.25,0.25),oma=c(3,3,1.5,0.5));
layout(matrix(1:3,3,1),heights=c(0.5,1,1))

xlim.val=date.fun(c("2020-05-01","2021-05-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Data.Value~Date.EST,rf.dat,ylim=rev(ylim.val),xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=rev(ymaj),v=xmaj,lty=3,col="grey")
with(subset(rf.dat,SITE=="S26"),segments(Date.EST,0,Date.EST,Data.Value,col="dodgerblue1",lwd=2))
# axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,rev(ymaj),rev(ymin),rev(ymaj));box(lwd=1)
mtext(side=2,line=2.5,"Rainfall\n(inches)")
mtext(side=3,"S26")

ylim.val=c(0,4000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Data.Value~Date.EST,q.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(q.dat,SITE=="S26"),shaded.range(Date.EST,rep(0,length(Date.EST)),ifelse(is.na(Data.Value)==T,0,Data.Value),"indianred1",lty=1))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(2,ymaj,ymin,format(ymaj/1e4));box(lwd=1)
mtext(side=1,line=2,"Date")
mtext(side=2,line=2.5,"Discharge (x10\u00B3 Ac-Ft d\u207B\u00B9)")

par(mar=c(1.5,3,2,0.25))
xlim.val=c(0,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
ylim.val=c(0,700000);by.y=200000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Data.Value~DOWY,q.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
tmp=ddply(subset(q.dat,SITE=="S26"&WY%in%seq(2001,2020,1)),"DOWY",summarise,mean.val=mean(cum.Q,na.rm=T),med.val=median(cum.Q,na.rm=T),Q10=quantile(cum.Q,na.rm=T,probs=0.1),Q90=quantile(cum.Q,na.rm=T,probs=0.9))
#with(subset(tmp,DOWY<366),lines(DOWY,Q10,lwd=1.25,lty=2,col="grey"))
#with(subset(tmp,DOWY<366),lines(DOWY,med.val,lwd=1.25,col="grey"))
#with(subset(tmp,DOWY<366),lines(DOWY,Q90,lwd=1.25,lty=2,col="grey"))
with(subset(tmp,DOWY<366),shaded.range(DOWY,med.val,Q90,"grey",lty=1))
with(subset(tmp,DOWY<366),shaded.range(DOWY,Q10,med.val,"grey",lty=1))
with(subset(q.dat,SITE=="S26"&WY==2021),lines(DOWY,cum.Q,col="indianred1",lwd=2))
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj/1e4);box(lwd=1)
mtext(side=1,line=2,"Day of Water Year")
mtext(side=2,line=2.5,"Cumulative Discharge\n(x10\u00B3 Ac-Ft d\u207B\u00B9)")
legend("topleft",legend=c("WY2001-2020\nQ90 - Q10 range","Median","WY2021"),
       pch=c(22,NA,NA),lty=c(NA,1,1),lwd=c(0.1,1.25,2),
       col=c("grey","grey","indianred"),
       pt.bg=c(adjustcolor("grey",0.25),NA,NA),
       pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()



# Basin vs WCA/Lake -------------------------------------------------------

q.dat$month=format(q.dat$Date.EST,"%m")
q.dat$CY=format(q.dat$Date.EST,"%Y")
q.dat$monCY=with(q.dat,date.fun(paste(CY,month,"01",sep="-")))

q.dat.xtab=cast(q.dat,monCY+WY~SITE,value="Data.Value",fun.aggregate = function(x)sum(cfs.to.acftd(x)))
q.dat.xtab$BB.Q=rowSums(q.dat.xtab[,c("S26","S27","S28","S29")],na.rm=T)
q.dat.xtab$Basin.Q=with(q.dat.xtab,BB.Q-S31)
q.dat.xtab[is.na(q.dat.xtab)]<-0

xlim.val=date.fun(c("2015-05-01","2021-05-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,15e4);by.y=5e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=hcl.colors(4, "Viridis", rev = T,alpha=1)
# tiff(filename=paste0(plot.path,"BBAP_discharge.tiff"),width=7,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(1,1.5,0.75,0.25));
layout(matrix(1:2,2,1,byrow=T),heights = c(1,0.25))

plot(BB.Q~monCY,q.dat.xtab,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(q.dat.xtab,shaded.range(monCY,rep(0,length(monCY)),S26,cols[1],lty=1,lwd=0.5))
with(q.dat.xtab,shaded.range(monCY,S26,S26+S27,cols[2],lty=1,lwd=0.5))
with(q.dat.xtab,shaded.range(monCY,S26+S27,S26+S27+S28,cols[3],lty=1,lwd=0.5))
with(q.dat.xtab,shaded.range(monCY,S26+S27+S28,S26+S27+S28+S29,cols[4],lty=1,lwd=0.5))
with(q.dat.xtab,pt_line(monCY,S31,2,"black",1,21,"grey",cex=0.75))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj/10e3))
box(lwd=1)
mtext(side=2,line=2,"Discharge\n(x10\u00B3 Acre-Ft Month\u207B\u00B9)")
mtext(side=1,line=1.5,"Date (Month-Year)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA);
legend(0.5,0,legend=c("S26","S27","S28","S29","S31"),
       pch=c(rep(22,4),21),lty=c(NA),lwd=0.1,
       col=c(cols,"black"),pt.bg=c(adjustcolor(cols,0.25),"grey"),
       pt.cex=1.5,ncol=5,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()