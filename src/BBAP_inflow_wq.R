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
library(openxlsx)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)

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


utm17=CRS(SRS_string ="EPSG:26917")
wgs84=CRS(SRS_string = "EPSG:4326")
# GIS ---------------------------------------------------------------------
BBWQ_DERM=spTransform(readOGR(paste0(wd,"/GIS/BBWQ_Monitoring_Stations.kml"),"BBWQ_Monitoring_Stations"),utm17)

AP=spTransform(readOGR(paste0(GIS.path,"/FDEP"),"AquaticPreserves"),utm17)
BBAP=subset(AP,SHORT_NAME=="Biscayne Bay")

wmd.monitoring=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),utm17)
wq.mon=subset(wmd.monitoring,ACTIVITY_S=="Surface Water Grab")

shore=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Shoreline"),utm17)
canals=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)
basins=spTransform(readOGR(paste0(GIS.path,"/AHED_release/AHED_20171102.gdb"),"WATERSHED"),utm17)
roads=spTransform(readOGR(paste0(GIS.path,"/FDOT"),"FDOT_Roads_SFWMDClip"),utm17)

# tmp.path="C:/Users/julian_p/Florida Department of Environmental Protection/Office of Resilience and Coastal Protection (ORCP) - SE - BBAP - Analyses/Spatial Information/shp"
# writeOGR(BBAP,tmp.path,"BBAP","ESRI Shapefile")
# writeOGR(shore,tmp.path,"SFWMD_shoreline","ESRI Shapefile")


# Data --------------------------------------------------------------------
dates=date.fun(c("2000-05-01",as.character(Sys.Date())))

# Hydro
q.sites=data.frame(SITE=c(paste0("S",26:29),"S31"),DBKEY=c(91469:91472,91477),derm.wq=c("MR07","LR06","BS04","SK02",NA))
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


# Water Quality
qaqc.flag=read.csv(paste0(data.path,"DERM/DERM_quals_FATAL.csv"))
qaqc.flag=qaqc.flag[,c("REM","FATAL")]
derm.dat=read.csv(paste0(data.path,"DERM/BBWQall_DERM_Sent20171117.csv"))
derm.dat$DATE=date.fun(as.character(derm.dat$DATE),form="%m/%d/%Y")

derm.dat$REM=as.character(derm.dat$REM)
# quals=ddply(derm.dat,"REM",summarise,N.val=N.obs(REM))
# write.csv(quals,paste0(export.path,"DERM_quals.csv"),row.names = F)
# str=strsplit(quals$REM,",")
# str1=data.frame(q1=sapply(str,"[",1),q2=sapply(str,"[",2),q3=sapply(str,"[",3),q4=sapply(str,"[",4))
derm.dat=merge(derm.dat,qaqc.flag,"REM",all.x=T)
derm.param=data.frame(PARAMETER=c("Phosphorus, Total (TP)","Ortho Phosphate (OPO4)","Nitrate/Nitrite (NOX)","Total Kjeldahl Nitrogen (TKN)","Ammonia Nitrogen (Dissolved)","Nitrate/Nitrite (NOx-Dissolved)"),
                      param=c("TP","SRP","NOx","TKN","NH4","NOx"))
derm.dat=merge(derm.dat,derm.param,"PARAMETER")

# Dataset explore
unique(derm.dat$DilutionFactor)
unique(derm.dat$PARAMETER)
# unique(derm.dat$SampleMatrix)
# unique(derm.dat$Depth)
head(derm.dat)
with(derm.dat,sum(RESULT<MDL,na.rm=T))
subset(derm.dat,RESULT<MDL)

# more QAQC
derm.dat$HalfMDL=with(derm.dat,ifelse(RESULT<=MDL,MDL,RESULT))
unique(subset(derm.dat,STATOIN%in%q.sites$derm.wq)$DepthCode)

derm.dat.xtab=data.frame(cast(subset(derm.dat,FATAL!="Y"),STATOIN+DATE+DepthCode~param,value="HalfMDL",mean))
derm.dat.xtab$STATOIN=as.character(derm.dat.xtab$STATOIN)
derm.dat.xtab$WY=WY(derm.dat.xtab$DATE)
derm.dat.xtab$TN=NA
derm.dat.xtab$TN=with(derm.dat.xtab,TN_Combine(NOx,TKN,TN))
derm.dat.xtab$hydroseason=with(derm.dat.xtab,FL.Hydroseason(DATE))
unique(derm.dat.xtab$STATOIN)

plot(TP~DATE,subset(derm.dat.xtab,STATOIN==q.sites$derm.wq[1]))
plot(TN~DATE,subset(derm.dat.xtab,STATOIN==q.sites$derm.wq[3]))

TP.samp=cast(derm.dat.xtab,STATOIN+WY~hydroseason,value="TP",fun.aggregate = function(x)N.obs(x))
TP.samp$TSamp=rowSums(TP.samp[,c("A_Wet",'B_Dry')],na.rm=T)
TP.samp$TP.screen=with(TP.samp,ifelse(TSamp>4&A_Wet>=1&B_Dry>=1,1,0))

derm.dat.xtab=merge(derm.dat.xtab,TP.samp[,c("STATOIN","WY","TP.screen")],c("STATOIN","WY"))

derm.dat.TP.WY=ddply(subset(derm.dat.xtab,TP.screen==1),c("STATOIN","WY"),summarise,TP.GM.ugL=exp(mean(log(TP*1000),na.rm=T)))

subset(derm.dat.xtab,STATOIN==q.sites$derm.wq[1])
tail(subset(derm.dat.xtab,STATOIN==q.sites$derm.wq[1]),50L)

q.wq=merge(q.dat,derm.dat.xtab,by.x=c("Date.EST","derm.wq","WY"),by.y=c("DATE","STATOIN","WY"),all.x=T)
q.wq=q.wq[order(q.wq$derm.wq,q.wq$Date.EST),]
q.wq$TP.int=with(q.wq,ave(TP,derm.wq,FUN=function(x) dat.interp(x)))
q.wq$TP.loadkg=with(q.wq,Load.Calc.kg(Data.Value,TP.int))

q.wq$TN.int=with(q.wq,ave(TN,derm.wq,FUN=function(x) dat.interp(x)))
q.wq$TN.loadkg=with(q.wq,Load.Calc.kg(Data.Value,TN.int))


plot(TN.loadkg~Date.EST,subset(q.wq,SITE=="S26"))
plot(TP.loadkg~Date.EST,subset(q.wq,SITE=="S27"))
plot(TP.loadkg~Date.EST,subset(q.wq,SITE=="S28"))

plot(TP.loadkg~Date.EST,subset(q.wq,SITE=="S29"))
plot(Data.Value~Date.EST,subset(q.wq,SITE=="S29"))
plot(TP~Date.EST,subset(q.wq,SITE=="S29"))

WY.load=ddply(subset(q.wq,WY%in%seq(2001,2017,1)),c("SITE","WY"),summarise,
              TFlow=sum(Data.Value,na.rm=T),
              TPLoad=sum(TP.loadkg,na.rm=T),
              TNLoad=sum(TN.loadkg,na.rm = T))
WY.load$TP.FWM=with(WY.load,(TPLoad*1e6)/(TFlow*1233481.84))*1000
WY.load$TN.FWM=with(WY.load,(TNLoad*1e6)/(TFlow*1233481.84))



cols=hcl.colors(4,alpha = 0.5)

plot(TPLoad~WY,WY.load,ylim=c(0,25e3))
with(subset(WY.load,SITE=="S26"),lines(WY,TPLoad,col=cols[1],lwd=2))
with(subset(WY.load,SITE=="S27"),lines(WY,TPLoad,col=cols[2],lwd=2))
with(subset(WY.load,SITE=="S28"),lines(WY,TPLoad,col=cols[3],lwd=2))
with(subset(WY.load,SITE=="S29"),lines(WY,TPLoad,col=cols[4],lwd=2))

tmp=cast(WY.load,WY~SITE,value="TPLoad")

#png(filename=paste0(plot.path,"StructureLoad.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
#tiff(filename=paste0(plot.path,"StructureLoad.tiff"),width=6.5,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1.5,1.5,0.1,0.1),oma=c(2,2,0.5,0.1));
layout(matrix(1:2,1,2,byrow = T),widths = c(1,0.25))

ylim.val=c(0,25e3);by.y=10e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2001,2017);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
x=barplot(t(tmp[,paste0("S",26:29)]),space=0,col=cols,axes=F,ylim=ylim.val)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj*0.001);box(lwd=1)
mtext(side=2,line=2,"TP Load (tons yr\u207B\u00B9)")
mtext(side=1,line=2,"Water Year (May - Apirl)")

plot(0:1,0:1,axes=F,ann=F,type="n")
legend(0.5,0.5,legend=c(rev(paste0("S",26:29))),
       pch=22,pt.bg=rev(cols),lty=0,
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

#png(filename=paste0(plot.path,"StructureFWM.png"),width=6.5,height=3.5,units="in",res=200,type="windows",,bg="white")
#tiff(filename=paste0(plot.path,"StructureFWM.tiff"),width=6.5,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(1.5,1.5,0.1,0.1),oma=c(2,2,0.5,0.1));
layout(matrix(1:2,1,2,byrow = T),widths = c(1,0.25))

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2001,2017);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(TP.FWM~WY,WY.load,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(WY.load,SITE=="S26"),pt_line(WY,TP.FWM,2,cols[1],2,21,cols[1],1.5))
with(subset(WY.load,SITE=="S27"),pt_line(WY,TP.FWM,2,cols[2],1,21,cols[2],1.5))
with(subset(WY.load,SITE=="S28"),pt_line(WY,TP.FWM,2,cols[3],1,21,cols[3],1.5))
with(subset(WY.load,SITE=="S29"),pt_line(WY,TP.FWM,2,cols[4],1,21,cols[4],1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"TP FWN (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=2,"Water Year (May - April)")

plot(0:1,0:1,axes=F,ann=F,type="n")
legend(0.5,0.5,legend=c(rev(paste0("S",26:29))),
       pch=21,pt.bg=rev(cols),lty=0,
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


site.dat=data.frame(BBWQ_DERM$Name,coordinates(BBWQ_DERM)[,1:2])
colnames(site.dat)=c("STATOIN","UTMX","UTMY")
site.dat=SpatialPointsDataFrame(site.dat[,c("UTMX","UTMY")],data=site.dat,proj4string=utm17)

TP.WY2017.shp=merge(site.dat,subset(derm.dat.TP.WY,WY==2017),"STATOIN",all.x=T)
TP.WY2010.shp=merge(site.dat,subset(derm.dat.TP.WY,WY==2010),"STATOIN",all.x=T)
library(classInt)
int=c(0,10,25,50,100,200,300)

# cols=heat.colors(length(int)-1,alpha=0.5)

#png(filename=paste0(plot.path,"WY2017TPGM.png"),width=5,height=6.5,units="in",res=200,type="windows",bg="white")
#tiff(filename=paste0(plot.path,"WY2017TPGM.tiff"),width=5,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2,byrow = T),widths = c(1,0.5))
col.val=findInterval(subset(TP.WY2017.shp,is.na(TP.GM.ugL)==F)$TP.GM.ugL,int)
cols=hcl.colors(length(int),palette = "Viridis",alpha = 0.5)

bbox.lims=bbox(TP.WY2017.shp)
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05)
plot(roads,col="grey",lty=2,add=T)
plot(canals,col="dodgerblue1",add=T)
plot(BBAP,add=T,col=adjustcolor("grey",0.5),border="grey")
plot(subset(TP.WY2017.shp,is.na(TP.GM.ugL)==F),pch=21,bg=cols[col.val],add=T,cex=1.5,lwd=0.01)
plot(subset(TP.WY2017.shp,is.na(TP.GM.ugL)==T),pch=21,bg=NA,add=T,cex=1.5,lwd=0.01)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)

plot(0:1,0:1,ann=F,axes=F,type="n")
labs=NA
text(x=0,y=0.95,"WY2017\nTP Geometric Mean\n(\u03BCg L\u207B\u00B9)",adj=0,cex=0.75)
text(x=0.5, y = c(0.5,0.90), labels = c("<10",">200"),cex=0.75,pos=4)
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,0.1, 0.5, 0.5,0.90)

legend(0.5,0.4,legend=c("Insufficient Data","BBAP","Canals","Roads"),
       pch=c(21,22,NA,NA),lty=c(NA,NA,1,2),lwd=c(0.1,0.1,1,1),
       col=c("black","grey","dodgerblue1","grey"),pt.bg=c(NA,adjustcolor("grey",0.5),NA,NA),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.00,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,title.adj = 0)
dev.off()

#png(filename=paste0(plot.path,"WY2010TPGM.png"),width=5,height=6.5,units="in",res=200,type="windows",bg="white")
#tiff(filename=paste0(plot.path,"WY2010TPGM.tiff"),width=5,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2,byrow = T),widths = c(1,0.5))
col.val=findInterval(subset(TP.WY2010.shp,is.na(TP.GM.ugL)==F)$TP.GM.ugL,int)
cols=hcl.colors(length(int),palette = "Viridis",alpha = 0.5)

bbox.lims=bbox(TP.WY2010.shp)
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05)
plot(roads,col="grey",lty=2,add=T)
plot(canals,col="dodgerblue1",add=T)
plot(BBAP,add=T,col=adjustcolor("grey",0.5),border="grey")
plot(subset(TP.WY2010.shp,is.na(TP.GM.ugL)==F),pch=21,bg=cols[col.val],add=T,cex=1.5,lwd=0.01)
plot(subset(TP.WY2010.shp,is.na(TP.GM.ugL)==T),pch=21,bg=NA,add=T,cex=1.5,lwd=0.01)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)

plot(0:1,0:1,ann=F,axes=F,type="n")
labs=NA
text(x=0,y=0.95,"WY2010\nTP Geometric Mean\n(\u03BCg L\u207B\u00B9)",adj=0,cex=0.75)
text(x=0.5, y = c(0.5,0.90), labels = c("<10",">200"),cex=0.75,pos=4)
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,0.1, 0.5, 0.5,0.90)

legend(0.5,0.4,legend=c("Insufficient Data","BBAP","Canals","Roads"),
       pch=c(21,22,NA,NA),lty=c(NA,NA,1,2),lwd=c(0.1,0.1,1,1),
       col=c("black","grey","dodgerblue1","grey"),pt.bg=c(NA,adjustcolor("grey",0.5),NA,NA),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.00,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,title.adj = 0)
dev.off()

## Spatial interpolation
library(fields)
library(raster)

BBAP.r=raster(BBAP)
res(BBAP.r)=250

bbox=extent(gBuffer(site.dat,width=1000))
bbox.poly=as(bbox,"SpatialPolygons")
proj4string(bbox.poly)=proj4string(site.dat)

DERM.r=raster(bbox.poly)
res(DERM.r)=250

TP.WY2017.shp2=subset(TP.WY2017.shp,is.na(TP.GM.ugL)==F)
# BBAP.DERM.sites=spatialEco::point.in.poly(TP.WY2017.shp,BBAP)@data
# BBAP.DERM.sites=subset(BBAP.DERM.sites,is.na(LONG_NAME)==F)$STATOIN
# BBAP.TP.WY2017.shp2=subset(TP.WY2017.shp,is.na(TP.GM.ugL)==F&STATOIN%in%BBAP.DERM.sites)

# Tension Spline
m=Tps(coordinates(TP.WY2017.shp2),TP.WY2017.shp2$TP.GM.ugL)
tps=interpolate(DERM.r,m)
tps.clp=mask(tps,BBAP)

plot(tps)
plot(BBAP,col=NA,add=T)
plot(site.dat,add=T,pch=21,bg=NA)

plot(tps.clp)

# IDW interpolation
library(gstat);# https://rspatial.org/raster/analysis/4-interpolation.html
gs=gstat(formula=TP.GM.ugL~1,locations=TP.WY2017.shp2)
idw=interpolate(DERM.r,gs)
idw.clp=mask(idw,BBAP)

plot(idw.clp)
plot(BBAP,col=NA,add=T)
plot(site.dat,add=T,pch=21,bg=NA)

library(tmap)
# tmap_mode("view")
tm_shape(idw)+tm_raster(alpha=0.5)
