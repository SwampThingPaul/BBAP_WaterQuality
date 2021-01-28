## 
## DERM and BBAP data
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
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
library(tmap)

library(fields)
library(spdep)

#Paths
wd="C:/Julian_LaCie/_GitHub/BBAP_WaterQuality"

paths=paste0(wd,c("/Plots/","/Exports/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
pro.GIS.path=paths[4]

GIS.path="C:/Julian_LaCie/_GISData"

utm17=CRS(SRS_string ="EPSG:26917")
wgs84=CRS(SRS_string = "EPSG:4326")

# Molecular weights
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107
H.mw=1.00784
O.mw=15.999


NH3.mw=(N.mw+(H.mw*3))
PO4.mw=P.mw+(O.mw*4)

tmap_mode("view")
# -------------------------------------------------------------------------
### GIS and Data are stored locally (not in this repo)

## GIS 
AP=spTransform(readOGR(paste0(GIS.path,"/FDEP"),"AquaticPreserves"),wkt(utm17))
BBAP=subset(AP,SHORT_NAME=="Biscayne Bay")

wmd.struct=spTransform(readOGR(paste0(GIS.path,"/AHED_release/AHED_20171102.gdb"),"STRUCTURE"),wkt(utm17))
BBAP_struct=subset(wmd.struct,NAME%in%c("S20F","S20G","S21A","S21","S123","S22","S26","S25A","S25B","S27","S28","S29"))

wmd.monitoring=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))
wq.mon=subset(wmd.monitoring,ACTIVITY_S=="Surface Water Grab")

shore=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Shoreline"),wkt(utm17))
canals=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),wkt(utm17))
basins=spTransform(readOGR(paste0(GIS.path,"/AHED_release/AHED_20171102.gdb"),"WATERSHED"),wkt(utm17))
roads=spTransform(readOGR(paste0(GIS.path,"/FDOT"),"FDOT_Roads_SFWMDClip"),wkt(utm17))

nnc.regions=spTransform(readOGR(pro.GIS.path,"SFL_NNC"),wkt(utm17))

bb.nnc=subset(nnc.regions,ESTUARY=="Biscayne Bay"&ESTUARY_SE%in%paste0("ENRH",3:9))
bb.nnc.buf=gBuffer(bb.nnc,width=1000,id=1)
bb.nnc.buf=SpatialPolygonsDataFrame(bb.nnc.buf,data.frame(ID=1))

## FDEP Station Data
storet.loc=read.table(paste0(data.path,"DERM/STORET/Station_Results.txt"),sep="|",header=T)
spl=sapply(strsplit(as.character(storet.loc$Latitude),split="\\s+"),as.numeric)
storet.loc$Latitude= spl[1,]+spl[2,]/60+spl[3,]/3600
spl=sapply(strsplit(as.character(storet.loc$Longitude),split="\\s+"),as.numeric)
storet.loc$Longitude= (spl[1,]+spl[2,]/60+spl[3,]/3600)*-1

vars=c("Station.ID","Latitude","Longitude")
storet.loc=storet.loc[,vars]
## 
win.loc=read.table(paste0(data.path,"DERM/WIN/WIN_WAVES_JULIAN_P_20201201161203_36285.txt"),skip=5,sep="|",header=T)
win.loc$DEP.Longitude=win.loc$DEP.Longitude*-1
win.loc$Station.ID=win.loc$Monitoring.Location.ID
win.loc$Latitude=win.loc$DEP.Latitude
win.loc$Longitude=win.loc$DEP.Longitude
win.loc=win.loc[,vars]

site.locs=rbind(storet.loc,win.loc)
site.locs=site.locs[order(site.locs$Station.ID),]
site.locs=site.locs[duplicated(site.locs[,vars[1]])==F,]

site.locs=SpatialPointsDataFrame(site.locs[,c("Longitude","Latitude")],data=site.locs,proj4string=wgs84)
site.locs=spTransform(site.locs,wkt(utm17))

bb.site.locs=site.locs[bb.nnc.buf,]

# Water Quality -----------------------------------------------------------
dat.qual=data.frame(QUALIFIER=c(NA,"!","A","D","E","F","I","R","T","U","*","?","B","H","J","K","L","M","N","O","Q","V","Y","Z"),
                    FATALYN=c("N",rep("N",9),rep("Y",14)))

## STORET data (downloaded 2020-12-02; Org ID 21FLDADE)
## https://floridadep.gov/dear/watershed-services-program/content/winstoret
storet.dat1=read.table(paste0(data.path,"DERM/STORET/Water_Quality_Results_20160516_20170710.txt"),sep="|",header=T)
storet.dat2=read.table(paste0(data.path,"DERM/STORET/Water_Quality_Results_20140501_20160516.txt"),sep="|",header=T)
storet.dat3=read.table(paste0(data.path,"DERM/STORET/Water_Quality_Results_20120501_20140430.txt"),sep="|",header=T)
storet.dat4=read.table(paste0(data.path,"DERM/STORET/Water_Quality_Results_20090501_20120430.txt"),sep="|",header=T)

storet.dat=rbind(storet.dat1,storet.dat2,storet.dat3,storet.dat4)
storet.dat$Date.EST=date.fun(storet.dat$Act.Date,form="%m/%d/%Y")

range(storet.dat$Date.EST)
unique(storet.dat$Medium)
unique(storet.dat$Matrix)

# QA/QC qualifiers 
quals=as.character(unique(storet.dat$VQ))
spl=strsplit(quals,split="")
quals=data.frame(VQ=quals,q1=sapply(spl,"[",1),q2=sapply(spl,"[",2),q3=sapply(spl,"[",3),q4=sapply(spl,"[",4))
quals$Fatal=with(quals,ifelse(q1%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q2%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q3%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q4%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER,"Y","N"))
# subset(storet.dat,VQ=="BONE")
# unique(storet.dat$Act.Type)
# sort(unique(storet.dat$Result.Value))
# subset(storet.dat,Result.Value=="*Non-detect")
# subset(storet.dat,Result.Value==0)

# subset(storet.dat,Station.ID=="CD05"&Date.EST==date.fun("2015-12-10")&Characteristic=="BOD, Biochemical oxygen demand")
storet.dat$Result.Value=with(storet.dat,ifelse(Result.Value=="*Non-detect",MDL*-1,as.numeric(as.character(Result.Value))))
subset(storet.dat,is.na(Result.Value))

storet.dat$HalfMDL=with(storet.dat,ifelse(Act.Type=="Field Msr/Obs",Result.Value,
                                                ifelse(abs(Result.Value)<=MDL,MDL/2,abs(Result.Value))))
# sort(unique(storet.dat$HalfMDL))
storet.dat=merge(storet.dat,quals[,c("VQ","Fatal")],"VQ",all.x=T)
storet.dat=subset(storet.dat,Fatal=="N")

# unique(as.character(storet.dat$Result.Units))
# dput(unique(as.character(storet.dat$Characteristic)))

ddply(storet.dat,c("Characteristic","Result.Units"),summarise,n.val=N.obs(Station.ID))

# head(subset(storet.dat,Characteristic=="Phosphorus, orthophosphate as PO4"))
# head(subset(storet.dat,Characteristic=="Phosphorus, phosphate (PO4) as PO4"))
# head(subset(storet.dat,Characteristic=="Nitrogen, ammonia (NH3) as NH3"))
# head(subset(storet.dat,Characteristic=="Nitrogen, ammonia as N"))

storet.parameters=data.frame(Characteristic=c("Turbidity", "Total Suspended Solids (TSS)", "Nitrogen, ammonia (NH3) as NH3", 
  "Phosphorus as PO4", "Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N", 
  "Phosphorus, phosphate (PO4) as PO4","Chlorophyll a, free of pheophytin", 
  "Apparent Color","Nitrogen, Kjeldahl","Phosphorus, orthophosphate as PO4","pH", "Dissolved oxygen saturation", 
  "Dissolved oxygen (DO)", "Salinity", "Specific conductance","Temperature, water", "Nitrogen, ammonia as N"),
  param=c("Turb","TSS","NH4","TP","NOx","TP","Chla_c","Color","TKN","SRP","pH","DOSat","DO","Sal","SPC","Temp","NH4"))

storet.dat=merge(storet.dat,storet.parameters,"Characteristic")
# storet.dat$cen=with(storet.dat,ifelse(HalfMDL==MDL/2,1,0))
# P.cf=(1/PO4.mw)*(PO4.mw/P.mw)
# N.cf=(1/NH3.mw)*(NH3.mw/N.mw)
# storet.dat$HalfMDL=with(storet.dat,ifelse(cen==0&Characteristic%in%c("Phosphorus as PO4","Phosphorus, phosphate (PO4) as PO4","Phosphorus, orthophosphate as PO4"),
#                                           HalfMDL*P.cf,HalfMDL))
# head(subset(storet.dat,Characteristic=="Phosphorus, orthophosphate as PO4"))
# head(subset(storet.dat,Characteristic=="Phosphorus, phosphate (PO4) as PO4"))
# 
# head(subset(storet.dat,Characteristic=="Nitrogen, ammonia (NH3) as NH3"))
# storet.dat$HalfMDL=with(storet.dat,ifelse(cen==0&Characteristic=="Nitrogen, ammonia (NH3) as NH3",
#                                           HalfMDL*N.cf,HalfMDL))
# 
# head(subset(storet.dat,Characteristic=="Nitrogen, ammonia (NH3) as NH3"))
# head(subset(storet.dat,Characteristic=="Nitrogen, ammonia as N"))

storet.xtab=cast(storet.dat,Station.ID+Date.EST~param,value="HalfMDL",mean)
storet.xtab$WY=WY(storet.xtab$Date.EST)
storet.xtab$TN=NA
storet.xtab$TN=with(storet.xtab,TN_Combine(NOx,TKN,TN))
storet.xtab$DIN=with(storet.xtab,NH4+NOx)

storet.xtab$TPReversal=with(storet.xtab,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
storet.xtab$TNReversal=with(storet.xtab,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

# sum(storet.xtab$TNReversal,na.rm=T)
# sum(storet.xtab$TPReversal,na.rm=T)
# subset(storet.xtab,TPReversal==1)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,storet.xtab,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,storet.xtab,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

storet.xtab$TP=with(storet.xtab,ifelse(TPReversal==1,NA,TP))
storet.xtab$SRP=with(storet.xtab,ifelse(TPReversal==1,NA,SRP))
storet.xtab$TN=with(storet.xtab,ifelse(TNReversal==1,NA,TN))
storet.xtab$DIN=with(storet.xtab,ifelse(TNReversal==1,NA,DIN))

# TP.N=ddply(storet.xtab,c('WY','Station.ID'),summarise,N.val=N.obs(TP),min.date=min(Date.EST),max.date=max(Date.EST))

## WIN data (downloaded 2020-12-01)
win.dat=read.table(paste0(data.path,"DERM/WIN/WIN_WAVES_JULIAN_P_20201201160158_36281.txt"),skip=12,sep="|",header=T)
win.dat$Activity.Start.Date.Time=date.fun(as.character(win.dat$Activity.Start.Date.Time),form="%m/%d/%Y %H:%M:%S")
win.dat$Date.EST=date.fun(win.dat$Activity.Start.Date.Time)
win.dat$Station.ID=win.dat$Monitoring.Location.ID

# QA/QC qualifiers 
quals=as.character(unique(win.dat$Value.Qualifier))
spl=strsplit(quals,split="")
quals=data.frame(Value.Qualifier=quals,q1=sapply(spl,"[",1),q2=sapply(spl,"[",2),q3=sapply(spl,"[",3),q4=sapply(spl,"[",4))
quals$Fatal=with(quals,ifelse(q1%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q2%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q3%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER|
                                q4%in%subset(dat.qual,FATALYN=="Y")$QUALIFIER,"Y","N"))

# unique(win.dat$Matrix)
win.dat.clean=subset(win.dat,Matrix=="AQUEOUS-Surface Water")
win.dat.clean$HalfMDL=with(win.dat.clean,ifelse(Sample.Collection.Type=="Field Testing-Discrete",DEP.Result.Value.Number,
                                                ifelse(DEP.Result.Value.Number<=DEP.MDL,DEP.MDL/2,DEP.Result.Value.Number)))
win.dat.clean=merge(win.dat.clean,quals[,c("Value.Qualifier","Fatal")],"Value.Qualifier",all.x=T)
win.dat.clean=subset(win.dat.clean,Fatal=="N")

# dput(unique(win.dat.clean$DEP.Analyte.Name))
win.parameters=data.frame(DEP.Analyte.Name=c("Ammonia (N)", 
                                             "Chlorophyll a- uncorrected", 
                                             "Chlorophyll a, free of pheophytin","Chlorophyll a- corrected", "Dissolved Oxygen","Dissolved Oxygen Saturation", "Nitrate-Nitrite (N)", 
                                             "Nitrogen- Total Kjeldahl", "Orthophosphate (P)", "Phosphorus- Total", 
                                             "Specific Conductance", "Temperature, Water","pH","Salinity","Turbidity","Residues- Nonfilterable (TSS)","Color- Apparent"),
                          param=c("NH4","Chla","Chla_c","Chla_c","DO","DOSat","NOx","TKN","SRP","TP","SPC","Temp","pH","Sal","Turb","TSS","Color"))
win.dat.clean=merge(win.dat.clean,win.parameters,"DEP.Analyte.Name")

win.xtab=cast(win.dat.clean,Station.ID+Date.EST~param,value="HalfMDL",mean)
win.xtab$WY=WY(win.xtab$Date.EST)
win.xtab$TN=NA
win.xtab$TN=with(win.xtab,TN_Combine(NOx,TKN,TN))
win.xtab$DIN=with(win.xtab,NH4+NOx)

win.xtab$TPReversal=with(win.xtab,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
win.xtab$TNReversal=with(win.xtab,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

# sum(win.xtab$TNReversal,na.rm=T)
# sum(win.xtab$TPReversal,na.rm=T)
# subset(win.xtab,TPReversal==1)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,win.xtab,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,win.xtab,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

win.xtab$TP=with(win.xtab,ifelse(TPReversal==1,NA,TP))
win.xtab$SRP=with(win.xtab,ifelse(TPReversal==1,NA,SRP))
win.xtab$TN=with(win.xtab,ifelse(TNReversal==1,NA,TN))
win.xtab$DIN=with(win.xtab,ifelse(TNReversal==1,NA,DIN))

## 
storet.xtab$Chla=NA
# head(storet.xtab)
# head(win.xtab)
storet.xtab=storet.xtab[,names(win.xtab)]

dat.xtab=rbind(storet.xtab,win.xtab)
dat.xtab$season=FL.Hydroseason(dat.xtab$Date.EST)

# TP
TP.samp.size=cast(dat.xtab,Station.ID+WY~season,value="TP",fun.aggregate = function(x)N.obs(x))
TP.samp.size$TSamp=rowSums(TP.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
TP.samp.size$sea.screen=with(TP.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
# subset(TP.samp.size,sea.screen==1)

dat.xtab.TP=merge(dat.xtab,TP.samp.size[,c("Station.ID","WY","sea.screen")],c("Station.ID","WY"))

TP.AGM=ddply(subset(dat.xtab.TP,sea.screen==1),c("Station.ID","WY"),summarise,GM.TP=exp(mean(log(TP*1000),na.rm=T)))
# max(TP.AGM$WY)

TP.AGM2=merge(bb.site.locs@data,TP.AGM,"Station.ID")
TP.AGM2=SpatialPointsDataFrame(TP.AGM2[,c("Longitude","Latitude")],data=TP.AGM2,proj4string=wgs84)
TP.AGM2=spTransform(TP.AGM2,wkt(utm17))

## TN
TN.samp.size=cast(dat.xtab,Station.ID+WY~season,value="TN",fun.aggregate = function(x)N.obs(x))
TN.samp.size$TSamp=rowSums(TN.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
TN.samp.size$sea.screen=with(TN.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))
# subset(TN.samp.size,sea.screen==1)

dat.xtab.TN=merge(dat.xtab,TN.samp.size[,c("Station.ID","WY","sea.screen")],c("Station.ID","WY"))

TN.AGM=ddply(subset(dat.xtab.TN,sea.screen==1),c("Station.ID","WY"),summarise,GM.TN=exp(mean(log(TN),na.rm=T)))
# max(TN.AGM$WY)

TN.AGM2=merge(bb.site.locs@data,TN.AGM,"Station.ID")
TN.AGM2=SpatialPointsDataFrame(TN.AGM2[,c("Longitude","Latitude")],data=TN.AGM2,proj4string=wgs84)
TN.AGM2=spTransform(TN.AGM2,wkt(utm17))

## Chla
Chl.samp.size=cast(dat.xtab,Station.ID+WY~season,value="Chla_c",fun.aggregate = function(x)N.obs(x))
Chl.samp.size$TSamp=rowSums(Chl.samp.size[,c("A_Wet","B_Dry")],na.rm=T)
Chl.samp.size$sea.screen=with(Chl.samp.size, ifelse(A_Wet>0&B_Dry>0&TSamp>=4,1,0))

dat.xtab.Chl=merge(dat.xtab,Chl.samp.size[,c("Station.ID","WY","sea.screen")],c("Station.ID","WY"))

Chl.AGM=ddply(subset(dat.xtab.Chl,sea.screen==1),c("Station.ID","WY"),summarise,GM.Chla=exp(mean(log(Chla_c),na.rm=T)))
# max(TN.AGM$WY)

Chl.AGM2=merge(bb.site.locs@data,Chl.AGM,"Station.ID")
Chl.AGM2=SpatialPointsDataFrame(Chl.AGM2[,c("Longitude","Latitude")],data=Chl.AGM2,proj4string=wgs84)
Chl.AGM2=spTransform(Chl.AGM2,wkt(utm17))

# GAM ---------------------------------------------------------------------
library(mgcv)
library(gratia)
library(ggplot2)
library(viridis)
library(tmap)
library(Hmisc)

tmp=TP.AGM2@data

length(unique(tmp$WY))
length(unique(tmp$Station.ID))

m.TP=bam(log(GM.TP)~
           s(WY,k=6)+
           s(Longitude,Latitude,bs="ds",k=50,m=c(1,0.5))+
           ti(Longitude,Latitude,WY,d=c(2,1),bs=c("ds","tp"),k=c(50,6)),
         data=tmp)

summary(m.TP)
nvar=3;layout(matrix(1:nvar,1,nvar))
plot(m.TP,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(m.TP)

shapiro.test(residuals(m.TP))

draw(m.TP)

pdata<-with(tmp,
            expand.grid(
              WY=c(2010:2019),
              Longitude=seq(min(Longitude),max(Longitude),length.out=100),
              Latitude=seq(min(Latitude),max(Latitude),length.out=100)
            ))

fit <- predict(m.TP, pdata)
# ind <- exclude.too.far(pdata$Longitude, pdata$Latitude,
#                        tmp$Longitude, tmp$Latitude, dist = 0.1)
# fit[ind] <- NA
pred <- cbind(pdata, Fitted = exp(fit))

ggplot(pred, aes(x = Longitude, y = Latitude)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ WY, ncol = 3) +
  scale_fill_viridis(name = "TP", option = 'plasma', na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'right')+theme_void()

WYs=2010:2019
for(i in 1:length(WYs)){
  tmp=subset(pred,WY==WYs[i])[,c("Fitted","Longitude","Latitude")]
  coordinates(tmp)<-~Longitude + Latitude
  gridded(tmp)<-TRUE
  # rasterDF<-raster::raster(tmp,layer=1,values=T)
  tmp=as(tmp,"RasterLayer")
  proj4string(tmp)<-wgs84
  tmp=raster::projectRaster(tmp,crs=wkt(utm17))
  tmp.m=raster::mask(tmp,bb.nnc.buf)
  assign(paste0("GAM.TP.",WYs[i]),tmp.m)
  print(i)
}

GAM.TP.stack2=stack(GAM.TP.2010,
                    GAM.TP.2011,
                    GAM.TP.2012,
                    GAM.TP.2013,
                    GAM.TP.2014,
                    GAM.TP.2015,
                    GAM.TP.2016,
                    GAM.TP.2017,
                    GAM.TP.2018,
                    GAM.TP.2019)

tmap_mode("plot")
bbox.lims=bbox(gBuffer(bb.nnc.buf,width = 2000))
BB.GAM.TP=tm_shape(GAM.TP.stack2,bbox=bbox.lims)+
  tm_raster(title="",palette="-viridis",breaks=c(0,2.5,5,10,15,25),labels=c("<2.5","2.5 - 5.0","5.0 - 10", "10 - 15", "15 - 25"),alpha=0.75)+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=WYs)+
  tm_legend(title="TP AGM (\u03BCg L\u207B\u00B9)",legend.outside=T, legend.text.size=0.5,legend.title.size=0.75)+
  tm_shape(subset(bb.site.locs,Station.ID%in%unique(TP.AGM2$Station.ID)))+tm_symbols(size=0.25,col="grey",shape=21,alpha=0.25)+
  tm_shape(shore)+tm_polygons(col="cornsilk")+tm_layout(bg.color="lightblue")+
tm_shape(canals)+tm_lines(col="grey",lwd=2,alpha=0.5)+
  tm_shape(BBAP_struct)+tm_symbols(size=0.5)+tm_text("NAME",just="right",size=0.75,shadow=T,xmod=-0.25)

tmap_animation(BB.GAM.TP,filename="./Plots/BB_TP_GAM.gif",delay=80,width=350,height=650,loop=TRUE)

tmp=plot(m.TP,residuals=T)
# png(filename=paste0(plot.path,"GAM_mTP_draw_base.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3,1,1.75),oma=c(2,1,0.25,0.25));
layout(matrix(1:3,1,3),widths=c(1,1,0.5))

crit=qnorm((1 - 0.95) / 2, lower.tail = FALSE)
ylim.val=c(-0.75,0.75);by.y=0.5;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2019);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(tmp[1][[1]]$fit~tmp[1][[1]]$x,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp[1][[1]],points(raw,p.resid,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(tmp[1][[1]],shaded.range(x,fit-(crit*se),fit+(crit*se),"grey",lty=1))
with(tmp[1][[1]],lines(x,fit,lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(WY)")
mtext(side=1,line=2,"Water Year")
mtext(side=2,line=2.25,"Effect")


ylim.val=range(tmp[2][[1]]$raw[2]);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=range(tmp[2][[1]]$raw[1]);by.x=0.05;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma=with(tmp[2][[1]],matrix(fit,nrow=length(y),ncol=length(x)))
brk=50
breaks.val=classInt::classIntervals(tmp[2][[1]]$fit[is.na(tmp[2][[1]]$fit)==F],style="equal",n=brk)
pal=hcl.colors(n=brk,alpha=0.75)
image(x=tmp[2][[1]]$x,y=tmp[2][[1]]$y,z=tmp.ma,
      breaks=breaks.val$brks,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=tmp[2][[1]]$x,y=tmp[2][[1]]$y,z=tmp.ma,add=T,drawlabels=F,lwd=1.25)
axis_fun(1,xmaj,xmin,format(round(xmaj,2)),line=-0.5);axis_fun(2,ymaj,ymin,format(round(ymaj,1)));box(lwd=1)
mtext(side=3,adj=0,"ti(Long,Lat)")
mtext(side=1,line=2.25,"Longitude")
mtext(side=2,line=3,"Latitude")

legend_image=as.raster(matrix(rev(pal),ncol=1))
par(xpd=NA,mar=c(2,1,1,0))
plot(c(0,1),c(0,1),type = 'n', axes = F,ann=F)
rasterImage(legend_image, 0, 0.25, 0.3,0.75)
text(x=0.3, y = seq(0.25,0.75,length.out=2), labels = format(round(range(breaks.val$brks),2)),cex=1,pos=4)
text(0.15,0.76,"Effect",pos=3,xpd=NA)
dev.off()


## TN
tmp=TN.AGM2@data

length(unique(tmp$WY))
length(unique(tmp$Station.ID))

m.TN=bam(log(GM.TN)~
           s(WY,k=6)+
           s(Longitude,Latitude,bs="ds",k=20,m=c(1,0.5))+
           ti(Longitude,Latitude,WY,d=c(2,1),bs=c("ds","tp"),k=c(20,6)),
         data=tmp)

summary(m.TN)
nvar=3;layout(matrix(1:nvar,1,nvar))
plot(m.TN,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(m.TN)

shapiro.test(residuals(m.TN))
draw(m.TN)

pdata<-with(tmp,
            expand.grid(
              WY=c(2010:2019),
              Longitude=seq(min(Longitude),max(Longitude),length.out=100),
              Latitude=seq(min(Latitude),max(Latitude),length.out=100)
            ))

fit <- predict(m.TN, pdata)
pred <- cbind(pdata, Fitted = exp(fit))

ggplot(pred, aes(x = Longitude, y = Latitude)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ WY, ncol = 3) +
  scale_fill_viridis(name = "TN", option = 'plasma', na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'right')+theme_void()

WYs=2010:2019
for(i in 1:length(WYs)){
  tmp=subset(pred,WY==WYs[i])[,c("Fitted","Longitude","Latitude")]
  coordinates(tmp)<-~Longitude + Latitude
  gridded(tmp)<-TRUE
  # rasterDF<-raster::raster(tmp,layer=1,values=T)
  tmp=as(tmp,"RasterLayer")
  proj4string(tmp)<-wgs84
  tmp=raster::projectRaster(tmp,crs=wkt(utm17))
  tmp.m=raster::mask(tmp,bb.nnc.buf)
  assign(paste0("GAM.TN.",WYs[i]),tmp.m)
  print(i)
}

GAM.TN.stack2=stack(GAM.TN.2010,
                    GAM.TN.2011,
                    GAM.TN.2012,
                    GAM.TN.2013,
                    GAM.TN.2014,
                    GAM.TN.2015,
                    GAM.TN.2016,
                    GAM.TN.2017,
                    GAM.TN.2018,
                    GAM.TN.2019)

bbox.lims=bbox(gBuffer(bb.nnc.buf,width = 2000))
BB.GAM.TN=tm_shape(GAM.TN.stack2,bbox=bbox.lims)+
  tm_raster(title="",palette="-viridis",alpha=0.75,breaks=c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4),labels=c("<0.2","0.2 - 0.4","0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0","1.0 - 1.2","1.2 - 1.4"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=WYs)+
  tm_legend(title="TN AGM (mg L\u207B\u00B9)",legend.outside=T, legend.text.size=0.5,legend.title.size=0.75)+
  tm_shape(subset(bb.site.locs,Station.ID%in%unique(TN.AGM2$Station.ID)))+tm_symbols(size=0.25,col="grey",shape=21,alpha=0.25)+
  tm_shape(shore)+tm_polygons(col="cornsilk")+tm_layout(bg.color="lightblue")+
  tm_shape(canals)+tm_lines(col="grey",lwd=2,alpha=0.5)+
  tm_shape(BBAP_struct)+tm_symbols(size=0.5)+tm_text("NAME",just="right",size=0.75,shadow=T,xmod=-0.25)

tmap_animation(BB.GAM.TN,filename="./Plots/BB_TN_GAM.gif",delay=80,width=350,height=650,loop=TRUE)

tmp=plot(m.TN,residuals=T)
# png(filename=paste0(plot.path,"GAM_mTN_draw_base.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3,1,1.75),oma=c(2,1,0.25,0.25));
layout(matrix(1:3,1,3),widths=c(1,1,0.5))

crit=qnorm((1 - 0.95) / 2, lower.tail = FALSE)
ylim.val=c(-1,1);by.y=0.5;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2010,2019);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(tmp[1][[1]]$fit~tmp[1][[1]]$x,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp[1][[1]],points(raw,p.resid,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(tmp[1][[1]],shaded.range(x,fit-(crit*se),fit+(crit*se),"grey",lty=1))
with(tmp[1][[1]],lines(x,fit,lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(WY)")
mtext(side=1,line=2,"Water Year")
mtext(side=2,line=2.25,"Effect")


ylim.val=range(tmp[2][[1]]$raw[2]);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=range(tmp[2][[1]]$raw[1]);by.x=0.05;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma=with(tmp[2][[1]],matrix(fit,nrow=length(y),ncol=length(x)))
brk=50
breaks.val=classInt::classIntervals(tmp[2][[1]]$fit[is.na(tmp[2][[1]]$fit)==F],style="equal",n=brk)
pal=hcl.colors(n=brk,alpha=0.75)
image(x=tmp[2][[1]]$x,y=tmp[2][[1]]$y,z=tmp.ma,
      breaks=breaks.val$brks,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=tmp[2][[1]]$x,y=tmp[2][[1]]$y,z=tmp.ma,add=T,drawlabels=F,lwd=1.25)
axis_fun(1,xmaj,xmin,format(round(xmaj,2)),line=-0.5);axis_fun(2,ymaj,ymin,format(round(ymaj,1)));box(lwd=1)
mtext(side=3,adj=0,"ti(Long,Lat)")
mtext(side=1,line=2.25,"Longitude")
mtext(side=2,line=3,"Latitude")

legend_image=as.raster(matrix(rev(pal),ncol=1))
par(xpd=NA,mar=c(2,1,1,0))
plot(c(0,1),c(0,1),type = 'n', axes = F,ann=F)
rasterImage(legend_image, 0, 0.25, 0.3,0.75)
text(x=0.3, y = seq(0.25,0.75,length.out=2), labels = format(round(range(breaks.val$brks),2)),cex=1,pos=4)
text(0.15,0.76,"Effect",pos=3,xpd=NA)
dev.off()


## Chla
tmp=Chl.AGM2@data

range(tmp$WY)
length(unique(tmp$WY))
length(unique(tmp$Station.ID))

m.Chl=bam(log(GM.Chla)~
           s(WY,k=5)+
           s(Longitude,Latitude,bs="ds",k=15,m=c(1,0.5))+
           ti(Longitude,Latitude,WY,d=c(2,1),bs=c("ds","tp"),k=c(15,5)),
         data=tmp)

summary(m.Chl)
nvar=3;layout(matrix(1:nvar,1,nvar))
plot(m.Chl,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(m.Chl)

shapiro.test(residuals(m.Chl))
draw(m.Chl)

pdata<-with(tmp,
            expand.grid(
              WY=c(2015:2019),
              Longitude=seq(min(Longitude),max(Longitude),length.out=100),
              Latitude=seq(min(Latitude),max(Latitude),length.out=100)
            ))

fit <- predict(m.Chl, pdata)
pred <- cbind(pdata, Fitted = exp(fit))

ggplot(pred, aes(x = Longitude, y = Latitude)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ WY, ncol = 3) +
  scale_fill_viridis(name = "Chla", option = 'plasma', na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'right')+theme_void()

WYs=2015:2019
for(i in 1:length(WYs)){
  tmp=subset(pred,WY==WYs[i])[,c("Fitted","Longitude","Latitude")]
  coordinates(tmp)<-~Longitude + Latitude
  gridded(tmp)<-TRUE
  # rasterDF<-raster::raster(tmp,layer=1,values=T)
  tmp=as(tmp,"RasterLayer")
  proj4string(tmp)<-wgs84
  tmp=raster::projectRaster(tmp,crs=wkt(utm17))
  tmp.m=raster::mask(tmp,bb.nnc.buf)
  assign(paste0("GAM.Chl.",WYs[i]),tmp.m)
  print(i)
}

GAM.Chl.stack2=stack(GAM.Chl.2010,
                    GAM.Chl.2011,
                    GAM.Chl.2012,
                    GAM.Chl.2013,
                    GAM.Chl.2014,
                    GAM.Chl.2015,
                    GAM.Chl.2016,
                    GAM.Chl.2017,
                    GAM.Chl.2018,
                    GAM.Chl.2019)

bbox.lims=bbox(gBuffer(bb.nnc.buf,width = 2000))
BB.GAM.Chl=tm_shape(GAM.Chl.stack2,bbox=bbox.lims)+
  tm_raster(title="",palette="-viridis",alpha=0.75,breaks=c(0,1,2,3,4,5),labels=c("<1","1.0 - 2.0","2.0 - 3.0", "3.0 - 4.0", "> 4.0"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=WYs)+
  tm_legend(title="Chl AGM (mg L\u207B\u00B9)",legend.outside=T, legend.text.size=0.5,legend.title.size=0.75)+
  tm_shape(subset(bb.site.locs,Station.ID%in%unique(Chl.AGM2$Station.ID)))+tm_symbols(size=0.25,col="grey",shape=21,alpha=0.25)+
  tm_shape(shore)+tm_polygons(col="cornsilk")+tm_layout(bg.color="lightblue")+
  tm_shape(canals)+tm_lines(col="grey",lwd=2,alpha=0.5)+
  tm_shape(BBAP_struct)+tm_symbols(size=0.5)+tm_text("NAME",just="right",size=0.75,shadow=T,xmod=-0.25)

tmap_animation(BB.GAM.Chl,filename="./Plots/BB_Chl_GAM.gif",delay=90,width=350,height=650,loop=TRUE)


tmp=plot(m.Chl,residuals=T)
# png(filename=paste0(plot.path,"GAM_mChl_draw_base.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3,1,1.75),oma=c(2,1,0.25,0.25));
layout(matrix(1:3,1,3),widths=c(1,1,0.5))

crit=qnorm((1 - 0.95) / 2, lower.tail = FALSE)
ylim.val=c(-0.75,0.75);by.y=0.5;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2015,2019);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(tmp[1][[1]]$fit~tmp[1][[1]]$x,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp[1][[1]],points(raw,p.resid,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(tmp[1][[1]],shaded.range(x,fit-(crit*se),fit+(crit*se),"grey",lty=1))
with(tmp[1][[1]],lines(x,fit,lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"s(WY)")
mtext(side=1,line=2,"Water Year")
mtext(side=2,line=2.25,"Effect")

ylim.val=range(tmp[2][[1]]$raw[2]);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2);ylen=length(ymaj)
xlim.val=range(tmp[2][[1]]$raw[1]);by.x=0.05;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2);xlen=length(xmaj)
tmp.ma=with(tmp[2][[1]],matrix(fit,nrow=length(y),ncol=length(x)))
brk=50
breaks.val=classInt::classIntervals(tmp[2][[1]]$fit[is.na(tmp[2][[1]]$fit)==F],style="equal",n=brk)
pal=hcl.colors(n=brk,alpha=0.75)
image(x=tmp[2][[1]]$x,y=tmp[2][[1]]$y,z=tmp.ma,
      breaks=breaks.val$brks,col=pal,
      ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
contour(x=tmp[2][[1]]$x,y=tmp[2][[1]]$y,z=tmp.ma,add=T,drawlabels=F,lwd=1.25)
axis_fun(1,xmaj,xmin,format(round(xmaj,2)),line=-0.5);axis_fun(2,ymaj,ymin,format(round(ymaj,1)));box(lwd=1)
mtext(side=3,adj=0,"ti(Long,Lat)")
mtext(side=1,line=2.25,"Longitude")
mtext(side=2,line=3,"Latitude")

legend_image=as.raster(matrix(rev(pal),ncol=1))
par(xpd=NA,mar=c(2,1,1,0))
plot(c(0,1),c(0,1),type = 'n', axes = F,ann=F)
rasterImage(legend_image, 0, 0.25, 0.3,0.75)
text(x=0.3, y = seq(0.25,0.75,length.out=2), labels = format(round(range(breaks.val$brks),2)),cex=1,pos=4)
text(0.15,0.76,"Effect",pos=3,xpd=NA)
dev.off()

summary(m.TP)
summary(m.TN)
summary(m.Chl)
