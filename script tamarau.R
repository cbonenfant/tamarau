library(reshape2)
library(summarytools)
library(googledrive)
library(ggplot2)
library(grid)
library(gridExtra)

setwd("C:/Users/Manon GHISLAIN/Documents/tamarau/datas pour analyses")

load("data.Rdata")
# #Ou pour être certain d'avoir la dernière MAJ (sur google drive):
# temp <- tempfile(fileext = ".Rdata")
# dl <- drive_download(
#   as_id("https://drive.google.com/file/d/1nLplDsw_bxB2D62RpokigYUxtecKQymv/view?usp=sharing"), path = temp, overwrite = TRUE)
# load(temp)

##### BILAN DES DONNESS - mise en forme ------------------
head(data)
view(dfSummary(data))
str(data)
hist(data$Number)
#différences d'échantillonnage entre années?
table(data$year)

table(data$year,data$Age)#2004->2008 : les calfs ne sont pas différenciés
table(data$year,data$Site) #2004->2006 : sites 17 et 18 non comptés


##### sélection datas pour un premier bilan : sites 1 à 16----- 
dataselec<-data[which(data$Site!="17.  Tangle" & data$Site!="17.  Saligi east" & data$Site!="18.  Malibayong"),]
dataselec<-droplevels(dataselec)
#constance de l'abondance par site dans le temps
zfac <-as.factor( dataselec$year[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"] )
mescouleurs <- rainbow(length(levels(zfac)))
plot(dataselec$Number[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"], 
     dataselec$Site[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"] 
     , col = mescouleurs[zfac], pch = rank(as.numeric(levels(zfac))),
     xlab="Nombre d'individus (M Ad)",
     ylab="Sites")
legend("topright", inset = 0.02, pch = rank(as.numeric(levels(zfac))), legend = levels(zfac), col = mescouleurs)

####### détails par site-clage-age-sex-----
annees<-unique(data$year)
facsite <-as.factor(data$numsite)
couleurssites<-rainbow(length(levels(facsite)))
####### Tous individus -----
ts_ind_data<-data[which(data$count=="AN"),]
ts_ind<-aggregate(ts_ind_data$Number, by=list(year=ts_ind_data$year, numsite=ts_ind_data$numsite), FUN=sum)
plot( x =annees,
      y=ts_ind$x[ts_ind$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,110), col=couleurssites[1],
      main="Nombre d'individus par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,110,5),labels=seq(0,110,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=4)
points(x=annees, y=ts_ind$x[ts_ind$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=ts_ind$x[ts_ind$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),ts_ind$x[ts_ind$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),ts_ind$x[ts_ind$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

####### Males Adultes -----------
mal_ad<-data[which(data$Age=="Ad" & data$Sexe=="M" & data$count=="AN"),]
plot( x =annees,
      y=mal_ad$Number[mal_ad$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (M Ad)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,20), col=couleurssites[1],
      main="Nombre de mâles adultes par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,20,5),labels=seq(0,20,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=4)
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=mal_ad$Number[mal_ad$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),mal_ad$Number[mal_ad$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),mal_ad$Number[mal_ad$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

####### Femelles Adultes -----
fem_ad<-data[which(data$Age=="Ad" & data$Sexe=="F" & data$count=="AN"),]
plot( x =annees,
      y=fem_ad$Number[fem_ad$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (F Ad)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,55), col=couleurssites[1], 
      main="Nombre de femelles adultes par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,55,5),labels=seq(0,55,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=fem_ad$Number[fem_ad$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),fem_ad$Number[fem_ad$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),fem_ad$Number[fem_ad$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

####### Unknown Adultes -----
unk_ad<-data[which(data$Age=="Ad" & data$Sexe=="U" & data$count=="AN"),]
plot( x =annees,
      y=unk_ad$Number[unk_ad$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (U Ad)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,10), col=couleurssites[1], 
      main="Nombre d'adultes non sexés par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,10,5),labels=seq(0,10,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=3)
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=unk_ad$Number[unk_ad$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),unk_ad$Number[unk_ad$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),unk_ad$Number[unk_ad$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])


####### Males Juv/subadult -----------
mal_j<-data[which(data$Age=="J" & data$Sexe=="M" & data$count=="AN"),]
plot( x =annees,
      y=mal_j$Number[mal_j$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (M J/SubAd)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,15), col=couleurssites[1],
      main="Nombre de mâles juvéniles et subadultes par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,15,5),labels=seq(0,15,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=mal_j$Number[mal_j$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=mal_j$Number[mal_j$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=mal_j$Number[mal_j$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=mal_j$Number[mal_j$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=mal_j$Number[mal_j$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=mal_j$Number[mal_j$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=mal_j$Number[mal_j$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=mal_j$Number[mal_j$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=mal_j$Number[mal_j$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=mal_j$Number[mal_j$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=mal_j$Number[mal_j$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=mal_j$Number[mal_j$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=mal_j$Number[mal_j$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=mal_j$Number[mal_j$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=mal_j$Number[mal_j$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),mal_j$Number[mal_j$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),mal_j$Number[mal_j$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

####### Femelles Juv/subadult -----
fem_j<-data[which(data$Age=="J" & data$Sexe=="F" & data$count=="AN"),]
plot( x =annees,
      y=fem_j$Number[fem_j$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (F J/SubAd)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,10), col=couleurssites[1], 
      main="Nombre de femelles juvéniles et subadultes par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,10,5),labels=seq(0,10,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=fem_j$Number[fem_j$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=fem_j$Number[fem_j$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=fem_j$Number[fem_j$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=fem_j$Number[fem_j$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=fem_j$Number[fem_j$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=fem_j$Number[fem_j$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=fem_j$Number[fem_j$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=fem_j$Number[fem_j$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=fem_j$Number[fem_j$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=fem_j$Number[fem_j$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=fem_j$Number[fem_j$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=fem_j$Number[fem_j$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=fem_j$Number[fem_j$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=fem_j$Number[fem_j$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=fem_j$Number[fem_j$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),fem_j$Number[fem_j$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),fem_j$Number[fem_j$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

####### Unknown Juv/subadult -----
unk_j<-data[which(data$Age=="J" & data$Sexe=="U" & data$count=="AN"),]
plot( x =annees,
      y=unk_j$Number[unk_j$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (U Ad)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,25), col=couleurssites[1], 
      main="Nombre de juvéniles et de subadultes non sexés par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,25,5),labels=seq(0,25,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=unk_j$Number[unk_j$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=unk_j$Number[unk_j$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=unk_j$Number[unk_j$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=unk_j$Number[unk_j$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=unk_j$Number[unk_j$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=unk_j$Number[unk_j$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=unk_j$Number[unk_j$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=unk_j$Number[unk_j$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=unk_j$Number[unk_j$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=unk_j$Number[unk_j$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=unk_j$Number[unk_j$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=unk_j$Number[unk_j$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=unk_j$Number[unk_j$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=unk_j$Number[unk_j$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=unk_j$Number[unk_j$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),unk_j$Number[unk_j$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),unk_j$Number[unk_j$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])



####### Males Yearling -----------
mal_y<-data[which(data$Age=="Y" & data$Sexe=="M" & data$count=="AN"),]
plot( x =annees,
      y=mal_y$Number[mal_y$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (M Yearling)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,6), col=couleurssites[1],
      main="Nombre de mâles yearling par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,6,1),labels=seq(0,6,1), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=mal_y$Number[mal_y$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=mal_y$Number[mal_y$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=mal_y$Number[mal_y$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=mal_y$Number[mal_y$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=mal_y$Number[mal_y$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=mal_y$Number[mal_y$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=mal_y$Number[mal_y$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=mal_y$Number[mal_y$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=mal_y$Number[mal_y$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=mal_y$Number[mal_y$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=mal_y$Number[mal_y$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=mal_y$Number[mal_y$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=mal_y$Number[mal_y$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=mal_y$Number[mal_y$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=mal_y$Number[mal_y$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),mal_y$Number[mal_y$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),mal_y$Number[mal_y$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

####### Femelles Yearling -----
fem_y<-data[which(data$Age=="Y" & data$Sexe=="F" & data$count=="AN"),]
plot( x =annees,
      y=fem_y$Number[fem_y$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (F Yearling)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,6), col=couleurssites[1], 
      main="Nombre de femelles yearling par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,6,1),labels=seq(0,6,1), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=fem_y$Number[fem_y$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=fem_y$Number[fem_y$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=fem_y$Number[fem_y$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=fem_y$Number[fem_y$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=fem_y$Number[fem_y$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=fem_y$Number[fem_y$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=fem_y$Number[fem_y$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=fem_y$Number[fem_y$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=fem_y$Number[fem_y$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=fem_y$Number[fem_y$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=fem_y$Number[fem_y$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=fem_y$Number[fem_y$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=fem_y$Number[fem_y$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=fem_y$Number[fem_y$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=fem_y$Number[fem_y$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),fem_y$Number[fem_y$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),fem_y$Number[fem_y$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])



####### Unknown Juv/subadult -----
unk_y<-data[which(data$Age=="Y" & data$Sexe=="U" & data$count=="AN"),]
plot( x =annees,
      y=unk_y$Number[unk_y$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (U Yearling)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,25), col=couleurssites[1], 
      main="Nombre de yearling non sexés par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,25,5),labels=seq(0,25,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=annees, y=unk_y$Number[unk_y$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=unk_y$Number[unk_y$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=unk_y$Number[unk_y$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=unk_y$Number[unk_y$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=unk_y$Number[unk_y$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=unk_y$Number[unk_y$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=unk_y$Number[unk_y$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=unk_y$Number[unk_y$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=unk_y$Number[unk_y$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=unk_y$Number[unk_y$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=unk_y$Number[unk_y$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=unk_y$Number[unk_y$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=unk_y$Number[unk_y$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=unk_y$Number[unk_y$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=unk_y$Number[unk_y$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),unk_y$Number[unk_y$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),unk_y$Number[unk_y$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])




####### Calf -----
all_c<-data[which(data$Age=="C" & data$count=="AN"),]
all_c<-aggregate(all_c$Number, by=list(Site=all_c$numsite, year=all_c$year ), FUN=sum)
plot( x =(2009:2018),
      y=all_c$x[all_c$Site=="1"  ] ,
      type="b", xlab="Années",ylab="Nombre d'individus (Calf)",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,25), col=couleurssites[1], 
      main="Nombre de calf par site par année") 
axis(1, at=seq(2009,2018,1), labels=(2009:2018),las=2)
axis(2, at=seq(0,25,5),labels=seq(0,25,5), las=1)
legend("topleft", inset = 0.02, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2)
points(x=(2009:2018), y=all_c$x[all_c$Site=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=(2009:2018), y=all_c$x[all_c$Site=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=(2009:2018), y=all_c$x[all_c$Site=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=(2009:2018), y=all_c$x[all_c$Site=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=(2009:2018), y=all_c$x[all_c$Site=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=(2009:2018), y=all_c$x[all_c$Site=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=(2009:2018), y=all_c$x[all_c$Site=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=(2009:2018), y=all_c$x[all_c$Site=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=(2009:2018), y=all_c$x[all_c$Site=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=(2009:2018), y=all_c$x[all_c$Site=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=(2009:2018), y=all_c$x[all_c$Site=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=(2009:2018), y=all_c$x[all_c$Site=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=(2009:2018), y=all_c$x[all_c$Site=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=(2009:2018), y=all_c$x[all_c$Site=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=(2009:2018), y=all_c$x[all_c$Site=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=(2009:2018), y=all_c$x[all_c$Site=="17"],type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=(2009:2018), y=all_c$x[all_c$Site=="18"],type="b",
       pch=18, cex=1, col=couleurssites[18])


####### relation Total Number TN et Actual Number AN (=doublons pris en compte) ----
#suppr 2005 car juste TN
dataselec2<-dataselec[which(dataselec$year!=2005),]
actual_number<-dataselec2$Number[dataselec2$count=="AN" & dataselec2$Sexe=="M"]
total_number<-dataselec2$Number[dataselec2$count=="TN" & dataselec2$Sexe=="M"]
plot(total_number,actual_number, main="relation entre Total Number et Actual Number (M)")
reg<-lm(actual_number~total_number)
summary(reg) #significativemet corréllé ***
abline(reg, col="blue")
# Equation de la droite de regression : 
coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

actual_number<-dataselec2$Number[dataselec2$count=="AN" & dataselec2$Sexe=="F"]
total_number<-dataselec2$Number[dataselec2$count=="TN" & dataselec2$Sexe=="F"]
plot(total_number,actual_number, main="relation entre Total Number et Actual Number (F)")
reg<-lm(actual_number~total_number)
summary(reg) #significativemet corréllé ***
abline(reg, col="blue")
# Equation de la droite de regression : 
coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

actual_number<-dataselec2$Number[dataselec2$count=="AN" & dataselec2$Age=="C"]
total_number<-dataselec2$Number[dataselec2$count=="TN" & dataselec2$Age=="C"]
plot(total_number,actual_number, main="relation entre Total Number et Actual Number (Calf)")
reg<-lm(actual_number~total_number)
summary(reg) #significativemet corréllé ***
abline(reg, col="blue")
# Equation de la droite de regression : 
coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

###### évolution du nombre d'individus par catégorie d'âge----
#somme tous sites confondus sur actualnumber
dataselec3<-dataselec[which(dataselec$count=="AN" &  dataselec$year>=2006),]
tableau<-aggregate(dataselec3$Number, by=list(Age=dataselec3$Age, year= dataselec3$year), FUN=sum)
plot( x = (2006:2018),  y=as.numeric(tableau$x[tableau$Age=="Ad"])  , type="b", xlab="",ylab="",
      pch=19 ,cex=0.5, ylim=c(0,300), bty="n",  axes = FALSE, col="blue",
      main="Nombre d'individus par an et par âge") #adultes
annees<-c(2006:2018)
axis(1, at=seq(2006,2018,1), labels=annees,las=2)
axis(2, at=seq(0,300,50),labels=seq(0,300,50), las=1)
legend("topleft", inset = 0.02,
       legend = c( "Adulte", "Juvénile/Subadulte", "Yearling", "Calf"), 
       lty = c(1, 1, 1), lwd = c(2, 2, 2),
        col = c( "blue", "red", "black", "orange"), 
       bty = "n", cex = 1) 
points(x=(2006:2018), y=as.numeric(tableau$x[tableau$Age=="J"]),type="b",
       pch=19, cex=0.5, col="red")  #juv/subadult
points(x=(2006:2018), y=as.numeric(tableau$x[tableau$Age=="Y"]),type="b",
       pch=19, cex=0.5, col="black")  #yearling
points(x=(2006:2018), y=c(rep(NA,3),as.numeric(tableau$x[tableau$Age=="C"])),type="b",
       pch=19, cex=0.5, col="orange")  #calf

#lien entre yearling t et calf t-1??
yearling<-tableau$x[tableau$year>=2010 & tableau$Age=="Y"]
calf<-tableau$x[tableau$year>=2009 & tableau$year!=2018 & tableau$Age=="C"]
plot(yearling,calf)
summary(lm(yearling~calf)) ##pas de corrélation (sur 9 années)

#boxplot pour "en moyenne", proportions jeunes par rapport à adultes
boxplot(tableau$x~tableau$Age,main="Nombre d'individus par an et par âge")

###### évolution du sex-ratio dans le temps (global et par site)-----
mal_ad$sexratio<-mal_ad$Number/(fem_ad$Number+mal_ad$Number)
annees<-c(2004:2018)
plot( x =annees,
      y=mal_ad$sexratio[mal_ad$numsite=="1"  ] ,
      type="b", xlab="Années",ylab="Pourcentage de mâles",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,1), col=couleurssites[1],
      main="Pourcentage de mâles (adultes) par site par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,1,0.1),labels=seq(0,1,0.1), las=1)
legend("topleft", inset = 0.01, 
       pch = rank(as.numeric(levels(facsite))), legend = levels(facsite), col = couleurssites,
       ncol=2, cex=0.8)
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="2"  ],type="b",
       pch=2, cex=1, col=couleurssites[2])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="3"  ],type="b",
       pch=3, cex=1, col=couleurssites[3])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="4"  ],type="b",
       pch=4, cex=1, col=couleurssites[4])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="5"  ],type="b",
       pch=5, cex=1, col=couleurssites[5])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="6"  ],type="b",
       pch=6, cex=1, col=couleurssites[6])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="7"  ],type="b",
       pch=7, cex=1, col=couleurssites[7])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="8"  ],type="b",
       pch=8, cex=1, col=couleurssites[8])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="9"  ],type="b",
       pch=9, cex=1, col=couleurssites[9])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="10"  ],type="b",
       pch=10, cex=1, col=couleurssites[10])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="11"  ],type="b",
       pch=11, cex=1, col=couleurssites[11])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="12"  ],type="b",
       pch=12, cex=1, col=couleurssites[12])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="13"  ],type="b",
       pch=13, cex=1, col=couleurssites[13])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="14"  ],type="b",
       pch=14, cex=1, col=couleurssites[14])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="15"  ],type="b",
       pch=15, cex=1, col=couleurssites[15])
points(x=annees, y=mal_ad$sexratio[mal_ad$numsite=="16"  ],type="b",
       pch=16, cex=1, col=couleurssites[16])
points(x=annees, y=c(rep(NA,3),mal_ad$sexratio[mal_ad$numsite=="17"]),type="b",
       pch=17, cex=1, col=couleurssites[17])
points(x=annees, y=c(rep(NA,3),mal_ad$sexratio[mal_ad$numsite=="18"]),type="b",
       pch=18, cex=1, col=couleurssites[18])

#boxplotparsite
boxplot(mal_ad$sexratio~mal_ad$numsite,las=1, main="Pourcentage de mâles (adultes) par site par année", xlab="sites")

#global (somme des sites) par année
sexratioM<-aggregate(mal_ad$Number, by=list(year=mal_ad$year),FUN=sum)
sexratioF<-aggregate(fem_ad$Number, by=list(year=fem_ad$year),FUN=sum)
plot( x =annees,
      y=(sexratioM$x/(sexratioF$x+sexratioM$x)) ,
      type="b", xlab="Années",ylab="Sexratio",
      pch=1 ,cex=1,  bty="n",  axes = FALSE,
      ylim=c(0,1), col=couleurssites[1],
      main="Pourcentage de mâles des adultes par année") 
axis(1, at=seq(2004,2018,1), labels=annees,las=2)
axis(2, at=seq(0,1,0.1),labels=seq(0,1,0.1), las=1)
abline(h=0.5, col="black")
#toujours un peu moins de M que de F
#test d'ue tendance avec glm (binomial)
y<-cbind(sexratioM$x,sexratioF$x)
model_sexratio<-glm(y~as.numeric(sexratioM$year), family=binomial(link = "logit"))
anova(model_sexratio, test = "Chisq")

######  Exploration de la densité-dépendance -----
Ntot <- aggregate(tableau$x, by = list(yr = tableau$year), sum)
Ntot$r <- c(diff(log(Ntot$x)), NA) # Calcul taux d'accroissement annuel

## Variation temporelle de la taille de pop
p1 <- ggplot(Ntot, aes(x = yr, y = x)) +
  geom_point(size= 2) +
  geom_line(linetype = 2) +
  xlab("Time (years)") +
  ylab("Population Size")
## Relation n(t+1) = f(n(t))
p2 <- ggplot(Ntot, aes(x = c(Ntot$x[1:12], NA), y = c(Ntot$x[2:13], NA))) +
  geom_point(size= 2) +
  geom_line(linetype = 2) +
  xlab("Population Size (t)") +
  ylab("Population Size (t+1)")
## Variation temporelle du taux d'accroissement annuel
p3 <- ggplot(subset(Ntot, !is.na(r)), aes(y = r, x = yr)) +
  geom_hline(yintercept = 0.25) +
  geom_point(size= 2) +
  geom_line(linetype = 2) +
  xlab("Time (years)") +
  ylab("Population Growth Rate")
## Relation entre taux d'accroissement annuel et taille de pop
p4 <- ggplot(subset(Ntot, !is.na(r)), aes(y = r, x = x)) +
  geom_point(size= 2) +
  geom_text(aes(label = yr), hjust = 0.5, vjust = 2) +
  xlab("Population Size") +
  ylab("Population Growth Rate")
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
dev.copy2pdf(file = "Fig_tamarau_1.pdf")
dev.off()





