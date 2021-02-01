#### Elevational pattern of environmental factors for each of the mountains####

png("grad_alt_F.png", width= 15, height= 15, units= "cm", res = 360)

par(mfrow=c(3,2), oma=c(1.8,1,0.5,1), mar =c(2,3,1,1), ljoin=1)

# Temperature
plot (altitud_Real, temp,pch=pch_site, col=col_site , cex= 1,
      cex.lab=1,cex.axis=0.9, mgp=c(1.2, 0.30, 0),tcl=-0.25,
      bty="n", xlim = c(1100, 2200), xaxt="n",
      xlab = "", ylab = "T (?C)",ylim=c(21,28))

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

lo <- loess(temp~altitud_Real, span=2) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)

pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

# Soil Water Content
plot (altitud_Real, H_suelo,pch=pch_site, col=col_site , cex= 1,
      cex.lab=1,cex.axis=0.9, mgp=c(1.2, 0.30, 0),tcl=-0.25,
      bty="n", xlim = c(1100, 2200), xaxt="n",
      xlab = "", ylab = "Soil Water Content",ylim=c(0.2,0.50))

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

lo <- loess(H_suelo~altitud_Real) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)

pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

legend ("topright", inset= (-0.1),xpd=NA, legend=c("1","2", "3" ),col=c("grey25", "gray45", "gray65"), cex= 0.8, pch= c( 15,16,17),
        bty="n", title= "Mountain")

#NDVI
plot (altitud_Real, NDVI,pch=pch_site, col=col_site , cex= 1,
      cex.lab=1,cex.axis=0.9, mgp=c(1.2, 0.30, 0),tcl=-0.25,
      bty="n", xlim = c(1100, 2200), xaxt="n",
      xlab = "", ylab = "NDVI",ylim=c(0.2,0.8))

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

lo <- loess(NDVI~altitud_Real) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)
pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

# Terrain Ruggedness Index
plot (altitud_Real, esc,pch=pch_site, col=col_site , cex= 1,
      cex.lab=1,cex.axis=0.9, mgp=c(1.2, 0.30, 0),tcl=-0.25,
      bty="n", xlim = c(1100, 2200), xaxt="n",
      xlab = "", ylab = "Terrain Ruggedness Index",ylim=c(1,50))

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

lo <- loess(esc~altitud_Real) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)
pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

#NDVI_sd
plot (altitud_Real, NDVI_stdev,pch=pch_site, col=col_site , cex= 1,
      cex.lab=1,cex.axis=0.9, mgp=c(1.2, 0.30, 0),tcl=-0.25,
      bty="n", xlim = c(1100, 2200), xaxt="n",
      xlab ="Elevation (m a.s.l.)", ylab = "NDVI Standard Desviation ",ylim=c(0,0.12))

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

lo <- loess(NDVI_stdev~altitud_Real) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)
pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

#soil nutrient content
plot (altitud_Real, CP1_nut,pch=pch_site, col=col_site , cex= 1,
      cex.lab=1,cex.axis=0.9, mgp=c(1.2, 0.30, 0),tcl=-0.25,
      bty="n", xlim = c(1100, 2200), xaxt="n",
      xlab = "", ylab = "Soil Nutrient Content",ylim=c(-6,4))

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

lo <- loess(CP1_nut ~altitud_Real) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)
pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

dev.off()

#### Elevational pattern of orthoptera and plant richness for each of the mountains #### 

pch_site<-c(15,16,17)[factor(R_data$camino)]
col_site<-c("grey25", "gray45", "gray65")[factor(R_data$camino)]

png("RO_RP_alt.png", width= 13, height= 15, units= "cm", res = 360)

par(mfrow=c(2,1), oma=c(1.8,1,0.5,1), mar =c(2,3,1,1), ljoin=1)

plot (altura,R_o,pch=pch_site, col=col_site , cex= 1,
      cex.lab=0.9,cex.axis=0.7, mgp=c(1.2, 0.30, 0),tcl=-0.25, 
      xlab = "", ylab = "Orthopterans Richness", bty="n", xlim = c(1100, 2200), xaxt="n",
      ylim= c(0,14))

lo <- loess(R_o~altura) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)

pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7, 
     mgp=c(1.2, 0.30, 0)) # eje x

legend ("topright", inset= (-0.1),xpd=NA, legend=c("1","2", "3" ),col=c("grey25", "gray45", "gray65"), cex= 0.8, pch= c( 15,16,17),
        bty="n", title= "Elevational Gradient")


plot (altura,R_P,pch=pch_site, col=col_site, cex= 1,
      cex.lab=0.9,cex.axis=0.7, mgp=c(1.2, 0.30, 0),tcl=-0.25,bty="n",xlim = c(1100, 2200),xaxt="n", 
      xlab = "Elevation (m a.s.l.)", ylab = "Plant Richness")

lo <- loess(R_P~altitud_Real) #smooth
xl <- seq(1050,2250, (2250 - 1050)/1000)
pred<-predict(lo,xl, se=TRUE)

lines(xl, pred$fit, col='black', lwd=1.5)
lines(xl,pred$fit-1.96*pred$se.fit,lty="dashed",lwd=1.5)
lines(xl,pred$fit+1.96*pred$se.fit,lty="dashed",lwd=1.5)

axis(side=1,lwd=1,tcl=-0.25,xlim = c(1100, 2200), at=seq(1100,2200,100),cex.lab=0.9,cex.axis=0.7,
     mgp=c(1.2, 0.30, 0)) # eje x

dev.off()

#### Model Selection #### 

library (lme4)
library (MuMIn)

#Orthopterans
O_00<-glm (R_o ~  1, data= R_data, family="poisson")
O_0<- glmer (R_o ~  (1 |camino), data= R_data, family="poisson")

O_NDVI<- glmer (R_o ~ NDVI + (1 |camino) , data= R_data, family="poisson")
O_NDVIstd<- glmer (R_o ~ NDVI_stdev + (1 |camino) , data= R_data, family="poisson")
O_t<- glmer (R_o ~  temp + (1 |camino), data= R_data, family="poisson")
O_esc<- glmer (R_o ~ esc + (1 |camino), data= R_data, family="poisson")
O_cross<- glmer (R_o ~ R_P + (1 |camino), data= R_data, family="poisson")


ort<-model.sel(O_0,O_NDVI,O_NDVIstd,O_t,O_esc,O_cross,rank = AICc)

r.squaredGLMM (O_t)

install.packages("blmeco");library("blmeco")
dispersion_glmer(O_t)

plot(O_t)

#plants
p_00<-glm (R_P ~  1, data= R_data, family="poisson")
p_0<- glmer (R_P ~ (1 |camino), data= R_data, family="poisson") 

p_t<- glmer (R_P ~  temp + (1 |camino), data= R_data, family="poisson")
p_esc<- glmer (R_P ~ esc + (1 |camino), data= R_data, family="poisson")
p_H<- glmer ( R_P~ H_suelo + (1 |camino), data= R_data, family="poisson")
p_Nut<- glmer (R_P ~ CP1_nut + (1 |camino), data= R_data, family="poisson")

Rp<-model.sel(p_0, p_t, p_esc, p_H, p_Nut,rank = AICc)

Rp

plot(p_t)
dispersion_glmer(p_t)
r.squaredGLMM (p_t)

####best fitted models figure####

library (ggeffects)
library (ggplot2)

#ortopterans conditional
pr_o <- ggpredict(O_t, terms = c("temp [21:28]","camino"), type="random")

data_po<-data.frame(cbind(pr_o$x,pr_o$predicted,pr_o$group))
colnames(data_po)<-c("x","y","group")
data_o<-data.frame(cbind(R_data$temp,R_data$R_o,R_data$camino))
colnames(data_o)<-c("x","y","group")

o<- ggplot(data_po, aes(x,y, group=as.factor(group),colour=as.factor(group)))+
  geom_line(show.legend=FALSE) + scale_color_manual(name="group",values=c("grey25", "grey45", "grey65")) + xlab ("") + ylab ("Orthopteran Richness") + theme(axis.line = element_line(size = 0.6, colour = "black", linetype=1), axis.ticks.length = unit(.07, "cm"),axis.title = element_text( color="black", size=9, face=2),axis.text = element_text( size=8))

ort<-o  + geom_point (data=data_o,aes(x,y, group=as.factor(group), colour=as.factor(group), shape=  as.factor(group)),size=2) + scale_shape_manual(name="group",values=c(15, 16,17))

ort

#Plants conditional

pr_p <- ggpredict(P_t, terms = c("temp [21:28]","camino"), type="random")

data_pp<-data.frame(cbind(pr_p$x,pr_p$predicted,pr_p$group))
colnames(data_pp)<-c("x","y","group")
data_p<-data.frame(cbind(R_data$temp,R_data$R_P,R_data$camino))
colnames(data_p)<-c("x","y","group")

p<- ggplot(data_pp, aes(x,y, group=as.factor(group), colour=as.factor(group)))+
  geom_line(show.legend=FALSE) + scale_color_manual(name="group",values=c("grey25", "grey45", "grey65")) + xlab ("Temperature (C?)") + ylab ("Plant Richness") + theme(axis.line = element_line(size = 0.6, colour = "black", linetype=1), axis.ticks.length = unit(.07, "cm"),axis.title = element_text( color="black", size=9, face=2),axis.text = element_text( size=8))

plant<-p  + geom_point (data=data_p,aes(x,y, group=as.factor(group), colour=as.factor(group), shape=  as.factor(group)),size=2)+ scale_shape_manual(name="group",values=c(15, 16,17))  

plant

#Final figures
png("mod_riq_ort.png", width= 15, height= 7, units= "cm", res = 360)
ort
dev.off()

png("mod_riq_plant.png", width= 15, height= 7, units= "cm", res = 360)
plant
dev.off()

####SEM####

library (piecewiseSEM)

MM<-psem (glmer (R_o ~ abd_o+temp +R_P +(1 |camino), data= R_data,family="poisson"),
          glmer (R_P ~ temp +(1 |camino), data= R_data, family="poisson"),
          glmer (abd_o ~ temp +(1 |camino) , data= R_data, family="poisson"))


summary (MM, direction = c("R_p -> abd_o"))

coefs(MM, standardize = "none")
coefs(MM)
rsquared(MM, method = NULL)




