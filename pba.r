library(readxl)
library(agricolae)
library(tidyverse)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(multcompView)
library(PerformanceAnalytics)

L8NDVI<-read_excel("E:/ANGLO/Ortiga_ExtraccionEspectral_PD_20200430_v1.xlsm", sheet = "1_L8NDVI")
L8NDWI<-read_excel("E:/ANGLO/Ortiga_ExtraccionEspectral_PD_20200430_v1.xlsm", sheet = "1_L8NDWI")
S2NDWI<-read_excel("E:/ANGLO/Ortiga_ExtraccionEspectral_PD_20200430_v1.xlsm", sheet = "1_S2NDWI")
S2NDVI<-read_excel("E:/ANGLO/Ortiga_ExtraccionEspectral_PD_20200430_v1.xlsm", sheet = "1_S2NDVI")
SPOTNDVI<-read_excel("E:/ANGLO/Ortiga_ExtraccionEspectral_PD_20200430_v1.xlsm", sheet = "1_SPOTNDVI")
SPOTNDWI<-read_excel("E:/ANGLO/Ortiga_ExtraccionEspectral_PD_20200430_v1.xlsm", sheet = "1_SPOTNDWI")

#UNIDAD 1

L8NDVI1 <- ggplot(data = L8NDVI, aes(x = Ano_1, y = L8NDVI_1, fill=Ano_1))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI1 <- ggplot(data = L8NDWI, aes(x = Ano_1, y = L8NDWI_1,fill=Ano_1))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI1 <- ggplot(data = S2NDWI, aes(x = Ano_1, y = S2NDWI_1,fill=Ano_1))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI1 <- ggplot(data = S2NDVI, aes(x = Ano_1, y = S2NDVI_1,fill=Ano_1))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI1 <- ggplot(data = SPOTNDVI, aes(x = Ano_1, y = SPOTNDVI_1,fill=Ano_1))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI1 <- ggplot(data = SPOTNDWI, aes(x = Ano_1, y = SPOTNDWI_1,fill=Ano_1))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI1,S2NDVI1,SPOTNDVI1,L8NDWI1,S2NDWI1,SPOTNDWI1, ncol=3, top=textGrob("UNIDAD 1", vjust=0.1))
ggsave(file="E:/grafico_1.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')


#HSD.test

modelo11<-lm(L8NDVI_1 ~ Ano_1, data=L8NDVI)
ANOVA11<- aov(modelo11) 
summary(ANOVA11)
TL8NDVI1<-grupos.varex1<-HSD.test(y=ANOVA11, trt = "Ano_1", group = T, console = T)

modelo21<-lm(L8NDWI_1 ~ Ano_1, data=L8NDWI)
ANOVA21<- aov(modelo21) 
summary(ANOVA21)
TL8NDWI1<-grupos.varex1<-HSD.test(y=ANOVA21, trt = "Ano_1", group = T, console = T)
df_TL8NDWI1<-TL8NDWI1$groups

modelo31<-lm(S2NDVI_1 ~ Ano_1, data=S2NDVI)
ANOVA31<- aov(modelo31) 
summary(ANOVA31)
TS2NDVI1<-grupos.varex1<-HSD.test(y=ANOVA31, trt = "Ano_1", group = T, console = T)
df_TS2NDVI1<-TS2NDVI1$groups

modelo41<-lm(S2NDWI_1 ~ Ano_1, data=S2NDWI)
ANOVA41<- aov(modelo41) 
summary(ANOVA41)
TS2NDWI1<-grupos.varex1<-HSD.test(y=ANOVA41, trt = "Ano_1", group = T, console = T)
df_TS2NDWI1<-TS2NDWI1$groups

modelo51<-lm(SPOTNDVI_1 ~ Ano_1, data=SPOTNDVI)
ANOVA51<- aov(modelo51) 
summary(ANOVA51)
TSPOTNDVI1<-grupos.varex1<-HSD.test(y=ANOVA51, trt = "Ano_1", group = T, console = T)
df_TSPOTNDVI1<-TSPOTNDVI1$groups

modelo61<-lm(SPOTNDWI_1 ~ Ano_1, data=SPOTNDWI)
ANOVA61<- aov(modelo61) 
summary(ANOVA61)
TSPOTNDWI1<-grupos.varex1<-HSD.test(y=ANOVA61, trt = "Ano_1", group = T, console = T)
df_TSPOTNDWI1<-TSPOTNDWI1$groups

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI1,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI1,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI1,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI1,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI1,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI1,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_1.png",height=18, width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 2
L8NDVI2 <- ggplot(data = L8NDVI, aes(x = Ano_2, y = L8NDVI_2, fill=Ano_2))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI2 <- ggplot(data = L8NDWI, aes(x = Ano_2, y = L8NDWI_2,fill=Ano_2))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI2 <- ggplot(data = S2NDWI, aes(x = Ano_2, y = S2NDWI_2,fill=Ano_2))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI2 <- ggplot(data = S2NDVI, aes(x = Ano_2, y = S2NDVI_2,fill=Ano_2))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI2 <- ggplot(data = SPOTNDVI, aes(x = Ano_2, y = SPOTNDVI_2,fill=Ano_2))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI2 <- ggplot(data = SPOTNDWI, aes(x = Ano_2, y = SPOTNDWI_2,fill=Ano_2))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI2,S2NDVI2,SPOTNDVI2,L8NDWI2,S2NDWI2,SPOTNDWI2, ncol=3, top=textGrob("UNIDAD 2", vjust=0.1))
ggsave(file="E:/grafico_2.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')


#HSD.test

modelo12<-lm(L8NDVI_2 ~ Ano_2, data=L8NDVI)
ANOVA12<- aov(modelo12) 
summary(ANOVA12)
TL8NDVI2<-grupos.varex1<-HSD.test(y=ANOVA12, trt = "Ano_2", group = T, console = T)

modelo22<-lm(L8NDWI_2 ~ Ano_2, data=L8NDWI)
ANOVA22<- aov(modelo22) 
summary(ANOVA22)
TL8NDWI2<-grupos.varex1<-HSD.test(y=ANOVA22, trt = "Ano_2", group = T, console = T)

modelo32<-lm(S2NDVI_2 ~ Ano_2, data=S2NDVI)
ANOVA32<- aov(modelo32) 
summary(ANOVA32)
TS2NDVI2<-grupos.varex1<-HSD.test(y=ANOVA32, trt = "Ano_2", group = T, console = T)

modelo42<-lm(S2NDWI_2 ~ Ano_2, data=S2NDWI)
ANOVA42<- aov(modelo42) 
summary(ANOVA42)
TS2NDWI2<-grupos.varex1<-HSD.test(y=ANOVA42, trt = "Ano_2", group = T, console = T)

modelo52<-lm(SPOTNDVI_2 ~ Ano_2, data=SPOTNDVI)
ANOVA52<- aov(modelo52) 
summary(ANOVA52)
TSPOTNDVI2<-grupos.varex1<-HSD.test(y=ANOVA52, trt = "Ano_2", group = T, console = T)

modelo62<-lm(SPOTNDWI_2 ~ Ano_2, data=SPOTNDWI)
ANOVA62<- aov(modelo62) 
summary(ANOVA62)
TSPOTNDWI2<-grupos.varex1<-HSD.test(y=ANOVA62, trt = "Ano_2", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI2,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI2,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI2,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI2,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI2,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI2,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
dev.print(png, "E:/Tgrafico_2.png",height=18, width=22, units = "cm", res = 400)
dev.off()



#UNIDAD 3
L8NDVI3 <- ggplot(data = L8NDVI, aes(x = Ano_3, y = L8NDVI_3, fill=Ano_3))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI3 <- ggplot(data = L8NDWI, aes(x = Ano_3, y = L8NDWI_3,fill=Ano_3))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI3 <- ggplot(data = S2NDWI, aes(x = Ano_3, y = S2NDWI_3,fill=Ano_3))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI3 <- ggplot(data = S2NDVI, aes(x = Ano_3, y = S2NDVI_3,fill=Ano_3))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI3 <- ggplot(data = SPOTNDVI, aes(x = Ano_3, y = SPOTNDVI_3,fill=Ano_3))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI3 <- ggplot(data = SPOTNDWI, aes(x = Ano_3, y = SPOTNDWI_3,fill=Ano_3))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI3,S2NDVI3,SPOTNDVI3,L8NDWI3,S2NDWI3,SPOTNDWI3, ncol=3, top=textGrob("UNIDAD 3", vjust=0.1))
ggsave(file="E:/grafico_3.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')


#HSD.test

modelo13<-lm(L8NDVI_3 ~ Ano_3, data=L8NDVI)
ANOVA13<- aov(modelo13) 
summary(ANOVA13)
TL8NDVI3<-grupos.varex1<-HSD.test(y=ANOVA13, trt = "Ano_3", group = T, console = T)

modelo23<-lm(L8NDWI_3 ~ Ano_3, data=L8NDWI)
ANOVA23<- aov(modelo23) 
summary(ANOVA23)
TL8NDWI3<-grupos.varex1<-HSD.test(y=ANOVA23, trt = "Ano_3", group = T, console = T)

modelo33<-lm(S2NDVI_3 ~ Ano_3, data=S2NDVI)
ANOVA33<- aov(modelo33) 
summary(ANOVA33)
TS2NDVI3<-grupos.varex1<-HSD.test(y=ANOVA33, trt = "Ano_3", group = T, console = T)

modelo43<-lm(S2NDWI_3 ~ Ano_3, data=S2NDWI)
ANOVA43<- aov(modelo43) 
summary(ANOVA43)
TS2NDWI3<-grupos.varex1<-HSD.test(y=ANOVA43, trt = "Ano_3", group = T, console = T)

modelo53<-lm(SPOTNDVI_3 ~ Ano_3, data=SPOTNDVI)
ANOVA53<- aov(modelo53) 
summary(ANOVA53)
TSPOTNDVI3<-grupos.varex1<-HSD.test(y=ANOVA53, trt = "Ano_3", group = T, console = T)

modelo63<-lm(SPOTNDWI_3 ~ Ano_3, data=SPOTNDWI)
ANOVA63<- aov(modelo63) 
summary(ANOVA63)
TSPOTNDWI3<-grupos.varex1<-HSD.test(y=ANOVA63, trt = "Ano_3", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI3,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI3,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI3,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI3,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI3,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI3,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_3.png",height=18,width= 22, units = "cm", res = 400)
dev.off()

#UNIDAD 4

L8NDVI4 <- ggplot(data = L8NDVI, aes(x = Ano_4, y = L8NDVI_4, fill=Ano_4))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI4 <- ggplot(data = L8NDWI, aes(x = Ano_4, y = L8NDWI_4,fill=Ano_4))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI4 <- ggplot(data = S2NDWI, aes(x = Ano_4, y = S2NDWI_4,fill=Ano_4))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI4 <- ggplot(data = S2NDVI, aes(x = Ano_4, y = S2NDVI_4,fill=Ano_4))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI4 <- ggplot(data = SPOTNDVI, aes(x = Ano_4, y = SPOTNDVI_4,fill=Ano_4))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI4 <- ggplot(data = SPOTNDWI, aes(x = Ano_4, y = SPOTNDWI_4,fill=Ano_4))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI4,S2NDVI4,SPOTNDVI4,L8NDWI4,S2NDWI4,SPOTNDWI4, ncol=3, top=textGrob("UNIDAD 4", vjust=0.1))
ggsave(file="E:/grafico_4.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo14<-lm(L8NDVI_4 ~ Ano_4, data=L8NDVI)
ANOVA14<- aov(modelo14) 
summary(ANOVA14)
TL8NDVI4<-grupos.varex1<-HSD.test(y=ANOVA14, trt = "Ano_4", group = T, console = T)

modelo24<-lm(L8NDWI_4 ~ Ano_4, data=L8NDWI)
ANOVA24<- aov(modelo24) 
summary(ANOVA24)
TL8NDWI4<-grupos.varex1<-HSD.test(y=ANOVA24, trt = "Ano_4", group = T, console = T)

modelo34<-lm(S2NDVI_4 ~ Ano_4, data=S2NDVI)
ANOVA34<- aov(modelo34) 
summary(ANOVA34)
TS2NDVI4<-grupos.varex1<-HSD.test(y=ANOVA34, trt = "Ano_4", group = T, console = T)

modelo44<-lm(S2NDWI_4 ~ Ano_4, data=S2NDWI)
ANOVA44<- aov(modelo44) 
summary(ANOVA44)
TS2NDWI4<-grupos.varex1<-HSD.test(y=ANOVA44, trt = "Ano_4", group = T, console = T)

modelo54<-lm(SPOTNDVI_4 ~ Ano_4, data=SPOTNDVI)
ANOVA54<- aov(modelo54) 
summary(ANOVA54)
TSPOTNDVI4<-grupos.varex1<-HSD.test(y=ANOVA54, trt = "Ano_4", group = T, console = T)

modelo64<-lm(SPOTNDWI_4 ~ Ano_4, data=SPOTNDWI)
ANOVA64<- aov(modelo64) 
summary(ANOVA64)
TSPOTNDWI4<-grupos.varex1<-HSD.test(y=ANOVA64, trt = "Ano_4", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI4,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI4,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI4,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI4,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI4,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI4,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_4.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 5
L8NDVI5 <- ggplot(data = L8NDVI, aes(x = Ano_5, y = L8NDVI_5, fill=Ano_5))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI5 <- ggplot(data = L8NDWI, aes(x = Ano_5, y = L8NDWI_5,fill=Ano_5))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI5 <- ggplot(data = S2NDWI, aes(x = Ano_5, y = S2NDWI_5,fill=Ano_5))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI5 <- ggplot(data = S2NDVI, aes(x = Ano_5, y = S2NDVI_5,fill=Ano_5))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI5 <- ggplot(data = SPOTNDVI, aes(x = Ano_5, y = SPOTNDVI_5,fill=Ano_5))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI5 <- ggplot(data = SPOTNDWI, aes(x = Ano_5, y = SPOTNDWI_5,fill=Ano_5))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI5,S2NDVI5,SPOTNDVI5,L8NDWI5,S2NDWI5,SPOTNDWI5, ncol=3, top=textGrob("UNIDAD 5", vjust=0.1))
ggsave(file="E:/grafico_5.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo15<-lm(L8NDVI_5 ~ Ano_5, data=L8NDVI)
ANOVA15<- aov(modelo15) 
summary(ANOVA15)
TL8NDVI5<-grupos.varex1<-HSD.test(y=ANOVA15, trt = "Ano_5", group = T, console = T)

modelo25<-lm(L8NDWI_5 ~ Ano_5, data=L8NDWI)
ANOVA25<- aov(modelo25) 
summary(ANOVA25)
TL8NDWI5<-grupos.varex1<-HSD.test(y=ANOVA25, trt = "Ano_5", group = T, console = T)

modelo35<-lm(S2NDVI_5 ~ Ano_5, data=S2NDVI)
ANOVA35<- aov(modelo35) 
summary(ANOVA35)
TS2NDVI5<-grupos.varex1<-HSD.test(y=ANOVA35, trt = "Ano_5", group = T, console = T)

modelo45<-lm(S2NDWI_5 ~ Ano_5, data=S2NDWI)
ANOVA45<- aov(modelo45) 
summary(ANOVA45)
TS2NDWI5<-grupos.varex1<-HSD.test(y=ANOVA45, trt = "Ano_5", group = T, console = T)

modelo55<-lm(SPOTNDVI_5 ~ Ano_5, data=SPOTNDVI)
ANOVA55<- aov(modelo55) 
summary(ANOVA55)
TSPOTNDVI5<-grupos.varex1<-HSD.test(y=ANOVA55, trt = "Ano_5", group = T, console = T)

modelo65<-lm(SPOTNDWI_5 ~ Ano_5, data=SPOTNDWI)
ANOVA65<- aov(modelo65) 
summary(ANOVA65)
TSPOTNDWI5<-grupos.varex1<-HSD.test(y=ANOVA65, trt = "Ano_5", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI5,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI5,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI5,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI5,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI5,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI5,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_5.png",height=18, width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 6
L8NDVI6 <- ggplot(data = L8NDVI, aes(x = Ano_6, y = L8NDVI_6, fill=Ano_6))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI6 <- ggplot(data = L8NDWI, aes(x = Ano_6, y = L8NDWI_6,fill=Ano_6))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI6 <- ggplot(data = S2NDWI, aes(x = Ano_6, y = S2NDWI_6,fill=Ano_6))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI6 <- ggplot(data = S2NDVI, aes(x = Ano_6, y = S2NDVI_6,fill=Ano_6))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI6 <- ggplot(data = SPOTNDVI, aes(x = Ano_6, y = SPOTNDVI_6,fill=Ano_6))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI6 <- ggplot(data = SPOTNDWI, aes(x = Ano_6, y = SPOTNDWI_6,fill=Ano_6))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI6,S2NDVI6,SPOTNDVI6,L8NDWI6,S2NDWI6,SPOTNDWI6, ncol=3, top=textGrob("UNIDAD 6", vjust=0.1))
ggsave(file="E:/grafico_6.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo16<-lm(L8NDVI_6 ~ Ano_6, data=L8NDVI)
ANOVA16<- aov(modelo16) 
summary(ANOVA16)
TL8NDVI6<-grupos.varex1<-HSD.test(y=ANOVA16, trt = "Ano_6", group = T, console = T)

modelo26<-lm(L8NDWI_6 ~ Ano_6, data=L8NDWI)
ANOVA26<- aov(modelo26) 
summary(ANOVA26)
TL8NDWI6<-grupos.varex1<-HSD.test(y=ANOVA26, trt = "Ano_6", group = T, console = T)

modelo36<-lm(S2NDVI_6 ~ Ano_6, data=S2NDVI)
ANOVA36<- aov(modelo36) 
summary(ANOVA36)
TS2NDVI6<-grupos.varex1<-HSD.test(y=ANOVA36, trt = "Ano_6", group = T, console = T)

modelo46<-lm(S2NDWI_6 ~ Ano_6, data=S2NDWI)
ANOVA46<- aov(modelo46) 
summary(ANOVA46)
TS2NDWI6<-grupos.varex1<-HSD.test(y=ANOVA46, trt = "Ano_6", group = T, console = T)

modelo56<-lm(SPOTNDVI_6 ~ Ano_6, data=SPOTNDVI)
ANOVA56<- aov(modelo56) 
summary(ANOVA56)
TSPOTNDVI6<-grupos.varex1<-HSD.test(y=ANOVA56, trt = "Ano_6", group = T, console = T)

modelo66<-lm(SPOTNDWI_6 ~ Ano_6, data=SPOTNDWI)
ANOVA66<- aov(modelo66) 
summary(ANOVA66)
TSPOTNDWI6<-grupos.varex1<-HSD.test(y=ANOVA66, trt = "Ano_6", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
plot(TL8NDVI6,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI6,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI6,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI6,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI6,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI6,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_6.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 7
L8NDVI7 <- ggplot(data = L8NDVI, aes(x = Ano_7, y = L8NDVI_7, fill=Ano_7))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI7 <- ggplot(data = L8NDWI, aes(x = Ano_7, y = L8NDWI_7,fill=Ano_7))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI7 <- ggplot(data = S2NDWI, aes(x = Ano_7, y = S2NDWI_7,fill=Ano_7))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI7 <- ggplot(data = S2NDVI, aes(x = Ano_7, y = S2NDVI_7,fill=Ano_7))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI7 <- ggplot(data = SPOTNDVI, aes(x = Ano_7, y = SPOTNDVI_7,fill=Ano_7))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI7 <- ggplot(data = SPOTNDWI, aes(x = Ano_7, y = SPOTNDWI_7,fill=Ano_7))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI7,S2NDVI7,SPOTNDVI7,L8NDWI7,S2NDWI7,SPOTNDWI7, ncol=3, top=textGrob("UNIDAD 7", vjust=0.1))
ggsave(file="E:/grafico_7.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo17<-lm(L8NDVI_7 ~ Ano_7, data=L8NDVI)
ANOVA17<- aov(modelo17) 
summary(ANOVA17)
TL8NDVI7<-grupos.varex1<-HSD.test(y=ANOVA17, trt = "Ano_7", group = T, console = T)

modelo27<-lm(L8NDWI_7 ~ Ano_7, data=L8NDWI)
ANOVA27<- aov(modelo27) 
summary(ANOVA27)
TL8NDWI7<-grupos.varex1<-HSD.test(y=ANOVA27, trt = "Ano_7", group = T, console = T)

modelo37<-lm(S2NDVI_7 ~ Ano_7, data=S2NDVI)
ANOVA37<- aov(modelo37) 
summary(ANOVA37)
TS2NDVI7<-grupos.varex1<-HSD.test(y=ANOVA37, trt = "Ano_7", group = T, console = T)

modelo47<-lm(S2NDWI_7 ~ Ano_7, data=S2NDWI)
ANOVA47<- aov(modelo47) 
summary(ANOVA47)
TS2NDWI7<-grupos.varex1<-HSD.test(y=ANOVA47, trt = "Ano_7", group = T, console = T)

modelo57<-lm(SPOTNDVI_7 ~ Ano_7, data=SPOTNDVI)
ANOVA57<- aov(modelo57) 
summary(ANOVA57)
TSPOTNDVI7<-grupos.varex1<-HSD.test(y=ANOVA57, trt = "Ano_7", group = T, console = T)

modelo67<-lm(SPOTNDWI_7 ~ Ano_7, data=SPOTNDWI)
ANOVA67<- aov(modelo67) 
summary(ANOVA67)
TSPOTNDWI7<-grupos.varex1<-HSD.test(y=ANOVA67, trt = "Ano_7", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI7,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI7,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI7,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI7,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI7,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI7,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_7.png",height=18, width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 8
L8NDVI8 <- ggplot(data = L8NDVI, aes(x = Ano_8, y = L8NDVI_8, fill=Ano_8))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI8 <- ggplot(data = L8NDWI, aes(x = Ano_8, y = L8NDWI_8,fill=Ano_8))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI8 <- ggplot(data = S2NDWI, aes(x = Ano_8, y = S2NDWI_8,fill=Ano_8))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI8 <- ggplot(data = S2NDVI, aes(x = Ano_8, y = S2NDVI_8,fill=Ano_8))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI8 <- ggplot(data = SPOTNDVI, aes(x = Ano_8, y = SPOTNDVI_8,fill=Ano_8))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI8 <- ggplot(data = SPOTNDWI, aes(x = Ano_8, y = SPOTNDWI_8,fill=Ano_8))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI8,S2NDVI8,SPOTNDVI8,L8NDWI8,S2NDWI8,SPOTNDWI8, ncol=3, top=textGrob("UNIDAD 8", vjust=0.1))
ggsave(file="E:/grafico_8.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')


#HSD.test

modelo18<-lm(L8NDVI_8 ~ Ano_8, data=L8NDVI)
ANOVA18<- aov(modelo18) 
summary(ANOVA18)
TL8NDVI8<-grupos.varex1<-HSD.test(y=ANOVA18, trt = "Ano_8", group = T, console = T)

modelo28<-lm(L8NDWI_8 ~ Ano_8, data=L8NDWI)
ANOVA28<- aov(modelo28) 
summary(ANOVA28)
TL8NDWI8<-grupos.varex1<-HSD.test(y=ANOVA28, trt = "Ano_8", group = T, console = T)

modelo38<-lm(S2NDVI_8 ~ Ano_8, data=S2NDVI)
ANOVA38<- aov(modelo38) 
summary(ANOVA38)
TS2NDVI8<-grupos.varex1<-HSD.test(y=ANOVA38, trt = "Ano_8", group = T, console = T)

modelo48<-lm(S2NDWI_8 ~ Ano_8, data=S2NDWI)
ANOVA48<- aov(modelo48) 
summary(ANOVA48)
TS2NDWI8<-grupos.varex1<-HSD.test(y=ANOVA48, trt = "Ano_8", group = T, console = T)

modelo58<-lm(SPOTNDVI_8 ~ Ano_8, data=SPOTNDVI)
ANOVA58<- aov(modelo58) 
summary(ANOVA58)
TSPOTNDVI8<-grupos.varex1<-HSD.test(y=ANOVA58, trt = "Ano_8", group = T, console = T)

modelo68<-lm(SPOTNDWI_8 ~ Ano_8, data=SPOTNDWI)
ANOVA68<- aov(modelo68) 
summary(ANOVA68)
TSPOTNDWI8<-grupos.varex1<-HSD.test(y=ANOVA68, trt = "Ano_8", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI8,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI8,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI8,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI8,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI8,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI8,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_8.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 9

L8NDVI9 <- ggplot(data = L8NDVI, aes(x = Ano_9, y = L8NDVI_9, fill=Ano_9))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI9 <- ggplot(data = L8NDWI, aes(x = Ano_9, y = L8NDWI_9,fill=Ano_9))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI9 <- ggplot(data = S2NDWI, aes(x = Ano_9, y = S2NDWI_9,fill=Ano_9))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI9 <- ggplot(data = S2NDVI, aes(x = Ano_9, y = S2NDVI_9,fill=Ano_9))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI9 <- ggplot(data = SPOTNDVI, aes(x = Ano_9, y = SPOTNDVI_9,fill=Ano_9))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI9 <- ggplot(data = SPOTNDWI, aes(x = Ano_9, y = SPOTNDWI_9,fill=Ano_9))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI9,S2NDVI9,SPOTNDVI9,L8NDWI9,S2NDWI9,SPOTNDWI9, ncol=3, top=textGrob("UNIDAD 9", vjust=0.1))
ggsave(file="E:/grafico_9.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo19<-lm(L8NDVI_9 ~ Ano_9, data=L8NDVI)
ANOVA19<- aov(modelo19) 
summary(ANOVA19)
TL8NDVI9<-grupos.varex1<-HSD.test(y=ANOVA19, trt = "Ano_9", group = T, console = T)

modelo29<-lm(L8NDWI_9 ~ Ano_9, data=L8NDWI)
ANOVA29<- aov(modelo29) 
summary(ANOVA29)
TL8NDWI9<-grupos.varex1<-HSD.test(y=ANOVA29, trt = "Ano_9", group = T, console = T)

modelo39<-lm(S2NDVI_9 ~ Ano_9, data=S2NDVI)
ANOVA39<- aov(modelo39) 
summary(ANOVA39)
TS2NDVI9<-grupos.varex1<-HSD.test(y=ANOVA39, trt = "Ano_9", group = T, console = T)

modelo49<-lm(S2NDWI_9 ~ Ano_9, data=S2NDWI)
ANOVA49<- aov(modelo49) 
summary(ANOVA49)
TS2NDWI9<-grupos.varex1<-HSD.test(y=ANOVA49, trt = "Ano_9", group = T, console = T)

modelo59<-lm(SPOTNDVI_9 ~ Ano_9, data=SPOTNDVI)
ANOVA59<- aov(modelo59) 
summary(ANOVA59)
TSPOTNDVI9<-grupos.varex1<-HSD.test(y=ANOVA59, trt = "Ano_9", group = T, console = T)

modelo69<-lm(SPOTNDWI_9 ~ Ano_9, data=SPOTNDWI)
ANOVA69<- aov(modelo69) 
summary(ANOVA69)
TSPOTNDWI9<-grupos.varex1<-HSD.test(y=ANOVA69, trt = "Ano_9", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI9,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI9,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI9,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI9,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI9,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI9,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_9.png",height=18, width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 10
L8NDVI10 <- ggplot(data = L8NDVI, aes(x = Ano_10, y = L8NDVI_10, fill=Ano_10))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI10 <- ggplot(data = L8NDWI, aes(x = Ano_10, y = L8NDWI_10,fill=Ano_10))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI10 <- ggplot(data = S2NDWI, aes(x = Ano_10, y = S2NDWI_10,fill=Ano_10))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI10 <- ggplot(data = S2NDVI, aes(x = Ano_10, y = S2NDVI_10,fill=Ano_10))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI10 <- ggplot(data = SPOTNDVI, aes(x = Ano_10, y = SPOTNDVI_10,fill=Ano_10))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI10 <- ggplot(data = SPOTNDWI, aes(x = Ano_10, y = SPOTNDWI_10,fill=Ano_10))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI10,S2NDVI10,SPOTNDVI10,L8NDWI10,S2NDWI10,SPOTNDWI10, ncol=3, top=textGrob("UNIDAD 10", vjust=0.1))
ggsave(file="E:/grafico_10.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo110<-lm(L8NDVI_10 ~ Ano_10, data=L8NDVI)
ANOVA110<- aov(modelo110) 
summary(ANOVA110)
TL8NDVI10<-grupos.varex1<-HSD.test(y=ANOVA110, trt = "Ano_10", group = T, console = T)

modelo210<-lm(L8NDWI_10 ~ Ano_10, data=L8NDWI)
ANOVA210<- aov(modelo210) 
summary(ANOVA210)
TL8NDWI10<-grupos.varex1<-HSD.test(y=ANOVA210, trt = "Ano_10", group = T, console = T)

modelo310<-lm(S2NDVI_10 ~ Ano_10, data=S2NDVI)
ANOVA310<- aov(modelo310) 
summary(ANOVA310)
TS2NDVI10<-grupos.varex1<-HSD.test(y=ANOVA310, trt = "Ano_10", group = T, console = T)

modelo410<-lm(S2NDWI_10 ~ Ano_10, data=S2NDWI)
ANOVA410<- aov(modelo410) 
summary(ANOVA410)
TS2NDWI10<-grupos.varex1<-HSD.test(y=ANOVA410, trt = "Ano_10", group = T, console = T)

modelo510<-lm(SPOTNDVI_10 ~ Ano_10, data=SPOTNDVI)
ANOVA510<- aov(modelo510) 
summary(ANOVA510)
TSPOTNDVI10<-grupos.varex1<-HSD.test(y=ANOVA510, trt = "Ano_10", group = T, console = T)

modelo610<-lm(SPOTNDWI_10 ~ Ano_10, data=SPOTNDWI)
ANOVA610<- aov(modelo610) 
summary(ANOVA610)
TSPOTNDWI10<-grupos.varex1<-HSD.test(y=ANOVA610, trt = "Ano_10", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI10,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI10,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI10,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI10,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI10,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI10,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_10.png",height=18, width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 11

L8NDVI11 <- ggplot(data = L8NDVI, aes(x = Ano_11, y = L8NDVI_11, fill=Ano_11))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI11 <- ggplot(data = L8NDWI, aes(x = Ano_11, y = L8NDWI_11,fill=Ano_11))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI11 <- ggplot(data = S2NDWI, aes(x = Ano_11, y = S2NDWI_11,fill=Ano_11))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI11 <- ggplot(data = S2NDVI, aes(x = Ano_11, y = S2NDVI_11,fill=Ano_11))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI11 <- ggplot(data = SPOTNDVI, aes(x = Ano_11, y = SPOTNDVI_11,fill=Ano_11))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI11 <- ggplot(data = SPOTNDWI, aes(x = Ano_11, y = SPOTNDWI_11,fill=Ano_11))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI11,S2NDVI11,SPOTNDVI11,L8NDWI11,S2NDWI11,SPOTNDWI11, ncol=3, top=textGrob("UNIDAD 11", vjust=0.1))
ggsave(file="E:/grafico_11.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo111<-lm(L8NDVI_11 ~ Ano_11, data=L8NDVI)
ANOVA111<- aov(modelo111) 
summary(ANOVA111)
TL8NDVI11<-grupos.varex1<-HSD.test(y=ANOVA111, trt = "Ano_11", group = T, console = T)

modelo211<-lm(L8NDWI_11 ~ Ano_11, data=L8NDWI)
ANOVA211<- aov(modelo211) 
summary(ANOVA211)
TL8NDWI11<-grupos.varex1<-HSD.test(y=ANOVA211, trt = "Ano_11", group = T, console = T)

modelo311<-lm(S2NDVI_11 ~ Ano_11, data=S2NDVI)
ANOVA311<- aov(modelo311) 
summary(ANOVA311)
TS2NDVI11<-grupos.varex1<-HSD.test(y=ANOVA311, trt = "Ano_11", group = T, console = T)

modelo411<-lm(S2NDWI_11 ~ Ano_11, data=S2NDWI)
ANOVA411<- aov(modelo411) 
summary(ANOVA411)
TS2NDWI11<-grupos.varex1<-HSD.test(y=ANOVA411, trt = "Ano_11", group = T, console = T)

modelo511<-lm(SPOTNDVI_11 ~ Ano_11, data=SPOTNDVI)
ANOVA511<- aov(modelo511) 
summary(ANOVA511)
TSPOTNDVI11<-grupos.varex1<-HSD.test(y=ANOVA511, trt = "Ano_11", group = T, console = T)

modelo611<-lm(SPOTNDWI_11 ~ Ano_11, data=SPOTNDWI)
ANOVA611<- aov(modelo611) 
summary(ANOVA611)
TSPOTNDWI11<-grupos.varex1<-HSD.test(y=ANOVA611, trt = "Ano_11", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI11,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI11,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI11,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI11,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI11,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI11,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_11.png",height=18, width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 12
L8NDVI12 <- ggplot(data = L8NDVI, aes(x = Ano_12, y = L8NDVI_12, fill=Ano_12))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI12 <- ggplot(data = L8NDWI, aes(x = Ano_12, y = L8NDWI_12,fill=Ano_12))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI12 <- ggplot(data = S2NDWI, aes(x = Ano_12, y = S2NDWI_12,fill=Ano_12))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI12 <- ggplot(data = S2NDVI, aes(x = Ano_12, y = S2NDVI_12,fill=Ano_12))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI12 <- ggplot(data = SPOTNDVI, aes(x = Ano_12, y = SPOTNDVI_12,fill=Ano_12))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI12 <- ggplot(data = SPOTNDWI, aes(x = Ano_12, y = SPOTNDWI_12,fill=Ano_12))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI12,S2NDVI12,SPOTNDVI12,L8NDWI12,S2NDWI12,SPOTNDWI12, ncol=3, top=textGrob("UNIDAD 12", vjust=0.1))
ggsave(file="E:/grafico_12.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo112<-lm(L8NDVI_12 ~ Ano_12, data=L8NDVI)
ANOVA112<- aov(modelo112) 
summary(ANOVA112)
TL8NDVI12<-grupos.varex1<-HSD.test(y=ANOVA112, trt = "Ano_12", group = T, console = T)

modelo212<-lm(L8NDWI_12 ~ Ano_12, data=L8NDWI)
ANOVA212<- aov(modelo212) 
summary(ANOVA212)
TL8NDWI12<-grupos.varex1<-HSD.test(y=ANOVA212, trt = "Ano_12", group = T, console = T)

modelo312<-lm(S2NDVI_12 ~ Ano_12, data=S2NDVI)
ANOVA312<- aov(modelo312) 
summary(ANOVA312)
TS2NDVI12<-grupos.varex1<-HSD.test(y=ANOVA312, trt = "Ano_12", group = T, console = T)

modelo412<-lm(S2NDWI_12 ~ Ano_12, data=S2NDWI)
ANOVA412<- aov(modelo412) 
summary(ANOVA412)
TS2NDWI12<-grupos.varex1<-HSD.test(y=ANOVA412, trt = "Ano_12", group = T, console = T)

modelo512<-lm(SPOTNDVI_12 ~ Ano_12, data=SPOTNDVI)
ANOVA512<- aov(modelo512) 
summary(ANOVA512)
TSPOTNDVI12<-grupos.varex1<-HSD.test(y=ANOVA512, trt = "Ano_12", group = T, console = T)

modelo612<-lm(SPOTNDWI_12 ~ Ano_12, data=SPOTNDWI)
ANOVA612<- aov(modelo612) 
summary(ANOVA612)
TSPOTNDWI12<-grupos.varex1<-HSD.test(y=ANOVA612, trt = "Ano_12", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI12,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI12,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI12,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI12,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI12,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI12,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
dev.print(png, "E:/Tgrafico_12.png",height=18, width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 13
L8NDVI13 <- ggplot(data = L8NDVI, aes(x = Ano_13, y = L8NDVI_13, fill=Ano_13))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI13 <- ggplot(data = L8NDWI, aes(x = Ano_13, y = L8NDWI_13,fill=Ano_13))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI13 <- ggplot(data = S2NDWI, aes(x = Ano_13, y = S2NDWI_13,fill=Ano_13))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI13 <- ggplot(data = S2NDVI, aes(x = Ano_13, y = S2NDVI_13,fill=Ano_13))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI13 <- ggplot(data = SPOTNDVI, aes(x = Ano_13, y = SPOTNDVI_13,fill=Ano_13))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI13 <- ggplot(data = SPOTNDWI, aes(x = Ano_13, y = SPOTNDWI_13,fill=Ano_13))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI13,S2NDVI13,SPOTNDVI13,L8NDWI13,S2NDWI13,SPOTNDWI13, ncol=3, top=textGrob("UNIDAD 13", vjust=0.1))
ggsave(file="E:/grafico_13.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo113<-lm(L8NDVI_13 ~ Ano_13, data=L8NDVI)
ANOVA113<- aov(modelo113) 
summary(ANOVA113)
TL8NDVI13<-grupos.varex1<-HSD.test(y=ANOVA113, trt = "Ano_13", group = T, console = T)

modelo213<-lm(L8NDWI_13 ~ Ano_13, data=L8NDWI)
ANOVA213<- aov(modelo213) 
summary(ANOVA213)
TL8NDWI13<-grupos.varex1<-HSD.test(y=ANOVA213, trt = "Ano_13", group = T, console = T)

modelo313<-lm(S2NDVI_13 ~ Ano_13, data=S2NDVI)
ANOVA313<- aov(modelo313) 
summary(ANOVA313)
TS2NDVI13<-grupos.varex1<-HSD.test(y=ANOVA313, trt = "Ano_13", group = T, console = T)

modelo413<-lm(S2NDWI_13 ~ Ano_13, data=S2NDWI)
ANOVA413<- aov(modelo413) 
summary(ANOVA413)
TS2NDWI13<-grupos.varex1<-HSD.test(y=ANOVA413, trt = "Ano_13", group = T, console = T)

modelo513<-lm(SPOTNDVI_13 ~ Ano_13, data=SPOTNDVI)
ANOVA513<- aov(modelo513) 
summary(ANOVA513)
TSPOTNDVI13<-grupos.varex1<-HSD.test(y=ANOVA513, trt = "Ano_13", group = T, console = T)

modelo613<-lm(SPOTNDWI_13 ~ Ano_13, data=SPOTNDWI)
ANOVA613<- aov(modelo613) 
summary(ANOVA613)
TSPOTNDWI13<-grupos.varex1<-HSD.test(y=ANOVA613, trt = "Ano_13", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI13,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI13,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI13,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI13,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI13,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI13,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_13.png",height=18,width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 14
L8NDVI14 <- ggplot(data = L8NDVI, aes(x = Ano_14, y = L8NDVI_14, fill=Ano_14))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI14 <- ggplot(data = L8NDWI, aes(x = Ano_14, y = L8NDWI_14,fill=Ano_14))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI14 <- ggplot(data = S2NDWI, aes(x = Ano_14, y = S2NDWI_14,fill=Ano_14))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI14 <- ggplot(data = S2NDVI, aes(x = Ano_14, y = S2NDVI_14,fill=Ano_14))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI14 <- ggplot(data = SPOTNDVI, aes(x = Ano_14, y = SPOTNDVI_14,fill=Ano_14))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI14 <- ggplot(data = SPOTNDWI, aes(x = Ano_14, y = SPOTNDWI_14,fill=Ano_14))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI14,S2NDVI14,SPOTNDVI14,L8NDWI14,S2NDWI14,SPOTNDWI14, ncol=3, top=textGrob("UNIDAD 14", vjust=0.1))
ggsave(file="E:/grafico_14.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo114<-lm(L8NDVI_14 ~ Ano_14, data=L8NDVI)
ANOVA114<- aov(modelo114) 
summary(ANOVA114)
TL8NDVI14<-grupos.varex1<-HSD.test(y=ANOVA114, trt = "Ano_14", group = T, console = T)

modelo214<-lm(L8NDWI_14 ~ Ano_14, data=L8NDWI)
ANOVA214<- aov(modelo214) 
summary(ANOVA214)
TL8NDWI14<-grupos.varex1<-HSD.test(y=ANOVA214, trt = "Ano_14", group = T, console = T)

modelo314<-lm(S2NDVI_14 ~ Ano_14, data=S2NDVI)
ANOVA314<- aov(modelo314) 
summary(ANOVA314)
TS2NDVI14<-grupos.varex1<-HSD.test(y=ANOVA314, trt = "Ano_14", group = T, console = T)

modelo414<-lm(S2NDWI_14 ~ Ano_14, data=S2NDWI)
ANOVA414<- aov(modelo414) 
summary(ANOVA414)
TS2NDWI14<-grupos.varex1<-HSD.test(y=ANOVA414, trt = "Ano_14", group = T, console = T)

modelo514<-lm(SPOTNDVI_14 ~ Ano_14, data=SPOTNDVI)
ANOVA514<- aov(modelo514) 
summary(ANOVA514)
TSPOTNDVI14<-grupos.varex1<-HSD.test(y=ANOVA514, trt = "Ano_14", group = T, console = T)

modelo614<-lm(SPOTNDWI_14 ~ Ano_14, data=SPOTNDWI)
ANOVA614<- aov(modelo614) 
summary(ANOVA614)
TSPOTNDWI14<-grupos.varex1<-HSD.test(y=ANOVA614, trt = "Ano_14", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI14,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI14,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI14,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI14,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI14,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI14,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_14.png",height=18, width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 15

L8NDVI15 <- ggplot(data = L8NDVI, aes(x = Ano_15, y = L8NDVI_15, fill=Ano_15))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI15 <- ggplot(data = L8NDWI, aes(x = Ano_15, y = L8NDWI_15,fill=Ano_15))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI15 <- ggplot(data = S2NDWI, aes(x = Ano_15, y = S2NDWI_15,fill=Ano_15))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI15 <- ggplot(data = S2NDVI, aes(x = Ano_15, y = S2NDVI_15,fill=Ano_15))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI15 <- ggplot(data = SPOTNDVI, aes(x = Ano_15, y = SPOTNDVI_15,fill=Ano_15))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI15 <- ggplot(data = SPOTNDWI, aes(x = Ano_15, y = SPOTNDWI_15,fill=Ano_15))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI15,S2NDVI15,SPOTNDVI15,L8NDWI15,S2NDWI15,SPOTNDWI15, ncol=3, top=textGrob("UNIDAD 15", vjust=0.1))
ggsave(file="E:/grafico_15.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo115<-lm(L8NDVI_15 ~ Ano_15, data=L8NDVI)
ANOVA115<- aov(modelo115) 
summary(ANOVA115)
TL8NDVI15<-grupos.varex1<-HSD.test(y=ANOVA115, trt = "Ano_15", group = T, console = T)

modelo215<-lm(L8NDWI_15 ~ Ano_15, data=L8NDWI)
ANOVA215<- aov(modelo215) 
summary(ANOVA215)
TL8NDWI15<-grupos.varex1<-HSD.test(y=ANOVA215, trt = "Ano_15", group = T, console = T)

modelo315<-lm(S2NDVI_15 ~ Ano_15, data=S2NDVI)
ANOVA315<- aov(modelo315) 
summary(ANOVA315)
TS2NDVI15<-grupos.varex1<-HSD.test(y=ANOVA315, trt = "Ano_15", group = T, console = T)

modelo415<-lm(S2NDWI_15 ~ Ano_15, data=S2NDWI)
ANOVA415<- aov(modelo415) 
summary(ANOVA415)
TS2NDWI15<-grupos.varex1<-HSD.test(y=ANOVA415, trt = "Ano_15", group = T, console = T)

modelo515<-lm(SPOTNDVI_15 ~ Ano_15, data=SPOTNDVI)
ANOVA515<- aov(modelo515) 
summary(ANOVA515)
TSPOTNDVI15<-grupos.varex1<-HSD.test(y=ANOVA515, trt = "Ano_15", group = T, console = T)

modelo615<-lm(SPOTNDWI_15 ~ Ano_15, data=SPOTNDWI)
ANOVA615<- aov(modelo615) 
summary(ANOVA615)
TSPOTNDWI15<-grupos.varex1<-HSD.test(y=ANOVA615, trt = "Ano_15", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI15,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI15,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI15,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI15,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI15,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI15,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
dev.print(png, "E:/Tgrafico_15.png",height=18, width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 16
L8NDVI16 <- ggplot(data = L8NDVI, aes(x = Ano_16, y = L8NDVI_16, fill=Ano_16))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI16 <- ggplot(data = L8NDWI, aes(x = Ano_16, y = L8NDWI_16,fill=Ano_16))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI16 <- ggplot(data = S2NDWI, aes(x = Ano_16, y = S2NDWI_16,fill=Ano_16))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI16 <- ggplot(data = S2NDVI, aes(x = Ano_16, y = S2NDVI_16,fill=Ano_16))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI16 <- ggplot(data = SPOTNDVI, aes(x = Ano_16, y = SPOTNDVI_16,fill=Ano_16))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI16 <- ggplot(data = SPOTNDWI, aes(x = Ano_16, y = SPOTNDWI_16,fill=Ano_16))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI16,S2NDVI16,SPOTNDVI16,L8NDWI16,S2NDWI16,SPOTNDWI16, ncol=3, top=textGrob("UNIDAD 16", vjust=0.1))
ggsave(file="E:/grafico_16.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo116<-lm(L8NDVI_16 ~ Ano_16, data=L8NDVI)
ANOVA116<- aov(modelo116) 
summary(ANOVA116)
TL8NDVI16<-grupos.varex1<-HSD.test(y=ANOVA116, trt = "Ano_16", group = T, console = T)

modelo216<-lm(L8NDWI_16 ~ Ano_16, data=L8NDWI)
ANOVA216<- aov(modelo216) 
summary(ANOVA216)
TL8NDWI16<-grupos.varex1<-HSD.test(y=ANOVA216, trt = "Ano_16", group = T, console = T)

modelo316<-lm(S2NDVI_16 ~ Ano_16, data=S2NDVI)
ANOVA316<- aov(modelo316) 
summary(ANOVA316)
TS2NDVI16<-grupos.varex1<-HSD.test(y=ANOVA316, trt = "Ano_16", group = T, console = T)

modelo416<-lm(S2NDWI_16 ~ Ano_16, data=S2NDWI)
ANOVA416<- aov(modelo416) 
summary(ANOVA416)
TS2NDWI16<-grupos.varex1<-HSD.test(y=ANOVA416, trt = "Ano_16", group = T, console = T)

modelo516<-lm(SPOTNDVI_16 ~ Ano_16, data=SPOTNDVI)
ANOVA516<- aov(modelo516) 
summary(ANOVA516)
TSPOTNDVI16<-grupos.varex1<-HSD.test(y=ANOVA516, trt = "Ano_16", group = T, console = T)

modelo616<-lm(SPOTNDWI_16 ~ Ano_16, data=SPOTNDWI)
ANOVA616<- aov(modelo616) 
summary(ANOVA616)
TSPOTNDWI16<-grupos.varex1<-HSD.test(y=ANOVA616, trt = "Ano_16", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI16,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI16,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI16,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI16,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI16,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI16,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
dev.print(png, "E:/Tgrafico_16.png",height=18, width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 17
L8NDVI17 <- ggplot(data = L8NDVI, aes(x = Ano_17, y = L8NDVI_17, fill=Ano_17))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI17 <- ggplot(data = L8NDWI, aes(x = Ano_17, y = L8NDWI_17,fill=Ano_17))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI17 <- ggplot(data = S2NDWI, aes(x = Ano_17, y = S2NDWI_17,fill=Ano_17))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI17 <- ggplot(data = S2NDVI, aes(x = Ano_17, y = S2NDVI_17,fill=Ano_17))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI17 <- ggplot(data = SPOTNDVI, aes(x = Ano_17, y = SPOTNDVI_17,fill=Ano_17))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI17 <- ggplot(data = SPOTNDWI, aes(x = Ano_17, y = SPOTNDWI_17,fill=Ano_17))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI17,S2NDVI17,SPOTNDVI17,L8NDWI17,S2NDWI17,SPOTNDWI17, ncol=3, top=textGrob("UNIDAD 17", vjust=0.1))
ggsave(file="E:/grafico_17.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')


#HSD.test

modelo117<-lm(L8NDVI_17 ~ Ano_17, data=L8NDVI)
ANOVA117<- aov(modelo117) 
summary(ANOVA117)
TL8NDVI17<-grupos.varex1<-HSD.test(y=ANOVA117, trt = "Ano_17", group = T, console = T)

modelo217<-lm(L8NDWI_17 ~ Ano_17, data=L8NDWI)
ANOVA217<- aov(modelo217) 
summary(ANOVA217)
TL8NDWI17<-grupos.varex1<-HSD.test(y=ANOVA217, trt = "Ano_17", group = T, console = T)

modelo317<-lm(S2NDVI_17 ~ Ano_17, data=S2NDVI)
ANOVA317<- aov(modelo317) 
summary(ANOVA317)
TS2NDVI17<-grupos.varex1<-HSD.test(y=ANOVA317, trt = "Ano_17", group = T, console = T)

modelo417<-lm(S2NDWI_17 ~ Ano_17, data=S2NDWI)
ANOVA417<- aov(modelo417) 
summary(ANOVA417)
TS2NDWI17<-grupos.varex1<-HSD.test(y=ANOVA417, trt = "Ano_17", group = T, console = T)

modelo517<-lm(SPOTNDVI_17 ~ Ano_17, data=SPOTNDVI)
ANOVA517<- aov(modelo517) 
summary(ANOVA517)
TSPOTNDVI17<-grupos.varex1<-HSD.test(y=ANOVA517, trt = "Ano_17", group = T, console = T)

modelo617<-lm(SPOTNDWI_17 ~ Ano_17, data=SPOTNDWI)
ANOVA617<- aov(modelo617) 
summary(ANOVA617)
TSPOTNDWI17<-grupos.varex1<-HSD.test(y=ANOVA617, trt = "Ano_17", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI17,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI17,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI17,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI17,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI17,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI17,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_17.png",height=18, width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 18
L8NDVI18 <- ggplot(data = L8NDVI, aes(x = Ano_18, y = L8NDVI_18, fill=Ano_18))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI18 <- ggplot(data = L8NDWI, aes(x = Ano_18, y = L8NDWI_18,fill=Ano_18))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI18 <- ggplot(data = S2NDWI, aes(x = Ano_18, y = S2NDWI_18,fill=Ano_18))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI18 <- ggplot(data = S2NDVI, aes(x = Ano_18, y = S2NDVI_18,fill=Ano_18))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI18 <- ggplot(data = SPOTNDVI, aes(x = Ano_18, y = SPOTNDVI_18,fill=Ano_18))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI18 <- ggplot(data = SPOTNDWI, aes(x = Ano_18, y = SPOTNDWI_18,fill=Ano_18))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI18,S2NDVI18,SPOTNDVI18,L8NDWI18,S2NDWI18,SPOTNDWI18, ncol=3, top=textGrob("UNIDAD 18", vjust=0.1))
ggsave(file="E:/grafico_18.png", graf,width=16, height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo118<-lm(L8NDVI_18 ~ Ano_18, data=L8NDVI)
ANOVA118<- aov(modelo118) 
summary(ANOVA118)
TL8NDVI18<-grupos.varex1<-HSD.test(y=ANOVA118, trt = "Ano_18", group = T, console = T)

modelo218<-lm(L8NDWI_18 ~ Ano_18, data=L8NDWI)
ANOVA218<- aov(modelo218) 
summary(ANOVA218)
TL8NDWI18<-grupos.varex1<-HSD.test(y=ANOVA218, trt = "Ano_18", group = T, console = T)

modelo318<-lm(S2NDVI_18 ~ Ano_18, data=S2NDVI)
ANOVA318<- aov(modelo318) 
summary(ANOVA318)
TS2NDVI18<-grupos.varex1<-HSD.test(y=ANOVA318, trt = "Ano_18", group = T, console = T)

modelo418<-lm(S2NDWI_18 ~ Ano_18, data=S2NDWI)
ANOVA418<- aov(modelo418) 
summary(ANOVA418)
TS2NDWI18<-grupos.varex1<-HSD.test(y=ANOVA418, trt = "Ano_18", group = T, console = T)

modelo518<-lm(SPOTNDVI_18 ~ Ano_18, data=SPOTNDVI)
ANOVA518<- aov(modelo518) 
summary(ANOVA518)
TSPOTNDVI18<-grupos.varex1<-HSD.test(y=ANOVA518, trt = "Ano_18", group = T, console = T)

modelo618<-lm(SPOTNDWI_18 ~ Ano_18, data=SPOTNDWI)
ANOVA618<- aov(modelo618) 
summary(ANOVA618)
TSPOTNDWI18<-grupos.varex1<-HSD.test(y=ANOVA618, trt = "Ano_18", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI18,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI18,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI18,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI18,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI18,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI18,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_18.png",height=18, width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 19
L8NDVI19 <- ggplot(data = L8NDVI, aes(x = Ano_19, y = L8NDVI_19, fill=Ano_19))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI19 <- ggplot(data = L8NDWI, aes(x = Ano_19, y = L8NDWI_19,fill=Ano_19))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI19 <- ggplot(data = S2NDWI, aes(x = Ano_19, y = S2NDWI_19,fill=Ano_19))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI19 <- ggplot(data = S2NDVI, aes(x = Ano_19, y = S2NDVI_19,fill=Ano_19))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI19 <- ggplot(data = SPOTNDVI, aes(x = Ano_19, y = SPOTNDVI_19,fill=Ano_19))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI19 <- ggplot(data = SPOTNDWI, aes(x = Ano_19, y = SPOTNDWI_19,fill=Ano_19))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI19,S2NDVI19,SPOTNDVI19,L8NDWI19,S2NDWI19,SPOTNDWI19, ncol=3, top=textGrob("UNIDAD 19", vjust=0.1))
ggsave(file="E:/grafico_19.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo119<-lm(L8NDVI_19 ~ Ano_19, data=L8NDVI)
ANOVA119<- aov(modelo119) 
summary(ANOVA119)
TL8NDVI19<-grupos.varex1<-HSD.test(y=ANOVA119, trt = "Ano_19", group = T, console = T)

modelo219<-lm(L8NDWI_19 ~ Ano_19, data=L8NDWI)
ANOVA219<- aov(modelo219) 
summary(ANOVA219)
TL8NDWI19<-grupos.varex1<-HSD.test(y=ANOVA219, trt = "Ano_19", group = T, console = T)

modelo319<-lm(S2NDVI_19 ~ Ano_19, data=S2NDVI)
ANOVA319<- aov(modelo319) 
summary(ANOVA319)
TS2NDVI19<-grupos.varex1<-HSD.test(y=ANOVA319, trt = "Ano_19", group = T, console = T)

modelo419<-lm(S2NDWI_19 ~ Ano_19, data=S2NDWI)
ANOVA419<- aov(modelo419) 
summary(ANOVA419)
TS2NDWI19<-grupos.varex1<-HSD.test(y=ANOVA419, trt = "Ano_19", group = T, console = T)

modelo519<-lm(SPOTNDVI_19 ~ Ano_19, data=SPOTNDVI)
ANOVA519<- aov(modelo519) 
summary(ANOVA519)
TSPOTNDVI19<-grupos.varex1<-HSD.test(y=ANOVA519, trt = "Ano_19", group = T, console = T)

modelo619<-lm(SPOTNDWI_19 ~ Ano_19, data=SPOTNDWI)
ANOVA619<- aov(modelo619) 
summary(ANOVA619)
TSPOTNDWI19<-grupos.varex1<-HSD.test(y=ANOVA619, trt = "Ano_19", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI19,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI19,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI19,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI19,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI19,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI19,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_19.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 20

L8NDVI20 <- ggplot(data = L8NDVI, aes(x = Ano_20, y = L8NDVI_20, fill=Ano_20))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI20 <- ggplot(data = L8NDWI, aes(x = Ano_20, y = L8NDWI_20,fill=Ano_20))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI20 <- ggplot(data = S2NDWI, aes(x = Ano_20, y = S2NDWI_20,fill=Ano_20))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI20 <- ggplot(data = S2NDVI, aes(x = Ano_20, y = S2NDVI_20,fill=Ano_20))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI20 <- ggplot(data = SPOTNDVI, aes(x = Ano_20, y = SPOTNDVI_20,fill=Ano_20))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI20 <- ggplot(data = SPOTNDWI, aes(x = Ano_20, y = SPOTNDWI_20,fill=Ano_20))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI20,S2NDVI20,SPOTNDVI20,L8NDWI20,S2NDWI20,SPOTNDWI20, ncol=3, top=textGrob("UNIDAD 20", vjust=0.1))
ggsave(file="E:/grafico_20.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo120<-lm(L8NDVI_20 ~ Ano_20, data=L8NDVI)
ANOVA120<- aov(modelo120) 
summary(ANOVA120)
TL8NDVI20<-grupos.varex1<-HSD.test(y=ANOVA120, trt = "Ano_20", group = T, console = T)

modelo220<-lm(L8NDWI_20 ~ Ano_20, data=L8NDWI)
ANOVA220<- aov(modelo220) 
summary(ANOVA220)
TL8NDWI20<-grupos.varex1<-HSD.test(y=ANOVA220, trt = "Ano_20", group = T, console = T)

modelo320<-lm(S2NDVI_20 ~ Ano_20, data=S2NDVI)
ANOVA320<- aov(modelo320) 
summary(ANOVA320)
TS2NDVI20<-grupos.varex1<-HSD.test(y=ANOVA320, trt = "Ano_20", group = T, console = T)

modelo420<-lm(S2NDWI_20 ~ Ano_20, data=S2NDWI)
ANOVA420<- aov(modelo420) 
summary(ANOVA420)
TS2NDWI20<-grupos.varex1<-HSD.test(y=ANOVA420, trt = "Ano_20", group = T, console = T)

modelo520<-lm(SPOTNDVI_20 ~ Ano_20, data=SPOTNDVI)
ANOVA520<- aov(modelo520) 
summary(ANOVA520)
TSPOTNDVI20<-grupos.varex1<-HSD.test(y=ANOVA520, trt = "Ano_20", group = T, console = T)

modelo620<-lm(SPOTNDWI_20 ~ Ano_20, data=SPOTNDWI)
ANOVA620<- aov(modelo620) 
summary(ANOVA620)
TSPOTNDWI20<-grupos.varex1<-HSD.test(y=ANOVA620, trt = "Ano_20", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI20,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI20,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI20,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI20,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI20,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI20,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_20.png",height=18,width=22, units = "cm", res = 400)
dev.off()


#UNIDAD 21
L8NDVI21 <- ggplot(data = L8NDVI, aes(x = Ano_21, y = L8NDVI_21, fill=Ano_21))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI21 <- ggplot(data = L8NDWI, aes(x = Ano_21, y = L8NDWI_21,fill=Ano_21))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI21 <- ggplot(data = S2NDWI, aes(x = Ano_21, y = S2NDWI_21,fill=Ano_21))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI21 <- ggplot(data = S2NDVI, aes(x = Ano_21, y = S2NDVI_21,fill=Ano_21))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI21 <- ggplot(data = SPOTNDVI, aes(x = Ano_21, y = SPOTNDVI_21,fill=Ano_21))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI21 <- ggplot(data = SPOTNDWI, aes(x = Ano_21, y = SPOTNDWI_21,fill=Ano_21))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI21,S2NDVI21,SPOTNDVI21,L8NDWI21,S2NDWI21,SPOTNDWI21, ncol=3, top=textGrob("UNIDAD 21", vjust=0.1))
ggsave(file="E:/grafico_21.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo121<-lm(L8NDVI_21 ~ Ano_21, data=L8NDVI)
ANOVA121<- aov(modelo121) 
summary(ANOVA121)
TL8NDVI21<-grupos.varex1<-HSD.test(y=ANOVA121, trt = "Ano_21", group = T, console = T)

modelo221<-lm(L8NDWI_21 ~ Ano_21, data=L8NDWI)
ANOVA221<- aov(modelo221) 
summary(ANOVA221)
TL8NDWI21<-grupos.varex1<-HSD.test(y=ANOVA221, trt = "Ano_21", group = T, console = T)

modelo321<-lm(S2NDVI_21 ~ Ano_21, data=S2NDVI)
ANOVA321<- aov(modelo321) 
summary(ANOVA321)
TS2NDVI21<-grupos.varex1<-HSD.test(y=ANOVA321, trt = "Ano_21", group = T, console = T)

modelo421<-lm(S2NDWI_21 ~ Ano_21, data=S2NDWI)
ANOVA421<- aov(modelo421) 
summary(ANOVA421)
TS2NDWI21<-grupos.varex1<-HSD.test(y=ANOVA421, trt = "Ano_21", group = T, console = T)

modelo521<-lm(SPOTNDVI_21 ~ Ano_21, data=SPOTNDVI)
ANOVA521<- aov(modelo521) 
summary(ANOVA521)
TSPOTNDVI21<-grupos.varex1<-HSD.test(y=ANOVA521, trt = "Ano_21", group = T, console = T)

modelo621<-lm(SPOTNDWI_21 ~ Ano_21, data=SPOTNDWI)
ANOVA621<- aov(modelo621) 
summary(ANOVA621)
TSPOTNDWI21<-grupos.varex1<-HSD.test(y=ANOVA621, trt = "Ano_21", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI21,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI21,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI21,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI21,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI21,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI21,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_21.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 22
L8NDWI22 <- ggplot(data = L8NDWI, aes(x = Ano_22, y = L8NDWI_22,fill=Ano_22))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI22 <- ggplot(data = S2NDWI, aes(x = Ano_22, y = S2NDWI_22,fill=Ano_22))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI22 <- ggplot(data = S2NDVI, aes(x = Ano_22, y = S2NDVI_22,fill=Ano_22))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI22 <- ggplot(data = SPOTNDVI, aes(x = Ano_22, y = SPOTNDVI_22,fill=Ano_22))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI22 <- ggplot(data = SPOTNDWI, aes(x = Ano_22, y = SPOTNDWI_22,fill=Ano_22))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI22,S2NDVI22,SPOTNDVI22,L8NDWI22,S2NDWI22,SPOTNDWI22, ncol=3, top=textGrob("UNIDAD 22", vjust=0.1))
ggsave(file="E:/grafico_22.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo122<-lm(L8NDVI_22 ~ Ano_22, data=L8NDVI)
ANOVA122<- aov(modelo122) 
summary(ANOVA122)
TL8NDVI22<-grupos.varex1<-HSD.test(y=ANOVA122, trt = "Ano_22", group = T, console = T)

modelo222<-lm(L8NDWI_22 ~ Ano_22, data=L8NDWI)
ANOVA222<- aov(modelo222) 
summary(ANOVA222)
TL8NDWI22<-grupos.varex1<-HSD.test(y=ANOVA222, trt = "Ano_22", group = T, console = T)

modelo322<-lm(S2NDVI_22 ~ Ano_22, data=S2NDVI)
ANOVA322<- aov(modelo322) 
summary(ANOVA322)
TS2NDVI22<-grupos.varex1<-HSD.test(y=ANOVA322, trt = "Ano_22", group = T, console = T)

modelo422<-lm(S2NDWI_22 ~ Ano_22, data=S2NDWI)
ANOVA422<- aov(modelo422) 
summary(ANOVA422)
TS2NDWI22<-grupos.varex1<-HSD.test(y=ANOVA422, trt = "Ano_22", group = T, console = T)

modelo522<-lm(SPOTNDVI_22 ~ Ano_22, data=SPOTNDVI)
ANOVA522<- aov(modelo522) 
summary(ANOVA522)
TSPOTNDVI22<-grupos.varex1<-HSD.test(y=ANOVA522, trt = "Ano_22", group = T, console = T)

modelo622<-lm(SPOTNDWI_22 ~ Ano_22, data=SPOTNDWI)
ANOVA622<- aov(modelo622) 
summary(ANOVA622)
TSPOTNDWI22<-grupos.varex1<-HSD.test(y=ANOVA622, trt = "Ano_22", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI22,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI22,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI22,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI22,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI22,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI22,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_22.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 23

L8NDVI23 <- ggplot(data = L8NDVI, aes(x = Ano_23, y = L8NDVI_23, fill=Ano_23))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI23 <- ggplot(data = L8NDWI, aes(x = Ano_23, y = L8NDWI_23,fill=Ano_23))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI23 <- ggplot(data = S2NDWI, aes(x = Ano_23, y = S2NDWI_23,fill=Ano_23))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI23 <- ggplot(data = S2NDVI, aes(x = Ano_23, y = S2NDVI_23,fill=Ano_23))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI23 <- ggplot(data = SPOTNDVI, aes(x = Ano_23, y = SPOTNDVI_23,fill=Ano_23))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI23 <- ggplot(data = SPOTNDWI, aes(x = Ano_23, y = SPOTNDWI_23,fill=Ano_23))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI23,S2NDVI23,SPOTNDVI23,L8NDWI23,S2NDWI23,SPOTNDWI23, ncol=3, top=textGrob("UNIDAD 23", vjust=0.1))
ggsave(file="E:/grafico_23.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo123<-lm(L8NDVI_23 ~ Ano_23, data=L8NDVI)
ANOVA123<- aov(modelo123) 
summary(ANOVA123)
TL8NDVI23<-grupos.varex1<-HSD.test(y=ANOVA123, trt = "Ano_23", group = T, console = T)

modelo232<-lm(L8NDWI_23 ~ Ano_23, data=L8NDWI)
ANOVA232<- aov(modelo232) 
summary(ANOVA232)
TL8NDWI23<-grupos.varex1<-HSD.test(y=ANOVA232, trt = "Ano_23", group = T, console = T)

modelo323<-lm(S2NDVI_23 ~ Ano_23, data=S2NDVI)
ANOVA323<- aov(modelo323) 
summary(ANOVA323)
TS2NDVI23<-grupos.varex1<-HSD.test(y=ANOVA323, trt = "Ano_23", group = T, console = T)

modelo423<-lm(S2NDWI_23 ~ Ano_23, data=S2NDWI)
ANOVA423<- aov(modelo423) 
summary(ANOVA423)
TS2NDWI23<-grupos.varex1<-HSD.test(y=ANOVA423, trt = "Ano_23", group = T, console = T)

modelo523<-lm(SPOTNDVI_23 ~ Ano_23, data=SPOTNDVI)
ANOVA523<- aov(modelo523) 
summary(ANOVA523)
TSPOTNDVI23<-grupos.varex1<-HSD.test(y=ANOVA523, trt = "Ano_23", group = T, console = T)

modelo623<-lm(SPOTNDWI_23 ~ Ano_23, data=SPOTNDWI)
ANOVA623<- aov(modelo623) 
summary(ANOVA623)
TSPOTNDWI23<-grupos.varex1<-HSD.test(y=ANOVA623, trt = "Ano_23", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI23,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI23,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI23,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI23,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI23,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI23,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_23.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 24
#UNIDAD 24
L8NDVI24 <- ggplot(data = L8NDVI, aes(x = Ano_24, y = L8NDVI_24, fill=Ano_24))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI24 <- ggplot(data = L8NDWI, aes(x = Ano_24, y = L8NDWI_24,fill=Ano_24))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI24 <- ggplot(data = S2NDWI, aes(x = Ano_24, y = S2NDWI_24,fill=Ano_24))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI24 <- ggplot(data = S2NDVI, aes(x = Ano_24, y = S2NDVI_24,fill=Ano_24))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI24 <- ggplot(data = SPOTNDVI, aes(x = Ano_24, y = SPOTNDVI_24,fill=Ano_24))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI24 <- ggplot(data = SPOTNDWI, aes(x = Ano_24, y = SPOTNDWI_24,fill=Ano_24))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI24,S2NDVI24,SPOTNDVI24,L8NDWI24,S2NDWI24,SPOTNDWI24, ncol=3, top=textGrob("UNIDAD 24", vjust=0.1))
ggsave(file="E:/grafico_24.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo124<-lm(L8NDVI_24 ~ Ano_24, data=L8NDVI)
ANOVA124<- aov(modelo124) 
summary(ANOVA124)
TL8NDVI24<-grupos.varex1<-HSD.test(y=ANOVA124, trt = "Ano_24", group = T, console = T)

modelo242<-lm(L8NDWI_24 ~ Ano_24, data=L8NDWI)
ANOVA242<- aov(modelo242) 
summary(ANOVA242)
TL8NDWI24<-grupos.varex1<-HSD.test(y=ANOVA242, trt = "Ano_24", group = T, console = T)

modelo324<-lm(S2NDVI_24 ~ Ano_24, data=S2NDVI)
ANOVA324<- aov(modelo324) 
summary(ANOVA324)
TS2NDVI24<-grupos.varex1<-HSD.test(y=ANOVA324, trt = "Ano_24", group = T, console = T)

modelo424<-lm(S2NDWI_24 ~ Ano_24, data=S2NDWI)
ANOVA424<- aov(modelo424) 
summary(ANOVA424)
TS2NDWI24<-grupos.varex1<-HSD.test(y=ANOVA424, trt = "Ano_24", group = T, console = T)

modelo524<-lm(SPOTNDVI_24 ~ Ano_24, data=SPOTNDVI)
ANOVA524<- aov(modelo524) 
summary(ANOVA524)
TSPOTNDVI24<-grupos.varex1<-HSD.test(y=ANOVA524, trt = "Ano_24", group = T, console = T)

modelo624<-lm(SPOTNDWI_24 ~ Ano_24, data=SPOTNDWI)
ANOVA624<- aov(modelo624) 
summary(ANOVA624)
TSPOTNDWI24<-grupos.varex1<-HSD.test(y=ANOVA624, trt = "Ano_24", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI24,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI24,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI24,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI24,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI24,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI24,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_24.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 25
L8NDVI25 <- ggplot(data = L8NDVI, aes(x = Ano_25, y = L8NDVI_25, fill=Ano_25))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI25 <- ggplot(data = L8NDWI, aes(x = Ano_25, y = L8NDWI_25,fill=Ano_25))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI25 <- ggplot(data = S2NDWI, aes(x = Ano_25, y = S2NDWI_25,fill=Ano_25))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI25 <- ggplot(data = S2NDVI, aes(x = Ano_25, y = S2NDVI_25,fill=Ano_25))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI25 <- ggplot(data = SPOTNDVI, aes(x = Ano_25, y = SPOTNDVI_25,fill=Ano_25))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI25 <- ggplot(data = SPOTNDWI, aes(x = Ano_25, y = SPOTNDWI_25,fill=Ano_25))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI25,S2NDVI25,SPOTNDVI25,L8NDWI25,S2NDWI25,SPOTNDWI25, ncol=3, top=textGrob("UNIDAD 25", vjust=0.1))
ggsave(file="E:/grafico_25.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo125<-lm(L8NDVI_25 ~ Ano_25, data=L8NDVI)
ANOVA125<- aov(modelo125) 
summary(ANOVA125)
TL8NDVI25<-grupos.varex1<-HSD.test(y=ANOVA125, trt = "Ano_25", group = T, console = T)

modelo252<-lm(L8NDWI_25 ~ Ano_25, data=L8NDWI)
ANOVA252<- aov(modelo252) 
summary(ANOVA252)
TL8NDWI25<-grupos.varex1<-HSD.test(y=ANOVA252, trt = "Ano_25", group = T, console = T)

modelo325<-lm(S2NDVI_25 ~ Ano_25, data=S2NDVI)
ANOVA325<- aov(modelo325) 
summary(ANOVA325)
TS2NDVI25<-grupos.varex1<-HSD.test(y=ANOVA325, trt = "Ano_25", group = T, console = T)

modelo425<-lm(S2NDWI_25 ~ Ano_25, data=S2NDWI)
ANOVA425<- aov(modelo425) 
summary(ANOVA425)
TS2NDWI25<-grupos.varex1<-HSD.test(y=ANOVA425, trt = "Ano_25", group = T, console = T)

modelo525<-lm(SPOTNDVI_25 ~ Ano_25, data=SPOTNDVI)
ANOVA525<- aov(modelo525) 
summary(ANOVA525)
TSPOTNDVI25<-grupos.varex1<-HSD.test(y=ANOVA525, trt = "Ano_25", group = T, console = T)

modelo625<-lm(SPOTNDWI_25 ~ Ano_25, data=SPOTNDWI)
ANOVA625<- aov(modelo625) 
summary(ANOVA625)
TSPOTNDWI25<-grupos.varex1<-HSD.test(y=ANOVA625, trt = "Ano_25", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI25,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI25,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI25,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI25,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI25,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI25,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_25.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 26
L8NDVI26 <- ggplot(data = L8NDVI, aes(x = Ano_26, y = L8NDVI_26, fill=Ano_26))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI26 <- ggplot(data = L8NDWI, aes(x = Ano_26, y = L8NDWI_26,fill=Ano_26))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI26 <- ggplot(data = S2NDWI, aes(x = Ano_26, y = S2NDWI_26,fill=Ano_26))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI26 <- ggplot(data = S2NDVI, aes(x = Ano_26, y = S2NDVI_26,fill=Ano_26))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI26 <- ggplot(data = SPOTNDVI, aes(x = Ano_26, y = SPOTNDVI_26,fill=Ano_26))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI26 <- ggplot(data = SPOTNDWI, aes(x = Ano_26, y = SPOTNDWI_26,fill=Ano_26))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI26,S2NDVI26,SPOTNDVI26,L8NDWI26,S2NDWI26,SPOTNDWI26, ncol=3, top=textGrob("UNIDAD 26", vjust=0.1))
ggsave(file="E:/grafico_26.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo126<-lm(L8NDVI_26 ~ Ano_26, data=L8NDVI)
ANOVA126<- aov(modelo126) 
summary(ANOVA126)
TL8NDVI26<-grupos.varex1<-HSD.test(y=ANOVA126, trt = "Ano_26", group = T, console = T)

modelo262<-lm(L8NDWI_26 ~ Ano_26, data=L8NDWI)
ANOVA262<- aov(modelo262) 
summary(ANOVA262)
TL8NDWI26<-grupos.varex1<-HSD.test(y=ANOVA262, trt = "Ano_26", group = T, console = T)

modelo326<-lm(S2NDVI_26 ~ Ano_26, data=S2NDVI)
ANOVA326<- aov(modelo326) 
summary(ANOVA326)
TS2NDVI26<-grupos.varex1<-HSD.test(y=ANOVA326, trt = "Ano_26", group = T, console = T)

modelo426<-lm(S2NDWI_26 ~ Ano_26, data=S2NDWI)
ANOVA426<- aov(modelo426) 
summary(ANOVA426)
TS2NDWI26<-grupos.varex1<-HSD.test(y=ANOVA426, trt = "Ano_26", group = T, console = T)

modelo526<-lm(SPOTNDVI_26 ~ Ano_26, data=SPOTNDVI)
ANOVA526<- aov(modelo526) 
summary(ANOVA526)
TSPOTNDVI26<-grupos.varex1<-HSD.test(y=ANOVA526, trt = "Ano_26", group = T, console = T)

modelo626<-lm(SPOTNDWI_26 ~ Ano_26, data=SPOTNDWI)
ANOVA626<- aov(modelo626) 
summary(ANOVA626)
TSPOTNDWI26<-grupos.varex1<-HSD.test(y=ANOVA626, trt = "Ano_26", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI26,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI26,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI26,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI26,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI26,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI26,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_26.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 27
L8NDVI27 <- ggplot(data = L8NDVI, aes(x = Ano_27, y = L8NDVI_27, fill=Ano_27))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI27 <- ggplot(data = L8NDWI, aes(x = Ano_27, y = L8NDWI_27,fill=Ano_27))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI27 <- ggplot(data = S2NDWI, aes(x = Ano_27, y = S2NDWI_27,fill=Ano_27))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI27 <- ggplot(data = S2NDVI, aes(x = Ano_27, y = S2NDVI_27,fill=Ano_27))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI27 <- ggplot(data = SPOTNDVI, aes(x = Ano_27, y = SPOTNDVI_27,fill=Ano_27))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI27 <- ggplot(data = SPOTNDWI, aes(x = Ano_27, y = SPOTNDWI_27,fill=Ano_27))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI27,S2NDVI27,SPOTNDVI27,L8NDWI27,S2NDWI27,SPOTNDWI27, ncol=3, top=textGrob("UNIDAD 27", vjust=0.1))
ggsave(file="E:/grafico_27.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo127<-lm(L8NDVI_27 ~ Ano_27, data=L8NDVI)
ANOVA127<- aov(modelo127) 
summary(ANOVA127)
TL8NDVI27<-grupos.varex1<-HSD.test(y=ANOVA127, trt = "Ano_27", group = T, console = T)

modelo272<-lm(L8NDWI_27 ~ Ano_27, data=L8NDWI)
ANOVA272<- aov(modelo272) 
summary(ANOVA272)
TL8NDWI27<-grupos.varex1<-HSD.test(y=ANOVA272, trt = "Ano_27", group = T, console = T)

modelo327<-lm(S2NDVI_27 ~ Ano_27, data=S2NDVI)
ANOVA327<- aov(modelo327) 
summary(ANOVA327)
TS2NDVI27<-grupos.varex1<-HSD.test(y=ANOVA327, trt = "Ano_27", group = T, console = T)

modelo427<-lm(S2NDWI_27 ~ Ano_27, data=S2NDWI)
ANOVA427<- aov(modelo427) 
summary(ANOVA427)
TS2NDWI27<-grupos.varex1<-HSD.test(y=ANOVA427, trt = "Ano_27", group = T, console = T)

modelo527<-lm(SPOTNDVI_27 ~ Ano_27, data=SPOTNDVI)
ANOVA527<- aov(modelo527) 
summary(ANOVA527)
TSPOTNDVI27<-grupos.varex1<-HSD.test(y=ANOVA527, trt = "Ano_27", group = T, console = T)

modelo627<-lm(SPOTNDWI_27 ~ Ano_27, data=SPOTNDWI)
ANOVA627<- aov(modelo627) 
summary(ANOVA627)
TSPOTNDWI27<-grupos.varex1<-HSD.test(y=ANOVA627, trt = "Ano_27", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI27,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI27,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI27,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI27,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI27,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI27,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_27.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 28
L8NDVI28 <- ggplot(data = L8NDVI, aes(x = Ano_28, y = L8NDVI_28, fill=Ano_28))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI28 <- ggplot(data = L8NDWI, aes(x = Ano_28, y = L8NDWI_28,fill=Ano_28))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI28 <- ggplot(data = S2NDWI, aes(x = Ano_28, y = S2NDWI_28,fill=Ano_28))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI28 <- ggplot(data = S2NDVI, aes(x = Ano_28, y = S2NDVI_28,fill=Ano_28))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI28 <- ggplot(data = SPOTNDVI, aes(x = Ano_28, y = SPOTNDVI_28,fill=Ano_28))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI28 <- ggplot(data = SPOTNDWI, aes(x = Ano_28, y = SPOTNDWI_28,fill=Ano_28))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI28,S2NDVI28,SPOTNDVI28,L8NDWI28,S2NDWI28,SPOTNDWI28, ncol=3, top=textGrob("UNIDAD 28", vjust=0.1))
ggsave(file="E:/grafico_28.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo128<-lm(L8NDVI_28 ~ Ano_28, data=L8NDVI)
ANOVA128<- aov(modelo128) 
summary(ANOVA128)
TL8NDVI28<-grupos.varex1<-HSD.test(y=ANOVA128, trt = "Ano_28", group = T, console = T)

modelo282<-lm(L8NDWI_28 ~ Ano_28, data=L8NDWI)
ANOVA282<- aov(modelo282) 
summary(ANOVA282)
TL8NDWI28<-grupos.varex1<-HSD.test(y=ANOVA282, trt = "Ano_28", group = T, console = T)

modelo328<-lm(S2NDVI_28 ~ Ano_28, data=S2NDVI)
ANOVA328<- aov(modelo328) 
summary(ANOVA328)
TS2NDVI28<-grupos.varex1<-HSD.test(y=ANOVA328, trt = "Ano_28", group = T, console = T)

modelo428<-lm(S2NDWI_28 ~ Ano_28, data=S2NDWI)
ANOVA428<- aov(modelo428) 
summary(ANOVA428)
TS2NDWI28<-grupos.varex1<-HSD.test(y=ANOVA428, trt = "Ano_28", group = T, console = T)

modelo528<-lm(SPOTNDVI_28 ~ Ano_28, data=SPOTNDVI)
ANOVA528<- aov(modelo528) 
summary(ANOVA528)
TSPOTNDVI28<-grupos.varex1<-HSD.test(y=ANOVA528, trt = "Ano_28", group = T, console = T)

modelo628<-lm(SPOTNDWI_28 ~ Ano_28, data=SPOTNDWI)
ANOVA628<- aov(modelo628) 
summary(ANOVA628)
TSPOTNDWI28<-grupos.varex1<-HSD.test(y=ANOVA628, trt = "Ano_28", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI28,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI28,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI28,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI28,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI28,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI28,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_28.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 29
L8NDVI29 <- ggplot(data = L8NDVI, aes(x = Ano_29, y = L8NDVI_29, fill=Ano_29))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI29 <- ggplot(data = L8NDWI, aes(x = Ano_29, y = L8NDWI_29,fill=Ano_29))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI29 <- ggplot(data = S2NDWI, aes(x = Ano_29, y = S2NDWI_29,fill=Ano_29))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI29 <- ggplot(data = S2NDVI, aes(x = Ano_29, y = S2NDVI_29,fill=Ano_29))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI29 <- ggplot(data = SPOTNDVI, aes(x = Ano_29, y = SPOTNDVI_29,fill=Ano_29))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI29 <- ggplot(data = SPOTNDWI, aes(x = Ano_29, y = SPOTNDWI_29,fill=Ano_29))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI29,S2NDVI29,SPOTNDVI29,L8NDWI29,S2NDWI29,SPOTNDWI29, ncol=3, top=textGrob("UNIDAD 29", vjust=0.1))
ggsave(file="E:/grafico_29.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo129<-lm(L8NDVI_29 ~ Ano_29, data=L8NDVI)
ANOVA129<- aov(modelo129) 
summary(ANOVA129)
TL8NDVI29<-grupos.varex1<-HSD.test(y=ANOVA129, trt = "Ano_29", group = T, console = T)

modelo292<-lm(L8NDWI_29 ~ Ano_29, data=L8NDWI)
ANOVA292<- aov(modelo292) 
summary(ANOVA292)
TL8NDWI29<-grupos.varex1<-HSD.test(y=ANOVA292, trt = "Ano_29", group = T, console = T)

modelo329<-lm(S2NDVI_29 ~ Ano_29, data=S2NDVI)
ANOVA329<- aov(modelo329) 
summary(ANOVA329)
TS2NDVI29<-grupos.varex1<-HSD.test(y=ANOVA329, trt = "Ano_29", group = T, console = T)

modelo429<-lm(S2NDWI_29 ~ Ano_29, data=S2NDWI)
ANOVA429<- aov(modelo429) 
summary(ANOVA429)
TS2NDWI29<-grupos.varex1<-HSD.test(y=ANOVA429, trt = "Ano_29", group = T, console = T)

modelo529<-lm(SPOTNDVI_29 ~ Ano_29, data=SPOTNDVI)
ANOVA529<- aov(modelo529) 
summary(ANOVA529)
TSPOTNDVI29<-grupos.varex1<-HSD.test(y=ANOVA529, trt = "Ano_29", group = T, console = T)

modelo629<-lm(SPOTNDWI_29 ~ Ano_29, data=SPOTNDWI)
ANOVA629<- aov(modelo629) 
summary(ANOVA629)
TSPOTNDWI29<-grupos.varex1<-HSD.test(y=ANOVA629, trt = "Ano_29", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI29,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI29,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI29,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI29,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI29,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI29,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_29.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 30
L8NDVI30 <- ggplot(data = L8NDVI, aes(x = Ano_30, y = L8NDVI_30, fill=Ano_30))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI30 <- ggplot(data = L8NDWI, aes(x = Ano_30, y = L8NDWI_30,fill=Ano_30))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI30 <- ggplot(data = S2NDWI, aes(x = Ano_30, y = S2NDWI_30,fill=Ano_30))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI30 <- ggplot(data = S2NDVI, aes(x = Ano_30, y = S2NDVI_30,fill=Ano_30))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI30 <- ggplot(data = SPOTNDVI, aes(x = Ano_30, y = SPOTNDVI_30,fill=Ano_30))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI30 <- ggplot(data = SPOTNDWI, aes(x = Ano_30, y = SPOTNDWI_30,fill=Ano_30))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI30,S2NDVI30,SPOTNDVI30,L8NDWI30,S2NDWI30,SPOTNDWI30, ncol=3, top=textGrob("UNIDAD 30", vjust=0.1))
ggsave(file="E:/grafico_30.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo130<-lm(L8NDVI_30 ~ Ano_30, data=L8NDVI)
ANOVA130<- aov(modelo130) 
summary(ANOVA130)
TL8NDVI30<-grupos.varex1<-HSD.test(y=ANOVA130, trt = "Ano_30", group = T, console = T)

modelo302<-lm(L8NDWI_30 ~ Ano_30, data=L8NDWI)
ANOVA302<- aov(modelo302) 
summary(ANOVA302)
TL8NDWI30<-grupos.varex1<-HSD.test(y=ANOVA302, trt = "Ano_30", group = T, console = T)

modelo330<-lm(S2NDVI_30 ~ Ano_30, data=S2NDVI)
ANOVA330<- aov(modelo330) 
summary(ANOVA330)
TS2NDVI30<-grupos.varex1<-HSD.test(y=ANOVA330, trt = "Ano_30", group = T, console = T)

modelo430<-lm(S2NDWI_30 ~ Ano_30, data=S2NDWI)
ANOVA430<- aov(modelo430) 
summary(ANOVA430)
TS2NDWI30<-grupos.varex1<-HSD.test(y=ANOVA430, trt = "Ano_30", group = T, console = T)

modelo530<-lm(SPOTNDVI_30 ~ Ano_30, data=SPOTNDVI)
ANOVA530<- aov(modelo530) 
summary(ANOVA530)
TSPOTNDVI30<-grupos.varex1<-HSD.test(y=ANOVA530, trt = "Ano_30", group = T, console = T)

modelo630<-lm(SPOTNDWI_30 ~ Ano_30, data=SPOTNDWI)
ANOVA630<- aov(modelo630) 
summary(ANOVA630)
TSPOTNDWI30<-grupos.varex1<-HSD.test(y=ANOVA630, trt = "Ano_30", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI30,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI30,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI30,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI30,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI30,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI30,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_30.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 31
L8NDVI31 <- ggplot(data = L8NDVI, aes(x = Ano_31, y = L8NDVI_31, fill=Ano_31))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI31 <- ggplot(data = L8NDWI, aes(x = Ano_31, y = L8NDWI_31,fill=Ano_31))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI31 <- ggplot(data = S2NDWI, aes(x = Ano_31, y = S2NDWI_31,fill=Ano_31))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI31 <- ggplot(data = S2NDVI, aes(x = Ano_31, y = S2NDVI_31,fill=Ano_31))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI31 <- ggplot(data = SPOTNDVI, aes(x = Ano_31, y = SPOTNDVI_31,fill=Ano_31))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI31 <- ggplot(data = SPOTNDWI, aes(x = Ano_31, y = SPOTNDWI_31,fill=Ano_31))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI31,S2NDVI31,SPOTNDVI31,L8NDWI31,S2NDWI31,SPOTNDWI31, ncol=3, top=textGrob("UNIDAD 31", vjust=0.1))
ggsave(file="E:/grafico_31.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo131<-lm(L8NDVI_31 ~ Ano_31, data=L8NDVI)
ANOVA131<- aov(modelo131) 
summary(ANOVA131)
TL8NDVI31<-grupos.varex1<-HSD.test(y=ANOVA131, trt = "Ano_31", group = T, console = T)

modelo312<-lm(L8NDWI_31 ~ Ano_31, data=L8NDWI)
ANOVA312<- aov(modelo312) 
summary(ANOVA312)
TL8NDWI31<-grupos.varex1<-HSD.test(y=ANOVA312, trt = "Ano_31", group = T, console = T)

modelo331<-lm(S2NDVI_31 ~ Ano_31, data=S2NDVI)
ANOVA331<- aov(modelo331) 
summary(ANOVA331)
TS2NDVI31<-grupos.varex1<-HSD.test(y=ANOVA331, trt = "Ano_31", group = T, console = T)

modelo431<-lm(S2NDWI_31 ~ Ano_31, data=S2NDWI)
ANOVA431<- aov(modelo431) 
summary(ANOVA431)
TS2NDWI31<-grupos.varex1<-HSD.test(y=ANOVA431, trt = "Ano_31", group = T, console = T)

modelo531<-lm(SPOTNDVI_31 ~ Ano_31, data=SPOTNDVI)
ANOVA531<- aov(modelo531) 
summary(ANOVA531)
TSPOTNDVI31<-grupos.varex1<-HSD.test(y=ANOVA531, trt = "Ano_31", group = T, console = T)

modelo631<-lm(SPOTNDWI_31 ~ Ano_31, data=SPOTNDWI)
ANOVA631<- aov(modelo631) 
summary(ANOVA631)
TSPOTNDWI31<-grupos.varex1<-HSD.test(y=ANOVA631, trt = "Ano_31", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI31,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI31,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI31,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI31,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI31,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI31,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_31.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 32
L8NDVI32 <- ggplot(data = L8NDVI, aes(x = Ano_32, y = L8NDVI_32, fill=Ano_32))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI32 <- ggplot(data = L8NDWI, aes(x = Ano_32, y = L8NDWI_32,fill=Ano_32))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI32 <- ggplot(data = S2NDWI, aes(x = Ano_32, y = S2NDWI_32,fill=Ano_32))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI32 <- ggplot(data = S2NDVI, aes(x = Ano_32, y = S2NDVI_32,fill=Ano_32))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI32 <- ggplot(data = SPOTNDVI, aes(x = Ano_32, y = SPOTNDVI_32,fill=Ano_32))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI32 <- ggplot(data = SPOTNDWI, aes(x = Ano_32, y = SPOTNDWI_32,fill=Ano_32))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI32,S2NDVI32,SPOTNDVI32,L8NDWI32,S2NDWI32,SPOTNDWI32, ncol=3, top=textGrob("UNIDAD 32", vjust=0.1))
ggsave(file="E:/grafico_32.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo132<-lm(L8NDVI_32 ~ Ano_32, data=L8NDVI)
ANOVA132<- aov(modelo132) 
summary(ANOVA132)
TL8NDVI32<-grupos.varex1<-HSD.test(y=ANOVA132, trt = "Ano_32", group = T, console = T)

modelo322<-lm(L8NDWI_32 ~ Ano_32, data=L8NDWI)
ANOVA322<- aov(modelo322) 
summary(ANOVA322)
TL8NDWI32<-grupos.varex1<-HSD.test(y=ANOVA322, trt = "Ano_32", group = T, console = T)

modelo332<-lm(S2NDVI_32 ~ Ano_32, data=S2NDVI)
ANOVA332<- aov(modelo332) 
summary(ANOVA332)
TS2NDVI32<-grupos.varex1<-HSD.test(y=ANOVA332, trt = "Ano_32", group = T, console = T)

modelo432<-lm(S2NDWI_32 ~ Ano_32, data=S2NDWI)
ANOVA432<- aov(modelo432) 
summary(ANOVA432)
TS2NDWI32<-grupos.varex1<-HSD.test(y=ANOVA432, trt = "Ano_32", group = T, console = T)

modelo532<-lm(SPOTNDVI_32 ~ Ano_32, data=SPOTNDVI)
ANOVA532<- aov(modelo532) 
summary(ANOVA532)
TSPOTNDVI32<-grupos.varex1<-HSD.test(y=ANOVA532, trt = "Ano_32", group = T, console = T)

modelo632<-lm(SPOTNDWI_32 ~ Ano_32, data=SPOTNDWI)
ANOVA632<- aov(modelo632) 
summary(ANOVA632)
TSPOTNDWI32<-grupos.varex1<-HSD.test(y=ANOVA632, trt = "Ano_32", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI32,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI32,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI32,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI32,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI32,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI32,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_32.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 33
L8NDVI33 <- ggplot(data = L8NDVI, aes(x = Ano_33, y = L8NDVI_33, fill=Ano_33))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
L8NDWI33 <- ggplot(data = L8NDWI, aes(x = Ano_33, y = L8NDWI_33,fill=Ano_33))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0033", "#9999FF","#9933CC","#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDWI33 <- ggplot(data = S2NDWI, aes(x = Ano_33, y = S2NDWI_33,fill=Ano_33))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
S2NDVI33 <- ggplot(data = S2NDVI, aes(x = Ano_33, y = S2NDVI_33,fill=Ano_33))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#CC9900","#999999"))
SPOTNDVI33 <- ggplot(data = SPOTNDVI, aes(x = Ano_33, y = SPOTNDVI_33,fill=Ano_33))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))
SPOTNDWI33 <- ggplot(data = SPOTNDWI, aes(x = Ano_33, y = SPOTNDWI_33,fill=Ano_33))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9933CC","#0000FF","#99FF00", "#669933","#FFCC33", "#FF9933", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI33,S2NDVI33,SPOTNDVI33,L8NDWI33,S2NDWI33,SPOTNDWI33, ncol=3, top=textGrob("UNIDAD 33", vjust=0.1))
ggsave(file="E:/grafico_33.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo133<-lm(L8NDVI_33 ~ Ano_33, data=L8NDVI)
ANOVA133<- aov(modelo133) 
summary(ANOVA133)
TL8NDVI33<-grupos.varex1<-HSD.test(y=ANOVA133, trt = "Ano_33", group = T, console = T)

modelo332<-lm(L8NDWI_33 ~ Ano_33, data=L8NDWI)
ANOVA332<- aov(modelo332) 
summary(ANOVA332)
TL8NDWI33<-grupos.varex1<-HSD.test(y=ANOVA332, trt = "Ano_33", group = T, console = T)

modelo333<-lm(S2NDVI_33 ~ Ano_33, data=S2NDVI)
ANOVA333<- aov(modelo333) 
summary(ANOVA333)
TS2NDVI33<-grupos.varex1<-HSD.test(y=ANOVA333, trt = "Ano_33", group = T, console = T)

modelo433<-lm(S2NDWI_33 ~ Ano_33, data=S2NDWI)
ANOVA433<- aov(modelo433) 
summary(ANOVA433)
TS2NDWI33<-grupos.varex1<-HSD.test(y=ANOVA433, trt = "Ano_33", group = T, console = T)

modelo533<-lm(SPOTNDVI_33 ~ Ano_33, data=SPOTNDVI)
ANOVA533<- aov(modelo533) 
summary(ANOVA533)
TSPOTNDVI33<-grupos.varex1<-HSD.test(y=ANOVA533, trt = "Ano_33", group = T, console = T)

modelo633<-lm(SPOTNDWI_33 ~ Ano_33, data=SPOTNDWI)
ANOVA633<- aov(modelo633) 
summary(ANOVA633)
TSPOTNDWI33<-grupos.varex1<-HSD.test(y=ANOVA633, trt = "Ano_33", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI33,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI33,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI33,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI33,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI33,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI33,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_33.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 34
L8NDVI34 <- ggplot(data = L8NDVI, aes(x = Ano_34, y = L8NDVI_34, fill=Ano_34))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0034", "#9999FF","#9934CC","#00FFFF","#0000FF","#99FF00", "#669934","#FFCC34", "#FF9934", "#FFFF00","#CC9900","#999999"))
L8NDWI34 <- ggplot(data = L8NDWI, aes(x = Ano_34, y = L8NDWI_34,fill=Ano_34))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0034", "#9999FF","#9934CC","#00FFFF","#0000FF","#99FF00", "#669934","#FFCC34", "#FF9934", "#FFFF00","#CC9900","#999999"))
S2NDWI34 <- ggplot(data = S2NDWI, aes(x = Ano_34, y = S2NDWI_34,fill=Ano_34))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669934","#FFCC34", "#FF9934", "#FFFF00","#CC9900","#999999"))
S2NDVI34 <- ggplot(data = S2NDVI, aes(x = Ano_34, y = S2NDVI_34,fill=Ano_34))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669934","#FFCC34", "#FF9934", "#FFFF00","#CC9900","#999999"))
SPOTNDVI34 <- ggplot(data = SPOTNDVI, aes(x = Ano_34, y = SPOTNDVI_34,fill=Ano_34))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9934CC","#0000FF","#99FF00", "#669934","#FFCC34", "#FF9934", "#FFFF00","#999999"))
SPOTNDWI34 <- ggplot(data = SPOTNDWI, aes(x = Ano_34, y = SPOTNDWI_34,fill=Ano_34))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9934CC","#0000FF","#99FF00", "#669934","#FFCC34", "#FF9934", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI34,S2NDVI34,SPOTNDVI34,L8NDWI34,S2NDWI34,SPOTNDWI34, ncol=3, top=textGrob("UNIDAD 34", vjust=0.1))
ggsave(file="E:/grafico_34.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo134<-lm(L8NDVI_34 ~ Ano_34, data=L8NDVI)
ANOVA134<- aov(modelo134) 
summary(ANOVA134)
TL8NDVI34<-grupos.varex1<-HSD.test(y=ANOVA134, trt = "Ano_34", group = T, console = T)

modelo342<-lm(L8NDWI_34 ~ Ano_34, data=L8NDWI)
ANOVA342<- aov(modelo342) 
summary(ANOVA342)
TL8NDWI34<-grupos.varex1<-HSD.test(y=ANOVA342, trt = "Ano_34", group = T, console = T)

modelo343<-lm(S2NDVI_34 ~ Ano_34, data=S2NDVI)
ANOVA343<- aov(modelo343) 
summary(ANOVA343)
TS2NDVI34<-grupos.varex1<-HSD.test(y=ANOVA343, trt = "Ano_34", group = T, console = T)

modelo434<-lm(S2NDWI_34 ~ Ano_34, data=S2NDWI)
ANOVA434<- aov(modelo434) 
summary(ANOVA434)
TS2NDWI34<-grupos.varex1<-HSD.test(y=ANOVA434, trt = "Ano_34", group = T, console = T)

modelo534<-lm(SPOTNDVI_34 ~ Ano_34, data=SPOTNDVI)
ANOVA534<- aov(modelo534) 
summary(ANOVA534)
TSPOTNDVI34<-grupos.varex1<-HSD.test(y=ANOVA534, trt = "Ano_34", group = T, console = T)

modelo634<-lm(SPOTNDWI_34 ~ Ano_34, data=SPOTNDWI)
ANOVA634<- aov(modelo634) 
summary(ANOVA634)
TSPOTNDWI34<-grupos.varex1<-HSD.test(y=ANOVA634, trt = "Ano_34", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI34,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI34,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI34,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI34,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI34,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI34,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_34.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 35
L8NDVI35 <- ggplot(data = L8NDVI, aes(x = Ano_35, y = L8NDVI_35, fill=Ano_35))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0035", "#9999FF","#9935CC","#00FFFF","#0000FF","#99FF00", "#669935","#FFCC35", "#FF9935", "#FFFF00","#CC9900","#999999"))
L8NDWI35 <- ggplot(data = L8NDWI, aes(x = Ano_35, y = L8NDWI_35,fill=Ano_35))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0035", "#9999FF","#9935CC","#00FFFF","#0000FF","#99FF00", "#669935","#FFCC35", "#FF9935", "#FFFF00","#CC9900","#999999"))
S2NDWI35 <- ggplot(data = S2NDWI, aes(x = Ano_35, y = S2NDWI_35,fill=Ano_35))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669935","#FFCC35", "#FF9935", "#FFFF00","#CC9900","#999999"))
S2NDVI35 <- ggplot(data = S2NDVI, aes(x = Ano_35, y = S2NDVI_35,fill=Ano_35))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669935","#FFCC35", "#FF9935", "#FFFF00","#CC9900","#999999"))
SPOTNDVI35 <- ggplot(data = SPOTNDVI, aes(x = Ano_35, y = SPOTNDVI_35,fill=Ano_35))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9935CC","#0000FF","#99FF00", "#669935","#FFCC35", "#FF9935", "#FFFF00","#999999"))
SPOTNDWI35 <- ggplot(data = SPOTNDWI, aes(x = Ano_35, y = SPOTNDWI_35,fill=Ano_35))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9935CC","#0000FF","#99FF00", "#669935","#FFCC35", "#FF9935", "#FFFF00","#999999"))

graf<-grid.arrange(S2NDVI35,SPOTNDVI35,S2NDWI35,SPOTNDWI35, ncol=2, top=textGrob("UNIDAD 35", vjust=0.1))
ggsave(file="E:/grafico_35.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo135<-lm(L8NDVI_35 ~ Ano_35, data=L8NDVI)
ANOVA135<- aov(modelo135) 
summary(ANOVA135)
TL8NDVI35<-grupos.varex1<-HSD.test(y=ANOVA135, trt = "Ano_35", group = T, console = T)

modelo352<-lm(L8NDWI_35 ~ Ano_35, data=L8NDWI)
ANOVA352<- aov(modelo352) 
summary(ANOVA352)
TL8NDWI35<-grupos.varex1<-HSD.test(y=ANOVA352, trt = "Ano_35", group = T, console = T)

modelo353<-lm(S2NDVI_35 ~ Ano_35, data=S2NDVI)
ANOVA353<- aov(modelo353) 
summary(ANOVA353)
TS2NDVI35<-grupos.varex1<-HSD.test(y=ANOVA353, trt = "Ano_35", group = T, console = T)

modelo435<-lm(S2NDWI_35 ~ Ano_35, data=S2NDWI)
ANOVA435<- aov(modelo435) 
summary(ANOVA435)
TS2NDWI35<-grupos.varex1<-HSD.test(y=ANOVA435, trt = "Ano_35", group = T, console = T)

modelo535<-lm(SPOTNDVI_35 ~ Ano_35, data=SPOTNDVI)
ANOVA535<- aov(modelo535) 
summary(ANOVA535)
TSPOTNDVI35<-grupos.varex1<-HSD.test(y=ANOVA535, trt = "Ano_35", group = T, console = T)

modelo635<-lm(SPOTNDWI_35 ~ Ano_35, data=SPOTNDWI)
ANOVA635<- aov(modelo635) 
summary(ANOVA635)
TSPOTNDWI35<-grupos.varex1<-HSD.test(y=ANOVA635, trt = "Ano_35", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI35,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI35,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI35,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI35,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI35,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI35,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_35.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 36
L8NDVI36 <- ggplot(data = L8NDVI, aes(x = Ano_36, y = L8NDVI_36, fill=Ano_36))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0036", "#9999FF","#9936CC","#00FFFF","#0000FF","#99FF00", "#669936","#FFCC36", "#FF9936", "#FFFF00","#CC9900","#999999"))
L8NDWI36 <- ggplot(data = L8NDWI, aes(x = Ano_36, y = L8NDWI_36,fill=Ano_36))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0036", "#9999FF","#9936CC","#00FFFF","#0000FF","#99FF00", "#669936","#FFCC36", "#FF9936", "#FFFF00","#CC9900","#999999"))
S2NDWI36 <- ggplot(data = S2NDWI, aes(x = Ano_36, y = S2NDWI_36,fill=Ano_36))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669936","#FFCC36", "#FF9936", "#FFFF00","#CC9900","#999999"))
S2NDVI36 <- ggplot(data = S2NDVI, aes(x = Ano_36, y = S2NDVI_36,fill=Ano_36))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669936","#FFCC36", "#FF9936", "#FFFF00","#CC9900","#999999"))
SPOTNDVI36 <- ggplot(data = SPOTNDVI, aes(x = Ano_36, y = SPOTNDVI_36,fill=Ano_36))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9936CC","#0000FF","#99FF00", "#669936","#FFCC36", "#FF9936", "#FFFF00","#999999"))
SPOTNDWI36 <- ggplot(data = SPOTNDWI, aes(x = Ano_36, y = SPOTNDWI_36,fill=Ano_36))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9936CC","#0000FF","#99FF00", "#669936","#FFCC36", "#FF9936", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI36,S2NDVI36,SPOTNDVI36,L8NDWI36,S2NDWI36,SPOTNDWI36, ncol=3, top=textGrob("UNIDAD 36", vjust=0.1))
ggsave(file="E:/grafico_36.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo136<-lm(L8NDVI_36 ~ Ano_36, data=L8NDVI)
ANOVA136<- aov(modelo136) 
summary(ANOVA136)
TL8NDVI36<-grupos.varex1<-HSD.test(y=ANOVA136, trt = "Ano_36", group = T, console = T)

modelo362<-lm(L8NDWI_36 ~ Ano_36, data=L8NDWI)
ANOVA362<- aov(modelo362) 
summary(ANOVA362)
TL8NDWI36<-grupos.varex1<-HSD.test(y=ANOVA362, trt = "Ano_36", group = T, console = T)

modelo363<-lm(S2NDVI_36 ~ Ano_36, data=S2NDVI)
ANOVA363<- aov(modelo363) 
summary(ANOVA363)
TS2NDVI36<-grupos.varex1<-HSD.test(y=ANOVA363, trt = "Ano_36", group = T, console = T)

modelo436<-lm(S2NDWI_36 ~ Ano_36, data=S2NDWI)
ANOVA436<- aov(modelo436) 
summary(ANOVA436)
TS2NDWI36<-grupos.varex1<-HSD.test(y=ANOVA436, trt = "Ano_36", group = T, console = T)

modelo536<-lm(SPOTNDVI_36 ~ Ano_36, data=SPOTNDVI)
ANOVA536<- aov(modelo536) 
summary(ANOVA536)
TSPOTNDVI36<-grupos.varex1<-HSD.test(y=ANOVA536, trt = "Ano_36", group = T, console = T)

modelo636<-lm(SPOTNDWI_36 ~ Ano_36, data=SPOTNDWI)
ANOVA636<- aov(modelo636) 
summary(ANOVA636)
TSPOTNDWI36<-grupos.varex1<-HSD.test(y=ANOVA636, trt = "Ano_36", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI36,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI36,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI36,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI36,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI36,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI36,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_36.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 37
L8NDVI37 <- ggplot(data = L8NDVI, aes(x = Ano_37, y = L8NDVI_37, fill=Ano_37))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0037", "#9999FF","#9937CC","#00FFFF","#0000FF","#99FF00", "#669937","#FFCC37", "#FF9937", "#FFFF00","#CC9900","#999999"))
L8NDWI37 <- ggplot(data = L8NDWI, aes(x = Ano_37, y = L8NDWI_37,fill=Ano_37))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0037", "#9999FF","#9937CC","#00FFFF","#0000FF","#99FF00", "#669937","#FFCC37", "#FF9937", "#FFFF00","#CC9900","#999999"))
S2NDWI37 <- ggplot(data = S2NDWI, aes(x = Ano_37, y = S2NDWI_37,fill=Ano_37))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669937","#FFCC37", "#FF9937", "#FFFF00","#CC9900","#999999"))
S2NDVI37 <- ggplot(data = S2NDVI, aes(x = Ano_37, y = S2NDVI_37,fill=Ano_37))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669937","#FFCC37", "#FF9937", "#FFFF00","#CC9900","#999999"))
SPOTNDVI37 <- ggplot(data = SPOTNDVI, aes(x = Ano_37, y = SPOTNDVI_37,fill=Ano_37))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9937CC","#0000FF","#99FF00", "#669937","#FFCC37", "#FF9937", "#FFFF00","#999999"))
SPOTNDWI37 <- ggplot(data = SPOTNDWI, aes(x = Ano_37, y = SPOTNDWI_37,fill=Ano_37))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9937CC","#0000FF","#99FF00", "#669937","#FFCC37", "#FF9937", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI37,S2NDVI37,SPOTNDVI37,L8NDWI37,S2NDWI37,SPOTNDWI37, ncol=3, top=textGrob("UNIDAD 37", vjust=0.1))
ggsave(file="E:/grafico_37.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo137<-lm(L8NDVI_37 ~ Ano_37, data=L8NDVI)
ANOVA137<- aov(modelo137) 
summary(ANOVA137)
TL8NDVI37<-grupos.varex1<-HSD.test(y=ANOVA137, trt = "Ano_37", group = T, console = T)

modelo372<-lm(L8NDWI_37 ~ Ano_37, data=L8NDWI)
ANOVA372<- aov(modelo372) 
summary(ANOVA372)
TL8NDWI37<-grupos.varex1<-HSD.test(y=ANOVA372, trt = "Ano_37", group = T, console = T)

modelo373<-lm(S2NDVI_37 ~ Ano_37, data=S2NDVI)
ANOVA373<- aov(modelo373) 
summary(ANOVA373)
TS2NDVI37<-grupos.varex1<-HSD.test(y=ANOVA373, trt = "Ano_37", group = T, console = T)

modelo437<-lm(S2NDWI_37 ~ Ano_37, data=S2NDWI)
ANOVA437<- aov(modelo437) 
summary(ANOVA437)
TS2NDWI37<-grupos.varex1<-HSD.test(y=ANOVA437, trt = "Ano_37", group = T, console = T)

modelo537<-lm(SPOTNDVI_37 ~ Ano_37, data=SPOTNDVI)
ANOVA537<- aov(modelo537) 
summary(ANOVA537)
TSPOTNDVI37<-grupos.varex1<-HSD.test(y=ANOVA537, trt = "Ano_37", group = T, console = T)

modelo637<-lm(SPOTNDWI_37 ~ Ano_37, data=SPOTNDWI)
ANOVA637<- aov(modelo637) 
summary(ANOVA637)
TSPOTNDWI37<-grupos.varex1<-HSD.test(y=ANOVA637, trt = "Ano_37", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI37,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI37,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI37,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI37,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI37,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI37,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_37.png",height=18,width=22, units = "cm", res = 400)
dev.off()

#UNIDAD 38
L8NDVI38 <- ggplot(data = L8NDVI, aes(x = Ano_38, y = L8NDVI_38, fill=Ano_38))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0038", "#9999FF","#9938CC","#00FFFF","#0000FF","#99FF00", "#669938","#FFCC38", "#FF9938", "#FFFF00","#CC9900","#999999"))
L8NDWI38 <- ggplot(data = L8NDWI, aes(x = Ano_38, y = L8NDWI_38,fill=Ano_38))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("LANDSAT 8") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666", "#FF0038", "#9999FF","#9938CC","#00FFFF","#0000FF","#99FF00", "#669938","#FFCC38", "#FF9938", "#FFFF00","#CC9900","#999999"))
S2NDWI38 <- ggplot(data = S2NDWI, aes(x = Ano_38, y = S2NDWI_38,fill=Ano_38))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669938","#FFCC38", "#FF9938", "#FFFF00","#CC9900","#999999"))
S2NDVI38 <- ggplot(data = S2NDVI, aes(x = Ano_38, y = S2NDVI_38,fill=Ano_38))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SENTINEL 2") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#00FFFF","#0000FF","#99FF00", "#669938","#FFCC38", "#FF9938", "#FFFF00","#CC9900","#999999"))
SPOTNDVI38 <- ggplot(data = SPOTNDVI, aes(x = Ano_38, y = SPOTNDVI_38,fill=Ano_38))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDVI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9938CC","#0000FF","#99FF00", "#669938","#FFCC38", "#FF9938", "#FFFF00","#999999"))
SPOTNDWI38 <- ggplot(data = SPOTNDWI, aes(x = Ano_38, y = SPOTNDWI_38,fill=Ano_38))+geom_boxplot() + theme_bw()+ labs(x="Año", y="NDWI") +theme(axis.title = element_text(size = 8)) + theme(text = element_text(size = 6))+ theme(legend.position="none")+ ggtitle("SPOT") +theme(plot.title = element_text(hjust = 0.5))+ scale_fill_manual(values = c("#FF6666","#9938CC","#0000FF","#99FF00", "#669938","#FFCC38", "#FF9938", "#FFFF00","#999999"))

graf<-grid.arrange(L8NDVI38,S2NDVI38,SPOTNDVI38,L8NDWI38,S2NDWI38,SPOTNDWI38, ncol=3, top=textGrob("UNIDAD 38", vjust=0.1))
ggsave(file="E:/grafico_38.png", graf,width=16,height = 8, dpi = 400, units = "in", devic='png')

#HSD.test

modelo138<-lm(L8NDVI_38 ~ Ano_38, data=L8NDVI)
ANOVA138<- aov(modelo138) 
summary(ANOVA138)
TL8NDVI38<-grupos.varex1<-HSD.test(y=ANOVA138, trt = "Ano_38", group = T, console = T)

modelo382<-lm(L8NDWI_38 ~ Ano_38, data=L8NDWI)
ANOVA382<- aov(modelo382) 
summary(ANOVA382)
TL8NDWI38<-grupos.varex1<-HSD.test(y=ANOVA382, trt = "Ano_38", group = T, console = T)

modelo383<-lm(S2NDVI_38 ~ Ano_38, data=S2NDVI)
ANOVA383<- aov(modelo383) 
summary(ANOVA383)
TS2NDVI38<-grupos.varex1<-HSD.test(y=ANOVA383, trt = "Ano_38", group = T, console = T)

modelo438<-lm(S2NDWI_38 ~ Ano_38, data=S2NDWI)
ANOVA438<- aov(modelo438) 
summary(ANOVA438)
TS2NDWI38<-grupos.varex1<-HSD.test(y=ANOVA438, trt = "Ano_38", group = T, console = T)

modelo538<-lm(SPOTNDVI_38 ~ Ano_38, data=SPOTNDVI)
ANOVA538<- aov(modelo538) 
summary(ANOVA538)
TSPOTNDVI38<-grupos.varex1<-HSD.test(y=ANOVA538, trt = "Ano_38", group = T, console = T)

modelo638<-lm(SPOTNDWI_38 ~ Ano_38, data=SPOTNDWI)
ANOVA638<- aov(modelo638) 
summary(ANOVA638)
TSPOTNDWI38<-grupos.varex1<-HSD.test(y=ANOVA638, trt = "Ano_38", group = T, console = T)

dev.off()
x11()
Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
layout(Conf3x2)
layout.show(6)
plot(TL8NDVI38,main="LANDSAT 8",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDVI38,main="SENTINEL 2",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDVI38,main="SPOT",las=3,ylab="NDVI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TL8NDWI38,main="LANDSAT 8",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TS2NDWI38,main="SENTINEL 2",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)
plot(TSPOTNDWI38,main="SPOT",las=3,ylab="NDVWI",font.axis="3") + title(xlab="Años", line=4, cex.lab=1.2)

dev.print(png, "E:/Tgrafico_38.png",height=18,width=22, units = "cm", res = 400)
dev.off()


#Creacion de data frames para el test de tukey
#Unidad 1
df_TL8NDVI1<-TL8NDVI1$groups
df_TL8NDWI1<-TL8NDWI1$groups
df_TS2NDVI1<-TS2NDVI1$groups
df_TS2NDWI1<-TS2NDWI1$groups
df_TSPOTNDVI1<-TSPOTNDVI1$groups
df_TSPOTNDWI1<-TSPOTNDWI1$groups

#Unidad 2
df_TL8NDVI2<-TL8NDVI2$groups
df_TL8NDWI2<-TL8NDWI2$groups
df_TS2NDVI2<-TS2NDVI2$groups
df_TS2NDWI2<-TS2NDWI2$groups
df_TSPOTNDVI2<-TSPOTNDVI2$groups
df_TSPOTNDWI2<-TSPOTNDWI2$groups

#Unidad 3
df_TL8NDVI3<-TL8NDVI3$groups
df_TL8NDWI3<-TL8NDWI3$groups
df_TS2NDVI3<-TS2NDVI3$groups
df_TS2NDWI3<-TS2NDWI3$groups
df_TSPOTNDVI3<-TSPOTNDVI3$groups
df_TSPOTNDWI3<-TSPOTNDWI3$groups

#Unidad 4
df_TL8NDVI4<-TL8NDVI4$groups
df_TL8NDWI4<-TL8NDWI4$groups
df_TS2NDVI4<-TS2NDVI4$groups
df_TS2NDWI4<-TS2NDWI4$groups
df_TSPOTNDVI4<-TSPOTNDVI4$groups
df_TSPOTNDWI4<-TSPOTNDWI4$groups

#Unidad 5
df_TL8NDVI5<-TL8NDVI5$groups
df_TL8NDWI5<-TL8NDWI5$groups
df_TS2NDVI5<-TS2NDVI5$groups
df_TS2NDWI5<-TS2NDWI5$groups
df_TSPOTNDVI5<-TSPOTNDVI5$groups
df_TSPOTNDWI5<-TSPOTNDWI5$groups

#Unidad 6
df_TL8NDVI6<-TL8NDVI6$groups
df_TL8NDWI6<-TL8NDWI6$groups
df_TS2NDVI6<-TS2NDVI6$groups
df_TS2NDWI6<-TS2NDWI6$groups
df_TSPOTNDVI6<-TSPOTNDVI6$groups
df_TSPOTNDWI6<-TSPOTNDWI6$groups

#Unidad 7
df_TL8NDVI7<-TL8NDVI7$groups
df_TL8NDWI7<-TL8NDWI7$groups
df_TS2NDVI7<-TS2NDVI7$groups
df_TS2NDWI7<-TS2NDWI7$groups
df_TSPOTNDVI7<-TSPOTNDVI7$groups
df_TSPOTNDWI7<-TSPOTNDWI7$groups

#Unidad 8
df_TL8NDVI8<-TL8NDVI8$groups
df_TL8NDWI8<-TL8NDWI8$groups
df_TS2NDVI8<-TS2NDVI8$groups
df_TS2NDWI8<-TS2NDWI8$groups
df_TSPOTNDVI8<-TSPOTNDVI8$groups
df_TSPOTNDWI8<-TSPOTNDWI8$groups

#Unidad 9
df_TL8NDVI9<-TL8NDVI9$groups
df_TL8NDWI9<-TL8NDWI9$groups
df_TS2NDVI9<-TS2NDVI9$groups
df_TS2NDWI9<-TS2NDWI9$groups
df_TSPOTNDVI9<-TSPOTNDVI9$groups
df_TSPOTNDWI9<-TSPOTNDWI9$groups

#Unidad 10
df_TL8NDVI10<-TL8NDVI10$groups
df_TL8NDWI10<-TL8NDWI10$groups
df_TS2NDVI10<-TS2NDVI10$groups
df_TS2NDWI10<-TS2NDWI10$groups
df_TSPOTNDVI10<-TSPOTNDVI10$groups
df_TSPOTNDWI10<-TSPOTNDWI10$groups

#Unidad 11
df_TL8NDVI11<-TL8NDVI11$groups
df_TL8NDWI11<-TL8NDWI11$groups
df_TS2NDVI11<-TS2NDVI11$groups
df_TS2NDWI11<-TS2NDWI11$groups
df_TSPOTNDVI11<-TSPOTNDVI11$groups
df_TSPOTNDWI11<-TSPOTNDWI11$groups

#Unidad 12
df_TL8NDVI12<-TL8NDVI12$groups
df_TL8NDWI12<-TL8NDWI12$groups
df_TS2NDVI12<-TS2NDVI12$groups
df_TS2NDWI12<-TS2NDWI12$groups
df_TSPOTNDVI12<-TSPOTNDVI12$groups
df_TSPOTNDWI12<-TSPOTNDWI12$groups

#Unidad 13
df_TL8NDVI13<-TL8NDVI13$groups
df_TL8NDWI13<-TL8NDWI13$groups
df_TS2NDVI13<-TS2NDVI13$groups
df_TS2NDWI13<-TS2NDWI13$groups
df_TSPOTNDVI13<-TSPOTNDVI13$groups
df_TSPOTNDWI13<-TSPOTNDWI13$groups

#Unidad 14
df_TL8NDVI14<-TL8NDVI14$groups
df_TL8NDWI14<-TL8NDWI14$groups
df_TS2NDVI14<-TS2NDVI14$groups
df_TS2NDWI14<-TS2NDWI14$groups
df_TSPOTNDVI14<-TSPOTNDVI14$groups
df_TSPOTNDWI14<-TSPOTNDWI14$groups

#Unidad 15
df_TL8NDVI15<-TL8NDVI15$groups
df_TL8NDWI15<-TL8NDWI15$groups
df_TS2NDVI15<-TS2NDVI15$groups
df_TS2NDWI15<-TS2NDWI15$groups
df_TSPOTNDVI15<-TSPOTNDVI15$groups
df_TSPOTNDWI15<-TSPOTNDWI15$groups

#Unidad 16
df_TL8NDVI16<-TL8NDVI16$groups
df_TL8NDWI16<-TL8NDWI16$groups
df_TS2NDVI16<-TS2NDVI16$groups
df_TS2NDWI16<-TS2NDWI16$groups
df_TSPOTNDVI16<-TSPOTNDVI16$groups
df_TSPOTNDWI16<-TSPOTNDWI16$groups

#Unidad 17
df_TL8NDVI17<-TL8NDVI17$groups
df_TL8NDWI17<-TL8NDWI17$groups
df_TS2NDVI17<-TS2NDVI17$groups
df_TS2NDWI17<-TS2NDWI17$groups
df_TSPOTNDVI17<-TSPOTNDVI17$groups
df_TSPOTNDWI17<-TSPOTNDWI17$groups

#Unidad 18
df_TL8NDVI18<-TL8NDVI18$groups
df_TL8NDWI18<-TL8NDWI18$groups
df_TS2NDVI18<-TS2NDVI18$groups
df_TS2NDWI18<-TS2NDWI18$groups
df_TSPOTNDVI18<-TSPOTNDVI18$groups
df_TSPOTNDWI18<-TSPOTNDWI18$groups

#Unidad 19
df_TL8NDVI19<-TL8NDVI19$groups
df_TL8NDWI19<-TL8NDWI19$groups
df_TS2NDVI19<-TS2NDVI19$groups
df_TS2NDWI19<-TS2NDWI19$groups
df_TSPOTNDVI19<-TSPOTNDVI19$groups
df_TSPOTNDWI19<-TSPOTNDWI19$groups

df_TL8NDVI20<-TL8NDVI20$groups
df_TL8NDWI20<-TL8NDWI20$groups
df_TS2NDVI20<-TS2NDVI20$groups
df_TS2NDWI20<-TS2NDWI20$groups
df_TSPOTNDVI20<-TSPOTNDVI20$groups
df_TSPOTNDWI20<-TSPOTNDWI20$groups

df_TL8NDVI21<-TL8NDVI21$groups
df_TL8NDWI21<-TL8NDWI21$groups
df_TS2NDVI21<-TS2NDVI21$groups
df_TS2NDWI21<-TS2NDWI21$groups
df_TSPOTNDVI21<-TSPOTNDVI21$groups
df_TSPOTNDWI21<-TSPOTNDWI21$groups

df_TL8NDVI22<-TL8NDVI22$groups
df_TL8NDWI22<-TL8NDWI22$groups
df_TS2NDVI22<-TS2NDVI22$groups
df_TS2NDWI22<-TS2NDWI22$groups
df_TSPOTNDVI22<-TSPOTNDVI22$groups
df_TSPOTNDWI22<-TSPOTNDWI22$groups

df_TL8NDVI23<-TL8NDVI23$groups
df_TL8NDWI23<-TL8NDWI23$groups
df_TS2NDVI23<-TS2NDVI23$groups
df_TS2NDWI23<-TS2NDWI23$groups
df_TSPOTNDVI23<-TSPOTNDVI23$groups
df_TSPOTNDWI23<-TSPOTNDWI23$groups

df_TL8NDVI24<-TL8NDVI24$groups
df_TL8NDWI24<-TL8NDWI24$groups
df_TS2NDVI24<-TS2NDVI24$groups
df_TS2NDWI24<-TS2NDWI24$groups
df_TSPOTNDVI24<-TSPOTNDVI24$groups
df_TSPOTNDWI24<-TSPOTNDWI24$groups

df_TL8NDVI25<-TL8NDVI25$groups
df_TL8NDWI25<-TL8NDWI25$groups
df_TS2NDVI25<-TS2NDVI25$groups
df_TS2NDWI25<-TS2NDWI25$groups
df_TSPOTNDVI25<-TSPOTNDVI25$groups
df_TSPOTNDWI25<-TSPOTNDWI25$groups

df_TL8NDVI26<-TL8NDVI26$groups
df_TL8NDWI26<-TL8NDWI26$groups
df_TS2NDVI26<-TS2NDVI26$groups
df_TS2NDWI26<-TS2NDWI26$groups
df_TSPOTNDVI26<-TSPOTNDVI26$groups
df_TSPOTNDWI26<-TSPOTNDWI26$groups

df_TL8NDVI27<-TL8NDVI27$groups
df_TL8NDWI27<-TL8NDWI27$groups
df_TS2NDVI27<-TS2NDVI27$groups
df_TS2NDWI27<-TS2NDWI27$groups
df_TSPOTNDVI27<-TSPOTNDVI27$groups
df_TSPOTNDWI27<-TSPOTNDWI27$groups

df_TL8NDVI28<-TL8NDVI28$groups
df_TL8NDWI28<-TL8NDWI28$groups
df_TS2NDVI28<-TS2NDVI28$groups
df_TS2NDWI28<-TS2NDWI28$groups
df_TSPOTNDVI28<-TSPOTNDVI28$groups
df_TSPOTNDWI28<-TSPOTNDWI28$groups

df_TL8NDVI29<-TL8NDVI29$groups
df_TL8NDWI29<-TL8NDWI29$groups
df_TS2NDVI29<-TS2NDVI29$groups
df_TS2NDWI29<-TS2NDWI29$groups
df_TSPOTNDVI29<-TSPOTNDVI29$groups
df_TSPOTNDWI29<-TSPOTNDWI29$groups

df_TL8NDVI30<-TL8NDVI30$groups
df_TL8NDWI30<-TL8NDWI30$groups
df_TS2NDVI30<-TS2NDVI30$groups
df_TS2NDWI30<-TS2NDWI30$groups
df_TSPOTNDVI30<-TSPOTNDVI30$groups
df_TSPOTNDWI30<-TSPOTNDWI30$groups

df_TL8NDVI31<-TL8NDVI31$groups
df_TL8NDWI31<-TL8NDWI31$groups
df_TS2NDVI31<-TS2NDVI31$groups
df_TS2NDWI31<-TS2NDWI31$groups
df_TSPOTNDVI31<-TSPOTNDVI31$groups
df_TSPOTNDWI31<-TSPOTNDWI31$groups

df_TL8NDVI32<-TL8NDVI32$groups
df_TL8NDWI32<-TL8NDWI32$groups
df_TS2NDVI32<-TS2NDVI32$groups
df_TS2NDWI32<-TS2NDWI32$groups
df_TSPOTNDVI32<-TSPOTNDVI32$groups
df_TSPOTNDWI32<-TSPOTNDWI32$groups

df_TL8NDVI33<-TL8NDVI33$groups
df_TL8NDWI33<-TL8NDWI33$groups
df_TS2NDVI33<-TS2NDVI33$groups
df_TS2NDWI33<-TS2NDWI33$groups
df_TSPOTNDVI33<-TSPOTNDVI33$groups
df_TSPOTNDWI33<-TSPOTNDWI33$groups

df_TL8NDVI34<-TL8NDVI34$groups
df_TL8NDWI34<-TL8NDWI34$groups
df_TS2NDVI34<-TS2NDVI34$groups
df_TS2NDWI34<-TS2NDWI34$groups
df_TSPOTNDVI34<-TSPOTNDVI34$groups
df_TSPOTNDWI34<-TSPOTNDWI34$groups

df_TL8NDVI35<-TL8NDVI35$groups
df_TL8NDWI35<-TL8NDWI35$groups
df_TS2NDVI35<-TS2NDVI35$groups
df_TS2NDWI35<-TS2NDWI35$groups
df_TSPOTNDVI35<-TSPOTNDVI35$groups
df_TSPOTNDWI35<-TSPOTNDWI35$groups

df_TL8NDVI36<-TL8NDVI36$groups
df_TL8NDWI36<-TL8NDWI36$groups
df_TS2NDVI36<-TS2NDVI36$groups
df_TS2NDWI36<-TS2NDWI36$groups
df_TSPOTNDVI36<-TSPOTNDVI36$groups
df_TSPOTNDWI36<-TSPOTNDWI36$groups

df_TL8NDVI37<-TL8NDVI37$groups
df_TL8NDWI37<-TL8NDWI37$groups
df_TS2NDVI37<-TS2NDVI37$groups
df_TS2NDWI37<-TS2NDWI37$groups
df_TSPOTNDVI37<-TSPOTNDVI37$groups
df_TSPOTNDWI37<-TSPOTNDWI37$groups

df_TL8NDVI38<-TL8NDVI38$groups
df_TL8NDWI38<-TL8NDWI38$groups
df_TS2NDVI38<-TS2NDVI38$groups
df_TS2NDWI38<-TS2NDWI38$groups
df_TSPOTNDVI38<-TSPOTNDVI38$groups
df_TSPOTNDWI38<-TSPOTNDWI38$groups

#Salida
df_TL8NDVI1
df_TL8NDWI1
df_TS2NDVI1
df_TS2NDWI1
df_TSPOTNDVI1
df_TSPOTNDWI1

df_TL8NDVI2
df_TL8NDWI2
df_TS2NDVI2
df_TS2NDWI2
df_TSPOTNDVI2
df_TSPOTNDWI2

df_TL8NDVI3
df_TL8NDWI3
df_TS2NDVI3
df_TS2NDWI3
df_TSPOTNDWI3
df_TSPOTNDVI3

df_TL8NDVI4
df_TL8NDWI4
df_TS2NDVI4
df_TS2NDWI4
df_TSPOTNDVI4
df_TSPOTNDWI4

df_TL8NDVI5
df_TL8NDWI5
df_TS2NDVI5
df_TS2NDWI5
df_TSPOTNDVI1
df_TSPOTNDWI5

df_TL8NDVI6
df_TL8NDWI6
df_TS2NDVI6
df_TS2NDWI6
df_TSPOTNDVI6
df_TSPOTNDWI6

df_TL8NDVI7
df_TL8NDWI7
df_TS2NDVI7
df_TS2NDWI7
df_TSPOTNDWI7
df_TSPOTNDVI7

df_TL8NDVI8
df_TL8NDWI8
df_TS2NDVI8
df_TS2NDWI8
df_TSPOTNDVI8
df_TSPOTNDWI8

df_TL8NDVI9
df_TL8NDWI9
df_TS2NDVI9
df_TS2NDWI9
df_TSPOTNDVI9
df_TSPOTNDWI9

df_TL8NDVI10
df_TL8NDWI10
df_TS2NDVI10
df_TS2NDWI10
df_TSPOTNDVI10
df_TSPOTNDWI10

df_TL8NDVI11
df_TL8NDWI11
df_TS2NDVI11
df_TS2NDWI11
df_TSPOTNDVI11
df_TSPOTNDWI11

df_TL8NDVI12
df_TL8NDWI12
df_TS2NDVI12
df_TS2NDWI12
df_TSPOTNDVI12
df_TSPOTNDWI12

df_TL8NDVI13
df_TL8NDWI13
df_TS2NDVI13
df_TS2NDWI13
df_TSPOTNDVI13
df_TSPOTNDWI13

df_TL8NDVI14
df_TL8NDWI14
df_TS2NDVI14
df_TS2NDWI14
df_TSPOTNDVI14
df_TSPOTNDWI14

df_TL8NDVI15
df_TL8NDWI15
df_TS2NDVI15
df_TS2NDWI15
df_TSPOTNDVI15
df_TSPOTNDWI15

df_TL8NDVI16
df_TL8NDWI16
df_TS2NDVI16
df_TS2NDWI16
df_TSPOTNDVI16
df_TSPOTNDWI16

df_TL8NDVI17
df_TL8NDWI17
df_TS2NDVI17
df_TS2NDWI17
df_TSPOTNDVI17
df_TSPOTNDWI17

df_TL8NDVI18
df_TL8NDWI18
df_TS2NDVI18
df_TS2NDWI18
df_TSPOTNDVI18
df_TSPOTNDWI18

df_TL8NDVI19
df_TL8NDWI19
df_TS2NDVI19
df_TS2NDWI19
df_TSPOTNDVI19
df_TSPOTNDWI19

df_TL8NDVI20
df_TL8NDWI20
df_TS2NDVI20
df_TS2NDWI20
df_TSPOTNDVI20
df_TSPOTNDWI20

df_TL8NDVI21
df_TL8NDWI21
df_TS2NDVI21
df_TS2NDWI21
df_TSPOTNDVI21
df_TSPOTNDWI21

df_TL8NDVI22
df_TL8NDWI22
df_TS2NDVI22
df_TS2NDWI22
df_TSPOTNDVI22
df_TSPOTNDWI22

df_TL8NDVI23
df_TL8NDWI23
df_TS2NDVI23
df_TS2NDWI23
df_TSPOTNDVI23
df_TSPOTNDWI23

df_TL8NDVI24
df_TL8NDWI24
df_TS2NDVI24
df_TS2NDWI24
df_TSPOTNDVI24
df_TSPOTNDWI24

df_TL8NDVI25
df_TL8NDWI25
df_TS2NDVI25
df_TS2NDWI25
df_TSPOTNDVI25
df_TSPOTNDWI25

df_TL8NDVI26
df_TL8NDWI26
df_TS2NDVI26
df_TS2NDWI26
df_TSPOTNDVI26
df_TSPOTNDWI26

df_TL8NDVI27
df_TL8NDWI27
df_TS2NDVI27
df_TS2NDWI27
df_TSPOTNDVI27
df_TSPOTNDWI27

df_TL8NDVI28
df_TL8NDWI28
df_TS2NDVI28
df_TS2NDWI28
df_TSPOTNDVI28
df_TSPOTNDWI28

df_TL8NDVI29
df_TL8NDWI29
df_TS2NDVI29
df_TS2NDWI29
df_TSPOTNDVI29
df_TSPOTNDWI29

df_TL8NDVI30
df_TL8NDWI30
df_TS2NDVI30
df_TS2NDWI30
df_TSPOTNDVI30
df_TSPOTNDWI30

df_TL8NDVI31
df_TL8NDWI31
df_TS2NDVI31
df_TS2NDWI31
df_TSPOTNDVI31
df_TSPOTNDWI31

df_TL8NDVI32
df_TL8NDWI32
df_TS2NDVI32
df_TS2NDWI32
df_TSPOTNDVI32
df_TSPOTNDWI32

df_TL8NDVI33
df_TL8NDWI33
df_TS2NDVI33
df_TS2NDWI33
df_TSPOTNDVI33
df_TSPOTNDWI33

df_TL8NDVI34
df_TL8NDWI34
df_TS2NDVI34
df_TS2NDWI34
df_TSPOTNDVI34
df_TSPOTNDWI34

df_TL8NDVI35
df_TL8NDWI35
df_TS2NDVI35
df_TS2NDWI35
df_TSPOTNDVI35
df_TSPOTNDWI35

df_TL8NDVI36
df_TL8NDWI36
df_TS2NDVI36
df_TS2NDWI36
df_TSPOTNDVI36
df_TSPOTNDWI36

df_TL8NDVI37
df_TL8NDWI37
df_TS2NDVI37
df_TS2NDWI37
df_TSPOTNDVI37
df_TSPOTNDWI37

df_TL8NDVI38
df_TL8NDWI38
df_TS2NDVI38
df_TS2NDWI38
df_TSPOTNDVI38
df_TSPOTNDWI38




cor.test(L8NDVI$L8NDVI_1, L8NDWI$L8NDWI_1,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_2, L8NDWI$L8NDWI_2,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_3, L8NDWI$L8NDWI_3,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_4, L8NDWI$L8NDWI_4,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_5, L8NDWI$L8NDWI_5,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_6, L8NDWI$L8NDWI_6,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_7, L8NDWI$L8NDWI_7,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_8, L8NDWI$L8NDWI_8,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_9, L8NDWI$L8NDWI_9,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_10, L8NDWI$L8NDWI_10,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_11, L8NDWI$L8NDWI_11,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_12, L8NDWI$L8NDWI_12,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_13, L8NDWI$L8NDWI_13,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_14, L8NDWI$L8NDWI_14,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_15, L8NDWI$L8NDWI_15,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_16, L8NDWI$L8NDWI_16,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_17, L8NDWI$L8NDWI_17,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_18, L8NDWI$L8NDWI_18,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_19, L8NDWI$L8NDWI_19,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_20, L8NDWI$L8NDWI_20,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_21, L8NDWI$L8NDWI_21,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_22, L8NDWI$L8NDWI_22,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_23, L8NDWI$L8NDWI_23,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_24, L8NDWI$L8NDWI_24,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_25, L8NDWI$L8NDWI_25,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_26, L8NDWI$L8NDWI_26,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_27, L8NDWI$L8NDWI_27,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_28, L8NDWI$L8NDWI_28,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_29, L8NDWI$L8NDWI_29,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_30, L8NDWI$L8NDWI_30,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_31, L8NDWI$L8NDWI_31,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_32, L8NDWI$L8NDWI_32,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_33, L8NDWI$L8NDWI_33,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_34, L8NDWI$L8NDWI_34,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_35, L8NDWI$L8NDWI_35,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_36, L8NDWI$L8NDWI_36,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_37, L8NDWI$L8NDWI_37,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)
cor.test(L8NDVI$L8NDVI_38, L8NDWI$L8NDWI_38,alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)

#prueba github