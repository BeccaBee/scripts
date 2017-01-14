#load data
soil_health<-read.csv("C:/Users/rbevans2/Desktop/FIELD DATA 2016!!/SoilHealth_corrected020716.csv")
chlorophyll<-read.csv("C:/Users/rbevans2/Desktop/FIELD DATA 2016!!/cla_2016_corrected.csv")
View(cla_2016_corrected)

#load libraries
library(tidyverse)
library(lattice)

#box-and-whisker plot
boxplot(cla_avg~factor(taxa), data=chlorophyll, main="", 
                     xlab="Taxa",
                     ylab="Chlorophyll",
                     drop.unused.levels = lattice.getOption("drop.unused.levels"))
              #layout=c(4,1))
              #dev.off())

#subset taxa, treatments, and chlorophyll average              
myvars<-c("taxa", "diversity", "trt", "cla_avg")
newdata<-chlorophyll[myvars]
head(newdata)

#subset ange:
ange<-subset(newdata, taxa=="ange", 
             select=(taxa:cla_avg))
head(ange)
#anova for ange:
ange_1<-aov(cla_avg~trt*diversity, data=ange)
summary(ange_1)

#subset solidago:
solidago<-subset(newdata, taxa=="solidago", 
                 select=(taxa:cla_avg))

#anova/boxplot for solidago:
solidago_1<-aov(cla_avg~trt*diversity, data=solidago)
summary(solidago_1)
boxplot(cla_avg~trt, data=solidago, 
        col=2:6)
boxplot(cla_avg~diversity, data=solidago)


#boxplot
boxplot(cla_avg~trt*diversity, data=ange, 
        notch=FALSE, col=(c("gold", "darkgreen")),
        main="Chlorophyll by treatment and diversity", 
        xlab="diversity and treatment", 
        ylab="chlorophyll")

#boxplot against 2 crossed factors:
boxplot(cla_avg~taxa:trt, data=newdata)
#,
 #       col=(c("gold", "darkgreen")), 
  #      main="Species-Treatment Interactions", 
   #     xlab="species and treatment", 
    #    ylab="leaf chlorophyll"))

chl_june<-read.csv("C:/Users/rbevans2/Desktop/FIELD DATA 2016!!/cla_june_16.csv")
head(chl_june)

#anova cla_avg by taxa
taxa_1<-aov(cla_avg~taxa, data=chl_june)
summary(taxa_1)
boxplot(cla_avg~taxa, data=chl_june)

solidago_dat<-subset(chl_june, taxa=="solidago", 
                      select=c(taxa, cla_avg))
soli<-lm(cla_avg~taxa, data=solidago_dat)

#import dataset
cla_angesoli<-read.csv("C:/Users/rbevans2/Desktop/FIELD DATA 2016!!/cla_angesoli.csv")
head(cla_angesoli)

#anova cla_avg by taxa
taxa2<-aov(cla_avg~taxa, data=cla_angesoli)
summary(taxa2)

boxplot(cla_avg~taxa, data=cla_angesoli, 
  col=(c("gold", "darkgreen")), 
  main="Chlorophyll by species", 
  xlab="species",
  ylab="chlorophyll")

#chlorophyll by treatment
taxa3<-aov(cla_avg~taxa:trt, data=cla_angesoli)
summary(taxa3)

boxplot(cla_avg~taxa:trt, data=cla_angesoli, 
        col=(c(2:5)), 
        main="chlorophyll by species and treatment", 
        xlab="species and treatment",
        ylab="chlorophyll")

#subset data
soli<-subset(cla_angesoli, taxa=="solidago")
boxplot(cla_avg~trt, data=soli)
soli1<-aov(cla_avg~trt, data=soli)
summary(soli1)

#import second dataset
head(cla_2016_corrected)
soli_july<-subset(cla_2016_corrected, taxa=="solidago")
soli2<-aov(cla_avg~trt, data=soli_july,
           
summary(soli2)
boxplot(cla_avg~trt, data=soli_july, 
        col=(c(2:6)), 
        main="Solidago chlorophyll by treatment", 
        xlab="treatment",
        ylab="chlorophyll")
pander(soli2)

#subset ange
ange_july<-subset(cla_2016_corrected, taxa=="ange")
ange1<-aov(cla_avg~trt, data=ange_july)
summary(ange1)
pander(ange1)
boxplot(cla_avg~trt, data=ange_july, 
        col=(c(2:6)), 
        main="Andropogon chlorophyll by treatment", 
        xlab="treatment",
        ylab="chlorophyll")

boxplot(cla_avg~trt+diversity, data=ange_july)
ange2<-aov(cla_avg~trt+diversity, data=ange_july)
summary(ange2)
pander(ange2)

library(MASS)
soli3<-lda(cla_avg~trt, data=ange_july, grouping=diversity)
