##Becca Script
dat= read.csv("SoilHealth_corrected020716.csv")
head(dat, 3)

library(lme4)
#stars(dat[,c(1,5:10)], scale=T)
mod1= lmer()
?barplot
library(lattice)
head(dat, 2)
pdf("BoxPlot1.SoilHealth.pdf")
bwplot(Soil_Health~ factor(dist_river), data=dat, main="", 
       xlab="Distance from River Category",
       ylab="WEOC (ppm)",
       drop.unused.levels = lattice.getOption("drop.unused.levels"))#, 
       #layout=c(4,1))
dev.off()
## MANOVA
  # Type II
  Y= cbind(dat$OMLOI,dat$SoilpH,dat$ppmC,dat$WEONppm,dat$WEOCppm )
lm1 = lm( Y ~ dist_river, data=dat )
library(car)
  manova(lm1)  
man.1 = manova(Y ~dat$dist_river)
summary(man.1)


summary(m1, test="Wilks")


#Multiv. multiple reg.
require(car)
sum1 = summary(Anova(lm1))
sum1$pval.adjustments
### Run scaling and centering for MV analyses
dat$SoilpH= scale(dat$SoilpH, center=T, scale=T)
dat$OMLOI= scale(dat$OMLOI, center=T, scale=T)
dat$ppmC= scale(dat$ppmC, center=T, scale=T)
dat$WEONppm= scale(dat$WEONppm, center=T, scale=T)
dat$OCON= scale(dat$OCON, center=T, scale=T)

library(MASS)

wine.lda = lda( dist_river~ 
                 SoilpH + OMLOI + ppmC + WEONppm + WEOCppm, #+ OCON, 
               data=dat)
wine.lda

wine.lda.values <- predict(wine.lda)

tab= table(dat$dist_river, wine.lda.values$class )
sum(tab[row(tab) == col(tab)]) / sum(tab)  # 92% accuracy

pdf("LDA_plot1.pdf")
ldahist(data = wine.lda.values$x[,1], g=dat$dist_river )#shows the graph in which by simple inspection we can see whether or not there is any discriminant effect
dev.off()

pdf("LDA_plot2.pdf")
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],dat$dist_river,cex=0.7,pos=4,col="red") # add labels
dev.off()


## This means that the first discriminant function (LD1), which 
  # accounts for 91.5% of the variance is:
wine.lda$svd
wine.lda
head(dat)
## BE SURE TO RUN ON THE RAW DATA AND NOT THE SCALED/TRANSFORMED
fit <- aov(OMLOI~dist_river+plot_left, data=dat) 

summary.aov(fit)             # univariate ANOVA tables
summary(fit, test = "Wilks") # ANOVA table of Wilks' lambda
summary(fit)                # same F statistics as single-df terms



         
