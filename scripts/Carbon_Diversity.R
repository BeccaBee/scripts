Boxplot of C and Diversity:
  ```{r}

library(ggplot2)
library(lattice)

#Plot ANOVA results for variance in total invasion by diversity and om
dat=invsoil
dat$diversity=as.factor(dat$diversity)
bc<-aov(all.index~avg.c*diversity, data=dat)
summary(bc)
dat$predbc=predict(bc)

plot1<-ggplot(data=dat, aes(x=diversity, y=predbc, fill=diversity)) + geom_boxplot() + labs(x="Diversity", y="Diversity*soil OM effect on total invasion") 

#plot ANOVA results for variance in woody invasion by div and om
dat=invsoil
dat$diversity=as.factor(dat$diversity)
bcw<-aov(woody.index~avg.c*diversity, data=dat)
summary(bcw)
dat$predbcw=predict(bcw)  
plot2<-ggplot(data=dat, aes(x=diversity, y=predbcw, fill=diversity)) + geom_boxplot() + labs(x="Diversity", y="diversity*soil OM effect on woody invasion") 

#plot ANOVA for variance in cirsium invasion by div and om
dat=invsoil
dat$diversity=as.factor(dat$diversity)
bcc<-aov(cirsium.index~avg.c*diversity, data=dat)
summary(bcc)
dat$predbcc=predict(bcc)  
plot3<-ggplot(data=dat, aes(x=diversity, y=predbcc, fill=diversity)) + geom_boxplot() + labs(x="Diversity", y="Diversity*soil OM effect on thistle invasion")

library(gridExtra)
grid.arrange (plot1, plot2, plot3, ncol=3)
```