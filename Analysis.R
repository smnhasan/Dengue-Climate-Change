library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Climate Change')
Dengue <- read.csv("DengueAndWeatherDataRough2.csv")

#Descriptive
describe.by(Dengue$DC, Dengue$Year)
describe(Dengue$DC)

YearwiseDC <- aggregate(Dengue$DC, by=list(Category=Dengue$Year), FUN=sum)
YearwiseDC
YearwiseDD <- aggregate(Dengue$DD, by=list(Category=Dengue$Year), FUN=sum)
YearwiseDD

YearwiseAvgT <- aggregate(Dengue$AvgT, by=list(Category=Dengue$Year), FUN=mean)
YearwiseAvgT
YearwiseRainfall <- aggregate(Dengue$Rainfall, by=list(Category=Dengue$Year), FUN=sum)
YearwiseRainfall

colnames(YearwiseDC) <- c("Year","DC")
YearwiseDC

colnames(YearwiseDD) <- c("Year","DD")
YearwiseDD

df2 <- data.frame(Dengue=rep(c("Cases", "Deaths"), each=23),
                  Years=rep(c(YearwiseDC$Year),2),
                  Numbers=c(YearwiseDC$DC,YearwiseDD$DD)+1)

# Change the colors manually
p <- ggplot(data=df2, aes(x=Years, y=Numbers, fill=Dengue)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) + 
  geom_bar(position="dodge", stat="identity")+
  theme_minimal()+  theme_bw() +
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.1, 0.9),
         text = element_text(size = 25))

# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
p<- p + scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p


monthwiseDC <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=mean)
monthwiseDC

Dengue$DD[is.na(Dengue$DD)] <- 0

monthwiseDD <- aggregate(Dengue$DD, by=list(Category=Dengue$Month), FUN=mean)
monthwiseDD

df2 <- data.frame(Dengue=rep(c("Cases", "Deaths"), each=12),
                  Months=rep(c(monthwiseDC$Category),2),
                  Numbers=c(monthwiseDC$x,
                            monthwiseDD$x)+1)


# Change the colors manually
q <- ggplot(data=df2, aes(x=Months, y=Numbers, fill=Dengue)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) +
  geom_bar(position="dodge", stat="identity")+
  
  theme_minimal() + theme_bw() +
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.1, 0.9),
         text = element_text(size = 25)) +

  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))
# Use custom colors
q + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
q <- q + scale_fill_brewer(palette="Dark2")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
q

tiff("DCDDM.tiff", units="in", width=18, height=20, res=300)
gridExtra::grid.arrange(p,q)
dev.off()


fYearwiseDC <- YearwiseDC[which(YearwiseDC$Year<='2010'), ]

mean(fYearwiseDC$DC, na.rm = T)
sd(fYearwiseDC$DC, na.rm = T)

sYearwiseDC <- YearwiseDC[which(YearwiseDC$Year>'2010'), ]

mean(sYearwiseDC$DC, na.rm = T)
sd(sYearwiseDC$DC, na.rm = T)

fYearwiseDD <- YearwiseDD[which(YearwiseDD$Year<='2010'), ]

mean(fYearwiseDD$DD, na.rm = T)
sd(fYearwiseDD$DD, na.rm = T)

sYearwiseDD <- YearwiseDD[which(YearwiseDD$Year>'2010'), ]

mean(sYearwiseDD$DD, na.rm = T)
sd(sYearwiseDD$DD, na.rm = T)

fYearwiseAvgT <- YearwiseAvgT[which(YearwiseAvgT$Category<='2010'), ]

mean(fYearwiseAvgT$x, na.rm = T)
sd(fYearwiseAvgT$x, na.rm = T)

sYearwiseAvgT <- YearwiseAvgT[which(YearwiseAvgT$Category>'2010'), ]

mean(sYearwiseAvgT$x, na.rm = T)
sd(sYearwiseAvgT$x, na.rm = T)

fYearwiseRainfall <- YearwiseRainfall[which(YearwiseRainfall$Category<='2010'), ]

mean(fYearwiseRainfall$x, na.rm = T)
sd(fYearwiseRainfall$x, na.rm = T)

sYearwiseRainfall <- YearwiseRainfall[which(YearwiseRainfall$Category>'2010'), ]

mean(sYearwiseRainfall$x, na.rm = T)
sd(sYearwiseRainfall$x, na.rm = T)


fmonthwise <- Dengue[which(Dengue$Year<='2010'), ]
NROW(fmonthwise)

x <- ggplot(fmonthwise, aes(x=as.factor(Month), y=Rainfall)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) + 
  ylab("Monthly rainfall (mm)") + xlab("") + ggtitle("Monthly rainfall Dhaka, Bangladesh (2000-2010)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                         axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5)
  )
fmonthwise <- Dengue[which(Dengue$Year>'2010'), ]

y <- ggplot(fmonthwise, aes(x=as.factor(Month), y=Rainfall)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) + 
  ylab("Monthly rainfall (mm)") + xlab("") + ggtitle("Monthly rainfall Dhaka, Bangladesh (2011-2022)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                          axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5)
  )
y

tiff("box.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(x,y)
dev.off()


mean(fmonthwise$DC, na.rm = T)
sd(fmonthwise$DC, na.rm = T)

mean(fmonthwise$DD, na.rm = T)
sd(fmonthwise$DD, na.rm = T)

mean(fmonthwise$AvgT, na.rm = T)
sd(fmonthwise$AvgT, na.rm = T)

mean(fmonthwise$Rainfall, na.rm = T)
sd(fmonthwise$Rainfall, na.rm = T)

smonthwise <- Dengue[which(Dengue$Year>'2010'), ]
NROW(smonthwise$DC)

mean(smonthwise$DC, na.rm = T)
sd(smonthwise$DC, na.rm = T)

mean(smonthwise$DD, na.rm = T)
sd(smonthwise$DD, na.rm = T)

mean(smonthwise$AvgT, na.rm = T)
sd(smonthwise$AvgT, na.rm = T)

mean(smonthwise$Rainfall, na.rm = T)
sd(smonthwise$Rainfall, na.rm = T)

t.test(fYearwiseDC$DC[1:132], sYearwiseDC$DC[1:132], paired = TRUE, alternative = "two.sided")
t.test(fYearwiseDD$DD[1:132], sYearwiseDD$DD[1:132], paired = TRUE, alternative = "two.sided")
t.test(fYearwiseAvgT$x[1:132], sYearwiseAvgT$x[1:132], paired = TRUE, alternative = "two.sided")
t.test(fYearwiseRainfall$x[1:132], sYearwiseRainfall$x[1:132], paired = TRUE, alternative = "two.sided")

monthwiseRainfall <- Dengue[which(Dengue$Year<='2010'), ]
fmonthwiseRainfallsm <- monthwiseRainfall[monthwiseRainfall$Month<7 |monthwiseRainfall$Month>10,]
smonthwiseRainfallsm <- monthwiseRainfall[monthwiseRainfall$Month>=7 |monthwiseRainfall$Month<=10,]

mean(fmonthwiseRainfallsm$Rainfall)
sd(fmonthwiseRainfallsm$Rainfall)

mean(smonthwiseRainfallsm$Rainfall)
sd(smonthwiseRainfallsm$Rainfall)

monthwiseRainfalll <- Dengue[which(Dengue$Year>'2010'), ]
fmonthwiseRainfallsml <- monthwiseRainfalll[monthwiseRainfalll$Month<7 |monthwiseRainfalll$Month>10,]
smonthwiseRainfallsml <- monthwiseRainfalll[monthwiseRainfalll$Month>=7 |monthwiseRainfalll$Month<=10,]

mean(fmonthwiseRainfallsml$Rainfall)
sd(fmonthwiseRainfallsml$Rainfall)

mean(smonthwiseRainfallsml$Rainfall)
sd(smonthwiseRainfallsml$Rainfall)


t.test(fmonthwiseRainfallsm$Rainfall, fmonthwiseRainfallsml$Rainfall[1:88], paired = TRUE, alternative = "two.sided")
t.test(smonthwiseRainfallsm$Rainfall, smonthwiseRainfallsml$Rainfall[1:132], paired = TRUE, alternative = "two.sided")

t.test(fmonthwiseRainfallsm$Rainfall, smonthwiseRainfallsm$Rainfall[1:88], paired = TRUE, alternative = "two.sided")
t.test(fmonthwiseRainfallsml$Rainfall, smonthwiseRainfallsml$Rainfall[1:96], paired = TRUE, alternative = "two.sided")
NROW(smonthwiseRainfallsml$Rainfall)

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=mean)
monthwise

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=min)
monthwise

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=sd)
monthwise

monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=max)
monthwise

#Monthly
library(ggplot2)
library(forecast)
theme_set(theme_classic())

DengueTS <- ts(Dengue$DC, frequency=12, start=c(2000,1), end=c(2022,12))

# Plot
a <- ggseasonplot(DengueTS)+ geom_line(size=1) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Number of dengue cases") + ggtitle("") +  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.2, 0.8),
         text = element_text(size = 25)) 

a

tiff("DC.tiff", units="in", width=18, height=12, res=300)
gridExtra::grid.arrange(a)
dev.off()


#t <- (Dengue$DC +1)/(lag(Dengue$DC)+1)
t <- Dengue$Gfexp
options(scipen=999)
DengueGF <- ts(t[2:276], frequency=12, start=c(2000,1), end=c(2022,12))
# Plot
b <- ggseasonplot(DengueGF) + geom_line(size=1) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                              labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Monthly growth factor") + ggtitle("") +   geom_hline(yintercept=1, linetype="dashed", 
                                                                                           color = "black", size=1)+
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18), legend.position = c(0.1, 0.75),
         text = element_text(size = 18) ) 

b

#GF
#Dengue$Values_GF <- log((Dengue$DC +1)/(lag(Dengue$DC)+1))

#DengueTSlog <- ts(Dengue$Values_GF, frequency=12, start=c(2000,1), end=c(2022,12))

DengueTSlog <- ts(Dengue$Gfexp, frequency=12, start=c(2000,1), end=c(2022,12))

# Dengue_mean <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Month), FUN=mean, na.rm=T)
# Dengue_mean
# mean(Dengue_mean$x)
# sd(Dengue_mean$x)
# 
# Dengue_sd <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Month), FUN=SD, na.rm=T)
# Dengue_sd
# mean(Dengue_sd$x)
#  
# margin <- qt(0.975,df=11-1)*Dengue_sd$x / sqrt(11)
# 
# Dengue_sd$lower.ci <- Dengue_mean$x - margin
# Dengue_sd$lower.ci
# mean(Dengue_sd$lower.ci)
# Dengue_sd$upper.ci = Dengue_mean$x + margin
# Dengue_sd$upper.ci
# mean(Dengue_sd$upper.ci)
library(Rmisc)
CIs <- group.CI(Dengue$Gfexp ~ Dengue$Month, data=Dengue, ci = 0.95)
CIs
mean(CIs$`Dengue$Gfexp.mean`)
sd(CIs$`Dengue$Gfexp.mean`)
my.data <- data.frame(time     = c(1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12),
                      means    = c(CIs$`Dengue$Gfexp.mean`),
                      lowerCI  = c(CIs$`Dengue$Gfexp.lower`),
                      upperCI  = c(CIs$`Dengue$Gfexp.upper`),
                      scenario = rep(c("Mean monthly growth factor"), each=3))


c <- ggplot(my.data, aes(x = factor(time), y = means, group = scenario))+
  geom_line(aes(colour = scenario), size =1)+
  geom_line(aes(y = lowerCI, colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  geom_line(aes(y = upperCI, colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  scale_colour_manual(values = c('Mean monthly growth factor' = 'black',
                                 '95% Confidence interval' = 'black'),
                      breaks = c( 'Mean monthly growth factor', '95% Confidence interval'))+  ylab("Monthly growth factor") + 
  xlab("Months") + ggtitle("") +  theme_bw()+ geom_hline(yintercept=1, linetype="dashed", 
                                                         color = "black", size=1)+
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18),
         legend.position = c(0.2, 0.9),
         text = element_text(size = 18)) +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))

c


tiff("GF.tiff", units="in", width=12, height=16, res=300)
gridExtra::grid.arrange(c,b)
dev.off()




#ARIMA

YearWiseCase <- aggregate(Dengue$DC, by=list(Category=Dengue$Year), FUN=sum)
YearWiseCase

DengueTS <- ts(YearWiseCase$x, start=c(2000))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(2,1,0),lambda=0 )
summary(Fit)

fcast <- forecast(Fit, h=10)
library(ggfortify)
z <- autoplot(fcast, size = 2) +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
  fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 40),
         text = element_text(size = 40))+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                        labels = trans_format("log10", math_format(10^.x)))
z



YearWiseDeath <- aggregate(Dengue$DD, by=list(Category=Dengue$Year), FUN=sum)
YearWiseDeath

DengueTS <- ts(YearWiseDeath$x, start=c(2003))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(0,1,0))
summary(Fit)

fcast <- forecast(Fit, h=10)

y <- autoplot(fcast)  +
  xlab("Years") + ylab("Number of dengue deaths") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.text = element_text(color = "Black", size = 25),
         text = element_text(size = 25))
y



tiff("arima.tiff", units="in", width=18, height=12, res=300)
gridExtra::grid.arrange(z)
dev.off()


#######Count GLM

## Dengue - BD data ##
rm(list=ls())
library(MASS)
library(tscount)
dendat <- read.csv("DengueAndWeatherDataRough2.csv", header=T)  
dim(dendat)
head(dendat)
names(dendat)
#dendat <- dendat[c(37:276),] # discarding the set of missing values #

fitglm <- glm(DC ~ AvgT #+ Rainfall #+ Lag1AvgT #+ Lag2AvgT
              + Lag1Rainfall + Lag2Rainfall, data=dendat, family=poisson(link = "log"))
summary(fitglm)
#stepAIC(fitglm)
cat("IRR for AvgT. = ", exp(fitglm$coefficients[2]))
cat("IRR for Lag1Rainfall = ", exp(fitglm$coefficients[3]*100))
cat("IRR for Lag2Rainfall = ", exp(fitglm$coefficients[4]*100))

confint(fitglm)
exp(confint(fitglm)[2,1:2])
exp(confint(fitglm)[3,1:2]*100)
exp(confint(fitglm)[4,1:2]*100)





## Analysis using tscount package ##
attach(dendat)  
xcov = cbind(AvgT, Rainfall, Lag1AvgT, Lag2AvgT, Lag1Rainfall, Lag2Rainfall, AvgT*Rainfall)
fittsglm <- tsglm(DC, xreg=xcov, link = "log", distr = "poisson")
summary(fittsglm)

summary(fittsglm)[5]$coefficient[,1]/summary(fittsglm)[5]$coefficient[,2]

exp(summary(fittsglm)[5]$coefficient[,1])


summary(fit_pois)
coeftest(fit_pois)

exp(fit_pois$coefficients)

round(exp(confint(fit_pois)),3)

#Menn kendal
library(Kendall)
library(trend)

myts <- ts(YearWiseCase$x)
t.test(YearWiseCase$x)$"conf.int"
mean(YearWiseCase$x)

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)

