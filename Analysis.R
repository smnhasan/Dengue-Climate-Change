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

setwd('E:\\Najmul Bhai\\Dengue\\Dengue New')
Dengue <- read.csv("WeatherData.csv")

#Descriptive
describe.by(Dengue$DC, Dengue$Year)
describe(Dengue$DC)

Yearwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Year), FUN=sum)
Yearwise
Yearwise <- aggregate(Dengue$DD, by=list(Category=Dengue$Year), FUN=sum)
Yearwise

colnames(Yearwise) <- c("Year","DC")


df2 <- data.frame(Dengue=rep(c("Cases", "Deaths"), each=23),
                  Years=rep(c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                         "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                         "2018", "2019", "2020", "2021", "2022"),2),
                  Numbers=c(5551, 2430, 6232, 486, 3934, 1048, 2200, 466, 1153, 474, 409, 1359,
                        671, 1749, 375, 3162, 6060, 2769, 10148, 101354, 1405, 28429, 62522,
                        0, 0, 0, 10, 13, 4, 11, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0,164, 0, 105, 281)+1)

# Change the colors manually
p <- ggplot(data=df2, aes(x=Years, y=Numbers, fill=Dengue)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) + 
  geom_bar(position="dodge", stat="identity")+
  theme_minimal()+  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.1, 0.9),
         text = element_text(size = 25))

# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
p<- p + scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p


monthwise <- aggregate(Dengue$DC, by=list(Category=Dengue$Month), FUN=mean)
monthwise

Dengue$DD[is.na(Dengue$DD)] <- 0

monthwise <- aggregate(Dengue$DD, by=list(Category=Dengue$Month), FUN=mean)
monthwise

df2 <- data.frame(Dengue=rep(c("Cases", "Deaths"), each=12),
                  Months=rep(c("1", "2", "3", "4", "5", "6", 
                                "7", "8", "9", "10", "11", "12"),2),
                  Numbers=c(23.57, 7.26, 6.70, 11.17, 30.22, 187.65, 1182.48, 3407.22, 2034.26, 1922.78, 1416.91, 389.17,
                            0,0,0,0.09,0,0.35,2.65,5.65,4.04,5.39,5.83,1.57)+1)


# Change the colors manually
q <- ggplot(data=df2, aes(x=Months, y=Numbers, fill=Dengue)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                                         labels = trans_format("log10", math_format(10^.x))) +
  geom_bar(position="dodge", stat="identity")+
  
  theme_minimal() +
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


fYearwise <- Yearwise[which(Yearwise$Year<='2010'), ]

mean(fYearwise$DC, na.rm = T)
sd(fYearwise$DC, na.rm = T)

fmonthwise <- Dengue[which(Dengue$Year<='2010'), ]
NROW(fmonthwise)

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


t.test(fmonthwise$DC[1:132], smonthwise$DC[1:132], paired = TRUE, alternative = "two.sided")
t.test(fmonthwise$DD[1:132], smonthwise$DD[1:132], paired = TRUE, alternative = "two.sided")
t.test(fmonthwise$AvgT[1:132], smonthwise$AvgT[1:132], paired = TRUE, alternative = "two.sided")
t.test(fmonthwise$Rainfall[1:132], smonthwise$Rainfall[1:132], paired = TRUE, alternative = "two.sided")


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

Fit<-Arima(DengueTS,order=c(2,1,0))
summary(Fit)

fcast <- forecast(Fit, h=10)

z <- autoplot(fcast)  + geom_line(size = 1) +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
  fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 25),
         text = element_text(size = 25))
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

deng <- data.frame(Dengue$AvgT, Dengue$Rainfall, Dengue$Lag1AvgT, Dengue$Lag2AvgT, Dengue$Lag1Rainfall, Dengue$Lag2Rainfall)

deng <- data.frame(Dengue$AvgT,    Dengue$Lag1Rainfall, Dengue$Lag2Rainfall)

#Dengue$DC[37:274]
library(tscount)

fit_pois <- tsglm(Dengue$DC[38:276], xreg = deng[38:276,],  link="identity",
                      distr="poisson")

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