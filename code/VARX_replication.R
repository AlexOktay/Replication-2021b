library(tidyverse)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(extrafont)
library(dplyr)
library(stargazer)
library(vars)
library(tsm)
library(mFilter)
library(DataCombine)
library(xtable)
library(corrplot)
library(plotrix)
library(STL)
library(ggpubr)
library(cowplot)
library(grid)
library(ggplotify)
#library(devtools)
#devtools::install_github("KevinKotze/tsm") #to installe non-CRAN packages

# Section 1: Setup & Data cleaning---------------------------------------------

#Setup
rm(list = ls())
graphics.off()
par(mfrow = c(1, 1))

#Working directory setup (please create the necessary folders)
wd_root<-"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE"
wd_persist<-"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/persistence"
wd_tables<-"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/Tables"
wd_others<-"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/Other_graphs"
wd_predictions<-"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/Predictions"
wd_IRF<-"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/IRF"

# Import
setwd(wd_root)
import <- read_csv("Merged_datasets.csv")

#Formatting
df<-import[-1]
row.names(df) <- import$X1
df[] <- lapply(df, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(df, class)
row.names(df) <- import$X1
rm(import)
df<-t(df)
df<-as.data.frame(df)
time<-as.Date(rownames(df),format="%d/%m/%y")

#Data cleaning 
#df<-df[-c(447,448,449,450,451,452),] #Remove empty variables
c19_tests<-ts(df$Covid_tests,start=c(2020,01,01),frequency = 365)
c19_percentagepositive<-ts(df$Covid_tests_percentage_positive,start=c(2020,01,01),frequency = 365)

### Merge (sum) monitoring consumption variables
df<-df %>%
  mutate(
    consumption=ATM_WITHDRAWAL+CREDIT_DOMESTIC_ECOM+CREDIT_DOMESTIC_POS+CREDIT_FOREIGN_ECOM+CREDIT_FOREIGN_POS+DEBIT_DOMESTIC+DEBIT_FOREIGN+MOBILE
  )
df<-df[-c(5:12)]
### Merge (average) google mobility
df<-df %>%
  mutate(
    mobility=(Retail_mobility+Grocery_mobility+Transit_mobility+Workplaces_mobility+Residential_mobility)/5
  )
df<-df[-c(8:12)]
### Merge company creation (new companies - provisional bankruptcies)
df<-df %>%
  mutate(
    net_bankruptcies=SHAB_hr01_newcompanies - SHAB_KK01_provisionalbankrupty
  )
df<-df[-c(19:21)]
### Merge (average) google consumption trends
df<-df %>%
  mutate(
    consumption_trend=(Trend_clothing+Trend_fooddelivery+Trend_garden+Trend_luxury+Trend_social+Trend_travel)/6
  )
df<-df[-c(19:24)]
### Merge (average) air pollution
df<-df %>%
  mutate(
    air_pollution=(Air_NO2+Air_O3)/2
  )
df<-df[-c(20:21)]
### Merge (weighted-index) economic indexes (SARON + Yields)
df<-df %>%
  mutate(
    interest_rate=(SARON*0.5)+(Swiss_yields_2y*0.3)+(Swiss_yields_3y*0.2)
  )
df<-df[-c(6)]
df<-df[-c(13:16)]
### Drop less pertinent variables
df<-df[-c(2,3,4,7,11,12,14)]

#deseasonalize pollution data 
pollution<- read_csv("Pollution.csv")
setwd(wd_others)
NO2<-ts(pollution$No2,frequency = 365,start=c(2016,1))
STL_NO2<-stats::stl(x=NO2,s.window="periodic", )
plot.stl <- function(..., xlab = "time") {mtext <- function(text, ...)
graphics::mtext(if (text == "time") xlab else text, ...)
plot.stl <- stats:::plot.stl
environment(plot.stl) <- environment()
plot.stl(...)
}
png("NO2_deseasonalized.png", height =4, width = 6, units = 'in', type="windows", res=400)
plot(STL_NO2,xlab="Time",main="Air pollution STL decomposition")
dev.off()
new_no2_full<-data.frame(STL_NO2$time.series[,2]+STL_NO2$time.series[,3])
new_no2<-tail(new_no2_full,nrow(new_no2_full)-1461)
avg_no2<-mean(new_no2$STL_NO2.time.series...2....STL_NO2.time.series...3.)

#Setup individual variables as time series
SMI<-ts(df$SMI,start=c(2020,01,01),frequency = 365)
stringency<-ts(df$Stringency_average,start=c(2020,01,01),frequency = 365)
c19_vaccines<-ts(df$Fully_vaccinated_percentage,start=c(2020,01,01),frequency = 365)
c19_cases<-ts(df$New_cases,start=c(2020,1),frequency = 365)
NEER<-ts(df$CHF_NEER_Overall,start=c(2020,01,01),frequency = 365)
unemployment_proxy<-ts(df$Jobroom_candidates,start=c(2020,01,01),frequency = 365)
consumption<-ts(df$consumption,start=c(2020,01,01),frequency = 365)
mobility<-ts(df$mobility,start=c(2020,01,01),frequency = 365)
net_bankruptcies<-ts(df$net_bankruptcies,start=c(2020,01,01),frequency = 365)
consumption_trend<-ts(df$consumption_trend,start=c(2020,01,01),frequency = 365)
air_pollution<-ts(new_no2,start=c(2020,01,01),frequency = 365)
interest_rate<-ts(df$interest_rate,start=c(2020,01,01),frequency = 365)

png("PRES_Covid.png", height=4, width =6, units = 'in', type="windows", res=400)
plot(c19_cases, xaxt="n", main="Cas Covid-19", xlab="", ylab="")
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
dev.off()

png("PRES_SMI.png", height=4, width =6, units = 'in', type="windows", res=400)
plot(SMI, xaxt="n", main="SMI", xlab="", ylab="")
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
dev.off()


imp<-read_csv("C:/Users/alexo/Downloads/seco_journalized.csv")
png("PRES_SECO.png", height=4, width =6, units = 'in', type="windows", res=400) 
plot(ts(imp$seco,frequency=365,start=c(2020,01,01)),xaxt="n",xlab="",ylab="",main="Indice du SECO (journalisé)")
abline(h=0,col="red",lty=2)
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
dev.off()

#net bankruptcies weekend correction
net_bankruptcies[1]<-net_bankruptcies[3]
for (i in 1:length(net_bankruptcies)) {
  if (net_bankruptcies[i]==0) {
    net_bankruptcies[i]<-net_bankruptcies[i-1]
  }
}

#deseasonalize electricity
setwd(wd_root)
electricity_import<- read_csv("Electricity.csv")
n<-which(electricity_import[,2] == 100)
n2<-n-1
n3<-n-1462
n4<-n3+1
setwd(wd_others)
elec<-ts(electricity_import$Value,frequency = 365,start=c(2016,1))
n5<-length(elec)-n2

#Predict missing values
elec_endog<-cbind(ts(elec[1462:n2]),ts(SMI[1:n3]))
colnames(elec_endog)<-c("elec","smi")
elec_exog<-cbind(ts(consumption[1:n3]), ts(unemployment_proxy[1:n3]), ts(mobility[1:n3]), 
                 ts(net_bankruptcies[1:n3]), ts(air_pollution[1:n3]), ts(c19_cases[1:n3]), ts(stringency[1:n3]))
colnames(elec_exog)<-c("con","une","mob","net","air","c19","str")
VAR_elec<-VAR(elec_endog, p=10, exogen=elec_exog)
exog_pred<-elec_exog[c(1:n5),]
exog_pred[,1]<-consumption[n4:length(consumption)]
exog_pred[,2]<-unemployment_proxy[n4:length(consumption)]
exog_pred[,3]<-mobility[n4:length(consumption)]
exog_pred[,4]<-net_bankruptcies[n4:length(consumption)]
exog_pred[,5]<-air_pollution[n4:length(consumption)]
exog_pred[,6]<-c19_cases[n4:length(consumption)]
exog_pred[,7]<-stringency[n4:length(consumption)]
colnames(exog_pred)<-c("con","une","mob","net","air","c19","str")
pred_elec<-predict(VAR_elec, n.ahead = n5, ci = 0.6, dumvar=exog_pred)
elec[n:length(elec)]<-pred_elec$fcst$elec[,1]
STL_elec<-stats::stl(x=elec,s.window="periodic", )
plot.stl <- function(..., xlab = "time") {mtext <- function(text, ...)
  graphics::mtext(if (text == "time") xlab else text, ...)
plot.stl <- stats:::plot.stl
environment(plot.stl) <- environment()
plot.stl(...)
}
png("electricity_deseasonalized.png", height =4, width = 6, units = 'in', type="windows", res=400)
plot(STL_elec,xlab="Time",mar=c(0,6,0,6),oma=c(6,0,0,0),main="Electricity STL decomposition")
dev.off()
new_elec<-data.frame(STL_elec$time.series[,2]+STL_elec$time.series[,3])
new_elec<-tail(new_elec,nrow(new_elec)-1461)
electricity<-ts(new_elec,start=c(2020,01,01),frequency = 365)

# Check christmas consumption seasonality # Ho ho ho
setwd(wd_root)
consumption_monthly<-read_csv("Monthly_consumption.csv")
setwd(wd_others)
cons<-ts(consumption_monthly$Value, frequency=12,start=c(2016,1))
STL_cons<-stats::stl(x=cons,s.window="periodic", )
png("cons_deseasonalized.png", height =4, width = 6, units = 'in', type="windows", res=400)
plot(STL_cons,xlab="Time",main="Consumption STL decomposition")
dev.off()


#Data
data<-cbind(consumption, SMI, unemployment_proxy, stringency, electricity, c19_cases, mobility,net_bankruptcies,air_pollution)
data_list<-list(consumption, SMI, unemployment_proxy, stringency, electricity, c19_cases, mobility,net_bankruptcies,air_pollution)
varnames<-c("consumption", "SMI", "unemployment_proxy", "stringency", "electricity", "c19_cases", "mobility","net_bankruptcies","air_pollution")
# all variables: SMI,electricity,stringency,c19_vaccines,c19_cases,NEER,unemployment_proxy,consumption,mobility,net_bankruptcies,consumption_trend,air_pollution,interest_rate


# Section 2: Persistence of data (autocorrelation)-----------------------------

# Correlation matrix
setwd(wd_others)
new_firms<-net_bankruptcies
correlation_matrix<-cor(cbind(consumption, SMI, unemployment_proxy, electricity, mobility, new_firms,air_pollution,c19_cases,stringency))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png("corr_matrix.png", height =5, width = 7, units = 'in', 
    type="windows", res=400)
corrplot(correlation_matrix, method="color", col=col(200),type="full", order="hclust", is.corr=TRUE,
         addCoef.col = "black", tl.col="black", tl.srt=45,mar=c(0,0,0,0))
dev.off()

# Persistence of data
#trace("ac", edit=TRUE) #to change the graph colour to grey in the library settup (#666666) & oma = c(0.2,0.2, 1.1, 0.2)
consumption<-ts(consumption)
SMI<-ts(SMI)
unemployment_proxy<-ts(unemployment_proxy)
electricity<-ts(electricity)
mobility<-ts(mobility)
net_bankruptcies<-ts(net_bankruptcies)
air_pollution<-ts(air_pollution)
c19_cases<-ts(c19_cases)
stringency<-ts(stringency)
setwd(wd_persist)
png("AC_1.png", height =2, width = 4, units = 'in', 
    type="windows", res=400)
consumption.acf <- ac(consumption,max.lag = 30, main = "Consumption")
dev.off()
png("AC_2.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
SMI.acf <- ac(SMI,max.lag = 30, main = "SMI")
dev.off()
png("AC_3.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
electricity.acf <- ac(electricity,max.lag = 30, main = "Electricity")
dev.off()
png("AC_4.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
stringency.acf <- ac(stringency,max.lag = 30, main = "Stringency")
dev.off()
png("AC_5.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
unemployment_proxy.acf <- ac(unemployment_proxy,max.lag = 30, main = "Unemployment proxy")
dev.off()
png("AC_6.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
c19_cases.acf <- ac(c19_cases,max.lag = 30, main = "Covid-19 cases")
dev.off()
png("AC_7.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
mobility.acf <- ac(mobility,max.lag = 30, main = "Mobility")
dev.off()
png("AC_8.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
net_bankruptcies.acf <- ac(net_bankruptcies,max.lag = 30, main = "Net new firms")
dev.off()
png("AC_9.png", height =2, width =4, units = 'in', 
    type="windows", res=400)
air_pollution.acf <- ac(air_pollution,max.lag = 30, main = "Air pollution")
dev.off()
consumption<-ts(consumption,frequency=365,start=c(2020,01,01))
SMI<-ts(SMI,frequency=365,start=c(2020,01,01))
unemployment_proxy<-ts(unemployment_proxy,frequency=365,start=c(2020,01,01))
electricity<-ts(electricity,frequency=365,start=c(2020,01,01))
mobility<-ts(mobility,frequency=365,start=c(2020,01,01))
net_bankruptcies<-ts(net_bankruptcies,frequency=365,start=c(2020,01,01))
air_pollution<-ts(air_pollution,frequency=365,start=c(2020,01,01))
c19_cases<-ts(c19_cases,frequency=365,start=c(2020,01,01))
stringency<-ts(stringency,frequency=365,start=c(2020,01,01))

# Section 3: Dickey-Fuller test------------------------------------------------

# Dickey-Fuller test of unit roots
difull_test<-data.frame(matrix(ncol=4, nrow=0))
colnames(difull_test)<-c("Variable", "Unit root?", "Timer trend?", "Drift term?")

for(i in 1:9) { 
  difull_test[i,1]<-varnames[i]
  temp<-ur.df(ts(data[,i]), type = "trend",7)
  # Gamma test of unit root
  if (temp@teststat[1]>temp@cval[1,1]) {
    difull_test[i,2]<-paste(round(temp@teststat[1],2),"***",sep="")
  }
  else if (temp@teststat[1]>temp@cval[1,2]) {
    difull_test[i,2]<-paste(round(temp@teststat[1],2),"**",sep="")
  }
  else if (temp@teststat[1]>temp@cval[1,2]) {
    difull_test[i,2]<-paste(round(temp@teststat[1],2),"*",sep="")
  }
  else {difull_test[i,2]<-paste(round(temp@teststat[1],2),sep="")}
  # Phi2 test of time trend
  if (temp@teststat[2]>temp@cval[2,1]) {
    difull_test[i,3]<-paste(round(temp@teststat[2],2),"***",sep="")
  }
  else if (temp@teststat[2]>temp@cval[2,2]) {
    difull_test[i,3]<-paste(round(temp@teststat[2],2),"**",sep="")
  }
  else if (temp@teststat[2]>temp@cval[2,3]) {
    difull_test[i,3]<-paste(round(temp@teststat[2],2),"*",sep="")
  }
  else {difull_test[i,3]<-paste(round(temp@teststat[2],2),"",sep="")}
  #Phi3 test of drift term
  if (temp@teststat[3]>temp@cval[3,1]) {
    difull_test[i,4]<-paste(round(temp@teststat[3],2),"***",sep="")
  }
  else if (temp@teststat[3]>temp@cval[3,2]) {
    difull_test[i,4]<-paste(round(temp@teststat[3],2),"**",sep="")
  }
  else if (temp@teststat[3]>temp@cval[3,3]) {
    difull_test[i,4]<-paste(round(temp@teststat[3],2),"*",sep="")
  }
  else {difull_test[i,4]<-paste(round(temp@teststat[3],2),"",sep="")}
}
setwd(wd_tables)
difull_table<-xtable(difull_test)
print.xtable(difull_table, type="latex", file="difulltest.tex")


# Section 4: HP-filter---------------------------------------------------------

# HP filter to decycle cyclical data
setwd(wd_others)

###Consumption
hp.decom <- hpfilter(consumption, freq = 4000, type = "lambda")
png("filter_1.png", height =2.5, width = 5, units = 'in', 
    type="windows", res=400)
par(mar = c(2, 2, 2, 2))
plot.ts(consumption, xaxt="n", ylab = "") 
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
lines(hp.decom$trend, col = "red") 
legend("topleft", legend = c("Consumption", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
title("Consumption")
dev.off()
consumption<-ts(hp.decom$trend)

#remove the annual seasonality:
consumption_t<-consumption
temp=1
for (i in 320:330) {
  consumption_t[i]<-consumption_t[i]*temp
  temp<-temp-0.0095
}
for (i in 331:350) {
  consumption_t[i]<-consumption_t[i]*temp
}
temp2<-(1-temp)/15
for (i in 351:366) {
  consumption_t[i]<-consumption_t[i]*temp
  temp<-temp+temp2
}
for (i in 367:380) {
  consumption_t[i]<-consumption_t[i]*temp
  temp<-temp+0.008
}
for (i in 381:400) {
  consumption_t[i]<-consumption_t[i]*temp
}
temp2<-(temp-1)/30
for (i in 401:431) {
  consumption_t[i]<-consumption_t[i]*temp
  temp<-temp-temp2
}
#plot(consumption_t,col="red",ylim=c(160000000,320000000))
#lines(consumption)
consumption<-consumption_t
 
###Electricity
hp.decom <- hpfilter(electricity, freq = 500, type = "lambda")
png("filter_2.png", height =2.5, width = 5, units = 'in', 
    type="windows", res=400)
par(mar = c(2, 2, 2, 2))
plot.ts(electricity, xaxt="n", ylab = "") 
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
lines(hp.decom$trend, col = "red") 
legend("topleft", legend = c("Electricity", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
title("Electricity")
dev.off()
electricity<-ts(hp.decom$trend)

###Covid-19
hp.decom <- hpfilter(c19_cases, freq = 500, type = "lambda")
png("filter_3.png", height =2.5, width = 5, units = 'in', 
    type="windows", res=400)
par(mar = c(2, 2, 2, 2))
plot.ts(c19_cases, ylab = "",xaxt="n") 
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
lines(hp.decom$trend, col = "red") 
legend("topleft", legend = c("Covid-19 cases", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
title("Covid-19 cases")
dev.off()
c19_cases<-ts(hp.decom$trend)

### Net bankruptcies
hp.decom <- hpfilter(net_bankruptcies, freq = 6000, type = "lambda")
png("filter_4.png", height =2.5, width = 5, units = 'in', 
    type="windows", res=400)
par(mar = c(2, 2, 2, 2))
plot.ts(net_bankruptcies, ylab = "",xaxt="n") 
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
lines(hp.decom$trend, col = "red") 
legend("topleft", legend = c("Net new firms", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
title("Net new firms")
dev.off()
net_bankruptcies<-ts(hp.decom$trend)

### Air pollution
hp.decom <- hpfilter(air_pollution, freq = 2000, type = "lambda")
png("filter_5.png", height =2.5, width = 5, units = 'in', 
    type="windows", res=400)
par(mar = c(2, 2, 2, 2))
plot.ts(air_pollution, ylab = "",xaxt="n")
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
lines(hp.decom$trend, col = "red") 
legend("topleft", legend = c("Air pollution", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
title("Air pollution")
dev.off()
air_pollution<-ts(hp.decom$trend)

#C19 early-pandemic correction
hp.decom <- hpfilter(c19_tests, freq = 6000, type = "lambda")
old_c19_cases<-c19_cases
c19_tests<-ts(hp.decom$trend)
mean<-mean(c19_tests[290:330])

temp0<-0
temp<-0
for (i in 60:75) {
  c19_cases[i]<-((1-temp0)*c19_cases[i])+temp0*c19_cases[i]*(1/(c19_tests[i]/mean))
  temp0<-temp0+0.06666
}
for (i in 76:270) {
  c19_cases[i]<-c19_cases[i]*(1/(c19_tests[i]/mean))
}
for (i in 271:290) {
  c19_cases[i]<-(temp*c19_cases[i])+(1-temp)*c19_cases[i]*(1/(c19_tests[i]/mean))
  temp<-temp+0.05
}

#Covid-19 plots:
setwd(wd_others)

png("covid19_cases_correction.png", height=3, width =5, units = 'in', type="windows", res=400)
par(mar=c(2,2,0.5,0.5))
plot(ts(c19_cases,frequency=365,start=c(2020,01,01)), col="red",ylab="",xlab="",xaxt="n")
axis(1, at = c(2020,2020.25,2020.5,2020.75,2021,2021.25), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
lines(ts(old_c19_cases,frequency=365,start=c(2020,01,01)))
legend("topleft", legend = c("Measured C19 cases", "Corrected C19 cases"), lty = 1, 
       col = c("black", "red"), bty = "n")
dev.off()

png("Cases_and_stringency.png", height=3, width =6, units = 'in', type="windows", res=400)
time=seq(1,n3+n5)
par(mar=c(3,5,2.5,4))
plot(time, old_c19_cases, pch=16, axes=FALSE, ylim=c(0,9000), xlab="", ylab="", 
     type="l",col="black",main="Stringency compared to Covid-19 cases")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("C19 cases",side=2,line=3.3)
box()
par(new=TRUE)
plot(time, stringency, pch=15,  xlab="", ylab="", ylim=c(0,90), 
     axes=FALSE, type="l", col="red")
mtext("Stringency",side=4,col="red",line=2.5) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
axis(1, at = c(0,90,180,270,360,450), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
dev.off()

c19_percentagepositive<-ts(c19_percentagepositive,frequency=365,start=c(2020,01,01))
c19_tests<-ts(c19_tests,frequency=365,start=c(2020,01,01))
png("positive_cases.png", height=3, width =6, units = 'in', type="windows", res=400)
time=seq(1,n3+n5)
par(mar=c(3,5,2.5,4))
plot(time, c19_tests, pch=16, axes=FALSE, ylim=c(0,35000), xlab="", ylab="", 
     type="l",col="black",main="Proportion of positive tests")
axis(2, ylim=c(0,35000),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Total C19 tests",side=2,line=3.8)
box()
par(new=TRUE)
plot(time, c19_percentagepositive, pch=15,  xlab="", ylab="", ylim=c(0,40), 
     axes=FALSE, type="l", col="red")
mtext("% positive",side=4,col="red",line=2) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
axis(1, at = c(0,90,180,270,360,450), labels=c("Jan-20", "Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"))
dev.off()

# Air pollution plots
setwd(wd_others)
pollution2019<-tail(pollution$No2, nrow(pollution)-1096)
pollution2019_deseason<-tail(data.frame(STL_NO2$time.series[,2]+STL_NO2$time.series[,3]),nrow(pollution)-1096)
hp.decom1 <- hpfilter(pollution2019, freq = 2000, type = "lambda")
hp.decom2 <- hpfilter(pollution2019_deseason, freq = 2000, type = "lambda")
nair<-length(pollution2019)
temp<-ts(as.data.frame(matrix(0, ncol = 1, nrow = 365)),start=c(2019,1),frequency=365)
new_stringency<-c(temp,stringency)

png("pol_compa_RAW.png", height=2.75, width =4.5, units = 'in', type="windows", res=400)
time=seq(1,nair)
par(mar=c(3,5,2.5,4))
plot(time, pollution2019, pch=16, axes=FALSE, ylim=c(0,60), xlab="", ylab="", 
     type="l",col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3),main="Raw data")
lines(hp.decom1$trend)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("NO2",side=2,line=3.3)
box()
par(new=TRUE)
plot(time, new_stringency, pch=15,  xlab="", ylab="", ylim=c(0,90), 
     axes=FALSE, type="l", col="red")
mtext("Stringency",side=4,col="red",line=2.5) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
axis(1, at = c(0,182,365,547,730,850), labels=c("Jan-19", "Jul-19","Jan-20","Jul-20","Jan-21", "Apr-21"))
dev.off()

png("pol_compa_DESEASON.png", height=2.75, width =4.5, units = 'in', type="windows", res=400)
time=seq(1,nair)
par(mar=c(3,5,2.5,4))
plot(time, ts(pollution2019_deseason), pch=16, axes=FALSE, ylim=c(0,60), xlab="", ylab="", 
     type="l",col= rgb(red = 0, green = 0, blue = 0, alpha = 0.3),main="Deseasonalized data")
lines(hp.decom2$trend)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("NO2",side=2,line=3.3)
box()
par(new=TRUE)
plot(time, new_stringency, pch=15,  xlab="", ylab="", ylim=c(0,90), 
     axes=FALSE, type="l", col="red")
mtext("Stringency",side=4,col="red",line=2.5) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
axis(1, at = c(0,182,365,547,730,850), labels=c("Jan-19", "Jul-19","Jan-20","Jul-20","Jan-21", "Apr-21"))
dev.off()

# Section 5: VAR estimation---------------------------------------------------
#VAR estimation
consumption<-ts(consumption,frequency=365,start=c(2020,01,01))
SMI<-ts(SMI,frequency=365,start=c(2020,01,01))
unemployment_proxy<-ts(unemployment_proxy,frequency=365,start=c(2020,01,01))
electricity<-ts(electricity,frequency=365,start=c(2020,01,01))
mobility<-ts(mobility,frequency=365,start=c(2020,01,01))
net_bankruptcies<-ts(net_bankruptcies,frequency=365,start=c(2020,01,01))
air_pollution<-ts(air_pollution,frequency=365,start=c(2020,01,01))
c19_cases<-ts(c19_cases,frequency=365,start=c(2020,01,01))
stringency<-ts(stringency,frequency=365,start=c(2020,01,01))

data_endogenous<-cbind(consumption, SMI, unemployment_proxy, electricity, mobility,net_bankruptcies,air_pollution)
data_exogenous<-cbind(c19_cases,stringency )
data_stacked<-cbind(consumption, SMI, unemployment_proxy, electricity, mobility,net_bankruptcies,air_pollution,c19_cases,stringency)
number_lags<-VARselect(data_endogenous) #lagselector
VAR_Results<-VAR(data_endogenous, p=5, exogen=data_exogenous)
VAR_Results_stacked<-VAR(data_stacked, p=5)
#summary(VAR_Results)

# Section 6: Tests-------------------------------------------------------------

#Cointegration test  
testdata<-data.frame(consumption, SMI, unemployment_proxy, electricity, 
                     mobility,net_bankruptcies,air_pollution,c19_cases, stringency)
jotest=ca.jo(testdata, type="trace", K=2, ecdet="none", spec="longrun")
#summary(jotest)

# Model fit: Portmanteau Test (serial correlation)
serial <- serial.test(VAR_Results, lags.pt = 5, type = "PT.adjusted")
serial
#plot(serial, names = "consumption")

# Heteroscedasticity: ARCH Lagrange-Multiplier test
bv.arch <- arch.test(VAR_Results, lags.multi = 5, multivariate.only = TRUE)
bv.arch

# Normality test: JB-test
bv.norm <- normality.test(VAR_Results, multivariate.only = TRUE)
bv.norm

# Section 7: Impulse responses-------------------------------------------------

# COVID-19 SHOCK
setwd(wd_IRF)
png("IRF_1_1.png", height =3, width = 3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "consumption", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Consumption response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_2.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "SMI", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "SMI response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_3.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "unemployment_proxy", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Unemployment response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_4.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "stringency", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Stringency response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_5.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "electricity", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Electricity response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_6.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "mobility", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Mobility response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_7.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "net_bankruptcies", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Net new firms response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_1_8.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "c19_cases", response = "air_pollution", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Air pollution response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()


# STRINGENCY SHOCK
setwd(wd_IRF)
png("IRF_2_1.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "consumption", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Consumption response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_2.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "SMI", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "SMI response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_3.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "unemployment_proxy", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Unemployment response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_4.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "c19_cases", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Covid-19 cases response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_5.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "electricity", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Electricity response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_6.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "mobility", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Mobility response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_7.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "net_bankruptcies", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Net new firms response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()
png("IRF_2_8.png", height =3, width =3, units = 'in', 
    type="windows", res=400)
plot(irf(VAR_Results_stacked, impulse = "stringency", response = "air_pollution", n.ahead = 45, boot = FALSE)
     , ylab = "", main = "Air pollution response", mar=c(0,0,0,0),oma=c(2.5,2.5,3,1))
dev.off()


# Section 8: Predictions-------------------------------------------------------

#Currently: covid =2000-1348; stringency=52.77
n_pred<-90

#Prediction (optimistic: go back to 100 daily cases, stringency go back to 10)
setwd(wd_predictions)
exogen_prediction<-data_exogenous[c(1:n_pred),]
exogen_prediction[,1]<-seq(1300,100,by=((100 - 1300)/(n_pred- 1)) )
exogen_prediction[,2]<-seq(52,10,by =((10 - 52)/(n_pred- 1)) )
prediction<-predict(VAR_Results, n.ahead = n_pred, ci = 0.9, dumvar = exogen_prediction)

#add back the no2 seasonality
  n=prediction$model$totobs
  ntot=n+n_pred
# temp <- hpfilter(STL_NO2$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
# STL_NO2$time.series[(n+1):ntot,1]<-ts(temp$trend)
# prediction$fcst$air_pollution[,1]<-prediction$fcst$air_pollution[,1]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,2]<-prediction$fcst$air_pollution[,2]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,3]<-prediction$fcst$air_pollution[,3]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$endog[,7]<-prediction$endog[,7]+STL_NO2$time.series[1:n,1]

#add back the electricity seasonality
# temp <- hpfilter(STL_elec$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
# STL_elec$time.series[(n+1):ntot,1]<-ts(temp$trend)
# prediction$fcst$electricity[,1]<-prediction$fcst$electricity[,1]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,2]<-prediction$fcst$electricity[,2]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,3]<-prediction$fcst$electricity[,3]+STL_elec$time.series[(n+1):ntot,1]
# prediction$endog[,4]<-prediction$endog[,4]+STL_elec$time.series[1:n,1]

#X-axis labels positions
start2021<-1-((ntot-365)/ntot)
mid2020<-start2021/2
mid2021<-mid2020+start2021

png("Prediction_1_1.png", height=2.2, width = 3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="consumption",xaxt="n", main="Positive",xaxt="n")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_1_2.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="SMI", xaxt="n", main="Positive")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_1_3.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="unemployment_proxy", xaxt="n", main="Positive")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_1_4.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="electricity", xaxt="n", main="Positive")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_1_5.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="mobility", xaxt="n", main="Positive")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_1_6.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="net_bankruptcies", xaxt="n", main="Positive")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_1_7.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="air_pollution", xaxt="n", main="Positive")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()

results_positive<-data.frame(consumption=prediction$fcst$consumption[,1],
                             SMI=prediction$fcst$SMI[,1],
                             unemployment_proxy=prediction$fcst$unemployment_proxy[,1],
                             electricity=prediction$fcst$electricity[,1],
                             mobility=prediction$fcst$mobility[,1],
                             net_bankruptcies=prediction$fcst$net_bankruptcies[,1],
                             air_pollution=prediction$fcst$air_pollution[,1],
                             c19_cases=exogen_prediction[,1],
                             stringency=exogen_prediction[,2])

#Prediction (Neutral: stay at 1300 daily cases, stringency stays at 52)
setwd(wd_predictions)
exogen_prediction<-data_exogenous[c(1:90),]
exogen_prediction[,1]<-seq(1300,1299,by =((1299 - 1300)/(n_pred- 1)) )
exogen_prediction[,2]<-seq(52,51,by =((51 - 52)/(n_pred- 1)) )
par(mfrow = c(1, 1), mar=c(2,2,2,2))
prediction<-predict(VAR_Results, n.ahead = n_pred, ci = 0.9, dumvar = exogen_prediction)
# prediction$fcst$air_pollution[,1]<-prediction$fcst$air_pollution[,1]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,2]<-prediction$fcst$air_pollution[,2]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,3]<-prediction$fcst$air_pollution[,3]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$endog[,7]<-prediction$endog[,7]+STL_NO2$time.series[1:n,1]
# temp <- hpfilter(STL_elec$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
# STL_elec$time.series[(n+1):ntot,1]<-ts(temp$trend)
# prediction$fcst$electricity[,1]<-prediction$fcst$electricity[,1]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,2]<-prediction$fcst$electricity[,2]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,3]<-prediction$fcst$electricity[,3]+STL_elec$time.series[(n+1):ntot,1]
# prediction$endog[,4]<-prediction$endog[,4]+STL_elec$time.series[1:n,1]

png("Prediction_2_1.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="consumption",xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_2_2.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="SMI", xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_2_3.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="unemployment_proxy", xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_2_4.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="electricity", xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_2_5.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="mobility", xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_2_6.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="net_bankruptcies", xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_2_7.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="air_pollution", xaxt="n", main="Neutral")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()

results_neutral<-data.frame(consumption=prediction$fcst$consumption[,1],
                            SMI=prediction$fcst$SMI[,1],
                            unemployment_proxy=prediction$fcst$unemployment_proxy[,1],
                            electricity=prediction$fcst$electricity[,1],
                            mobility=prediction$fcst$mobility[,1],
                            net_bankruptcies=prediction$fcst$net_bankruptcies[,1],
                            air_pollution=prediction$fcst$air_pollution[,1],
                            c19_cases=exogen_prediction[,1],
                            stringency=exogen_prediction[,2])


#Prediction (Negative: go up to 5500 daily cases, stringency go up to 80)
setwd(wd_predictions)
exogen_prediction<-data_exogenous[c(1:90),]
exogen_prediction[,1]<-seq(1300,5500,by =((5500 - 1300)/(n_pred- 1)) )
exogen_prediction[,2]<-seq(52,80,by =((80 - 52)/(n_pred- 1)) )
par(mfrow = c(1, 1), mar=c(2,2,2,2))
prediction<-predict(VAR_Results, n.ahead = n_pred, ci = 0.9, dumvar = exogen_prediction)
# prediction$fcst$air_pollution[,1]<-prediction$fcst$air_pollution[,1]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,2]<-prediction$fcst$air_pollution[,2]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,3]<-prediction$fcst$air_pollution[,3]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$endog[,7]<-prediction$endog[,7]+STL_NO2$time.series[1:n,1]
# temp <- hpfilter(STL_elec$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
# STL_elec$time.series[(n+1):ntot,1]<-ts(temp$trend)
# prediction$fcst$electricity[,1]<-prediction$fcst$electricity[,1]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,2]<-prediction$fcst$electricity[,2]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,3]<-prediction$fcst$electricity[,3]+STL_elec$time.series[(n+1):ntot,1]
# prediction$endog[,4]<-prediction$endog[,4]+STL_elec$time.series[1:n,1]

png("Prediction_3_1.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="consumption",xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_3_2.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="SMI", xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_3_3.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="unemployment_proxy", xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_3_4.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="electricity", xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_3_5.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="mobility", xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_3_6.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="net_bankruptcies", xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_3_7.png", height=2.2, width =3, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="air_pollution", xaxt="n", main="Negative")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()

results_negative<-data.frame(consumption=prediction$fcst$consumption[,1],
                             SMI=prediction$fcst$SMI[,1],
                             unemployment_proxy=prediction$fcst$unemployment_proxy[,1],
                             electricity=prediction$fcst$electricity[,1],
                             mobility=prediction$fcst$mobility[,1],
                             net_bankruptcies=prediction$fcst$net_bankruptcies[,1],
                             air_pollution=prediction$fcst$air_pollution[,1],
                             c19_cases=exogen_prediction[,1],
                             stringency=exogen_prediction[,2])


#Prediction (Low covid, High stringency: go back to 100 daily cases, stringency go up to 90)
setwd(wd_predictions)
exogen_prediction<-data_exogenous[c(1:n_pred),]
exogen_prediction[,1]<-seq(1300,100,by =((100 - 1300)/(n_pred- 1)) )
exogen_prediction[,2]<-seq(52,80,by =((80 - 52)/(n_pred- 1)) )
prediction<-predict(VAR_Results, n.ahead = n_pred, ci = 0.9, dumvar = exogen_prediction)
prediction$fcst$air_pollution[,1]<-prediction$fcst$air_pollution[,1]+STL_NO2$time.series[(n+1):ntot,1]
prediction$fcst$air_pollution[,2]<-prediction$fcst$air_pollution[,2]+STL_NO2$time.series[(n+1):ntot,1]
prediction$fcst$air_pollution[,3]<-prediction$fcst$air_pollution[,3]+STL_NO2$time.series[(n+1):ntot,1]
prediction$endog[,7]<-prediction$endog[,7]+STL_NO2$time.series[1:n,1]
temp <- hpfilter(STL_elec$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
STL_elec$time.series[(n+1):ntot,1]<-ts(temp$trend)
prediction$fcst$electricity[,1]<-prediction$fcst$electricity[,1]+STL_elec$time.series[(n+1):ntot,1]
prediction$fcst$electricity[,2]<-prediction$fcst$electricity[,2]+STL_elec$time.series[(n+1):ntot,1]
prediction$fcst$electricity[,3]<-prediction$fcst$electricity[,3]+STL_elec$time.series[(n+1):ntot,1]
prediction$endog[,4]<-prediction$endog[,4]+STL_elec$time.series[1:n,1]

png("Prediction_4_1.png", height =3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="consumption",xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_4_2.png", height =3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="SMI", xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_4_3.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="unemployment_proxy", xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_4_4.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="electricity", xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_4_5.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="mobility", xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_4_6.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="net_bankruptcies", xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_4_7.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="air_pollution", xaxt="n", main="Low covid, High stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()


#Prediction (High covid, Low stringency: go up to 5500 daily cases, stringency go down to 5)
setwd(wd_predictions)
exogen_prediction<-data_exogenous[c(1:n_pred),]
exogen_prediction[,1]<-seq(1300,5500,by =((5500 - 1300)/(n_pred- 1)) )
exogen_prediction[,2]<-seq(50,5,by =((5 - 50)/(n_pred- 1)) )
prediction<-predict(VAR_Results, n.ahead = n_pred, ci = 0.9, dumvar = exogen_prediction)
prediction$fcst$air_pollution[,1]<-prediction$fcst$air_pollution[,1]+STL_NO2$time.series[(n+1):ntot,1]
prediction$fcst$air_pollution[,2]<-prediction$fcst$air_pollution[,2]+STL_NO2$time.series[(n+1):ntot,1]
prediction$fcst$air_pollution[,3]<-prediction$fcst$air_pollution[,3]+STL_NO2$time.series[(n+1):ntot,1]
prediction$endog[,7]<-prediction$endog[,7]+STL_NO2$time.series[1:n,1]
temp <- hpfilter(STL_elec$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
STL_elec$time.series[(n+1):ntot,1]<-ts(temp$trend)
prediction$fcst$electricity[,1]<-prediction$fcst$electricity[,1]+STL_elec$time.series[(n+1):ntot,1]
prediction$fcst$electricity[,2]<-prediction$fcst$electricity[,2]+STL_elec$time.series[(n+1):ntot,1]
prediction$fcst$electricity[,3]<-prediction$fcst$electricity[,3]+STL_elec$time.series[(n+1):ntot,1]
prediction$endog[,4]<-prediction$endog[,4]+STL_elec$time.series[1:n,1]

png("Prediction_5_1.png", height =3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="consumption",xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_5_2.png", height =3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="SMI", xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_5_3.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="unemployment_proxy", xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_5_4.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="electricity", xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_5_5.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="mobility", xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_5_6.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="net_bankruptcies", xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()
png("Prediction_5_7.png", height=3, width =4.5, units = 'in', type="windows", res=400)
par(mar=c(2.5, 2.5, 2, 1))
plot(prediction,names="air_pollution", xaxt="n", main="High covid, Low stringency")
axis(1, at = c(0,mid2020,start2021,mid2021), labels=c("Jan-20","Jul-20","Jan-21","Jul-21"))
dev.off()

#Export dataframe of data
data_stacked<-cbind(consumption, SMI, unemployment_proxy, electricity, mobility,net_bankruptcies,air_pollution,c19_cases,stringency)
results_positive<-rbind(data_stacked,results_positive)
results_neutral<-rbind(data_stacked,results_neutral)
results_negative<-rbind(data_stacked,results_negative)
write.csv(data_stacked,"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/data_final.csv", row.names = FALSE)
write.csv(results_positive,"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/results_positive.csv", row.names = FALSE)
write.csv(results_neutral,"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/results_neutral.csv", row.names = FALSE)
write.csv(results_negative,"C:/Users/alexo/Desktop/Recherche Empirique/2. Projet/ANALYSE/results_negative.csv", row.names = FALSE)

# Section 9: Validation-------------------------------------------------------

data_endogenous_cv<-data_endogenous[1:(nrow(data_endogenous)-70),]
data_exogenous_cv<-data_exogenous[1:(nrow(data_exogenous)-70),]
number_lags_cv<-VARselect(data_endogenous)
VAR_Results_cv<-VAR(data_endogenous_cv, p=7, exogen=data_exogenous_cv)

n_pred<-70
setwd(wd_predictions)
exogen_prediction<-data_exogenous_cv[c(1:n_pred),]
# Actual (02.02.2021): 1500 cases; 75 stringency
# Real 90 days (02.05.2021): 1300 cases; 52 stringency
# exogen_prediction[,1]<-seq(1500,1300,by=((1300 - 1500)/(n_pred- 1)) )
# exogen_prediction[,2]<-seq(75,52,by =((52 - 75)/(n_pred- 1)) )
exogen_prediction[,1]<-data_exogenous[419:488,1]
exogen_prediction[,2]<-data_exogenous[419:488,2]
prediction<-predict(VAR_Results_cv, n.ahead = n_pred, ci = 0.9, dumvar = exogen_prediction)

#add back the no2 seasonality
# n=prediction$model$totobs
# ntot=n+n_pred
# np<-nrow(data_endogenous)
# data_endogenous[,7]<-data_endogenous[,7]+STL_NO2$time.series[1:np,1]
# temp <- hpfilter(STL_NO2$time.series[(n+1):ntot,1], freq = 500, type = "lambda")
# STL_NO2$time.series[(n+1):ntot,1]<-ts(temp$trend)
# prediction$fcst$air_pollution[,1]<-prediction$fcst$air_pollution[,1]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,2]<-prediction$fcst$air_pollution[,2]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$fcst$air_pollution[,3]<-prediction$fcst$air_pollution[,3]+STL_NO2$time.series[(n+1):ntot,1]
# prediction$endog[,7]<-prediction$endog[,7]+STL_NO2$time.series[1:n,1]

#add back the electricity seasonality
# data_endogenous[,4]<-data_endogenous[,4]+STL_elec$time.series[1:np,1]
# temp <- hpfilter(STL_elec$time.series[(n+1):ntot,1], freq = 1000, type = "lambda")
# STL_elec$time.series[(n+1):ntot,1]<-ts(temp$trend)
# prediction$fcst$electricity[,1]<-prediction$fcst$electricity[,1]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,2]<-prediction$fcst$electricity[,2]+STL_elec$time.series[(n+1):ntot,1]
# prediction$fcst$electricity[,3]<-prediction$fcst$electricity[,3]+STL_elec$time.series[(n+1):ntot,1]
# prediction$endog[,4]<-prediction$endog[,4]+STL_elec$time.series[1:n,1]

#Graph
x<-seq(419,488)

p1<-ggplot()+
  theme_classic() + 
  ggtitle("Consumption")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,1]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=prediction$fcst$consumption[,3], ymin=prediction$fcst$consumption[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,1],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(1.7E8,3.2E8)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p2<-ggplot()+
  theme_classic() + 
  ggtitle("SMI")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,2]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$SMI[,3], ymin=y2<-prediction$fcst$SMI[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,2],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(8000,12000)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p3<-ggplot()+
  theme_classic() + 
  ggtitle("Unemployment")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,3]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$unemployment_proxy[,3], ymin=y2<-prediction$fcst$unemployment_proxy[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,3],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(140000,220000)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p4<-ggplot()+
  theme_classic() + 
  ggtitle("Electricity")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,4]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$electricity[,3], ymin=y2<-prediction$fcst$electricity[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,4],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(80,170)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p5<-ggplot()+
  theme_classic() + 
  ggtitle("Mobility")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,5]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$mobility[,3], ymin=y2<-prediction$fcst$mobility[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,5],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(-65,10)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p6<-ggplot()+
  theme_classic() + 
  ggtitle("Net new firms")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,6]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$net_bankruptcies[,3], ymin=y2<-prediction$fcst$net_bankruptcies[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,6],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(50,260)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p7<-ggplot()+
  theme_classic() + 
  ggtitle("Air pollution")+
  geom_line(aes(x=seq(1,nrow(data_endogenous)), y=data_endogenous[,7]))+
  geom_vline(xintercept=419,color="grey60")+
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$air_pollution[,3], ymin=y2<-prediction$fcst$air_pollution[,2]), fill="grey", alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,7],x=seq(419,488)),col="red")+
  labs(y = "", x = "")+
  ylim(10,45)+
  scale_x_continuous(labels = c("Jan-20","Apr-20","Jul-20","Oct-20","Jan-21","Apr-21"), breaks = c(0,91,182,273,365,456))+
  theme(text = element_text(size=14),plot.title = element_text(size=22))

p8<-ggplot()+ #Legend in P8
  theme_classic() + 
  geom_ribbon(aes(x=x, ymax=y2<-prediction$fcst$air_pollution[,3], ymin=y2<-prediction$fcst$air_pollution[,2],fill="Predicted (90% CI)"), alpha=.7, color="blue", linetype=2)+
  geom_line(aes(y=data_endogenous[419:488,7],x=seq(419,488),col="Actual"))+
  labs(y = "", x = "")+
  ylim(10,45)+
  scale_fill_manual(name = '', values = c("grey","red")) +
  scale_color_manual(name = '', values = c("red","red"))+
  theme(legend.title = element_text(size=1),
        legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'),
        legend.spacing.x = unit(0.4, 'cm'),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
legend <- cowplot::get_legend(p8)
p8_leg<-as.ggplot(legend)


ptot<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8_leg, ncol=2, nrow=4)
pdf("fig1.pdf", width=12.5, height=14)
ptot
dev.off()

#MAE
mae_df<-data.frame()
mae_df[1,1]<-"Consumption"
mae_df[1,2]<-mean(abs(prediction$fcst$consumption[,1] - data_endogenous[419:488,1]))
mae_df[2,1]<-"SMI"
mae_df[2,2]<-mean(abs(prediction$fcst$SMI[,1] - data_endogenous[419:488,2]))
mae_df[3,1]<-"Unemployment"
mae_df[3,2]<-mean(abs(prediction$fcst$unemployment_proxy[,1] - data_endogenous[419:488,3]))
mae_df[4,1]<-"Electricity"
mae_df[4,2]<-mean(abs(prediction$fcst$electricity[,1] - data_endogenous[419:488,4]))
mae_df[5,1]<-"Mobility"
mae_df[5,2]<-mean(abs(prediction$fcst$mobility[,1] - data_endogenous[419:488,5]))
mae_df[6,1]<-"Net new firms"
mae_df[6,2]<-mean(abs(prediction$fcst$net_bankruptcies[,1] - data_endogenous[419:488,6]))
mae_df[7,1]<-"Air pollution"
mae_df[7,2]<-mean(abs(prediction$fcst$air_pollution[,1] - data_endogenous[419:488,7]))