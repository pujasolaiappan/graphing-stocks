#Empties the workspace
rm(list = ls())

library(readr)
library(dplyr)
library(zoo)
library(readxl)
library(mosaic)
library(ggplot2)
library(quantmod)
library(lubridate)
library(xts)
library(gridExtra)
library(plotly)
library(caret)
library(pracma)
library(splines)
library(polynom)
library(graphics)
require(stats)


ROCRatio<- function(x, n) {
  x = as.vector(x)
  y = rep(0,n)
  for (i in n+1: length(x))
  {
    y[i] = x[i]/x[i-n] 
  }
  return (y)
}

#reads the csv and creates the table
createData <- function (companyName, DataAdress, env = globalenv()){
  assign(eval(substitute(companyName)), read_csv(DataAdress), envir = env)
}

#Returns the date column in Date Format
getDate <- function(dfName){
  d < - dfName$Date
  d <- ymd(d)
  return(d)
}

#Creates the data frame
createDF <- function(companyName, dfName, env = globalenv()){
  d <- (companyName %>% select(Date))
  d <- ymd(d)
  assign(deparse(substitute(companyName)), mutate(companyName, Date = d))
  assign(eval(substitute(dfName)), data.frame(companyName), envir = env)
  names(eval(substitute(dfName)))[1]<-"Date"
}

#Creates index by date using an xts object
getIndex <- function(dfName){
  timexts<-xts(dfName$Close,order.by = dfName[,1])
  return(c(1:length(timexts)))
}

#Adds the index by date to the dataframe
createIndex <- function(dfName){
  dataAP<-data.frame(getIndex(dfName),dfName)
}

#Adds columns to the dataframe storing average and true average values
AddAvgs <- function(companyName, env = globalenv())
{
  assign(deparse(substitute(companyName)), mutate(companyName, Avg = (companyName$High + companyName$Low)/2, TrueAvg = pmax(companyName$High- companyName$Low, companyName$High - companyName$`Prev Close`, companyName$`Prev Close` - companyName$Low)), envir = env)
}

#Adds columns to the dataframe storing moving average values
AddMAs <- function (companyName, env = globalenv()){
  assign(
    deparse(substitute(companyName)), 
    mutate(companyName, RollingAvg9 = (rollmean(as.numeric(companyName$Avg), k=9, fill=NA, align='right')), 
           RollingAvg21 = (rollmean(as.numeric(companyName$Avg), k=21, fill=NA, align='right')),
           RollingAvg50 = (rollmean(as.numeric(companyName$Avg), k=50, fill=NA, align='right')),
           RollingAvg120 = (rollmean(as.numeric(companyName$Avg), k=120, fill=NA, align='right')),
           RollingAvg300 = (rollmean(as.numeric(companyName$Avg), k=300, fill=NA, align='right'))
    ),
    envir = env)
}

AddROC <- function (companyName, env = globalenv()){
  assign(
    deparse(substitute(companyName)),
    mutate(ROC40 = (ROCRatio(as.numeric(companyName$Close), 40))),
    envir = env)
}

initializeCo <- function(companyName, dirAddress, dfName, env = globalenv()){
  createData(companyName, dirAddress)
  var <- eval(substitute(companyName)) 
  assign(var, get(eval(companyName)))
  AddAvgs(eval(as.name(companyName)))
  assign(eval(substitute(companyName)), var, envir = env)
  #colnames(CN3)
  #AddMAs(get(eval(companyName)))
  #AddROC(CN)
  #assign(eval(substitute(companyName)), CN, envir = env)
  #createDF(get(eval(eval(substitute(CompanyName)))), dfName)
}

#Gives the polynomial approximation of the data for polynomials of input degree n if it exists
polyApprox <- function(dfName, var, n)
{
  date <- dfName$Date 
  date <- ymd(date)
  ggplot(dfName, aes(date, {{var}}))+
    geom_point(cex = 0.75)+
    geom_smooth(method = "lm", formula = y ~ poly(x, n), linewidth= 1)
}

#Gives the polynomial approximation of the data for polynomials of degrees 5 10 and 15 if they exist
polyApprox5<- function(dfName, var)
{
  date <- dfName$Date 
  date <- ymd(date)
  ggplot(dfName, aes(date, {{var}}))+
    geom_point(cex = 0.75)+
    geom_smooth(method = "lm", formula = y ~ poly(x, 5), color = "orange", linewidth= 1)+
    geom_smooth(method = "lm", formula = y ~ poly(x, 10), color = "blue", linewidth= 1)+
    geom_smooth(method = "lm", formula = y ~ poly(x, 15), color = "purple", linewidth= 1)
}
#polyApprox(dataJS, 18)
#polyApprox5(dataNI)

#Gives the data pertaining to the specified year from th input dataset
filterByYear <- function(dfName, year){
  return(dfName %>% filter(as.numeric(format(dfName$Date, "%Y")) %in% year))}
#dataAP10 <- filterByYear(dataAP, 2010)
#dataAP08 <- filterByYear(dataAP, 2008)

#Gives the data pertaining to the specified moth from th input dataset
filterByMonth <- function(dfName, month){
  return(dfName %>% filter(as.numeric(format(dfName$Date, "%m")) %in% month))
}

#Gives a dataframe containing data between the two years specified
filterYearInterval <- function(dfName, y1, y2){
  y = y1
  f<- filterByYear(dfName, y1) 
  while (y < y2){
    y = y+1
    f<-rbind( f, filterByYear(dfName, y))
  }
  return(f)
}

#Plots a generic line graph given the dataframe and variable name
plotLineGraph <- function(dfName, var){
  ggplot(dfName) + geom_line(aes(x= Date, y= {{var}}, group =1), color = 'maroon') + 
    xlab ("Time")+
    ylab (paste(deparse(substitute(dfName)), deparse(substitute(var))))
}

#Plots a semilog graph given the dataframe and the variable name
LplotLineGraph<- function(dfName, var){
  ggplot(dfName) + geom_line(aes(x= Date, y= {{var}}, group =1), color = 'maroon') + 
    scale_y_log10()+ 
    ylab(paste("Log", deparse(substitute(var)))) +
    xlab("Time")
}

#Plots 2 vaiables of a single company on separate graphs
Lplot2 <- function(dfName, var1, var2){
  p1 <- ggplot(dfName,aes(x = as.Date(Date), y = log(as.numeric({{var1}})), group = 1))+ 
    geom_line() + 
    geom_point(size = 0.05) + 
    ylab(paste("Log", deparse(substitute(var1)))) + 
    scale_x_date(date_breaks = "1 year") + 
    xlab("Time")
  p2 <- ggplot(dfName, aes(x= as.Date(Date), y= as.numeric({{var2}}), group =1)) + 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_y_log10()+ 
    ylab(paste("Log", deparse(substitute(var2)))) + 
    scale_x_date(date_breaks = "1 year") + 
    xlab("Time")
  grid.arrange(p1, p2, nrow = 2) 
}
#Lplot2(dataAP10, Close, Volume)

#Draws three log plots of three different companies
Lcompare3Co<- function(dfName1, dfName2, dfName3, var, dateBr){
  p1 <- ggplot(dfName1,aes(x = as.Date(Date), y = log(as.numeric({{var}})), group = 1))+ 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_x_date(date_breaks = dateBr) + 
    ylab(paste("Log", deparse(substitute(var)), deparse(substitute(dfName1))))+
    xlab("Time")
  p2 <- ggplot(dfName2, aes(x= as.Date(Date), y= as.numeric({{var}}), group =1)) + 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_y_log10()+ 
    scale_x_date(date_breaks = dateBr) + 
    ylab(paste("Log", deparse(substitute(var)), deparse(substitute(dfName2))))+
    xlab("Time")
  p3 <- ggplot(dfName3, aes(x= as.Date(Date), y= as.numeric({{var}}), group =1)) + 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_y_log10()+ 
    scale_x_date(date_breaks = dateBr)+ 
    ylab(paste("Log", deparse(substitute(var)), deparse(substitute(dfName3))))+
    xlab("Time")
  
  grid.arrange(p1, p2, p3, nrow = 3) 
}
#Lcompare3Co(dataAP, dataJS, dataNI, Close, "1 year")

#Draws plots of three diferent companies on separate graphs
Compare3Co<- function(dfName1, dfName2, dfName3, var, dateBr){
  p1 <- ggplot(dfName1,aes(x = as.Date(Date), y = as.numeric({{var}}), group = 1))+ 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_x_date(date_breaks = dateBr) + 
    ylab(paste(deparse(substitute(var)), deparse(substitute(dfName1))))+
    xlab("Time")
  p2 <- ggplot(dfName2, aes(x= as.Date(Date), y= as.numeric({{var}}), group =1)) + 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_x_date(date_breaks = dateBr) + 
    ylab(paste(deparse(substitute(var)), deparse(substitute(dfName2))))+
    xlab("Time")
  p3 <- ggplot(dfName3, aes(x= as.Date(Date), y= as.numeric({{var}}), group =1)) + 
    geom_line() + 
    geom_point(size = 0.05) + 
    scale_x_date(date_breaks = dateBr)+ 
    ylab(paste(deparse(substitute(var)), deparse(substitute(dfName3))))+
    xlab("Time")
  
  grid.arrange(p1, p2, p3, nrow = 3) 
}

candlestick <- function(dfName){
  n <- nrow(dfName)
  fig <- dfName %>% plot_ly(x = dfName$Date, type="candlestick",
                            open = dfName$Open, close = dfName$Close,
                            high = dfName$High, low = dfName$Low) 
  fig <- fig %>% layout(title = paste("CandleStick Chart from ", toString(dfName$Date[1]), "to", toString(dfName$Date[n])))
  fig
}
#candlestick(dataNI)

#Plots two line graphs for the diferent variables of the same company on a single graph
plot2Vars <- function(dfName, var1, var2){
  p <- ggplot(data = dfName, aes(x = Date, y = {{var1}}, group = 1)) + 
    geom_line(colour = "grey", aes(Date, {{var2}}), group =1) + 
    geom_line(colour = "#408FA6")
  ggplotly(p)
}
#plot2Vars(dataJS, RollingAvg9, RollingAvg300)

localMaxMin <- function(dfName,var, p)
{
  date <- dfName %>% select(Date)
  date <- date$Date
  date <- ymd(date)
  n = nrow(dfName)
  a1 <- as.numeric(date)[1]
  a2 <- as.numeric(date)[n]
  x = c()
  for (i in a1:a2){
    l = p(i-1)
    h = p(i+1)
    m = p(i)
    if(i<m && h<m || l>m && h>m)
    {
      append(x,i)
    }
  }
  return(x)
}

TrendwPoly <- function(dfName, var, p)
{
  n = nrow(dfName)
  x = localMaxMin(p)
  m = length(x)
  ggplot(dfName) + 
    geom_line(aes(x= Date, y= {{var}}, group =1), color = 'green') + 
    for (j in 1:m-1)
    {
      geom_segment(x = Date[j], xend = Date[j+1], y= {{var}}[j], yend = {{var}}[j+1])
    }
  geom_segment(x = Date[x[m-1]], xend = Date[x[m]], y= {{var}}[x[m-1]], yend = {{var}}[x[m]])
}
TrendwPoly(dataAP, Close, p1)

stagGraph <- function(dfName, var, p){
  x = localMaxMin(p)
  eq <- c()
  m = length(x)
  e = 10
  for (i in 2: m-3)
  {
    b <- p(i) +e
    a <- p(i) -e
    if((p(i-2)<b && p(i-2)>a) && (p(i+2)<b && p(i+2)>a))
    {
      c <- p(i-1) -e
      d <- p(i-1) +e
      if((p(i+1)<d && p(i+1)>c)){
        append(eq, i)
      }
    }
  }
  v1 = dfName{{var}}[eq[1]]
  v2 = dfName{{var}}[eq[1]+1]
  xMin = dfName$Date[eq[1]-4]
  xMax = dfName$Date[eq[1]+4]
  ggplot(dfName) + 
    geom_line(aes(x= Date, y= {{var}}, group =1), color = 'green') + 
    geom_segment(x = xMin, xend = xMax, y= v1, yend = v1)+
    geom_segment(x = xMin, xend = xMax, y= v2, yend = v2)
}

fitPolyCoefs <- function(dfName, n){
  date <- dfName$Date 
  date <- ymd(date)
  return(polyfit(as.numeric(date),as.numeric(dfName$Close), n))
}

fitPolyExp <- function(dfName, n){
  p = polynomial(coef = fitPolyCoefs(dfName, n))
  return(as.polynomial(p))
}

polyFunc <- function(vec){
  n = length(vec)
  f <- function(x){
    p = 0
    for(i in 1:n){
      p <- (p+ vec[i]*x^(i-1))
    }
    return(p)
  }
  return(f)
}

plotPoly <- function(dfName, vec){
  x = dfName$Close
  f <- polyFunc(vec)
  ggplot(dfName, aes(x)) +
    stat_function(fun=f, colour="red")
}


fitPolys <- function(dfName, vec){
  plot(dfName$Close~dfName$Date)
  for (i in 0:length(vec)){
    lm = createPolyLM(dfName, var, vec[i])
    points(dfName[,1], fitted(lm), col= "red", pch=10, cex = 0.1)
  }
}

#Testing different degrees of polynomial approximation using LM polynomial version
createPolyLM <- function(dfName, var, n){
  lm(dfName{{var}} ~ poly(dfName[,1], n, raw=TRUE))
}
#lm3 = createPolyLM(dataAP08, Close, 3)
#fitPolyCoefs(dataAP08, Close, 3)

getWtRsiVals<- function(dfName, m){
  price <- dfName$Close
  rsi <- RSI(price)
  Rwt <- RSI(price, n=m, maType="WMA", wts=dfName$Volume)
  return(Rwt)
}

getRsi2Vals<- function(dfName, m){
  price <- dfName$Close
  rsi <- RSI(price)
  R<- RSI(price, n=m, maType=list(maUp=list(EMA),maDown=list(WMA)))
  return(R)
}
#getRsiVals(dataNI, 14)

plotWtRSI2<- function(dfName, m1, m2){
  ggplot(dfName)+ 
    geom_line(aes(x=as.Date(Date), y = Close, group =1), color = "black")+ 
    geom_line(aes(x=as.Date(Date), y = getWtRsiVals(dfName, m1)), color = "blue")+ 
    geom_line(aes(x=as.Date(Date), y = getWtRsiVals(dfName, m2)), color = "red")+
    xlab("Date")+
    ylab("RSI")
}
#dataNI12 <- filterByYear(dataNI, 2012)
#plotWtRSI2(dataNI12, 14, 41)

getObvVal <- function(dfName){
  return(OBV(dfName$Close, dfName$Volume))
}

plotObv<- function(dfName,m){
  ggplot(dfName)+ 
    geom_line(aes(x=as.Date(Date), y = getObvVal(dfName))) + 
    ylab("OBV")+
    xlab("Date")
}
#plotObv(dataNI, 14)

#Rolling Averages of various periods to check what works best in a 15 year plot
plotMAs <- function(dfName){
  gMA <- ggplot(dfName) + 
    geom_line(aes(x= as.Date(Date), y= as.numeric(Close), group =1), color = 'black', linewidth = 0.5)+
    geom_line(aes(x= as.Date(Date), y= as.numeric(RollingAvg300), group =1), color = 'red') +
    geom_line(aes(x= as.Date(Date), y= RollingAvg50, group = 1), color = 'blue') +
    geom_line(aes(x= as.Date(Date), y= RollingAvg120, group = 1), color = 'purple') +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_discrete(breaks = seq(from = min(as.numeric(dfName$Low))-1000, to = max(as.numeric(dfName$High)) +10, by = 20)) +
    xlab("Time") + ylab("Moving Averages")
  ggplotly(gMA)
}
#plotMAs(dataNI)

plotROC <- function(dfName){
  gROC <- ggplot(dfName)+
    geom_line(aes(x= as.Date(Date), y = as.numeric(ROC40)))+
    scale_y_continuous(breaks = seq(from =0, to = 2, by = 0.2))
  ggplotly(gROC)
}

#Bollinger Bands
chartSeries(dataNI,theme=chartTheme('white',up.col='green',dn.col='red'),TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"),addRSI(n=14)))

createData("AdaniPorts", "ADANIPORTS.csv")
AddAvgs(AdaniPorts)
AddMAs(AdaniPorts)
AddROC(AdaniPorts)
createDF(AdaniPorts, "dataAP")
createIndex(dataAP)

createData("CoalIndia", "COALINDIA.csv")
AddAvgs(CoalIndia)
AddMAs(CoalIndia)
AddROC(CoalIndia)
createDF(CoalIndia, "dataCI")
createIndex(dataCI)


createData("BPCL", "BPCL.csv")
AddAvgs(BPCL)
AddMAs(BPCL)
AddROC(BPCL)
createDF(BPCL, "dataBPCL")
createIndex(dataBPCL)
dataBPCL

createData("JswSteel", "JSWSTEEL.csv")
AddAvgs(JswSteel)
AddMAs(JswSteel)
AddROC(JswSteel)
createDF(JswSteel, "dataJS")
createIndex(dataJS)
dataJS

createData("BajajAuto", "BAJAJ-AUTO.csv")
AddAvgs(BajajAuto)
AddMAs(BajajAuto)
AddROC(BajajAuto)
createDF(BajajAuto, "dataBA")
createIndex(dataBA)

createData("NestleInd", "NESTLEIND.csv")
AddAvgs(NestleInd)
AddMAs(NestleInd)
AddROC(NestleInd)
createDF(NestleInd, "dataNI")
createIndex(dataNI)

createData("AxisBank", "AXISBANK.csv")
AddAvgs(AxisBank)
AddMAs(AxisBank)
AddROC(AxisBank)
createDF(AxisBank, "dataAB")
createIndex(dataAB)

createData("PowerGrid", "POWERGRID.csv")
AddAvgs(PowerGrid)
AddMAs(PowerGrid)
AddROC(PowerGrid)
createDF(PowerGrid, "dataPG")
createIndex(dataPG)

createData("DrReddy", "DRREDDY.csv")
AddAvgs(DrReddy)
AddMAs(DrReddy)
AddROC(DrReddy)
createDF(DrReddy, "dataDR")
createIndex(dataDR)

createData("HeroMotorCo", "HEROMOTOCO.csv")
AddAvgs(HeroMotorCo)
AddMAs(HeroMotorCo)
AddROC(HeroMotorCo)
createDF(HeroMotorCo, "dataHM")
createIndex(dataHM)

createData("Titan", "TITAN.csv")
AddAvgs(Titan)
AddMAs(Titan)
AddROC(Titan)
createDF(Titan, "dataT")
createIndex(dataT)

createData("ZeeTV", "ZEEL.csv")
AddAvgs(ZeeTV)
AddMAs(ZeeTV)
AddROC(ZeeTV)
createDF(ZeeTV, "dataZ")
createIndex(dataZ)

createData("Reliance", "RELIANCE.csv")
AddAvgs(Reliance)
AddMAs(Reliance)
AddROC(Reliance)
createDF(Reliance, "dataR")
createIndex(dataR)

createData("Britannia", "BRITANNIA.csv")
AddAvgs(Britannia)
AddMAs(Britannia)
AddROC(Britannia)
createDF(Britannia, "dataB")
createIndex(dataB)

createData("Hindalco", "HINDALCO.csv")
AddAvgs(Hindalco)
AddMAs(Hindalco)
AddROC(Hindalco)
createDF(Hindalco, "dataH")
createIndex(dataH)

#The same can be done using xts objects:
#converting the entire dataset to xts datatype indexing by date 
#AdaniPortsXTS <- xts(AdaniPorts, order.by = AdaniPorts$Date)
#APdf <- fortify.zoo(AdaniPortsXTS)
#Splitting the table into yearly data to use when necessary
#Large list of XTS datatype objects for each year
#AdaniPortsXTS_yearly <- split(AdaniPortsXTS,f="years")
#Selecting the 2008 data
#AdaniPorts08 <- AdaniPortsXTS_yearly$`2008`
#List of XTS type objects containing data for each month in 2008
#AdaniPorts08Months <- split(AdaniPorts08,f="months")
#Converting the XTS to a dataframe
#APdf08 <- fortify.zoo(AdaniPorts08)
#First three months of data
#Index3Months = nrow(AdaniPorts08Months$`Jan 2008`) + nrow(AdaniPorts08Months$`Feb 2008`) + nrow(AdaniPorts08Months$`Mar 2008`) 
#APdf08JFM <-  APdf08[1:Index3Months, ]
 
#Explicitly the general idea of drawing trendlines is a mechanized way of what we can intuitively guess and easily eyeball 
# for example, here are hard coded trend lines with closing prices for 2008 data of Adani Ports
APdf08 <- filterByYear(dataAP, 2008)
gclose08 <- ggplot(APdf08) + 
  geom_line(aes(x= Date, y= Close, group =1), color = 'green') + 
  geom_line(aes(x= Date, y= Volume, group = 1), color = 'blue') + 
  geom_segment(x = APdf08$Date[5], xend = APdf08$Date[45], y= APdf08$Close[5], yend = APdf08$Close[45]) +
  geom_segment(x = APdf08$Date[62], xend = APdf08$Date[95], y= APdf08$Close[62], yend = APdf08$Close[95]) +
  geom_segment(x = APdf08$Date[125], xend = APdf08$Date[175], y= APdf08$Close[127], yend = APdf08$Close[174]) +
  geom_segment(x = APdf08$Date[42], xend = APdf08$Date[158], y= APdf08$Volume[42], yend = APdf08$Volume[155]) +
  geom_segment(x = APdf08$Date[162], xend = APdf08$Date[243], y= APdf08$Volume[162], yend = APdf08$Volume[243]) 

gclose08


d1<- filterYearInterval(dataCI, 2021, 2023)
d2 <- filterYearInterval(dataJS, 2021, 2023)
d3 <- filterYearInterval(dataBPCL, 2021, 2023)
Lcompare3Co(d1, d2, d3, Volume, "1 month")
compare3Co(dataCI, dataJS, dataBPCL, Volume, "1 year")
  
dataNI2019_21<- filterYearInterval(dataNI, 2019, 2021)
plotLineGraph(dataNI2019_21, Close)

plotLineGraph(dataBP, Close)

Lcompare3Co(dataCI, dataBP, dataPG, Close, "1 year")

subCI<- filterYearInterval(dataCI, 2019, 2021)
subBP<- filterYearInterval(dataBP, 2019, 2021)
subPG <- filterYearInterval(dataPG, 2019, 2021)
Compare3Co(subCI, subBP, subPG, Close, "1 year")


subDR <- filterYearInterval(dataDR, 2019, 2021)
subHM <- filterYearInterval(dataHM, 2019, 2021)
subZ <- filterYearInterval(dataZ, 2019, 2021)
Compare3Co(subDR, subHM, subZ, Close, "1 year")


Lplot2(dataAB, Avg, Volume)

plotMAs(filterYearInterval(dataR, 2005,2015))

plotWtRSI2(filterYearInterval(dataT, 2015, 2020), 14, 41)

p1<- plotWtRSI2(filterByYear(dataT, 2002), 14, 41)
p2<- plotWtRSI2(filterByYear(dataT, 2006), 14, 41)
p3<- plotWtRSI2(filterByYear(dataT, 2012), 14, 41)
p4 <- plotWtRSI2(filterByYear(dataT, 2017), 14, 41)

grid.arrange(p1,p2,p3, p4, nrow = 2)
q1 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 1)
q2 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 2)
q3 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 3)
q4 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 4)
q5 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 5)
q6 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 6)
q7 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 7)
q8 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 8)
q9 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 9)
q10 <- polyApprox(filterYearInterval(dataB, 2019, 2021), Close, 10)

grid.arrange(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, nrow = 2)
  
plotObv(dataAP, 21)

candlestick(filterByYear(dataCI, 2015))

plotLineGraph(filterByYear(dataH, 2005), Close)
plotLineGraph(filterByYear(dataH, 2018), Close)
 