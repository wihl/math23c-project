knitr::opts_chunk$set(echo = TRUE)
library(xts)
library(quantmod)
library(ggplot2)
library(knitr)
op <- par(no.readonly = TRUE)
# Read one price history file per currency
BTCdf = read.csv("data/bitcoin_price.csv", stringsAsFactors = F)
ETHdf = read.csv("data/ethereum_price.csv", stringsAsFactors = F)
XMRdf = read.csv("data/monero_price.csv", stringsAsFactors = F)
XRPdf = read.csv("data/ripple_price.csv", stringsAsFactors = F)
# Fix rest of data:
# 1- Make dates native format
# 2- Convert Volume and market cap:
#     a) From string ("123,456") to numeric (123456). 
#     b) Convert "-" to 0.
# 3- Sort chronologically
fixVolCap = function(df) {
  df$Date = as.Date(df$Date,"%b %d, %Y")
  df$Volume = as.numeric(gsub("-","0",gsub(",","",df$Volume)))
  df$Market.Cap = as.numeric(gsub("-","0",gsub(",","",df$Market.Cap)))
  return (df[order(df$Date),])
}
BTCdf = fixVolCap(BTCdf)
ETHdf = fixVolCap(ETHdf)
XMRdf = fixVolCap(XMRdf)
XRPdf = fixVolCap(XRPdf)

# Ensure that all data start from the same date
earliestCommonDate =  max(min(BTCdf$Date),
                          min(ETHdf$Date),
                          min(XMRdf$Date),
                          min(XRPdf$Date))
BTCdf = BTCdf[BTCdf$Date>=earliestCommonDate,]
ETHdf = ETHdf[ETHdf$Date>=earliestCommonDate,]
XMRdf = XMRdf[XMRdf$Date>=earliestCommonDate,]
XRPdf = XRPdf[XRPdf$Date>=earliestCommonDate,]


# Read in traditional, noncryptocurrency data
noncrypto = read.csv("data/noncrypto.csv",stringsAsFactors = FALSE)
noncrypto$DATE = as.Date(noncrypto$DATE,"%Y-%m-%d")
# Plot price and volume data over time
plotSeries = function(df,label){
  dfdata = xts(df[,2:7],order.by = df[,1])
  OHLC = as.quantmod.OHLC(dfdata)
  chartSeries(OHLC,name=label,layout=NULL, TA=NULL)
}
layout(matrix(1:4,nrow=4))
plotSeries(BTCdf, "Bitcoin")
plotSeries(ETHdf, "Ethereum")
plotSeries(XMRdf, "Monero")
plotSeries(XRPdf, "Ripple")
dev.off()
# Weekly Comparison - not used
# wk = BTCdata
# data.wk = to.weekly(wk)
# plot(data.wk)
# OHLC = as.quantmod.OHLC(BTCdata)
# chartSeries(OHLC)


BTCdata = xts(BTCdf[,2:7],order.by = BTCdf[,1])

close.prices = BTCdata$Close
close.prices = cbind(close.prices,ETHdf$Close,XMRdf$Close,XRPdf$Close)

multidf = cbind(index(close.prices), data.frame(close.prices))
names(multidf) = paste(c("Date","BTC","ETH","XMR","XRP"))
# Merge in the non-crypto metrics
multidf=merge(multidf,noncrypto,by.x="Date", by.y="DATE")

# Create a new column with the relative price changes (scaled to the starting price)
multidf$BTC.idx = multidf$BTC / multidf$BTC[1]
multidf$ETH.idx = multidf$ETH / multidf$ETH[1]
multidf$XMR.idx = multidf$XMR / multidf$XMR[1]
multidf$XRP.idx = multidf$XRP / multidf$XRP[1]
multidf$SP500.idx = multidf$SP500 / multidf$SP500[1]
multidf$GOLDAMGBD228NLBM.idx = multidf$GOLDAMGBD228NLBM / multidf$GOLDAMGBD228NLBM[1]
# default y scale 
par(op)
plot(x = multidf$Date,y=multidf$BTC.idx,type="l",xlab="Date",col="black",lty=1,lwd=2,
     main="Comparison of Four Major Cryptocurrencies vs. S&P 500\n (scaled by BTC)")
lines(x=multidf$Date,y=multidf$ETH,col="red")
lines(x=multidf$Date,y=multidf$XMR,col="blue")
lines(x=multidf$Date,y=multidf$XRP,col="green")
lines(x=multidf$Date,y=multidf$SP500.idx,col="purple",lwd=4)
legend("topleft",c("BTC","ETH","XMR","XRP","SP500"),col=c("black","red","blue","green","purple"),
       lty=c(1,1,1,1,1),
       lwd=c(2,2,2,2,2))
# y scale from 0-500
plot(x = multidf$Date,y=multidf$BTC.idx,type="l",xlab="Date",col="black",lty=1,lwd=2,ylim=c(0,500),
     main="Comparison of Four Major Cryptocurrencies vs. S&P 500\n (scaled by ETH)")
lines(x=multidf$Date,y=multidf$ETH,col="red")
lines(x=multidf$Date,y=multidf$XMR,col="blue")
lines(x=multidf$Date,y=multidf$XRP,col="green")
lines(x=multidf$Date,y=multidf$SP500.idx,col="purple",lwd=4)
legend("topleft",c("BTC","ETH","XMR","XRP","SP500"),col=c("black","red","blue","green","purple"),
       lty=c(1,1,1,1,1),
       lwd=c(2,2,2,2,2))
# Display bar plots showing Overall Return and Change in Daily Volume 
overallReturn = function(df){
  return ((df$Close[nrow(df)] - df$Close[1]) / df$Close[1])
}

volIncrease = function(df){
  return ((df$Volume[nrow(df)] - df$Volume[1]) / df$Volume[1])
}

returns = c(overallReturn(BTCdf),overallReturn(ETHdf),overallReturn(XMRdf),overallReturn(XRPdf))
volumes = c(volIncrease(BTCdf),volIncrease(ETHdf),volIncrease(XMRdf),volIncrease(XRPdf))

barData = data.frame(Currency=c("BTC","ETH","XMR","XRP"), Returns=returns,Volumes=volumes)
kable(barData, caption="Cryptocurrency ROI",
      row.names = F,digits=2)
# REQ: barplot
ggplot(data=barData, aes(x=Currency, y=Returns)) +
  geom_bar(stat="identity",fill="steelblue") + 
  ggtitle("Cumulative Return on Investment (not percent!) Aug 2015 - Feb 2018")

ggplot(data=barData, aes(x=Currency, y=Volumes)) +
  geom_bar(stat="identity",fill="chocolate4") +
  ggtitle("Change in Daily Volume Aug 2015 - Feb 2018")

# distribution of relative price changes
multidf$BTCdelta = c(0,diff(multidf$BTC.idx))
multidf$ETHdelta = c(0,diff(multidf$ETH.idx))
multidf$XMRdelta = c(0,diff(multidf$XMR.idx))
multidf$XRPdelta = c(0,diff(multidf$XRP.idx))
multidf$SP500delta = c(0,diff(multidf$SP500.idx))

# REQ: display a histogram
par(op)
mu_SP500delta = mean(multidf$SP500delta)
sd_SP500delta = sd(multidf$SP500delta)
multidf$SP500deltaStd = (multidf$SP500delta - mu_SP500delta) / sd_SP500delta
# Plot histogram of standardized daily price changes in S&P500, overlaid with equivalent
# Gaussian distribution.
ggplot(multidf, aes(x=SP500deltaStd)) +
  geom_histogram(aes(y=..density..),
                 binwidth=0.01,colour="purple",
                 fill="white") +
  #geom_density(alpha=0.2,fill="#FF6666") +
  stat_function(
    fun = function(x,mean,sd,n){
      n*dnorm(x=x, mean=mean, sd=sd)
    },
    args=with(multidf, c(mean=mu_SP500delta, sd=sd_SP500delta, n=length(multidf)))
  )

# QQplot to see if a distribution is normal
qqnorm(multidf$SP500deltaStd,main="QQ Plot of S&P 500 Daily Price Changes")
abline(0,1)
shapiro.test(multidf$SP500deltaStd)
# From http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

p1 = ggplot(multidf, aes(x=BTCdelta)) +
  geom_histogram(aes(y=..density..),
                 binwidth=0.1,colour="black",
                 fill="white") +
  geom_density(alpha=0.2,fill="#FF6666") +
  stat_function(
    fun = function(x,mean,sd,n){
      n*dnorm(x=x, mean=mean, sd=sd)
    },
    args=with(multidf, c(mean=mean(BTCdelta), sd=sd(BTCdelta), n=length(multidf)))
  )

p2 = ggplot(multidf, aes(x=ETHdelta)) +
  geom_histogram(aes(y=..density..),
                 binwidth=0.1,colour="black",
                 fill="white") +
  geom_density(alpha=0.2,fill="#FF6666")

p3 = ggplot(multidf, aes(x=XMRdelta)) +
  geom_histogram(aes(y=..density..),
                 binwidth=0.1,colour="black",
                 fill="white") +
  geom_density(alpha=0.2,fill="#FF6666")

p4 = ggplot(multidf, aes(x=XRPdelta)) +
  geom_histogram(aes(y=..density..),
                 binwidth=0.1,colour="black",
                 fill="white") +
  geom_density(alpha=0.2,fill="#FF6666")

multiplot(p1,p2,p3,p4,cols=2)
# TODO: overlay S&P500 over any cryptocurrency

# TODO: normalize values and overlay Gaussian

# Categorical Variables

# Converting VIX into a categorical
#TODO

multidf$VIXCLS.idx = multidf$VIXCLS / multidf$VIXCLS[1]
multidf$VIXCLSdelta = c(0,diff(multidf$VIXCLS.idx))
multidf$VIXCLSsgn = ifelse(multidf$VIXCLSdelta>=0,1,-1)
multidf$BTCsgn = ifelse(multidf$BTCdelta>=0,1,-1)
# TODO how many standard deviations are exceeded

overlayGaussian = function(v,label){
  mu_v = mean(v)
  sd_v = sd(v)
  v_std = (v - mu_v) / sd_v
  hist(v_std,main=paste("Distribution of Standardized",label))
  xfit = seq(min(v_std), max(v_std), length=length(v_std))
  yfit = dnorm(xfit, mean=mu_v, sd = sd_v)
  lines(xfit,yfit,col="red", lwd=2)
  qqnorm(v_std,main=paste("QQ Plot of",label))
  abline(0,1)

}
overlayGaussian(multidf$BTCdelta,"BTC Daily Price Changes")
overlayGaussian(multidf$SP500delta,"S&P 500 Daily Price Changes")

# Overlay Beta
overlayBeta = function(v,label){
  # this will rescale vector v to [0,1]
  v_norm = (v - min(v)) / (max(v) - min(v))
  hist(v_norm,main=label)
  xfit = seq(0, 1, length=length(v))
  yfit = dbeta(xfit, 1000,600)
  lines(xfit,yfit,col="red", lwd=2)
}
#overlayBeta(multidf$BTCdelta,"Distribution of Normalized BTC Daily Price Changes")
overlayBeta(multidf$SP500delta,"Distribution of Normalized S&P 500 Daily Price Changes")



cor(BTCdf$Close, ETHdf$Close)
cor(BTCdf$Close, XMRdf$Close)
cor(BTCdf$Close, XRPdf$Close)
cor(ETHdf$Close, XMRdf$Close)
cor(ETHdf$Close, XRPdf$Close)
cor(XMRdf$Close, XRPdf$Close)

#Largest correlation between BTC and XMR
plot(BTCdf$Close,XMRdf$Close,pch = ".",cex = 3)
#b is slope
b <- cov(BTCdf$Close,XMRdf$Close)/var(BTCdf$Close)
#a is intercept
a <- mean(XMRdf$Close) - b*mean(BTCdf$Close);a    
#We can add this regression line to the plot of the data
abline(a, b, col = "red")
## 
