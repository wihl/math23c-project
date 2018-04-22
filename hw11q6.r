
#Q6 Partner: David Wihl
cur <- read.csv("data/crypto.csv"); head(cur)
length(cur[,1])
index <- which(cur$Symbol=="LTC" | cur$Symbol=="ETH" | cur$Symbol=="ZEC" | cur$Symbol=="DASH" | cur$Symbol=="XRP" | cur$Symbol=="XMR"); index
cur1 <- cur[index,]; head(cur1)

index <- which(cur$Symbol=="LTC")
ltc <- cur[index,]; head(ltc)
plot(ltc$Close, type = "o")
max(ltc$Close)
which(ltc$Close==0.06241)
ltc[909,]
ltc[910,]
#likely an error here so let's just pretend the value stayed the same
ltc[909,]$Close <- ltc[909,]$Open
plot(ltc$Close, type = "o")
#Some interesting points around index 700 and also all around 1200
interest1 <- ltc[seq(690,780),]
plot(interest1$Close, type = "o") #large spike

interest2 <- ltc[seq(1000,1400),] #steady rise then a big fall 
plot(interest2$Close, type = "o")

interest3 <- interest2[seq(290,380),]
plot(interest3$Close, type = "o") #zoom in and see the drop

