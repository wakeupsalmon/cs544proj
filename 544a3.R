#CS544 M3 Assignment
#Juntian Chen U45629545
#Sept 25, 2021
#Part 1)------------------------------------------------------
if (!is.element("UsingR", installed.packages()[,"Package"]))
  install.packages("UsingR", dep = TRUE)

library(UsingR)
  #a)
df <- read.csv("http://people.bu.edu/kalathur/datasets/myPrimes.csv")
head(df)
tail(df)
barplot(table(df$LastDigit), col = "cyan", ylim = c(0,350),xlab="Last Digit", ylab="Frequency")
  #b)
barplot(table(df$FirstDigit), col = "red", ylim = c(0,200),xlab="First Digit", ylab="Frequency")


#Part 2)------------------------------------------------------
us_quarters <- read.csv("http://people.bu.edu/kalathur/datasets/us_quarters.csv")
head(us_quarters)
  #a)
us_quarters[which.max(us_quarters$DenverMint),]
us_quarters[which.max(us_quarters$PhillyMint),]
  #b)
(denv<-sum(us_quarters$DenverMint))
(philly<-sum(us_quarters$PhillyMint))
(tot<-(denv+philly)/4)
  #c)
DenverMint <- us_quarters$DenverMint
PhillyMint <- us_quarters$PhillyMint
d <- data.frame(DenverMint,PhillyMint)
options(scipen=999)
barplot(t(as.matrix(d)), beside=TRUE, ylim = c(0,1000000), names = us_quarters$State,
        col=c("blue","grey"), las = 2, cex.names=0.7,legend.text = TRUE)
  #d)
plot(us_quarters$DenverMint,us_quarters$PhillyMint)
  #e)
boxplot(us_quarters$DenverMint,us_quarters$PhillyMint, horizontal = TRUE)
  #f)
fivedenver <- fivenum(us_quarters$DenverMint)
denverout1 <- fivedenver[2] - 1.5*(fivedenver[4] - fivedenver[2])
denverout2 <- fivedenver[2] + 1.5*(fivedenver[4] - fivedenver[2])
us_quarters$State[which.min(abs(us_quarters$DenverMint-denverout1))]
us_quarters$State[which.min(abs(us_quarters$DenverMint-denverout2))]

fivephilly <- fivenum(us_quarters$PhillyMint)
phillyout1 <- fivephilly[2] - 1.5*(fivephilly[4] - fivephilly[2])
phillyout2 <- fivephilly[2] + 1.5*(fivephilly[4] - fivephilly[2])
us_quarters$State[which.min(abs(us_quarters$PhillyMint-phillyout1))]
us_quarters$State[which.min(abs(us_quarters$PhillyMint-phillyout2))]

#Part 3)------------------------------------------------------
stocks <- read.csv("http://people.bu.edu/kalathur/datasets/faang.csv")
stocks
  #a)
pairs(stocks[2:6])
  #b)
cor(stocks[2:6])

#Part 4)------------------------------------------------------
scores <- read.csv("http://people.bu.edu/kalathur/datasets/scores.csv")
scores
  #a)
hist(scores$Score)
ss <- hist(scores$Score)
for (i in seq(1,(length(ss$breaks) - 1))){
  cat(ss$counts[i]," students in range (",
      ss$breaks[i],",",ss$breaks[i+1],"]\n")
}
  #b)
(sss <- hist(scores$Score, breaks = c(30,50,70,90)))
cat(sss$counts[1]," students in C grade range (",
    sss$breaks[1],",",sss$breaks[2],"]\n")
cat(sss$counts[2]," students in B grade range (",
    sss$breaks[2],",",sss$breaks[3],"]\n")
cat(sss$counts[3]," students in A grade range (",
    sss$breaks[3],",",sss$breaks[4],"]\n")