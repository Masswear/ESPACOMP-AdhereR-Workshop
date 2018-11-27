
##############
# read data ##
##############

RIVA <- read.csv2("......../RIVA.csv",na.string = "",header=T)  # enter the directory of the data file RIVA
RIVA$observed

####################
# look at the data #
####################

RIVA[RIVA$id == 1,]

##################
# Implementation #
##################

RIVA$z <- 1*(RIVA$observed>=RIVA$expected)
t <- unique(RIVA$drel)
m <- length(t)
imp <- rep(NA,m)
for (i in 1:m) {imp[i] <- mean(RIVA$z[RIVA$drel==t[i]],na.rm=T)}
plot(t,imp,type="l",ylim=c(0,1),xlab="Time (days)",ylab="Implementation",col="pink",lwd=2)

# (4 patients observed for 6 months: 8 22 26 31) 

# consider the first three months: drel<=90

RIVA <- RIVA[RIVA$drel<=90,]

t <- unique(RIVA$drel)
m <- length(t)
imp <- imp[1:length(t)]
plot(t,imp,type="l",ylim=c(0,1),xlab="Time (days)",ylab="Implementation",col="pink",lwd=2)


#######################################
##### persistence and adherence #######
#######################################


# persistence

RIVA[RIVA$id == 1,]

id <- unique(RIVA$id)
l <- length(id)

# definition of durations of persistence and censoring indicator #

surv <- cens <- rep(NA,l)
for (i in 1:l) {surv[i] <- nrow(RIVA[RIVA$id==id[i],])}
for (i in 1:l) {cens[i] <- 1*(RIVA$sort[RIVA$id==id[i]][1]=="Stop_treatment")}

cbind(id,surv,cens)


# Kaplan Meier #

library(survival)

mm <- survfit(Surv(surv,cens)~1)
summary(mm)

x11()
plot(mm,col=4,lwd=3,conf.int=F,xlab = "day",ylab="",mark.time=T)


# adherence (indirect estimate)

ti <- c(0,summary(mm)$time,max(t))
S <- summary(mm)$surv
su <- c(1,S,S[length(S)])

pers <- rep(NA,max(t))
for (i in 1:max(t)) { for (j in 1:(length(ti)-1)){
if (ti[j]<=t[i] & t[i]<ti[j+1]) {pers[i] <- su[j]}}}
pers[max(t)] <- S[length(S)]

adh <- imp*pers

lines(t,adh,col="pink",lwd=2)
legend("bottomleft",c("Persistence","Adherence"),col=c(4,"red"),lty=c(1,1),lwd=c(2,2),bty="n")



#######################################
## GEE model for implementation  ######
#######################################

library(gee)

# cange the active device
dev.set(2)

# polynomial GEE #

M0 <- cbind(1,RIVA$drel,RIVA$drel^2,RIVA$drel^3,RIVA$drel^4,RIVA$drel^5,RIVA$drel^6,RIVA$drel^7)  
colnames(M0) <- c("Int","t","t2","t3","t4","t5","t6","t7") 

out1 <- gee(z~M0-1,id,family=binomial,data=RIVA,corstr="exchangeable")
summary(out1)
beta <- summary(out1)$coef[,1]
zstat <- summary(out1)$coef[,5]
pvalue <- 2*(1-pnorm(abs(zstat)))

cbind(beta,pvalue)



# backward selection #

repeat {
M <- M0
out1 <- gee(z~M-1,id,family=binomial,data=RIVA,corstr="exchangeable")
beta <- summary(out1)$coef[,1]
zstat <- summary(out1)$coef[,5]
pvalue <- 2*(1-pnorm(abs(zstat)))
if (sum(pvalue<0.05)==length(pvalue)) break;         # break if all p-values are less than 0.05
M0 <- M[,-(1:length(pvalue))[pvalue==max(pvalue)]]}

cbind(beta,pvalue)



# prediction of the model #

X <- cbind(1,t,t^2,t^3,t^4,t^5)
beta <- as.matrix(beta)
mu.hat <- X%*%beta
pr <- plogis(mu.hat) 
lines(t,pr,col=2,lwd=2)



# Confidence intervals around the prediction: function CI.pred() 

CI.pred <- function(out,dat) {

C <- out$robust.variance
dia <- matrix(NA,nrow(dat),ncol(dat))
for (i in 1:ncol(dia)) {
dia[,i] <- dat[,i]^2*C[i,i] } 

ndia <- matrix(NA,nrow(dat),sum(upper.tri(C)))
for (i in 1:(ncol(dat)-1)) {for (j in ((i+1):ncol(dat))) {
ndia[,(1:ncol(ndia))[ C[upper.tri(C)]==C[i,j]]] <- 2*dat[,i]*dat[,j]*C[i,j]}}

Var <- apply(dia,1,sum)+apply(ndia,1,sum)
mu.l <- mu.hat - 2 * sqrt(Var) ; lower = plogis(mu.l)
mu.u <- mu.hat + 2 * sqrt(Var) ; upper = plogis(mu.u)

I <- as.data.frame(cbind(lower,upper))
names(I) <- c("lower","upper")
I}


CI <- CI.pred(out1,X)
CI

lines(t,CI$lower,col=2,lwd=2,lty=2)
lines(t,CI$upper,col=2,lwd=2,lty=2)


#########################
## piecewise GEE model ##
#########################

t0 <- list()  # time spent in phase 0
t1 <- list()  # time spent in phase 1
change <- rep(NA,l) # day of change

for (i in 1:l) {

n0 <- nrow(RIVA[RIVA$id == id[i] & RIVA$phase==0,])   # time totally spent in phase 0
n1 <- nrow(RIVA[RIVA$id == id[i] & RIVA$phase==1,])   # time totally spent in phase 1

t0[[i]] <- c(1:n0,rep(n0,n1))                                         # time spent in phase 0 at each instant
if (n1>0) {t1[[i]] <- c(rep(0,n0),1:n1)} else {t1[[i]] <- rep(0,n0)}  # time spent in phase 1 at each instant

if (n1>0) {change[i] <- n0+1} else {change[i] <- NA}}

RIVA$t0 <- unlist(t0)
RIVA$t1 <- unlist(t1)



# polynomial piecewise GEE model #

M0 <- cbind(1,RIVA$t0,RIVA$t0^2,RIVA$t0^3,RIVA$t0^4,RIVA$phase,
RIVA$t1,RIVA$t1^2,RIVA$t1^3,RIVA$t1^4)
colnames(M0) <- c("Int","t0","t0.2","t0.3","t0.4","ph","t1","t1.2","t1.3","t1.4") 


# backward selection #

repeat {
M <- M0
out2 <- gee(z~M-1,id,family=binomial,data=RIVA,corstr="exchangeable")
beta <- summary(out2)$coef[,1]
zstat <- summary(out2)$coef[,5]
pvalue <- 2*(1-pnorm(abs(zstat)))
if (sum(pvalue<0.05)==length(pvalue)) break; 
M0 <- M[,-(1:length(pvalue))[pvalue==max(pvalue)]]}

cbind(beta,pvalue)


# new graph of implementation #

x11()
plot(t,imp,type="l",ylim=c(0,1),xlab="Time (days)",ylab="Implementation",col="pink",lwd=2)


# prediction of the model #

T0 <- c(1:20,rep(20,70))
PH <- c(rep(0,20),rep(1,70))

X <- cbind(1,T0,PH)            # prediction matrix for a patient changing at 21 days
beta <- as.matrix(beta)
mu.hat <- X%*%beta
pr <- plogis(mu.hat) 
lines(t,pr,col=2,lwd=2)



# Confidence intervals around the prediction #

CI <- CI.pred(out2,X)
CI

lines(t,CI$lower,col=2,lwd=2,lty=2)
lines(t,CI$upper,col=2,lwd=2,lty=2)

