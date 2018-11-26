library(AdhereR)
library(data.table)
library(survival)
library(gee)

##############
# read data ##
##############

# these are three datasets covering prescription, dispensation and hospitalisation events in a 2-year follow-up time window, available in the AdhereR package
str(durcomp.prescribing)
str(durcomp.dispensing)
str(durcomp.hospitalisation)

####################
# look at the data #
####################
# how many prescriptions for each patient?
table(durcomp.prescribing$ID, exclude=NULL)

# how do the prescriptions look for the first patient?
durcomp.prescribing[durcomp.prescribing$ID == 1,]

# summary of all variables in prescribing data
summary(durcomp.prescribing)

# how many dispensings for each patient?
table(durcomp.dispensing$ID, exclude=NULL)

# how do the dispensings look for the first patient?
durcomp.dispensing[durcomp.dispensing$ID == 1,]

# summary of all variables in dispensing data
summary(durcomp.dispensing)

# how many hospitalisations?
table(durcomp.hospitalisation$ID, exclude=NULL)

# and for the first patient?
durcomp.hospitalisation[durcomp.hospitalisation$ID == 1,]

# summary of all variables in hospitalisation data
summary(durcomp.hospitalisation)

####################
# prepare the data #
####################

# in the output of the str() function for the dispensing table, we see that the format of DATE.DISP is year-month-day, and ATC.CODE and UNIT are character (chr); we could change this as follows:

durcomp.prescribing[,`:=` (DATE.PRESC = as.Date(DATE.PRESC, format = "%d.%m.%Y"), #convert Date to date format day-month-year
                          ATC.CODE = as.factor(ATC.CODE), #convert ATC-Code to factor variable
                          UNIT = as.factor(UNIT) #convert UNIT to factor variable
)]
durcomp.dispensing[,`:=` (DATE.DISP = as.Date(DATE.DISP, format = "%d.%m.%Y"), #convert Date to date format day-month-year
                   ATC.CODE = as.factor(ATC.CODE), #convert ATC-Code to factor variable
                   UNIT = as.factor(UNIT) #convert UNIT to factor variable
)]

# we only want to keep prescriptions with a intended duration of longer than 30 days
durcomp.prescribing.select <- durcomp.prescribing[PRESC.DURATION > 30 | is.na(PRESC.DURATION)]

# Note: depending on the 'messiness' of the raw data, other data cleaning steps might be required,
# like excluding/replacing impossible values, processing open text fields, etc. 

# Merge dispensing and prescription data:
event_durations <- compute_event_durations(disp.data = durcomp.dispensing, 
                                           presc.data = durcomp.prescribing.select, 
                                           hosp.data = durcomp.hospitalisation, 
                                           ID.colname = "ID", 
                                           presc.date.colname = "DATE.PRESC", 
                                           disp.date.colname = "DATE.DISP", 
                                           date.format = "%d.%m.%Y",
                                           medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"), 
                                           total.dose.colname = "TOTAL.DOSE", 
                                           presc.daily.dose.colname = "DAILY.DOSE", 
                                           presc.duration.colname = "PRESC.DURATION", 
                                           visit.colname = "VISIT", 
                                           force.init.presc = FALSE, 
                                           force.presc.renew = TRUE, 
                                           split.on.dosage.change = TRUE, 
                                           trt.interruption = "continue", 
                                           suppress.warnings = FALSE, 
                                           return.data.table = TRUE)
# see the new dataset
str(event_durations)
# check missing data - look at NAs for each variable in the resulting table
summary(event_durations)
# if you want to look in more detail at patterns of missing data, you can use the function md.pattern in the package 'mice'
# install.packages('mice')
# library(mice)
# Miss.patterns <- md.pattern(as.matrix(event_durations)) 
# Miss.patterns

# here we decide to exclude all records with duration missing (NA)
event_durations <- na.omit(event_durations, cols = "DURATION")

# we also have to exclude events with a duration of 0
event_durations <- event_durations[DURATION != 0,]

# check again the summary of your variables - some start and end dates for prescriptions missing, but all good apart from that
summary(event_durations)

# visualise the med histories of these patients

cma0 <- CMA0(data=event_durations, # use the two selected patients
             ID.colname="ID", # the name of the column containing the IDs
             event.date.colname="DISP.START", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="DAILY.DOSE", # the name of the column containing the dosage
             medication.class.colname="ATC.CODE"); # the name of the column containing the category
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphs.pdf", width = 8, height = 200)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=FALSE); # show timelines relative to the earliest dispensation across patients 
dev.off()

# these patients are taking multiple medications in the same time:
table(event_durations$ATC.CODE, exclude=NULL)

# to compute adherence, we need to select which medication we compute it for
# for example, here we select treatments in the A09A class
med_events_A09 <- event_durations[grepl("^A09A",ATC.CODE),]

#med_events_A09 <- as.data.frame(med_events_A09)

######################
# visualise the data #
######################

cma0 <- CMA0(data= med_events_A09, # use the two selected patients
             ID.colname="ID", # the name of the column containing the IDs
             event.date.colname="DISP.START", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="DAILY.DOSE", # the name of the column containing the dosage
             medication.class.colname="ATC.CODE"); # the name of the column containing the category
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphsA09.pdf", width = 8, height = 20)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=TRUE); # show timelines relative to the earliest dispensation for each patient
dev.off()

# we can see that all patients had the medication dispensed more than once
# if we had patients with one dispensation only, we could consider excluding them, as it could be considered as indicating non-initiation
# med_events_A09_2plus <- subset(med_events_A09,duplicated(med_events_A09$PATIENT.ID))
#the choice of excluding this type of records depends on the duration of the prescription, 

# interactive plotting
plot_interactive_cma(data=med_events_A09,
                     cma.class="simple",
                     ID.colname="ID",
                     event.date.colname="DISP.START",
                     event.duration.colname="DURATION",
                     event.daily.dose.colname="DAILY.DOSE",
                     medication.class.colname="ATC.CODE");

# try out different values for CMA (simple, per episode, sliding window)
# for CMA per episode, try out different values for permissible gaps; you will see how the number of episodes per person changes depending on these values


#############
# Adherence #
#############

# Single value per person per observation window

cma7 <- CMA7(med_events_A09,
             ID.colname="ID",
             event.date.colname="DISP.START",
             event.duration.colname="DURATION",
             event.daily.dose.colname="DAILY.DOSE",
             medication.class.colname="ATC.CODE",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start="START.PRESC", 
             followup.window.duration=3*365,
             observation.window.start=0, 
             observation.window.duration=3*365);

plot(cma7, 
    patients.to.plot=c("8"), 
     show.legend=FALSE);

# this patient over 3 years from the first prescription date has a CMA7 of 22.6%,
# but we see from the plot that adherence varied in time: the medication was dispensed a while after the prescription,
# there is a gap before the last 5 dispensation, and another after the last dispensing event;
# there is also a variation in delays to refill during periods of relatively regular dispensing.
# This suggests that it is important to describe in more detail the stages of adherence 
# (initiation, implementation and non-persistence), and to ensure that the period on which we compute
# adherence corresponds with the period on which we have data recorded for each patient


##############
# Initiation #
##############

# the function time_to_initiation calculates the time between the prescription date and the dispensation date,
# for prescriptions that are dispensed.
# Note; for non-initiation, prescriptions which are not dispensed appear as is.na(DATE.DISP) in the output of the
# compute_event_duration function.

time_init <- time_to_initiation(presc.data = durcomp.prescribing[grepl("^A09A",ATC.CODE),], 
                                disp.data = event_durations[!is.na(event_durations$DURATION) & grepl("^A09A",ATC.CODE),],
                                ID.colname = "ID", 
                                presc.start.colname = "DATE.PRESC", 
                                disp.date.colname = "DATE.DISP", 
                                medication.class.colnames = c("ATC.CODE", "FORM", "UNIT"))

# we have now a dataset with a time to initiation per person
str(time_init)
summary(time_init$time.to.initialization)
hist(time_init$time.to.initialization)

###############
# Persistence #
###############

# this is the function to compute treatment episodes

TEs<- compute.treatment.episodes(med_events_A09,
                                 ID.colname="ID",
                                 event.date.colname="DISP.START",
                                 event.duration.colname="DURATION",
                                 event.daily.dose.colname="DAILY.DOSE",
                                 medication.class.colname="ATC.CODE",
                                 carryover.within.obs.window = TRUE, # carry-over into the OW
                                 carry.only.for.same.medication = TRUE, # not applicable to the example
                                 consider.dosage.change = TRUE, # not applicable to the example
                                 medication.change.means.new.treatment.episode = TRUE, # not applicable to the example
                                 maximum.permissible.gap = 90, # & a gap longer than 90 days
                                 maximum.permissible.gap.unit = "days", # unit for the above (days)
                                 followup.window.start = 0, # 2-years FUW starts at earliest event
                                 followup.window.start.unit = "days",
                                 followup.window.duration = 365 * 2,
                                 followup.window.duration.unit = "days");
# see the first lines of the resulting dataset
head(TEs)

# we have now a dataset with several TEs per person, episode duration can be considered as time to discontinuation
# we can summarize the episode duration - the main variable we are interested in, for example in survival analysis
# (time to event)
summary(TEs$episode.duration)
hist(TEs$episode.duration)


##################
# Implementation #
##################

cmaE <- CMA_per_episode(CMA="CMA7", # apply the simple CMA7 to each treatment episode
                        as.data.frame(med_events_A09),
                        ID.colname="ID",
                        event.date.colname="DISP.START",
                        event.duration.colname="DURATION",
                        event.daily.dose.colname="DAILY.DOSE",
                        medication.class.colname="ATC.CODE",
                        carryover.within.obs.window = TRUE,
                        carry.only.for.same.medication = FALSE,
                        consider.dosage.change = FALSE, # conditions on treatment episodes
                        medication.change.means.new.treatment.episode = TRUE,
                        maximum.permissible.gap = 90,
                        maximum.permissible.gap.unit = "days",
                        followup.window.start=0,
                        followup.window.start.unit = "days",
                        followup.window.duration = 365*3,
                        followup.window.duration.unit = "days",
                        observation.window.start=0,
                        observation.window.start.unit = "days",
                        observation.window.duration=365*3,
                        observation.window.duration.unit = "days");

plot(cmaE, 
     patients.to.plot=c("8"), 
     show.legend=FALSE);
# the output of the function includes several items
str(cmaE);
# the CMA estimates table can be called with:
head(cmaE$CMA)
# Note: you will see that 3 treatment episodes consist of a single dispensation: you might decide to exclude these,
# if you assume that medication initiation requires a second dispensation to attest engagement with the treatment
# prescribed


# assuming that all patients were engaged in regular use of medication over a period of 2 years and we have complete data
# from all sources, we can compute multiple values per patient per observation window

cmaW <- CMA_sliding_window(CMA.to.apply="CMA9", 
                           as.data.frame(med_events_A09),
                           ID.colname="ID",
                           event.date.colname="DISP.START",
                           event.duration.colname="DURATION",
                           event.daily.dose.colname="DAILY.DOSE",
                           medication.class.colname="ATC.CODE",
                           carry.only.for.same.medication=FALSE,
                           consider.dosage.change=FALSE,
                           followup.window.start=0, 
                           followup.window.duration=2*365,
                           observation.window.start=0,
                           observation.window.duration=2*365,
                           sliding.window.start=0, # sliding windows definition
                           sliding.window.start.unit="days",
                           sliding.window.duration=60,
                           sliding.window.duration.unit="days",
                           sliding.window.step.duration=30,
                           sliding.window.step.unit="days");

plot(cmaW, 
     patients.to.plot=c("7"), 
     show.legend=FALSE);

# the output of the function includes several items
str(cmaW);
# the CMA estimates table can be called with:
head(cmaW$CMA)

# we see that this patient had variable adherence during the follow-up period
summary(cmaW$CMA$CMA[cmaW$CMA$ID==7])
hist(cmaW$CMA$CMA[cmaW$CMA$ID==7])


##################
#  What's next?  #
##################

# we can use these variables in ways similar to the EM data

# for treatment episode length: survival analysis up to end of first episode
TEs1 <- TEs[TEs$episode.ID==1,]
# compute variable censoring - if length of episode < observation window attribute value 2, rest is 1
TEs1$censored <- 1
TEs1$censored[TEs1$episode.duration < 2*365] <- 2
table(TEs1$censored)

# plot the survival curve
plot(survfit(Surv(TEs1$episode.duration,TEs1$censored)~1),col=4,lwd=3,conf.int=F,xlim=c(0,750),ylim=c(0,1),xlab = "day",ylab="",mark.time=T)
# you could continue to test with log-rank test - replace x with your variable
# survdiff(Surv(TEs1$episode.duration,TEs1$censored)~x)
# ... and adjust for covariates with Cox's proportional hasards regression - replace x and y with your main predictor & covariate(s)
# coxph(Surv(time,status)~ x + y)


# for implementation: a GEE model
# Note: the script below is adapted from the EM demo for illustration purposes
# Note2: CMA is a continuous variable with a non-normal distribution; we dichotomise it here at a 80% cutoff to obtain a binary variable as for the EM data; this is however not an endorsement of the 80% cutoff - we recommend exploring your data and choosing appropriate distribution families or cut-offs

hist(cmaW$CMA$CMA)
cmaW$CMA$CMA_80 <- 1
cmaW$CMA$CMA_80[cmaW$CMA$CMA < 0.8] <- 0
barplot(table(cmaW$CMA$CMA_80))

# plotting across time for both average % implementation per period and % of patients with implementation >=80%
t <- unique(cmaW$CMA$window.ID)
m <- length(t)
exe <- rep(NA,m)
exe80 <- rep(NA,m)
for (i in 1:m) {exe[i] <- mean(cmaW$CMA$CMA[cmaW$CMA$window.ID==t[i]],na.rm=T)}
for (i in 1:m) {exe80[i] <- mean(cmaW$CMA$CMA_80[cmaW$CMA$window.ID==t[i]],na.rm=T)}

# ploting both lines - binary variable gives lower estimates
plot(t,exe,type="l",ylim=c(0,1),xlab="Time (sliding windows)",ylab="Implementation",col="pink",lwd=2)
par(new=TRUE)
plot(t,exe80,type="l",ylim=c(0,1),xlab="Time (sliding windows)",ylab="Implementation",col="blue",lwd=2)

# polynomial GEE #

M0 <- cbind(1,cmaW$CMA$window.ID,cmaW$CMA$window.ID^2,cmaW$CMA$window.ID^3,cmaW$CMA$window.ID^4,
            cmaW$CMA$window.ID^5,cmaW$CMA$window.ID^6,cmaW$CMA$window.ID^7)  # design matrix
colnames(M0) <- c("Int","t","t2","t3","t4","t5","t6","t7") 

out1 <- gee(CMA_80~M0-1,ID,family=binomial,data=cmaW$CMA,corstr="exchangeable")
summary(out1)
beta <- summary(out1)$coef[,1]
zstat <- summary(out1)$coef[,5]
pvalue <- 2*(1-pnorm(abs(zstat)))

cbind(beta,pvalue)

# backward selection #

repeat {
  M <- M0
  out1 <- gee(CMA_80~M0-1,ID,family=binomial,data=cmaW$CMA,corstr="exchangeable")
  beta <- summary(out1)$coef[,1]
  zstat <- summary(out1)$coef[,5]
  pvalue <- 2*(1-pnorm(abs(zstat)))
  if (sum(pvalue<0.05)==length(pvalue)) break; 
  M0 <- M[,-(1:length(pvalue))[pvalue==max(pvalue)]]
  }

cbind(beta,pvalue)


# prediction of the model #

pdata <- cbind(t^5,t^6,t^7)
mu.hat <- pdata%*%as.matrix(beta)
pr <- plogis(mu.hat) 
lines(t,pr,col=2,lwd=2)


# Confidence intervals around the prediction #

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

CI <- CI.pred(out1,pdata)
CI

lines(t,CI$lower,col=2,lwd=2,lty=2)
lines(t,CI$upper,col=2,lwd=2,lty=2)

##################
#  Polypharmacy  #
##################

# To assess adherence to multiple medications, we have to create treatment groups.
# We create treatment groups based on level 4 of the ATC classification (chemical subgroup)
event_durations[,group := substr(ATC.CODE,1,5)]

table(event_durations$group)

# Different treatments might be prescribed for various prescription episodes. 
# We want to calculate CMAs for each prescription episode separately, so we
# need to add a duration for each prescription episode (with a maximum of 1 year). 

event_durations <- event_durations[!is.na(START.PRESC)] #select only events during prescription periods
event_durations[,PRESC.DURATION := pmin(as.numeric(END.PRESC - START.PRESC), 365, na.rm=TRUE)]

# Now we can calculate CMAs by treatment group and prescription episode,
# using our preferred CMA function with some data.table syntax

CMA7_by_group <- event_durations[,getCMA(CMA7(data = as.data.frame(c(.BY, .SD)),
                                              ID.colname = "ID",
                                              event.date.colname = "DISP.START",
                                              event.duration.colname = "DURATION",
                                              medication.class.colname = "ATC.CODE",
                                              observation.window.start = "START.PRESC",
                                              observation.window.duration = "PRESC.DURATION",
                                              observation.window.duration.unit = "days",
                                              followup.window.start = as.Date("2056-01-01"),
                                              followup.window.duration = 3*365,
                                              followup.window.duration.unit = "days",
                                              carry.only.for.same.medication = TRUE,
                                              consider.dosage.change = FALSE,
                                              force.NA.CMA.for.failed.patients = TRUE)),
                                     by = .(group, START.PRESC, END.PRESC, PRESC.DURATION)]

# Calculate mean adherence per group
mean_CMA7_per_group <- CMA7_by_group[,mean(CMA, na.rm=TRUE), by = .(ID, group)]

# Calculate mean adherence per patient

mean_CMA7_per_patient <- mean_CMA7_per_group[,mean(V1, na.rm = TRUE), by = .(ID)]

summary(mean_CMA7_per_patient$V1)
hist(mean_CMA7_per_patient$V1)

# We can see that for one patient, no CMA was calculated. This is because the first dispensing was more
# than one year after the first prescription events and consequently outside the observation period.



