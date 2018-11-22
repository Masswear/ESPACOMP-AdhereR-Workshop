library(AdhereR)
library(data.table)
library(gee)

##############
# read data ##
##############

load("./AdhereR Tutorial/data/durcomp.rda")

# these are three datasets covering prescription, dispensation and hospitalisation events in a 2-year follow-up time window
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

# how many dispensings for each patient?
table(durcomp.dispensing$ID, exclude=NULL)

# how do the dispensings look for the first patient?
durcomp.dispensing[durcomp.dispensing$ID == 1,]

####################
# prepare the data #
####################

# in the output of the str() function for the dispensing table, we see that the format of DATE.DISP is year-month-day, and ATC.CODE and UNIT are character (chr); we could change this as follows:

durcomp.prescribing[,`:=` (DATE.PRESC = as.Date(DATE.PRESC, format = "%d.%m.%Y"), #convert Date to date format dd.mm.yyyy
                          ATC.CODE = as.factor(ATC.CODE), #convert ATC-Code to factor variable
                          UNIT = as.factor(UNIT) #convert UNIT to factor variable
)]
durcomp.dispensing[,`:=` (DATE.DISP = as.Date(DATE.DISP, format = "%d.%m.%Y"), #convert Date to date format dd.mm.yyyy
                   ATC.CODE = as.factor(ATC.CODE), #convert ATC-Code to factor variable
                   UNIT = as.factor(UNIT) #convert UNIT to factor variable
)]

# summary for the prescriptions?
summary(durcomp.prescribing)

# summary for the dispensings?
summary(durcomp.dispensing)

# we only want to keep chronic medications without a prescribed end-date
presc_selection <- durcomp.prescribing[PRESC.DURATION > 30 | is.na(PRESC.DURATION)]

# Note: depending on the 'messiness' of the raw data, other data cleaning steps might be required, like excluding/replacing impossible values, processing open text fields, etc. 


# Merge dispensing and prescription data:
event_durations <- compute_event_durations(disp.data = durcomp.dispensing, 
                                           presc.data = durcomp.prescribing, 
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

# check again the summary of your variables - some start and end dates for prescriptions missing, but all good apart from that
summary(event_durations)

# visualise the med histories of these patients

cma0 <- CMA0(data=event_durations, # use the two selected patients
             ID.colname="ID", # the name of the column containing the IDs
             event.date.colname="DATE.DISP", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="DAILY.DOSE", # the name of the column containing the dosage
             medication.class.colname="ATC.CODE", # the name of the column containing the category
             followup.window.start=0,  # FUW start in days since earliest event
             followup.window.duration=3*365, # OW duration in days
             observation.window.start=0, # OW start in days since earliest event
             observation.window.duration=3*365); # OW duration in days
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphs.pdf", width = 8, height = 200)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=FALSE); # align all patients for easier comparison
dev.off()

# these patients are taking multiple medications in the same time:
table(event_durations$ATC.CODE, exclude=NULL)

# to compute adherence, we need to select which medication we compute it for
# for example, here we select treatments in the A09A class
med_events_A09 <- event_durations[grepl("^A09A",ATC.CODE),]

######################
# visualise the data #
######################

cma0 <- CMA0(data= med_events_A09, # use the two selected patients
             ID.colname="ID", # the name of the column containing the IDs
             event.date.colname="DATE.DISP", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="DAILY.DOSE", # the name of the column containing the dosage
             medication.class.colname="ATC.CODE", # the name of the column containing the category
             followup.window.start=0,  # FUW start in days since earliest event
             followup.window.duration=2*365, # OW duration in days
             observation.window.start=0, # OW start in days since earliest event
             observation.window.duration=2*365); # OW duration in days
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphsA09.pdf", width = 8, height = 20)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=FALSE); # align all patients for easier comparison
dev.off()

# we can see that all patients had the medication dispensed more than once
# if we had patients with one dispensation only, we could consider excluding them, as it could be considered as indicating non-initiation
# med_events_A09_2plus <- subset(med_events_A09,duplicated(med_events_A09$PATIENT.ID))
#the choice of excluding this type of records depends on the duration of the prescription, 

# interactive plotting
plot_interactive_cma(data=med_events_A09,
                     cma.class="simple",
                     ID.colname="ID",
                     event.date.colname="DATE.DISP",
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
             event.date.colname="DATE.DISP",
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
# this patient over 3 years from the first prescription date has a CMA7 of 32.9%, 
# but we see from the plot that adherence varied in time: the medication was dispensed 
# a while after the prescription, there is a gap before the last 5 dispensation, and another
# after the last dispensing event; there is also a variation in delays to refill during periods
# of relatively regular dispensing. This suggests that it is important to describe in more detail
# the stages of adherence (initiation, implementation and non-persistence), and to ensure that the
# period on which we compute adherence corresponds with the period on which we have data recorded
# for each patient.


##############
# Initiation #
##############

# note: add initiation calculation: first row per patient, difftime( dispensing date, prescription date )

time_init <- AdhereR::time_to_initiation(presc.data = durcomp.prescribing, 
                                disp.data = durcomp.dispensing, 
                                ID.colname = "ID", 
                                presc.start.colname = "DATE.PRESC", 
                                disp.date.colname = "DATE.DISP", 
                                medication.class.colnames = c("ATC.CODE", "FORM", "UNIT"), 
                                suppress.warnings = FALSE, 
                                return.data.table = TRUE)

###############
# Persistence #
###############

# this is the function to compute treatment episodes

TEs<- compute.treatment.episodes(med_events_A09,
                                 ID.colname="ID",
                                 event.date.colname="DATE.DISP",
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

View(TEs)

# we have now a dataset with several TEs per person, episode duration can be considered as time to discontinuation
# we can summarize the episode duration - the main variable we are interested in, for example in survival
# analysis (time to event)
summary(TEs$episode.duration)
hist(TEs$episode.duration)


##################
# Implementation #
##################

cmaE <- CMA_per_episode(CMA="CMA7", # apply the simple CMA7 to each treatment episode
                        med_events_A09,
                        ID.colname="ID",
                        event.date.colname="DATE.DISP",
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


# for patients who show a regular use of medication over the whole period (for example 7), multiple values per patient per observation window

cmaW <- CMA_sliding_window(CMA.to.apply="CMA9", 
                           med_events_A09,
                           ID.colname="ID",
                           event.date.colname="DATE.DISP",
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
                           sliding.window.step.duration=60,
                           sliding.window.step.unit="days");

plot(cmaW, 
     patients.to.plot=c("7"), 
     show.legend=FALSE);

# the output of the function includes several items
str(cmaW);
# the CMA estimates table can be called with:
View(cmaW$CMA)

# we see that this patient had variable adherence during the follow-up period
summary(cmaW$CMA$CMA[cmaW$CMA$ID==7])
hist(cmaW$CMA$CMA[cmaW$CMA$ID==7])


# we can use this variable in a similar way as for the EM data, for example in a GEE model(script adapted from the EM demo)

# plotting across time
t <- unique(cmaW$CMA$window.ID)
m <- length(t)
exe <- rep(NA,m)
for (i in 1:m) {exe[i] <- mean(cmaW$CMA$CMA[cmaW$CMA$window.ID==t[i]],na.rm=T)}
plot(t,exe,type="l",ylim=c(0,1),xlab="Time (sliding windows)",ylab="Implementation",col="pink",lwd=2)

# polynomial GEE #

M0 <- cbind(1,cmaW$CMA$window.ID,cmaW$CMA$window.ID^2,cmaW$CMA$window.ID^3,cmaW$CMA$window.ID^4,
            cmaW$CMA$window.ID^5,cmaW$CMA$window.ID^6,cmaW$CMA$window.ID^7)  # design matrix
colnames(M0) <- c("Int","t","t2","t3","t4","t5","t6","t7") 

out1 <- gee(CMA~M0-1,ID,family=quasi,data=cmaW$CMA,corstr="exchangeable")
summary(out1)
beta <- summary(out1)$coef[,1]
zstat <- summary(out1)$coef[,5]
pvalue <- 2*(1-pnorm(abs(zstat)))

cbind(beta,pvalue)

# backward selection #

repeat {
  M <- M0
  out1 <- gee(CMA~M0-1,ID,family=quasi,data=cmaW$CMA,corstr="exchangeable")
  beta <- summary(out1)$coef[,1]
  zstat <- summary(out1)$coef[,5]
  pvalue <- 2*(1-pnorm(abs(zstat)))
  if (sum(pvalue<0.05)==length(pvalue)) break; 
  M0 <- M[,-(1:length(pvalue))[pvalue==max(pvalue)]]}

cbind(beta,pvalue)



# prediction of the model #

pdata <- cbind(1,t^6,t^7)
mu.hat <- pdata%*%as.matrix(beta)
pr <- plogis(mu.hat) 
lines(t,pr,col=2,lwd=2)


# Confidence intervals around the prediction #

co <- out1$robust.variance

V0 <- co[1,1]+t^2*co[2,2]+t^4*co[3,3]+t^6*co[4,4]+t^8*co[5,5]+t^10*co[6,6]+
  2*t*co[1,2]+2*t^2*co[1,3]+2*t^3*co[1,4]+2*t^4*co[1,5]+2*t^5*co[1,6]+
  2*t^3*co[2,3]+2*t^4*co[2,4]+2*t^5*co[2,5]+2*t^6*co[2,6]+
  2*t^5*co[3,4]+2*t^6*co[3,5]+2*t^7*co[3,6]+
  2*t^7*co[4,5]+2*t^8*co[4,6]+
  2*t^9*co[5,6]


var.pred <- function(X,C) {
  
  dia <- matrix(NA,nrow(X),ncol(X))
  for (i in 1:ncol(dia)) {
    dia[,i] <- X[,i]^2*C[i,i] } 
  
  ndia <- matrix(NA,nrow(X),sum(upper.tri(C)))
  for (i in 1:(ncol(X)-1)) {for (j in ((i+1):ncol(X))) {
    ndia[,(1:ncol(ndia))[ C[upper.tri(C)]==C[i,j]]] <- 2*X[,i]*X[,j]*C[i,j]}}
  
  Var <- apply(dia,1,sum)+apply(ndia,1,sum)
  Var}

V <- var.pred(pdata,co)
se <- sqrt(V)

mu.l <- mu.hat - 2 * se
mu.u <- mu.hat + 2 * se

lcl = plogis(mu.l)
ucl = plogis(mu.u)

lines(1:m,ucl,col=2,lwd=2,lty=2)
lines(1:m,lcl,col=2,lwd=2,lty=2)



