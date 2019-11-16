#####################################################
#                                                   #
#      ESPACOMP 2019 Preconference Workshop 2       #
#              Adherence data analysis              #
#                                                   #
#        Part 3: Electronic Healthcare Data         #
#                                                   #
#       S Allemann, Nov 2019; Example script        #
#           Contact: s.allemann@unibas.ch           #
#####################################################

##################
# load packages ##
##################

library(AdhereR) # Version 0.5 or above
library(data.table)
library(survival)
library(gee)
library(ggplot2)

##############
# read data ##
##############

# we start with the ready-to-use med.events dataset from the AdhereR package. 
# Let's take a look at the structure of this data:
str(med.events)

####################
# look at the data #
####################

# this is a data.frame, so we use the data.frame notation

# how many events for each patient?
table(med.events$PATIENT_ID, exclude=NULL)

# how do the events look for the first patient?
med.events[med.events$PATIENT_ID == 1,]

# summary of all variables in med.events
summary(med.events)

# visualise the med histories of these patients

cma0 <- CMA0(data=med.events, 
             ID.colname="PATIENT_ID", # the name of the column containing the IDs
             event.date.colname="DATE", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="PERDAY", #  the name of the column containing the prescribed daily dose
             medication.class.colname="CATEGORY", # the name of the column containing the category
             date.format = "%m/%d/%Y"); # the date format

# Plot the object (CMA0 shows the actual event data only):
plot(cma0, # the object to plot
     patients.to.plot = c(76,5), # select two patients to plot
     show.legend= TRUE , # show the legend
     col.cats = rainbow, # use rainbow colours
     align.all.patients=TRUE); # show timelines relative to the earliest dispensation across patients 

# We can also use interactive plotting to examine individual or multiple patients with different options
plot_interactive_cma(ID.colname="PATIENT_ID", # the name of the column containing the IDs
                     event.date.colname="DATE", # the name of the column containing the event date
                     event.duration.colname="DURATION", # the name of the column containing the duration
                     event.daily.dose.colname="PERDAY", #  the name of the column containing the prescribed daily dose
                     medication.class.colname="CATEGORY", # the name of the column containing the category
                     date.format = "%m/%d/%Y");

#############
# Adherence #
#############

# Compute single value per person for a fixed observation window of 2 years after the first event

cma7 <- CMA7(med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, 
             followup.window.duration=2*365,
             observation.window.start=0, 
             observation.window.duration=2*365);

# plot adherence for patient with ID 5
plot(cma7, 
     patients.to.plot=c("5"), 
     show.legend=FALSE);

# this patient over 2 years from the first event date has a CMA7 of 37%,
# but we see from the plot that adherence varied in time: there are 4 dispensing events in the first 5 months,
# then there is a gap, another dispensing event followed by a shorter gap and two dispensing events 
# almost at the same time; then there is a single event of a different medication and 
# no dispensing events in the last 6 or so months of the observation period.
# This suggests that it is important to differentiate better the adherence phases 
# (implementation and non-persistence), and to ensure that the period on which we compute
# adherence corresponds with the period on which we have data recorded for each patient

###############
# Persistence #
###############

# this is the function to compute treatment episodes

TEs<- compute.treatment.episodes(med.events,
                                 ID.colname="PATIENT_ID",
                                 event.date.colname="DATE",
                                 event.duration.colname="DURATION",
                                 event.daily.dose.colname="PERDAY",
                                 medication.class.colname="CATEGORY",
                                 carryover.within.obs.window = TRUE, # carry-over into the OW
                                 carry.only.for.same.medication = TRUE, # not applicable to the example
                                 consider.dosage.change = TRUE, # not applicable to the example
                                 medication.change.means.new.treatment.episode = TRUE, # not applicable to the example
                                 maximum.permissible.gap = 180, # & a gap longer than 180 days
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

# this is the function to compute CMA per treatment episode

cmaE <- CMA_per_episode(CMA="CMA7", # apply the simple CMA7 to each treatment episode
                        med.events,
                        ID.colname="PATIENT_ID",
                        event.date.colname="DATE",
                        event.duration.colname="DURATION",
                        event.daily.dose.colname="PERDAY",
                        medication.class.colname="CATEGORY",
                        carry.only.for.same.medication = FALSE,
                        consider.dosage.change = FALSE, # conditions on treatment episodes
                        medication.change.means.new.treatment.episode = TRUE,
                        maximum.permissible.gap = 180,
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
     patients.to.plot=c("5"), 
     show.legend=FALSE);
# the output of the function includes several items
str(cmaE);
# the CMA estimates table can be called with:
head(cmaE$CMA)

# assuming that all patients were engaged in regular use of medication over a period of 2 years and we have
# complete data from all sources, we can compute multiple values per patient per observation window

cmaW <- CMA_sliding_window(CMA.to.apply="CMA9", # use CMA9 to estimate adherence between dispensing events
                           as.data.frame(med.events),
                           ID.colname="PATIENT_ID",
                           event.date.colname="DATE",
                           event.duration.colname="DURATION",
                           event.daily.dose.colname="PERDAY",
                           medication.class.colname="CATEGORY",
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
     patients.to.plot=c("5"), 
     show.legend=FALSE);

# the output of the function includes several items
str(cmaW);
# the CMA estimates table can be called with:
head(cmaW$CMA)

# we see that this patient had variable adherence during the follow-up period
summary(cmaW$CMA$CMA[cmaW$CMA$PATIENT_ID==5])
hist(cmaW$CMA$CMA[cmaW$CMA$PATIENT_ID==5])

#######################
# Trajectory modeling #
#######################

# create a wide format table with the CMA trajectory for each patient per line
cmaW_wide <- dcast(cmaW$CMA, PATIENT_ID ~ window.ID, value.var = "CMA")

# create object for cluster analysis with kml
conv_clust <- clusterLongData(idAll = cmaW_wide$PATIENT_ID, # the ids
                              time = 1:23, # the number of measurements (windows)
                              traj = cmaW_wide[,2:24] # the columns in the data.frame with CMA values
                              ) 

# run cluster analysis with kml
kml(conv_clust)

# plot quality criteria to choose number of clusters
plotAllCriterion(conv_clust)

# the criteria are not concordant: some suggest just 2, some suggest 6 clusters

# for interactive choice of model
x11(type = "Xlib") # open graphic device, required in windows, not necessary in linux
choice(conv_clust)

# we select 6 clusters and assign the clusters to the patient id's
cmaW_wide$GROUP <- getClusters(conv_clust, 6)

# look at the number of patients per group
summary(cmaW_wide$GROUP)

# create a long format table again with the assigned group
cmaW_long <- melt(cmaW_wide,
                  id.vars = c("PATIENT_ID", "GROUP"),
                  variable.name = "window.ID",
                  value.name = "CMA")

# convert window ID's back to numeric
cmaW_long$window.ID <- as.numeric(cmaW_long$window.ID)

# plot trajectories in facets per group
ggplot(data = cmaW_long, aes(x = window.ID, y = CMA)) + 
       geom_line(aes(group = PATIENT_ID, color = GROUP), alpha = 0.3) +
       facet_wrap(facets = "GROUP") +
       geom_smooth(color = "black", se = TRUE, size=0.5) +
       ylim(0,1) + guides(color=FALSE) +
       xlim(0, 15) +
       xlab("window ID")

###############################################################################################

# now let's move on to a more complex example with multiple sources that need to be linked

# these are three datasets covering prescription, dispensation and hospitalisation events 
# in a 2-year follow-up time window, available in the AdhereR package:
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
### Compute event durations
event_durations_list <- compute_event_durations(# the data
                                                disp.data = durcomp.dispensing,
                                                presc.data = durcomp.prescribing,
                                                special.periods.data = durcomp.hospitalisation,
                                                # common column names for all datasets
                                                ID.colname = "ID", 
                                                medication.class.colnames = c("ATC.CODE","UNIT", "FORM"),
                                                # columns in dispensing data
                                                disp.date.colname = "DATE.DISP", 
                                                total.dose.colname = "TOTAL.DOSE",
                                                # columns in prescribing data 
                                                presc.date.colname = "DATE.PRESC",
                                                presc.daily.dose.colname = "DAILY.DOSE",
                                                presc.duration.colname = "PRESC.DURATION",
                                                visit.colname = "VISIT",
                                                # settings for event durations
                                                split.on.dosage.change = TRUE,
                                                force.init.presc = FALSE,
                                                force.presc.renew = TRUE,
                                                trt.interruption = "discard",
                                                special.periods.method = "carryover",
                                                # additional settings
                                                date.format = "%d.%m.%Y",
                                                suppress.warnings = FALSE,
                                                return.data.table = TRUE,
                                                progress.bar = TRUE)

### extract event durations
event_durations <- event_durations_list$event_durations

# see the new dataset
str(event_durations)
# check missing data - look at NAs for each variable in the resulting table
summary(event_durations)
# if you want to look in more detail at patterns of missing data, you can use the function md.pattern in the package 'mice'
# install.packages('mice')
# library(mice)
# Miss.patterns <- md.pattern(as.matrix(event_durations))
# Miss.patterns

# look at DURATION longer than 90 days
event_durations[DURATION > 90]

#############################################################
## Prune events and cover special periods                   #
#############################################################

### prune dataset
event_durations_pruned <- prune_event_durations(event_durations_list, # output of compute_event_durations
                                                include = c("special periods"), # only consider special periods
                                                medication.class.colnames = "ATC.CODE", 
                                                days.within.out.date.1 = 7, # flag carryover durations if there are new events within 7 days after the end of special periods
                                                days.within.out.date.2 = 30, # flag carryover durations if there are no new events within 30 days after the end of special periods
                                                keep.all = FALSE, # remove flagged events from dataset
                                                return.data.table = TRUE)

# here we decide to exclude all records with duration missing (NA)
event_durations <- na.omit(event_durations_pruned, cols = "DURATION")

# we also have to exclude events with a duration of 0
event_durations_complete <- event_durations[DURATION != 0,]

### cover special periods
event_durations_covered <- cover_special_periods(events.data = event_durations_complete, # use pruned dataset
                                                 special.periods.data = event_durations_list$special_periods,
                                                 ID.colname = "ID",
                                                 disp.start.colname = "DISP.START",
                                                 duration.colname = "DURATION",
                                                 medication.class.colnames = "ATC.CODE",
                                                 
                                                 days.before = 7, # look for durations ending 7 days before special period
                                                 days.after = 7, # look for durations startin 7 days after special period
                                                 
                                                 date.format = "%Y-%m-%d",
                                                 return.data.table = TRUE)


# check again the summary of your variables - some start and end dates for prescriptions missing, but all good apart from that
summary(event_durations)

# visualise the med histories of these patients

cma0 <- CMA0(data=event_durations_covered, # use the two selected patients
             ID.colname="ID", # the name of the column containing the IDs
             event.date.colname="DISP.START", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             #event.daily.dose.colname="DAILY.DOSE", # can't plot daily dose with covered special periods
             medication.class.colname="ATC.CODE"); # the name of the column containing the category

# Plot the object (CMA0 shows the actual event data only). You can also save directly to pdf
pdf("Prescription-graphs.pdf", width = 8, height = 200)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=FALSE); # show timelines relative to the earliest dispensation across patients 
dev.off()

# these patients are taking multiple medications in the same time:
table(event_durations_covered$ATC.CODE, exclude=NULL)

# to compute adherence, we need to select which medication we compute it for
# for example, here we select treatments in the J01 class
med_events_J01 <- event_durations_covered[grepl("^J01EE01",ATC.CODE),]

######################
# visualise the data #
######################

cma0 <- CMA0(data= med_events_J01, # use the two selected patients
             ID.colname="ID", # the name of the column containing the IDs
             event.date.colname="DISP.START", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             #event.daily.dose.colname="DAILY.DOSE", # can't plot daily dose with covered special periods
             medication.class.colname="ATC.CODE"); # the name of the column containing the category
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphsJ01.pdf", width = 8, height = 20)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=TRUE); # show timelines relative to the earliest dispensation for each patient
dev.off()

# we can see that all patients had the medication dispensed more than once
# if we had patients with one dispensation only, we could consider excluding them, as it could be considered as indicating non-initiation
# med_events_J01_2plus <- subset(med_events_J01,duplicated(med_events_J01$PATIENT.ID))
#the choice of excluding this type of records depends on the duration of the prescription, 

# interactive plotting
plot_interactive_cma(data=med_events_J01,
                     cma.class="simple",
                     ID.colname="ID",
                     event.date.colname="DISP.START",
                     event.duration.colname="DURATION",
                     #event.daily.dose.colname="DAILY.DOSE", # can't plot daily dose with covered special periods
                     medication.class.colname="ATC.CODE");

# try out different values for CMA (simple, per episode, sliding window)
# for CMA per episode, try out different values for permissible gaps; you will see how the number of episodes per person changes depending on these values


#############
# Adherence #
#############

# Single value per person per observation window

cma7 <- CMA7(med_events_J01,
             ID.colname="ID",
             event.date.colname="DISP.START",
             event.duration.colname="DURATION",
             #event.daily.dose.colname="DAILY.DOSE", # can't consider dosage change with covered special periods
             medication.class.colname="ATC.CODE",
             carry.only.for.same.medication=FALSE,
             #consider.dosage.change=FALSE, # can't consider dosage change with covered special periods
             followup.window.start=0, 
             followup.window.duration=3*365,
             observation.window.start=0, 
             observation.window.duration=365);

plot(cma7, 
    patients.to.plot=c("3"), 
     show.legend=FALSE);

# this patient over one year from the first prescription date has a CMA7 of 76.7%,
# but we see from the plot that adherence varied in time: the medication was dispensed multiple times,
# with some gaps in between; this suggests that it is important to describe in more detail the stages 
# of adherence (initiation, implementation and non-persistence), and to ensure that the period on which we compute
# adherence corresponds with the period on which we have data recorded for each patient

##############
# Initiation #
##############

# the function time_to_initiation calculates the time between the prescription date and the dispensation date,
# for prescriptions that are dispensed.
# Note; for non-initiation, prescriptions which are not dispensed appear as is.na(DATE.DISP) in the output of the
# compute_event_duration function.

time_init <- time_to_initiation(presc.data = durcomp.prescribing[grepl("^J01EE01",ATC.CODE),], # select prescriptions for ATC.CODE starting with J01EE01
                                disp.data = event_durations[!is.na(event_durations$DURATION) & grepl("^J01EE01",ATC.CODE),], # select events with non-missing durations
                                ID.colname = "ID", 
                                presc.start.colname = "DATE.PRESC", 
                                disp.date.colname = "DATE.DISP", 
                                medication.class.colnames = c("ATC.CODE", "FORM", "UNIT"))

# we have now a dataset with a time to initiation per person
str(time_init)
summary(time_init$time.to.initiation)
hist(time_init$time.to.initiation)

###############
# Persistence #
###############

# From the compute_event_durations function, we not only get the event durations of dispensing events,
# but also the prescription episodes.

### extract prescription episodes
presc_episodes <- event_durations_list$prescription_episodes

View(presc_episodes)

# similar to the output of compute.treatment.episodes, we have now a dataset with several TEs per person.
# we can also summarize the episode duration:
summary(presc_episodes$episode.duration)
hist(presc_episodes$episode.duration)

# we see that episode durations is missing in many cases. This is when prescriptions don't end 
# within the follow-up period. To compute implementation, we have to manually set an end date,
# e.g. the end of the observation window

# select only prescription episodes for medication of interest
presc_episodes_J01 <- copy(presc_episodes[grepl("^J01EE01",ATC.CODE),]) 

# impute missing end dates
presc_episodes_J01[is.na(episode.end), episode.end := as.Date("2057-12-31")]
presc_episodes_J01[,episode.duration2 := as.numeric(episode.end-episode.start)]

summary(presc_episodes_J01$episode.duration2)
hist(presc_episodes_J01$episode.duration2)

##################
# Implementation #
##################

# before we can compute implementation, we have to drop unnecessary columns in the datasets
presc_episodes_J01[,`:=` (ATC.CODE = NULL,
                          UNIT = NULL,
                          FORM = NULL,
                          DAILY.DOSE = NULL)]

med_events_J01[,`:=` (episode.start = NULL,
                      episode.end = NULL)]

cmaE <- CMA_per_episode(CMA="CMA7", # apply the simple CMA7 to each treatment episode
                        med_events_J01,
                        treat.epi = presc_episodes_J01,
                        ID.colname="ID",
                        event.date.colname="DISP.START",
                        event.duration.colname="DURATION",
                        medication.class.colname="ATC.CODE",
                        carryover.within.obs.window = TRUE,
                        carry.only.for.same.medication = FALSE,
                        medication.change.means.new.treatment.episode = TRUE,
                        followup.window.start=0,
                        followup.window.start.unit = "days",
                        followup.window.duration = 365*3,
                        followup.window.duration.unit = "days");

plot(cmaE, 
     patients.to.plot=c("3"), 
     show.legend=FALSE);
# the output of the function includes several items
str(cmaE);

# the CMA estimates table can be called with:
cmaE$CMA[cmaE$CMA$ID == 3,]

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

out1 <- gee(CMA_80~M0-1,PATIENT_ID,family=binomial,data=cmaW$CMA,corstr="exchangeable")
summary(out1)
beta <- summary(out1)$coef[,1]
zstat <- summary(out1)$coef[,5]
pvalue <- 2*(1-pnorm(abs(zstat)))

cbind(beta,pvalue)

# backward selection #

repeat {
  M <- M0
  out1 <- gee(CMA_80~M0-1,PATIENT_ID,family=binomial,data=cmaW$CMA,corstr="exchangeable")
  beta <- summary(out1)$coef[,1]
  zstat <- summary(out1)$coef[,5]
  pvalue <- 2*(1-pnorm(abs(zstat)))
  if (sum(pvalue<0.05)==length(pvalue)) break; 
  M0 <- M[,-(1:length(pvalue))[pvalue==max(pvalue)]]
  }

cbind(beta,pvalue)


# prediction of the model #

pdata <- cbind(1,t^3)
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