library(AdhereR)
library(data.table)


##############
# read data ##
##############

disp_events <- fread("./AdhereR Tutorial/data/example_disp_events.csv")
load("./AdhereR Tutorial/data/example_presc_events.RData")

# these are two datasets covering dispensation and prescription events in a 2-year follow-up time window
str(disp_events)
str(presc_events)

####################
# look at the data #
####################

disp_events[disp_events$PATIENT.ID == 42,]

####################
# prepare the data #
####################

disp_events[,`:=` (DATE.DISP = as.Date(DATE.DISP, format = "%d.%m.%Y"), #convert Date to date format
                   ATC.CODE = as.factor(ATC.CODE) #convert ATC-Code to factor variable
)]

disp_events[,c("DOSE.num", "UNIT"):= tstrsplit(DOSE, " ")] #split Dose on whitespace
disp_events[,DOSE := NULL]

disp_events[,`:=` (DOSE.num = as.numeric(DOSE.num), #convert DOSE.num to numeric
                   UNIT = as.factor(UNIT) #convert UNIT to factor variable
)]

# Assign *MCG* and *MICROG* to the same factor level
levels(disp_events$UNIT) <- list(MICROG=c("MCG", "MICROG"), MG="MG", UI="UI")

# Merge dispensing and prescription data:
med_events <- merge(disp_events, presc_events, by = c("PATIENT.ID", "ATC.CODE", "UNIT"), all = TRUE, sort = FALSE)

# Calculate the supply duration 
med_events[,DURATION := (DOSE.num*QUANTITY)/DAILY.DOSE]


# check missing data - look at NAs for each variable in the resulting table
summary(med_events)
# Look at patterns of missing data



# here we decide to exclude all records with duration missing (NA)
med_events <- med_events[!is.na(med_events$DURATION),]

# check again the summary of your variables - none missing
summary(med_events)

# visualise the med histories of these patients

cma0 <- CMA0(data=as.data.frame(med_events), # use the two selected patients
             ID.colname="PATIENT.ID", # the name of the column containing the IDs
             event.date.colname="DATE.DISP", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="DAILY.DOSE", # the name of the column containing the dosage
             medication.class.colname="ATC.CODE", # the name of the column containing the category
             followup.window.start=0,  # FUW start in days since earliest event
             followup.window.duration=2*365, # OW duration in days
             observation.window.start=0, # OW start in days since earliest event
             observation.window.duration=2*365, # OW duration in days
             date.format="%m/%d/%Y"); # date format (mm/dd/yyyy)
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphs.pdf", width = 8, height = 60)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=TRUE); # align all patients for easier comparison
dev.off()

# these patients are taking multiple medications in the same time; to compute adherence, we need to select which medication we compute it for

# for example, here we select treatments in the A09A class
med_events_A09 <- med_events[grepl("^A09A",ATC.CODE),]

######################
# visualise the data #
######################

cma0 <- CMA0(data=as.data.frame(med_events_A09), # use the two selected patients
             ID.colname="PATIENT.ID", # the name of the column containing the IDs
             event.date.colname="DATE.DISP", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="DAILY.DOSE", # the name of the column containing the dosage
             medication.class.colname="ATC.CODE", # the name of the column containing the category
             followup.window.start=0,  # FUW start in days since earliest event
             followup.window.duration=2*365, # OW duration in days
             observation.window.start=0, # OW start in days since earliest event
             observation.window.duration=2*365, # OW duration in days
             date.format="%m/%d/%Y"); # date format (mm/dd/yyyy)
# Plot the object (CMA0 shows the actual event data only):
pdf("Prescription-graphs.pdf", width = 8, height = 20)
plot(cma0, # the object to plot
     show.legend= TRUE ,
     col.cats = rainbow, # not to show the legend
     align.all.patients=TRUE); # align all patients for easier comparison
dev.off()

# we notice that one patient has only had the medication dispensed once - the choice of excluding this type of records depends on the duration of the prescription, we could consider excluding patients that only had a single dispensation, as it could be considered as indicating non-initiation
med_events_A09_2plus <- subset(med_events_A09,duplicated(med_events_A09$PATIENT.ID))

# interactive plotting
plot_interactive_cma(data=as.data.frame(med_events_A09_2plus),
                     cma.class="simple",
                     ID.colname="PATIENT.ID",
                     event.date.colname="DATE.DISP",
                     event.duration.colname="DURATION",
                     event.daily.dose.colname="DAILY.DOSE",
                     medication.class.colname="ATC.CODE",
                     date.format="%m/%d/%Y");

# try out different values for CMA (simple, per episode, sliding window)
# for CMA per episode, try out different values for permissible gaps; you will see how the number of episodes per person changes depending on these values

#############
# Adherence #
#############

# Single value per person per observation window


cma7 <- CMA7(med_events_A09_2plus,
             ID.colname="PATIENT.ID",
             event.date.colname="DATE.DISP",
             event.duration.colname="DURATION",
             event.daily.dose.colname="DAILY.DOSE",
             medication.class.colname="ATC.CODE",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start="DATE.PRESC", 
             followup.window.duration=3*365,
             observation.window.start=0, 
             observation.window.duration=3*365,
             date.format="%m/%d/%Y");

plot(cma7, 
    patients.to.plot=c("45"), 
     show.legend=FALSE);
# this patient over 3 years from the first prescription date has a CMA7 of 30.2%, but we see from the plot that adherence varied in time: the medication was dispensed a while after the prescription, there is a gap before the last 5 dispensation, and another after the last dispensing event; there is also a variation in delays to refill during periods of relatively regular dispensing. This suggests that it is important to describe in more detail the stages of adherence, initiation, persistence and implementation, and to ensure that the period on which we compute adherence corresponds with the period on which we have data recorded for each patient


##############
# Initiation #
##############

# note: add initiation calculation: first row per patient, difftime( dispensing date, prescription date )




###############
# Persistence #
###############

# this is the function to compute treatment episodes

TEs<- compute.treatment.episodes(med_events_A09_2plus,
                                 ID.colname="PATIENT.ID",
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
                                  followup.window.duration.unit = "days",
                                  date.format = "%m/%d/%Y");

View(TEs)

# we have now a dataset with several TEs per person, episode duration can be considered as time to discontinuation
# we can summarize the episode duration - the main variable we are interested in, for example in survival analysis (time to event)
summary(TEs$episode.duration)
hist(TEs$episode.duration)

# Note: survival plots for TEs


##################
# Implementation #
##################


cmaE <- CMA_per_episode(CMA="CMA7", # apply the simple CMA7 to each treatment episode
                        as.data.frame(med_events_A09_2plus),
                        ID.colname="PATIENT.ID",
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
                        followup.window.duration = 365 * 3,
                        followup.window.duration.unit = "days",
                        observation.window.start=0,
                        observation.window.start.unit = "days",
                        observation.window.duration=365*3,
                        observation.window.duration.unit = "days",
                        date.format="%m/%d/%Y");
plot(cmaE, 
     patients.to.plot=c("45"), 
     show.legend=FALSE);


# for patients who show a regular use of medication over the whole period (for example 46), multiple values per patient per observation window

cmaW <- CMA_sliding_window(CMA.to.apply="CMA9", 
                           as.data.frame(med_events_A09_2plus),
                           ID.colname="PATIENT.ID",
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
                           sliding.window.step.unit="days",
                           date.format="%m/%d/%Y");

plot(cmaW, 
     patients.to.plot=c("45"), 
     show.legend=FALSE);

# the output of the function includes several items
str(cmaW);
# the CMA estimates table can be called with:
cmaW$CMA

# we see that this patient had variable adherence during the follow-up period
summary(cmaW$CMA$CMA[cmaW$CMA$PATIENT.ID==46])
hist(cmaW$CMA$CMA[cmaW$CMA$PATIENT.ID==46])

# we can use this variable as outcome for a 2-level longitudinal analysis (measurements within patients)




