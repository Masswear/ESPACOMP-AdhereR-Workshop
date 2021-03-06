
####################################################
#                                                  #
#                  ABC of R scripting              #
#                                                  #
#          A Dima, Nov 2019; Example script        #
#      Contact: alexandra.dima@univ-lyon1.fr       #
####################################################
# Acknowledgements: Thanks to Dan Dediu and Mike Allerhand for previous R intros
# other sources: http://venus.ifca.unican.es/Rintro/ ; http://www.ats.ucla.edu/stat/r/
####################################################

# R script is commented with #'s 

# first make sure you set a folder for your R work, e.g. 
setwd("~/ACTIVE/ESPACOMP/ADAworkshop19")
# OR go to Session --> Set Working Directory --> To Source File Location
# check where you are
getwd()

# now see what R does if you write a number

1 

# R echoes back your input

# and if you want to add 2 numbers?

1+2 

# R behaves like a calculator
# for example if you want to calculate % adherence (implementation) 
# for a patient who had 90 days of medication supply over 4 months:
# days supply/ total days * 100

90/120*100

# but nothing has been saved in the environment so far
# for this to happen, you need to give a name to an object

supply <- 90 

# the 'supply' object is now in the environment, and has the value 90 
# (it is called a scalar = vector with a single value/element)

# you can now call it by its name

supply

# or 

print(supply)

# R prints the value

# now you can do things with supply

supply+30 # addition
supply-30 # substraction
supply*2 # multiplication
supply/2 # division
supply^2 # power
sqrt(supply) # square root


# if you assign a different value to supply, the previous value is forgotten

supply <- 60

# you can assign numbers as above, but also strings...

supply <- "patient's medication supply"
supply

# ... and add to the same string to form a new object

supply2 <- paste("another ", supply,sep="") # "paste" concatenates strings together
supply2


# ... and make vectors with more than 1 value/element
supply.list <- c(supply, supply2, "third patient's supply", "last patient's supply")
supply.list

# OR 

x <- c(1,2,3,4,5,6)
x

# OR

x <- 1:6
x

# you can do maths with these sorts of vectors

# define x and y as 2 vectors
x <- 1:10
y <- 11:20
# check how they look like
x
y
# and do maths with them
x+y # Adding two vectors
x*y # Multiplying two vectors
x/y # Division
x^y # Power


# you can also access(select) one or some elements in a vector
y[2]
y[3]
y[1:2]
# and reassign another value to an element
supply.list[1] <- "first patient's supply"
supply.list


# ... and also make Boolean (logical) vectors
q <- c(TRUE, FALSE, FALSE, TRUE)
# for these, other logical operations apply
# "and" - TRUE & FALSE
# "or"  - TRUE | FALSE
# "not" or "negation" - !TRUE

##-------------##
#### TO DO 1 ####

# here is an object November.days which represents a vector of all days in this month
November.days <- c(1:30)
November.days.seq <- seq(1:30) # identical to November.days
# and an object week.days which represents a vector of all days in a week
week.days <- c("M", "Tu", "W", "Th", "F", "Sa", "Su")

# access the day of the month for today (19 Nov) from the *November.days* vector
# fill in here

# access the day of the week for today (Tu) from the week.days vector
# fill in here

##-------------##

# you can create matrices and give them names
a <- matrix(NA,nrow=3,ncol=3) # define a matrix of 9 elements (3 rows and 3 colums) full with "missing data"
a
b <- matrix(1:9,nrow=3,ncol=3) # define a matrix of 9 elements (3 rows and 3 colums) but put the number 1 to 9 in it
b
c <- matrix(1:9,nrow=3,ncol=3,byrow=T) # the same as above but put these number by row
c

# Accessing matrix elements
b
b[1,3]
b[3,1]
b[1,]
b[,3]
b[,1:2] 
b[2:3,c(1,3)]
b[-1,-2]


##-------------##
#### TO DO 2 ####

# define a matrix four.weeks with 7 columns and 4 rows which includes 28 elements from 1 to 28

# fill in here

# access the second day of the 3rd week

# fill in here

##-------------##


# you can also create data frames - the type of objects that would store a usual dataset
implem.scores <- data.frame( "SubjID"=1:4, # first column has the subject IDs
                 "Label"=supply.list, # second column has the supply.list object
                 "Supply"=c(90,NA,60,120) ) # the third column has the values of their supplies, the second score is missing

# check how it looks like
implem.scores

# you can see it in a separate window
View(implem.scores)


# ..and also make lists
l <- list(1,2,"aa",3,list("bb",NA,0),10) # list composed of 4 numbers, 1 string and 1 sublist (embedded list) composed of a string, a number and the special value for "missing data" (NA)
l

# in case you forgot what you have here (or just want to check)...

class(supply.list)
class(implem.scores)
class(l)

# ... class tells you what these objects are

# find out more about what this command does 
# (writing a question mark in front of a command displays info about it in the help window)

?class


# since we are talking about class, there are 2 other sorts you shold know of: factors and ordered
# these can be used to store value labels for variables in R 

# if we make a variable f is a vector of numbers 1, 2 or 3
f <- c(1,3,2,2,3,1,1)
# we can attach value labels to it (1=red, 2=blue, 3=green)
f <- factor(f,
            levels = c(1,2,3),
            labels = c("red", "blue", "green"))
f

# if we make a variable o is a vector of numbers  1, 3 or 5
o <-c(1,3,5,5,3,1,5,1)
# we can attach ordered value labels to it (1=Low, 3=Medium, 5=High)
o <- ordered(o,
            levels = c(1,3, 5),
            labels = c("Low", "Medium", "High")) 

# remember: it is always good to check what class your objects are
# because various functions treat different objects in different ways
# ... or send error messages instead of doing what you want them to do

##-------------##
#### TO DO 3 ####

# find out what class is the object called f
# fill in here

##-------------##


# you can also check what is stored in a data frame 

# what are the variable names
names(implem.scores)
# ... which are the same with column names
colnames(implem.scores)
# check the name of the second variable
names(implem.scores)[2]
# change it if you don't like it
names(implem.scores)[2] <- "Description"
# check which are the row names
rownames(implem.scores)
# and change these too
rownames(implem.scores) <- c("John","Mike","Anna","Kenny")
# or only one of them
rownames(implem.scores)[4] <- "Kenny Jr."
# and view the results
View(implem.scores)

##-------------##
#### TO DO 4 ####

# access the score and label of the second respondent
# fill in here

##-------------##


# Now we've placed a lot of stuff in the environment, and it gets messy
# it is time for some basic memory management
ls() # lists the currently defined objects (e.g., variables)
rm(o) # removes an object from memory, freeing some space

#...and do your first descriptive statistic
mean(implem.scores$Supply)

# R does not ignore NAs automatically, so you have to tell it to do so
mean(implem.scores$Supply, na.rm=TRUE)

# ... and we can calculate implementation over 4 months given the medication supply 
implem.scores$Implem <- implem.scores$Supply/120*100
# see the values
implem.scores$Implem

# ... and see how well they implement on average
mean(implem.scores$Implem, na.rm=TRUE)
# and the minimum score
min(implem.scores$Implem, na.rm=TRUE)


# ... and dichotomize Implementation scores (only if you have good reasons to do so!)
implem.scores$Implem2cat[implem.scores$Implem>=90] <- "90 or above" 
implem.scores$Implem2cat[implem.scores$Implem<90] <- "less than 90"

# and see the new variable
implem.scores$Implem2cat
# and check if this is coded properly - compare with initial variable
implem.scores$Implem


# you can do this as a function
# for calculation of implementation

impute.implem <- function(supplydays, totaldays){
  
  implem.perc <- supplydays/totaldays*100
  return(implem.perc)
}

implem.scores$ImplemF <- impute.implem(implem.scores$Supply, 120)
implem.scores$ImplemF

# for dichotomisation

dichot.implem <- function(implem.value, threshold){
  implem.cat <- c()
  for (i in 1:length(implem.value) ){
    if (is.na(implem.value[i])){
      implem.cat[i] <- NA
    } else if (implem.value[i] >= threshold){
      implem.cat[i] <- paste(threshold, " or over", sep="")
    } else {
      implem.cat[i] <- paste("less than ", threshold, sep="")
    }
  }
  return(implem.cat)
}

implem.scores$Implem2catF <- dichot.implem(implem.scores$ImplemF, 90)
implem.scores$Implem2catF

# and view the results
implem.scores[,c("Implem", "Implem2cat","ImplemF" ,  "Implem2catF")]


##-------------##
#### TO DO 5 ####

# let's imagine that the total number of days for which these supplies were calculated was 150 (5 months)
# calculate a new variable with implementation scores for the 5-month period

implem.scores$Implem5M <- # fill in here
  
# check what is the maximum and minumum 5-month implementation  score in your sample

# fill in here

##-------------##


# if you want, save the environment in the folder: Go to Environment -> Save
# or write this:
save.image(file = "my1stEnvironment.RData")

# don't forget to clean up your environment before starting a new session - click on the broom icon in the 'environment' tab.




