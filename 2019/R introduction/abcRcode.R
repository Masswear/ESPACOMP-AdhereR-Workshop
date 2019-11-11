
####################################################
#                                                  #
#                  ABC of R scripting              #
#                                                  #
#          A Dima, Nov 2019; Example script        #
#      Contact: alexandra.dima@univ-lyon1.fr       #
####################################################

# R script is commented with #'s 
# Optional things that might make your life more meaningful are marked with ^^^^^

# ^^^^^ for more info and exercises, check this intro to R: http://cran.r-project.org/doc/contrib/Torfs+Brauer-Short-R-Intro.pdf

###########################################################
# Acknowledgements:
# Thanks to Dan Dediu and to Mike Allerhand for previous R intros
# other sources: 
# http://venus.ifca.unican.es/Rintro/
# http://www.ats.ucla.edu/stat/r/
###########################################################


# first make sure you set a folder for your R work, e.g. 
setwd("~/ACTIVE/ESPACOMP/ADAworkshop19")
# OR go to Session --> Set Working Directory --> To Source File Location


# now see what R does if you write a number

1 

# R echoes back your input

# what if you want to add 2 numbers?

1+2 

# R behaves like a calculator

(3465-435)*23/12

# it is really a calculator 
# but nothing has been saved in the environment so far
# for this to happen, you need to give a name to an object

x <- 3 

# your x is now in the environment, and has the value 1 
# (it is called a scalar = vector with a single value/element)

# you can now call it by its name

x

# or 

print(x)

# R prints the value

# now you can do things with x

x+1 # addition
x-1 # substraction
x*2 # multiplication
x/2 # division
x^2 # power
sqrt(x) # square root
log(x) # log base e
exp(x) # e^x
log(x,base=10) # log base 10
2^x

# if you asign a different value to x, the previous value is forgotten

x <- 7

# you can assign numbers as above, but also strings...

x <- "respondent"
x

# ... and add to the same string to form a new object

y <- paste("second ", x,sep="") # "paste" concatenates strings together
y


# ... and make vectors with more than 1 value/element
z <- c(x,y, "third", "last one")
z

# OR 

w <- c(1,2,3,4,5,6)
w

# OR

w <- 1:6
w


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


# you can also select one or some elements in a vector
z[2]
z[3]
z[1:2]
# and reassign another value to an element
z[1] <- "the very first respondent"
z


# ... and also make Boolean (logical) vectors
q <- c(TRUE, FALSE, FALSE, TRUE)
# for these, other logical operations apply
# "and" - TRUE & FALSE
# "or"  - TRUE | FALSE
# "not" or "negation" - !TRUE

##-------------##
#### TO DO 1 ####

# define an object February.days which represents a vector of all days in this month

February.days <- c(1:28)
February.days.seq <- seq(1:28)

# define an object week.days which represents a vector of all days in a week

week.days <- c("M", "Tu", "W", "Th", "F", "Sa", "Su")

# access the day of the month for today
February.days[22]

# access the day of the week for today
week.days[3]

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

four.weeks <- matrix(1:28,nrow=4,ncol=7) 

# access the second day of the 3rd week

selected.day <- four.weeks[3, 2]

##-------------##



# and can also create data frames - the type of objects that would store an usual dataset
respondents.scores <- data.frame( "SubjID"=1:4, # first column has the subject IDs
                 "Label"=z, # second column has the z object (the vector with our respondents
                 "Score"=c(NA,23,15,54.5) ) # the third column has their scores, the first score is missing

# check how it looks like
respondents.scores

# you can see it in a separate window
View(respondents.scores)


# ..and you can also make lists
l <- list(1,2,"aa",3,list("bb",NA,0),10) # list composed of 4 numbers, 1 string and 1 sublist (embedded list) composed of a string, a number and the special value for "missing data" (NA)
l

# in case you forgot what you have here (or just want to check)...

class(x)
class(z)
class(w)
class(a)
class(respondents.scores)
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
class(f)

##-------------##


# you can also check what is stored in a data frame 

# what are the variable names
names(respondents.scores)
# ... which are the same with column names
colnames(respondents.scores)
# check the name of the second variable
names(respondents.scores)[2]
# change it if you don't like it
names(respondents.scores)[2] <- "Name"
# check which are the row names
rownames(respondents.scores)
# and change these too
rownames(respondents.scores) <- c("John","Mike","Anna","Kenny")
# or only one of them
rownames(respondents.scores)[4] <- "Kenny Jr."
# and view the results
View(respondents.scores)

##-------------##
#### TO DO 4 ####

# access the score and label of the second respondent
respondents.scores[2, c("SubjID", "Score") ]

##-------------##


# Now we've placed a lot of stuff in the environment, and it gets messy
# it is time for some basic memory management
ls() # lists the currently defined objects (e.g., variables)
rm(a) # removes an object from memory, freeing some space

# respondents.scores[, "Score"]
#...and do your first descriptive statistic
mean(respondents.scores$Score)

# R does not ignore NAs automatically, so you have to tell it to do so
mean(respondents.scores$Score, na.rm=TRUE)

# ... and we can add their random IQ scores by the following function
respondents.scores$IQ <- rnorm(4, mean=100, sd=15)
# see the values
respondents.scores$IQ

# ... and see how clever they are on average
mean(respondents.scores$IQ)

# ... and dichotomize IQ scores
respondents.scores$IQ2cat[respondents.scores$IQ>100] <- "above" 
respondents.scores$IQ2cat[respondents.scores$IQ<=100] <- "average or less"

# and see the new variable
respondents.scores$IQ2cat
# and check if this is coded properly - compare with initial variable
respondents.scores$IQ

# we can also increase the IQ scores of some values
# first copy the variable
respondents.scores$IQplus <- respondents.scores$IQ
# then modify only 2 values (2 and 3)
respondents.scores$IQplus[2:3] <- respondents.scores$IQ[2:3] +15

# and see the new variable
respondents.scores$IQplus

##-------------##
#### TO DO 5 ####

# generate a new variable that represents their emotional intelligence scores (EQ)
respondents.scores$EQ <- rnorm(4, mean=100, sd=15)
# check what is the maximum and minumum EQ score in your sample
min(respondents.scores$EQ)
max(respondents.scores$EQ)

##-------------##


# if you want, save the environment in the folder: Go to Environment -> Save
# or write this:
save.image(file = "my1stEnvironment.RData")

# don't forget to clean up your environment before starting a new session. There is a little broom in the 'environment' tab.




