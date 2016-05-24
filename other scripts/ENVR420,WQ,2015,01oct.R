# R script to compare water quality within Biodiversity water feature
# e.g. "Is A statistically different than B, and why might this be?"

# clear memory
rm(list=ls())

# confirm path
?getwd
getwd()

# Step 0: Make sure your .csv dataset is "clean" - use NA for any blank cells, no complex column names

# Step 1: read data from .csv file
# 1.1: Set up data path
link.wq <- url("http://ecohydro.ires.ubc.ca/pdfs/ENVR420,WQlab,2015.csv")
# or ENVR420,WQlab,2013_2015.csv to use data from 3 years
link.wq.3yrs <- url("http://ecohydro.ires.ubc.ca/pdfs/ENVR420,WQlab,2013_2015.csv")

# 1.2: Import data into a data.frame
?read.csv
wq <- read.csv(link.wq)
# wq <- read.csv(link.wq.3yrs)  ## Un-comment this line to load data from 3 years

# Step 2. Explore data
# 2.1: have a look at how the data is structured, ensuring that it imported properly
?head
head(wq)

# 2.2: look at the overall structure of the data
# wq is a data.frame
class(wq)
?summary
summary(wq) #this will give a summary of each variable in the data.frame

wq$doc_no3_ratio <- wq$DOC.mgL / wq$NO3_N.mgL

# Site is a variable withing the data.frame "wq"; written as: wq$Site
# wq$Site is a factor variable, e.g. it has levels or descriptors rather than being a numerical variable
?class
class(wq$Location)
summary(wq$Location)

#recode the date values from class=factor to class=date
class(wq$Date)
wq$Date <- as.Date(wq$Date,format = "%m/%d/%y")
class(wq$Date)

# wq$DOC.mgL is a numeric variable
summary(wq$DOC.mgL)
class(wq$DOC.mgL)

# 2.3: subset the data.frame into groups cooresponding to the days of analysis
# recall that the samples were analyzed at time zero and after 7 days. 

#the next lines will create separate data.frames
?subset
wq.day0 <- subset(wq,Date == "2015-09-17")
wq.day7 <- subset(wq,Date == "2015-09-24")

# 2.4: make a few exploratory plots
?boxplot
boxplot(DOC.mgL ~ Location, data = wq.day0) # this makes plots that 

plot(wq$DOC.mgL,wq$NO3_N.mgL)

# 3. Do some analysis. Here, we will do a T-test to compare sites at time zero (only two at a time!).

# Create new data.frames to facilitate comparison
wq.upper.day0 <- subset(wq.day0, Location == "Upper")
wq.middle.day0 <- subset(wq.day0, Location == "Middle")

?t.test
t.test(wq.upper.day0$DOC.mgL, wq.middle.day0$DOC.mgL)

# 3.1 Determine if there are differences between sample locations within a site - must use ANOVA since more than 2 locations 
?aov
DOC.aov <- aov(DOC.mgL ~ Location, data = wq.day0) # tells if there are statistical differences within the Site (e.g. if DOC at "Locations" are statistically different)
summary(DOC.aov)
?TukeyHSD
TukeyHSD(DOC.aov)

# Compute mean and SD for one parameter at a single location [note - this is not the mos elegant way but is useful for leanring/understanding R]
mean(subset(wq.day0,Location=="Upper")$DOC.mgL)
sd(subset(wq.day0,Location=="Upper")$DOC.mgL)

# Reference for DOC analysis method
#Jollymore, A., M.S. Johnson and I. Hawthorne (2012). Submersible UV-Vis spectroscopy for quantifying streamwater organic carbon dynamics: implementation and challenges before and after forest harvest in a headwater stream. Sensors 12(4):3798-3813. doi:10.3390/s120403798

# Note for handling NA's
# "na.omit" is your friend

mean(wq$NO3_N.mgL)
mean(na.omit(wq$NO3_N.mgL))


### Assignment:
# Prepare a brief report describing the water quality determined from the samples collected.
#
# Use creative comparisons and critical thinking!
#
# Format: 
# 1. Introduction
# 2. Materials and Methods
# 3. Results and Discussion (include mean +/- SD when reporting results, include p-values when making comparisons)
# 4. Summary Conclusions