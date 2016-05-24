### Starter script to read in salt gauging data

#Clear memory, confirm working directory
rm(list = ls())
ls()
getwd()

# Load the file containing data
saltQ <- read.csv("http://ecohydro.ires.ubc.ca/pdfs/CR_saltQ_25JUN2010.csv")

#SALT MASS added (g NaCl)               
massSalt <- c(40.04,40.13,40.14)

# Header of the data.frame
head(saltQ)

# Summary of the data.frame
summary(saltQ)
plot(saltQ$EC)

# a few suggestions for getting started
# do some subsetting
Q1 <- subset(saltQ, What == "Salt1")
head(Q1)

# BG1 = background EC at start of measurements
# Salt1 = salt trace for first measurement 
# Salt2 = salt trace for first measurement 
# Salt3 = salt trace for first measurement 
# CP0 = EC for calibration point 0 [0 mg NaCl L-1]
# CP1 = EC for calibration point 1 [20.2 mg NaCl L-1]
# CP2 = EC for calibration point 2 [40.0 mg NaCl L-1]

### Assignment:
# Prepare a brief report describing the experiment and results determined from the data collected.
# Here, the results are just the discharge you calculated. 
# Format: 
# 1. Introduction
# 2. Materials and Methods
# 3. Results and Discussion (include mean +/- SD when reporting results, include p-values when making comparisons)
# 4. Conclusions

# Things to think about: The data aren't perfect - what do you think could be sources of error in the measurements, and which 
# of these are likely observed in the data?

