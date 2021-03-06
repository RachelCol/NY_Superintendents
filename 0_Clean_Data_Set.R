# CREATE DATA SET

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("readxl")

library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# -----

case_data <- read_csv("ny_superintendents.csv")
NYS <- case_data

# delete rows with information about the survey (i.e. they are not respondents)
NYS <- NYS[-c(1:2), ]
# delete rows that have NA for the very first question
NYS <- NYS[-c(which(is.na(NYS$Q1))), ]
# delete unneeded columns (extraneous data)
NYS <- NYS[, -c(1:4, 8, 13:14)]

# fix numbers in "how many children are homeschooled in your district"
NYS$Q6_1
which( colnames(NYS)=="Q6_1")
NYS[104, 13] <- "45"
NYS[126, 13] <- "1"
NYS$Q6_1 <- as.numeric(NYS$Q6_1)

# remove incomplete rows

NYS <- subset(NYS, Progress == 100)

# change multiple choice responses to categorical
NYS$Q2 <- ifelse(NYS$Q2 == "Yes", 1,
                 ifelse(NYS$Q2 == "No", 2, NA))
NYS$Q7 <- ifelse(NYS$Q7 == "Less than 1%", 1,
                 ifelse(NYS$Q7 == "Between 1% and 5%", 2, 
                        ifelse(NYS$Q7 == "Between 5% and 10%", 3,
                               ifelse(NYS$Q7 == "Greater than 10%", 3, NA))))
NYS$Q3 <- ifelse(NYS$Q3 == "Strongly disagree", 1,
                 ifelse(NYS$Q3 == "Disagree", 2, 
                        ifelse(NYS$Q3 == "Neutral", 3,
                               ifelse(NYS$Q3 == "Agree", 4, 
                                      ifelse(NYS$Q3 == "Strongly agree", 5, NA)))))
NYS$Q4 <- ifelse(NYS$Q4 == "Increased", 1,
                 ifelse(NYS$Q4 == "Decreased", 2, 
                        ifelse(NYS$Q4 == "Kept the same", 3,
                               ifelse(NYS$Q4 == "Don't know", 4, NA))))
NYS$Q5 <- ifelse(NYS$Q5 == "Strongly disagree", 1,
                 ifelse(NYS$Q5 == "Disagree", 2, 
                        ifelse(NYS$Q5 == "Neutral", 3,
                               ifelse(NYS$Q5 == "Agree", 4, 
                                      ifelse(NYS$Q5 == "Strongly agree", 5, NA)))))
NYS$Q9_1 <- ifelse(NYS$Q9_1 == "Very ineffective", 1,
                 ifelse(NYS$Q9_1 == "Ineffective", 2, 
                        ifelse(NYS$Q9_1 == "Neutral", 3,
                               ifelse(NYS$Q9_1 == "Effective", 4, 
                                      ifelse(NYS$Q9_1 == "Very Effective", 5, NA)))))
NYS$Q9_2 <- ifelse(NYS$Q9_2 == "Very ineffective", 1,
                   ifelse(NYS$Q9_2 == "Ineffective", 2, 
                          ifelse(NYS$Q9_2 == "Neutral", 3,
                                 ifelse(NYS$Q9_2 == "Effective", 4, 
                                        ifelse(NYS$Q9_2 == "Very Effective", 5, NA)))))
NYS$Q9_3 <- ifelse(NYS$Q9_3 == "Very ineffective", 1,
                   ifelse(NYS$Q9_3 == "Ineffective", 2, 
                          ifelse(NYS$Q9_3 == "Neutral", 3,
                                 ifelse(NYS$Q9_3 == "Effective", 4, 
                                        ifelse(NYS$Q9_3 == "Very Effective", 5, NA)))))
NYS$Q9_4 <- ifelse(NYS$Q9_4 == "Very ineffective", 1,
                   ifelse(NYS$Q9_4 == "Ineffective", 2, 
                          ifelse(NYS$Q9_4 == "Neutral", 3,
                                 ifelse(NYS$Q9_4 == "Effective", 4, 
                                        ifelse(NYS$Q9_4 == "Very Effective", 5, NA)))))
NYS$Q34 <- ifelse(NYS$Q34 == "at a public school, by its professional staff.", 1,
                   ifelse(NYS$Q34 == "at a private school, by its professional staff.", 2, 
                          ifelse(NYS$Q34 == "at a student's home, by a NYS certified teacher.", 3,
                                 ifelse(NYS$Q34 == "at a student's home, by the student's parents.", 4, 
                                        ifelse(NYS$Q34 == "other", 5, NA)))))
NYS$Q33 <- ifelse(NYS$Q33 == "Yes", 1,
                 ifelse(NYS$Q33 == "No", 2, 
                        ifelse(NYS$Q33 == "Don't know", 3, NA)))
NYS$Q11 <- ifelse(NYS$Q11 == "Strongly disagree", 1,
                 ifelse(NYS$Q11 == "Disagree", 2, 
                        ifelse(NYS$Q11 == "Neutral", 3,
                               ifelse(NYS$Q11 == "Agree", 4, 
                                      ifelse(NYS$Q11 == "Strongly agree", 5, NA)))))
NYS$Q12 <- ifelse(NYS$Q12 == "Lowered", 1,
                  ifelse(NYS$Q12 == "Raised", 2, 
                         ifelse(NYS$Q12 == "Kept the same", 3,
                                ifelse(NYS$Q12 == "Don't know", 4, NA))))
NYS$Q30 <- ifelse(NYS$Q30 == "NYS certified teachers", 1,
                  ifelse(NYS$Q30 == "Homeschool peer group review panels", 2, 
                         ifelse(NYS$Q30 == "Parents prepare a written narrative for their own children", 3,
                                ifelse(NYS$Q30 == "Other", 4, NA))))
NYS$Q32 <- ifelse(NYS$Q32 == "Yes", 1,
                  ifelse(NYS$Q32 == "No", 2, 
                         ifelse(NYS$Q32 == "Don't know", 3, NA)))
NYS$Q13 <- ifelse(NYS$Q13 == "Strongly disagree", 1,
                  ifelse(NYS$Q13 == "Disagree", 2, 
                         ifelse(NYS$Q13 == "Neutral", 3,
                                ifelse(NYS$Q13 == "Agree", 4, 
                                       ifelse(NYS$Q13 == "Strongly agree", 5, NA)))))
NYS$Q14 <- ifelse(NYS$Q14 == "Strongly disagree", 1,
                  ifelse(NYS$Q14 == "Disagree", 2, 
                         ifelse(NYS$Q14 == "Neutral", 3,
                                ifelse(NYS$Q14 == "Agree", 4, 
                                       ifelse(NYS$Q14 == "Strongly agree", 5, NA)))))
NYS$Q15 <- ifelse(NYS$Q15 == "Yes", 1,
                  ifelse(NYS$Q15 == "No", 2, 
                         ifelse(NYS$Q15 == "Don't know", 3, NA)))
NYS$Q16 <- ifelse(NYS$Q16 == "High school graduation or its equivalent", 1,
                  ifelse(NYS$Q16 == "Some college", 2, 
                         ifelse(NYS$Q16 == "College degree", 3,
                                ifelse(NYS$Q16 == "Teaching certification", 4, NA))))
NYS$Q17 <- ifelse(NYS$Q17 == "Strongly disagree", 1,
                  ifelse(NYS$Q17 == "Disagree", 2, 
                         ifelse(NYS$Q17 == "Neutral", 3,
                                ifelse(NYS$Q17 == "Agree", 4, 
                                       ifelse(NYS$Q17 == "Strongly agree", 5, NA)))))
NYS$Q18 <- ifelse(NYS$Q18 == "Strongly disagree", 1,
                  ifelse(NYS$Q18 == "Disagree", 2, 
                         ifelse(NYS$Q18 == "Neutral", 3,
                                ifelse(NYS$Q18 == "Agree", 4, 
                                       ifelse(NYS$Q18 == "Strongly agree", 5, NA)))))
NYS$Q19 <- ifelse(NYS$Q19 == "Yes", 1,
                  ifelse(NYS$Q19 == "No", 2, 
                         ifelse(NYS$Q19 == "Don't know", 3, NA)))
NYS$Q20 <- ifelse(NYS$Q20 == "Yes", 1,
                  ifelse(NYS$Q20 == "No", 2, 
                         ifelse(NYS$Q20 == "Don't know", 3, NA)))
NYS$Q21 <- ifelse(NYS$Q21 == "Strongly disagree", 1,
                  ifelse(NYS$Q21 == "Disagree", 2, 
                         ifelse(NYS$Q21 == "Neutral", 3,
                                ifelse(NYS$Q21 == "Agree", 4, 
                                       ifelse(NYS$Q21 == "Strongly agree", 5, NA)))))
NYS$Q22 <- ifelse(NYS$Q22 == "Strongly disagree", 1,
                  ifelse(NYS$Q22 == "Disagree", 2, 
                         ifelse(NYS$Q22 == "Neutral", 3,
                                ifelse(NYS$Q22 == "Agree", 4, 
                                       ifelse(NYS$Q22 == "Strongly agree", 5, NA)))))
NYS$Q24 <- ifelse(NYS$Q24 == "Strongly disagree", 1,
                  ifelse(NYS$Q24 == "Disagree", 2, 
                         ifelse(NYS$Q24 == "Neutral", 3,
                                ifelse(NYS$Q24 == "Agree", 4, 
                                       ifelse(NYS$Q24 == "Strongly agree", 5, NA)))))
NYS$Q25 <- ifelse(NYS$Q25 == "Yes", 1,
                  ifelse(NYS$Q25 == "No", 2, 
                         ifelse(NYS$Q25 == "Don't know", 3, NA)))

# Create new variable combining all where most administer standardized tests 
# themselves at home (Q34 == 4), or are at least allowed to do so (Q33 == 1).
table(NYS$Q34, NYS$Q33)
NYS$TESTHOME <- ifelse(NYS$Q34 == 4 | NYS$Q33 == 1, 1, 0)

# Create new variable combining all where most parents do the written narratives 
# themselves (Q30 == 3), or are at least allowed to do so (Q32 == 1).
table(NYS$Q30, NYS$Q32)
NYS$NARRHOME <- ifelse(NYS$Q30 == 3 | NYS$Q32 == 1, 1, 0)

# create count row, for counting the number of respondents per county later
NYS$countn <- ifelse(NYS$Progress == "100", 1, 0) # change to 1 for complete survey

# -----

# CHECK whether all entries are complete:



# -----

# ADD COUNTY FIPS CODES

# create county name and values columns in NYS data set; remove "County"
NYS$county_name <- NYS$Q1
NYS$county_name <- substr(NYS$county_name, 1, nchar(NYS$county_name)-7)
NYS$county_name[NYS$county_name == "Saint Lawrence"] <- "St Lawrence"

# load county codes data set
county_codes <- read.csv("NY_Municipalities_and_County_FIPS_codes.csv")
code_data <- county_codes[, -c(2:5)]
colnames(code_data) <- c("county_name", "fips")
# additional data cleaning
code_data <- code_data[!duplicated(code_data), ] # remove duplicates
code_data[30, 2] <- 36089 # fix the St. Lawrence Code (it was incorrect)
code_data <- code_data[-28, ] # remove New York City (it's a duplicate)

# merge into original data set, for other analysis: 
NYS <- merge(x = NYS, y = code_data, 
             by = "county_name", all.x=TRUE)

# -----

# ADDING COUNTY-LEVEL INFORMATION

# The following data sets were downloaded here: 
# https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/ 

# cleaning income data set, which is labeled 2019
income <- read_excel("PovertyEstimates.xls")
# PCTPOVALL_2019 = poverty rate in 2019
# MEDHHINC_2019 = median household income in 2019
income_data <- income[, c(1, 11, 26)]
income_data <- income_data[-c(1:4), ]
colnames(income_data) <- c("fips", "pctpov", "medhhinc")
income_data$pctpov <- round(as.numeric(income_data$pctpov), digits = 1)
income_data$medhhinc <- as.numeric(income_data$medhhinc)

# merge into original data set: 
NYS <- merge(x = NYS, y = income_data, by = "fips", all.x=TRUE)

# cleaning education data set, which is labeled 2015-2019
education <- read_excel("Education.xls")
ba_data <- education[, c(1, 47)]
ba_data <- ba_data[-c(1:4), ]
colnames(ba_data) <- c("fips", "pctba")

# merge into original data set: 
NYS <- merge(x = NYS, y = ba_data, 
             by = "fips", all.x=TRUE)

# cleaning education data set, creating 2015 column
population <- read_excel("PopulationEstimates.xlsx")
pop_data <- population[, c(1, 7:8)]
pop_data <- pop_data[-1, ]
colnames(pop_data) <- c("fips", "pop2010", "pop2020")
pop_data$pop2010 <- as.numeric(pop_data$pop2010)
pop_data$pop2020 <- as.numeric(pop_data$pop2020)
pop_data$pop2015 <- round((pop_data$pop2010 + pop_data$pop2020)/2)

# merge into original data set: 
NYS <- merge(x = NYS, y = pop_data, 
             by = "fips", all.x=TRUE)

# -----

# Create variables for regression: 



# ----- 

# save cleaned data set as new file:

write.csv(NYS,"/Users/Rachel/R-Projects/new_york_superintendents/NYS_cleaned_file.csv", row.names = TRUE)

# end script creation