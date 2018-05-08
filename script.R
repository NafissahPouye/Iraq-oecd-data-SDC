# Load required packages 
library(readxl)      # for read & write excel/csv files
library(sdcMicro)    # sdcMicro package with functions for the SDC process 
library(tidyverse)   #for data merging and extraction

# Set working directory 
setwd("C:/Users/LENOVO T46OS/Desktop/SDC Documentation/Iraq-oecd-data-anonymization") 

# Import files after removing ages quantiles column
file <- read_excel("sample1.xlsx", 
                   col_types = c("numeric","text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "text", "numeric", "text", 
                                 "numeric", "text", "numeric", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text", "numeric", "text", 
                                 "numeric", "text", "text", "numeric", 
                                 "numeric", "numeric", "text", "numeric", 
                                 "numeric", "text", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "numeric", "text", "text", 
                                 "numeric", "numeric", "text", "text", 
                                 "numeric", "text", "text", "numeric", 
                                 "text", "numeric", "text", "text", 
                                 "numeric", "text", "text", "numeric", 
                                 "text", "text", "numeric", "text", 
                                 "text", "numeric", "text", "text", 
                                 "text", "numeric", "text"))
dim(file)

# Convert key variables into factors
file$A4_status<-as.factor(file$A4_status)
file$A5_type_of_accomodation<-as.factor(file$A5_type_of_accomodation)
file$A6_location<-as.factor(file$A6_location)
file$A6_location_2<-as.factor(file$A6_location_2)
file$D1_gender<-as.factor(file$D1_gender)
file$D8_where_is_your_home_where_you_moved_here_from<-as.factor(file$D8_where_is_your_home_where_you_moved_here_from)
file$D6_what_is_your_ethno_religious_affiliation<-as.factor(file$D6_what_is_your_ethno_religious_affiliation)
file$D6a_if_other_please_specify<-as.factor(file$D6a_if_other_please_specify)
file$D7_since_when_do_you_live_in_this_site<-as.factor(file$D7_since_when_do_you_live_in_this_site)

# Store variables to anonymize - keys variables and PRAM variables
selectedKeyVars = c('D8_where_is_your_home_where_you_moved_here_from','A4_status','A5_type_of_accomodation', 'D1_gender','D2_Age', 'D6_what_is_your_ethno_religious_affiliation', 'D6a_if_other_please_specify','D7_since_when_do_you_live_in_this_site','D6_sparse')
pramsVars = c('A6_location','A6_location_2')
subVars = c('ID', selectedKeyVars, pramsVars)

# Create a data frame the variables to anonymize
fileSub<-file[,subVars]
fileSub <- fileSub[which(!duplicated(fileSub$ID)),] 
fileSub<-as.data.frame(fileSub)

# Create a SDC object
sdcObj<- createSdcObj(dat = fileSub, keyVars = selectedKeyVars, pramVars = pramsVars, weightVar = NULL, numVars = NULL)
print(sdcObj)

# Summary of the disclosure risk at global and individual level
print(sdcObj,"risk")
max(sdcObj@risk$individual[, "risk"])

# Apply recoding to the key variable D2_Age - set classes of length 10
fileSub$D2_Age<-as.numeric(fileSub$D2_Age)
sdcObj <- globalRecode(sdcObj, column = "D2_Age", breaks = c( 18,28,38, 48, 58, 68, 78, 88))
table(sdcObj@manipKeyVars$D2_Age)

# Apply local suppression to the variables refering to ethno-religious affiliation, arrival dates to the sites, gender, age, home country
sdcObj <- localSupp(sdcObj, threshold = 0.1, keyVar = 'D6_what_is_your_ethno_religious_affiliation') 
sdcObj<- localSupp(sdcObj, threshold = 0.1, keyVar = 'D6a_if_other_please_specify')
sdcObj<- localSupp(sdcObj, threshold = 0.1, keyVar = 'D7_since_when_do_you_live_in_this_site') 
sdcObj<- localSupp(sdcObj, threshold = 0.1, keyVar = 'D6_sparse')
sdcObj<- localSupp(sdcObj, threshold = 0.1, keyVar = 'D1_gender')
sdcObj<- localSupp(sdcObj, threshold = 0.5, keyVar = 'D2_Age')
sdcObj<- localSupp(sdcObj, threshold = 0.1, keyVar = 'D8_where_is_your_home_where_you_moved_here_from')
print(sdcObj, "ls")

# Apply PRAM to variables refering to locations with default diagonal entries 0.8
set.seed(123) 
sdcObj <- pram(sdcObj, variables = pramsVars)
table(sdcObj@pramVars)

# Extract and store anonymized data
dataAnon <- extractManipData(sdcObj)
fileUnanonymized<-file
fileUnanonymized[,c('ID','A4_status','A5_type_of_accomodation', 'D1_gender','D2_Age', 'D6_what_is_your_ethno_religious_affiliation', 'D6a_if_other_please_specify','D7_since_when_do_you_live_in_this_site','A6_location','A6_location_2','D8_where_is_your_home_where_you_moved_here_from','D6_sparse')]<-list(NULL)
fileCombined<-bind_cols(x=dataAnon, y=fileUnanonymized)
write.csv(fileCombined,'Iraq_OECD_anonymized_data.csv') 
print(sdcObj, 'risk')

# Utility loss measures
# Check the number of missing values (NA) after local suppression application
namesKeyVars<- names(sdcObj@manipKeyVars) 
NAcount <- matrix(NA, nrow = 2, ncol = length(namesKeyVars)) 
colnames(NAcount)  <- c(paste0('NA', namesKeyVars))
rownames(NAcount)  <- c('initial', 'treated')
for(i in 1:length(namesKeyVars)) 
  {
    NAcount[1, i] <- sum(is.na(sdcObj@origData[,namesKeyVars[i]]))
    NAcount[2, i] <- sum(is.na(sdcObj@manipKeyVars[,i]))
}   
NAcount

# Assess number of records changed for the PRAMmed variables
namesPramVars <- names(sdcObj@manipPramVars) 
recordChanged<- rep(0, length(namesPramVars))  
names(recordChanged)  <- c(paste0('RC', namesPramVars)) 
for(j in 1:length(namesPramVars)) 
  {comp <- sdcObj@origData[namesPramVars[j]] != sdcObj@manipPramVars[namesPramVars[j]] 
    temp1 <- sum(comp, na.rm = TRUE)
    temp2 <- sum(is.na(comp)) 
    temp3 <- sum(is.na(sdcObj@origData[namesPramVars[j]]) +
                   is.na(sdcObj@manipPramVars[namesPramVars[j]])==2)
    recordChanged[j] <- temp1 + temp2 - temp3 
  }
recordChanged

# Contingency tables between pairs of variables
# Contigency tables between the variables D2_Age & D1_gender
table(sdcObj@origData[,c('D2_Age','D1_gender')])
table(sdcObj@manipKeyVars[,c('D2_Age','D1_gender')])

# Contigency tables between the variables A6_location & A6_location_2
table(sdcObj@origData[,c('A6_location','A6_location_2')])
table(sdcObj@manipPramVars[,c('A6_location','A6_location_2')])

#Contigency tables between the variables D7_since_when_do_you_live_in_this_site & D8_where_is_your_home_where_you_moved_here_from 
table(sdcObj@origData[,c('D7_since_when_do_you_live_in_this_site','D8_where_is_your_home_where_you_moved_here_from')])
table(sdcObj@manipKeyVars[,c('D7_since_when_do_you_live_in_this_site','D8_where_is_your_home_where_you_moved_here_from')])

#Contigency tables between the variables D6_what_is_your_ethno_religious_affiliation, D6a_if_other_please_specify & D6_sparse
table(sdcObj@origData[,c('D6_what_is_your_ethno_religious_affiliation','D6a_if_other_please_specify','D6_sparse')])
table(sdcObj@manipKeyVars[,c('D6_what_is_your_ethno_religious_affiliation','D6a_if_other_please_specify','D6_sparse')])
 
# Mosaic plots to compare original data and anonymized data
# mosaic plot for the key variable D6_sparse -ethno-religious affiliation
ethnicity <-t(cbind(table(sdcObj@origData$D6_sparse), table(sdcObj@manipKeyVars$D6_sparse)))
rownames(ethnicity) <- c("original \n data", "treated \n  data")
mosaicplot(ethnicity, main="Ethno-religious affiliation - Sparse", las = 2, color = 2:10)

# mosaic plot for the key variable D6_what_is_your_ethno_religious_affiliation -religion
religion<-t(cbind(table(sdcObj@origData$D6_what_is_your_ethno_religious_affiliation), table(sdcObj@manipKeyVars$D6_what_is_your_ethno_religious_affiliation)))
rownames(religion) <- c("original \n data", "treated \n  data")
mosaicplot(religion,main="Ethno-religious affiliation", las = 2, color = 2:8)

# mosaic plot for the key variables D1_gender & D2_Age- Gender and Age after anonymization
ageGender<-t(cbind(table(sdcObj@manipKeyVars$D1_gender), table(sdcObj@manipKeyVars$D2_Age)))
rownames(ageGender) <- c("original \n data", "treated \n  data")
mosaicplot(ageGender,main="Gender & Age", las = 2, color = 2:9)

# mosaic plot for the key variable D8_where_is_your_home_where_you_moved_here_from -home country
homeCountry<-t(cbind(table(sdcObj@origData$D8_where_is_your_home_where_you_moved_here_from), table(sdcObj@manipKeyVars$D8_where_is_your_home_where_you_moved_here_from)))
rownames(homeCountry) <- c("original \n data", "treated \n data")
mosaicplot(homeCountry,main="Home Country", las = 2, color = 2:11)    

# Generate an internal report
report(sdcObj, filename ="SDC internal report",internal = T, verbose = TRUE) 

