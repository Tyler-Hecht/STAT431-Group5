require(readxl)
require(tidyverse)

inc <- read_xls("C:\\Users\\tyler\\Documents\\Courses\\23S\\STAT 431\\Project/Dec-2022-Prison-Population-Data-Set.xls")
#Change appropriate columns to data format
inc$'Date of Birth' <- as.Date(inc$'Date of Birth', format = "%m/%d/%y")
inc$'Current Admission Date' <- as.Date(inc$'Current Admission Date', format = "%m/%d/%y")
inc$'Custody Date' <- as.Date(inc$'Custody Date', format = "%m/%d/%y")
inc$'Sentence Date' <- as.Date(inc$'Sentence Date', format = "%m/%d/%y")
inc$'Projected Discharge Date' <- as.Date(inc$'Projected Discharge Date', format = "%m/%d/%y")
inc$'Projected Mandatory Supervised Release (MSR) Date' <- as.Date(inc$'Projected Mandatory Supervised Release (MSR) Date', format = "%m/%d/%y")
#Eliminate leading zeros and add zeros back
inc$'Sentence Years' = as.numeric(inc$'Sentence Years')
inc$'Sentence Months' = as.numeric(inc$'Sentence Months')

#Analyzing NAs in dataset
colSums(is.na(inc))
nas = inc %>% 
  filter(is.na(`Projected Discharge Date`) == TRUE)
cnts = nas %>% 
  group_by(`Sentence Years`) %>% 
  count()

#Building final dataset
#Accumulating sentence time
inc1 = inc %>% 
  mutate(`Sentence Months` = round(inc$`Sentence Months` / 12, 2)) %>% 
  mutate(`Sentence Time` = round(`Sentence Months` + `Sentence Years`, 2)) %>% 
  filter(is.na(`Sentence Time`) == FALSE)

write.csv(inc1, file = "C:\\Users\\andre\\OneDrive - University of Illinois - Urbana\\Current Classes\\STAT 431\\incar_data.csv")
  
# inc = inc %>% 
#   mutate('Sentence Months' = ifelse(inc$'Sentence Years' == "SDP", "SDP", inc$'Sentence Months')) %>% 
#   mutate('Sentence Months' = ifelse(inc$'Sentence Years' == "LIFE", "LIFE", inc$'Sentence Months')) %>% 
#   mutate('Projected Discharge Date' = ifelse(inc$'Sentence Years' == "LIFE", "LIFE", inc$'Projected Discharge Date')) %>% 
#   mutate('Projected Mandatory Supervised Release (MSR) Date' = ifelse(inc$'Sentence Years' == "LIFE", "LIFE", inc$'Projected Mandatory Supervised Release (MSR) Date')) %>% 
#   mutate('Projected Discharge Date' = ifelse(inc$'Sentence Years' == "SDP", "SDP", inc$'Projected Discharge Date')) %>% 
#   mutate('Projected Mandatory Supervised Release (MSR) Date' = ifelse(inc$'Sentence Years' == "SDP", "SDP", inc$'Projected Mandatory Supervised Release (MSR) Date'))


#Project Discharge Date
#Custody Date
#Sentence Date
#age distance
