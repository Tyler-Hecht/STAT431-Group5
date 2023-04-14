require(readxl)
require(tidyverse)
require(lubridate) 

inc <- read_xlsx("C:\\Users\\andre\\OneDrive - University of Illinois - Urbana\\Current Classes\\STAT 431\\incareration.xlsx")
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
cnts = inc %>% 
  group_by(`Sentence Years`) %>% 
  count()

#Region Grouping
region1 <- c(
  'Cook')
region2 <- c(
  'JoDaviess', 
  'Stephenson', 
  'Winnebago', 
  'Boone',
  'McHenry',
  'Lake',
  'Carroll',
  'Ogle',
  'DeKalb',
  'Kane',
  'DuPage',
  'Whiteside',
  'Lee',
  'Kendall',
  'Grundy',
  'Will',
  'Kankakee')
region3 <- c(
  'Rock Island',
  'Mercer',
  'Henry',
  'Bureau',
  'LaSalle',
  'Henderson',
  'Warren',
  'Knox',
  'Stark',
  'Putnam',
  'Marshall',
  'Livingston',
  'Ford',
  'Iroquois',
  'Vermillion',
  'Champaign',
  'McLean',
  'Woodford',
  'Tazewell',
  'Mason',
  'Peoria',
  'Fulton',
  'McDonough')
region4 <- c(
  'Hancock',
  'Adams',
  'Schuyler',
  'Brown',
  'Cass',
  'Menard',
  'Logan',
  'Dewitt',
  'Piatt',
  'Douglas',
  'Edgar',
  'Clark',
  'Coles',
  'Cumberland',
  'Effingham',
  'Shelby',
  'Moultrie',
  'Macon',
  'Christian',
  'Montgomery',
  'Sangamon',
  'Morgan',
  'Macoupin',
  'Green',
  'Jersey',
  'Calhoun',
  'Scott',
  'Pike')
region5 <- c(
  'Madison',
  'Bond',
  'Fayette',
  'Clay',
  'Jasper',
  'Crawford',
  'Lawerence',
  'Richland',
  'Edwards',
  'Wabash',
  'Wayne',
  'Marion',
  'Clinton',
  'St. Clair',
  'Monroe',
  'Randolph',
  'Washington',
  'Jefferson',
  'Perry',
  'Jackson',
  'Franklin',
  'Hamilton',
  'White',
  'Williamson',
  'Saline',
  'Union',
  'Johnson',
  'Pope',
  'Hardin',
  'Alexander',
  'Pulaski',
  'Massac',
  'Gallatin')

#Building final dataset
#Accumulating sentence time
inc1 = inc %>% 
  mutate(`Sentence Months` = round(inc$`Sentence Months` / 12, 2)) %>% 
  mutate(`Sentence Time` = round(`Sentence Months` + `Sentence Years`, 2)) %>%
  mutate(`Sentence Time` = ifelse(`Offense Type` == "Sex Crimes" & is.na(`Sentence Time`) == TRUE, "SDP", `Sentence Time`)) %>% 
  mutate(`Sentence Time` = ifelse(is.na(`Sentence Time`) == TRUE, "LIFE", `Sentence Time`)) %>% 
  filter(`Sentence Time` != "SDP") %>% 
  mutate(`Life` = ifelse(`Sentence Time` == "LIFE", 1, 0)) %>% 
  mutate(`Sentence Time (num)` = round(as.numeric(`Sentence Time`), 2)) %>%
  #Region variable (Region # matches # in term)
  mutate(`IDHS Region` = ifelse(`Sentencing County` %in% region1, 1, 
                                ifelse(`Sentencing County` %in% region2, 2,
                                       ifelse(`Sentencing County` %in% region3, 3, 
                                              ifelse(`Sentencing County` %in% region4, 4, 
                                                     5))))) %>% 
  #Sex variable (Male = 1, Female = 2)
  mutate(`Sex` = ifelse(`Sex` == "Male", 1, 2)) %>% 
  #Race variable (Black = 1, White = 2, Hispanic = 3, Asian = 4, American Indian = 5, Biracial = 6, Unknown = 7)
  mutate(`Race` = ifelse(`Race` == "Black", 1,
                         ifelse(`Race` == "White", 2, 
                                ifelse(`Race` == "Hispanic", 3, 
                                       ifelse(`Race` == "Asian", 4, 
                                              ifelse(`Race` == "American Indian", 5, 
                                                     ifelse(`Race` == "Bi-Racial", 6, 7))))))) %>% 
  #Age variable (difference between date of birth and sentencing date)
  mutate(`Age` = round(time_length(difftime(`Sentence Date`, `Date of Birth`), "years"), 2)) %>% 
  filter(`Age` > 0) %>% 
  #Offense Type variable (Person Crimes = 1, Sex Crimes = 2, Drug Crimes = 3, Property Crimes = 4, Other Crimes = 5)
  mutate(`Offense Type` = ifelse(`Offense Type` == "Person Crimes", 1,
                                 ifelse(`Offense Type` == "Sex Crimes", 2, 
                                        ifelse(`Offense Type` == "Drug Crimes", 3, 
                                               ifelse(`Offense Type` == "Property Crimes", 4, 5))))) %>% 
  #Veteran Status variable (Veteran = 1, Not Veteran = 2, Unknown = 3)
  mutate(`Veteran Status` = ifelse(`Veteran Status` == "Yes", 1,
                                   ifelse(`Veteran Status` == "No", 2, 3))) %>% 
  #Crime Class variable (Class X and Murder = 5, Class 1 = 4, Class 2 = 3, Class 3 = 2, Class 4 = 1, Unclassified = NA)
  mutate(`Crime Class` = ifelse(`Crime Class` == "Murder", 5,
                                ifelse(`Crime Class` == "Class X", 5, 
                                       ifelse(`Crime Class` == "Class 1", 4, 
                                              ifelse(`Crime Class` == "Class 2", 3, 
                                                     ifelse(`Crime Class` == "Class 3", 2, 
                                                            ifelse(`Crime Class` == "Class 4", 1, NA)))))))
  

cnts2 = inc1 %>% 
  group_by(`Sentence Time`) %>% 
  count()

table(inc1$`Race`)

unc <- inc %>% 
  filter(Race == "Bi-Racial")

table(inc$Race)


table(unc$`Crime Class`)
  

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

