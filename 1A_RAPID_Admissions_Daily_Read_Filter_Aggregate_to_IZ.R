#### Script produced with R3.6.1 (64 bit)

#####  in Rstudio 1.1.456 

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 30 June 2020 

### Partly Based on script by Cecilia Puech, Place and Wellbeing, 
#Public Health Scotland

### Run time on R studio server - approximately 2 minutes

#########################
### Step 1
### Set libraries and paths
###########################

#### load libraries - if not present please install packages

library(readr)
#library(readxl)
library(dplyr)

#code to install phsmethods
#note requires library(remotes)
#remotes::install_github("Health-SocialCare-Scotland/phsmethods", upgrade = "never")

library(phsmethods)
library(lubridate)
library(glue)


####check the current working directory if needed
getwd()

### Set filepaths for inputs, checks and final outputs, and any key file names
filepath_RAPID_In <- glue(
  "/conf/PHSCOVID19_Analysis/RAPID Reporting/Daily_extracts")
filepath_data <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/data")
filepath_checks <-glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/checks")
filepath_outputs <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs")
filepath_postcodes <- glue(
  "/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory")

input_RAPID <- "RAPID_admissions"
input_postcode <- "Scottish_Postcode_Directory_2019_2"

########################################
#  Step 2 - set date ranges and any exlusions
#
########################################

# set date range to be selected
# set as from 1 March 2020 to 31st May 2020
key_date_start <- ymd(20200301)
key_date_end <- ymd(20200531)

#set date range to append to any output and check files 
# should match key dates above
date_range <- "20200301_to_20200531"
# set the date of extract file
extract_date <- "Extract2020June30"

#### set up definition lists 
# Set admission type codes used for an Emergency Admission
emergency_admission_type <- c(20,21,22,30,31,32,33,34,35,36,38,39)

# set the excluded specialty group
excluded_specialty_group <- '0 (OTHER)'

#set excluded specific specialties - only needed if not covered by excluding specialty group
#excluded_specialties <- c('G1', 'G3', 'G4', 'G5') 

# set any hospial codes to exclude
# excluded here G303H in Glasgow and W106H in Western Isles
excluded_hospitals <- c('G303H', 'W106H')

# set data suppression value - 
# counts to anonymised or suppressed of less than ?

data_suppress_value <- 5
##########################
# Step 3 - read in data
# Select only required fields
# Filter out un needed fields
# Add  fields for coding for emergency admissions and to enable count
# of observations (achieved by variable = 1 for each row - note for some unique rows
# in the files being read in the
# number of stays is > 1)
###########################

#### Read in data
# read in the daily admissions file - to end May
RAPID_admissions_daily <- read_csv(glue("{filepath_RAPID_In}/{input_RAPID}.csv"))

# check column names
colnames(RAPID_admissions_daily)

#rename and filter required variables
RAPID_admissions_keyvar <- RAPID_admissions_daily %>% 
  #rename columns with spaces that will be selectes
  rename(c(Number_of_Stays = "Number of Stays",
           Hospital_Treatment_Code = "Hospital of Treatment Location Code",
           Hospital_Treatment_Name = "Hospital of Treatment Location Name",
           HB_Residence = "Health Board of Residence Description - as at date of episode",
           HB_Treatment_Code = "Hospital of Treatment NHS Board Code - as at date of episode",
           HB_Treatment_Name = "Hospital of Treatment NHS Board Name - as at date of episode",
           Postcode_Residence = "Postcode [C]",
           Specialty_Group = "Specialty Group",
           Admission_Date = "Admission Date",
           Admission_Type = "Admission Type",
           Admission_Time = "Admission Time",
           Management_of_Patient = "Management of Patient")) %>% 
  #select required variables
  select(Number_of_Stays,
         Hospital_Treatment_Code,
         Hospital_Treatment_Name,
         HB_Residence,
         HB_Treatment_Code,
         HB_Treatment_Name,
         Postcode_Residence,
         Specialty,
         Specialty_Group,
         Admission_Date,
         Admission_Type,
         Admission_Time,
         Management_of_Patient) %>% 
  #add a formatted postcode field - to give give consistent 8 character postcode to
  # so can be used to join data from Scottish Postcode directory
  mutate(pc8 = postcode(Postcode_Residence, format = "pc8")) %>% 
  #- note uses phsmethods postcode function
  #add a date field for admission date
  mutate(Dt_Admission_Date = ymd_hms(Admission_Date)) %>% 
  #filter data from week ending 02 Mar 20 to weekending 31 May 20 to check
  filter(Dt_Admission_Date >= key_date_start & Dt_Admission_Date <= key_date_end)  %>% 
  ######### Add a field coding emergency admissions where an emergency is 1 and any other is 0
  mutate(Emergency_Flag = case_when(
    Admission_Type %in% emergency_admission_type ~ 1,
    TRUE ~ 0)
  ) %>% 
  mutate(Admission_Group = case_when(
    Emergency_Flag == 1 ~ "emergency",
    Emergency_Flag == 0 ~ "elective (planned)",
    TRUE ~ "other")
  ) %>% 
  # add a field set as 1 that can be used to sum number of observations
  mutate(Number_Observations = 1) %>% 
  # sort by admission date and admission time
  arrange(Admission_Date, Admission_Time)

#check column names of newly created file
colnames(RAPID_admissions_keyvar)

#check min and max stays (may be >1 for max) - and max and min observations (should be 1)
min_stays_all <- min(RAPID_admissions_keyvar$Number_of_Stays)
max_stays_all <- max(RAPID_admissions_keyvar$Number_of_Stays)  
min_obs <- min(RAPID_admissions_keyvar$Number_Observations)
max_obs <- max(RAPID_admissions_keyvar$Number_Observations)  



####produce sum - all stays V1 before filtering out specialties
sum_stays_all_spec <- sum(RAPID_admissions_keyvar$Number_of_Stays)

## if all as expected remove large intial file
rm(RAPID_admissions_daily)

#######################################
### STEP 4 - perform some checks
#
#
####################################

#check Emergency Admission coding

check_emergency_admit_coding_all <- RAPID_admissions_keyvar %>% 
  distinct(Emergency_Flag, Admission_Group, Admission_Type) %>% 
  arrange(Emergency_Flag, Admission_Type)

View(check_emergency_admit_coding_all)  

write.csv(check_emergency_admit_coding_all,
          (glue("{filepath_checks}/check_emergency_admit_coding_all_{date_range}_{extract_date}.csv")))

check_admission_group_all <- RAPID_admissions_keyvar %>% 
  group_by(Emergency_Flag, Admission_Group) %>% 
  arrange(Emergency_Flag) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum)

View(check_admission_group_all)

write.csv(check_admission_group_all,
          (glue("{filepath_checks}/check_admission_group_all_{date_range}_{extract_date}.csv")))

##### Exlcude any other specialties or hospitals here

#check specialty and grouping - before filter
check_specialty_coding_all <- RAPID_admissions_keyvar %>% 
  group_by(Specialty_Group,Specialty) %>% 
  arrange(Specialty_Group,Specialty) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum)

#View(check_specialty_coding_all)

#suppress values less than suppression value

check_specialty_coding_all_S <- check_specialty_coding_all %>% 
  mutate(Number_Observations = case_when(
    Number_Observations < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_Observations)
  )) %>% 
  mutate(Number_of_Stays = case_when(
    Number_of_Stays < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_of_Stays)
  ))
  

View(check_specialty_coding_all_S)


write.csv(check_specialty_coding_all_S,
          (glue("{filepath_checks}/check_specialty_coding_all_S_{date_range}_{extract_date}.csv")))

### Assuming G1 to 4 codes currently coded as other 
#- then only need to exlcude Specialty Group 0

#check distinct hospital codes
check_distinct_hospitals_all <- RAPID_admissions_keyvar %>% 
  group_by(HB_Treatment_Code, Hospital_Treatment_Code,Hospital_Treatment_Name) %>% 
  arrange(HB_Treatment_Code, Hospital_Treatment_Code) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum)
#View(check_distinct_hospitals_all)

#suppress values less than suppression value

check_distinct_hospitals_all_S <- check_distinct_hospitals_all %>% 
  mutate(Number_Observations = case_when(
    Number_Observations < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_Observations)
  )) %>% 
  mutate(Number_of_Stays = case_when(
    Number_of_Stays < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_of_Stays)
  ))


#View(check_distinct_hospitals_all_S)


write.csv(check_distinct_hospitals_all_S,
          (glue("{filepath_checks}/check_distinct_hospitals_all_S_{date_range}_{extract_date}.csv")))


#filter out chosen specialty group - excluded hospitals here and - check totals
check_admission_group_filtered <- RAPID_admissions_keyvar %>% 
  filter(!Specialty_Group %in% excluded_specialty_group) %>% 
  filter(!Hospital_Treatment_Code %in% excluded_hospitals & Hospital_Treatment_Code !="") %>% 
  group_by(Emergency_Flag, Admission_Group) %>% 
  arrange(Emergency_Flag) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum)

View(check_admission_group_filtered)

write.csv(check_admission_group_filtered,
          (glue("{filepath_checks}/check_admission_group_filtered_{date_range}_{extract_date}.csv")))

#########################
### STEP 5 - produced filtered dataset to aggregate to IG for emergency admissions only
#   (You may wish to look at check files to see if performing as expected before you do this)
###############

 RAPID_admissions_filtered <- RAPID_admissions_keyvar %>% 
  filter(!Specialty_Group %in% excluded_specialty_group & 
           !Hospital_Treatment_Code %in% excluded_hospitals & 
           Hospital_Treatment_Code !="" &
           Emergency_Flag == 1)
  
#### Check the number of admissions matches the previous filter
Total_Emergency_Admits_Filtered_Check <- check_admission_group_filtered %>% 
  filter(Emergency_Flag == 1) %>% 
  select(Number_Observations)

Total_Emergency_Admits_Filtered_Check <- Total_Emergency_Admits_Filtered_Check$Number_Observations

#clear unfiltered data from environment
rm(RAPID_admissions_keyvar)

############
# STEP 6  
#Load the postcode file with relevant variables only and join to filtered data
# remove after joining
############

PCFile <- read_rds(glue("{filepath_postcodes}/{input_postcode}.rds")) %>% 
  select(pc8, IntZone2011, IntZone2011Name, DataZone2011, DataZone2011Name,
         HB2019, HB2019Name, HB2018, 
         Latitude, Longitude, 
         Grid_Reference_Easting, Grid_Reference_Northing
         )
#check have correct columns
colnames(PCFile)


#### joins onto RAPID admission filtered

RAPID_admissions_filtered_wPC <- RAPID_admissions_filtered %>% 
  left_join(PCFile, by = "pc8") %>% 
  mutate(No_Match_IG = case_when(
    is.na(IntZone2011) ~ 1,
    TRUE ~ 0)
  )

#check column names for new df
colnames(RAPID_admissions_filtered_wPC)

#View(RAPID_admissions_filtered_wPC)

# remove the postcode file and the admission file without IGs
rm(PCFile)
rm(RAPID_admissions_filtered)

#####
##Step 7 run some checks for how data has matched - and unmatched data
#
### 

#Check total observations - and total where no IG

check_admission_with_IG <- RAPID_admissions_filtered_wPC %>% 
    summarise_at(vars(Number_Observations,No_Match_IG),sum) %>% 
    mutate(Observations_Matched_withIntZone2011 = Number_Observations - No_Match_IG) %>% 
    rename(All_Observations = Number_Observations, 
           Observations_No_Match_IntZone2011 = No_Match_IG) 
      

View(check_admission_with_IG)

write.csv(check_admission_with_IG,
          (glue("{filepath_checks}/check_admission_withIG_{date_range}_{extract_date}.csv")))

# check what data looks like aggregated to health board level

# create a distinct list of all HB of Residecne in this file
HB_Residence_List <- RAPID_admissions_filtered_wPC %>%
  distinct(HB_Residence) %>% 
  arrange(HB_Residence)

# check HB_Residence coding for unmatched IG data
check_HB_Residence_NoIG <- RAPID_admissions_filtered_wPC %>% 
  filter(No_Match_IG == 1) %>% 
  group_by(HB_Residence) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum) %>% 
  # joins on the HB Residence list to makes sure any HB without unmatched IG are included
  full_join(HB_Residence_List) %>% 
  # suppress values below data suppression value or where NA ie all IG matched
  mutate(Number_Observations = case_when(
    Number_Observations < data_suppress_value ~ '*' ,
    is.na(Number_Observations) ~ '*' , 
    TRUE ~ as.character(Number_Observations)
  )) %>% 
  mutate(Number_of_Stays = case_when(
    Number_of_Stays < data_suppress_value ~ '*' ,
    is.na(Number_of_Stays)  ~ '*' ,
    TRUE ~ as.character(Number_of_Stays)
  )) %>% 
  arrange(HB_Residence)


#View(check_HB_Residence_NoIG)  

write.csv(check_HB_Residence_NoIG,
          (glue("{filepath_checks}/check_HB_Residence_NoIG_{date_range}_{extract_date}.csv")))
  
### Check Health Board level data quality (versus treatment and versus PCFile Coding
#and write out check files

check_HB_Residence_HB_Treatment_RAPID_EmergencyAdmissions <- RAPID_admissions_filtered_wPC %>% 
  group_by(HB_Residence, HB_Treatment_Code, HB_Treatment_Name) %>% 
  arrange(HB_Residence) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum) %>% 
  # suppress values below data suppression value
  mutate(Number_Observations = case_when(
    Number_Observations < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_Observations)
  )) %>% 
  mutate(Number_of_Stays = case_when(
    Number_of_Stays < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_of_Stays)
  ))

#View(check_HB_Residence_HB_Treatment_RAPID_EmergencyAdmissions)


write.csv(check_HB_Residence_HB_Treatment_RAPID_EmergencyAdmissions,
          (glue("{filepath_checks}/check_HB_Residence_HB_Treatment_{date_range}_{extract_date}.csv")))

check_HB_Residence_HB_PCfilesCodes_EmergencyAdmissions <- RAPID_admissions_filtered_wPC %>% 
  group_by(HB_Residence,HB2018, HB2019, HB2019Name) %>% 
  arrange(HB_Residence) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum) %>% 
  # suppress values below data suppression value
  mutate(Number_Observations = case_when(
    Number_Observations < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_Observations)
  )) %>% 
  mutate(Number_of_Stays = case_when(
    Number_of_Stays < data_suppress_value ~ '*' ,
    TRUE ~ as.character(Number_of_Stays)
  ))

#View(check_HB_Residence_HB_PCfilesCodes_EmergencyAdmissions)

write.csv(check_HB_Residence_HB_PCfilesCodes_EmergencyAdmissions,
          (glue("{filepath_checks}/check_HB_Residence_HB_PCfilesCodes_{date_range}_{extract_date}.csv")))

#######
# Aggregate to IG level and write out to file in outputs directory
#
########

#### Aggregate to IG level  - adding a check variable for where below data suppress level

IntZone2011_RAPID_EmergencyAdmissions <- RAPID_admissions_filtered_wPC %>% 
  group_by(IntZone2011, IntZone2011Name) %>% 
  arrange(IntZone2011) %>% 
  summarise_at(vars(Number_Observations,Number_of_Stays),sum) %>% 
  mutate(Number_Observations_Below5_Flag = case_when(
    Number_Observations < data_suppress_value ~ 1 ,
    TRUE ~ 0
  )) %>% 
  mutate(Number_of_Stays_Below5_Flag = case_when(
    Number_of_Stays < data_suppress_value ~ 1 ,
    TRUE ~ 0
  ))

#View(IntZone2011_RAPID_EmergencyAdmissions)

# quick check - what is the range of values for observations and stays
# ? any data suppression likely needed ?
Summary_Observations_IG <- summary(IntZone2011_RAPID_EmergencyAdmissions$Number_Observations)
Summary_Observations_IG 

Summary_Stays_IG <- summary(IntZone2011_RAPID_EmergencyAdmissions$Number_of_Stays)
Summary_Stays_IG 

# write out file
write.csv(IntZone2011_RAPID_EmergencyAdmissions,
          (glue("{filepath_outputs}/IntZone2011_RAPID_EmergencyAdmissions_{date_range}.csv")))



