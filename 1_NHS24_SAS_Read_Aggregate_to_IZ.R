#### Script produced with R3.6.1 (64 bit)

#####  in Rstudio 1.1.456 

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 22 July 2020 

### Partly Based on script by Cecilia Puech, Place and Wellbeing, 
#Public Health Scotland

### Run time on R studio server - approximately 2 minutes

# Script reads in pre-aggregated to postcode level and for 
#a pre-specified time period data supplied for NHS24 and SAS

#########################
### Step 1
### Set libraries and paths
##

#### load libraries - if not present please install packages

library(readr)
#library(readxl)
library(dplyr)

#code to install phsmethods
#note requires library(remotes)
#remotes::install_github("Health-SocialCare-Scotland/phsmethods", upgrade = "never")

library(phsmethods) 

#library(lubridate)
library(glue)

library(tidyr)

####check the current working directory if needed
getwd()

### Set filepaths for inputs, checks and final outputs, and any key file names
filepath_data <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/data/ucd")
filepath_checks <-glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/checks/nhs24_sas")
filepath_outputs <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs/nhs24_sas")
# check this has not changed
filepath_postcodes <- glue(
  "/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory")
filepath_lookups <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/lookups")

input_SAST1 <- "TimePeriod1SAS"
input_SAST2 <- "TimePeriod2SAS"

input_NHS24T1 <- "TimePeriod1NHS24"
input_NHS24T2 <- "TimePeriod2NHS24"

# may need to check this is most recent
input_postcode <- "Scottish_Postcode_Directory_2020_1"

input_IZ_Lookups <- "int_names_lookup"

########################################
#  Step 2 - set date ranges and any exlusions
#
####

# set date range to be selected
# Not needed here as data pre-aggregated for date range
#key_date_start <- ymd(20200301)
#key_date_end <- ymd(20200531)

#set date range to append to any output and check files 
# should match key dates above
date_rangeT1 <- "20190301_to_20190531"
date_rangeT2 <- "20200301_to_20200531"
date_range_months <- "March_May"
date_range_years <- "2020_2019"
# set the date of extract file
extract_date <- "Extract2020July21"

#### set up definition lists 

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
##

## Read in data
# read in SAS and NHS 24 data
SAST1 <- read_csv(glue("{filepath_data}/{input_SAST1}.csv"))  %>% 
  rename(c(sas_attendances_2019 = "Number of Incidents",
           patient_postcode = "Patient Postcode [C]"
           )) %>% 
  #add a formatted postcode field - to give give consistent 8 character postcode to
  # so can be used to join data from Scottish Postcode directory
  mutate(pc8 = postcode(patient_postcode, format = "pc8"))

  # check column names
  colnames(SAST1)
  

SAST2 <- read_csv(glue("{filepath_data}/{input_SAST2}.csv")) %>% 
  rename(c(sas_attendances_2020 = "Number of Incidents",
           patient_postcode = "Patient Postcode [C]"
  )) %>% 
  #add a formatted postcode field - to give give consistent 8 character postcode to
  # so can be used to join data from Scottish Postcode directory
  mutate(pc8 = postcode(patient_postcode, format = "pc8"))

# check column names
colnames(SAST2)


NHS24T1 <- read_csv(glue("{filepath_data}/{input_NHS24T1}.csv")) %>% 
  rename(c(completed_calls_nhs24_2019 = "Number of NHS 24 Records",
           patient_postcode = "Patient Postcode [C]"
  )) %>% 
  #add a formatted postcode field - to give give consistent 8 character postcode to
  # so can be used to join data from Scottish Postcode directory
  mutate(pc8 = postcode(patient_postcode, format = "pc8"))


# check column names
colnames(NHS24T1)

NHS24T2 <- read_csv(glue("{filepath_data}/{input_NHS24T2}.csv")) %>% 
  rename(c(completed_calls_nhs24_2020 = "Number of NHS 24 Records",
           patient_postcode = "Patient Postcode [C]"
  )) %>% 
  #add a formatted postcode field - to give give consistent 8 character postcode to
  # so can be used to join data from Scottish Postcode directory
  mutate(pc8 = postcode(patient_postcode, format = "pc8"))

# check column names
colnames(NHS24T2)

#####
#STEP 4 run some checks
#
###

#check high postcodes
high_pc_SAST1 <- SAST1 %>% 
  filter(sas_attendances_2019 > 50) %>% 
  arrange(desc(sas_attendances_2019))

high_pc_SAST2 <- SAST2 %>% 
  filter(sas_attendances_2020 > 50) %>% 
  arrange(desc(sas_attendances_2020))


high_pc_NHS24T1 <- NHS24T1 %>% 
  filter(completed_calls_nhs24_2019 > 50) %>% 
  arrange(desc(completed_calls_nhs24_2019))

high_pc_NHS24T2 <- NHS24T2 %>% 
  filter(completed_calls_nhs24_2020 > 50) %>% 
  arrange(desc(completed_calls_nhs24_2020))


# create and save total observations and number of null postcodes

total_SAST1 <- SAST1 %>% 
  summarise_at(vars(sas_attendances_2019), sum) %>% 
  mutate(Summary = "Total")
  
no_postcode_SAST1 <- SAST1 %>% 
  filter(is.na(pc8)) %>% 
  select(sas_attendances_2019) %>% 
  mutate(Summary = "Postcode_is_null")


summary_SAST1 <- bind_rows(no_postcode_SAST1, total_SAST1)
rm(total_SAST1,no_postcode_SAST1)

total_SAST2 <- SAST2 %>% 
  summarise_at(vars(sas_attendances_2020), sum) %>% 
  mutate(Summary = "Total")

no_postcode_SAST2 <- SAST2 %>% 
  filter(is.na(pc8)) %>% 
  select(sas_attendances_2020) %>% 
  mutate(Summary = "Postcode_is_null")

summary_SAST2 <- bind_rows(no_postcode_SAST2, total_SAST2)
rm(total_SAST2,no_postcode_SAST2)

total_NHS24T1 <- NHS24T1 %>% 
  summarise_at(vars(completed_calls_nhs24_2019), sum) %>% 
  mutate(Summary = "Total")

no_postcode_NHS24T1 <- NHS24T1 %>% 
  filter(is.na(pc8)) %>% 
  select(completed_calls_nhs24_2019) %>% 
  mutate(Summary = "Postcode_is_null")

summary_NHS24T1 <- bind_rows(no_postcode_NHS24T1, total_NHS24T1)
rm(total_NHS24T1,no_postcode_NHS24T1)

total_NHS24T2 <- NHS24T2 %>% 
  summarise_at(vars(completed_calls_nhs24_2020), sum) %>% 
  mutate(Summary = "Total")

no_postcode_NHS24T2 <- NHS24T2 %>% 
  filter(is.na(pc8)) %>% 
  select(completed_calls_nhs24_2020) %>% 
  mutate(Summary = "Postcode_is_null")

summary_NHS24T2 <- bind_rows(no_postcode_NHS24T2, total_NHS24T2)
rm(total_NHS24T2,no_postcode_NHS24T2)

Totals_NullPostcodes_NH24_SAS <- bind_cols(
  summary_SAST1,
  summary_SAST2,
  summary_NHS24T1,
  summary_NHS24T2) %>% 
  select(Summary= Summary...2,
    sas_attendances_2019,
    sas_attendances_2020,
    completed_calls_nhs24_2019,
    completed_calls_nhs24_2020
  )

Totals_NullPostcodes_NH24_SAS_Pivot <- Totals_NullPostcodes_NH24_SAS %>% 
  pivot_longer(cols = 2:5,values_to = "a" ) %>% 
  pivot_wider(names_from = Summary, values_from = a) %>% 
  mutate(Percent_null_postcode = (Postcode_is_null/Total)*100)
View(Totals_NullPostcodes_NH24_SAS_Pivot)

write.csv(Totals_NullPostcodes_NH24_SAS_Pivot,
          (glue("{filepath_checks}/Totals_NullPostcodes_NH24_SAS_{date_range_years}{date_range_months}_{extract_date}.csv")))

rm(summary_NHS24T1, summary_NHS24T2, summary_SAST1, summary_SAST2)

############
# STEP 5  
#Load the postcode file with relevant variables only and join to filtered data
# remove after joining
############

PCFile <- read_rds(glue("{filepath_postcodes}/{input_postcode}.rds")) %>% 
  select(pc8, intzone2011)
#check have correct columns
colnames(PCFile)

## read in the IZ lookups file

IntZone2011_Lookup <- read_csv(glue(
  "{filepath_lookups}/{input_IZ_Lookups}.csv")) %>% 
  rename (intzone2011 = InterZone,
          intzone2011_name = Name) 

colnames(IntZone2011_Lookup)


#### join both onto NHS24 and SAS files

SAST1_wPC <- SAST1 %>% 
  left_join(PCFile, by = "pc8") %>% 
  mutate(No_Match_IG = case_when(
    is.na(intzone2011) ~ 1,
    TRUE ~ 0)
  ) %>%
  left_join(IntZone2011_Lookup, by = "intzone2011")

SAST2_wPC <- SAST2 %>% 
  left_join(PCFile, by = "pc8") %>% 
  mutate(No_Match_IG = case_when(
    is.na(intzone2011) ~ 1,
    TRUE ~ 0)
  ) %>%
  left_join(IntZone2011_Lookup, by = "intzone2011")

NHS24T1_wPC <- NHS24T1 %>% 
  left_join(PCFile, by = "pc8") %>% 
  mutate(No_Match_IG = case_when(
    is.na(intzone2011) ~ 1,
    TRUE ~ 0)
  ) %>%
  left_join(IntZone2011_Lookup, by = "intzone2011")

NHS24T2_wPC <- NHS24T2 %>% 
  left_join(PCFile, by = "pc8") %>% 
  mutate(No_Match_IG = case_when(
    is.na(intzone2011) ~ 1,
    TRUE ~ 0)
  ) %>%
  left_join(IntZone2011_Lookup, by = "intzone2011")


# remove the postcode file
rm(PCFile)
# remove uneeds original loaded files
rm(NHS24T1, NHS24T2, SAST1, SAST2)

#####
##STEP 6 run some checks for how data has matched - and unmatched data
#
### 

#Check total observations - and total where no IG - and write out to a check file

check_total_SAST1_with_IG <- SAST1_wPC %>%
  mutate(no_match = sas_attendances_2019*No_Match_IG) %>% 
  summarise_at(vars(sas_attendances_2019,no_match),sum) %>% 
  rename(all = sas_attendances_2019) %>%   
  mutate(matched = all - no_match) %>% 
  mutate(perc_no_match = (no_match/all)*100) %>% 
  mutate(data_source = "SAST1")

check_total_SAST2_with_IG <- SAST2_wPC %>%
  mutate(no_match = sas_attendances_2020*No_Match_IG) %>% 
  summarise_at(vars(sas_attendances_2020,no_match),sum) %>% 
  rename(all = sas_attendances_2020) %>%   
  mutate(matched = all - no_match) %>% 
  mutate(perc_no_match = (no_match/all)*100) %>% 
  mutate(data_source = "SAST2")

check_total_NHS24T1_with_IG <- NHS24T1_wPC %>%
  mutate(no_match = completed_calls_nhs24_2019*No_Match_IG) %>% 
  summarise_at(vars(completed_calls_nhs24_2019,no_match),sum) %>% 
  rename(all = completed_calls_nhs24_2019) %>%   
  mutate(matched = all - no_match) %>% 
  mutate(perc_no_match = (no_match/all)*100) %>% 
  mutate(data_source = "NHS24T1")

check_total_NHS24T2_with_IG <- NHS24T2_wPC %>%
  mutate(no_match = completed_calls_nhs24_2020*No_Match_IG) %>% 
  summarise_at(vars(completed_calls_nhs24_2020,no_match),sum) %>% 
  rename(all = completed_calls_nhs24_2020) %>%   
  mutate(matched = all - no_match) %>% 
  mutate(perc_no_match = (no_match/all)*100) %>% 
  mutate(data_source = "NHS24T2")

check_SAS_NHS24_with_IG <- bind_rows(
  check_total_SAST1_with_IG,
  check_total_SAST2_with_IG,
  check_total_NHS24T1_with_IG,
  check_total_NHS24T2_with_IG
)

write.csv(check_SAS_NHS24_with_IG,
          (glue("{filepath_checks}/check_SAS_NHS24_with_IG_{date_rangeT1}_{extract_date}.csv")))



#######
# STEP 7 Aggregate to IG level and write out to file in outputs directory
#
########

#### Aggregate to IG level  - adding a check variable for where below data suppress level

IntZone2011_SAS_Attendances_T1 <- SAST1_wPC %>% 
  group_by(intzone2011, intzone2011_name) %>% 
  arrange(intzone2011) %>% 
  summarise_at(vars(sas_attendances_2019),sum) %>% 
  mutate(sas_attend_2019_below5 = case_when(
    sas_attendances_2019 < data_suppress_value ~ 1 ,
    TRUE ~ 0
  )) 


# quick check - what is the range of values for observations and stays
# ? any data suppression likely needed ?
Summary_SAS_2019_IG <- summary(IntZone2011_SAS_Attendances_T1$sas_attendances_2019)
Summary_SAS_2019_IG


# write out file
write.csv(IntZone2011_SAS_Attendances_T1,
          (glue("{filepath_outputs}/IntZone2011_SAS_Attendances_{date_rangeT1}.csv")))


IntZone2011_SAS_Attendances_T2 <- SAST2_wPC %>% 
  group_by(intzone2011, intzone2011_name) %>% 
  arrange(intzone2011) %>% 
  summarise_at(vars(sas_attendances_2020),sum) %>% 
  mutate(sas_attend_2020_below5 = case_when(
    sas_attendances_2020 < data_suppress_value ~ 1 ,
    TRUE ~ 0
  )) 


# quick check - what is the range of values for observations and stays
# ? any data suppression likely needed ?
Summary_SAS_2020_IG <- summary(IntZone2011_SAS_Attendances_T2$sas_attendances_2020)
Summary_SAS_2020_IG


# write out file
write.csv(IntZone2011_SAS_Attendances_T2,
          (glue("{filepath_outputs}/IntZone2011_SAS_Attendances_{date_rangeT2}.csv")))

IntZone2011_NHS24_Completed_Calls_T1 <- NHS24T1_wPC %>% 
  group_by(intzone2011, intzone2011_name) %>% 
  arrange(intzone2011) %>% 
  summarise_at(vars(completed_calls_nhs24_2019),sum) %>% 
  mutate(completed_calls_nhs24_2019_below5 = case_when(
    completed_calls_nhs24_2019 < data_suppress_value ~ 1 ,
    TRUE ~ 0
  )) 

# quick check - what is the range of values for observations and stays
# ? any data suppression likely needed ?
Summary_NHS242019_IG <- summary(IntZone2011_NHS24_Completed_Calls_T1$completed_calls_nhs24_2019)
Summary_NHS242019_IG


# write out file
write.csv(IntZone2011_NHS24_Completed_Calls_T1,
          (glue("{filepath_outputs}/IntZone2011_NHS24_Completed_Calls_{date_rangeT1}.csv")))

IntZone2011_NHS24_Completed_Calls_T2 <- NHS24T2_wPC %>% 
  group_by(intzone2011, intzone2011_name) %>% 
  arrange(intzone2011) %>% 
  summarise_at(vars(completed_calls_nhs24_2020),sum) %>% 
  mutate(completed_calls_nhs24_2020_below5 = case_when(
    completed_calls_nhs24_2020 < data_suppress_value ~ 1 ,
    TRUE ~ 0
  )) 

# quick check - what is the range of values for observations and stays
# ? any data suppression likely needed ?
Summary_NHS242020_IG <- summary(IntZone2011_NHS24_Completed_Calls_T2$completed_calls_nhs24_2020)
Summary_NHS242020_IG


# write out file
write.csv(IntZone2011_NHS24_Completed_Calls_T2,
          (glue("{filepath_outputs}/IntZone2011_NHS24_Completed_Calls_{date_rangeT2}.csv")))
