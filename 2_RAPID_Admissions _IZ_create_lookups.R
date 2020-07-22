#### Script produced with R3.6.1

#####  in Rstudio Server

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 24 June 2020 

### Partly Based on script by Cecilia Puech, Place and Wellbeing,
# 
#Public Health Scotland

## 

# This script creates necessary look up files and map_markers as rds

#########################
### Step 1
### Set libraries and paths
###########################

#### load libraries - if not present please install packages


library(dplyr)
library(readr)

library(glue)


####check the current working directory if needed
#getwd()

### Set filepaths for inputs, checks and final outputs, and any key file names
filepath_lookups <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/lookups")


# current intermediate zone 2011 labels with health board and hscp
# note this link / and / or file name may change - check here
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels and right
# click on the download option to get current link
# files in csv format
filepath_izlookup <- glue("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/e3e885cc-2530-4b3c-bead-9eda9782264f/download")
input_izlookup <- "iz2011_codes_and_labels_21042020.csv"



# current hospital open data
# note this link / and / or file name may change - check here
# https://www.opendata.nhs.scot/dataset/hospital-codes and right
# click on the download option to get current link
# files in csv format

filepath_hosp_current <- glue(
  "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download")
# note this file is updated regularly
input_hosp_current <- "current_nhs_hospitals_in_scotland_160620.csv"

# current  A&E site list from open data
# note this link / and / or file name may change - check here
# https://www.opendata.nhs.scot/dataset/nhs-scotland-accident-emergency-sites and  choose 
# the explore option to get current link

filepath_hosp_ae <- glue(
  "https://www.opendata.nhs.scot/dataset/a877470a-06a9-492f-b9e8-992f758894d0/resource/1a4e3f48-3d9b-4769-80e9-3ef6d27852fe/download")
input_hosp_ae <- "hospital_site_list.csv"

# Scottish Postcode directory
filepath_SPD <- glue(
"/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/")
input_SPD <- "Scottish_Postcode_Directory_2019_2"



##Create lookups

# intermediate zone 2011 lookup table with code, name, 
int_names_lookup <- read_csv(glue("{filepath_izlookup}/{input_izlookup}")) %>% 
  select(InterZone = IntZone, Name = IntZoneName, HB, HBName)


write_csv(int_names_lookup,
          (glue("{filepath_lookups}/int_names_lookup.csv")))


###### Read in postcode directory to get AE sites latitude, longitude

SPD <- read_csv(glue("{filepath_SPD}/{input_SPD}.csv")) %>% 
  select(pc7, Latitude, Longitude) %>% 
  mutate(Postcode = gsub(" ", "", pc7))


##Get list of A&E sites
AE <- read_csv(glue("{filepath_hosp_ae}/{input_hosp_ae}")) %>% 
  filter(Status == "Open") %>% 
  select(TreatmentLocationName, Location = TreatmentLocationCode, CurrentDepartmentType)

hospital_lkp <- read_csv(glue("{filepath_hosp_current}/{input_hosp_current}")) %>% 
  select(1:3)

#Get marker coordinates with SPD
AE_markers <- left_join(AE, hospital_lkp) %>% 
  mutate(Postcode = gsub(" ", "", Postcode)) %>% 
  select(2:5)%>% 
  left_join(SPD, by = "Postcode") %>% 
  filter(CurrentDepartmentType == "Emergency Department")


# write out necessary files 

write_csv(AE_markers,
          (glue("{filepath_lookups}/AE_markers.csv")))