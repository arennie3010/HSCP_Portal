#### Script produced with R3.6.1

#####  in Rstudio Server

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 30 June 2020 

### Partly Based on script by Cecilia Puech, Place and Wellbeing, 
#Public Health Scotland

## Runtime approximately 2 minutes

#########################
### Step 1
### Set libraries and paths
###########################

#### load libraries - if not present please install packages

library(readr)
#library(readxl)
library(dplyr)

library(glue)


####check the current working directory if needed
#getwd()

### Set filepaths for inputs, checks and final outputs, and any key file names
filepath_RAPID_IZ_In <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs")
filepath_lookups <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/lookups")
filepath_checks <-glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/checks")
filepath_outputs <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs")
filepath_IZPops <- ("/conf/linkage/output/lookups/Unicode/Populations/Estimates/")  

 
input_IZ_2020 <- "IntZone2011_RAPID_EmergencyAdmissions_20200301_to_20200531"
input_IZ_2019 <- "IntZone2011_RAPID_EmergencyAdmissions_20190301_to_20190531"
input_IZ_Lookups <- "int_names_lookup"
input_IZ_Pops <- "IntZone2011_pop_est_2011_2018"

#set date range to append to any output files
date_range <- "2020_2019_March_to_May"


####### STEP 1 - read in required files to create wide file

# read in the two IZ files adding relevant years to variable names
IZ_2020 <- read_csv(glue("{filepath_RAPID_IZ_In}/{input_IZ_2020}.csv")) %>% 
  select(IntZone2011, 
         Number_Observations, 
         Number_of_Stays, 
         Number_Observations_Below5_Flag,
         Number_of_Stays_Below5_Flag) %>% 
  rename(
    Number_Observations_2020 = Number_Observations,
    Number_of_Stays_2020 = Number_of_Stays,
    Number_Observations_Below5_Flag_2020 = Number_Observations_Below5_Flag ,
    Number_Stays_Below5_Flag_2020 = Number_of_Stays_Below5_Flag 
  )

colnames(IZ_2020)

IZ_2019 <- read_csv(glue("{filepath_RAPID_IZ_In}/{input_IZ_2019}.csv")) %>% 
  select(IntZone2011, 
         Number_Observations, 
         Number_of_Stays, 
         Number_Observations_Below5_Flag,
         Number_of_Stays_Below5_Flag) %>% 
  rename(
    Number_Observations_2019 = Number_Observations,
    Number_of_Stays_2019 = Number_of_Stays,
    Number_Observations_Below5_Flag_2019 = Number_Observations_Below5_Flag ,
    Number_Stays_Below5_Flag_2019 = Number_of_Stays_Below5_Flag 
  )

colnames(IZ_2019)


IZ_2020_2019 <- IZ_2020 %>% 
  full_join(IZ_2019, by = "IntZone2011")

colnames(IZ_2020_2019)

rm(IZ_2019, IZ_2020)


###### STEP 2 - write out a summary file to the checks folder


#select and write out the unmatched data

IZ2020_2019_Unmatched <- IZ_2020_2019 %>% 
  filter(is.na(IntZone2011)) %>% 
  select(IntZone2011,
         Number_Observations_2019,
         Number_Observations_2020,
         Number_of_Stays_2019,
         Number_of_Stays_2020
         )

#View(IZ2020_2019_Unmatched)

IZ2020_2019_Totals <- IZ_2020_2019 %>% 
    summarise_at(vars(Number_Observations_2019, Number_Observations_2020, 
                      Number_of_Stays_2019, Number_of_Stays_2020),
         sum) %>% 
  mutate(IntZone2011 = "Total_Emergency_Admissions")
  

IZ2020_2019_Summary <- bind_rows(IZ2020_2019_Totals, IZ2020_2019_Unmatched)

write.csv(IZ2020_2019_Summary,
          (glue("{filepath_checks}/IZ2020_2019_Summary_{date_range}.csv")))

rm(IZ2020_2019_Totals, IZ2020_2019_Unmatched)

##### STEP 3 Join to the Int_names_lookup file (will discard any NA)

#### read in IntZone Lookup file

IntZone2011_Lookup <- read_csv(glue(
  "{filepath_lookups}/{input_IZ_Lookups}.csv")) %>% 
  rename (IntZone2011 = InterZone,
          IntZone2011_Name = Name) 

colnames(IntZone2011_Lookup)


IZ2020_2019_Join_to_Lookup_File <- IntZone2011_Lookup %>% 
  left_join(IZ_2020_2019, by = "IntZone2011") 

colnames(IZ2020_2019_Join_to_Lookup_File)

IZ2020_2019_IZ_with_Obs_below5 <- IZ2020_2019_Join_to_Lookup_File %>% 
  summarise_at(vars(Number_Observations_Below5_Flag_2019, 
                    Number_Observations_Below5_Flag_2020, 
                    Number_Stays_Below5_Flag_2019, 
                    Number_Stays_Below5_Flag_2020),
               sum)

View(IZ2020_2019_IZ_with_Obs_below5 )

write.csv(IZ2020_2019_IZ_with_Obs_below5,
          (glue("{filepath_checks}/IZ2020_2019_IZ_with_Obs_below5_{date_range}.csv")))


#### STEP 4 produce long format of data for stays only

IZ_2020_L <- read_csv(glue("{filepath_RAPID_IZ_In}/{input_IZ_2020}.csv")) %>% 
  select(IntZone2011, Number_of_Stays) %>% 
  rename(number_emergency_admissions = Number_of_Stays) %>% 
  mutate(year = 2020)

# join to IZ Lookup file to be sure have all IZs - plus removes unmatched data
IZ_2020_Full_L <- IntZone2011_Lookup %>% 
  left_join(IZ_2020_L, by = "IntZone2011")

colnames(IZ_2020_Full_L)

IZ_2019_L <- read_csv(glue("{filepath_RAPID_IZ_In}/{input_IZ_2019}.csv")) %>% 
  select(IntZone2011, Number_of_Stays) %>% 
  rename(number_emergency_admissions = Number_of_Stays) %>% 
  mutate(year = 2019)

# join to IZ Lookup file to be sure have all IZs  
IZ_2019_Full_L <- IntZone2011_Lookup %>% 
  left_join(IZ_2019_L, by = "IntZone2011")

colnames(IZ_2019_Full_L)


IZ_2020_2019L <- bind_rows(IZ_2019_Full_L, IZ_2020_Full_L)

col_order <- c("IntZone2011", "IntZone2011_Name", 
               "HB", "HBName", 
               "year", "number_emergency_admissions")

IZ_2020_2019L <- IZ_2020_2019L[, col_order] %>% 
  arrange(IntZone2011)

View(IZ_2020_2019L) 

###

rm(IZ_2019_Full_L, IZ_2019_L, IZ_2020_Full_L, IZ_2020_L)



### STEP 5A
#### Create wide file with required variables, calculate rates and difference - write out

IZ2011_Emergency_Admissions <- IZ2020_2019_Join_to_Lookup_File %>% 
  select(IntZone2011, IntZone2011_Name, HB, HBName,  
         Number_Observations_2019, Number_Observations_2020, 
         Number_of_Stays_2019, Number_of_Stays_2020)
  

colnames(IZ2011_Emergency_Admissions)


# read in the population file
IZPops2018 <-readRDS(glue("{filepath_IZPops}/{input_IZ_Pops}.rds")) %>% 
  rename(IntZone2011 = intzone2011)
# group to give totals only
IZPops2018 <- IZPops2018 %>%
  group_by(year, IntZone2011) %>%
  summarise(Total_Pop_IZ_2018 = sum(total_pop))%>%
  ungroup()
#keep 2018 only
IZPops2018 <-filter(IZPops2018,year==2018) %>% 
  select(IntZone2011, Total_Pop_IZ_2018)

View(IZPops2018)

# join population file to IZ wide file and create rates
IZ2011_Emergency_Admissions <- IZ2011_Emergency_Admissions %>% 
  left_join(IZPops2018, by = "IntZone2011") %>% 
  mutate(Obs_rate_per_1000_pop_2019 = (Number_Observations_2019/Total_Pop_IZ_2018)*1000) %>% 
  mutate(Obs_rate_per_1000_pop_2020 = (Number_Observations_2020/Total_Pop_IZ_2018)*1000) %>% 
  mutate(Stays_rate_per_1000_pop_2019 = (Number_of_Stays_2019/Total_Pop_IZ_2018)*1000) %>% 
  mutate(Stays_rate_per_1000_pop_2020 = (Number_of_Stays_2020/Total_Pop_IZ_2018)*1000) %>% 
  mutate(perc.diff.obs = ((Obs_rate_per_1000_pop_2020 - Obs_rate_per_1000_pop_2019)
                          /Obs_rate_per_1000_pop_2019)*100) %>% 
  mutate(perc.diff.stays = ((Stays_rate_per_1000_pop_2020 - Stays_rate_per_1000_pop_2019)
                            /Stays_rate_per_1000_pop_2019)*100)
  

View(IZ2011_Emergency_Admissions)


write.csv(IZ2011_Emergency_Admissions,
          (glue("{filepath_outputs}/IZ2011_RAPID_Emergency_Admissions_{date_range}.csv")))


#join population file to IZ long file and create rates
IZ2011_Emergency_Admissions_L <- IZ_2020_2019L %>% 
  left_join(IZPops2018, by = "IntZone2011") %>% 
  mutate(rate = (number_emergency_admissions/Total_Pop_IZ_2018)*1000)
  
IZ2011_Emergency_Admissions_L <- IZ2011_Emergency_Admissions_L %>% 
  group_by(IntZone2011) %>% 
  mutate(perc.diff= (rate - lag(rate))/lag(rate)*100) %>% 
  ungroup()

View(IZ2011_Emergency_Admissions_L)

write.csv(IZ2011_Emergency_Admissions_L,
          (glue("{filepath_outputs}/IZ2011_RAPID_Emergency_Admissions_Long_{date_range}.csv")))



#### Step 6 - write out summary file - from wide data
####summarise minimum and maximum numbers, Rates, and change in final IZ file

IZ2011_Emergency_Admit_Min <- IZ2011_Emergency_Admissions %>% 
  summarise_at(vars(Number_Observations_2019,
                    Number_Observations_2020,
                    Number_of_Stays_2019,
                    Number_of_Stays_2020,
                    Total_Pop_IZ_2018,
                    Obs_rate_per_1000_pop_2019,
                    Obs_rate_per_1000_pop_2020,
                    Stays_rate_per_1000_pop_2019,
                    Stays_rate_per_1000_pop_2020,
                    perc.diff.obs,
                    perc.diff.stays),
               min) %>% 
  mutate(Summary = "Min")

IZ2011_Emergency_Admit_Max <- IZ2011_Emergency_Admissions %>% 
  summarise_at(vars(Number_Observations_2019,
                    Number_Observations_2020,
                    Number_of_Stays_2019,
                    Number_of_Stays_2020,
                    Total_Pop_IZ_2018,
                    Obs_rate_per_1000_pop_2019,
                    Obs_rate_per_1000_pop_2020,
                    Stays_rate_per_1000_pop_2019,
                    Stays_rate_per_1000_pop_2020,
                    perc.diff.obs,
                    perc.diff.stays),
               max) %>% 
  mutate(Summary = "Max")

IZ2011_Emergency_Admit_Median <- IZ2011_Emergency_Admissions %>% 
  summarise_at(vars(Number_Observations_2019,
                    Number_Observations_2020,
                    Number_of_Stays_2019,
                    Number_of_Stays_2020,
                    Total_Pop_IZ_2018,
                    Obs_rate_per_1000_pop_2019,
                    Obs_rate_per_1000_pop_2020,
                    Stays_rate_per_1000_pop_2019,
                    Stays_rate_per_1000_pop_2020,
                    perc.diff.obs,
                    perc.diff.stays),
               median) %>% 
  mutate(Summary = "Median")

IZ2011_Emergency_Admit_Mean <- IZ2011_Emergency_Admissions %>% 
  summarise_at(vars(Number_Observations_2019,
                    Number_Observations_2020,
                    Number_of_Stays_2019,
                    Number_of_Stays_2020,
                    Total_Pop_IZ_2018,
                    Obs_rate_per_1000_pop_2019,
                    Obs_rate_per_1000_pop_2020,
                    Stays_rate_per_1000_pop_2019,
                    Stays_rate_per_1000_pop_2020,
                    perc.diff.obs,
                    perc.diff.stays),
               mean) %>% 
  mutate(Summary = "Mean")

IZ2011_Emergency_Admit_Total <- IZ2011_Emergency_Admissions %>% 
  summarise_at(vars(Number_Observations_2019,
                    Number_Observations_2020,
                    Number_of_Stays_2019,
                    Number_of_Stays_2020,
                    Total_Pop_IZ_2018
                    ),
               sum) %>% 
  mutate(Summary = "Total")

IZ2011_Emergency_Admit_Summary <- bind_rows(
  IZ2011_Emergency_Admit_Min,
  IZ2011_Emergency_Admit_Max,
  IZ2011_Emergency_Admit_Median,
  IZ2011_Emergency_Admit_Mean,
  IZ2011_Emergency_Admit_Total
)

View(IZ2011_Emergency_Admit_Summary)

write.csv(IZ2011_Emergency_Admit_Summary,
          (glue("{filepath_outputs}/IZ2011_RAPID_Emergency_Admissions_Summary_{date_range}_DailyExtract2020June30.csv")))
