#### Script produced with R3.6.1

#####  in Rstudio Server

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 23 July 2020 

### Partly Based on script by Cecilia Puech, Place and Wellbeing, 
#Public Health Scotland

## Runtime approximately 1 minute

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
### If re-suing this code check these paths are still correcy
filepath_IZ_data_in <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs/nhs24_sas")
filepath_lookups <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/lookups")
filepath_checks <-glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/checks/nhs24_sas")
filepath_outputs <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs/nhs24_sas")
filepath_IZPops <- glue(
  "/conf/linkage/output/lookups/Unicode/Populations/Estimates/")  

 
input_IZ_2020 <- "IntZone2011_SAS_Attendances_20200301_to_20200531"
input_IZ_2019 <- "IntZone2011_SAS_Attendances_20190301_to_20190531"

#input_IZ_2020_nhs24 <- "IntZone2011_NHS24_Completed_Calls_20200301_to_20200531"
#input_IZ_2019_nhs24 <- "IntZone2011_NHS24_Completed_Calls_20190301_to_20190531"

input_IZ_Lookups <- "int_names_lookup"
input_IZ_Pops <- "IntZone2011_pop_est_2011_2018"

#set date range to append to any output files
date_range <- "2020_2019_March_to_May"


####### STEP 1 - read in required files to create wide file

# read in the two IZ files adding relevant years to variable names
IZ_2020 <- read_csv(glue("{filepath_IZ_data_in}/{input_IZ_2020}.csv")) %>% 
  select(intzone2011, 
         sas_attendances_2020, 
         sas_attend_2020_below5) 

colnames(IZ_2020)

IZ_2019 <- read_csv(glue("{filepath_IZ_data_in}/{input_IZ_2019}.csv")) %>% 
  select(intzone2011, 
         sas_attendances_2019, 
         sas_attend_2019_below5)

colnames(IZ_2019)


IZ_2020_2019 <- IZ_2020 %>% 
  full_join(IZ_2019, by = "intzone2011")

colnames(IZ_2020_2019)

rm(IZ_2019, IZ_2020)


###### STEP 2 - write out a summary file to the checks folder


#select and write out the unmatched data

IZ2020_2019_Unmatched <- IZ_2020_2019 %>% 
  filter(is.na(intzone2011)) %>% 
  select(intzone2011,
         sas_attendances_2019,
         sas_attendances_2020
         ) %>% 
  mutate(intzone2011 = "Unmatched")

#View(IZ2020_2019_Unmatched)

IZ2020_2019_Totals <- IZ_2020_2019 %>% 
    summarise_at(vars(sas_attendances_2019, sas_attendances_2020),
         sum) %>% 
  mutate(intzone2011 = "Total_SAS_Attendances")
  

IZ2020_2019_Summary <- bind_rows(IZ2020_2019_Totals, IZ2020_2019_Unmatched)

write.csv(IZ2020_2019_Summary,
          (glue("{filepath_checks}/IZ2020_2019_SAS_Attendances_Summary_{date_range}.csv")))

rm(IZ2020_2019_Totals, IZ2020_2019_Unmatched)

##### STEP 3 Join to the Int_names_lookup file (will discard any NA)

#### read in IntZone Lookup file

IntZone2011_Lookup <- read_csv(glue(
  "{filepath_lookups}/{input_IZ_Lookups}.csv")) %>% 
  rename (intzone2011 = InterZone,
          intzone2011_name = Name) 

colnames(IntZone2011_Lookup)


IZ2020_2019_Join_to_Lookup_File <- IntZone2011_Lookup %>% 
  left_join(IZ_2020_2019, by = "intzone2011") 

colnames(IZ2020_2019_Join_to_Lookup_File)

IZ2020_2019_IZ_with_Atts_below5 <- IZ2020_2019_Join_to_Lookup_File %>% 
  summarise_at(vars(sas_attend_2019_below5, 
                    sas_attend_2020_below5),
               sum)

View(IZ2020_2019_IZ_with_Atts_below5 )

write.csv(IZ2020_2019_IZ_with_Atts_below5,
          (glue("{filepath_checks}/IZ2020_2019_IZ_SAS_Attendances_below5_{date_range}.csv")))




### STEP 5A
#### Create wide file with required variables, calculate rates and difference - write out

IZ2011_SAS_Attendances <- IZ2020_2019_Join_to_Lookup_File %>% 
  select(intzone2011, intzone2011_name, HB, HBName,  
         sas_attendances_2019, sas_attendances_2020)
  

colnames(IZ2011_SAS_Attendances)


# read in the population file
IZPops2018 <-readRDS(glue("{filepath_IZPops}/{input_IZ_Pops}.rds")) 
# group to give totals only
IZPops2018 <- IZPops2018 %>%
  group_by(year, intzone2011) %>%
  summarise(Total_Pop_IZ_2018 = sum(total_pop))%>%
  ungroup()
#keep 2018 only
IZPops2018 <-filter(IZPops2018,year==2018) %>% 
  select(intzone2011, Total_Pop_IZ_2018)

View(IZPops2018)

# join population file to IZ wide file and create rates
IZ2011_SAS_Attendances <- IZ2011_SAS_Attendances %>% 
  left_join(IZPops2018, by = "intzone2011") %>% 
  mutate(Atts_rate_per_1000_pop_2019 = (sas_attendances_2019/Total_Pop_IZ_2018)*1000) %>% 
  mutate(Atts_rate_per_1000_pop_2020 = (sas_attendances_2020/Total_Pop_IZ_2018)*1000) %>% 
    mutate(perc.diff.atts = ((Atts_rate_per_1000_pop_2020 - Atts_rate_per_1000_pop_2019)
                          /Atts_rate_per_1000_pop_2019)*100)
  

View(IZ2011_SAS_Attendances)


write.csv(IZ2011_SAS_Attendances,
          (glue("{filepath_outputs}/IZ2011_SAS_Attendances_{date_range}.csv")))


#### Step 6 - write out summary file - from wide data
####summarise minimum and maximum numbers, Rates, and change in final IZ file

IZ2011_SAS_Attendances_Min <- IZ2011_SAS_Attendances %>% 
  summarise_at(vars(sas_attendances_2019,
                    sas_attendances_2020,
                    Total_Pop_IZ_2018,
                    Atts_rate_per_1000_pop_2019,
                    Atts_rate_per_1000_pop_2020,
                    perc.diff.atts),
               min) %>% 
  mutate(Summary = "Min")

IZ2011_SAS_Attendances_Max <- IZ2011_SAS_Attendances %>% 
  summarise_at(vars(sas_attendances_2019,
                    sas_attendances_2020,
                    Total_Pop_IZ_2018,
                    Atts_rate_per_1000_pop_2019,
                    Atts_rate_per_1000_pop_2020,
                    perc.diff.atts),
               max) %>% 
  mutate(Summary = "Max")

IZ2011_SAS_Attendances_Median <- IZ2011_SAS_Attendances %>% 
  summarise_at(vars(sas_attendances_2019,
                    sas_attendances_2020,
                    Total_Pop_IZ_2018,
                    Atts_rate_per_1000_pop_2019,
                    Atts_rate_per_1000_pop_2020,
                    perc.diff.atts),
               median) %>% 
  mutate(Summary = "Median")

IZ2011_SAS_Attendances_Mean <- IZ2011_SAS_Attendances %>% 
  summarise_at(vars(sas_attendances_2019,
                    sas_attendances_2020,
                    Total_Pop_IZ_2018,
                    Atts_rate_per_1000_pop_2019,
                    Atts_rate_per_1000_pop_2020,
                    perc.diff.atts),
               mean) %>% 
  mutate(Summary = "Mean")

IZ2011_SAS_Attendances_Total <- IZ2011_SAS_Attendances %>% 
  summarise_at(vars(sas_attendances_2019,
                    sas_attendances_2020,
                    Total_Pop_IZ_2018
                    ),
               sum) %>% 
  mutate(Summary = "Total")

IZ2011_SAS_Attendances_Summary <- bind_rows(
  IZ2011_SAS_Attendances_Min,
  IZ2011_SAS_Attendances_Max,
  IZ2011_SAS_Attendances_Median,
  IZ2011_SAS_Attendances_Mean,
  IZ2011_SAS_Attendances_Total
)

View(IZ2011_SAS_Attendances_Summary)

write.csv(IZ2011_SAS_Attendances_Summary,
          (glue("{filepath_outputs}/IZ2011_SAS_Attendances_Summary_{date_range}.csv")))
