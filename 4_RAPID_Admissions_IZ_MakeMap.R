#### Script produced with R3.6.1

#####  in Rstudio Server

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 1 July 2020 

### Partly Based on script by Cecilia Puech, Place and Wellbeing,
# 
#Public Health Scotland

## 

#########################
### Step 1
### Set libraries and paths
###########################

#### load libraries - if not present please install packages


# Work around for  loading rgdal suggested by TM
#install.packages("rgdal",
 #                configure.args = c(
  #                 "--with-gdal-config=/usr/bin/gdal-config_Apr.03.20",
   #                "--with-proj-data=/usr/share/proj",
    #               "--with-data-copy=yes"),
     #            configure.vars = c("GDAL_DATA=/usr/share/gdal"))

#library(tidyverse)
library(dplyr)
library(readr)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(janitor)
library(htmltools)
library(DT)
library(glue)

#library(readxl)


# Alternative method of shapefile reading if ReadOGR keeps crashing
# #library(sf)
#example sf code
#chi_shape <- here("static/data/Boundaries - Community Areas (current)/geo_export_328cdcbf-33ba-4997-8ce8-90953c6fec19.shp") %>%
 # st_read()
#from here
#https://cfss.uchicago.edu/notes/simple-features/

####check the current working directory if needed
#getwd()

### Set filepaths for inputs, checks and final outputs, and any key file names
filepath_RAPID_IZ_In <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs")
filepath_outputs <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs")
filepath_shapefiles<- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/shapefiles/IZ2011/SG_IZ_2011_Simplified_100mtolerance")
filepath_lookups <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/lookups")


#Note - no longer used after revisions to code
#input_IZ_EmergencyAdmit <- "IZ2011_RAPID_Emergency_Admissions_Long_2020_2019_March_to_May"

input_IZ_shape <- "SG_IZ_2011_Simplified"
input_IZ_EmergencyAdmit_table <- "IZ2011_RAPID_Emergency_Admissions_2020_2019_March_to_May"

#get lookups

#lookup <- read_csv(glue("{filepath_lookups}/int_names_lookup.csv"))


AE_markers <- read_csv(glue("{filepath_lookups}/AE_markers.csv"))


## Get IZ Admissions data

data <- read_csv(glue("{filepath_outputs}/{input_IZ_EmergencyAdmit_table}.csv")) %>% 
  mutate(Stays_rate_2019 = round_half_up(Stays_rate_per_1000_pop_2019,1), 
         Stays_rate_2020 = round_half_up(Stays_rate_per_1000_pop_2020,1),
         perc_diff = round_half_up(perc.diff.stays, 1)) %>% 
  select(InterZone = IntZone2011,
         Name = IntZone2011_Name,
         HB, HBName,
         Stays_rate_2019, Stays_rate_2020,
         perc_diff) 

int_dat <- data %>% 
  arrange(desc(Stays_rate_2020))



## Get Intermediate Zone Shapefile
## Note a simplified smaller shape file (with less detail on lochs and coastlines)
## is being used here

int_shp <- readOGR(glue("{filepath_shapefiles}"),
                   "SG_IZ_2011_Simplified")
int_shp <-spTransform(int_shp,"+init=epsg:4326")


## links data to shapefile

int_shp <- int_shp[int_shp$InterZone %in% int_dat$InterZone, ]
int_shp <- merge(int_shp, int_dat, by = "InterZone")
#int_shp <- int_shp[row.names(int_shp)[order(int_shp@data$Stays_rate_2020)],]

#Awesome icons
#markers <- awesomeIcons(icon = "hospital-o", library = "fa", markerColor = "blue")

HospIcon <- list(
  iconUrl = glue("{filepath_lookups}/hosp.png"),
  iconSize = c(20, 20),
  opacity = 1)
  

##Percentage change map

pal_diff <- colorNumeric(palette = colorRampPalette(c("#E31A1C", "#FC4E2A","#FD8D3C","#FEB24C", "#FED976", "#FFEDA0", "#FFFFCC", "#ffffff", "#e4f0f6", "#9ac7dd", "#60a6ca","#3c8bb3", "#2d6987"))(length(int_shp$perc_diff)), 
                         domain= int_shp$perc_diff)

map.change <- leaflet() %>%
  addMapPane(name = "Place Names", zIndex = 420) %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>%
  addProviderTiles(provider = providers$CartoDB.PositronOnlyLabels, options = leafletOptions(pane = "Place Names")) %>%
  addPolygons(data = int_shp, 
              fillColor = ~pal_diff(perc_diff), 
              color = "#0f243e",
              stroke = T, 
              weight = 1, 
              fillOpacity = .7, 
              group = "Intermediate Zone",
              popup = paste0("Intermediate Zone: <b>", int_shp$Name.x,
                             "</b><br>Health Board: <b>", int_shp$HBName,
                             "</b><br>Admission per 1000 people 2019: <b>", int_shp$Stays_rate_2019,
                             "</b><br>Admission per 1000 people 2020: <b>", int_shp$Stays_rate_2020,
                             "</b><br>Change in Emergency Admissions from 2019 to 2020: <b>", paste0(int_shp$perc_diff,"%"), "</b>")) %>% 
  addLegend("bottomright", pal = pal_diff, values = int_shp@data$perc_diff,
            title = "Percentage change", labFormat = labelFormat(suffix = "%"), 
            opacity = 1)%>% 
  #addAwesomeMarkers(data = AE_markers, ~Longitude, ~Latitude, icon = ~markers, label = ~as.character(LocationName))
  addMarkers(data = AE_markers, lng = ~Longitude, lat = ~Latitude, 
             icon = HospIcon, label = ~as.character(LocationName))


##Data for table
## need to set correct path here
table_data <- read_csv(glue("{filepath_outputs}/{input_IZ_EmergencyAdmit_table}.csv")) %>% 
  mutate(Stays_rate_per_1000_pop_2019 = round_half_up(Stays_rate_per_1000_pop_2019,1), 
         Stays_rate_per_1000_pop_2020 = round_half_up(Stays_rate_per_1000_pop_2020,1),
         perc.diff.stays = round_half_up(perc.diff.stays, 1))%>% 
  select(IntZone2011, 
         IntZone2011_Name, 
         Health_Board_Code = HB,
         Health_Board_Name = HBName,
         Emergency_Admissions_Stays_2019 = Number_of_Stays_2019, 
         Emergency_Admissions_Stays_2020 = Number_of_Stays_2020, 
         Total_Population_Estimated_2018 = Total_Pop_IZ_2018, 
         Emergency_Admission_Rate_per_1000_people_2019
         = Stays_rate_per_1000_pop_2019,
         Emergency_Admission_Rate_per_1000_people_2020 
         = Stays_rate_per_1000_pop_2020,
         Percentage_Difference_Emergency_Admissions
         = perc.diff.stays)



DT_table <- DT::datatable(table_data, style = 'bootstrap4', rownames = FALSE,
                          extensions = 'Buttons', 
                          options = list(pageLength = nrow(table), dom = 'Bti',
                                         buttons = c('csv', 'excel','print')))
