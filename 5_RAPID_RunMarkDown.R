#### Script produced with R3.6.1

#####  in Rstudio Server

#### By Ellie Bates, Service Access. Place and Wellbeing, Public Health Scotland

#####Date - 1 July 2020 

### Based on script by Cecilia Puech, Place and Wellbeing,
# 
#Public Health Scotland

## This srcipt runs markdown and outputs flexdashboard based html file

#########################

library(glue)

library(knitr)
library(markdown)

filepath_outputs <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/outputs")
filepath_scripts <- glue(
  "/conf/linkage/output/eleanb01/projects/UC_maps/scripts")

markdown_script <- "RAPID_Admission_Dashboard.Rmd"

output <- "RAPID_Admission_Dashboard"

#set date range to append to any output files
date_range <- "2020_2019_March_to_May"


rmarkdown::render(glue("{filepath_scripts}/{markdown_script}"), 
                  output_file =  glue("{output}_{date_range}.html"),
output_dir = glue("{filepath_outputs}"))
