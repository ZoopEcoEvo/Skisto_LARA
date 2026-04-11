# Load in required packages
library(rmarkdown)
library(tidyverse)
library(lmerTest)

#Determine which scripts should be run
set_up_experiment = F #Will assign tubes and create the raw data file
process_data = F #Runs data analysis 
make_report = T #Runs project summary
knit_manuscript = F #Compiles manuscript draft


if(set_up_experiment == T){
  source(file = "Scripts/00_tube_assignments.R")
}


############################
### Read in the RAW data ###
############################

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################
ctmax_data = readr::read_csv(list.files(path = "Raw_data/ctmax_data", 
                                        pattern = "*.csv", 
                                        full.names = TRUE),
                             show_col_types = FALSE) %>% 
  mutate(datetime = lubridate::as_datetime(paste(exp_date, start_time, sep = " "), 
                                           format = "%m/%d/%Y %H:%M:%S")) %>% 
  filter(pop != "thermometer") %>% 
  filter((exp_rep == 1 & ctmax > 35) | (exp_rep == 0.5 & ctmax > 33.5) | (exp_rep == 3 & ctmax > 30)) %>% # Removing anomalously low CTmax values; threshold differs across experiments
  group_by(exp_rep) %>% 
  arrange(datetime) %>% 
  mutate(acc_hours = as.numeric(difftime(time1 = datetime, time2 = first(datetime), units = "hours")))
  
inc_temps = readr::read_csv(list.files(path = "Raw_data/incubator_temps", 
                           pattern = "*.csv", 
                           full.names = TRUE),
                id = "file",
                show_col_types = FALSE) %>% 
  janitor::clean_names() %>% 
  mutate(date_time_est = lubridate::as_datetime(date_time, format = "%m/%d/%Y %H:%M:%S")) %>% 
  drop_na(temperature_c) %>% 
  mutate("exp_rep" = parse_number(file),
         "incubator_id" = parse_number(str_extract(file, pattern = "_inc.")), 
         "incubator_temp" = parse_number(str_remove(file, pattern = "Raw_data/incubator_temps/rep._inc._"))) %>% 
  select(exp_rep, incubator_id, incubator_temp, "datetime" = date_time_est, "temp_c" = temperature_c)

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
