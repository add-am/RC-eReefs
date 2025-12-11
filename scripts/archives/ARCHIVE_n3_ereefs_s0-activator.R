#n3_ereefs_s0-activator

#This script simply contains the code to run script 6 several times over without manual input

#load the required library
library(quarto)
library(tidyverse)
library(stringr)

#build the args to supply as parameters
params_to_supply <- list(TargetFyear = c(2023, 2022, 2021, 2020),
                         TargetRegion = c("Dry Tropics", "Wet Tropics", "Mackay Whitsunday Isaac"))

#combine the arguments in every unique way
params_to_supply <- expand.grid(params_to_supply, stringsAsFactors = F)

#add constant parameters
params_to_supply <- params_to_supply |> 
  mutate(input = "scripts/02. northern_three/n3_ereefs_s6-contextual-results.qmd",
         ProjectCrs = "EPSG:7844",
         Resolution = 0,
         Buffer = case_when(str_detect(TargetRegion, "Tropics") ~ 1.2, T ~ 0))

#combine all parameters into a list column (all arguments per row bunched together)
params_to_supply <- params_to_supply |> 
  mutate(execute_params = pmap(list(ProjectCrs, TargetFyear, TargetRegion, Resolution, Buffer), #take each of the columns
                               ~ list(project_crs = ..1, #and combine them into a list column
                                      target_fyear = ..2,
                                      target_region = ..3,
                                      resolution = ..4,
                                      buffer = ..5))) |> 
  select(input, execute_params) #select the columns we are interested in

#run quarto render, the params table supplies each of the arguments needed to the quarto_render function
pwalk(params_to_supply, quarto_render)
