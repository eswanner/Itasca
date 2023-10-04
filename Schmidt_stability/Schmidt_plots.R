######################################## required packages ######################################

require("tidyverse") #tidyverse needed for data manipulations and plotting
require("ggpubr") #for lining up plots and exporting
library(readxl) # for importing xlsx
library(hms)

####################################### load data #################################################

(Schmidt <- read_excel("~/Documents/R/Itasca/Schmidt_stability/Itasca_Schmidt_Results.xlsx", 
                                     col_types = c("text", "date", "numeric")))


####################################### plot data ##############################################

#check format of date
str(Schmidt) 

#convert date to Date format
Schmidt$Date <- as.Date(Schmidt$Date, "%Y-%m-%d")

#plot time series of Schmidt
temp <- ggplot(Schmidt, aes(x=Date, y=Schmidt)) +
          geom_point() +
          geom_line() +
  ylab("Schmidt Stability (J/m^2)") +
          facet_wrap(~Lake, nrow = 1) +
  theme_bw()
             
temp
