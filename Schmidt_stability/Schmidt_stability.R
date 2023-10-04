####################################### Calculate Schmidt stability ##################

# load required packages 

require("readr")
require("rLakeAnalyzer") #calculate lake data https://cran.r-project.org/web/packages/rLakeAnalyzer/rLakeAnalyzer.pdf
require("tidyverse") #tidyverse needed for data manipulations and plotting


######################################## import data ############################

#import a ysi dataset with only data needed for calculation, data as double, date column to date format
#change file name for next file

YSI <- read_csv("~/Documents/R/Itasca/Schmidt_stability/20211007_Deming_YSI.csv", 
                col_types = cols(DATE = col_datetime(format = "%m/%d/%y")), 
                skip = 5)
View(YSI)   

################################### Wrangle the YSI dataframe ########################################

#rename columns and remove all unneeded columns, remove depths less than zero and NA
YSI <- YSI %>%
  rename(Depth="Depth (m)", Salinity = "Sal (psu)", Temperature = "Temp (F)") %>% #rename columns to remove ()
  select(Depth, Salinity, Temperature) %>% #select only columns needed in calculations
  filter(Depth >=0) %>% #remove depths less than zero
  distinct(Depth, .keep_all = TRUE) %>% #remove any rows with duplicated depths
  slice(which(Depth > cummax(lag(Depth, default = 0)))) %>% #remove any rows where depth is less than the cumulative max
  mutate(Temperature = (Temperature-32)*(5/9)) #convert temperature to C from F


################################### Create YSI vectors ###############################################

#a numeric vector of water temperatures in degrees C
wtr <- YSI$Temperature

#a numeric vector corresponding to the depths (in m) of the the wtr measurements
depths <- YSI$Depth

#a numeric vector of salinity in Practical Salinity Scale units
sal <- YSI$Salinity

#import the csv with readr
#change to other lake file names if changing lake
((bth <- read_table("deming_1m.csv")))

#a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
bthA <- bth$Area
  
#a numeric vector of depths (m) which correspond to areal measures in bthA
bthD <- bth$Depth  
  
################################# Calculate Schmidt stability (J/m^2) ##############################

#run schmidt stability code
schmidt.stability(wtr, depths, bthA, bthD, sal)


