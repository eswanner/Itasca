######################################## required packages ######################################

require("readr")
require("rLakeAnalyzer") #calculate lake data https://cran.r-project.org/web/packages/rLakeAnalyzer/rLakeAnalyzer.pdf
require("tidyverse") #tidyverse needed for data manipulations and plotting
library(readxl)
require("ggpubr")

######################################## import data ############################


#import a ysi dataset with only data needed for calculation, data as double, date column to date format
YSI <- read_csv("~/Documents/R/Itasca/Schmidt_stability/20220809-10_YSI_Deming.csv", 
                col_types = cols(DATE = col_datetime(format = "%m/%d/%y")), 
                skip = 5)
                     
################################### Wrangle the YSI dataframe ########################################

#rename columns and remove all unneeded columns, remove depths less than zero and NA
YSI <- YSI %>%
  rename(Depth="Depth (m)", Oxygen = "ODO (mg/L)", Temperature = "Temp (F)") %>% #rename columns to remove ()
  select(Depth, Oxygen, Temperature) %>% #select only columns needed in calculations
  filter(Depth >=0.26) %>% #remove depths less than zero
  distinct(Depth, .keep_all = TRUE) %>% #remove any rows with duplicated depths
  slice(which(Depth > cummax(lag(Depth, default = 0)))) %>% #remove any rows where depth is less than the cumulative max
  mutate(Temperature = (Temperature-32)*(5/9)) #convert temperature to C from F


################################### Create YSI vectors ###############################################

#a numeric vector of water temperatures in degrees C
wtr <- YSI$Temperature

#a numeric vector corresponding to the depths (in m) of the the wtr measurements
depths <- YSI$Depth

  
################################# Calculate Thermocline Depth ##############################

#run thermocline depth code
thermo.depth(wtr, depths, seasonal=FALSE)

#run thermocline depth code
thermo.depth(wtr, depths, seasonal=TRUE)

################################# Find Max DO depth ###############################

#find depth of max DO
YSI %>%
  slice_max(Oxygen, with_ties = FALSE)  %>% 
  select(Depth, Oxygen) 

####################### Find thermocline from hand-recorded YSI data ################

require("readxl") #for importing data


#import dataset
YSI <- read_excel("~/Documents/R/Itasca/Itasca_database_2022_v2.1.xlsx", 
                 col_types = c("text", "date", "text", 
                               "numeric", "text", "text", "numeric", 
                               "text", "text", "text", "numeric"))

#drop date so month and year become categorical
YSI$Date <- format(YSI$Date, format="%Y-%m-%d")

#make a temperature only df
YSI_temps <- YSI %>% # a df of all temp data
  select(Lake, Date, Depth, Measurement, Value, Device, Units) %>% # select columns to include
  filter(Measurement=="Temperature") %>% # want only temp data
  #filter(Units=="degreesCelsius") %>% # exclude LED readings
  filter(Value!=-99999) %>%
  group_by(Date,Measurement) %>% #don't need to group by lake this time
  distinct(Depth, .keep_all=TRUE) %>% 
  ungroup()

#select lake and date
Lake <- YSI_temps %>%
  filter(Lake=="Budd")  %>% #change this to desired lake
  filter(Date=="2021-10-06") #change this to desired data

#a numeric vector of water temperatures in degrees C
wtr <- Lake$Value

#a numeric vector corresponding to the depths (in m) of the the wtr measurements
depths <- Lake$Depth

#run thermocline depth code
thermo.depth(wtr, depths, seasonal=FALSE)

#run thermocline depth code
thermo.depth(wtr, depths, seasonal=TRUE)

################################# Find Max DO on hand-recorded data ###############################

#make an O2 only df
YSI_O2 <- YSI %>%
  select(Lake, Date, Depth, Measurement, Value, Device, Units) %>% # select columns to include
  filter(Measurement=="Dissolved Oxygen") %>% # want only DO data
  filter(Value!=-99999) %>%
  group_by(Lake,Date,Measurement) %>% #group by distinct times
  distinct(Depth, .keep_all=TRUE) %>% 
  slice_max(Value, with_ties = FALSE)  %>% 
  select(Depth, Value) %>% 
  ungroup() 

################################# Find chl max on hand-recorded data ###############################

#make an chl only df
YSI_chl <- YSI %>%
  select(Lake, Date, Depth, Measurement, Value, Device, Units) %>% # select columns to include
  filter(Measurement=="Chlorophyll") %>% # want only DO data
  filter(Value!=-99999) %>%
  group_by(Lake,Date,Measurement) %>% #group by distinct times
  distinct(Depth, .keep_all=TRUE) %>% 
  slice_max(Value, with_ties = FALSE)  %>% 
  select(Depth, Value) %>% 
  ungroup()

################################ Make plots ###############

max <- read_excel("~/Documents/R/Itasca/SCML/Itasca_TD_Results.xlsx")

#drop date so month and year become categorical
max$Date <- format(max$Date, format="%Y-%m")

max <- max %>%
  rename(TD="Thermocline Depth (m)")%>%
  rename(DO="DO max depth (m)") %>%
  rename(SCML="YSI SCML depth (m)") %>%
  select(Lake, Date, TD, DO, SCML) %>%
  na.omit()

max_nofall <-max %>%
  filter(Date!='2021-10') 

max_nowinter <- max_nofall %>%
  filter(Date!='2020-01') 
 

#plot thermocline depth vs. DO max depth
TD_DO <- ggscatter(max, x="TD", y="DO",
                   color = "Date",
                   shape = "Lake",
                   add = "reg.line",
                   add.params = list(color="black", fill="lightgrey"),
                   conf.int = TRUE) +
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 2)+
  ylab("Maximum dissolved oxygen depth (m)")+
  xlab("Thermocline depth (m)") +
  theme_classic()  #clean white and black
TD_DO

TD_DO_n <- ggscatter(max_nofall, x="TD", y="DO",
                   color = "Date",
                   shape = "Lake",
                   add = "reg.line",
                   add.params = list(color="black", fill="lightgrey"),
                   conf.int = TRUE) +
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 2)+
  ylab("Maximum dissolved oxygen depth (m)")+
  xlab("Thermocline depth (m)") +
  theme_classic()  #clean white and black
TD_DO_n

#plot thermocline depth vs. SCML depth
TD_SCML <- ggscatter(max, x="TD", y="SCML",
                     color = "Date",
                     shape = "Lake",
                     add = "reg.line",
                     add.params = list(color="black", fill="lightgrey"),
                     conf.int = TRUE) +
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 3)+
  xlab("Thermocline depth (m)")+
  ylab("Subsurface chlorophyll maximum depth (m)") +
  theme_classic()  #clean white and black
TD_SCML

TD_SCML_n <- ggscatter(max_fall, x="TD", y="SCML",
                     color = "Date",
                     shape = "Lake",
                     add = "reg.line",
                     add.params = list(color="black", fill="lightgrey"),
                     conf.int = TRUE) +
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 2)+
  xlab("Thermocline depth (m)")+
  ylab("Subsurface chlorophyll maximum depth (m)") +
  theme_classic()  #clean white and black
TD_SCML_n

DO_SCML <-ggscatter(max, x="DO", y="SCML",
                    color = "Date",
                    shape = "Lake",
                    add = "reg.line",
                    add.params = list(color="black", fill="lightgrey"),
                    conf.int = TRUE) +
   stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 3)+ 
  xlab("Maximum dissolved oxygen depth (m)")+
  ylab("Subsurface chlorophyll maximum depth(m)") +
  theme_classic()  #clean white and black
           
DO_SCML

DO_SCML_n <-ggscatter(max_nowinter, x="DO", y="SCML",
                      color = "Date",
                      shape = "Lake",
                      add = "reg.line",
                      add.params = list(color="black", fill="lightgrey"),
                      conf.int = TRUE)+
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 2) +
  xlab("Maximum dissolved oxygen depth (m)")+
  ylab("Subsurface chlorophyll maximum depth (m)") +
  theme_classic()  #clean white and black
DO_SCML_n
                    
# line up depth plots in a row and label them
Thermocline<-ggarrange(TD_DO, TD_SCML, DO_SCML, TD_DO_n, TD_SCML_n, DO_SCML_n, # names of plots in order desired
                labels = c("A", "B", "C", "D", "E", "F"), # labels plots
                ncol = 3, nrow = 2, align = "h", #  align horizontally
                legend = c("bottom"),
                common.legend=TRUE) 
Thermocline

#removing SCML
TD_only <-ggarrange(TD_DO, TD_DO_n,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1, align = "h",
                   legend = c("bottom"),
                   common.legend=TRUE)
TD_only

#save plot in WD as pdf with dimensions that you want
ggsave("Thermocline.pdf", device = "pdf", plot=TD_only, path="~/Documents/R/MN_lakes/SCML",
       width=8.5, height=5, units="in") 


#save plot in WD as pdf with dimensions that you want
ggsave("Thermocline.tif", device = "tiff", plot=TD_only, path="~/Documents/R/MN_lakes/SCML",
       width=8.5, height=5, dpi= 300, units="in") 
