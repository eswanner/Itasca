
########################### Plotting Fluoroprobe data #################

#load required packages
require("tidyverse") #for data wrangling and plotting
require("readxl") #for importing data
require("lubridate") #for parsing dates
require("ggpubr") #for lining up plots and exporting
require("RcppRoll") #for calculating rolling averages

############################### Import & Wrangle Data ##############################

#import dataset
df <- read_excel("~/Documents/R/Itasca/Itasca_database_2022_v2.2.xlsx", 
                 col_types = c("text", "date", "text", 
                               "numeric", "text", "text", "numeric", 
                               "text", "text", "text", "numeric"))

#drop date so month and year become categorical
df$date <- format(df$Date, format="%Y-%m-%d")

df <- df %>% # a df of all data
  select(Lake, date, Depth, Measurement, Value, Device, Units) %>% # select columns to include
  rename(Date=date) #%>% #rename categorical date for legend

############################# Make df for each lake chla only ##################

#Arco df
FP_AL<-df %>%
  filter(Lake=="Arco") %>% #same as above but making lake specific df
  filter(Device=="BBE Fluoroprobe") %>% # want only FP data
  filter(Units=="microgramsPerLiter") %>% # exclude LED readings
  group_by(Date,Measurement) %>% #don't need to group by lake this time
  distinct(Depth, .keep_all=TRUE) %>% 
  ungroup()

#calculate the total chlorophyll
FP_AL_tot <- FP_AL %>%
  group_by(Date, Depth) %>%
  mutate('Total Chlorophyll' = sum(Value)) %>%
  gather('Total Chlorophyll', key="Measurement", value = "Value") %>%
  distinct()

#bind the total chlorophyll into the FP df
FP_AL <- rbind(FP_AL, FP_AL_tot)


#Budd df
FP_BL<-df %>%
  filter(Lake=="Budd") %>%
  filter(Device=="BBE Fluoroprobe") %>% # want only FP data
  filter(Units=="microgramsPerLiter") %>% # exclude LED readings
  group_by(Date,Measurement) %>% #don't need to group by lake this time
  distinct(Depth, .keep_all=TRUE) %>% 
  ungroup()

#calculate the total chlorophyll
FP_BL_tot <- FP_BL %>%
  group_by(Date, Depth) %>%
  mutate('Total Chlorophyll' = sum(Value)) %>%
  gather('Total Chlorophyll', key="Measurement", value = "Value") %>%
  distinct()

#bind the total chlorophyll into the FP df
FP_BL <- rbind(FP_BL, FP_BL_tot)

#Deming df
FP_DL<-df %>%
  filter(Lake=="Deming") %>%
  filter(Device=="BBE Fluoroprobe") %>% # want only FP data
  filter(Units=="microgramsPerLiter") %>% # exclude LED readings
  group_by(Date,Measurement) %>% #don't need to group by lake this time
  distinct(Depth, .keep_all=TRUE) %>% 
  ungroup() %>%
  filter(Date!="2022-05-19",
         Date!="2021-07-14")

#calculate the total chlorophyll
FP_DL_tot <- FP_DL %>%
  group_by(Date, Depth) %>%
  mutate('Total Chlorophyll' = sum(Value)) %>%
  gather('Total Chlorophyll', key="Measurement", value = "Value") %>%
  distinct()

#bind the total chlorophyll into the FP df
FP_DL <- rbind(FP_DL, FP_DL_tot)

#Josephine df
FP_JL<-df %>%
  filter(Lake=="Josephine") %>%
  filter(Device=="BBE Fluoroprobe") %>% # want only FP data
  filter(Units=="microgramsPerLiter") %>% # exclude LED readings
  group_by(Date,Measurement) %>% #don't need to group by lake this time
  distinct(Depth, .keep_all=TRUE) %>% 
  ungroup()

#calculate the total chlorophyll
FP_JL_tot <- FP_JL %>%
  group_by(Date, Depth) %>%
  mutate('Total Chlorophyll' = sum(Value)) %>%
  gather('Total Chlorophyll', key="Measurement", value = "Value") %>%
  distinct()

#bind the total chlorophyll into the FP df
FP_JL <- rbind(FP_JL, FP_JL_tot)

############################# Plot depth distribution of chlorophyll ##############

#add a color blind-friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#make a plot of chlorophyll vs depth for each lake 
AL <- ggplot(FP_AL,  aes(Depth, Value)) + #use depth and d18O data as x and y
  geom_line(aes(color=Measurement)) + #plot points, each date is a shape, also legend name
  ylab("Chlorophyll (ug/L)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        axis.title.x=element_blank(), #remove x axis labels
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9)) + #change font size of axis label
  facet_grid(~Date, scales="free") +  #facet by date to compare distributions
  scale_color_manual(name="Taxon", labels=c("Chlorophyta", "Cryptophyta", "Cyanobacteria", "Diatoms and Dinoflagellates", "Total"), 
                   values=c("#009E73", "#CC79A7", "#0072B2", "#E69F00", "#000000")) # rename the legend labels and give them good colors
AL


BL <- ggplot(FP_BL,  aes(Depth, Value)) + #use depth and d18O data as x and y
  geom_line(aes(color=Measurement)) + #plot points, each date is a shape, also legend name
  ylab("Chlorophyll (ug/L)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        axis.title.x=element_blank(), #remove y axis labels
        axis.title.y=element_blank(), #remove y axis labels
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9)) + #change font size of axis label
  facet_grid(~Date, scales="free") +  #facet by date to compare distributions
  scale_color_manual(name="Taxon", labels=c("Chlorophyta", "Cryptophyta", "Cyanobacteria", "Diatoms and Dinoflagellates","Total"), 
                     values=c("#009E73", "#CC79A7", "#0072B2", "#E69F00", "#000000")) # rename the legend labels and give them good colors
BL

DL <- ggplot(FP_DL,  aes(Depth, Value)) + #use depth and d18O data as x and y
  geom_line(aes(color=Measurement)) + #plot points, each date is a shape, also legend name
  ylab("Chlorophyll (ug/L)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(#legend.position = "none", #remove legend
        axis.title.y=element_blank(), #remove y axis labels
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9)) + #change font size of axis label
  facet_grid(~Date, scales="free") +  #facet by date to compare distributions
  scale_color_manual(name="Taxon", labels=c("Chlorophyta", "Cryptophyta", "Cyanobacteria", "Diatoms and Dinoflagellates", "Total"), 
                     values=c("#009E73", "#CC79A7", "#0072B2", "#E69F00", "#000000")) # rename the legend labels and give them good colors
DL

JL <- ggplot(FP_JL,  aes(Depth, Value)) + #use depth and d18O data as x and y
  geom_line(aes(color=Measurement)) + #plot points, each date is a shape, also legend name
  ylab("Chlorophyll (ug/L)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9)) + #change font size of axis label
  facet_grid(~Date, scales="free") +  #facet by date to compare distributions
  scale_color_manual(name="Taxon", labels=c("Chlorophyta", "Cryptophyta", "Cyanobacteria", "Diatoms and Dinoflagellates", "Total"), 
                     values=c("#009E73", "#CC79A7", "#0072B2", "#E69F00", "#000000")) # rename the legend labels and give them good colors
JL

#################################### Line up and export plots ##############
# line up plots in a row and label them
Fluoroprobe<-ggarrange(AL, BL, JL, DL, # names of two plots in order desired
                     labels = c("A", "B", "C", "D"), # labels plots
                     ncol = 2, nrow = 2, align = "h", # 2 across; align horizontally
                  legend = c("bottom"),
                  common.legend=TRUE) 
Fluoroprobe

#save plot in WD as pdf with dimensions that you want
ggsave("Fluoroprobe.pdf", device = "pdf", plot=Fluoroprobe, width=7, height=5, units="in") 


############### Find Max Chlorophyll depth in Fluoroprobe ###############################

#find depth of max chl
FP_DL_tot <- FP_DL %>%
  group_by(Date, Depth) %>%
  summarise(across(c(Value), list(sum=sum))) 

FP_DL_tot %>% 
  group_by(Date) %>%
  slice_max(Value_sum, with_ties = FALSE)  %>% 
  select(Depth, Value_sum) 

#find depth of max chl Arco
FP_AL_tot <- FP_AL %>%
  group_by(Date, Depth) %>%
  summarise(across(c(Value), list(sum=sum))) 

FP_AL_tot %>% 
  group_by(Date) %>%
  slice_max(Value_sum, with_ties = FALSE)  %>% 
  select(Depth, Value_sum) 


#find depth of max chl Budd
FP_BL_tot <- FP_BL %>%
  group_by(Date, Depth) %>%
  summarise(across(c(Value), list(sum=sum))) 

FP_BL_tot %>% 
  group_by(Date) %>%
  slice_max(Value_sum, with_ties = FALSE)  %>% 
  select(Depth, Value_sum) 

#calculate the total chlorophyll
FP_JL_tot <- FP_JL %>%
  group_by(Date, Depth) %>%
  mutate('Total Chlorophyll' = sum(Value)) %>%
  gather('Total Chlorophyll', key="Measurement", value = "Value") %>%
  distinct()

#bind the total chlorophyll into the FP df
FP_JL <- rbind(FP_JL, FP_JL_tot)

FP_JL %>% 
  group_by(Date) %>%
  slice_max(Value, with_ties = FALSE)  %>% 
  select(Depth, Value) 


############################ look at YSI turbidity profiles #######################

T_AL<-df %>%
  filter(Lake=="Arco") %>% #same as above but making lake specific df
  filter(Measurement=="Turbidity") %>%
  filter(Value!=-99999) %>% 
  select(Lake, date, Depth, Value) %>%
  rename(Date=date) 
  
T_BL<-df %>%
  filter(Lake=="Budd") %>%
  filter(Measurement=="Turbidity") %>%
  filter(Value!=-99999) %>% 
  select(Lake, date, Depth, Value) %>%
  rename(Date=date)
  

T_DL<-df %>%
  filter(Lake=="Deming") %>%
  filter(Measurement=="Turbidity") %>%
  filter(Value!=-99999) %>% 
  select(Lake, date, Depth, Value) %>%
  rename(Date=date)

T_JL<-df %>%
  filter(Lake=="Josephine") %>%
  filter(Measurement=="Turbidity") %>%
  filter(Value!=-99999) %>% 
  select(Lake, date, Depth, Value) %>%
  rename(Date=date)

AL2 <- ggplot(T_AL,  aes(Depth, Value)) + #use depth and turbidity data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Turbidity (NTU)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
AL2


BL2 <- ggplot(T_BL,  aes(Depth, Value)) + #use depth and turbidity data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Turbidity (NTU)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
BL2

JL2 <- ggplot(T_JL,  aes(Depth, Value)) + #use depth and turbidity data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Turbidity (NTU)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
JL2

DL2 <- ggplot(T_DL,  aes(Depth, Value)) + #use depth and turbidity data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Turbidity (NTU)")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
DL2

#################################### Line up and export plots ##############
# line up plots in a row and label them
Turbidity<-ggarrange(AL2, BL2, JL2, DL2, # names of two plots in order desired
                       labels = c("A", "B", "C", "D"), # labels plots
                       ncol = 2, nrow = 2, align = "h", # 2 across; align horizontally
                       legend = c("bottom"),
                       common.legend=TRUE) 
Turbidity

#save plot in WD as pdf with dimensions that you want
ggsave("Turbidity.pdf", device = "pdf", plot=Turbidity, width=7, height=5, units="in") 




################################ Plot PAR data ######################

PAR_AL<-df %>%
  filter(Lake=="Arco") %>% #same as above but making lake specific df
  filter(Measurement=="Photosynthetically Active Radiation") %>%
  filter(Value!=-99999) %>% 
  select(Lake, Date, Depth, Measurement, Value, Units) 
 

PAR_BL<-df %>%
  filter(Lake=="Budd") %>%
  filter(Measurement=="Photosynthetically Active Radiation") %>%
  filter(Value!=-99999) %>% 
  select(Lake, Date, Depth, Measurement, Value, Units) 


PAR_DL<-df %>%
  filter(Lake=="Deming") %>%
  filter(Measurement=="Photosynthetically Active Radiation") %>%
  filter(Value!=-99999) %>% 
  select(Lake, Date, Depth, Measurement, Value, Units) 

PAR_JL<-df %>%
  filter(Lake=="Josephine") %>%
  filter(Measurement=="Photosynthetically Active Radiation") %>%
  filter(Value!=-99999) %>% 
  select(Lake, Date, Depth, Measurement, Value, Units) 

AL3 <- ggplot(PAR_AL,  aes(Depth, Value)) + #use depth and PAR data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Photosynthetically Active Radiation (umol Einsteins m-2 s-1")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  scale_y_log10() + #need log for exponential decay
  theme_classic() + #clean white and black
  theme(#legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
AL3


BL3 <- ggplot(PAR_BL,  aes(Depth, Value)) + #use depth and PAR data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Photosynthetically Active Radiation (umol Einsteins m-2 s-1")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  scale_y_log10() + #need log for exponential decay
  theme_classic() + #clean white and black
  theme(#legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
BL3

JL3 <- ggplot(PAR_JL,  aes(Depth, Value)) + #use depth and PAR data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Photosynthetically Active Radiation (umol Einsteins m-2 s-1")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  scale_y_log10() + #need log for exponential decay
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
JL3

DL3 <- ggplot(PAR_DL,  aes(Depth, Value)) + #use depth and PAR data as x and y
  geom_line(aes(color=Date)) + #plot points, each date is a shape, also legend name
  ylab("Photosynthetically Active Radiation (umol Einsteins m-2 s-1")+
  xlab("Depth (m)") +
  coord_flip() + #geom line won't work to connect observations when on a y-axis
  scale_x_reverse() + #make depth going down
  scale_y_log10() + #need log for exponential decay
  theme_classic() + #clean white and black
  theme(legend.position = "none", #remove legend
        strip.text.x = element_text(size = 8), #change font size of facet label
        axis.text = element_text(size = 8), #change font size of axis grid label
        axis.title = element_text(size = 9))  #change font size of axis label
DL3

#################################### Line up and export plots ##############
# line up plots in a row and label them
PAR<-ggarrange(AL3, BL3, JL3, DL3, # names of two plots in order desired
                     labels = c("A", "B", "C", "D"), # labels plots
                     ncol = 1, nrow = 1, align = "h", # 4 across; align horizontally
                     legend = c("bottom"),
                     common.legend=TRUE) 
PAR

#save plot in WD as pdf with dimensions that you want
ggsave("PAR.pdf", device = "pdf", plot=PAR, width=7, height=5, units="in") 


