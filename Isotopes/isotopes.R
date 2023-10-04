
########################### Analysis of d2H and d18O data #################

#load required packages
require("tidyverse") #for data wrangling and plotting
require("readxl") #for importing data
require("lubridate") #for parsing dates
require("ggpubr") #for lining up plots and exporting
require("RcppRoll") #for calculating rolling averages

############################### Import & Wrangle Data ##############################
#import dataset
df <- read_excel("~/Documents/R/Itasca/Itasca_database_2022_v2.xlsx", 
                                      col_types = c("text", "date", "text", 
                                                    "numeric", "text", "text", "numeric", 
                                                    "text", "text", "text", "numeric"))
#drop date so month and year become categorical
df$month <- format(df$Date, format="%Y-%m")

## Assign data names and constraints
d2H<-df %>%
  filter(Measurement=="delta2HH2O") %>%
  rename(d2HH2O=Value) %>%
  select(Lake, month, Depth, d2HH2O) %>%
  rename(Date=month)
  
d18O<-df %>%
  filter(Measurement=="delta18OH2O") %>%
  rename(d18OH2O=Value) %>%
  select(Lake, month, Depth, d18OH2O) %>%
  rename(Date=month)

#combine the two tables for cross-plotting in a long format
d2Hd18O <- d18O %>%
  left_join(d2H, by = NULL) 

############################# Plot depth distribution of isotopes ##############

#make a plot of d18O vs depth by lake 
d18O_depth <- ggplot(d18O,  aes(d18OH2O, Depth)) + #use depth and d18O data as x and y
  geom_point(aes(shape=Date)) + #plot points, each date is a shape, also legend name
  ylab("Depth (m)")+
  xlab("d18O (%)") +
  scale_y_reverse() + #revese x and y axis
  theme_classic() + #clean white and black
  theme(legend.position = "none") +
  facet_grid(~Lake) #facet by lake to compare distributions
d18O_depth

#make a plot of d18O vs depth by lake 
d2H_depth <- ggplot(d2H,  aes(d2HH2O, Depth)) +
  geom_point(aes(shape=Date)) +
  ylab("Depth (m)")+
  xlab("d2H (%)") +
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none") +
  facet_grid(~Lake)
d2H_depth

######################### plot box & whisker plots ############################

#d18O data
d18O_dist <- ggplot(d18O, aes(d18OH2O,Lake)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.title.y=element_blank())  #remove y axis labels
d18O_dist

max_DL <- max(d2Hd18O$d18OH2O)
min_DL <- min(d2Hd18O$d18OH2O)
diff_DL <- max_DL-min_DL

#d2H data
d2H_dist <- ggplot(d2H, aes(d2HH2O,Lake)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.title.y=element_blank())  #remove y axis labels
d2H_dist

######################### plot data on MWL #####################################

#calculate a linear regression of the Itasca data
fit <- lm(d2HH2O ~ d18OH2O, data = d2Hd18O)
coef(fit)

#calculate r squared value for fit
summary(fit)$r.squared 

#4 putative groundwaters sampled in Spring 2022
#2 Elk lake spring points 2010 and 2011 taken from the Minnesota Spring Inventory
#add in spring and source values
O <- c(-6.83,-10.90,-10.23,-9.34, -9.76, -9.9)
H <- c(-61.40,-81.36,-77.36,-73.92, -74.3, -75.8)
site <- c("Deming Bog", "Spring", "Elk3","Elk4","Elk2011", "Elk2010")

#make a tibble of this data
GW <- tibble(site,H,O)

#make a crossplot of d18O on x vs. d2H on y 
crossplot <- ggplot(d2Hd18O, aes(d18OH2O, d2HH2O)) + 
  geom_point(aes(shape=Lake), size=2) +
  scale_shape_manual(values=c(0, 1, 2, 6))+
  geom_point(data=GW, mapping = aes(x=O, y=H), shape =3) +
  ylab("d2H-H2O (%)")+
  xlab("d18O-H2O (%)") +
  theme_classic() +
  theme(legend.position=c(0.8,0.3)) +
  #geom_abline(intercept = 6.41, slope = 7.76) + #LMWL Shingobee; Kendall et al., 1997
  geom_abline(intercept = 9.41, slope = 7.93) + #LMWL Marcell Exp Forest; Stelling et al., 2021
  geom_abline(intercept = -28.809443, slope = 4.722778, linetype="dashed") + #linear model of Itasca lakes
  xlim(-15,0) +
  ylim(-90, -40)
crossplot

#make a crossplot of d18O on x vs. d2H on y only for Deming

D_d2Hd18O <- d2Hd18O %>%
  filter(Lake=="Deming")

D_crossplot <- ggplot(D_d2Hd18O, aes(d18OH2O, d2HH2O)) + 
  geom_point(aes(shape=Date), size=2) +
  scale_shape_manual(values=c(0, 1, 2, 6))+
  geom_point(data=GW, mapping = aes(x=O, y=H), shape =3) +
  ylab("d2H-H2O (%)")+
  xlab("d18O-H2O (%)") +
  theme_classic() +
  theme(legend.position=c(0.8,0.3)) +
  #geom_abline(intercept = 6.41, slope = 7.76) + #LMWL Shingobee; Kendall et al., 1997
  geom_abline(intercept = 9.41, slope = 7.93) + #LMWL Marcell Exp Forest; Stelling et al., 2021
  geom_abline(intercept = -28.809443, slope = 4.722778, linetype="dashed") + #linear model of Itasca lakes
  xlim(-15,0) +
  ylim(-90, -40)
D_crossplot

#################################### Line up and export plots ##############

# line up depth plots in a row and label them
Depths<-ggarrange(d18O_depth, d2H_depth, # names of two plots in order desired
                     labels = c("A", "B"), # labels plots
                     ncol = 2, nrow = 1, align = "h", # 2 across; align horizontally
                  legend = c("bottom"),
                  common.legend=TRUE) 
Depths

#save depth plot as pdf with dimensions that you want
ggsave("Depths.pdf", device = "pdf", plot=Depths, width=7, height=3.5, units="in") 



# line up box plots in a row and label them
Boxes<-ggarrange(d18O_dist, d2H_dist, # names of two plots in order desired
                  labels = c("A", "B"), # labels plots
                  ncol = 2, nrow = 1, align = "h") # 2 across; align horizontally
Boxes

#save box plots in WD as pdf with dimensions that you want
ggsave("Boxes.pdf", device = "pdf", plot=Boxes, width=7, height=3.5, units="in") 

#save MW plot
ggsave("MW.pdf", device = "pdf", plot=crossplot, width=3.5, height=3, units="in")


############################# Calculate seasonal d18O in water vapor ############

#Engel and Magner 2019 calculates the d18O of water vapor with an equation for temperature. 
#Let's populate the equation with temperature data and plot it as a time series

#import a wind speed data curated from 
wnd <- read_csv("~/Documents/R/Itasca/wind/glff_data_20221013033928.csv", 
                col_types = cols(dattim = col_datetime(format = "%Y%m%d%H%M")))

#A data frame of wind speeds (in m/s). Can be loaded using load.ts if in the same format as wtr
wnd <- wnd %>%
  rename(datetime="dattim", temp = "tmpf") %>% #rename columns 
  select(datetime, temp) %>% #select only column with wind speed (now a single variable)
  mutate(temp=(temp-32)*(5/9)) %>% #convert temp from F to C
  mutate(d18Ou=0.535*temp-15.17) %>% #calculate upper band of d18O water vapor
  mutate(d18Ol=0.507*temp-15.17) %>% #calculate upper band of d18O water vapor
  na.omit()
  
#convert datetime to POSIXct with UTC tz
(wnd$datetime <- as.POSIXct(wnd$datetime, format = "%Y%m%d%H%M", tz="UTC"))

#reduce to daily values 
daily_avg.wnd <- wnd %>% 
  group_by(datetime = floor_date(x = datetime, unit = "day")) %>% 
  summarise(d18Ou = mean(d18Ou, na.rm = T), d18Ol = mean(d18Ol, na.rm = T))


#calculate amplitude
max<-max(daily_avg.wnd$d18Ou)
min<-min(daily_avg.wnd$d18Ou)
diff<-max-min


#calculate water residence time in days
tau <- (1/0.0712)*sqrt((diff/diff_DL)^2-1)

#plot time series
vapor <- ggplot(daily_avg.wnd, aes(x=datetime, y=d18Ou)) +
  geom_point() +
  theme_classic() +
  xlab("Date") +
  ylab("d18O-H2O")
vapor  

#save depth plot in WD as pdf with dimensions that you want
ggsave("Vapor.pdf", device = "pdf", plot=vapor, width=3.5, height=3.5, units="in") 
