######################################## required packages ######################################


require("gsw") #the gibbs sea water package described here https://rdrr.io/cran/gsw/
require("tidyverse") #tidyverse needed for data manipulations and plotting
require("RcppRoll") #for calculating rolling averages
require("ggpubr") #for lining up plots and exporting



################################ import YSI data ###########################################

#import a ysi dataset with only data needed for calculation, data as double, date column to date format
#change name for each file
(YSI <- read_csv("~/Documents/R/Itasca/Brunt-Vaisala/20210714_Deming.csv", 
                 col_types = cols(...9 = col_double(), 
                                  DATE = col_date(format = "%m/%d/%y")), 
                 skip = 5))

################################### Wrangle the dataframe ########################################

#rename columns and remove all unneeded columns, remove depths less than zero and NA
YSI <- YSI %>%
  rename(Depth="Depth (m)", Latitude = "GPS Latitude ()", Pressure = "Pressure (psi a)", 
         Salinity = "Sal (psu)", Temperature = "Temp (F)") %>% #rename columns to remove ()
  select(Depth, Latitude, Pressure, Salinity, Temperature) %>% #select only columns needed in calculations
  filter(Depth >=0.3) %>% #remove depths less than zero
  distinct(Depth, .keep_all = TRUE) %>% #remove any rows with duplicated depths
  slice(which(Depth > cummax(lag(Depth, default = 0)))) %>% #remove any rows where depth is less than the cumulative max
  mutate(Temperature = (Temperature-32)*(5/9), #convert temperature to C from F
         Pressure = (Pressure*0.689)) #convert pressure psi a to dbar


#make a value for latitude
latitude <- as.numeric(YSI[1,2])

#use RcppRoll to calculate moving averages of data as new vectors
(avg_Temperature <- roll_mean(YSI$Temperature, 10))
(avg_Pressure <- roll_mean(YSI$Pressure, 10))
(avg_Salinity <- roll_mean(YSI$Salinity, 10))
(avg_Depth <- roll_mean(YSI$Depth, 10))

#check to see that temp is clean
T_plot <- ggplot(YSI, aes(Temperature, Depth), color = 'blue') +
  geom_point() +
  scale_y_reverse() +
  xlab("Temperature (C)") +
  ylab("Depth (m)") +
  theme_classic()
T_plot

#check to see that sal is clean
S_plot <- ggplot(YSI, aes(Salinity, Depth), color = 'blue') +
  geom_point() +
  scale_y_reverse() +
  xlab("Salinity (PSU)") +
  ylab("Depth (m)") +
  theme_classic()
S_plot

#create a df of the averages
roll_avg <- data.frame(avg_Depth, avg_Pressure, avg_Salinity, avg_Temperature)



###################### Calculate and plot in situ density ############################

rho <- gsw_rho(avg_Salinity, avg_Temperature, avg_Pressure)

#Make new df of N2 vs. depth
rho <- rho %>%
  bind_cols(roll_avg$avg_Depth)

colnames(rho) <- c("rho", "Depth") 

#export rho data to csv
#change name for each file
write.csv(rho,"~/Documents/R/Itasca/Brunt-Vaisala/Deming_20210714_rho.csv", row.names = FALSE)

rho_plot <- ggplot(rho, aes(rho, Depth), color = 'blue') +
  geom_point() +
  scale_y_reverse() +
  xlab("Density (kg/m^2)") +
  ylab("Depth (m)") +
  theme_classic() 
rho_plot

#calculate average rho above and below chemocline
#change rows to slice
mixo_rho <- rho %>%
  slice(1:14) %>%
  summarise(mean = mean(rho))

mono_rho <- rho %>%
  slice(15:16) %>%
  summarise(mean = mean(rho))

############calculate meromictic stability (Campbell 1970 thesis) ####################

#import bathymetric data
bth <- read_table("~/Documents/R/Itasca/Schmidt_stability/deming_1m.csv")
 
#extract the surface area in m^2
Ao <- bth %>%
  filter(Depth == 0) %>% #remove all rows by zero depth
  select(Area) #remove depth column leaving only a single variable vector

#import volume data
vol <- read_csv("~/Documents/R/Itasca/Volume_by_depth_all_lakes.csv", 
                                            skip = 1)

#name columns
colnames(vol) <- c("BL_depth", "BL_volup", "BL_voldown", "Blank",
                   "JL_depth", "JL_volup", "JL_voldown", "Blank",
                   "DL_depth", "DL_volup", "DL_voldown", "Blank", 
                   "AL_depth", "AL_volup", "AL_voldown")

#just took volumes from the df
Vmix <- 154571.696
Vmoni <- 122852.773

#calculate meromictic stability per Campbell Thesis on Lake 120, 
#but convert units per Hongve 1999 Nordic Hydrology

S <- 10*Vmoni*((mono_rho-mixo_rho)/Ao)*0.98



###################### Calculate Brunt-Vaisala for all depths gsw_Nsquared #####################

#Do calculation with rolled average strings
r <- gsw_Nsquared(avg_Salinity, avg_Temperature, avg_Pressure, latitude)

#This was example code from gsw documentation but I'm not sure if I need it
#stopifnot(all.equal(r$N2*1e3, c(0.060843209693499, 0.235723066151305, 0.216599928330380,
#                                0.012941204313372, 0.008434782795209)))
#stopifnot(all.equal(r$p_mid, c(30, 87.5, 187.5, 425, 800)))


################################### Plot the Brunt-Vaisala for all depths #######################

#remove first row of dataframe so it is equal length and corresponding to depths of N2 values
Lake = roll_avg[-1,]

#Make new df of N2 vs. depth
N2 <- bind_cols(r$N2, Lake$avg_Depth) 

#add column names
colnames(N2) <- c("N2", "Depth") 

#remove NA and infinite and outliers that are very small
N2 <- N2[!is.infinite(rowSums(N2)),]
N2 <- na.omit(N2)
N2 <- filter(N2, N2 >0.0000001)

#export df to csv
write.csv(N2,"~/Documents/R/Itasca/Brunt-Vaisala/N2.csv", row.names = FALSE)

#Plot N2 vs. depth on reversed axis
N2_plot <- ggplot(N2, aes(Depth, N2), color = 'blue') +
  geom_line() + 
  coord_flip() +
  scale_x_reverse() +
  scale_y_log10() +
  ylab("N2 (s^-2)") +
  xlab("Depth (m)") +
  theme_classic() 
N2_plot

#################################### Line up and export plots ##############
# line up plots in a row and label them
Stability<-ggarrange(T_plot, S_plot, rho_plot, N2_plot, # names of two plots in order desired
                   labels = c("A", "B", "C", "D"), # labels plots
                   ncol = 4, nrow = 1, align = "h") # 2 across; align horizontally
Stability

#save plot in WD as pdf with dimensions that you want
ggsave("Stability.pdf", device = "pdf", plot=Stability, width=180, height=100, units="mm") 

#################################### Calculate rho for non-averaged data #####

#use RcppRoll to calculate moving averages of data as new vectors
Temperature <- YSI$Temperature
Pressure <- YSI$Pressure
Salinity <- YSI$Salinity
Depth <- YSI$Depth

rho2 <- gsw_rho(Salinity, Temperature, Pressure)

#Make new df of N2 vs. depth
rho2 <- rho2 %>%
  bind_cols(YSI$Depth)

colnames(rho2) <- c("rho", "Depth") 

#export rho data to csv
write.csv(rho2,"~/Documents/R/Itasca/Brunt-Vaisala/Deming_20210714_rho2.csv", row.names = FALSE)
