
library("rLakeAnalyzer")
library("ggplot2")
library("tidyverse")
library("lubridate")
library("readr")
# install.packages("filled.contour")
# library("filled.contour")

#rLakeAnalyzer requires data to be organized in specific columns and with specific names
#first colume must be named "datetime" and the time must be in YYYY-MM-DD HH:MM:SS format, for example 2020-05-20 09:30:00.
#The proceeding columns will be depths and whatever data is recorded at those depths (for this code we detected temp).
#The depths columns must be in VAR_##.# format. For example, a depth column of 1.5m would be written VAR_1.5.
#Save this data as a .txt file.

#To obtain datafile set up a path
path.hourlysave<-'~/Documents/R/MN_lakes/Timeseries/CombinedData_Arco.txt'

#load in the time-series data using the path you just created above
wtr.hourlytemp=load.ts(path.hourlysave)

#to create the color/gradient map and constrain the z column - in this case to make our temperature key have a range from 0 to 30
wtr.heat.map(wtr.hourlytemp, zlim=c(0,30))

#to plot each temperature time series as lines
wtr.lineseries(wtr.hourlytemp)

#to plot thermocline and metalimnion boundaries
wtr.plot.temp(wtr.hourlytemp)

#calculate buoyancy (and seasonal) frequency on hourly sorted data
N2 = ts.buoyancy.freq(wtr.hourlytemp, seasonal=FALSE)
SN2 = ts.buoyancy.freq(wtr.hourlytemp, seasonal=TRUE)

#remove NA rows
N2 <- na.omit(N2)
SN2 <- na.omit(SN2)

#check for negatives
N2$n2 < 0 

#join N2 and SN2
all_N2 <- full_join(N2, SN2, by = 'datetime')

#rename columns
all_N2 <- all_N2 %>% 
  rename(
    N2 = n2.x,
    SN2 = n2.y
  )

#gather N2 and SN2 by date
all_N2 <- all_N2 %>%
  gather(Type,N2,N2:SN2)
all_N2

#plot the buoyancy on hourly sorted data and check to see if N2 and SN2 are different
all_N2_plot <-ggplot(all_N2, aes(datetime, N2, label=Type)) +
  geom_line(aes(x=datetime, y=N2, color=Type)) +
  xlab("Date") +
  ylab("Brunt-Vaisala buoyancy frequency (s-2)")
all_N2_plot

#plot just N2
N2_plot <-ggplot(N2, aes(datetime, n2)) +
  geom_line(aes(x=datetime,y=n2)) +
  xlab("Date") +
  ylab("Brunt-Vaisala buoyancy frequency (s-2)") +
  theme_classic()
N2_plot

#calculate the thermocline depth
td = ts.thermo.depth(wtr.hourlytemp)

#plot the thermocline depth
td_plot <-ggplot(td, aes(datetime, thermo.depth)) +
  geom_line() +
  scale_y_reverse() +
  xlab("Date") +
  ylab("Depth (m)") +
  theme_classic()
td_plot

################################# Calculate uStar using wind speeds ###########

#uStar is the water friction velocity due to wind stress at the lake surface, it is calculated following
#the methods of Imberger (1985) as a function of the shear stress of air (Fischer et al., 1979), drag
#coefficient for momentum (Hicks, 1972), and a dimensionless constant (von Karman constant) that
#decribes the logarithmic velocity profile at the air-water interface

#use uStar if you have a vector of epilimnion density
#or use ts.uStar if you have a vector of water temperatures

#import a wind speed data curated from 
wnd <- read_csv("~/Documents/R/MN_lakes/wind/glff_data_20221013034114.csv", 
                col_types = cols(dattim = col_datetime(format = "%Y%m%d%H%M")))

#A data frame of wind speeds (in m/s). Can be loaded using load.ts if in the same format as wtr
wnd <- wnd %>%
  rename(datetime="dattim", wnd = "wspd") %>% #rename columns 
  select(datetime, wnd) %>% #select only column with wind speed (now a single variable)
  mutate(wnd=wnd/2.237)  #convert wind speed from mph to m/s

#Height of the anemometer above the ground surface in meters
wnd.height <- 6.1

bth <- read_table("~/Documents/R/MN_lakes/Schmidt_stability/arco_1m.csv")

#rename columns
bth <- bth %>%
  rename(depths="Depth", areas = "Area")  #rename columns 

#extract the surface area in m^2
Ao <- bth %>%
  filter(depths == 0) %>% #remove all rows by zero depth
  select(areas) #remove depth column leaving only a single variable vector

#run uStar calculation for each time point
uStar <- ts.uStar(wtr.hourlytemp, wnd, wnd.height, bth, seasonal = TRUE)

#export df to csv
write.csv(uStar,"~/Documents/R/MN_lakes/Timeseries/Arco_uStar.csv", row.names = FALSE)

#The Lake Number, defined by Imberger and Patterson (1990), has been used to describe processes
#relevant to the internal mixing of lakes induced by wind forcings. Lower values of Lake Number
#represent a higher potential for increased diapycnal mixing, which increases the vertical flux of
#mass and energy across the metalimnion through the action of non-linear internal waves. Lake
#Number is a dimensionless index.

#calculate lake number
lake_num <- ts.lake.number(wtr.hourlytemp, wnd, wnd.height, bth, seasonal = TRUE)

#export df to csv
write.csv(lake_num,"~/Documents/R/MN_lakes/Timeseries/Arco_lake_num.csv", row.names = FALSE)

#plot lake number
lake.number.plot(wtr.hourlytemp, wnd, wnd.height, bth)

#calculate Wedderburn number
wedderburn <- ts.wedderburn.number(wtr.hourlytemp, wnd, wnd.height, bth, Ao, seasonal = TRUE)

#Calculates the internal energy of the water column with temperature and hypsography
#Internal energy is the thermal energy in the water column, which is calculated by multiplying the
#specific heat of water (J kg-1 K-1) by the temperature and mass of the water in the lake.
internal_energy <- ts.internal.energy(wtr.hourlytemp, bth, na.rm = FALSE)

#export df to csv
write.csv(internal_energy,"~/Documents/R/MN_lakes/Timeseries/Arco_internal_energy.csv", row.names = FALSE)



