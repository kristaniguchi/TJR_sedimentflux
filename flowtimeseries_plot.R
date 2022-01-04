#flow timeseries entire POR

library("dplyr")
library("tidyverse")

#setwd("C:/Users/KristineT/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/IBWC_Q/historical/")
setwd("C:/Users/KristineT.SCCWRP2K/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/IBWC_Q/historical/")

#historical flows from IBWC Tijuana River at International Boundary: https://www.ibwc.gov/Water_Data/histflo3.htm

Q = read.csv("IBWC_historicalmeandailyQ_TJintlborder.csv",header=TRUE)

names(Q) <- c("Date", "Q.cms")
Q$Date.time = strptime(Q$Date,format="%m/%d/%Y")
Q$Year = as.numeric(format(Q$Date.time,"%Y"))
Q$Month = as.numeric(format(Q$Date.time,"%m"))
Q$Q.cms = as.numeric(Q$Q.cms)
Q$WY = Q$Year
Q$WY[Q$Month >= 10] = Q$WY[Q$Month >= 10]+1
Q$Date = as.Date(Q$Date.time)

#subset out WY 2000-present (first date in 2000 dataset is 6/1/00)
Q.1 <- Q %>% 
  filter(Date < "2000-06-01")

####read in 2000-2019 ibwc data
Q2 = read.csv("IBWC2000-2021.csv",header=TRUE,skip=1)
Q2 = Q2[1:(nrow(Q2)-1),]  # Last row is a disclaimer
Q2$Date.time = strptime(Q2$Timestamp..UTC.08.00.,format="%Y-%m-%d %H:%M:%S")
Q2$Year = as.numeric(format(Q2$Date.time,"%Y"))
Q2$Month = as.numeric(format(Q2$Date.time,"%m"))
Q2$Q.cms = as.numeric(Q2$Value..Cubic.Meters.Per.Second.)
Q2$WY = Q2$Year
Q2$WY[Q2$Month >= 10] = Q2$WY[Q2$Month >= 10]+1
Q2$Date = as.Date(Q2$Date.time)

# Calculate mean discharge over whole period
Q2.daily = aggregate(Q2$Q.cms,by=list(Q2$Date),FUN="mean",na.rm=TRUE)
names(Q2.daily) = c("Date","Q.cms")
Q2.daily$Date = as.Date(Q2.daily$Date)
Q2.daily$Year = as.numeric(format(Q2.daily$Date,"%Y"))
Q2.daily$Month = as.numeric(format(Q2.daily$Date,"%m"))
Q2.daily$WY = Q2.daily$Year
Q2.daily$WY[Q2.daily$Month>=10] = Q2.daily$WY[Q2.daily$Month>=10]+1

#join two datasets
Q.all <- Q %>% 
  full_join(Q2.daily)

#plot timeseries
timeseries <- ggplot(Q.all, mapping = aes(x=Date, y = Q.cms)) +
  geom_line() + 
  #geom_point(data=)
  ylab("Discharge (cms)") +
  xlab("Date") +
  #scale_color_manual(values = c("black", "red"), name="Time Period") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(timeseries)

#save
file.name <- "timeseriesplot_19622019.jpg"
ggsave(timeseries, filename=file.name, dpi=300, height=4, width=5)

