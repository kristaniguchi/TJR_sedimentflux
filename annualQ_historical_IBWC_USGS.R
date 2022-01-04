library("dplyr")
library("tidyverse")

#setwd("C:/Users/KristineT/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/IBWC_Q/historical/")
setwd("C:/Users/KristineT.SCCWRP2K/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/IBWC_Q/historical/")

#historical flows from IBWC Tijuana River at International Boundary: https://www.ibwc.gov/Water_Data/histflo3.htm

Q = read.csv("IBWC_historicalmeandailyQ_TJintlborder.csv",header=TRUE,skip=1)
Q = Q[1:(nrow(Q)-1),]  # Last row is a disclaimer

names(Q) <- c("Date", "Q.cms")
Q$Date.time = strptime(Q$Date,format="%m/%d/%Y")
Q$Year = as.numeric(format(Q$Date.time,"%Y"))
Q$Month = as.numeric(format(Q$Date.time,"%m"))
Q$Q.cms = as.numeric(Q$Q.cms)
Q$WY = Q$Year
Q$WY[Q$Month >= 10] = Q$WY[Q$Month >= 10]+1
Q$Date = as.Date(Q$Date.time)

# Calculate mean discharge over whole period
Q.ann = aggregate(Q$Q.cms,by=list(Q$WY),FUN="mean",na.rm=TRUE)
names(Q.ann) = c("WY","Q.cms")
area = 1138 # km2

Q.ann$Q.mm = Q.ann$Q.cms*3600*24*365/(1000*1138)
#Q ann in MCM
Q.ann$Q.MCM = Q.ann$Q.cms*3600*24*365/(1000000)


write.csv(Q.ann,"Qann.mm.allQ.historicalIBWC.csv")


#historical flows from UGSG TJR Nestor Gage - 1938-1977 https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=11013500&referred_module=sw&period=&begin_date=1936-10-01&end_date=1978-06-06

Q.usgs = read.csv("USGS_historicalmeandailyQ_nestorgage_hollister_1938_1977.csv",header=TRUE)
Q.usgs$Date.time = strptime(Q.usgs$datetime,format="%m/%d/%Y")
Q.usgs$Year = as.numeric(format(Q.usgs$Date.time,"%Y"))
Q.usgs$Month = as.numeric(format(Q.usgs$Date.time,"%m"))
Q.usgs$Q.usgs.cms = as.numeric(Q.usgs$mean.daily.Q.cfs)/35.314667
Q.usgs$WY = Q.usgs$Year
Q.usgs$WY[Q.usgs$Month >= 10] = Q.usgs$WY[Q.usgs$Month >= 10]+1
Q.usgs$Date = as.Date(Q.usgs$Date.time)

# Calculate mean discharge over whole period
Q.usgs.ann = aggregate(Q.usgs$Q.usgs.cms,by=list(Q.usgs$WY),FUN="mean",na.rm=TRUE)
names(Q.usgs.ann) = c("WY","Q.usgs.cms")
area = 1138 # km2

Q.usgs.ann$Q.usgs.mm = Q.usgs.ann$Q.usgs.cms*3600*24*365/(1000*1138)
#Q.usgs ann in MCM
Q.usgs.ann$Q.usgs.MCM = Q.usgs.ann$Q.usgs.cms*3600*24*365/(1000000)


write.csv(Q.usgs.ann,"Q.ann.mm.allQ.usgs.historical.csv")

#compare IBWC with USGS overlapping time period
Q.ann.USGS.IBWC <- Q.usgs.ann %>% 
  left_join(Q.ann, by= "WY") %>% 
  filter(WY > 1961) %>% 
  mutate(Q.MCM.IBWC = Q.MCM)

#1:1 line
x <- c(0, 8)
y <- c(0, 8)
ref <- data.frame(cbind(x, y))

#plot
ggplot(Q.ann.USGS.IBWC, aes(x=Q.usgs.MCM, y=Q.MCM.IBWC)) +
  geom_point() +
  geom_line(ref, mapping=aes(x=x, y=y))

#Compare IBWC historical from mean daily data to IBWC contemporary using 15-min data
Q.ann.ibwc.15 <- read.csv(file = "Qann.mm.allQ.csv")

#join with IBWC historical 
Q.ann.ibwc.join <- Q.ann.ibwc.15 %>% 
  left_join(Q.ann, by= "WY") %>% 
  filter(WY < 2010) %>% 
  mutate(Q.mm.IBWC.Hist = Q.mm.y) %>% 
  rename(Q.mm.IBWC = Q.mm.x)

#1:1 line
x <- c(0, 80)
y <- c(0, 80)
ref <- data.frame(cbind(x, y))

#plot
ggplot(Q.ann.ibwc.join, aes(x=Q.mm.IBWC.Hist, y=Q.mm.IBWC)) +
  geom_point() +
  geom_line(ref, mapping=aes(x=x, y=y))

