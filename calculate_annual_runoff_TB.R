setwd("C:/Users/KristineT.SCCWRP2K/Documents/Git/TJR_sedimentflux/")

Q = read.csv("IBWC2000-2021.csv",header=TRUE,skip=1)
Q = Q[1:(nrow(Q)-1),]  # Last row is a disclaimer
Q$Date.time = strptime(Q$Timestamp..UTC.08.00.,format="%Y-%m-%d %H:%M:%S")
Q$Year = as.numeric(format(Q$Date.time,"%Y"))
Q$Month = as.numeric(format(Q$Date.time,"%m"))
Q$Q.cms = as.numeric(Q$Value..Cubic.Meters.Per.Second.)
Q$WY = Q$Year
Q$WY[Q$Month >= 10] = Q$WY[Q$Month >= 10]+1
Q$Date = as.Date(Q$Date.time)

# Calculate mean discharge over whole period
Q.ann = aggregate(Q$Q.cms,by=list(Q$WY),FUN="mean",na.rm=TRUE)
Q.daily = aggregate(Q$Q.cms,by=list(Q$Date),FUN="mean",na.rm=TRUE)
names(Q.daily) = c("Date","Qmean")
Q.daily$Date = as.Date(Q.daily$Date)
Q.daily$Year = as.numeric(format(Q.daily$Date,"%Y"))
Q.daily$Month = as.numeric(format(Q.daily$Date,"%m"))
Q.daily$WY = Q.daily$Year
Q.daily$WY[Q.daily$Month>=10] = Q.daily$WY[Q.daily$Month>=10]+1

Q.daily.2020 = Q.daily[Q.daily$Year==2020,]

Q.daily = Q.daily[!is.na(Q.daily$Qmean),]

Q.daily.N = aggregate(Q.daily$Qmean,by=list(Q.daily$WY),FUN="length")
names(Q.ann) = c("WY","Q.cms")
area = 1138 # km2

Q.ann$Q.mm = Q.ann$Q.cms*3600*24*365/(1000*1138)

write.csv(Q.ann,"Qann.mm.allQ.csv")
