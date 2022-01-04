library("dplyr")
library("tidyverse")

#setwd("C:/Users/KristineT/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/IBWC_Q/historical/")
setwd("C:/Users/KristineT.SCCWRP2K/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/IBWC_Q/historical/")

#historical 1962-2010 annual flows from IBWC Tijuana River at International Boundary: source https://www.ibwc.gov/Water_Data/histflo3.htm
Q.ann.19622010.ibwc <- read.csv("Qann.mm.allQ.historicalIBWC.csv")
#subset to 1962-2000 to combine with contemporary
Q.ann.19621999.ibwc <- Q.ann.19622010.ibwc %>% 
  filter(WY < 2001 & WY > 1962) #not full WY for 1962 in this dataset and contemporary is not full for 2000 year

#Contemporary 2000-2019 IBWC historical from mean daily data to IBWC contemporary using 5-min data
Q.ann.ibwc.20002019 <- read.csv(file = "Qann.mm.allQ.csv") %>% 
  filter(WY < 2020 & WY > 2000) #not full WY 2000, start at 2001
#combined with historical
Q.ann.19622019 <- Q.ann.19621999.ibwc %>% 
  full_join(Q.ann.ibwc.20002019)

#Cumulative distribution of annual runoff
#2000-2019 calculations
#total n
Q.ann.ibwc.20002019$n <- length(Q.ann.ibwc.20002019$Q.mm)
#rank m
Q.ann.ibwc.20002019$m <- rank(-Q.ann.ibwc.20002019$Q.mm)
#exceedence probability
Q.ann.ibwc.20002019$p <- (Q.ann.ibwc.20002019$m/(Q.ann.ibwc.20002019$n+1))
#return period (years) <- 1/p
Q.ann.ibwc.20002019$returnperiod <- 1/Q.ann.ibwc.20002019$p

# exceedence plot 2000-2019
ex.plot1 <- ggplot(Q.ann.ibwc.20002019, aes(x=Q.mm, y=p)) +
  geom_point() +
  geom_line()
print(ex.plot1)

#add time period as column
Q.ann.ibwc.20002019$period <- "WY 2000-2019"

#exceedence plot 1962-2019
#total n
Q.ann.19622019$n <- length(Q.ann.19622019$Q.mm)
#rank m
Q.ann.19622019$m <- rank(-Q.ann.19622019$Q.mm)
#exceedence probability
Q.ann.19622019$p <- (Q.ann.19622019$m/(Q.ann.19622019$n+1))
Q.ann.19622019$returnperiod <- 1/Q.ann.19622019$p

# exceedence plot 2000-2019
ex.plot2 <- ggplot(Q.ann.19622019, aes(x=Q.mm, y=p)) +
  geom_line(color="red") 

print(ex.plot2)

#combined plot
ex.plot.all <- ex.plot2 +
  #geom_line(Q.ann.ibwc.20002019, mapping = aes(x=Q.mm, y=p)) +
  geom_line(Q.ann.ibwc.20002019, mapping= aes(x=Q.mm, y=p))
print(ex.plot.all)

#add time period as column
Q.ann.19622019$period <- "WY 1963-2019"

#join datasets together for plot
Q.ann.all.periods <- Q.ann.19622019 %>% 
  full_join(Q.ann.ibwc.20002019)

#find range of flows from 2000-2019
range20002019 <- range(ann.flow)
#range of flows from 1970-1978 
ann.flow2.sub <- Q.ann.19622019$Q.mm[Q.ann.19622019$WY > 1969 & Q.ann.19622019$WY < 1979]
range197078 <- range(ann.flow2.sub)

#flood frequency
flood.freq.curve <- ggplot(Q.ann.all.periods, mapping = aes(x=Q.mm, y = p, color=period)) +
  geom_line() + 
  #geom_point(data=)
  ylab("Exceedence Probability of Annual Runoff") +
  xlab("Annual Runoff (mm)") +
  scale_color_manual(values = c("black", "red"), name="Time Period") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(flood.freq.curve)

#save
file.name <- "freq.curve.annualQ.jpg"
ggsave(flood.freq.curve, filename=file.name, dpi=300, height=4, width=5)








ggplot(Q.ann.19622019, mapping = aes(x=Q.mm, y = p)) +
  geom_line() + 
  geom_point(data=)
ylab("Cumulative Probability of Annual Runoff") +
  xlab("Annual Runoff (mm)") +
  #scale_color_manual(values = c("black", "grey"), name="Time Period") +
  #geom_vline(xintercept = range20002019[1], linetype="dotted") +
  #geom_vline(xintercept = range20002019[2], linetype="dotted") +
  #geom_vline(xintercept = range197078[1], linetype="dashed") +
  #geom_vline(xintercept = range197078[2], linetype="dashed") +
  
  #geom_segment(x=range20002019[1], y = 0, xend = range20002019[2], yend=0) +
  #geom_segment(x=range197078[1], y = 3, xend = range197078[2], yend=3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#combine dataframes Q.ann.19622019 and 



#cumulative frequency
###EXAMPLE
duration = faithful$eruptions 
#breaks are the intervals to get frequency from
breaks = seq(1.5, 5.5, by=0.5) 
#cut determines which interval each value falls in
duration.cut = cut(duration, breaks, right=FALSE) 
#creates a table of frequencies for each interval
duration.freq = table(duration.cut)
#calc cumulative frequency with cumsum, add a strating zero element, and plot on graph
cumfreq0 = c(0, cumsum(duration.freq)) 
plot(breaks, cumfreq0,            # plot the data 
  main="Old Faithful Eruptions",  # main title 
  xlab="Duration minutes",        # x−axis label 
  ylab="Cumulative eruptions")   # y−axis label
lines(breaks, cumfreq0) 


#cumulative frequency
ann.flow <- Q.ann.ibwc.20002019$Q.mm
#breaks are the intervals to get frequency from
breaks = seq(0, 645, by=1) 
#cut determines which interval each value falls in
flow.cut = cut(ann.flow, breaks, right=FALSE) 
#creates a table of frequencies for each interval
duration.freq = table(flow.cut)
#calc cumulative frequency with cumsum, add a strating zero element, and plot on graph
cumfreq0 = c(0, cumsum(duration.freq)) 
plot(breaks, cumfreq0,            # plot the data 
     main="IBWC 2000-2019",  # main title 
     xlab="Total Annual Discharge (mm)",        # x−axis label 
     ylab="Cumulative Years")   # y−axis label
lines(breaks, cumfreq0) 

cumfreq.20002019 <- data.frame(cbind(breaks, cumfreq0)) %>% 
  mutate(time = "2000-2019")

#cumulative frequency for 1962-2019
ann.flow2 <- Q.ann.19622019$Q.mm
#breaks are the intervals to get frequency from
breaks = seq(0, 645, by=1) 
#cut determines which interval each value falls in
flow.cut2 = cut(ann.flow2, breaks, right=FALSE) 
#creates a table of frequencies for each interval
duration.freq2 = table(flow.cut2)
#calc cumulative frequency with cumsum, add a strating zero element, and plot on graph
cumfreq0 = c(0, cumsum(duration.freq2)) 
plot(breaks, cumfreq0,            # plot the data 
     main="IBWC 1962-2019",  # main title 
     xlab="Total Annual Discharge (mm)",        # x−axis label 
     ylab="Cumulative Years")   # y−axis label
lines(breaks, cumfreq0) 

cumfreq.19622019 <- data.frame(cbind(breaks, cumfreq0)) %>% 
  mutate(time = "1962-2019")

#join the two datasets and plot together as 2 separate lines
join.cum.freq <- cumfreq.19622019 %>% 
  full_join(cumfreq.20002019)

ggplot(join.cum.freq, mapping = aes(x=breaks, y = cumfreq0, color=time)) +
  geom_line() + 
  ylab("Cumulative Frequency (years)") +
  xlab("Total Annual Discharge (mm)") +
  scale_color_manual(values = c("black", "grey"), name="Time Period") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  
