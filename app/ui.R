


library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(readr)

#########################################
#########################################
#########################################
#                                      ##
#                                      ##
#          ┌─┐       ┌─┐               ##
#       ┌──┘ ┴───────┘ ┴──┐            ##
#       │                 │            ##
#       │       ───       │            ##
#       │  ─┬┘       └┬─  │            ##
#       │                 │            ##
#       │       ─┴─       │            ##
#       │                 │            ##
#       └───┐         ┌───┘            ##
#           │         │                ##
#           │         │                ##
#           │         │                ##
#           │         └──────────────┐ ##
#           │                        │ ##
#           │                        ├─┐#
#           │                        ┌─┘#    
#           │                        │ ##
#           └─┐  ┐  ┌───────┬──┐  ┌──┘ ##        
#             │ ─┤ ─┤       │ ─┤ ─┤    ##     
#             └──┴──┘       └──┴──┘    ##
#                 神兽保佑             ##   
#                代码无BUG!            ##  
#########################################

# Create the data frame of restaurant inspection
# Keep only one record of each restaurant and drop NA rows
restaurant <- read.csv("../data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv", header=T, sep=",")
res_new <- res_new <- restaurant %>% 
  filter(INSPECTION.DATE>'01/01/1900' & BORO!='0' & BORO!='210' & GRADE!='') %>%
  group_by(CAMIS) %>%
  slice(1) %>%
  ungroup()
df_res = res_new[, c('CAMIS', 'GRADE', 'BORO')]
df_res = na.omit(df_res)
df_res

# Let grades other than A, B, and C be OTHER
df_res$grade = ifelse((df_res$GRADE=='A')|(df_res$GRADE=='B')|(df_res$GRADE=='C'), df_res$GRADE, 'OTHER')
class(df_res)
counts = table(df_res$BORO, df_res$grade)
total = rowSums(counts)
A_amounts = counts[, "A"]
ratios = A_amounts/total

# Create a new data frame of counts and ratios
boro = c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island')
df_counts = as.data.frame.matrix(counts)
df_counts$Ratios = ratios
df_counts$Borough = boro
df_counts



#data visilization
# Create the bar plot

# Add a line and points for the portion of A


df_counts
#export dataset
write.csv(df_res, "df_res.csv", row.names=FALSE)
write.csv(df_counts, "df_counts.csv", row.names=FALSE)


ggplot() + geom_col(aes(x = as.factor(BORO), y = 1, fill = grade), data = df_res, group = 1, position = "stack")
+ geom_line(aes(x = as.factor(df_counts$Borough), y = 39000*df_counts$Ratios, group = 1), color = "red",lwd=1)+ 
  geom_point(aes(x = as.factor(df_counts$Borough), y = 39000*df_counts$Ratios), color = "black") +
  scale_fill_manual(values=c("OTHER"='#E0A96D', "A"='#DDC3A5', "B"='gray24', "C"='seashell1')) +
  labs(title= "Restaurants Grade Distribution", x = "Borough", y = "Counts") +
  guides(fill=guide_legend(title="GRADE")) +
  scale_y_continuous(sec.axis=sec_axis(~.*1/400, name="Ratios of Grade A")) +
  theme(plot.title = element_text(hjust = 0.5))

###traffic
traffic <- read.csv("Automated_Traffic_Volume_Counts.csv", header=T, sep=",")
traffic$time <- with(traffic, ISOdatetime(Yr, M, D, HH, MM, 0L))

##group by different borough and sum up the volumes
mantot <- traffic %>% filter(Boro=='Manhattan') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
brotot <- traffic %>% filter(Boro=='Brooklyn') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
qnstot <- traffic %>% filter(Boro=='Queens') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
brxtot <- traffic %>% filter(Boro=='Bronx') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
sitot <- traffic %>% filter(Boro=='Staten Island') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')

#outer join dataframes
m1 <- merge(x=mantot,y=brotot,by="time", all=TRUE)
colnames(m1)[2] <- 'man'
colnames(m1)[3] <- 'bro'
m2 <- merge(x=m1,y=brxtot,by="time", all=TRUE)
colnames(m2)[4] <- 'brx'
m3 <- merge(x=m2,y=qnstot,by="time", all=TRUE)
colnames(m3)[5] <- 'qns'
m4 <- merge(x=m3,y=sitot,by="time", all=TRUE)
colnames(m4)[6] <- 'si'
m4[is.na(m4)] <- 0

#set a proper time period to avoid too much missing data
m4 <- m4[m4$time>'2019-09-10 00:00:00'&m4$time<'2019-11-25 00:00:00',]

#data visualization
x <- m4$time

plot(x, m4$man, type="o", col="cornflowerblue", lwd=3, pch="o", ylab="Traffic Volume Counts",xlab='Month',lty=1, main='Daily Traffic Volume Counts from 7am to 10am')

points(x, m4$bro, col="firebrick2", lwd=3, pch="*")
lines(x, m4$bro, col="firebrick2", lwd=3,lty=1)

points(x, m4$brx, col="burlywood1", lwd=3,pch="+")
lines(x, m4$brx, col="burlywood1", lwd=3, lty=1)

points(x, m4$qns, col="blueviolet", lwd=3,pch=5)
lines(x, m4$qns, col="blueviolet", lwd=3, lty=1)

points(x, m4$si, col="darkseagreen1", lwd=3,pch=7)
lines(x, m4$si, col="darkseagreen1", lwd=3, lty=1)

legend('topleft',legend=c("Manhattan","Brooklyn","Bronx",'Queens','Staten Island'), col=c("cornflowerblue","firebrick2","burlywood1",'blueviolet','darkseagreen1'),
       lwd=3,pch=c(1,8,3,5,7),cex=1,lty=1,ncol=1)

write.csv(m4, "morningtraffic.csv", row.names=FALSE)


traffic2 <- read.csv("Automated_Traffic_Volume_Counts2.csv", header=T, sep=",")
traffic2$time <- with(traffic2, ISOdatetime(Yr, M, D, HH, MM, 0L))
##group by different borough and sum up the volumes
man2 <- traffic2 %>% filter(Boro=='Manhattan') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
bro2 <- traffic2 %>% filter(Boro=='Brooklyn') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
qns2 <- traffic2 %>% filter(Boro=='Queens') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
brx2 <- traffic2 %>% filter(Boro=='Bronx') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')
si2 <- traffic2 %>% filter(Boro=='Staten Island') %>% arrange(time) %>% group_by(time) %>% summarise(total_vol=sum(Vol),.groups = 'drop')

#outer join dataframes
m21 <- merge(x=man2,y=bro2,by="time", all=TRUE)
colnames(m21)[2] <- 'man'
colnames(m21)[3] <- 'bro'
m22 <- merge(x=m21,y=brx2,by="time", all=TRUE)
colnames(m22)[4] <- 'brx'
m23 <- merge(x=m22,y=qns2,by="time", all=TRUE)
colnames(m23)[5] <- 'qns'
m24 <- merge(x=m23,y=si2,by="time", all=TRUE)
colnames(m24)[6] <- 'si'
m24[is.na(m24)] <- 0

#set a proper time period to avoid too much missing data
m24 <- m24[m24$time>'2019-09-10 00:00:00'&m24$time<'2019-11-25 00:00:00',]

#data visualization
x2 <- m24$time

plot(x2, m24$man, type="o", col="cornflowerblue", pch="o", ylab="Counts",xlab='Month', lwd=3,lty=1,main='Daily Traffic Volume Counts from 5pm to 8pm')

points(x2, m24$bro, col='firebrick2', lwd=3, pch="*")
lines(x2, m24$bro, col='firebrick2', lwd=3,lty=1)

points(x2, m24$brx, col="burlywood1", lwd=3,pch="+")
lines(x2, m24$brx, col="burlywood1", lwd=3, lty=1)

points(x2, m24$qns, col="blueviolet", lwd=3,pch=5)
lines(x2, m24$qns, col="blueviolet", lwd=3, lty=1)

points(x2, m24$si, col="darkseagreen1", lwd=3,pch=7)
lines(x2, m24$si, col="darkseagreen1", lwd=3, lty=1)

legend('topleft',legend=c("Manhattan","Brooklyn","Bronx",'Queens','Staten Island'), col=c("cornflowerblue","firebrick2","burlywood1",'blueviolet','darkseagreen1'),
       lwd=3,pch=c(1,8,3,5,7),cex=1,lty=1,ncol=1)

write.csv(m24, "eveningtraffic.csv", row.names=FALSE)


########################################