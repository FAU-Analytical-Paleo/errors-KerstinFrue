
# Question 1 --------------------------------------------------------------
radians <- function(d, m, s){
  rad <- pi*(d+m/60+s/3600)/180
  return(rad)
}

height <- tan(radians(1,21,0))*(2550)
uncertainty_height <- (tan(radians(0,1,0))/tan(radians(1,21,0))+25/2550)*height
round(height, digits=2)
round(uncertainty_height, digits=2)

# Question 2 --------------------------------------------------------------

#25,53 +- 0,1 Ma ; (26600000 +- 100,000)
#29,66 +- 0,2 Ma ; (25530000 +- 200,000)
((100000/26600000) + (200000/25530000))
26600000 - 25530000

d<-("1.07 Ma +- 0.01 Ma")
d

# Question 3 --------------------------------------------------------------


ex3<-read.csv2("ex3_eqscals.csv", header = FALSE, sep ="", col.names=c("X in km)","r in m", "Mo in Nm"))


# 3 a) --------------------------------------------------------------------

is.character(ex3$X.in.km.)
num_x<-as.numeric(ex3$X.in.km.)
num_r<-as.numeric(ex3$r.in.m)
num_Mo<-as.numeric(ex3$Mo.in.Nm)


#Mean, median, standard deviation and MAD of r

mean.r<-mean(num_r)
median.r<-median(num_r)
sd.r<-sd(num_r)
MAD.r<-mad(num_r)

round(mean.r, digits=2)
round(median.r, digits=2)
round(sd.r, digits=2)
round(MAD.r, digits=2)


#Mean, median, standard deviation and MAD of Mo


mean.Mo<-mean(num_Mo)
median.Mo<-median(num_Mo)
sd.Mo<-sd(num_Mo)
MAD.Mo<-mad(num_Mo)

round(mean.Mo, digits=2)
round(median.Mo, digits=2)
round(sd.Mo, digits=2)
round(MAD.Mo, digits=2)


# 3 b) --------------------------------------------------------------------

x11()
par(mfrow=c(1,2))
plot(ex3$r.in.m, xlab="measurement n", ylab=("r in m"), col = "orange", main="Estimated fault radius ")
plot(ex3$Mo.in.Nm, xlab="measurement n", ylab=("Mo in Nm"), col="blue", main="Seismic moment [earthquake magnitude]")
?plot


# 3 c) --------------------------------------------------------------------

trim.r<-num_r[num_r<(median.r+(3*MAD.r))]
trim.r
num_r

trim.Mo<-num_Mo[num_Mo<(median.Mo+(3*MAD.r))]
trim.Mo
num_Mo

#Recalculating mean, median and standard deviation

mean.trim.r<-mean(trim.r)
median.trim.r<-median(trim.r)
sd.trim.r<-sd(trim.r)

round(mean.trim.r, digits=2)
round(median.trim.r, digits=2)
round(sd.trim.r, digits=2)

  

mean.trim.Mo<-mean(trim.Mo)
median.trim.Mo<-median(trim.Mo)
sd.trim.Mo<-sd(trim.Mo)

round(mean.trim.Mo, digits=2)
round(median.trim.Mo, digits=2)
round(sd.trim.Mo, digits=2)
