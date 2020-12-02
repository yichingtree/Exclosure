#######################################################################
#
#  Data analysis on Aug 6, 2020
#
######################################################################


#setwd("~/Desktop/MS_exclosure_2020/Data_analysis_2020")

#setwd("D:/YC_THU/Research/MS_in_progress/MS_exclosure_2020/Data_analysis_2020")

seedling.all=read.csv("data_file_2020/exclosure_2020_0807_fix.csv", na.strings=".")

str(seedling.all)

#'data.frame':	50073 obs. of  19 variables


sp.list.all=as.data.frame(table(seedling.all$csp))

names(sp.list.all)=c("csp", "no")

# Unknown   182
# 64 species + unknown

seedling.all=subset(seedling.all, csp!="Unknown")

str(seedling.all)

#'data.frame':	49891 obs. of  19 variables:

sp.list.all_known=as.data.frame(table(seedling.all$csp))

names(sp.list.all_known)=c("csp", "no")

#write.csv(sp.list.all_known, "Density_diversity_2020/sp.list.all_test.csv")


######################### Importing species code

spcode=read.csv("data_file_2020/spcode_0806_2020.csv", na.strings=".")

str(spcode)
#'data.frame':	182 obs. of  3 variables


seedling.all.merge=merge(seedling.all, spcode, by="csp", all.x=TRUE)

str(seedling.all.merge)

#'data.frame':	49891 obs. of  21 variables:


sp.list.merge=as.data.frame(table(seedling.all.merge$csp))

names(sp.list.merge)=c("sp", "no")

check.csp=as.matrix(table(seedling.all.merge$csp, seedling.all.merge$sp))


write.csv(check.csp, "Density_diversity_2020/check.csp.csv")

##########################################################
#  Extracting the tree and shrub species 
#
#########################################################

seedling.ts=subset(seedling.all.merge, growth_form=="T"|growth_form=="S")

str(seedling.ts)


#'data.frame':	42649 obs. of  21 variables

csp.list.ts=as.data.frame(table(seedling.ts$csp)) 


sp.list.ts=as.data.frame(table(seedling.ts$sp)) 

names(sp.list.ts)=c("sp", "no")

sp.list.ts=subset(sp.list.ts, no>0)

## 45 species 

sum(sp.list.ts$no)
#[1] 42649

######################################################################################
#
# Data analysis 
#
################################################################################## 

#  Correcting data
#seedling.ts$date=ifelse(seedling.ts$date==20140625, 20170625,seedling.ts$date)

#seedling.ts$status=ifelse(seedling.ts$status=="B", "L",seedling.ts$status)
#seedling.ts$status=ifelse(seedling.ts$status=="M3"|seedling.ts$status=="M4"|seedling.ts$status=="M5", ".",seedling.ts$status) 



# Change the format of species

seedling.ts$sp=as.character(seedling.ts$sp)

unique(seedling.ts$sp)

# CHange the format of height

seedling.ts$height=as.numeric(seedling.ts$height)

summary(seedling.ts$height)


# Creating a new variable 
seedling.ts$slplot=paste(seedling.ts$block, seedling.ts$pair,seedling.ts$plot, sep="." )

## Extract year
seedling.ts$year=as.numeric(substr(seedling.ts$date, 1, 4))

sort(unique(seedling.ts$year))


## Extract month
seedling.ts$month=as.numeric(substr(seedling.ts$date, 5, 6))

sort(unique(seedling.ts$month))

head(seedling.ts)

##  Changing the format of status

seedling.ts$status=as.character(seedling.ts$status)

unique(seedling.ts$status)

save(seedling.ts, file="data_file_2020/exclosure.working.2020.rdata")


##############################################
#
# Select seedlings from 14 pairs of plots 
#
############################################

load("data_file_2020/exclosure.working.2020.RData")

head(seedling.ts)

plot.id=sort(unique(seedling.ts$slplot))

# Remove two plots with extreme seedling densities

plot.id.adj=plot.id[-c(5,6)]
seedling.ts.adj=subset(seedling.ts, slplot%in%plot.id.adj)

str(seedling.ts.adj)

#'data.frame':	41563 obs. of  23 variables


sp.list.ts.adj=as.data.frame(table(seedling.ts.adj$sp))

names(sp.list.ts.adj)=c("sp", "no")

# 45 species 

sum(sp.list.ts.adj$no)
# [1] 41563 

######################################################
#
# Seedling density (Aug 7, 2020)
#
######################################################

head(seedling.ts.adj)

n.census=length(unique(seedling.ts.adj$census))

#21

## Importing the last census

last.census=paste("2020", "01", sep=".")

## Creating the census year

census.year=sort(unique(seedling.ts.adj$year))

## Creating the census month

census.month=c("01","04","07","10")

## Creating a vector for census time

census.time=vector()

for (i in 1:(length(census.year)-1))
{
  temp.year=rep(census.year[i], 4)
  temp=paste(temp.year, census.month, sep=".")
  census.time=c(census.time, temp)
}

census.time=c(census.time, last.census)

## Census time for 21 censuses
census.time
#[1] "2015.01" "2015.04" "2015.07" "2015.10" "2016.01" "2016.04" "2016.07" "2016.10" "2017.01" "2017.04"
#[11] "2017.07" "2017.10" "2018.01" "2018.04" "2018.07" "2018.10" "2019.01" "2019.04" "2019.07" "2019.10"
#[21] "2020.01"

## Creating the vectors

mean.den.census.ex=vector()
sd.den.census.ex=vector()

mean.den.census.con=vector()
sd.den.census.con=vector()

### Extract plot ids 

plotid=sort(unique(seedling.ts.adj$slplot))

ex.plotid= sort(unique(subset(seedling.ts.adj,trt=="E")$slplot))
con.plotid= sort(unique(subset(seedling.ts.adj,trt=="C")$slplot))

## Create den

ex.den.vec=rep(0, length(ex.plotid))

con.den.vec=rep(0, length(con.plotid))

n.plot=length (ex.plotid)

area=6*6

census.id=sort(unique(seedling.ts.adj$census))


status=unique(seedling.ts.adj$status)

#[1] "L"  "W"  "M1" "T"  "M2" "D"  NA  

for (i in 1:n.census)
{
  
  temp_census=subset(seedling.ts.adj, census==census.id[i])
  
  temp=subset(temp_census,status=="L"|status=="W"|status=="T" )
  
  
  ## Exclosure
  
  temp.ex=subset(temp, trt=="E")
  
  # ex.freq=as.data.frame(table(temp.ex$plotid))
  
  ex.freq=as.data.frame(table(temp.ex$slplot))
  
  names(ex.freq)=c("plotid", "no") 
  
  yes=which(ex.freq$plotid%in%ex.plotid)
  
  ex.den.vec[yes]=ex.freq$no/area
  
  
  ## Density 
  
  mean.den.census.ex[i]=mean(ex.den.vec)
  sd.den.census.ex[i]=sd(ex.den.vec)
  
  
  ### Control 
  
  temp.con=subset(temp, trt=="C")
  
  #con.freq=as.data.frame(table(temp.con$plotid))
  con.freq=as.data.frame(table(temp.con$slplot))
  
  names(con.freq)=c("plotid", "no") 
  
  yes=which(con.freq$plotid%in%con.plotid)
  
  con.den.vec[yes]=con.freq$no/area
  
  
  ## Density 
  
  mean.den.census.con[i]=mean(con.den.vec)
  sd.den.census.con[i]=sd(con.den.vec)
}

##  Create data.frame for density



den.data=data.frame(census=census.id, den.ex=mean.den.census.ex, sd.ex.den=sd.den.census.ex,
                    den.con=mean.den.census.con, sd.con.den=sd.den.census.con)





#### Making the density graph 

tiff("Density_diversity_2020/plot.density.2020.tiff", res = 600, width = 5, height =4, units = 'in')

{
  
  y.max=max(den.data$den.ex)+2
  
  plot(den.data$census,den.data$den.ex, type="b", col="chartreuse4", ylim=c(0,y.max),
       lwd=3,cex.lab=2,  ylab="", xlab="", axes=F)
  
  lines(den.data$census,den.data$den.con, col="slateblue4",lwd=3,lty=2)
  points(den.data$census,den.data$den.con,  col="slateblue4",pch=19)
  
  ## Adding error bars (Mean + SE)
  
  # Exclosures 
  segments(den.data$census, den.data$den.ex, 
           den.data$census, den.data$den.ex+(den.data$sd.ex.den/sqrt(n.plot)),lwd=3,
           col="chartreuse4")
  
  segments(den.data$census, den.data$den.ex, 
           den.data$census, den.data$den.ex-(den.data$sd.ex.den/sqrt(n.plot)),lwd=3,
           col="chartreuse4")
  
  # Control
  segments(den.data$census, den.data$den.con, 
           den.data$census, den.data$den.con+(den.data$sd.con.den/sqrt(n.plot)),cex=2,lwd=3,
           col="slateblue4")
  
  segments(den.data$census, den.data$den.con, 
           den.data$census, den.data$den.con-(den.data$sd.con.den/sqrt(n.plot)),cex=2,lwd=3,
           col="slateblue4")
  
  
  
  ## Adding labels  
  
  
  axis(1, las=2, cex.axis=0.7, at=seq(1,length(census.time)), labels=census.time, lwd=1.5) 
  
  #mtext(side=1, line =4, cex=1.2, "Time")
  
  axis(2, las=1, cex.axis=1.2, lwd=2)
  mtext(side=2, line =2, cex=1.1, expression("Number of seedlings / m"^{2}) )
  
  
  box(lwd=2)
  
  legend(12, 7.5, lty=1, col="chartreuse4", "Exclosure", lwd=1, bty = "n")
  legend(12, 6.8, lty=2, col="slateblue4",  "Control ", lwd=1, bty = "n")
}

dev.off()


