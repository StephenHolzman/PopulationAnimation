#####Initial Variables#####
#####in the process of automating for other countries in the Human Fertility & Mortality Databases######
#####Excuse the mess#####

country <- "USA"
location <- "~/Documents/PopulationAnimation/"
country_index <- c("Japan","Sweden","USA")
country_abreviation <- c("JPN","SWE","USA")

library(ggplot2)
library(gridExtra)
library(demography)
library(grid)

#####Grab mortality and population data from the Human Mortality Database#####
#####Requires registration for username/password at mortality.org######
#####hmd.mx is a function from the demography package that reads the data into a "demogdata" object#####
USAmort<-hmd.mx("USA","username","password","USA")

#####Read fertility data previously downloaded from the Human Fertility Database#####
#####Data available at humanfertility.org#####
raw_fert <- data.frame(read.table("~/Documents/FertilityAnimation/USA/USAasfrRR.txt",sep="",skip = 3))


#####Shape fertility data and build fertility demogdata object######
reshape_fert <- reshape(raw_fert,timevar = "V1", idvar = "V2", direction = "wide")
USAfert<-NULL
USAfert$type<-"fertility"
USAfert$label<-"USA"
USAfert$year<-c(1933:2010)
USAfert$age<-c(12:55)
USAfert$rate$female<-as.matrix(reshape_fert[,2:79])
USAfert$lambda<-1
USAfert$pop$female<-USAmort$pop$female[13:56,1:78]
class(USAfert)<-"demogdata"

#####Smooth Rates to minimize noise for forecasting#####
#####Full documentation and links to author Hyndman's backing papers available in demography documentation#####

USAmort.sm <- smooth.demogdata(set.upperage(extract.years(USAmort,1933:2010),100))
USAfert.sm <- smooth.demogdata(extract.years(USAfert,1933:2010))
USAmig <- netmigration(set.upperage(USAmort,100),USAfert,mfratio=1.05)

#####Fit Functional Demographic Models#####
#####Usually pretty useful function with grouped ages, happy to use on single-years#####

USAmort.fit <- coherentfdm(USAmort.sm)
USAfert.fit <- fdm(USAfert.sm)
USAmig.fit <- coherentfdm(USAmig)

#####Forecast Rates#####
USAmortf <- forecast(USAmort.fit, h=90)
USAfertf <- forecast(USAfert.fit, h=90)
USAmigf <- forecast(USAmig.fit, h=90, stationary=TRUE)

#####Monte Carlo simulation using the rate forecasts######
USA.sim <- pop.sim(USAmortf, USAfertf, USAmigf, firstyearpop=set.upperage(extract.years(USAmort,1933:2011),100), N=1000)

#####Prepare forecasted population for visualization#####

## medians and intervals
USApopm.median <- apply(USA.sim$male,c(1,2),median)

USApopm.lo <- apply(USA.sim$male,c(1,2),quantile,p=.1,na.rm=TRUE)
USApopm.hi <- apply(USA.sim$male,c(1,2),quantile,p=.9,na.rm=TRUE)

USApopf.median <- apply(USA.sim$female,c(1,2),median)

USApopf.lo <- apply(USA.sim$female,c(1,2),quantile,p=.1,na.rm=TRUE)
USApopf.hi <- apply(USA.sim$female,c(1,2),quantile,p=.9,na.rm=TRUE)

USApopm.lo <-cbind(c(0:100),USApopm.lo)
USApopm.hi <- cbind(c(0:100),USApopm.hi)
USApopf.lo <- cbind(c(0:100),USApopf.lo)
USApopf.hi <- cbind(c(0:100),USApopf.hi)

#####Prepare the base population for graphing######
USAbasepop<-set.upperage(extract.years(USAmort,1933:2010),100)
USAbasepop$rate<-NULL
USAbasepop$type<-"population"


#####Merge base population with forecasted population median#####
#####Data frame everything for ggplot2#####
USApopf<-cbind(c(0:100),USAbasepop$pop$female,USApopf.median[,1:90])
USApopm<-cbind(c(0:100),USAbasepop$pop$male,USApopm.median[,1:90])

colnames(USApopf)<-c("ages",c(1933:2100))
colnames(USApopm)<-c("ages",c(1933:2100))
colnames(USApopf.lo)<-c("ages",c(2011:2100))
colnames(USApopm.lo)<-c("ages",c(2011:2100))
colnames(USApopf.hi)<-c("ages",c(2011:2100))
colnames(USApopm.hi)<-c("ages",c(2011:2100))

USApopf<-data.frame(USApopf)
USApopm<-data.frame(USApopm)
USApopf.lo<-data.frame(USApopf.lo)
USApopm.lo<-data.frame(USApopm.lo)
USApopf.hi<-data.frame(USApopf.hi)
USApopm.hi<-data.frame(USApopm.hi)

#####Annotation functions#######
citesource <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= 1, color= grey(.5),family = "Courier")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}
author <- function(footnoteText=
                            format(Sys.time(), "%d %b %Y"),
                          size= 1.5, color= grey(.5),family = "Courier")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(.01,"npc"),
            y= unit(.01, "npc"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}
clarification <- function(footnoteText=
                     format(Sys.time(), "%d %b %Y"),
                   size= 1, color= grey(.5),family = "Courier")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(.99,"npc"),
            y= unit(.97, "npc"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

#####Graph the base years############
for (year in c(1933:2010)){
  p<- ggplot()+
    ggtitle(bquote(atop(.(paste(country," Population Age Distribution",sep="")), atop(italic(.(paste("Males                   Total Population: ",sprintf("%.0f", signif(sum(USApopf[,year-1931],USApopm[,year-1931])/1000000))," Million                  Females",sep=""))), "")))) +
    theme(plot.title=element_text(family="Courier", face="bold", size=32)) +
    geom_path(data=USApopf,aes_string(x=paste("X",year,sep=""),y="ages"),size=3) +
    geom_path(data=USApopm,aes_string(x=paste("-X",year,sep=""),y="ages"),size=3) +
    geom_vline(xintercept=0,size=3)+
    geom_text(aes(x=-2650000, y=95), label=year,family="Courier",size=50,alpha=.5) +
    #geom_text(aes(x=2650000, y=95),family="Courier",size=50,alpha=.5,label=paste(round(100*(sum(USApopf[1:16,year-1931],USApopf[66:101,year-1931],USApopm[1:16,year-1931],USApopm[66:101,year-1931])/sum(USApopf[17:65,year-1931],USApopm[17:65,year-1931])))))+
    labs(y = "Age",x="Population (Thousands)",family="Courier") +
    theme(axis.text.x = element_text(family="Courier", size =26,color="black", angle = 00, vjust=.25)) +
    theme(axis.text.y = element_text(family="Courier", size =26,color="black", angle = 00, vjust=.25)) +
    theme(axis.title.y = element_text(family="Courier", size=28, angle=90, vjust=0.25)) +
    theme(axis.title.x = element_text(family="Courier", size=28, angle=00, vjust=0.25)) +
    #geom_point(data = USApopf, aes(USApopf$ages,USApopf$ages, shape = NA), colour = "grey50") +
    guides(size=guide_legend("Source", override.aes=list(shape=15, size = 10))) +
    scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=10))+
    scale_x_continuous(limits=c(-3000000,3000000),breaks=seq(-3000000,3000000,by=500000),labels=c(3000,2500,2000,1500,1000,500,0,500,1000,1500,2000,2500,3000))
  
  png(paste(location,country,"/",year,".png",sep=""), width=1920, height=1080)
  print(p + theme(plot.margin = unit(c(1,1,2,1), "lines")))
  citesource("Source: Human Fertility Database, Human Mortality Database")
  author("Creator: Stephen Holzman")
  clarification("*1933 to 2010 is observed population, 2011 to 2100 is median projection and 80% prediction intervals \n created with the demography R package (Hyndman 2015)")
  dev.off()
}
#####Graph the projection years######
#####Still need to work out the proper if statement to get these in one loop####
#####Or maybe convert to function so I can run through apply#####
for (year in c(2011:2100)){
  femalepolygon <- as.data.frame(cbind(USApopf.hi[,year-2009],USApopf.hi$ages,USApopf.lo[,year-2009],USApopf.lo$ages))
  USAintervalf <- rbind(setNames(femalepolygon[,1:2],c('x','y')),
                            setNames(femalepolygon[101:1,3:4],c('x','y')))
  malepolygon <- as.data.frame(cbind(USApopm.hi[,year-2009],USApopf.hi$ages,USApopm.lo[,year-2009],USApopf.lo$ages))
  USAintervalm <- rbind(setNames(malepolygon[,1:2],c('x','y')),
                      setNames(malepolygon[101:1,3:4],c('x','y')))

  p<- ggplot()+
        ggtitle(bquote(atop(.(paste(country," Population Age Distribution",sep="")), atop(italic(.(paste("Males                   Total Population: ",sprintf("%.0f", signif(sum(USApopf[,year-1931],USApopm[,year-1931])/1000000))," Million                  Females",sep=""))), "")))) +
        theme(plot.title=element_text(family="Courier", face="bold", size=32)) +
        geom_path(data=USApopm.lo,aes_string(x=paste("-X",year,sep=""),y="ages"),colour="blue",size=2) +
        geom_path(data=USApopm.hi,aes_string(x=paste("-X",year,sep=""),y="ages"),colour="blue",size=2) +
        geom_path(data=USApopf.lo,aes_string(x=paste("X",year,sep=""),y="ages"),colour="red",size=2) +
        geom_path(data=USApopf.hi,aes_string(x=paste("X",year,sep=""),y="ages"),colour="red",size=2) +
        geom_polygon(data = USAintervalf,aes(x = x,y = y),fill = "red",alpha = 0.3) +
        geom_polygon(data = USAintervalm,aes(x = -x,y = y),fill = "blue",alpha = 0.3) +
        geom_path(data=USApopf,aes_string(x=paste("X",year,sep=""),y="ages"),size=3) +
        geom_path(data=USApopm,aes_string(x=paste("-X",year,sep=""),y="ages"),size=3) +
        geom_vline(xintercept=0,size=3)+
        geom_text(aes(x=-2650000, y=95), label=year,family="Courier",size=50,alpha=.5) +
        #geom_text(aes(x=2650000, y=95),family="Courier",size=50,alpha=.5,label=paste(round(100*(sum(USApopf[1:16,year-1931],USApopf[66:101,year-1931],USApopm[1:16,year-1931],USApopm[66:101,year-1931])/sum(USApopf[17:65,year-1931],USApopm[17:65,year-1931])))))+
        labs(y = "Age",x="Population (Thousands)",family="Courier") +
        theme(axis.text.x = element_text(family="Courier", size =26,color="black", angle = 00, vjust=.25)) +
        theme(axis.text.y = element_text(family="Courier", size =26,color="black", angle = 00, vjust=.25)) +
        theme(axis.title.y = element_text(family="Courier", size=28, angle=90, vjust=0.25)) +
        theme(axis.title.x = element_text(family="Courier", size=28, angle=00, vjust=0.25)) +
        scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=10))+
        scale_x_continuous(limits=c(-3000000,3000000),breaks=seq(-3000000,3000000,by=500000),labels=c(3000,2500,2000,1500,1000,500,0,500,1000,1500,2000,2500,3000))
  
  png(paste(location,country,"/",year,".png",sep=""), width=1920, height=1080)
    print(p + theme(plot.margin = unit(c(1,1,2,1), "lines")))
    citesource("Source: Human Fertility Database, Human Mortality Database")
    author("Creator: Stephen Holzman")
    clarification("*1933 to 2010 is observed population, 2011 to 2100 is median projection and 80% prediction intervals \n created with the demography R package (Hyndman 2015)")
    dev.off()
}



######I merge all the PNG files in Premiere, but there may be a way to do it in R.######
######Please shoot me a message if you jump on one of the many ways to improve this script!#####
