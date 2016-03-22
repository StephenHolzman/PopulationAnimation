library(ggplot2)
library(grid)
library(gridExtra)

makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= 2, color= grey(.5),family = "Courier")
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

makeFootnote2 <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= 2, color= grey(.5),family = "Courier")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(.15,"npc"),
            y= unit(.01, "npc"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

animate.fertility <- function(country, country2, framesperyear, location){
  
  country_index <- c("Japan","Sweden","USA")
  country_abreviation <- c("JPN","SWE","USA")
  
  raw_table <- data.frame(read.table(paste(location,country,"/",country_abreviation[match(country,country_index)],"asfrRR.txt",sep=""),skip = 3,colClasses = c("numeric","character","numeric")))
  raw_table2 <- data.frame(read.table(paste(location,country2,"/",country_abreviation[match(country2,country_index)],"asfrRR.txt",sep=""),skip = 3, colClasses = c("numeric","character","numeric")))
  ages <- c(12:55)
  years <- min(raw_table$V1):max(raw_table$V1)
  years2 <- min(raw_table2$V1):max(raw_table$V1)
  
  
  reshape_table <- reshape(raw_table,timevar = "V1", idvar = "V2", direction = "wide")
  reshape_table2 <- reshape(raw_table2, timevar = "V1", idvar = "V2", direction = "wide")
  
  minyear1 <- as.numeric(min(raw_table$V1))
  minyear2 <- min(raw_table2$V1)
  maxyear1 <- max(raw_table$V1)
  maxyear2 <- max(raw_table2$V1)
  bottomyear <- max(c(minyear1,minyear2))
  topyear <- min(c(maxyear1,maxyear2))
  
  
  
  plot_table <-  matrix(NA, nrow= length(ages), ncol = framesperyear*length(bottomyear:topyear))
  plot_table2 <- matrix(NA, nrow= length(ages), ncol = framesperyear*length(bottomyear:topyear))
  
  
  
  for (age in 12:55){
    temp <- NULL
    temp$rates <- as.numeric(as.vector(reshape_table[age-11,((match(bottomyear,minyear1:maxyear1))+1):(match(topyear,minyear1:maxyear1)+1)]))
    temp$years <- bottomyear:topyear
    temp2 <- spline(temp$years,temp$rates, n = framesperyear*length(temp$years))
    plot_table[age-11,] <- as.vector(temp2$y)
  }
  temp<-NULL
  for (age in 12:55){
    temp <- NULL
    temp$rates <- as.numeric(as.vector(reshape_table2[age-11,((match(bottomyear,minyear2:maxyear2))+1):(match(topyear,minyear2:maxyear2)+1)]))
    temp$years <- bottomyear:topyear
    temp2 <- spline(temp$years,temp$rates, n = framesperyear*length(temp$years))
    plot_table2[age-11,] <- as.vector(temp2$y)
  }
  
  date<-NULL
  for (year in bottomyear:topyear){
    for (x in 1:framesperyear){
      date <- c(date,year)
    }
  }
  
  date2<-NULL
  for (year in years2){
    for (x in 1:framesperyear){
      date2 <- c(date2,year)
    }
  }
  frame<-NULL
  frame$x <- c(40,40,50,50)
  frame$y <- c(.17,.3,.3,.17)
  frame<-data.frame(frame)
  for (i in 1:(framesperyear*length(temp$years))){
    plot_table3 <- data.frame(ages = c(12:55), rates = c(plot_table[,i]))
    plot_table4 <- data.frame(ages= c(12:55), rates = c(plot_table2[,i]))
    p <- ggplot(data=plot_table3, aes(x=ages, y=rates)) +
      geom_polygon(colour="black",fill="blue",alpha=.5, size= .02) +
      geom_polygon(data=plot_table4,colour="black",fill="red",alpha=.5, size= .02) +
      geom_polygon(data=frame,aes(x=x,y=y),colour="black",fill="white", size= 2)+
      #guides() +
      ylim(0,.3) +
      ggtitle(bquote(atop(.(paste(country," and ",country2," Fertility Over Time",sep=""))))) +
      theme(plot.title=element_text(family="Courier", face="bold", size=40)) +
      labs(x = "Age",y="Births per Woman",family="Courier") +
      theme(axis.title.y = element_text(family="Courier", size=26,color="black", angle=90, vjust=0.25)) +
      theme(axis.title.x = element_text(family="Courier", size=26,color="black", angle=00, vjust=0.25)) +
      theme(axis.text.x = element_text(family="Courier", size =28,color="black", angle = 00, vjust=.25)) +
      theme(axis.text.y = element_text(family="Courier", size =28,color="black", angle = 00, vjust=.25)) +
      geom_text(aes(x= 45, y=.27), label=date[i],family="Courier",size=50,alpha=.5) +
      geom_text(aes(x=45, y=.22), label=paste(country,": ",sprintf("%.2f", signif(sum(plot_table[1:44,i]),digits=3)),sep=""),family="Courier",color="blue",size=20,alpha=.2) +
      geom_text(aes(x=45, y=.19), label=paste(country2,": ",sprintf("%.2f", signif(sum(plot_table2[1:44,i]),digits=3)),sep=""),family="Courier",color="red",size=20,alpha=.2) +
      geom_text(aes(x=45, y=.24), label="Total Fertility Rates",family="Courier",color="black",size=10,alpha=.5) +
      
      
      scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1'))
    png(paste(location,country,"/",i,".png",sep=""), width=1920, height=1080) # open an appropriate graphics device
    print(p + theme(plot.margin = unit(c(1,1,2,1), "lines")))
    makeFootnote("Source: Human Fertility Database") # from webpage above (uses grid.text; ggplot2 is based on grid)
    makeFootnote2("Creator: Stephen Holzman")
    dev.off()
  }
}
animate.fertility("USA","Japan",2,"/Volumes/Storage/FertilityAnimation/")




#The following is all sketches for the function if I remember correctly.
#Uploading this after months away from it for posterity, so keeping it here just in case.

country<-"USA"
country2<-"Japan"
framesperyear<-7
location<-"~/Documents/FertilityAnimation/"



country_index <- c("Japan","Sweden","USA")
country_abreviation <- c("JPN","SWE","USA")

raw_table <- data.frame(read.table(paste(location,country,"/",country_abreviation[match(country,country_index)],"asfrRR.txt",sep=""),skip = 3,colClasses = c("numeric","character","numeric")))
raw_table2 <- data.frame(read.table(paste(location,country2,"/",country_abreviation[match(country2,country_index)],"asfrRR.txt",sep=""),skip = 3, colClasses = c("numeric","character","numeric")))
ages <- c(12:55)
years <- min(raw_table$V1):max(raw_table$V1)
years2 <- min(raw_table2$V1):max(raw_table$V1)


reshape_table <- reshape(raw_table,timevar = "V1", idvar = "V2", direction = "wide")
reshape_table2 <- reshape(raw_table2, timevar = "V1", idvar = "V2", direction = "wide")

minyear1 <- as.numeric(min(raw_table$V1))
minyear2 <- min(raw_table2$V1)
maxyear1 <- max(raw_table$V1)
maxyear2 <- max(raw_table2$V1)
bottomyear <- max(c(minyear1,minyear2))
topyear <- min(c(maxyear1,maxyear2))


  
plot_table <-  matrix(NA, nrow= length(ages), ncol=framesperyear*length(bottomyear:topyear))
plot_table2 <- matrix(NA, nrow= length(ages), ncol = framesperyear*length(bottomyear:topyear))



for (age in 12:55){
  temp <- NULL
  temp$rates <- as.numeric(as.vector(reshape_table[age-11,((match(bottomyear,minyear1:maxyear1))+1):(match(topyear,minyear1:maxyear1)+1)]))
  temp$years <- bottomyear:topyear
  temp2 <- spline(temp$years,temp$rates, n = framesperyear*length(temp$years))
  plot_table[age-11,] <- as.vector(temp2$y)
}
temp<-NULL
for (age in 12:55){
  temp <- NULL
  temp$rates <- as.numeric(as.vector(reshape_table2[age-11,((match(bottomyear,minyear2:maxyear2))+1):(match(topyear,minyear2:maxyear2)+1)]))
  temp$years <- bottomyear:topyear
  temp2 <- spline(temp$years,temp$rates, n = framesperyear*length(temp$years))
  plot_table2[age-11,] <- as.vector(temp2$y)
}

date<-NULL
for (year in bottomyear:topyear){
  for (x in 1:framesperyear){
    date <- c(date,year)
  }
}

date2<-NULL
for (year in years2){
  for (x in 1:framesperyear){
    date2 <- c(date2,year)
  }
}
frame<-NULL
frame$x <- c(40,40,50,50)
frame$y <- c(.17,.3,.3,.17)
frame<-data.frame(frame)
for (i in 1:(framesperyear*length(temp$years))){
  plot_table3 <- data.frame(ages = c(12:55), rates = c(plot_table[,i]))
  plot_table4 <- data.frame(ages= c(12:55), rates = c(plot_table2[,i]))
  p <- ggplot(data=plot_table3, aes(x=ages, y=rates)) +
    geom_polygon(colour="black",fill="blue",alpha=.5, size= .02) +
    geom_polygon(data=plot_table4,colour="black",fill="red",alpha=.5, size= .02) +
    geom_polygon(data=frame,aes(x=x,y=y),colour="black",fill="white", size= 2)+
    #guides() +
    ylim(0,.3) +
    ggtitle(bquote(atop(.(paste(country," and ",country2," Fertility Over Time",sep=""))))) +
    theme(plot.title=element_text(family="Courier", face="bold", size=40)) +
    labs(x = "Age",y="Births per Woman",family="Courier") +
    theme(axis.title.y = element_text(family="Courier", size=26,color="black", angle=90, vjust=0.25)) +
    theme(axis.title.x = element_text(family="Courier", size=26,color="black", angle=00, vjust=0.25)) +
    theme(axis.text.x = element_text(family="Courier", size =28,color="black", angle = 00, vjust=.25)) +
    theme(axis.text.y = element_text(family="Courier", size =28,color="black", angle = 00, vjust=.25)) +
    geom_text(aes(x= 45, y=.27), label=date[i],family="Courier",size=50,alpha=.5) +
    geom_text(aes(x=45, y=.22), label=paste(country,": ",sprintf("%.2f", signif(sum(plot_table[1:44,i]),digits=3)),sep=""),family="Courier",color="blue",size=20,alpha=.2) +
    geom_text(aes(x=45, y=.19), label=paste(country2,": ",sprintf("%.2f", signif(sum(plot_table2[1:44,i]),digits=3)),sep=""),family="Courier",color="red",size=20,alpha=.2) +
    geom_text(aes(x=45, y=.24), label="Total Fertility Rates",family="Courier",color="black",size=10,alpha=.5) +
    
    
    scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1'))
  png(paste(location,country,"/",i,".png",sep=""), width=1920, height=1080) # open an appropriate graphics device
  print(p + theme(plot.margin = unit(c(1,1,2,1), "lines")))
  makeFootnote("Source: Human Fertility Database") # from webpage above (uses grid.text; ggplot2 is based on grid)
  makeFootnote2("Creator: Stephen Holzman")
  dev.off()
}
