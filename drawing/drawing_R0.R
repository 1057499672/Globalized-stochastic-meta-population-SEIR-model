drawing_R0=function(){
  
  start_date <- as.Date("2019-12-10") # first case
  end_date <- as.Date("2020-06-26") # period to forecast ahead
  #end_date <- as.Date("2020-10-04")
  date_range <- seq(start_date,end_date,1)
  date_rangeF <- date_range[date_range>as.Date("2020-05-01")]
  date_rangeB <- date_range
  ttotal=200
  forecastF <- rep(0,length(date_rangeF))
  
  ttotal=300
  R0_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  
  # Calate quantiles
  for(ii in 1:length(citylist)){
    R0_quantile[,,ii] <- apply(R0_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
  }
  
  # Plot outputs
  a_col <- 0.4 # alpha
  xMin1 <- as.Date("2019-12-10") #min(as.Date("2019-12-01")) 
  xMin <- xMin1         #min(as.Date("2020-01-01"))
  xMax <- end_date #max(date_range)
  
  
  # Plot reproduction number
  xMaxR <- as.Date("2020-06-26")
  #  xMaxR <- as.Date("2020-10-04")
  date_rangeB <- date_range  #[date_rangeA>as.Date("2019-12-15")]
  R0_quantile <- R0_quantile   #[,date_rangeA>as.Date("2019-12-15")]
  wuhan_travel_restrictions=as.Date("2020-01-23")
  #date_range=seq.Date(as.Date("2019-12-10"),by="day",length.out=200)
  #date_rangeF <- date_range[date_range>as.Date("2020-4-16")]
  #date_rangeB <- date_range
  
  #####111111111
  foreign_country=c("Wuhan, China","Hubei Province (excluding Wuhan), China","Switzerland","Sweden","Austria","France","The United Kingdom","Germany","Spain",
                    "Italy","Norway","The Netherlands","Belgium","Denmark","The United States")
  city=c(1,2,33,34,35,36,37,38,39,40,41,42,43,44,45)
  par(mar=c(1.5,2.7,1.3,0.5),mgp=c(1.5,0.55,0)) #mfrow=c(4,2),
  #  layout(matrix(c(1,2,3,4,5,6,7,8,9,10), 5, 2, byrow = TRUE))
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 5, 3, byrow = TRUE))
  
  for (jj in 1:15){
    name=foreign_country[jj]
    ii=city[jj]
    plot(date_rangeB,R0_quantile[1,1:200,ii],col="white",ylim=c(0,max(R0_quantile[3,,ii])+1),xlim=c(xMin1+10,xMaxR-10),xlab="",ylab=expression(paste(R[t])),xaxt="n")
    c=c(as.Date("2020-01-01"),as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
    axis.Date(1,at=c,format="%Y-%m")
    polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+max(R0_quantile[3,1:200,ii])+1)),lty=0,col=rgb(0.1,0.5,1,0.35))
    polygon(c(date_rangeB,rev(date_rangeB)),c(R0_quantile[2,1:200,ii],rev(R0_quantile[4,1:200,ii])),lty=0,col=rgb(1,0.1,0,0.35))
    polygon(c(date_rangeB,rev(date_rangeB)),c(R0_quantile[1,1:200,ii],rev(R0_quantile[5,1:200,ii])),lty=0,col=rgb(1,0.2,0,0.2))
    lines(date_rangeB,R0_quantile[3,1:200,ii],type="l",col="red",xaxt="s" ,yaxt="s",xlab="s",ylab="")
    lines(date_rangeB,1+0*R0_quantile[3,1:200,ii],lty=2,col="black")
    lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(-1,max(R0_quantile[5,,ii])+1),col="black")
    # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
    # title(LETTERS[1],adj=0,"Wuhan",adj=0,"Wuhan",adj=0,NULL); 
    letR = 3 
    # title("Regions of Hubei outside Wuhan",adj=0)
    # title(main="The United States",adj=0,cex.main=0.85,font.main=2)
    title(main=name,adj=0,cex.main=0.85,font.main=2)
  }   
}
