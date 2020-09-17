drawing_prediction_trajectories=function(country1,country2,country3,country4,country5,country6,country7,country8,country9){
  
  II_plot2=yousee210.130under_reporting[[3]]
  Iso_all_plot2=yousee210.130under_reporting[[13]]
  Rep_all_plot2=yousee210.130under_reporting[[14]]
  
  ttotal=300
  S_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  I_quantile =  array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  II_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  E_quantile =array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_quantile =  array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  #Iso_plot = array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Iso_all_quantile=array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_all_quantile2= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Iso_all_quantile2=array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  II_quantile2 = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  # Calate quantiles
  for(ii in 1:length(citylist)){
    S_quantile[,,ii] <- apply(S_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    II_quantile[,,ii] <- apply(II_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    I_quantile[,,ii] <- apply(I_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})/theta[ii,"population"]
    E_quantile[,,ii] <- apply(E_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})/theta[ii,"population"]
    C_quantile[,,ii] <- apply(C_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Rep_quantile[,,ii] <- apply(Rep_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Rec_quantile[,,ii] <- apply(Rec_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Die_quantile[,,ii] <- apply(Die_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Rep_all_quantile[,,ii] <- apply(Rep_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Rec_all_quantile[,,ii] <- apply(Rec_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Iso_all_quantile[,,ii] <- apply(Iso_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    Die_all_quantile[,,ii] <- apply(Die_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    C_all_quantile[,,ii] <- apply(C_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    Iso_all_quantile2[,,ii] <- apply(Iso_all_plot2[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    II_quantile2[,,ii] <- apply(II_plot2[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    Rep_all_quantile2[,,ii] <- apply(Rep_all_plot2[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
  }
  
  start_date=as.Date("2019-12-10")
  end_date=as.Date("2020-10-04")
  date_rangeB <- seq(start_date,end_date,1)
  start_date2 <- as.Date("2019-12-10") # first case
  end_date2 <- as.Date("2020-05-01")   # period to forecast ahead
  date_rangeC <- seq(start_date2,end_date2,1)
  par(mar=c(0.1,1.5,2,0.3),mgp=c(2,0.55,0)) #mfrow=c(4,2),
  #layout(matrix(c(1,1,1,2,3,4,5), 3, 2, byrow = TRUE),heights=c(1,4.8,4.8,4.8))
  ii=1
  layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10),4, 3, byrow = T),heights=c(1,4.5,4.5,4.5))
 # layout(matrix(c(1,1,1,2,3,4,5,6,7,8,8,8),4, 3, byrow = T),heights=c(1,4.8,4.8,4.8))
 
   plot(date_rangeB,-Rep_all_quantile[1,,ii]-1,pch=19,ylim=c(0,1),xlim=c(as.Date("2019-12-10"),as.Date("2020-10-04")),ylab="",col="white",xaxt="n",yaxt="n",bty="n")
  legend(as.Date("2019-12-10")-10, 5,c("Total confirmed cases reported","New recoveries","Total deaths","New confirmed cases reported","Real total confirmed cases reported","Individuals infected but not hospitalized","Total confirmed cases reported CFU","Individuals infected but not hospitalized CFU"),
         ncol=4,text.font=1,fill=c("blue","green","red",rgb(1,0.6,0.1),"black",rgb(0,1,1,1),rgb(0.65,0.2,0.35),rgb(1,0,0.4,1)),col=c("blue","green","red",rgb(1,0.6,0.1),"black",rgb(0.65,0.2,0.35),rgb(0,1,1,1),rgb(1,0,0.4,1)),border="black",
         cex=0.7,xpd=T,bty="n",text.width = 70)
  
  foreign_country=c("Covid-19 in Wuhan, China","Covid-19 in Hubei Province (excluding Wuhan), China","Covid-19 in Switzerland","Covid-19 in Sweden","Covid-19 in Austria","Covid-19 in France","Covid-19 in The United Kingdom","Covid-19 in Germany","Covid-19 in Spain",
                    "Covid-19 in Italy","Covid-19 in Norway","Covid-19 in The Netherlands","Covid-19 in Belgium","Covid-19 in Denmark","Covid-19 in The United States")
 countrylist=c(country1,country2,country3,country4,country5,country6,country7,country8,country9)
 # countrylist=c(1,2,45,33)
  
  for (jj in 1:9){
    
    ii=countrylist[jj]
    if(ii<=2){
      name=foreign_country[ii]
    }else{
      name=foreign_country[ii-30]
    }
    
    par(mar=c(2,3,1.5,2),mgp=c(2,0.55,0)) #mfrow=c(4,2),
    
    plot(date_rangeB,-Rep_all_quantile[1,,ii]-1,cex=0.5,pch=19,ylim=c(5,pmax(max(Rep_all_quantile[3,,ii]),max(Rep_all_quantile2[3,,ii]),max(II_quantile2[3,,ii]+Iso_all_quantile2[3,,ii]),max(II_quantile[3,,ii]+Iso_all_quantile[3,,ii]))*1.15),xlim=c(as.Date("2019-12-10"),as.Date("2020-10-04")),ylab="Number of people",col="white",xaxt="n")
    c=c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),as.Date("2020-09-01"),as.Date("2020-10-01"))
    axis.Date(1,at=c,format="%Y-%m")
    #axis(2,at=c(3,6,9,12),labels = T)
    #polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+max(Rep_all_quantile[3,,ii]))*1.1),lty=0,col=rgb(1,0.8,0.8))
    lines(c(as.Date("2020-05-01"),as.Date("2020-05-01")),c(-1,1e6),col="blue",lty=3)
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rep_all_quantile[2,,ii],rev(Rep_all_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rep_all_quantile[1,,ii],rev(Rep_all_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.1))
    lines(date_rangeB,Rep_all_quantile[3,,ii],type="l",col=rgb(0,0,1),xaxt="n",yaxt="n",xlab="",ylab="",cex=3)
    
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rep_all_quantile2[2,,ii],rev(Rep_all_quantile2[4,,ii])),lty=0,col=rgb(0.65,0.2,0.35,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rep_all_quantile2[1,,ii],rev(Rep_all_quantile2[5,,ii])),lty=0,col=rgb(0.65,0.2,0.35,0.1))
    lines(date_rangeB,Rep_all_quantile2[3,,ii],type="l",col=rgb(0.65,0.2,0.35,1),xaxt="n",yaxt="n",xlab="",ylab="",cex=3)
    
    polygon(c(date_rangeB,rev(date_rangeB)),c(II_quantile[2,,ii]+Iso_all_quantile[2,,ii],rev(II_quantile[4,,ii]+Iso_all_quantile[4,,ii])),lty=0,col=rgb(0,1,1,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(II_quantile[1,,ii]+Iso_all_quantile[1,,ii],rev(II_quantile[5,,ii]+Iso_all_quantile[5,,ii])),lty=0,col=rgb(0,1,1,0.1))
    lines(date_rangeB,II_quantile[3,,ii]+Iso_all_quantile[3,,ii],type="l",col=rgb(0,1,1,1),xaxt="n",yaxt="n",xlab="",ylab="",cex=3)
    
    
    polygon(c(date_rangeB,rev(date_rangeB)),c(II_quantile2[2,,ii]+Iso_all_quantile2[2,,ii],rev(II_quantile2[4,,ii]+Iso_all_quantile2[4,,ii])),lty=0,col=rgb(1,0,0.4,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(II_quantile2[1,,ii]+Iso_all_quantile2[1,,ii],rev(II_quantile2[5,,ii]+Iso_all_quantile2[5,,ii])),lty=0,col=rgb(1,0,0.4,0.1))
    lines(date_rangeB,II_quantile2[3,,ii]+Iso_all_quantile2[3,,ii],type="l",col=rgb(1,0,0.4,1),xaxt="n",yaxt="n",xlab="",ylab="",cex=3)
    
    
    
    #points(date_rangeA,case_data_wuhan_conf_time)
    # text(labels="model estimate",x=xMin1,y=0.9*1e3,adj=0,col="blue")
    # text(labels="non-fitted data (used for validation)",x=xMin1,y=0.8*1e3,adj=0,col="pink")
    # points(date_rangeB,Rep_quantile[3,,ii],col=rgb(0.5,0.5,1),xaxt="n",yaxt="n",xlab="",ylab="",pch=1,cex=1)
    #  lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(-1,1e6),col="blue",lty=1,cex=2)
    
    #points(date_rangeB,Rec_all_quantile[3,,ii],type="l",col=rgb(0,1,1),xaxt="n",yaxt="n",xlab="",ylab="")
    points(date_rangeC,confirmation[1:144,ii],cex=0.8,pch=1,col="black")
    
    # c("blue","green","red",rgb(1,0.5,0.1),"black")
    par(new=TRUE) 
    
    ym1a <- pmax(max(Rep_quantile[3,,ii])*1.7,max(Die_all_quantile[3,,ii])*1.7)
    #plot(gbs.data$date,gbs.data$GBS,ylim=c(0,20),yaxs="i",lwd=2,type="l",xaxt="n",bty="l",yaxt="n",xlab="",ylab="",col=col.list[[3]])
    plot(date_rangeB,rep(-1,length(Rep_quantile[1,,ii])),lty=0,xaxt="n",col="white",xlim=c(as.Date("2019-12-10"),as.Date("2020-6-24")),bty="l",yaxt="n",xaxt="n",bty="l",yaxt="n",xlab="",ylab="",ylim=c(0,ym1a))
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rep_quantile[2,,ii],rev(Rep_quantile[4,,ii])),lty=0,col=rgb(1,0.6,0.1,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rep_quantile[1,,ii],rev(Rep_quantile[5,,ii])),lty=0,col=rgb(1,0.6,0.1,0.1))
    lines(date_rangeB,Rep_quantile[3,,ii],type="l",col=rgb(1,0.6,0.1),xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)
    axis(4)
    # mtext("the number of new confirmations", side=4, cex=0.7,line=-1)
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rec_quantile[2,,ii],rev(Rec_quantile[4,,ii])),lty=0,col=rgb(0,1,0,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(Rec_quantile[1,,ii],rev(Rec_quantile[5,,ii])),lty=0,col=rgb(0,1,0,0.1))
    lines(date_rangeB,Rec_quantile[3,,ii],type="l",col=rgb(0,1,0),xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)
    
    polygon(c(date_rangeB,rev(date_rangeB)),c(Die_all_quantile[2,,ii],rev(Die_all_quantile[4,,ii])),lty=0,col=rgb(1,0,0,0.2))
    polygon(c(date_rangeB,rev(date_rangeB)),c(Die_all_quantile[1,,ii],rev(Die_all_quantile[5,,ii])),lty=0,col=rgb(1,0,0,0.1))
    lines(date_rangeB,Die_all_quantile[3,,ii],type="l",col=rgb(1,0,0),xaxt="n",yaxt="n",xlab="",ylab="",cex=0.5)
    
   
    title(main=name,adj=0.5,cex.main=0.75,font.main=1,line=0.5); letR = letR + 1 
  }
}
