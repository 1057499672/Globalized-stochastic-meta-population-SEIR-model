drawing_intervention_comparison=function(){
  ttotal=300
  
  start_date=as.Date("2020-1-23")
  end_date=as.Date("2020-6-26")
  date_rangeB <- seq(start_date,end_date,1)
  
   S_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  I_quantile =  array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  II_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  E_quantile =array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_quantile =  array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  #Iso_plot = array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  R0_quantile = array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Iso_all_quantile=array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_all_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  For_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  sus_rate_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  track_rate_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  iso_rate_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  home_rate_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  inhome_quantile= array(0,dim=c(5,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
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
    Iso_all_quantile[,,ii] <- apply(Iso_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})/theta[ii,"population"]
    Die_all_quantile[,,ii] <- apply(Die_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    C_all_quantile[,,ii] <- apply(C_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)}) 
    R0_quantile[,,ii] <- apply(R0_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    For_quantile[,,ii] <- apply(For_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    sus_rate_quantile[,,ii] <- apply(sus_rate_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    track_rate_quantile[,,ii] <- apply(track_rate_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    iso_rate_quantile[,,ii] <- apply(iso_rate_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    home_rate_quantile[,,ii] <- apply(home_rate_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    inhome_quantile[,,ii] <- apply(inhome_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    Iso_all_quantile2[,,ii] <- apply(Iso_all_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
    II_quantile2[,,ii] <- apply(II_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})
  }
  for(ii in 1:length(citylist)){
    inhome_quantile[,,ii] <- apply(inhome_plot[,,ii],1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975),na.rm=T)})/(S_quantile[3,,ii]+inhome_quantile[3,,ii])
  }
  for(ii in 1:length(citylist)){
    Iso_all_quantile2[3,,ii] <- Iso_all_quantile2[3,,ii]/(II_quantile2[3,,ii]+Iso_all_quantile2[3,,ii])
  }
  
  
  Chinese_average1=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal){
    Chinese_average1[3,ii-44]=sum(home_rate_quantile[3,ii,1:32])/32
    Chinese_average1[1,ii-44]=sum(home_rate_quantile[1,ii,1:32])/32
    Chinese_average1[2,ii-44]=sum(home_rate_quantile[2,ii,1:32])/32
    Chinese_average1[4,ii-44]=sum(home_rate_quantile[4,ii,1:32])/32
    Chinese_average1[5,ii-44]=sum(home_rate_quantile[5,ii,1:32])/32
  }
  west_average1=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal)
  {west_average1[3,ii-44]=sum(home_rate_quantile[3,ii,33:45])/13
  west_average1[1,ii-44]=sum(home_rate_quantile[1,ii,33:45])/13
  west_average1[2,ii-44]=sum(home_rate_quantile[2,ii,33:45])/13
  west_average1[4,ii-44]=sum(home_rate_quantile[4,ii,33:45])/13
  west_average1[5,ii-44]=sum(home_rate_quantile[5,ii,33:45])/13}
  
  Chinese_average2=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal){
    Chinese_average2[3,ii-44]=sum(sus_rate_quantile[3,ii,1:32])/32
    Chinese_average2[1,ii-44]=sum(sus_rate_quantile[1,ii,1:32])/32
    Chinese_average2[2,ii-44]=sum(sus_rate_quantile[2,ii,1:32])/32
    Chinese_average2[4,ii-44]=sum(sus_rate_quantile[4,ii,1:32])/32
    Chinese_average2[5,ii-44]=sum(sus_rate_quantile[5,ii,1:32])/32
  }
  west_average2=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal)
  {west_average2[3,ii-44]=sum(sus_rate_quantile[3,ii,33:45])/13
  west_average2[1,ii-44]=sum(sus_rate_quantile[1,ii,33:45])/13
  west_average2[2,ii-44]=sum(sus_rate_quantile[2,ii,33:45])/13
  west_average2[4,ii-44]=sum(sus_rate_quantile[4,ii,33:45])/13
  west_average2[5,ii-44]=sum(sus_rate_quantile[5,ii,33:45])/13}
  
  Chinese_average3=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal){
    Chinese_average3[3,ii-44]=sum(inhome_quantile[3,ii,1:32])/32
    Chinese_average3[1,ii-44]=sum(inhome_quantile[1,ii,1:32])/32
    Chinese_average3[2,ii-44]=sum(inhome_quantile[2,ii,1:32])/32
    Chinese_average3[4,ii-44]=sum(inhome_quantile[4,ii,1:32])/32
    Chinese_average3[5,ii-44]=sum(inhome_quantile[5,ii,1:32])/32
  }
  west_average3=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal)
  {west_average3[3,ii-44]=sum(inhome_quantile[3,ii,33:45])/13
  west_average3[1,ii-44]=sum(inhome_quantile[1,ii,33:45])/13
  west_average3[2,ii-44]=sum(inhome_quantile[2,ii,33:45])/13
  west_average3[4,ii-44]=sum(inhome_quantile[4,ii,33:45])/13
  west_average3[5,ii-44]=sum(inhome_quantile[5,ii,33:45])/13}
  
  Chinese_average4=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal){
    Chinese_average4[3,ii-44]=sum(iso_rate_quantile[3,ii,1:32])/32
    Chinese_average4[1,ii-44]=sum(iso_rate_quantile[1,ii,1:32])/32
    Chinese_average4[2,ii-44]=sum(iso_rate_quantile[2,ii,1:32])/32
    Chinese_average4[4,ii-44]=sum(iso_rate_quantile[4,ii,1:32])/32
    Chinese_average4[5,ii-44]=sum(iso_rate_quantile[5,ii,1:32])/32
  }
  west_average4=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal)
  {west_average4[3,ii-44]=sum(iso_rate_quantile[3,ii,33:45])/13
  west_average4[1,ii-44]=sum(iso_rate_quantile[1,ii,33:45])/13
  west_average4[2,ii-44]=sum(iso_rate_quantile[2,ii,33:45])/13
  west_average4[4,ii-44]=sum(iso_rate_quantile[4,ii,33:45])/13
  west_average4[5,ii-44]=sum(iso_rate_quantile[5,ii,33:45])/13}
  
  Chinese_average5=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal){
    Chinese_average5[3,ii-44]=sum(Iso_all_quantile[3,ii,1:32])/32
    Chinese_average5[1,ii-44]=sum(Iso_all_quantile[1,ii,1:32])/32
    Chinese_average5[2,ii-44]=sum(Iso_all_quantile[2,ii,1:32])/32
    Chinese_average5[4,ii-44]=sum(Iso_all_quantile[4,ii,1:32])/32
    Chinese_average5[5,ii-44]=sum(Iso_all_quantile[5,ii,1:32])/32
  }
  west_average5=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal)
  {west_average5[3,ii-44]=sum(Iso_all_quantile[3,ii,33:45])/13
  west_average5[1,ii-44]=sum(Iso_all_quantile[1,ii,33:45])/13
  west_average5[2,ii-44]=sum(Iso_all_quantile[2,ii,33:45])/13
  west_average5[4,ii-44]=sum(Iso_all_quantile[4,ii,33:45])/13
  west_average5[5,ii-44]=sum(Iso_all_quantile[5,ii,33:45])/13}
  
  Chinese_average6=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal){
    Chinese_average6[3,ii-44]=sum(Iso_all_quantile2[3,ii,1:32])/32
    Chinese_average6[1,ii-44]=sum(Iso_all_quantile2[1,ii,1:32])/32
    Chinese_average6[2,ii-44]=sum(Iso_all_quantile2[2,ii,1:32])/32
    Chinese_average6[4,ii-44]=sum(Iso_all_quantile2[4,ii,1:32])/32
    Chinese_average6[5,ii-44]=sum(Iso_all_quantile2[5,ii,1:32])/32
  }
  west_average6=array(0,dim=c(5,ttotal-44),dimnames=list(NULL,NULL))
  for(ii in 45:ttotal)
  {west_average6[3,ii-44]=sum(Iso_all_quantile2[3,ii,33:45])/13
  west_average6[1,ii-44]=sum(Iso_all_quantile2[1,ii,33:45])/13
  west_average6[2,ii-44]=sum(Iso_all_quantile2[2,ii,33:45])/13
  west_average6[4,ii-44]=sum(Iso_all_quantile2[4,ii,33:45])/13
  west_average6[5,ii-44]=sum(Iso_all_quantile2[5,ii,33:45])/13}
  
  
  par(mar=c(0.3,0.3,2,0.3),mgp=c(2,0.55,0)) #mfrow=c(4,2),
  ii=1
  # layout(matrix(c(1,1,2,3,4,5),3, 2, byrow = T),heights=c(1,6,6))
  layout(matrix(c(1,1,1,2,3,4,5,6,7,8,8,8),4, 3, byrow = T),heights=c(1,4.6,4.6,4.6))
  plot(date_rangeB,home_rate_quantile[1,45:200,ii],pch=19,ylim=c(0,1),xlim=c(as.Date("2019-12-10")+5,as.Date("2020-10-04")-5),ylab="",col="white",xaxt="n",yaxt="n",bty="n")
  legend(as.Date("2019-12-10")+5,6,c("Wuhan level","Chinese level","Western level","The United States level"),
         fill=c(rgb(1,0,0),rgb(0.1,0.7,0.2),rgb(0,0,1),"purple"),ncol=4,text.font=1,cex=0.7,bty="n",xpd=T,x.intersp = 0.5,text.width=40)
  
  
  ii=1
  par(mar=c(2,3,2,1),mgp=c(2,0.55,0))
  plot(date_rangeB,home_rate_quantile[1,45:200,ii],col="white",ylim=c(0,14),xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="Days",yaxt="n")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  axis(2,at=c(3,6,9,12),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+14)),lty=0,col=rgb(0.1,0.5,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,home_rate_quantile[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  # lines(date_rangeB,home_rate_quantile[3,45:200,40],type="l",col="orangered",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,home_rate_quantile[3,45:200,45],type="l",col="purple",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,west_average1[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,Chinese_average1[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,45],type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  #   lines(date_rangeB,sus_rate_quantile[3,45:180,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  #  lines(date_rangeB,sus_rate_quantile[3,45:180,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  #   lines(date_rangeB,sus_rate_quantile[3,45:180,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,40],type="l",col="hotpink",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,39],type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,38],type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,37],type="l",col="lightgreen",xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,36],type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,35],type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,34],type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,33],type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  # legend(as.Date("2020-01-25"),14,c("Wuhan level","Western average level","Chinese average level"),text.col=
  #                     c("red","blue","green"),
  #                   cex=0.5,ncol=1)
  #    legend(as.Date("2020-01-25"),14,c("Wuhan","UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                      ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #             c("red","slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #               "royalblue","violet","yellow","gray40"),
  #           cex=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),ncol=8)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
  lines(date_rangeB,1+0*home_rate_quantile[3,45:200,1],lty=2)
  #lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,14),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  title(main="A",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  title(main="Days for an individual to socailly-distance himself",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
  
  
  
  
  plot(date_rangeB,sus_rate_quantile[1,45:200,ii],col="white",ylim=c(0,14),xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="Days",yaxt="n")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  axis(2,at=c(3,6,9,12),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+14)),lty=0,col=rgb(0.1,0.5,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,sus_rate_quantile[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,sus_rate_quantile[3,45:200,45],type="l",col="purple",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,west_average2[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,Chinese_average2[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,45],type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  #   lines(date_rangeB,sus_rate_quantile[3,45:180,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  #  lines(date_rangeB,sus_rate_quantile[3,45:180,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  #   lines(date_rangeB,sus_rate_quantile[3,45:180,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,40],type="l",col="hotpink",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,39],type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,38],type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,37],type="l",col="lightgreen",xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,36],type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,35],type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,34],type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  #    lines(date_rangeB,sus_rate_quantile[3,45:180,33],type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  # legend(as.Date("2020-01-25"),14,c("Wuhan level","Western average level","Chinese average level"),text.col=
  #                     c("red","blue","green"),
  #                   cex=0.5,ncol=1)
  #    legend(as.Date("2020-01-25"),14,c("Wuhan","UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                      ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #             c("red","slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #               "royalblue","violet","yellow","gray40"),
  #           cex=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),ncol=8)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
  lines(date_rangeB,1+0*home_rate_quantile[3,45:200,1],lty=2)
  #lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,14),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  title(main="B",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  title(main="Days for an individual to resume work",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
  
  
  plot(date_rangeB,inhome_quantile[1,45:200,ii],col="white",ylim=c(0,1),xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="",yaxt="n")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  axis(2,at=c(0.3,0.6,0.9),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+1)),lty=0,col=rgb(0.1,0.5,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,inhome_quantile[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,inhome_quantile[3,45:200,45],type="l",col="purple",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,west_average3[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,Chinese_average3[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  #    lines(date_rangeB,inhome_quantile[3,45:180,45],type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,40],type="l",col="hotpink",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,39],type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,38],type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,37],type="l",col="lightgreen",xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,36],type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,35],type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,34],type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,33],type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  #   legend(as.Date("2020-01-25"),1,c("Wuhan level","Western average level","Chinese average level"),text.col=
  #               c("red","blue","green"),
  #            cex=0.65,ncol=1)
  #    legend(as.Date("2020-01-25"),14,c("Wuhan","UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                      ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #             c("red","slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #               "royalblue","violet","yellow","gray40"),
  #           cex=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),ncol=8)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
  lines(date_rangeB,0.3+0*inhome_quantile[3,45:200,1],lty=2)
  #lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,1),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  
  title(main="C",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  title(main="Proportion of population getting socially-distanced",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
  
  plot(date_rangeB,iso_rate_quantile[1,45:200,ii],col="white",ylim=c(2.5,8),xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="Days",yaxt="n")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  axis(2,at=c(3,6,9,12),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+8)),lty=0,col=rgb(0.1,0.5,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,iso_rate_quantile[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  #  lines(date_rangeB,iso_rate_quantile[3,45:200,45],type="l",col="purple",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,west_average4[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,Chinese_average4[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,45],type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,40],type="l",col="hotpink",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,39],type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,38],type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,37],type="l",col="lightgreen",xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,36],type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,35],type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  #    lines(date_rangeB,iso_rate_quantile[3,45:180,34],type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  #     lines(date_rangeB,iso_rate_quantile[3,45:180,33],type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  #   legend(as.Date("2020-01-25"),14,c("Wuhan level","Western average level","Chinese average level"),text.col=
  #             c("red","blue","green"),
  #           cex=0.65,ncol=1)
  #     legend(as.Date("2020-01-25"),14,c("Wuhan","UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                       ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #              c("red","slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #               "royalblue","violet","yellow","gray40"),
  #            cex=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),ncol=8)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
  lines(date_rangeB,1+0*home_rate_quantile[3,45:200,1],lty=2)
 # lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,14),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  
  title(main="D",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  title(main="Days for a suspected case to be isolated",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
  
  
  plot(date_rangeB,inhome_quantile[1,45:200,ii],col="white",
       ylim=c(0,max(Iso_all_quantile[3,45:200,1])*1),
       xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  #axis(2,at=c(0.3,0.6,0.9),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+1)),lty=0,col=rgb(0.1,0.5,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,Iso_all_quantile[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,Iso_all_quantile[3,45:200,45],type="l",col="purple",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,west_average5[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,Chinese_average5[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  #    lines(date_rangeB,inhome_quantile[3,45:180,45],type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,40],type="l",col="hotpink",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,39],type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,38],type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,37],type="l",col="lightgreen",xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,36],type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,35],type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,34],type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,33],type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  #   legend(as.Date("2020-01-25"),1,c("Wuhan level","Western average level","Chinese average level"),text.col=
  #               c("red","blue","green"),
  #            cex=0.65,ncol=1)
  #    legend(as.Date("2020-01-25"),14,c("Wuhan","UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                      ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #             c("red","slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #               "royalblue","violet","yellow","gray40"),
  #           cex=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),ncol=8)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
  lines(date_rangeB,0.3+0*inhome_quantile[3,45:200,1],lty=2)
 # lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,1),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  
  title(main="E",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  title(main="Proportion of population getting isolated",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
  
  
  
  
  
  plot(date_rangeB,inhome_quantile[1,45:200,ii],col="white",ylim=c(0,1),xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="",yaxt="n")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  axis(2,at=c(0.3,0.6,0.9),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+1)),lty=0,col=rgb(0.1,0.5,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,Iso_all_quantile2[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,Iso_all_quantile2[3,45:200,45],type="l",col="purple",xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  lines(date_rangeB,west_average6[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,Chinese_average6[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  #    lines(date_rangeB,inhome_quantile[3,45:180,45],type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,40],type="l",col="hotpink",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,39],type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,38],type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,37],type="l",col="lightgreen",xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,36],type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,35],type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  #    lines(date_rangeB,inhome_quantile[3,45:180,34],type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  #   lines(date_rangeB,inhome_quantile[3,45:180,33],type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  #   legend(as.Date("2020-01-25"),1,c("Wuhan level","Western average level","Chinese average level"),text.col=
  #               c("red","blue","green"),
  #            cex=0.65,ncol=1)
  #    legend(as.Date("2020-01-25"),14,c("Wuhan","UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                      ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #             c("red","slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #               "royalblue","violet","yellow","gray40"),
  #           cex=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),ncol=8)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
  lines(date_rangeB,0.3+0*inhome_quantile[3,45:200,1],lty=2)
 # lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,1),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  
  title(main="F",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  title(main="Proportion of suspected cases getting isolated",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
  
  
  plot(date_rangeB,home_rate_quantile[1,45:200,ii],col="white",ylim=c(0,max(cumsum(For_quantile[3,45:200,37]))),xlim=c(xMin1+5,xMaxR-5),xlab="",xaxt="n",
       ylab="Numbers")
  c=c(as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))
  axis.Date(1,at=c,format="%Y-%m")
  # axis(2,at=c(3,6,9,12),labels = T)
  polygon(c(date_rangeF,rev(date_rangeF)),c(forecastF,rev(forecastF+max(cumsum(For_quantile[3,45:200,37])))),lty=0,col=rgb(1,0.5,0,0.15))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[2,,ii],rev(sus_rate_quantile[4,,ii])),lty=0,col=rgb(0,0.3,1,0.35))
  # polygon(c(date_rangeB,rev(date_rangeB)),c(sus_rate_quantile[1,,ii],rev(sus_rate_quantile[5,,ii])),lty=0,col=rgb(0,0.3,1,0.2))
  #lines(date_rangeB,For_quantile[3,45:200,1],type="l",col=rgb(1,0,0),xaxt="n",xlab="n",ylab="Wuhan",pch=1,lty=1,cex=0.5)
  # lines(date_rangeB,west_average[3,1:156],type="l",col=rgb(0,0,1),xaxt="n",xlab="n",ylab="West_average",pch=2,lty=1,cex=1)
  #lines(date_rangeB,Chinese_average[3,1:156],type="l",col=rgb(0.1,0.7,0.2),xaxt="n",xlab="n",ylab="Chinese_average",pch=2,lty=1,cex=1)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,45]),type="l",col="slateblue1",xaxt="n",yaxt="n",xlab="",ylab="UN",pch=2,lty=1,cex=0.5)
  # lines(date_rangeB,For_quantile[3,45:200,44],type="l",col="thistle3",xaxt="n",yaxt="n",xlab="",ylab="Denmark",pch=3,lty=1,cex=0.5)
  # lines(date_rangeB,For_quantile[3,45:200,43],type="l",col="coral4",xaxt="n",yaxt="n",xlab="",ylab="Belgium",pch=4,lty=1,cex=0.5)
  # lines(date_rangeB,For_quantile[3,45:200,42],type="l",col="green",xaxt="n",yaxt="n",xlab="",ylab="Netherlands",pch=5,lty=1,cex=0.5)
  # lines(date_rangeB,For_quantile[3,45:200,41],type="l",col="burlywood1",xaxt="n",yaxt="n",xlab="",ylab="Norway",pch=6,lty=1,cex=0.5)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,40]),type="l",col="Red",xaxt="n",yaxt="n",xlab="",ylab="Italy",pch=7,lty=1,cex=2)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,39]),type="l",col="magenta",xaxt="n",yaxt="n",xlab="",ylab="Spain",pch=8,lty=1,cex=0.5)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,38]),type="l",col="purple",xaxt="n",yaxt="n",xlab="",ylab="Germany",pch=9,lty=1,cex=2)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,37]),type="l",col=rgb(0.1,0.7,0.2),xaxt="n",yaxt="n",xlab="",ylab="UK",pch=10,lty=1,cex=0.5)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,36]),type="l",col="royalblue",xaxt="n",yaxt="n",xlab="",ylab="France",pch=11,lty=1,cex=0.5)
  # lines(date_rangeB,cumsum(For_quantile[3,45:200,35]),type="l",col="violet",xaxt="n",yaxt="n",xlab="",ylab="Austria",pch=12,lty=1,cex=0.5)
  lines(date_rangeB,cumsum(For_quantile[3,45:200,34]),type="l",col="yellow",xaxt="n",yaxt="n",xlab="",ylab="Sweden",pch=13,lty=1,cex=0.5)
  # lines(date_rangeB,cumsum(For_quantile[3,45:200,33]),type="l",col="gray40",xaxt="n",yaxt="n",xlab="",ylab="Switzerland",pch=14,lty=1,cex=0.5)
  # legend(as.Date("2020-01-25"),22,c("Wuhan level","Western average level","Chinese average level"),fill= c("red","blue",rgb(0.1,0.7,0.2)),text.font=1,border="black",
  #        cex=0.8 ,ncol=2,xpd=T,bty="n")
  
  # legend(as.Date("2020-01-25"),cumsum(For_quantile[3,45:200,37]),c("UN","Denmark","Belgium","Netherlands","Norway","Italy","Spain","Germany"
  #                                   ,"UK","France","Austria","Sweden","Switzerland"),text.col=
  #          c("slateblue1","thistle3","coral4","green","burlywood1","hotpink","magenta","purple","lightgreen",
  #             "royalblue","violet","yellow","gray40"),
  #         cex=0.5,ncol=8)
  legend(as.Date("2020-01-25"),max(cumsum(For_quantile[3,45:200,37])),c("The United States","Italy","Spain","Germany"
                                                                        ,"The United Kingdom","France","Sweden"),text.col=
           c("slateblue1","Red","magenta","purple",rgb(0.1,0.7,0.2),
             "royalblue","yellow"),
         cex=0.7,ncol=4)
  # lines(c(as.Date("2020-02-15"),as.Date("2020-02-15")),c(0,1),col="blue",lty=4)
  # lines(c(as.Date("2020-03-01"),as.Date("2020-03-03")),c(0,1),col="blue",lty=4)          
 # lines(date_rangeB,1+0*home_rate_quantile[3,45:200,1],lty=2)
 # lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,14),col="blue",lty=2)
  # text(labels="Hubei outside Wuhan",x=as.Date("2020-04-23"),y=max(R0_quantile[3,,ii])-10,adj=0,col="blue")
  title(main="G",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=1.3); letR = 2 
  # title(main="Days for an individual to social-distance himself",adj=0.5,cex.main=1,font.main=1,
  #      cex.sub=1,font.sub=1,line=0.5)
  #  title(main="Proportion of suspected cases getting isolated",adj=0.5,cex.main=1,font.main=1,
  #       cex.sub=1,font.sub=1,line=0.5)
  #title(main="Proportion of population getting isolated",adj=0.5,cex.main=1,font.main=1,
  #     cex.sub=1,font.sub=1,line=0.5)
  title(main="Cumulated imported cases from other western countries",adj=0.5,cex.main=0.85,font.main=1,
        cex.sub=1,font.sub=1,line=0.5)
}
