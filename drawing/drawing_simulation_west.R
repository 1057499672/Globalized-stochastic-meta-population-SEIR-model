drawing_simulation_west=function(ii){
  foreign_country=c("Simulation for Switzerland","Simulation for Sweden","Simulation for Austria","Simulation for France","Simulation for The United Kingdom","Simulation for Germany","Simulation for Spain",
                    "Simulation for Italy","Simulation for Norway","Simulation for The Netherlands","Simulation for Belgium","Simulation for Denmark","Simulation for The United States")
  name=foreign_country[ii-32]
  
  par(mar=c(2,3,2.5,2),mgp=c(2,0.55,0)) #mfrow=c(4,2),
  #layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6),4, 3, byrow = F))
  layout(matrix(c(1,2,3,4,5,6),2, 3, byrow = T),heights=c(1,1))
  
  plot(date_rangeB,a1[1,,13,ii],pch=19,ylim=c(0,pmax(
    max(a1[1,,13,ii]-c(0,head(a1[1,,13,ii],-1))),  max(a16China_noaction[1,,13,ii]-c(0,head(a16China_noaction[1,,13,ii],-1))),
    max(a16west_noaction[1,,13,ii]-c(0,head(a16west_noaction[1,,13,ii],-1)))  
    
    #max(a16China_noaction_China[1,]-c(0,head(a16China_noaction_China[1,],-1))),
    # max(a16west_noaction_China[1,]-c(0,head(a16west_noaction_China[1,],-1)))
  )*1.3)
  ,xlim=c(as.Date("2019-12-10")+5,as.Date("2020-6-26")-5),ylab="New confirmed cases reported everyday",col="white",xaxt="n")
  title(main="A",adj=0.5,cex.main=0.9,font.main=1,line=0.5)
  c=c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),as.Date("2020-09-01"),as.Date("2020-10-01"))
  lines(c(as.Date("2020-05-01"),as.Date("2020-05-01")),c(-1,1e8),col="blue",lty=2)
  axis.Date(1,at=c,format="%Y-%m")
  legend(as.Date("2019-12-10")-2, pmax(
    max(a1[1,,13,ii]-c(0,head(a1[1,,13,ii],-1))),  max(a16China_noaction[1,,13,ii]-c(0,head(a16China_noaction[1,,13,ii],-1))),
    max(a16west_noaction[1,,13,ii]-c(0,head(a16west_noaction[1,,13,ii],-1)))  
    
    # max(a16China_noaction_China[1,]-c(0,head(a16China_noaction_China[1,],-1))),
    #  max(a16west_noaction_China[1,]-c(0,head(a16west_noaction_China[1,],-1)))
  )*1.3,
  c("Original trajectory (R)","No NPI in China (R) ","No NPI in Western countries (L)"),                                        
  lty=1,col=c(rgb(1,0.2,0.1,1),rgb(0.2,0.8,0.1,0.9),rgb(0,0.2,0.9,1)),ncol=1,text.font=1,cex=0.7,bty="n",xpd=F)
  lines(date_rangeB,a16west_noaction[1,,13,ii]-c(0,head(a16west_noaction[1,,13,ii],-1)),type="l",col=rgb(0,0.2,0.9,1),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  par(new=T)
  plot(date_rangeB,rep(pmax(max(a16China_noaction[1,,13,ii]),max(a16west_noisolation[1,,13,ii]),max(a16west_noaction[1,,13,ii]),max(a16China_noisolation[1,,13,ii]),max(a16China_nosocial_distancing[1,,13,ii]),max(a16west_nosocial_distancing[1,,13,ii]),max(a16China_noshutdown[1,,13,ii]))*100,length(a12[1,,13,ii])),pch=19,
       ylim=c(0,pmax(max(a16China_noaction[1,,13,ii]-c(0,head(a16China_noaction[1,,13,ii],-1))))*1.7),
       #pmax(max(a16west_noaction_China[1,]-c(0,head(a16west_noaction_China[1,],-1))))*1.7),
       xlim=c(as.Date("2019-12-10")+5,as.Date("2020-06-26")-5),ylab="",yaxt="n",col="white",xaxt="n")
  axis(4)
  lines(date_rangeB,a16China_noaction[1,,13,ii]-c(0,head(a16China_noaction[1,,13,ii],-1)),type="l",col=rgb(0.2,0.8,0.1,0.9),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  lines(date_rangeB,a1[1,,13,ii]-c(0,head(a1[1,,13,ii],-1)),type="l",col=rgb(1,0.2,0.1,1),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  
  
  plot(date_rangeB,a1[1,,13,ii],pch=19,ylim=c(0,pmax(#max(a16all_noaction_China[1,]-c(0,head(a16all_noaction_China[1,],-1))),
    #max(a16China_noaction_China[1,]-c(0,head(a16China_noaction_China[1,],-1))),
    #max(a16all_noaction[1,,13,ii]-c(0,head(a16all_noaction[1,,13,ii],-1))),
    max(a16China_noisolation[1,,13,ii]-c(0,head(a16China_noisolation[1,,13,ii],-1))),
    max(a16China_nosocial_distancing[1,,13,ii]-c(0,head(a16China_nosocial_distancing[1,,13,ii],-1))),
    max(a16China_noshutdown[1,,13,ii]-c(0,head(a16China_noshutdown[1,,13,ii],-1))))*1.3 
    
    #max(a16China_noisolation_China[1,]-c(0,head(a16China_noisolation_China[1,],-1))),
    # max(a16China_nosocial_distancing_China[1,]-c(0,head(a16China_noisolation_China[1,],-1))),
    # max(a16China_noshutdown_China[1,]-c(0,head(a16China_noisolation_China[1,],-1))))*1.3
  )
  ,xlim=c(as.Date("2019-12-10")+5,as.Date("2020-6-26")-5),ylab="",col="white",xaxt="n")
  title(main="B",adj=0.5,cex.main=0.9,font.main=1,line=0.5)
  title(main=name,adj=0.5,cex.main=0.9,font.main=1,line=1.3)
  c=c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),as.Date("2020-09-01"),as.Date("2020-10-01"))
  lines(c(as.Date("2020-05-01"),as.Date("2020-05-01")),c(-1,1e8),col="blue",lty=2)
  axis.Date(1,at=c,format="%Y-%m")
  legend(as.Date("2019-12-10")-2, pmax(#max(a16all_noaction_China[1,]-c(0,head(a16all_noaction_China[1,],-1))),
    #max(a16China_noaction_China[1,]-c(0,head(a16China_noaction_China[1,],-1))),
    #max(a16all_noaction[1,,13,ii]-c(0,head(a16all_noaction[1,,13,ii],-1))),
    max(a16China_noisolation[1,,13,ii]-c(0,head(a16China_noisolation[1,,13,ii],-1))),
    max(a16China_nosocial_distancing[1,,13,ii]-c(0,head(a16China_nosocial_distancing[1,,13,ii],-1))),
    max(a16China_noshutdown[1,,13,ii]-c(0,head(a16China_noshutdown[1,,13,ii],-1))))*1.3,
    #max(a16China_noisolation_China[1,]-c(0,head(a16China_noisolation_China[1,],-1))),
    #max(a16China_nosocial_distancing_China[1,]-c(0,head(a16China_noisolation_China[1,],-1))),
    #max(a16China_noshutdown_China[1,]-c(0,head(a16China_noisolation_China[1,],-1))))*1.3,
    c("No inter-city travel restriction in China (L)","Original trajectory (R)","No isolation in China (R)","No social-distancing in China (R)"),                                        
    lty=1,col=c( rgb(0,0,0,1),rgb(1,0.2,0.1,1),rgb(1,0.5,0.1,0.9),rgb(0.1,0.2,0.9,0.9)),ncol=1,text.font=1,cex=0.7,bty="n",xpd=F)
  lines(date_rangeB,(a16China_noshutdown[1,,13,ii]-c(0,head(a16China_noshutdown[1,,13,ii],-1))),type="l",col=rgb(0,0,0,1),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  par(new=T)
  plot(date_rangeB,rep(pmax(max(a16China_noaction[1,,13,ii]),max(a16west_noisolation[1,,13,ii]),max(a16west_noaction[1,,13,ii]),max(a16China_noisolation[1,,13,ii]),max(a16China_nosocial_distancing[1,,13,ii]),max(a16west_nosocial_distancing[1,,13,ii]),max(a16China_noshutdown[1,,13,ii]))*100,length(a12[1,,13,ii])),pch=19,
       ylim=c(0,pmax(max(a16China_nosocial_distancing[1,,13,ii]-c(0,head(a16China_nosocial_distancing[1,,13,ii],-1))),max(a16China_noisolation[1,,13,ii]-c(0,head(a16China_noisolation[1,,13,ii],-1))))*1.7),
       #pmax(max(a16west_noaction_China[1,]-c(0,head(a16west_noaction_China[1,],-1))))*1.7),
       xlim=c(as.Date("2019-12-10")+5,as.Date("2020-06-26")-5),ylab="",yaxt="n",col="white",xaxt="n")
  axis(4)
   lines(date_rangeB,(a16China_nosocial_distancing[1,,13,ii]-c(0,head(a16China_nosocial_distancing[1,,13,ii],-1))),type="l",col=rgb(0.1,0.2,0.9,0.9),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  lines(date_rangeB,a16China_noisolation[1,,13,ii]-c(0,head(a16China_noisolation[1,,13,ii],-1)),type="l",col=rgb(1,0.5,0.1,0.9),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  lines(date_rangeB,a1[1,,13,ii]-c(0,head(a1[1,,13,ii],-1)),type="l",col=rgb(1,0.2,0.1,1),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  
  plot(date_rangeB,a1[1,,13,ii],pch=19,ylim=c(0,pmax(#max(a16all_noaction_China[1,]-c(0,head(a16all_noaction_China[1,],-1))),
    #max(a16China_noaction_China[1,]-c(0,head(a16China_noaction_China[1,],-1))),
    #max(a16all_noaction[1,,13,ii]-c(0,head(a16all_noaction[1,,13,ii],-1))),
    # max(a16west_nosocial_distancing_China[1,]-c(0,head(a16west_nosocial_distancing_China[1,],-1))),
    #  max(a16west_nosocial_distancing_China[1,]-c(0,head(a16west_nosocial_distancing_China[1,],-1))))*1.3)
    max(a16west_noisolation[1,,13,ii]-c(0,head(a16west_noisolation[1,,13,ii],-1))),
    max(a16west_nosocial_distancing[1,,13,ii]-c(0,head(a16west_nosocial_distancing[1,,13,ii],-1))))*1.3)
    ,xlim=c(as.Date("2019-12-10")+5,as.Date("2020-6-26")-5),ylab="",col="white",xaxt="n")
  title(main="C",adj=0.5,cex.main=0.9,font.main=1,line=0.5)
  c=c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),as.Date("2020-09-01"),as.Date("2020-10-01"))
  lines(c(as.Date("2020-05-01"),as.Date("2020-05-01")),c(-1,1e8),col="blue",lty=2)
  axis.Date(1,at=c,format="%Y-%m")
  legend(as.Date("2019-12-10")-2, pmax(#max(a16all_noaction_China[1,]-c(0,head(a16all_noaction_China[1,],-1))),
    #max(a16China_noaction_China[1,]-c(0,head(a16China_noaction_China[1,],-1))),
    #max(a16all_noaction[1,,13,ii]-c(0,head(a16all_noaction[1,,13,ii],-1))))*1.3,
    max(a16west_noisolation[1,,13,ii]-c(0,head(a16west_noisolation[1,,13,ii],-1))),
    max(a16west_nosocial_distancing[1,,13,ii]-c(0,head(a16west_nosocial_distancing[1,,13,ii],-1))))*1.3,
    c("No social-distancing in Western countries (L)","Original trajectory (R)","No isolation in Western countries (L)"),                                        
    lty=1,col=c(rgb(0.1,0.2,0.9,0.6),rgb(1,0.2,0.1,1),rgb(1,0.5,0.1,0.6)),ncol=1,text.font=1,cex=0.7,bty="n",xpd=F)
  lines(date_rangeB,a16west_noisolation[1,,13,ii]-c(0,head(a16west_noisolation[1,,13,ii],-1)),type="l",col=rgb(1,0.5,0.1,0.6),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  lines(date_rangeB,a16west_nosocial_distancing[1,,13,ii]-c(0,head(a16west_nosocial_distancing[1,,13,ii],-1)),type="l",col=rgb(0.1,0.2,0.9,0.6),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  par(new=T)
  plot(date_rangeB,rep(pmax(max(a16China_noaction[1,,13,ii]),max(a16west_noisolation[1,,13,ii]),max(a16west_noaction[1,,13,ii]),max(a16China_noisolation[1,,13,ii]),max(a16China_nosocial_distancing[1,,13,ii]),max(a16west_nosocial_distancing[1,,13,ii]),max(a16China_noshutdown[1,,13,ii]))*100,length(a12[1,,13,ii])),pch=19,
       ylim=c(0,pmax(max(a1[1,,13,ii]-c(0,head(a1[1,,13,ii],-1))))*1.7),
       #pmax(max(a16west_noaction_China[1,]-c(0,head(a16west_noaction_China[1,],-1))))*1.7),
       xlim=c(as.Date("2019-12-10")+5,as.Date("2020-06-26")-5),ylab="",yaxt="n",col="white",xaxt="n")
  axis(4)
  lines(date_rangeB,a1[1,,13,ii]-c(0,head(a1[1,,13,ii],-1)),type="l",col=rgb(1,0.2,0.1,1),xaxt="n",yaxt="n",xlab="",ylab="noaction",cex=2)
  }
