drawing_simulation_mobility_ban=function(country1,country2,country3,country4,country5,country6,original_trajectory){
  
  par(mar=c(0.3,0.3,2,0.3),mgp=c(2,0.55,0)) #mfrow=c(4,2),
  layout(matrix(c(1,1,1,2,3,4,5,6,7),3, 3, byrow = T),heights=c(1,5,5))
  plot(date_rangeB,a1[1,,13,1],pch=19,ylim=c(0,1),xlim=c(as.Date("2019-12-10")+5,as.Date("2020-10-04")-5),ylab="",col="white",xaxt="n",yaxt="n",bty="n")
  legend(as.Date("2019-12-10")+5,2,c("Original trajectory","Mobility ban since 2020-01-23","Mobility ban since 2020-02-07","Mobility ban since 2020-02-22","Mobility ban since 2020-03-08"),                                        
         fill=c(rgb(0.1,0.2,0.1,1),rgb(0.1,0.2,0.1,0.6),rgb(0.1,0.2,0.1,0.5),rgb(0.1,0.2,0.1,0.4),rgb(0.1,0.2,0.1,0.3)),ncol=4,text.font=1,cex=0.85,bty="n",xpd=T)
  
  #legend(as.Date("2019-12-10")+5,2,c("Original trajectory","Taking action from 2020-05-07","Taking action from 2020-04-22","Taking action from 2020-04-07",
  #                                  "Taking action from 2020-03-28","Taking action from 2020-03-18","Taking action from 2020-03-08","Taking action from 2020-02-27"),
  #     fill=c(rgb(1,0,0,1),rgb(1,0,0,0.55),rgb(1,0,0,0.4),rgb(1,0,0,0.3),rgb(1,0,0,0.2),rgb(1,0,0,0.1),rgb(1,0.5,0,0.9),rgb(1,0.5,0,0.6)),ncol=4,text.font=1,cex=0.85,bty="n",xpd=T)
  
  foreign_country=c("Simulation for Switzerland","Simulation for Sweden","Simulation for Austria","Simulation for France","Simulation for The United Kingdom","Simulation for Germany","Simulation for Spain",
                    "Simulation for Italy","Simulation for Norway","Simulation for The Netherlands","Simulation for Belgium","Simulation for Denmark","Simulation for The United States")
  
  countrylist=c(country1,country2,country3,country4,country5,country6)
  
  par(mar=c(2,3,1.5,2),mgp=c(2,0.55,0)) #mfrow=c(4,2),
  #layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6),4, 3, byrow = F))
  #layout(matrix(c(1,2,3,4,5,6),2, 3, byrow = T),heights=c(1,1))
  for (ii in 1:6){
    country=countrylist[ii]
    name=foreign_country[countrylist[ii]-32]
    
    plot(date_rangeB,a1[1,,13,country],pch=19,ylim=c(0,pmax(#max(a16all_noaction_China[1,]-c(0,head(a16all_noaction_China[1,],-1))),
      max(a1[1,,13,country]),max(a12[1,,13,country]),max(a13[1,,13,country]),max(a14[1,,13,country]),max(a15[1,,13,country]))*1.1)
      ,xlim=c(as.Date("2019-12-10")+5,as.Date("2020-10-04")-5),ylab="",col="white",xaxt="n")
    #title(main="C",adj=0.5,cex.main=0.9,font.main=1,line=0.5)
    c=c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),as.Date("2020-09-01"),as.Date("2020-10-01"))
    lines(c(as.Date("2020-05-01"),as.Date("2020-05-01")),c(-1,1e8),col="blue",lty=2)
    axis.Date(1,at=c,format="%Y-%m")
    lines(date_rangeB,a1[1,,13,country],type="l",col=rgb(0,0,0,1),xaxt="n",yaxt="n",xlab="",ylab="299",cex=2)
    #points(date_rangeC,confirmation[1:144,ii],cex=0.85,pch=1,col="black")
    #lines(date_rangeB,a2[1,,13,country],type="l",col=rgb(1,0,0,0.55),xaxt="n",yaxt="n",xlab="2020-05-07",ylab="150",cex=2)
    #lines(date_rangeB,a3[1,,13,country],type="l",col=rgb(1,0,0,0.4),xaxt="n",yaxt="n",xlab="",ylab="2020-04-22",cex=2)
    #lines(date_rangeB,a4[1,,13,country],type="l",col=rgb(1,0,0,0.3),xaxt="n",yaxt="n",xlab="",ylab="2020-04-07",cex=2)
    #lines(date_rangeB,a5[1,,13,country],type="l",col=rgb(1,0,0,0.2),xaxt="n",yaxt="n",xlab="",ylab="2020-03-28",cex=2)
    #lines(date_rangeB,a6[1,,13,country],type="l",col=rgb(1,0,0,0.1),xaxt="n",yaxt="n",xlab="",ylab="2020-03-18",cex=2)
    #lines(date_rangeB,a7[1,,13,country],type="l",col=rgb(1,0.5,0,0.9),xaxt="n",yaxt="n",xlab="",ylab="2020-03-08",cex=2)
    #lines(date_rangeB,a8[1,,13,country],type="l",col=rgb(1,0.5,0,0.6),xaxt="n",yaxt="n",xlab="",ylab="2020-02-27",cex=2)
    #lines(date_rangeB,a9[1,,13,ii],type="l",col=rgb(1,0,0,0.2),xaxt="n",yaxt="n",xlab="",ylab="2020-02-17",cex=2)
    #lines(date_rangeB,a10[1,,13,ii],type="l",col=rgb(1,0,0,0.1),xaxt="n",yaxt="n",xlab="",ylab="2020-02-07",cex=2)
    #lines(date_rangeB,a10[1,,13,ii],type="l",col=rgb(1,0,0,1),xaxt="n",yaxt="n",xlab="",ylab="2020-01-23",cex=2)
    #lines(date_rangeB,a11[1,,13,ii],type="l",col=rgb(1,0,0,1),xaxt="n",yaxt="n",xlab="",ylab="2020-01-23",cex=2)
    lines(date_rangeB,a12[1,,13,country],type="l",col=rgb(0.1,0.2,0.1,1),xaxt="n",yaxt="n",xlab="",ylab="299+105",cex=2)
    lines(date_rangeB,a13[1,,13,country],type="l",col=rgb(0.1,0.2,0.1,0.6),xaxt="n",yaxt="n",xlab="",ylab="299+90",cex=2)
    lines(date_rangeB,a14[1,,13,country],type="l",col=rgb(0.1,0.2,0.1,0.5),xaxt="n",yaxt="n",xlab="",ylab="299+75",cex=2)
    lines(date_rangeB,a15[1,,13,country],type="l",col=rgb(0.1,0.2,0.1,0.4),xaxt="n",yaxt="n",xlab="",ylab="299+60",cex=2)
    lines(date_rangeB,a16[1,,13,country],type="l",col=rgb(0.1,0.2,0.1,0.3),xaxt="n",yaxt="n",xlab="",ylab="299+45",cex=2)
    title(main=name,adj=0.7,cex.main=0.9,font.main=1,line=0.5)
    start_date <- as.Date("2019-12-10") # first case
    end_date=as.Date("2020-05-01")# period to forecast ahead
    date_rangeC <- seq(start_date,end_date,1)
    if(original_trajectory==TRUE){
      points(date_rangeC,confirmation[1:144,country],cex=0.85,pch=1,col="blue")}
    par(new=T)
    plot(date_rangeB,a1[1,,13,country],pch=19,
         ylim=c(0,pmax(max(a1[1,,13,country]-c(0,head(a1[1,,13,country],-1))),
                       max(a12[1,,13,country]-c(0,head(a12[1,,13,country],-1))),
                       max(a13[1,,13,country]-c(0,head(a13[1,,13,country],-1))),
                       max(a14[1,,13,country]-c(0,head(a14[1,,13,country],-1))),
                       max(a15[1,,13,country]-c(0,head(a15[1,,13,country],-1))),
                       max(a16[1,,13,country]-c(0,head(a16[1,,13,country],-1))))*3.95),
         #pmax(max(a16west_noaction_China[1,]-c(0,head(a16west_noaction_China[1,],-1))))*1.7),
         xlim=c(as.Date("2019-12-10")+5,as.Date("2020-10-04")-5),ylab="",col="white",xaxt="n",yaxt="n")
    axis(4)
    lines(date_rangeB,a1[1,,13,country]-c(0,head(a1[1,,13,country],-1)),type="l",col=rgb(0,0,0,1),xaxt="n",yaxt="n",xlab="",ylab="299+105",cex=2)
    lines(date_rangeB,a12[1,,13,country]-c(0,head(a12[1,,13,country],-1)),type="l",col=rgb(0.1,0.2,0.1,1),xaxt="n",yaxt="n",xlab="",ylab="299+105",cex=2)
    lines(date_rangeB,a13[1,,13,country]-c(0,head(a13[1,,13,country],-1)),type="l",col=rgb(0.1,0.2,0.1,0.6),xaxt="n",yaxt="n",xlab="",ylab="299+90",cex=2)
    lines(date_rangeB,a14[1,,13,country]-c(0,head(a14[1,,13,country],-1)),type="l",col=rgb(0.1,0.2,0.1,0.5),xaxt="n",yaxt="n",xlab="",ylab="299+75",cex=2)
    lines(date_rangeB,a15[1,,13,country]-c(0,head(a15[1,,13,country],-1)),type="l",col=rgb(0.1,0.2,0.1,0.4),xaxt="n",yaxt="n",xlab="",ylab="299+60",cex=2)
    lines(date_rangeB,a16[1,,13,country]-c(0,head(a16[1,,13,country],-1)),type="l",col=rgb(0.1,0.2,0.1,0.3),xaxt="n",yaxt="n",xlab="",ylab="299+45",cex=2)
  }
  
}
