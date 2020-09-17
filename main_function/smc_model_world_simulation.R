smc_model_world_simulation=function(para,theta,nn,dt,ttotal,wave,start_day,rank,quantile1,quantile2,
         quantile9,quantile10,
         quantile11, quantile12, quantile13, quantile14, quantile15, quantile16,quantile17,quantile18,
         quantile19,quantile20,quantile21,quantile22,a,r,citya,wuhanc,interc,ban_day,iso,sus,home,Chinac){
  
  t_length=ttotal
 
  traj_names=c("S_traj","E_traj","I_traj","C_traj","Rep_traj","beta_traj","Rec_traj","Die_traj","ISO_traj","For_traj","sus_traj","track_traj","iso_traj","home_traj","inhome_traj")
  GGcitiesline=array(0,dim=c(ttotal,length(traj_names),length(citylist)),dimnames=list(NULL,traj_names,citycode))
 
  # to test whether Wuhan level's interventions are more effective in controlling the epidemic,
  # we make western countries execute non-pharmaceutical interventions since a given timepoint at the first-month-average level of Wuhan after Wuhan shutdown
  Chinese_home_rate=mean(home_rate_quantile[3,45:75,1])
  Chinese_sus_rate=mean(sus_rate_quantile[3,45:75,1])
  Chinese_track_rate=mean(track_rate_quantile[3,45:75,1])
  Chinese_iso_rate=mean(iso_rate_quantile[3,45:75,1])
  
  # naming the transmission matrix needed for cross-regions transmission 
  nnfeature_names=c("susceptible","exposed1","exposed2","infect1","infect2")
  nnCities=array(0,dim=c(nn,length(citylist),5),dimnames=list(NULL,citylist,nnfeature_names))
  feature_names=c("susceptible","exposed1","exposed2","infect1","infect2","hospital",
                  "recovery1","recovery","to_death","death","wait","case","report","isolate","foreign","home")
  
  # the main simulation matrix, used to deposit the predicted daily data created during simulation, with totally 16 trajectories created and recorded
  storeL = array(0,dim=c(nn,t_length,length(feature_names),length(citylist)),dimnames = list(NULL,NULL,feature_names,citylist))

   # We assume cases are able to infect others since the second phase of incubation period with the same infectivity,
  scale_incubation=(0.5+0.5*theta[1,"pre_symp"])*theta[1,"incubation"]
  simzeta=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  
  # Different quantiles of R0 trajectory are destinated for each region to make them grow in a way
  # closer to real historical data during the period from 20191210 to 20200501.
  for (ii in 1:length(citylist)){
 
    for (tt in 1:144){
      #R0_plot[ttotal,,1]
      if(ii<=1){ 
        
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile1,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
        
      } else if(ii==2){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile2,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      } else if(ii<=32){  #simzeta[tt,,ii] <- matrix(rnorm(nn,mean = R0_plot[tt,b,ii]/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile9,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
       } else if(ii==33){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile10,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==34){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile11,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==35){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile12,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==36){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile13,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==37){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile14,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      
      else if(ii==38){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile15,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==39){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile16,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==40){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile17,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==41){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile18,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==42){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile19,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==43){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile20,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==44){
        simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile21,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
      else if(ii==45){
       simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],quantile22,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
      }
     }       
   }
  if(ttotal>=145){
    for(tt in 145:ttotal){
      simzeta[tt,,ii] <- matrix(rnorm(nn,mean = quantile(R0_plot[tt,,ii],0.5,na.rm=T)/(theta[ii,"isolation"]+scale_incubation),sd=wave),nrow = 1)
    }
  }
  
 
  
  # creating intervention-parameter matrix, including social-distancing rate, work-resuming rate, suspected-cases isolation rate and tracking-success possibility
  wave=wave
  sus_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:32){
    for(tt in 1:45){
      sus_r[tt,,ii]=matrix(rnorm(nn,mean=sus_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:ttotal){
       sus_r[tt,,ii]=matrix(rnorm(nn,mean=sus_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
    }
  }
  for (ii in 33:length(citylist)){
    for(tt in 1:45){
      sus_r[tt,,ii]=matrix(rnorm(nn,mean=sus_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:start_day-1){
      
      sus_r[tt,,ii]=matrix(rnorm(nn,mean=sus_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
      
      }
    for(tt in start_day:ttotal){
      sus_r[tt,,ii]=matrix(rnorm(nn,mean=Chinese_sus_rate,sd=wave),nrow=1,ncol=nn)
    }
  }
  track_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:32){
    for(tt in 1:45){
      track_r[tt,,ii]=matrix(rnorm(nn,mean=track_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:ttotal){
      
       track_r[tt,,ii]=matrix(rnorm(nn,mean=track_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
    }
  }
  for (ii in 33:length(citylist)){
    for(tt in 1:45){
      track_r[tt,,ii]=matrix(rnorm(nn,mean=track_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:start_day-1){
      track_r[tt,,ii]=matrix(rnorm(nn,mean=track_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
    }
  
    for(tt in start_day:ttotal){
      track_r[tt,,ii]=matrix(rnorm(nn,mean=Chinese_track_rate,sd=wave),nrow=1,ncol=nn)
    }
  }
  iso_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:32){
    for(tt in 1:45){
      iso_r[tt,,ii]=matrix(rnorm(nn,mean=iso_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:ttotal){
      iso_r[tt,,ii]=matrix(rnorm(nn,mean=iso_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
      }
  }
  for (ii in 33:length(citylist)){
    for(tt in 1:45){
      iso_r[tt,,ii]=matrix(rnorm(nn,mean=iso_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:start_day-1){
      iso_r[tt,,ii]=matrix(rnorm(nn,mean=iso_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
    }
    
    for(tt in start_day:ttotal){
      iso_r[tt,,ii]=matrix(rnorm(nn,mean=Chinese_iso_rate,sd=wave),nrow=1,ncol=nn)
    }
  }
  
  home_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:32){
    for(tt in 1:45){
      home_r[tt,,ii]=matrix(rnorm(nn,mean=home_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    for(tt in 46:ttotal){
      if(tt==1){home_rate_quantile[3,tt,ii]=0}
       home_r[tt,,ii]=matrix(rnorm(nn,mean=home_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
    }
  }
  for (ii in 33:length(citylist)){
    for(tt in 1:45){
      home_r[tt,,ii]=matrix(rnorm(nn,mean=home_rate_quantile[3,tt,ii],sd=0),nrow=1,ncol=nn)
    }
    
    for(tt in 46:start_day-1){
      home_r[tt,,ii]=matrix(rnorm(nn,mean=home_rate_quantile[3,tt,ii],sd=wave),nrow=1,ncol=nn)
    }
   
    for(tt in start_day:ttotal){
      home_r[tt,,ii]=matrix(rnorm(nn,mean=Chinese_home_rate,sd=wave),nrow=1,ncol=nn)
    }
  }
  
  
 
  
  
  # Add initial conditions
  for(ii in 1:length(citylist)){
    storeL[,1,"exposed1",citylist[ii]] <- theta[ii,"init_cases"]
    storeL[,1,"exposed2",citylist[ii]] <- theta[ii,"init_cases"]
    storeL[,1,"infect1",citylist[ii]] <- theta[ii,"init_cases"]/2
    storeL[,1,"infect2",citylist[ii]] <- theta[ii,"init_cases"]/2
    storeL[,1,"susceptible",citylist[ii]] <- theta[ii,"population"] - theta[ii,"init_cases"]
    storeL[,1,"hospital",citylist[ii]] <- 0
    storeL[,1,"recovery1",citylist[ii]] <- 0
    storeL[,1,"recovery",citylist[ii]] <- 0
    storeL[,1,"to_death",citylist[ii]] <- 0
    storeL[,1,"death",citylist[ii]] <- 0
    storeL[,1,"wait",citylist[ii]] <- 0
    storeL[,1,"case",citylist[ii]] <- 0
    storeL[,1,"report",citylist[ii]] <- 0
    #storeL[,1,"beta",citylist[ii]] <- t(simzeta[1,,citylist[ii]])
    #storeL[,1,"iso",citylist[ii]] <- t(iso_r[1,,citylist[ii]])
    #storeL[,1,"track",citylist[ii]] <- t(track_r[1,,citylist[ii]])
    #storeL[,1,"sus",citylist[ii]] <- t(sus_r[1,,citylist[ii]])
    storeL[,1,"isolate",citylist[ii]] <- 0
    storeL[,1,"foreign",citylist[ii]] <- 0 
    storeL[,1,"home",citylist[ii]] <- 0 
  }
  
  S_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  C_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  Rep_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  E_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  I_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  beta_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  Die_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  Rec_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  ISO_traj=matrix(NA,ncol=length(citylist),nrow=ttotal)
  For_traj=matrix(NA,ncol=length(citylist),nrow=ttotal)
  iso_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  sus_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  track_traj = matrix(NA,ncol=length(citylist),nrow=ttotal) 
  home_traj = matrix(NA,ncol=length(citylist),nrow=ttotal)
  inhome_traj= matrix(NA,ncol=length(citylist),nrow=ttotal)
  

  w=array(0,dim=c(nn,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  w[,1,]=1
  W=array(0,dim=c(nn,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  A=array(0,dim=c(nn,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  l_sample <- rep(NA,ttotal)
  lik_values=matrix(0,nrow=ttotal,ncol=length(citylist))
  
  # creating controller, used to decide whether western countries or China execure all or less of interventions.
  # if a, sus, home, iso are equal to 1 simultaneously, then none of NPIs are executed for China or western countries.
  if(a==1&citya<=32){
    for(ii in 1:citya){
      for (tt in 45:ttotal){
        iso_r[tt,,ii]= iso_r[tt,,ii]+10000*iso
        if(sus==1){
          sus_r[tt,,ii]=1
        } else{ sus_r[tt,,ii]=sus_r[tt,,ii]}
        home_r[tt,,ii]= home_r[tt,,ii]+10000*home
        simzeta[tt,,ii]=simzeta[tt,,ii]*r
      }
    }
  } #none NPIs are executed in China, instead of western countries
  else if(a==1&citya>=33){
    for(ii in 33:citya){
      for (tt in 45:ttotal){
        iso_r[tt,,ii]= iso_r[tt,,ii]+10000*iso
        if(sus==1){
          sus_r[tt,,ii]=1
        } else{ sus_r[tt,,ii]=sus_r[tt,,ii]}
        home_r[tt,,ii]= home_r[tt,,ii]+10000*home
        simzeta[tt,,ii]=simzeta[tt,,ii]*r
      }
    }
  } #none NPIs are executed in western countries, instead of China
  
  # adjustment of human mobility matrix
  for(tt in 2:ttotal){
    for(ii in 1:length(citylist)){
      for(country in 1:45){
        if(tt>=144){mobility4[ii,country,144]=mobility4[ii,country,144]*median((storeL[,tt-1,"susceptible",ii]+storeL[,tt-1,"exposed1",ii]+storeL[,tt-1,"exposed2",ii]))/theta[ii,"population"] }
        else{ mobility4[ii,country,tt]=mobility4[ii,country,tt]*median((storeL[,tt-1,"susceptible",ii]+storeL[,tt-1,"exposed1",ii]+storeL[,tt-1,"exposed2",ii]))/theta[ii,"population"] }
      }
    }
    
    wuhan_travel_time=45
    mobility3=mobility4
    if(tt<23){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.5*mobility4[domestic1,domestic2,tt]
        }
      }
    }
    
    else if(tt<=wuhan_travel_time){mobility3=mobility4}
    else if(tt<=99){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.3*mobility4[domestic1,domestic2,tt]
        }
      }
      mobility3[1,,tt]=0.1*mobility4[1,,tt]
      mobility3[,1,tt]=0.1*mobility4[,1,tt]
      mobility3[2,,tt]=0.1*mobility4[2,,tt]
      mobility3[,2,tt]=0.1*mobility4[,2,tt]
      for(domestic in 1:32){
        for(foreign in 33:45){
          # mobility3[domestic,foreign,tt]=0.13*mobility4[domestic,foreign,tt]
          # mobility3[foreign,domestic,tt]=0.13*mobility4[foreign,domestic,tt]
          mobility3[domestic,foreign,tt]=mobility4[domestic,foreign,tt]*27690/sum(mobility4[33:45,1:32,tt])
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*27690/sum(mobility4[33:45,1:32,tt])
        }
      }
    }else if(tt<=101){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.3*mobility4[domestic1,domestic2,tt]
        }
      }
      mobility3[1,,tt]=0.1*mobility4[1,,tt]
      mobility3[,1,tt]=0.1*mobility4[,1,tt]
      mobility3[2,,tt]=0.1*mobility4[2,,tt]
      mobility3[,2,tt]=0.1*mobility4[,2,tt]
      for(domestic in 1:32){
        for(foreign in 33:45){
          #mobility3[domestic,foreign,tt]=0.13*mobility4[domestic,foreign,tt]
          #mobility3[foreign,domestic,tt]=0.13*mobility4[foreign,domestic,tt]
          mobility3[domestic,foreign,tt]=mobility4[domestic,foreign,tt]*27690/sum(mobility4[33:45,1:32,tt])
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*27690/sum(mobility4[33:45,1:32,tt])
        }
      } 
      for(foreign1 in 33:44){
        mobility3[foreign1,45,tt]=0.01*mobility4[foreign1,45,tt]
        mobility3[45,foreign1,tt]=0.01*mobility4[45,foreign1,tt]
      }
      for(foreign1 in 33:44){
        for(foreign2 in 33:44){
          mobility3[foreign1,foreign2,tt]=0.5*mobility4[foreign1,foreign2,tt]
        }
      }
    } else if(tt<=110){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.3*mobility4[domestic1,domestic2,tt]
        }
      }
      mobility3[1,,tt]=0.1*mobility4[1,,tt]
      mobility3[,1,tt]=0.1*mobility4[,1,tt]
      mobility3[2,,tt]=0.1*mobility4[2,,tt]
      mobility3[,2,tt]=0.1*mobility4[,2,tt]
      for(domestic in 1:32){
        for(foreign in 33:45){
          #mobility3[domestic,foreign,tt]=0.13*0.63*mobility4[domestic,foreign,tt]
          # mobility3[foreign,domestic,tt]=0.13*0.63*mobility4[foreign,domestic,tt]
          mobility3[domestic,foreign,tt]=mobility4[domestic,foreign,tt]*17446/sum(mobility4[33:45,1:32,tt])
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*17446/sum(mobility4[33:45,1:32,tt])
        }
      }
      for(foreign1 in 33:44){
        mobility3[foreign1,45,tt]=0.01*mobility4[foreign1,45,tt]
        mobility3[45,foreign1,tt]=0.01*mobility4[45,foreign1,tt]
      } 
      for(foreign1 in 33:44){
        for(foreign2 in 33:44){
          mobility3[foreign1,foreign2,tt]=0.3*mobility4[foreign1,foreign2,tt]
        }
      }
    }else if(tt<=144){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.5*mobility4[domestic1,domestic2,tt]
        }
      }
      mobility3[1,,tt]=0.2*mobility4[1,,tt]
      mobility3[,1,tt]=0.2*mobility4[,1,tt]
      
      mobility3[2,,tt]=0.2*mobility4[2,,tt]
      mobility3[,2,tt]=0.2*mobility4[,2,tt]
      for(domestic in 1:32){
        for(foreign in 33:45){
          #mobility3[domestic,foreign,tt]=0.13*0.63*0.15*0.75*mobility4[domestic,foreign,tt]
          # mobility3[foreign,domestic,tt]=0.13*0.63*0.15*0.75*mobility4[foreign,domestic,tt]
          mobility3[domestic,foreign,tt]=mobility4[domestic,foreign,tt]*2000/sum(mobility4[33:45,1:32,tt])
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*2000/sum(mobility4[33:45,1:32,tt])
        }
      }
      
      for(foreign1 in 33:44){
        mobility3[foreign1,45,tt]=0.01*mobility4[foreign1,45,tt]
        mobility3[45,foreign1,tt]=0.01*mobility4[45,foreign1,tt]
      }
      for(foreign1 in 33:44){
        for(foreign2 in 33:44){
          mobility3[foreign1,foreign2,tt]=0.2*mobility4[foreign1,foreign2,tt]
        }
      }
    }
    
    # simulation of no Wuhan shutdown if wuhanc==1
    if(wuhanc==1){
      if(tt>=45&tt<=144){
        for (domestic1 in 1:32){
          for(domestic2 in 1:32){
            mobility3[domestic1,domestic2,tt]=0.5*mobility4[domestic1,domestic2,1]
          }
        }
      }
    }
    
    # simulation of complete travel ban between international cities since ban_day if interc==1
    if(interc==1&tt>=ban_day&tt<=144){
      for(foreign1 in 33:45){
        for(foreign2 in 33:45){
          mobility3[foreign1,foreign2,tt]=0
        }
      }
    }
    # simulation of complete travel ban between China and western countries if Chinac==1
    if(Chinac==1){
      for(domestic1 in 1:32){
        for(foreign2 in 33:45){
          mobility3[domestic1,foreign2,tt]=0
          mobility3[foreign2,domestic1,tt]=0
        }
      }
    }
    
    for (pp in 1:length(nnfeature_names)){
      zekcol=match(nnfeature_names[pp],feature_names)
      for (gg in 1:length(citylist)){
        h=storeL[,tt-1,zekcol,gg]
        for(hh in 1:nn){
          if(is.na(h[hh])==TRUE){h[hh]=0} #storeL[hh,119,zekcol,gg]} #storeL[hh,tt-2,zekcol,gg]}
        }
        # if(is.na(max(h))){h=storeL[,tt-2,zekcol,gg]}
        # b=which(h=="NaN")
        # if(length(bb)>0){bb=list(b[,1])
        # for (xx in bb){h[xx,]=storeL[,tt-2,zekcol,gg]}}
        nnCities[,gg,pp]=h
      }
    }
    date=tt
    print(datetime[date])
    # run process model
    storeL[,tt,,] <- process_model_world(nn,tt-1,tt,dt,theta,para,storeL[,tt-1,,],simzeta[tt,,],nnCities,citylist,citycode,mobility3,susceptibility,date,sus_r[tt,,],track_r[tt,,],iso_r[tt,,],home_r[tt,,])
    
    
    
    print(head(storeL[,tt,"report",]))
   
  }
  W=array(1,dim=c(nn,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  A=array(1,dim=c(nn,ttotal,length(citylist)),dimnames=list(NULL,NULL,citylist))
  
  return(storeL)
  write_csv(GGcitiesline,"C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/ggcitiesline5.csv")
}
