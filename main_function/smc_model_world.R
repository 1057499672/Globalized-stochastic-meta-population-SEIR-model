smc_model_world=function(para,theta,nn,dt,ttotal,wave,under_reporting){
  
  t_length=ttotal
  # nn = 100;   dt <- 0.25
  traj_names=c("S_traj","E_traj","I_traj","C_traj","Rep_traj","beta_traj","Rec_traj","Die_traj","ISO_traj","For_traj","sus_traj","track_traj","iso_traj","home_traj","inhome_traj")
  GGcitiesline=array(0,dim=c(ttotal,length(traj_names),length(citylist)),dimnames=list(NULL,traj_names,citycode))
  
  nnfeature_names=c("susceptible","exposed1","exposed2","infect1","infect2")
  #nnfeature_names matrix will be used for recording the movement of susceptibles, exposed infectives, and symptomatic infectives between regions during human mobility.
  
  nnCities=array(0,dim=c(nn,length(citylist),5),dimnames=list(NULL,citylist,nnfeature_names))
  
  feature_names=c("susceptible","exposed1","exposed2","infect1","infect2","hospital",
                  "recovery1","recovery","to_death","death","wait","case","report","isolate","foreign","home")
  
  storeL = array(0,dim=c(nn,t_length,length(feature_names),length(citylist)),dimnames = list(NULL,NULL,feature_names,citylist))
  
  
  #we define the serial interval as the sum of the first half of incubation period multiplying assymptomatic proportion, 
  #plus the second half of incubation period, and plus the symptomatic period before hospitalization
  simzeta=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist)) #to simulate random change of time-variant R0
  scale_incubation=(0.5+0.5*theta[1,"pre_symp"])*theta[1,"incubation"]  
  for (ii in 1:length(citylist)){
    simzeta1 <- matrix(rnorm(nn*t_length,mean = 0, sd = 0.6),nrow=ttotal)
    simzeta[1,,citylist[ii]] <-exp(simzeta1[1,])*theta[ii,"r0"]/(theta[ii,"free_infection_before_hospitalization"]+scale_incubation) #define IC
  } 
  
  #probability of successful tracking 
  track_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:length(citylist)){
    track_response<- matrix(rnorm(nn*t_length,mean = 0, sd = wave),nrow=ttotal)
    track_r[1,,citylist[ii]] <-exp(track_response[1,])*0.1/(exp(track_response[1,])*0.1+1)  # define IC
  } 
  #the speed of suspected-cases solation
  iso_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:length(citylist)){
    iso_response<- matrix(rnorm(nn*t_length,mean = 0, sd = wave),nrow=ttotal)
    iso_r[1,,citylist[ii]] <-exp(iso_response[1,])*7  # define IC
    iso_r[1,,citylist[ii]]=pmin(iso_r[1,,citylist[ii]],14)
    iso_r[1,,citylist[ii]]=pmax(iso_r[1,,citylist[ii]],1)
  } 
  #the speed for social-distanced people to resume work and become susceptible people again
  sus_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:length(citylist)){
    sus_response<- matrix(rnorm(nn*t_length,mean = 0, sd = wave),nrow=ttotal)
    #simzeta1 <- matrix(0,nrow=ttotal,ncol=nn)
    sus_r[1,,citylist[ii]] <-exp(sus_response[1,])*7  # define IC
    sus_r[1,,citylist[ii]]=pmin(sus_r[1,,citylist[ii]],14)
    sus_r[1,,citylist[ii]]=pmax(sus_r[1,,citylist[ii]],1)
    
  } 
  
  #the speed for susceptible people to become social-distanced and end susceptible state
  home_r=array(0,dim=c(ttotal,nn,length(citylist)),dimnames=list(NULL,NULL,citylist))
  for (ii in 1:length(citylist)){
    home_response<- matrix(rnorm(nn*t_length,mean = 0, sd = wave),nrow=ttotal)
    home_r[1,,citylist[ii]] <-exp(home_response[1,])*7 # define IC
    home_r[1,,citylist[ii]]=pmin(home_r[1,,citylist[ii]],14)
    home_r[1,,citylist[ii]]=pmax(home_r[1,,citylist[ii]],1)
  } 
  
  # Add initial condition of iterations
  for(ii in 1:length(citylist)){
    storeL[,1,"exposed1",citylist[ii]] <- theta[ii,"init_cases"]
    storeL[,1,"exposed2",citylist[ii]] <- theta[ii,"init_cases"] #initial exposed infectives 
    storeL[,1,"infect1",citylist[ii]] <- theta[ii,"init_cases"]/2
    storeL[,1,"infect2",citylist[ii]] <- theta[ii,"init_cases"]/2 #initial symptomatic infectives 
    storeL[,1,"susceptible",citylist[ii]] <- theta[ii,"population"] - theta[ii,"init_cases"] #initial susceptible people for each country
    storeL[,1,"hospital",citylist[ii]] <- 0 #initial hospitalized people due to COVID-19
    storeL[,1,"recovery1",citylist[ii]] <- 0 #recovering cases in the first compartment period
    storeL[,1,"recovery",citylist[ii]] <- 0  #recovering cases in the second compartment period
    storeL[,1,"to_death",citylist[ii]] <- 0  #dying cases in the first compartment period
    storeL[,1,"death",citylist[ii]] <- 0  #dying cases in the second compartment period
    storeL[,1,"wait",citylist[ii]] <- 0 #cases wating for antibody test or nuclear test
    storeL[,1,"case",citylist[ii]] <- 0 #total infected people
    storeL[,1,"report",citylist[ii]] <- 0 #total reported infected people
    storeL[,1,"isolate",citylist[ii]] <- 0 #total suspected cases in isolation
    storeL[,1,"foreign",citylist[ii]] <- 0  #foreign trajectory is used to record imported cases from 13 western countries
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
  
  for(tt in 2:ttotal){
    
    # DEBUG  tt=2
    # Add random walk on transmission 
    simzeta[tt,,] <- simzeta[tt-1,,]*exp(simzeta1[tt,])
    if(tt>143){simzeta[tt,,]=pmin(simzeta[tt,,],0.72046)} #set a ceiling at 5.2 for R0 in prediction period after 20200501
    
    iso_r[tt,,] <- iso_r[tt-1,,]*exp(iso_response[tt,])
    iso_r[tt,,]=pmin(iso_r[tt,,],14)
    iso_r[tt,,]=pmax(iso_r[tt,,],1)
    if(tt<=45){iso_r[tt,,]=0}
    if(tt==45){iso_r[tt,,]=7} #initialize suspected-cases isolation rate on 20200123 as 7 days per capita
    
    sus_r[tt,,] <- sus_r[tt-1,,]*exp(sus_response[tt,])
    sus_r[tt,,]=pmin(sus_r[tt,,],14)
    sus_r[tt,,]=pmax(sus_r[tt,,],1)
    if(tt<=45){sus_r[tt,,]=0}
    if(tt==45){sus_r[tt,,]=7} #initialize work-resuming rate on 20200123 as 7 days per capita
    
    home_r[tt,,] <- home_r[tt-1,,]*exp(home_response[tt,])
    home_r[tt,,]=pmin(home_r[tt,,],14)
    home_r[tt,,]=pmax(home_r[tt,,],1)
    if(tt<=45){home_r[tt,,]=0}
    if(tt==45){home_r[tt,,]=7} #initialize social-distancing rate on 20200123 as 7 days per capita
    
    track_r[tt,,] <- track_r[tt-1,,]*exp(track_response[tt,])/(track_r[tt-1,,]*exp(track_response[tt,])+1)
    if(tt<=45){track_r[tt,,]=0}
    if(tt==45){track_r[tt,,]=0.1} #initialize probability of successful tracking on 20200123 as 10%
    
    wuhan_travel_time=45
    # travel restrictions in place?
    
    #this part is to recorrect original mobility data between regions 
    mobility3=mobility4
    if(tt<23){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.5*mobility4[domestic1,domestic2,tt] #Human mobility between 20191210 and 20200101 is estiamted to be 50% of that on 20200101
        }
      }
    }
    else if(tt<=wuhan_travel_time){mobility3=mobility4}
    else if(tt<=99){
      for (domestic1 in 1:32){
        for(domestic2 in 1:32){
          mobility3[domestic1,domestic2,tt]=0.3*mobility4[domestic1,domestic2,tt] 
          #after Wuhan shutdown, cross-provinces mobility within China is restricted and screened,
          #as we estimate the proportion of infectives in people moving in or out is the same as the proportion of infectives in whole city populaiton, a scaling is adopted to reflect a stronger cross-provinces moving screen in China
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
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*27690/sum(mobility4[33:45,1:32,tt]) #after Wuhan shutdown, we estimated totally no more than 27690 people can enter or leave China
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
        mobility3[foreign1,45,tt]=0.01*mobility4[foreign1,45,tt] #after 20200317, we assume flights between Europe and the US reduced to 1% of before
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
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*17446/sum(mobility4[33:45,1:32,tt]) #after 20200319, totally no more than 17446 people can enter or leave China
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
    }else if(tt<=143){
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
          mobility3[foreign,domestic,tt]=mobility4[domestic,foreign,tt]*2000/sum(mobility4[33:45,1:32,tt])  #after 20200328, totally no more than 2000 people can enter or leave China
        }
      }
      for(foreign1 in 33:44){
        mobility3[foreign1,45,tt]=0.01*mobility4[foreign1,45,tt]
        mobility3[45,foreign1,tt]=0.01*mobility4[45,foreign1,tt] # mobility reduction between the US and other 12 western countries
      }
      for(foreign1 in 33:44){
        for(foreign2 in 33:44){
          mobility3[foreign1,foreign2,tt]=0.2*mobility4[foreign1,foreign2,tt] #mobility reduction between western countries other than the US
        }
      }
    }
    
    #this part is recording daily infectious state of each region, which is the basis for cross-regional infection through human mobility 
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
    
    #this part is realizations of iterating
    date=tt
    print(datetime[date])
    # run process model
    if(under_reporting==1){
      storeL[,tt,,] <- process_model_world2(nn,tt-1,tt,dt,theta,para,storeL[,tt-1,,],simzeta[tt,,],nnCities,citylist,citycode,mobility3,susceptibility,date,sus_r[tt,,],track_r[tt,,],iso_r[tt,,],home_r[tt,,])
    }
    else if(under_reporting==0){
      storeL[,tt,,] <- process_model_world(nn,tt-1,tt,dt,theta,para,storeL[,tt-1,,],simzeta[tt,,],nnCities,citylist,citycode,mobility3,susceptibility,date,sus_r[tt,,],track_r[tt,,],iso_r[tt,,],home_r[tt,,])
    }
    print(head(storeL[,tt,"report",]))
    # calculate weights
    #w[,tt] <- AssignWeights(confirmation_data,storeL,nn,theta,tt,ii)
    
    #w[,tt]=matrix(runif(nn,0,1),nn)
    # check likelihood isn't NA
    #if(is.na(max(w[1:nn,tt])) | max(w[1:nn,tt]) == 0){
    #  likelihood0 = -Inf
    #  return(list(S_trace=S_traj,
    #              C_local_trace=C_traj,
    #              Rep_local_trace=Rep_traj,
    #              E_trace=E_traj,
    #              I_trace=I_traj,
    #              beta_trace=beta_traj,
    #              recovery_trace=Rec_traj,
    #              death_trace=Die_traj,
    #              lik=likelihood0 ))
    # }
    
    
    # particle weights 
    if(tt<=143){
      w[,tt,] <- AssignWeights_World(confirmation_data,storeL,nn,theta,tt)
      #W[floor(runif(nn,min=1,max=nn)),]=0.1
      for (ii in 1:length(citylist)){
        b=which(is.na(storeL[,tt,"report",ii])==TRUE,arr.ind = T)
        # print(b)
        if(length(b)>0){
          for(bb in 1:length(b)){w[b[bb],tt,ii]=0}
        }
      }
      
      for (ii in 1:length(citylist)){
        if(is.na(max(w[1:nn,tt,ii])) | max(w[1:nn,tt,ii]) == 0){w[1:nn,tt,ii]=1} 
        b=which(is.na(w[,tt,ii])==TRUE,arr.ind = T)
        #  print(b)
        #   print(length(b))
        #  print(head(w[,tt,ii]))
        if(length(b)>0){
          for(bb in 1:length(b)){w[b[bb],tt,ii]=0}
        }
        
        #for (ii in 1:length(citylist)){
        # if(is.na(max(w[1:nn,tt,ii])) | max(w[1:nn,tt,ii]) == 0){w[1:nn,tt,ii]=1} 
        #  } 
        sum_weights <- sum(w[1:nn,tt,ii])
        #print(sum_weights)
        if(sum_weights==0|is.na(sum_weights)){sum_weights=1}
        W[1:nn,tt,ii] <- w[1:nn,tt,ii]/sum_weights
        if(max(W[1:nn,tt,ii])==0){W[1:nn,tt,ii]=1} 
        # print(head(W[1:nn,tt,ii]))
        #  b=which( W[1:nn,tt,ii]==max(W[1:nn,tt,ii]),arr.ind=T)
        # if(length(b)>1){b=b[1]}
        # if(length(b)==0){b=which(W[1:nn,tt,ii]==quantile(W[1:nn,tt,ii],0.5,na.rm=T),arr.ind = T)}
        
        A[, tt,ii] <- sample(1:nn,prob = W[1:nn,tt,ii],replace = T)
        storeL[,tt,,ii] <- storeL[ A[, tt,ii] ,tt,,ii]
        simzeta[tt,,ii] <- simzeta[tt, A[, tt,ii],ii] #- needed for random walk on beta
        sus_r[tt,,ii] <- sus_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        iso_r[tt,,ii] <- iso_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        track_r[tt,,ii] <- track_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        home_r[tt,,ii] <- home_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        # rep_r[tt,,ii] <- rep_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
      }
    }
    else{
      for (ii in 1:45){
        w[,tt,ii]=exp(dpois(floor(median(storeL[,tt,"report",ii],na.rm=F)),lambda = storeL[,tt,"report",ii],log=T))
        # w[,tt,ii]=1
        b=which(is.na(storeL[,tt,"report",ii])==TRUE,arr.ind = T)
        # print(b)
        if(length(b)>0){
          for(bb in 1:length(b)){w[b[bb],tt,ii]=0}
        }
        sum_weights <- sum(w[1:nn,tt,ii])
        #print(sum_weights)
        if(sum_weights==0|is.na(sum_weights)){sum_weights=1}
        W[1:nn,tt,ii] <- w[1:nn,tt,ii]/sum_weights
        if(max(W[1:nn,tt,ii])==0){W[1:nn,tt,ii]=1} 
        # print(head(W[1:nn,tt,ii]))
        # b=which(W[1:nn,tt,ii]==max(W[1:nn,tt,ii]),arr.ind=T)
        # print(b)
        # if(length(b)>1){b=b[1]}
        # if(length(b)==0){b=which(W[1:nn,tt,ii]==quantile(W[1:nn,tt,ii],0.5,na.rm=T),arr.ind = T)}
        
        A[, tt,ii] <- sample(1:nn,prob = W[1:nn,tt,ii],replace = T)
        storeL[,tt,,ii] <- storeL[ A[, tt,ii] ,tt,,ii]
        simzeta[tt,,ii] <- simzeta[tt, A[, tt,ii],ii] #- needed for random walk on beta
        sus_r[tt,,ii] <- sus_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        iso_r[tt,,ii] <- iso_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        track_r[tt,,ii] <- track_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        home_r[tt,,ii] <- home_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
        # rep_r[tt,,ii] <- rep_r[tt, A[, tt,ii],ii] #- needed for random walk on beta
      }
    }
    
    print(head(w[,tt,]))
    
  }
  #Output of result
  for (ii in 1:length(citylist)){
    locs <-  sample(1:nn,prob = W[1:nn,ttotal,ii],replace = T)
    l_sample[ttotal] <- locs[1]
    
    C_traj[ttotal,ii] <- storeL[l_sample[ttotal],ttotal,"case",citylist[ii]]
    Rep_traj[ttotal,ii] <- storeL[l_sample[ttotal],ttotal,"report",citylist[ii]]
    S_traj[ttotal,ii] <- storeL[l_sample[ttotal],ttotal,"susceptible",citylist[ii]]
    E_traj[ttotal,ii] <- storeL[l_sample[ttotal],ttotal,"exposed2",citylist[ii]] +storeL[l_sample[ttotal],ttotal,"exposed1",citylist[ii]]
    I_traj[ttotal,ii] <- storeL[l_sample[ttotal],ttotal,"infect1",citylist[ii]]+storeL[l_sample[ttotal],ttotal,"infect2",citylist[ii]]
    beta_traj[ttotal,ii] <- simzeta[ttotal,l_sample[ttotal],citylist[ii]]
    Rec_traj[ttotal,ii]=storeL[l_sample[ttotal],ttotal,"recovery",citylist[ii]]
    Die_traj[ttotal,ii]=storeL[l_sample[ttotal],ttotal,"death",citylist[ii]]
    ISO_traj[ttotal,ii]=storeL[l_sample[ttotal],ttotal,"isolate",citylist[ii]]
    For_traj[ttotal,ii]=storeL[l_sample[ttotal],ttotal,"foreign",citylist[ii]]
    iso_traj[ttotal,ii] <- iso_r[ttotal,l_sample[ttotal],citylist[ii]]
    sus_traj[ttotal,ii] <- sus_r[ttotal,l_sample[ttotal],citylist[ii]]
    track_traj[ttotal,ii] <- track_r[ttotal,l_sample[ttotal],citylist[ii]]
    home_traj[ttotal,ii] <- home_r[ttotal,l_sample[ttotal],citylist[ii]]
    inhome_traj[ttotal,ii]=storeL[l_sample[ttotal],ttotal,"home",citylist[ii]]
    
    # rep_traj[ttotal,ii] <- rep_r[ttotal,l_sample[ttotal],citylist[ii]]
    
    for(hh in seq(ttotal,2,-1)){
      l_sample[hh-1] <- A[l_sample[hh],hh,ii] # have updated indexin
      C_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"case",citylist[ii]]
      Rep_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"report",citylist[ii]]
      S_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"susceptible",citylist[ii]]
      E_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"exposed2",citylist[ii]]+ storeL[l_sample[hh-1],hh-1,"exposed1",citylist[ii]]
      I_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"infect1",citylist[ii]]+ storeL[l_sample[hh-1],hh-1,"infect2",citylist[ii]]
      beta_traj[hh-1,ii] <- simzeta[hh-1,l_sample[hh-1],citylist[ii]]
      Rec_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"recovery",citylist[ii]]
      Die_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"death",citylist[ii]]
      ISO_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"isolate",citylist[ii]]
      For_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"foreign",citylist[ii]]
      iso_traj[hh-1,ii] <- iso_r[hh-1,l_sample[hh-1],citylist[ii]]
      # rep_traj[hh-1,ii] <- rep_r[hh-1,l_sample[hh-1],citylist[ii]]
      track_traj[hh-1,ii] <- track_r[hh-1,l_sample[hh-1],citylist[ii]]
      sus_traj[hh-1,ii] <- sus_r[hh-1,l_sample[hh-1],citylist[ii]] 
      home_traj[hh-1,ii] <- home_r[hh-1,l_sample[hh-1],citylist[ii]] 
      inhome_traj[hh-1,ii] <- storeL[l_sample[hh-1],hh-1,"home",citylist[ii]]
    }
    
    traj=matrix(0,nrow=ttotal,ncol=length(traj_names))
    traj[,1]=S_traj[,ii]
    traj[,2]=E_traj[,ii]
    traj[,3]=I_traj[,ii]
    traj[,4]=C_traj[,ii]
    traj[,5]=Rep_traj[,ii]
    traj[,6]=beta_traj[,ii]
    traj[,7]=Rec_traj[,ii]
    traj[,8]=Die_traj[,ii]
    traj[,9]=ISO_traj[,ii]
    traj[,10]=For_traj[,ii]
    traj[,11]=sus_traj[,ii]
    traj[,12]=track_traj[,ii]
    traj[,13]=iso_traj[,ii]
    traj[,14]=home_traj[,ii]
    traj[,15]=inhome_traj[,ii]
    #traj[,14]=rep_traj[,ii]
    
    for(dd in 1:length(traj_names)){
      GGcitiesline[,dd,ii]=traj[,dd]
    }
  }
  
  #recording imported cases into China from 13 western countries
  foreign_into_China=array(0,dim=c(ttotal,length(citylist)),dimnames=list(NULL,citylist))
  for (gg in 1:length(citylist)){
    h=GGcitiesline[,10,gg]
    for (today in 1:ttotal){
      k=h[today]
      foreign_into_China[today,gg]=k 
    }
  }  
  
  #-----------------------------------------------------------------------------------------#
  #calculate likelihood
  lik_values[1,]=0
  likelihood0=rep(0,length(citylist))
  for(ii in 1:length(citylist)){
    for(tt in 2:ttotal){
      if(w[1,tt,ii]==1){lik_values[tt,ii]=-1000000}
      else{lik_values[tt,ii] = log(sum(w[1:nn,tt,ii]))}
      # log-likelihoods
    }
  }
  
  for(ii in 1:length(citylist)){
    likelihood0[ii] = -ttotal*log(nn)+ sum(lik_values[,ii]) # add full averaged log-likelihoods   
  }
  
  #GGcitiesline=as_tibble(GGcitiesline)
  return(list(GGcitiesline,foreign_into_China,likelihood0)) #,simulator))  
  
  #GGcitiesline=as_tibble(GGcitiesline)
  #storeL=as_tibble(storeL)    
  #simulator=as_tibble(simulator)
  #write_csv(storeL,"C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/storeL.csv")
  write_csv(GGcitiesline,"C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/ggcitiesline5.csv")
  #write_csv(simulator,"C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/simulator.csv")
}
