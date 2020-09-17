process_model_world=function(nn,t_start,t_end,dt,theta,para,simTab,simzetaA,nnCities,citylist,citycode,mobility3,susceptibility,tt,susA,trackA,isoA,homeA){
 
  for(ii in 1:length(citycode)){
   
    susceptible_t <- simTab[,"susceptible",citylist[ii]] # input function
    exposed_t1 <- simTab[,"exposed1",citylist[ii]] # input functio
    exposed_t2 <- simTab[,"exposed2",citylist[ii]] # input function
    infectious_t1 <- simTab[,"infect1",citylist[ii]] # input function
    infectious_t2 <- simTab[,"infect2",citylist[ii]] # input function
   
    hospital_t= simTab[,"hospital",citylist[ii]]  # input function
    recovery_t1=simTab[,"recovery1",citylist[ii]]  # input function
    recovery_t=simTab[,"recovery",citylist[ii]]  # input function
    to_death_t=simTab[,"to_death",citylist[ii]]  # input function
    death_t=simTab[,"death",citylist[ii]]  # input function
  
    waiting_t <- simTab[,"wait",citylist[ii]] # input function
    cases_t <- simTab[,"case",citylist[ii]] # input function
    reports_t <- simTab[,"report",citylist[ii]] # input function
    isolate_t=simTab[,"isolate",citylist[ii]]  # input function
    home_t=simTab[,"home",citylist[ii]]  # input function
    
    inf_rate = (simzetaA[,ii]/theta[ii,"population"])*dt  
    inc_rate = (1/theta[ii,"incubation"])*2*dt
    rec_rate = (1/theta[ii,"free_infection_before_hospitalization"])*2*dt 
    recover_rate=(1/theta[ii,"recover"])*2*dt 
    die_rate=(1/theta[ii,"die"])*dt  
    track_prop=trackA[,ii]
    
    if(tt>=45){
      sus_rate=(1/susA[,ii])*dt
      home_rate=(1/homeA[,ii])*dt
      iso_rate=(1/isoA[,ii])*dt
    }
    else
    {sus_rate=0
    home_rate=0
    iso_rate=0
    }
   
    #rep_rate = (1/theta[ii,"report"])*dt  
    #Here, we set different initial reporting rate for Chinese and western region on 20191210, and this reporting rate will self-accelerate.
    if(tt<=49&ii<=32){rep_rate=(1/(para[ii,"report1"]-tt*1.7/49))*dt}
    else if(tt<=60&ii<=32){rep_rate=(1/para[ii,"report2"])*dt}
    else if(tt<=75&ii<=32){rep_rate=(1/para[ii,"report3"])*dt}
    else if(tt<=90&ii<=32){rep_rate=(1/para[ii,"report4"])*dt}
    else if(tt>90&ii<=32){rep_rate=(1/para[ii,"report5"])*dt}
    #else if(tt>85&ii<=32){rep_rate=(1/para[ii,"report6"])*dt}
    # else if(tt<=49&ii>=33){rep_rate=(1/(para[ii,"report1"]-tt*7/49))*dt}
    #  else if(tt<=60&ii>=33){rep_rate=(1/(para[ii,"report2"]-(tt-49)*2.5/11))*dt}
    else if(tt<=72&ii>=33){rep_rate=(1/(para[ii,"report1"]-(tt)*67.5/72))*dt}
    else if(tt<=90&ii>=33){rep_rate=(1/para[ii,"report3"])*dt}
    else if(tt>90&ii>=33){rep_rate=(1/para[ii,"report4"])*dt}
    
    iso_to_h_rate=(1/7)*dt  #1.45=6.4-(6.4*3.2+3.8*(1.+6.4))/(3.5+6.4)
    die_prop=theta[ii,"die_prop"]  
    
    # prob_rep <- exp(-(1/(rep_rate/dt))*(1/theta[ii,"recover"])) # \u{fffd}\u{173}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{1f0}\u{fffd}\u{53c}\u{fffd}\u{fffd}\u{47e}\u{fffd}\u{22c}\u{fffd}\u{fffd}\u{fffd}\u{132}\u{fffd}\u{fffd}\u{fffd}
    # prob_before_iso=exp(-(1/(iso_rate/dt))*(1/theta[ii,"recover"]))
    prob_recover_in_iso_exp1=exp(-(1/(iso_to_h_rate/dt))*(1/(theta[ii,"recover"]+theta[ii,"free_infection_before_hospitalization"]+theta[ii,"incubation"]*0.75)))
    prob_recover_in_iso_exp2=exp(-(1/(iso_to_h_rate/dt))*(1/(theta[ii,"recover"]+theta[ii,"free_infection_before_hospitalization"]+theta[ii,"incubation"]*0.25)))
    prob_recover_in_iso_inf1=exp(-(1/(iso_to_h_rate/dt))*(1/(theta[ii,"recover"]+theta[ii,"free_infection_before_hospitalization"]*0.75)))
    prob_recover_in_iso_inf2=exp(-(1/(iso_to_h_rate/dt))*(1/(theta[ii,"recover"]+theta[ii,"free_infection_before_hospitalization"]*0.25)))
    prob_recover_in_iso=(exposed_t1*prob_recover_in_iso_exp1+exposed_t2*prob_recover_in_iso_exp2+prob_recover_in_iso_inf1*infectious_t1+prob_recover_in_iso_inf2*infectious_t2)/(exposed_t1+exposed_t2+infectious_t1+infectious_t2)
    
    ###############\u{3a2}\u{52a}\u{123}\u{fffd}\u{2fea}\u{2bc}    
    if(tt<=143){
      tt=tt
    }else {tt=143}
    
    
    for(uu in seq((t_start+dt),t_end,dt)){
      mobility=t(as.matrix(mobility3[,,tt]))
      outbound_susceptible=nnCities[,ii,"susceptible"]*(sum(mobility[,ii])*dt/theta[ii,"population"])
      outbound_exposed1=nnCities[,ii,"exposed1"]*(sum(mobility[,ii])*dt/theta[ii,"population"])
      outbound_exposed2=nnCities[,ii,"exposed2"]*(sum(mobility[,ii])*dt/theta[ii,"population"])
      outbound_infective1=nnCities[,ii,"infect1"]*(sum(mobility[,ii])*dt/theta[ii,"population"])
      outbound_infective2=nnCities[,ii,"infect2"]*(sum(mobility[,ii])*dt/theta[ii,"population"])
      
      
      susceptible_t=(susceptible_t-outbound_susceptible+nnCities[,,"susceptible"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])) #\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{4e6}\u{fffd}\u{534}\u{fffd}\u{2a9}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{4bb}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{5f8}\u{fffd}\u{fffd}\u{fffd}\u{23a}
      if(tt<=45){
        S1=susceptible_t*theta[ii,"pre_symp"]*pmax((exposed_t1-outbound_exposed1),0)+susceptible_t*pmax((exposed_t2-outbound_exposed2),0)+susceptible_t*(infectious_t1-outbound_infective1)+susceptible_t*(infectious_t2-outbound_infective2)
        S2=susceptible_t*(nnCities[,,"exposed2"]+nnCities[,,"exposed1"]*theta[ii,"pre_symp"]+nnCities[,,"infect1"]+nnCities[,,"infect2"])%*%(mobility3[,ii,tt]*dt/theta[,"population"])}
      else{
        S1=susceptible_t*theta[ii,"pre_symp"]*pmax((exposed_t1-outbound_exposed1),0)+susceptible_t*pmax((exposed_t2-outbound_exposed2),0)+susceptible_t*infectious_t1+susceptible_t*infectious_t2
        S2=susceptible_t*(nnCities[,,"exposed2"]+nnCities[,,"exposed1"]*theta[ii,"pre_symp"])%*%(mobility3[,ii,tt]*dt/theta[,"population"])
      }    
      # S_to_home=pmax(susceptible_t*home_rate,0)
      # home_to_S=      
      S_to_S0=pmax(susceptible_t*home_rate,0)
      S0_to_S=pmax(home_t*sus_rate,0)
      
      S_to_E1 <-  inf_rate*(S1+S2)  #*(1-sus_prop*susceptibility[ii,tt])

      E1_to_ISO=pmax(exposed_t1*iso_rate*track_prop,0)
      E2_to_ISO=pmax(exposed_t2*iso_rate*track_prop,0)
      I1_to_ISO=pmax(infectious_t1*iso_rate*track_prop,0)
      I2_to_ISO=pmax(infectious_t2*iso_rate*track_prop,0)
      
      E1_to_E2 <- pmax((exposed_t1-E1_to_ISO-outbound_exposed1)*inc_rate+(nnCities[,,"exposed1"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])*inc_rate),0)
      E2_to_I1 <- (pmax((exposed_t2-E2_to_ISO-outbound_exposed2)*inc_rate+(nnCities[,,"exposed2"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])*inc_rate),0))*(1-theta[1,"pre_symp"])
      E2_to_H=(pmax((exposed_t2-E2_to_ISO-outbound_exposed2)+(nnCities[,,"exposed2"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])),0))*theta[1,"pre_symp"]*(1/(theta[ii,"recover"]+3.8))*dt
      if(tt<=45){
        I1_to_I2 <- pmax((infectious_t1-I1_to_ISO-outbound_infective1)*rec_rate+(nnCities[,,"infect1"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])*rec_rate),0)
        I2_to_H <- pmax((infectious_t2-I2_to_ISO-outbound_infective2)*rec_rate+(nnCities[,,"infect2"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])*rec_rate),0)
      }else{
        I1_to_I2 <- pmax((infectious_t1-I1_to_ISO)*rec_rate,0)
        I2_to_H <- pmax((infectious_t2-I2_to_ISO)*rec_rate,0)
      }
      ISO_to_H=pmax(isolate_t*iso_to_h_rate*prob_recover_in_iso,0)
      # ISO_to_Case=pmax(isolate_t*(1/6.4)*dt,0)  #1/6.4=1/4*1/1.6
      ISO_to_Case=pmax(isolate_t*(1/1.6*((exposed_t1)/(exposed_t1+exposed_t2+infectious_t1+infectious_t2)))*dt,0)
      #\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}
      H_to_recovery1=hospital_t*recover_rate  #\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{1f0}\u{fffd}\u{fffd}\u{fffd}
      recovery1_to_recovery2=recovery_t1*recover_rate  #\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{13a}\u{fffd}\u{fffd}\u{fffd}
      new_death_t=to_death_t*die_rate #\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}
      #\u{fffd}\u{3f1}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}      
      W_to_Rep <- waiting_t*rep_rate
      
      #############SEIR\u{3f5}\u{373}      
      # Process model for SEIR
      home_t=pmax(home_t+S_to_S0-S0_to_S,0)
      susceptible_t <- pmax(susceptible_t - S_to_E1-S_to_S0+S0_to_S,0)
      
      exposed_t1 <- exposed_t1-outbound_exposed1-E1_to_ISO + S_to_E1-E1_to_E2 +nnCities[,,"exposed1"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])  
      exposed_t2 <- exposed_t2-E2_to_ISO-outbound_exposed2 + E1_to_E2 - E2_to_I1+nnCities[,,"exposed2"]%*%(mobility3[,ii,tt]*dt/theta[,"population"])-E2_to_H
      
      infectious_t1 <- infectious_t1-I1_to_ISO + E2_to_I1 - I1_to_I2
      infectious_t2 <- infectious_t2-I2_to_ISO + I1_to_I2 - I2_to_H
      ##############\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{3f5}\u{373}
      isolate_t=isolate_t+E1_to_ISO+E2_to_ISO+I1_to_ISO+I2_to_ISO-ISO_to_H
      
      #print(head(isolate_t))
      #print("isolate_t")
      #############\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{3f5}\u{373}      
      hospital_t=hospital_t+(I2_to_H)*(1-die_prop)-H_to_recovery1+ISO_to_H*(1-die_prop)
      recovery_t1=recovery_t1+H_to_recovery1-recovery1_to_recovery2
      recovery_t=recovery_t+recovery1_to_recovery2
      to_death_t=to_death_t+I2_to_H*(die_prop)-new_death_t+(ISO_to_H)*die_prop
      death_t=death_t+new_death_t
      ##############\u{fffd}\u{3f1}\u{fffd}\u{3f5}\u{373}      
      #waiting_t <- waiting_t + (I2_to_H)*prob_rep - W_to_Rep
      waiting_t <- waiting_t +I2_to_H+ISO_to_H-W_to_Rep
      #cases_t <- cases_t + (E1_to_E2+ISO_to_Case)*prob_rep
      cases_t <- cases_t + (E1_to_E2+ISO_to_Case)
      reports_t <- reports_t + W_to_Rep
      
      foreign_infect_into_domestic_t=array(0,dim=c(nn,13),dimnames=list(NULL,NULL))
      foreign_infect_into_domestic=array(0,dim=c(nn,1),dimnames=list(NULL,NULL))
      nnCities1=as.matrix(nnCities[,,"exposed1"])
      nnCities2=as.matrix(nnCities[,,"exposed2"])
      nnCities3=as.matrix(nnCities[,,"infect1"])
      nnCities4=as.matrix(nnCities[,,"infect2"])
      
      for (foreign in 33:45){
        dd=foreign-32
        foreign_infect_into_domestic_t[,dd]=(nnCities1[,foreign]+nnCities2[,foreign]+nnCities3[,foreign]+nnCities4[,foreign])*(mobility3[foreign,ii,tt]*dt/theta[foreign,"population"])
        ################\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}      
      }
      for (n in 1:nn){
        foreign_infect_into_domestic[n,1]=sum(foreign_infect_into_domestic_t[n,1:13])
      }
    } 
    ################\u{fffd}??????\u{fffd}\u{fffd}\u{fffd}
    #SEIR\u{3f5}\u{373}      
    if(length(isolate_t)==0){isolate_t=simTab[,"isolate"]}
    #print(head(hospital_t))
    if(length(hospital_t)==0){hospital_t=simTab[,"hospital"]}
    if(length(home_t)==0){hospital_t=simTab[,"home"]}
    if(length(reports_t)==0){reports_t=simTab[,"report"]}
    if(length(cases_t)==0){cases_t=simTab[,"case"]}
    if(length(recovery_t)==0){recovery_t=simTab[,"recovery"]}
    if(length(death_t)==0){death_t=simTab[,"death"]}
    if(length(infectious_t1)==0){infectious_t1=simTab[,"infect1"]}
    if(length(infectious_t2)==0){infectious_t2=simTab[,"infect2"]}
    if(length(exposed_t1)==0){exposed_t1=simTab[,"exposed1"]}
    if(length(exposed_t2)==0){exposed_t2=simTab[,"exposed2"]}
    if(length(recovery_t1)==0){recovery_t1=simTab[,"recovery1"]}
    if(length(recovery_t)==0){recovery_t=simTab[,"recovery"]}
    if(length(exposed_t2)==0){exposed_t2=simTab[,"exposed2"]}
    if(length(waiting_t)==0){waiting_t=simTab[,"wait"]}
    if(length(to_death_t)==0){to_death_t=simTab[,"to_death"]}
    if(length(foreign_infect_into_domestic)==0){foreign_infect_into_domestic=simTab[,"foreign"]}
    
    
    simTab[,"susceptible",citylist[ii]] <- susceptible_t # output
    simTab[,"exposed1",citylist[ii]] <- exposed_t1 # output
    simTab[,"exposed2",citylist[ii]] <- exposed_t2 # output
    simTab[,"infect1",citylist[ii]] <- infectious_t1 # output
    simTab[,"infect2",citylist[ii]] <- infectious_t2 # output
    #\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{fffd}\u{3f5}\u{373}
    simTab[,"hospital",citylist[ii]] <- hospital_t # output
    simTab[,"recovery1",citylist[ii]] <- recovery_t1# output
    simTab[,"recovery",citylist[ii]] <- recovery_t # output
    simTab[,"to_death",citylist[ii]] <- to_death_t # output
    simTab[,"death",citylist[ii]] <- death_t # output
    simTab[,"home",citylist[ii]] <- home_t # output
    #\u{fffd}\u{3f1}\u{fffd}\u{3f5}\u{373}
    simTab[,"wait",citylist[ii]] <- waiting_t # output
    simTab[,"case",citylist[ii]] <- cases_t # output
    simTab[,"report",citylist[ii]] <- reports_t # output
    simTab[,"isolate",citylist[ii]] <- isolate_t # output
    simTab[,"foreign",citylist[ii]] <- foreign_infect_into_domestic # output
    
  }
  
  simTab
}
