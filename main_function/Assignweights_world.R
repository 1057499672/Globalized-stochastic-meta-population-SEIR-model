Assignweights_world=function(confirmation_data,storeL,nn,theta,tt){
  
  
  likehood=matrix(0,nrow=nn,ncol=length(citylist))

  for(ii in 1:length(citylist)){
   
    if(tt<=143&length(filter(filter(confirmation_data,date==datetime[tt]),country==citycode[ii])$total.confirmation)>0){
      confirmation_tt=filter(filter(confirmation_data,date==datetime[tt]),country==citycode[ii])$total.confirmation
    }else if(tt>143){
      confirmation_tt=NaN
    }else if(length(filter(filter(confirmation_data,date==datetime[tt]),country==citycode[ii])$total.confirmation)==0){confirmation_tt=0}
    
    
   
    expected_report_val <- storeL[,tt,"report",ii]
   
    ################################  
    if(!is.na(confirmation_tt)&tt<25&ii==1){
      #*theta[ii,"report_prop"]*confirmation_tt_scale # scale by reporting proportion and known onsets
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt*1),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt>=25&tt<=29&ii==1){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt*1),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt>29&tt<=34&ii==1){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt*1),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt>34&tt<=38&ii==1){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt*1),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt<=70&ii==1){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt>70&ii==1){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/5000),lambda = expected_report_val/5000,log=T)
    }else if(!is.na(confirmation_tt)&tt<25&ii==2){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }
    else if(!is.na(confirmation_tt)&tt>=25&tt<=29&ii==2){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt>29&tt<=34&ii==2){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt>34&tt<=38&ii==2){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt<=70&ii==2){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt>70&ii==2){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/5000),lambda =expected_report_val/5000,log=T)
    } else if(!is.na(confirmation_tt)&tt<25&ii<=32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }
    else if(!is.na(confirmation_tt)&tt>=25&tt<=29&ii<=32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }
    else if(!is.na(confirmation_tt)&tt>29&tt<=34&ii<=32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt>34&tt<=38&ii<=32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt<=70&ii<=32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt>70&ii<=32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/5000),lambda =expected_report_val/5000,log=T)
    }else if(is.na(confirmation_tt)){
      loglikSum_confirmation <- 0
    }else if(!is.na(confirmation_tt)&tt<=45&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt<=50&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt),lambda = expected_report_val,log=T)
    }else if(!is.na(confirmation_tt)&tt<=55&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt<=60&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }
    else if(!is.na(confirmation_tt)&tt<=65&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt<=70&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
      
    }else if(!is.na(confirmation_tt)&tt<=75&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt<104&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }else if(!is.na(confirmation_tt)&tt>=104&ii>32){
      #confirmation_scaled=floor(confirmation_scaled)# -a[1,]*(confirmation_tt-quantile(expected_case_val,a[2,],na.rm=TRUE))
      loglikSum_confirmation <- dpois(floor(confirmation_tt/1000),lambda = expected_report_val/1000,log=T)
    }
    #likehood=likehood+loglikSum_confirmation
    b=which(is.na(loglikSum_confirmation)==TRUE,arr.ind = T)
    #   print(length(b))
    #  print(head(w[,tt,ii]))
    likehood[,ii]=loglikSum_confirmation
    
  }
  a=exp(likehood) 
  for(ii in 1:length(citylist)){
    b=which(is.na(a[,ii])==TRUE,arr.ind = T)
    # print(b)
    if(length(b)>0){
      for(bb in 1:length(b)){a[b[bb],ii]=0}
    }
  }
  return(a)
  #likehood[,ii]=loglikSum_confirmation
  #exp(likehood) 
  
}
