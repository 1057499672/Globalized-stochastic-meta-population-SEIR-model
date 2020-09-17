run_fits=function(rep_plot,nn,dt,ttotal,filename="4",wave,under_reporting){
  
  traj_names=c("S_traj","E_traj","I_traj","C_traj","Rep_traj","beta_traj","Rec_traj","Die_traj","ISO_traj","For_traj","sus_traj","track_traj","iso_traj","home_traj","inhome_traj")
  #rep_plot <- 20; cut_off=0; dt=0.25
  metaoutput=array(0,dim=c(ttotal,length(traj_names),length(citylist),rep_plot),dimnames=list(NULL,traj_names,citylist,NULL))
  
  for(kk in 1:rep_plot){
    MM=smc_model_world(para,theta,nn,dt,ttotal,wave,under_reporting)
    metaoutput[,,,kk]=MM[[1]]
  }
  
  S_plot = array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  I_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  II_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  E_plot = array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  #Iso_plot = array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  R0_plot =  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_all_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_all_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_all_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Iso_all_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_all_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  For_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  sus_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  track_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  iso_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  home_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  # Real_R0_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  # Real_sus_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  # Real_home_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  # Real_iso_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  # Real_track_rate_plot=  array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  inhome_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  
  for(kk in 1:rep_plot){
    for (ii in 1:length(citylist)){
      output_smc <- metaoutput[,,,kk]
      E_plot[,kk,ii] <- output_smc[,"E_traj",ii] 
      I_plot[,kk,ii] <- output_smc[,"I_traj",ii]
      II_plot[,kk,ii] <- output_smc[,"E_traj",ii]  + output_smc[,"I_traj",ii]
      S_plot[,kk,ii] <- output_smc[,"S_traj",ii] 
      
      case_all_pos=theta[1,"confirmed_prop"]*(output_smc[,"C_traj",ii])
      C_all_plot[,kk,ii] <- rpois(length(case_all_pos),lambda=case_all_pos)
      
      report_all_pos=theta[1,"confirmed_prop"]*(output_smc[,"Rep_traj",ii])
      Rep_all_plot[,kk,ii] <- rpois(length(report_all_pos),lambda=report_all_pos)
      
      recover_all_pos=theta[1,"confirmed_prop"]*(output_smc[,"Rec_traj",ii])
      Rec_all_plot[,kk,ii] <- rpois(length(recover_all_pos),lambda=recover_all_pos)
      
      death_all_pos=theta[1,"confirmed_prop"]*(output_smc[,"Die_traj",ii])
      Die_all_plot[,kk,ii] <- rpois(length(death_all_pos),lambda=death_all_pos)
      
      isolation_all_pos=theta[1,"confirmed_prop"]*(output_smc[,"ISO_traj",ii])
      Iso_all_plot[,kk,ii] <- rpois(length(isolation_all_pos),lambda=isolation_all_pos)
      
      case_pos <-theta[1,"confirmed_prop"]*(output_smc[,"C_traj",ii] - c(0,head(output_smc[,"C_traj",ii],-1)))
      C_plot[,kk,ii] <- rpois(length(case_pos),lambda=case_pos)
      
      rep_pos <- theta[1,"confirmed_prop"]*(output_smc[,"Rep_traj",ii] - c(0,head(output_smc[,"Rep_traj",ii],-1))) # ALL CASES: theta[["local_rep_prop"]]*
      Rep_plot[,kk,ii] <- rpois(length(rep_pos),lambda=rep_pos)
      
      rec_pos <- theta[1,"confirmed_prop"]*(output_smc[,"Rec_traj",ii] - c(0,head(output_smc[,"Rec_traj",ii],-1)))
      Rec_plot[,kk,ii] <- rpois(length(rec_pos),lambda=rec_pos)
      
      die_pos <- theta[1,"confirmed_prop"]*(output_smc[,"Die_traj",ii] - c(0,head(output_smc[,"Die_traj",ii],-1)))
      Die_plot[,kk,ii] <- rpois(length(die_pos),lambda=die_pos)
      
      #  iso_pos <- theta[1,"confirmed_prop"]*(output_smc[,"ISO_traj",ii])   #- c(0,head(output_smc[,"ISO_traj",ii],-1)))
      # print(iso_pos)
      #   iso_pos=pmax(iso_pos,0)
      #  for (ii in 1:length(iso_pos)){
      #    if(is.na(iso_pos[ii])){iso_pos[ii]=1}
      #  }
      #  print(iso_pos)
      #  Iso_plot[,kk,ii] <- rpois(length(iso_pos),lambda=iso_pos)
      
      scale_incubation=(0.5+0.5*theta[ii,"pre_symp"])*theta[ii,"incubation"]
      R0_plot[,kk,ii] <- output_smc[,"beta_traj",ii]*(theta[ii,"isolation"]+scale_incubation)
      
      For_pos=  output_smc[,"For_traj",ii]
      For_plot[,kk,ii]=rpois(length(For_pos),lambda=For_pos)
      
      inhome_pos=  output_smc[,"inhome_traj",ii]
      inhome_plot[,kk,ii]=rpois(length(inhome_pos),lambda=inhome_pos)
      
      #  for(tt in 1:ttotal){
      #    if(tt>=119){tt=119}else{tt=tt}
      #   sus_rate_plot[,kk,ii]= output_smc[,"sus_traj",ii]*(1-susceptibility[ii,tt])
      # }
      sus_rate_plot[,kk,ii]= output_smc[,"sus_traj",ii]
      track_rate_plot[,kk,ii]=  output_smc[,"track_traj",ii]
      iso_rate_plot[,kk,ii]= output_smc[,"iso_traj",ii]
      home_rate_plot[,kk,ii]= output_smc[,"home_traj",ii]
      
      #  Real_R0_plot[,kk,ii]=output_smc[,"Real_R0_traj",ii]
      #  Real_sus_rate_plot[,kk,ii]=output_smc[,"Real_sus_rate_traj",ii]
      #  Real_track_rate_plot[,kk,ii]=output_smc[,"Real_track_rate_traj",ii]
      #  Real_iso_rate_plot[,kk,ii]= output_smc[,"Real_iso_rate_traj",ii]
      #  Real_home_rate_plot[,kk,ii]=output_smc[,"Real_home_rate_traj",ii]
      
      
      #Rep_plot[,kk] <- theta[["confirmed_prop"]]*fit_int_cases(output_smc$Rep_trace - c(0,head(output_smc$Rep_trace,-1))) # case difference
      #  save(
      #   S_plot,
      #   I_plot,
      #   II_plot,
      #   E_plot,
      #   Rec_plot,
      #   Die_plot,
      #   C_plot,
      #   Rep_plot,
      #   R0_plot,
      #   Rec_all_plot,
      #   Die_all_plot,
      #   C_all_plot,
      #   Iso_all_plot,
      #   Rep_all_plot,
      #   For_plot,
      #   sus_rate_plot,
      #   track_rate_plot,
      #   iso_rate_plot,
      #   home_rate_plot,
      #   inhome_plot,
      #   Real_R0_plot,
      #   Real_sus_rate_plot,
      #   Real_home_rate_plot,
      #   Real_iso_rate_plot,
      #   Real_track_rate_plot,
      #  file=paste0("C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/metacity",filename,".RData")) 
    }
  }
  
  
  return(list( S_plot,
               I_plot,
               II_plot,
               E_plot,
               Rec_plot,
               Die_plot,
               C_plot,
               Rep_plot,
               R0_plot,
               Rec_all_plot,
               Die_all_plot,
               C_all_plot,
               Iso_all_plot,
               Rep_all_plot,
               For_plot,
               sus_rate_plot,
               track_rate_plot,
               iso_rate_plot,
               home_rate_plot,
               inhome_plot
               #      Real_R0_plot,
               #      Real_sus_rate_plot,
               #      Real_home_rate_plot,
               #      Real_iso_rate_plot,
               #      Real_track_rate_plot
  ))
}
