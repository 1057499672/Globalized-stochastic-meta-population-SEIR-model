
yousee210.130=run_fits(40,2000,0.2,300,filename="4",0.2,0)
yousee210.130under_reporting=run_fits(30,2000,0.2,300,filename="4",0.2,1)
#  As simulation is different from random walk fitting, instead of assuming a fixed R0 in a traditional way, we allow for the dynamic fluctuation of R0. Different R0-percentile are manually assigned for each region to make them grow close to original trajectory. 
#  simulation of scenario that western countries adopt Wuhan-level interventions from different time-points
#  western countries never adopt Wuhan-level interventions 
a1=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200507
a2=smc_model_world_simulation(para,theta,2,0.2,300,0,150,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200422
a3=smc_model_world_simulation(para,theta,2,0.2,300,0,135,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200407
a4=smc_model_world_simulation(para,theta,2,0.2,300,0,120,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200328
a5=smc_model_world_simulation(para,theta,2,0.2,300,0,110,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200318
a6=smc_model_world_simulation(para,theta,2,0.2,300,0,100,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200308
a7=smc_model_world_simulation(para,theta,2,0.2,300,0,90,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200227
a8=smc_model_world_simulation(para,theta,2,0.2,300,0,80,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200217
a9=smc_model_world_simulation(para,theta,2,0.2,300,0,70,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)
#western countries adopt Wuhan-level interventions from 20200207
a10=smc_model_world_simulation(para,theta,2,0.2,300,0,60,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,0,90,0,0,0,0)

#scenario simulation that western countries cut the international mobility with China from different time-points
#western countries cut the international mobility with China from 20200323
a12=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,1,105,0,0,0,0)
#western countries cut the international mobility with China from 20200308
a13=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,1,90,0,0,0,0)
#western countries cut the international mobility with China from 20200222
a14=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,1,75,0,0,0,0)
#western countries cut the international mobility with China from 20200207
a15=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,1,60,0,0,0,0)
#western countries cut the international mobility with China from 20200123
a16=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1.08,32,0,1,45,0,0,0,0)

#scenario simulation that China did not take any non-pharmaceutical interventions 
a16China_noaction=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.54,32,1,0,105,1,1,1,0)
#scenario simulation that China did not shut down Wuhan city since 20200123
a16China_noshutdown=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,32,1,0,60,0,0,0,0)

#a16China_noaction_percentile=smc_model_world_simulation(para,theta,100,0.2,300,0.2,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.54,32,1,0,105,1,1,1,0)
#a1_percentile=smc_model_world_simulation(para,theta,100,0.2,300,0.2,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,0,1,32,0,0,90,0,0,0,0)
#a16west_noaction_percentile=smc_model_world_simulation(para,theta,100,0.2,300,0.2,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.54,45,0,0,45,1,1,1,0)

#scenario simulation that China did not take any suspected-cases isolation
a16China_noisolation=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.5,32,0,0,60,1,0,0,0)
#scenario simulation that China did not take any social-distancing
a16China_nosocial_distancing=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.01,32,0,0,45,0,1,1,0)
#scenario simulation that both China and western countries did not take any suspected-cases isolation
a16all_noisolation=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.5,45,0,0,45,1,0,0,0)
#scenario simulation that both China and western countries did not take any social-distancing
a16all_nosocial_distancing=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.01,45,0,0,45,0,1,1,0)

#scenario simulation that western countries did not take any social-distancing                                         
a16west_nosocial_distancing=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.01,45,0,0,45,0,1,1,0)
#scenario simulation that western countries did not take any suspected-cases isolation
a16west_noisolation=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.5,45,0,0,45,1,0,0,0)
#scenario simulation that western countries did not take any non-pharmaceutical interventions 
a16west_noaction=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1.54,45,0,0,45,1,1,1,0)



K=NULL
for (dd in seq(1,1.2,0.01)){
  output=smc_model_world_simulation(para,theta,2,0.2,144,0,143,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,dd,32,1,0,60,0,0,0,1)
  Wuhan_fold=output[1,144,13,1]/confirmation[144,1]
  Hubei_excluding_Wuhan_fold=output[1,144,13,2]/confirmation[144,2]
  K=rbind(K,c(dd,output[1,144,13,1],output[1,144,13,2],Wuhan_fold,Hubei_excluding_Wuhan_fold))
}

for (dd in seq(1,1.2,0.01)){
  output=smc_model_world_simulation(para,theta,2,0.2,144,0,143,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,dd,32,0,0,45,0,1,1,1)
  Wuhan_fold=output[1,144,13,1]/confirmation[144,1]
  Hubei_excluding_Wuhan_fold=output[1,144,13,2]/confirmation[144,2]
  K=rbind(K,c(dd,output[1,144,13,1],output[1,144,13,2],Wuhan_fold,Hubei_excluding_Wuhan_fold))
}
for (dd in seq(1.38,1.8,0.01)){
  output=smc_model_world_simulation(para,theta,2,0.2,144,0,143,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,dd,32,0,0,60,1,0,0,1)
  Wuhan_fold=output[1,144,13,1]/confirmation[144,1]
  Hubei_excluding_Wuhan_fold=output[1,144,13,2]/confirmation[144,2]
  K=rbind(K,c(dd,output[1,144,13,1],output[1,144,13,2],Wuhan_fold,Hubei_excluding_Wuhan_fold))
}
for (dd in seq(1.38,1.8,0.01)){
  output=smc_model_world_simulation(para,theta,2,0.2,144,0,143,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,dd,32,1,0,105,1,1,1,1)
  Wuhan_fold=output[1,144,13,1]/confirmation[144,1]
  Hubei_excluding_Wuhan_fold=output[1,144,13,2]/confirmation[144,2]
  K=rbind(K,c(dd,output[1,144,13,1],output[1,144,13,2],Wuhan_fold,Hubei_excluding_Wuhan_fold))
}

colnames(K)=c("fold_index","Wuhan","Hubei Province excluding Wuhan","Wuhan_fold","Hubei_excluding_Wuhan_fold")
K=as_tibble(K)
write_csv(K,"C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/fold_index1.csv")



a16China_noaction=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,32,1,0,105,1,1,1,0)
a16all_noaction=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,45,1,0,105,1,1,1,0)
a16China_noshutdown=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,32,1,0,60,0,0,0,0)


a16China_noisolation=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,32,0,0,60,1,0,0,0)
a16China_nosocial_distancing=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,32,0,0,45,0,1,1,0)
#a16all_noisolation=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,45,0,0,45,1,0,0,0)
#a16all_nosocial_distancing=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,45,0,0,45,0,1,1,0)

a16west_nosocial_distancing=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,45,0,0,45,0,1,1,0)
a16west_noisolation=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,45,0,0,45,1,0,0,0)
a16west_noaction=smc_model_world_simulation(para,theta,2,0.2,300,0,299,20,0.555,0.79,0.723,0.9315,0.9044,0.928,0.909,0.902,0.916,0.8824,0.824,0.93,0.9282,0.847,0.977,0.681,1,1,45,0,0,45,1,1,1,0)








