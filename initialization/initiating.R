  theta=read.table("E:/Globalized-stochastic-meta-population-SEIR-model/data/theta.csv",header=T,sep=",")
  para=read.table("E:/Globalized-stochastic-meta-population-SEIR-model/data/para.csv",header=T,sep=",")
  mobility_data=read.table("E:/covid-19/data/global_mobility.csv",header=T,sep=",")
  #mobility_data records the human mobility movement between 32 regions in mainland China, between 13 western countries, and betweeen regions in China and regions in western_countries.
  
   
  mobility_data[,2]=as.numeric(mobility_data[,2]) #transform to numeric type
  mobility_data[,3]=as.numeric(mobility_data[,3]) #transform to numeric type
  citylist1=c("wuhan","dewuhan","bj11","tj12","hb13","sx14","nmg15","ln21","jl22","hlj23","sh31","js32","zj33","ah34","fj35","jx36","sd37","hn41","hn43")
  citylist2=c("gd44","gx45","hn46","cq50","sc51","gz52","yn53","xz54","sx61","gs62","qh63","nx64","xj65","switzerland","sweden","austria","france","britain","germany")
  citylist3=c("spain","italy","norway","holand","belgium","danmark","UN")
  citylist=c(citylist1,citylist2,citylist3) 
  #citylist of 45 regions, including 32 Chinese regions (Wuhan and 31 provinces in mainland China) and 13 western countries)
  
  citycode=c("420100","770000","110000","120000","130000","140000","150000","210000","220000",
             "230000","310000","320000","330000","340000","350000","360000","370000","410000",
             "430000","440000","450000","460000","500000","510000","520000","530000","540000",
             "610000","620000","630000","640000","650000","901","902","903","904",
             "905","906","907","908","909","910","911","912","990000")
  # each region has a citycode, which is used as an index to pick up data in Excel
  
  datetime=c("20191210", "20191211", "20191212", "20191213", "20191214", "20191215", "20191216", "20191217", "20191218",
             "20191219", "20191220", "20191221", "20191222", "20191223", "20191224", "20191225", "20191226", "20191227",
             "20191228" ,"20191229", "20191230", "20191231", "20200101", "20200102", "20200103", "20200104", "20200105",
             "20200106", "20200107", "20200108", "20200109", "20200110", "20200111", "20200112", "20200113", "20200114",
             "20200115", "20200116", "20200117", "20200118", "20200119", "20200120", "20200121", "20200122", "20200123",
             "20200124", "20200125", "20200126", "20200127", "20200128", "20200129", "20200130", "20200131", "20200201",
             "20200202", "20200203", "20200204", "20200205", "20200206", "20200207", "20200208", "20200209", "20200210",
             "20200211", "20200212", "20200213", "20200214", "20200215", "20200216", "20200217", "20200218", "20200219",
             "20200220", "20200221", "20200222", "20200223", "20200224", "20200225", "20200226", "20200227", "20200228",
             "20200229", "20200301", "20200302", "20200303", "20200304", "20200305", "20200306", "20200307", "20200308",
             "20200309", "20200310", "20200311", "20200312", "20200313", "20200314", "20200315", "20200316", "20200317",
             "20200318", "20200319", "20200320", "20200321", "20200322", "20200323", "20200324", "20200325", "20200326",
             "20200327","20200328","20200329","20200330","20200331","20200401","20200402","20200403","20200404","20200405",
             "20200406", "20200407", "20200408", "20200409", "20200410", "20200411", "20200412", "20200413", "20200414", 
             "20200415", "20200416","20200417","20200418","20200419","20200420","20200421","20200422","20200423","20200424","20200425",
             "20200426","20200427","20200428","20200429","20200430","20200501")
  #datetime list is the total period during which total confirmations reported data of each region will be combined together to estimate and predict the R0-specific time-variant parameters before and after 20200501. 
  
  mobility4=array(0,dim=c(length(citylist),length(citylist),length(datetime)),dimnames=list(citylist,citylist,datetime))
  foreign_contact_chinesecity=c(3,20,15,11,12,17,24,8,33,34,35,36,37,38,39,40,41,42,43,44,45)
  
  
  
  S_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  I_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  II_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  E_plotarray(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  R0_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rec_all_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Die_all_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  C_all_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Iso_all_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  Rep_all_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  For_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  sus_rate_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  track_rate_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  iso_rate_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  home_rate_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  inhome_plot=array(0,dim=c(ttotal,rep_plot,length(citylist)),dimnames=list(NULL,NULL,citylist))
  
  S_plot3=yousee210.1[[1]]
  I_plot3=yousee210.1[[2]]
  II_plot3=yousee210.1[[3]]
  E_plot3=yousee210.1[[4]]
  Rec_plot3=yousee210.1[[5]]
  Die_plot3=yousee210.1[[6]]
  C_plot3=yousee210.1[[7]]
  Rep_plot3=yousee210.1[[8]]
  R0_plot3=yousee210.1[[9]]
  Rec_all_plot3=yousee210.1[[10]]
  Die_all_plot3=yousee210.1[[11]]
  C_all_plot3=yousee210.1[[12]]
  Iso_all_plot3=yousee210.1[[13]]
  Rep_all_plot3=yousee210.1[[14]]
  For_plot3=yousee210.1[[15]]
  sus_rate_plot3=yousee210.1[[16]]
  track_rate_plot3=yousee210.1[[17]]
  iso_rate_plot3=yousee210.1[[18]]
  home_rate_plot3=yousee210.1[[19]]
  inhome_plot3=yousee210.1[[20]]
  
  S_plot1=yousee210.130[[1]]
  I_plot1=yousee210.130[[2]]
  II_plot1=yousee210.130[[3]]
  E_plot1=yousee210.130[[4]]
  Rec_plot1=yousee210.130[[5]]
  Die_plot1=yousee210.130[[6]]
  C_plot1=yousee210.130[[7]]
  Rep_plot1=yousee210.130[[8]]
  R0_plot1=yousee210.130[[9]]
  Rec_all_plot1=yousee210.130[[10]]
  Die_all_plot1=yousee210.130[[11]]
  C_all_plot1=yousee210.130[[12]]
  Iso_all_plot1=yousee210.130[[13]]
  Rep_all_plot1=yousee210.130[[14]]
  For_plot1=yousee210.130[[15]]
  sus_rate_plot1=yousee210.130[[16]]
  track_rate_plot1=yousee210.130[[17]]
  iso_rate_plot1=yousee210.130[[18]]
  home_rate_plot1=yousee210.130[[19]]
  inhome_plot1=yousee210.130[[20]]
  
  S_plot2=yousee210.130under_reporting[[1]]
  I_plot2=yousee210.130under_reporting[[2]]
  II_plot2=yousee210.130under_reporting[[3]]
  E_plot2=yousee210.130under_reporting[[4]]
  Rec_plot2=yousee210.130under_reporting[[5]]
  Die_plot2=yousee210.130under_reporting[[6]]
  C_plot2=yousee210.130under_reporting[[7]]
  Rep_plot2=yousee210.130under_reporting[[8]]
  R0_plot2=yousee210.130under_reporting[[9]]
  Rec_all_plot2=yousee210.130under_reporting[[10]]
  Die_all_plot2=yousee210.130under_reporting[[11]]
  C_all_plot2=yousee210.130under_reporting[[12]]
  Iso_all_plot2=yousee210.130under_reporting[[13]]
  Rep_all_plot2=yousee210.130under_reporting[[14]]
  For_plot2=yousee210.130under_reporting[[15]]
  sus_rate_plot2=yousee210.130under_reporting[[16]]
  track_rate_plot2=yousee210.130under_reporting[[17]]
  iso_rate_plot2=yousee210.130under_reporting[[18]]
  home_rate_plot2=yousee210.130under_reporting[[19]]
  inhome_plot2=yousee210.130under_reporting[[20]]
  
  
  for(ii in 1:length(citylist)){
    S_plot[,,ii]=cbind(S_plot1[,,ii],S_plot3[,,ii])
    I_plot[,,ii]=cbind(I_plot1[,,ii],I_plot3[,,ii])
    II_plot[,,ii]=cbind(II_plot1[,,ii],II_plot3[,,ii])
    
    Rec_plot[,,ii]=cbind(Rec_plot1[,,ii],Rec_plot3[,,ii])
    Die_plot[,,ii]=cbind(Die_plot1[,,ii],Die_plot3[,,ii])
    C_plot[,,ii]=cbind(C_plot1[,,ii],C_plot3[,,ii])
    Rep_plot[,,ii]=cbind(Rep_plot1[,,ii],Rep_plot3[,,ii])
    R0_plot[,,ii]=cbind(R0_plot1[,,ii],R0_plot3[,,ii])
    Rec_all_plot[,,ii]=cbind(Rec_all_plot1[,,ii],Rec_all_plot3[,,ii])
    Die_all_plot[,,ii]=cbind(Die_all_plot1[,,ii],Die_all_plot3[,,ii])
    C_all_plot[,,ii]=cbind(C_all_plot1[,,ii],C_all_plot3[,,ii])
    Iso_all_plot[,,ii]=cbind(Iso_all_plot1[,,ii],Iso_all_plot3[,,ii])
    Rep_all_plot[,,ii]=cbind(Rep_all_plot1[,,ii],Rep_all_plot3[,,ii])
    For_plot[,,ii]=cbind(For_plot1[,,ii],For_plot3[,,ii])
    sus_rate_plot[,,ii]=cbind(sus_rate_plot1[,,ii],sus_rate_plot3[,,ii])
    track_rate_plot[,,ii]=cbind(track_rate_plot1[,,ii],track_rate_plot3[,,ii])
    iso_rate_plot[,,ii]=cbind(iso_rate_plot1[,,ii],iso_rate_plot3[,,ii])
    home_rate_plot[,,ii]=cbind(home_rate_plot1[,,ii],home_rate_plot3[,,ii])
    inhome_plot[,,ii]=cbind(inhome_plot1[,,ii], inhome_plot3[,,ii])
  }
  
  

