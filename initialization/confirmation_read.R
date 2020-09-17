confirmation_read=function(){
  confirmation_data=read.table("E:/covid-19/data/confirmation_data.csv",header=T,sep=",")
  confirmation_data[,1]=as.numeric(confirmation_data[,1])
  confirmation_data[,2]=as.numeric(confirmation_data[,2])
  confirmation_data[,3]=as.numeric(confirmation_data[,3])
  confirmation_data[,4]=as.numeric(confirmation_data[,4])
  confirmation_data[,5]=as.numeric(confirmation_data[,5])
}

confirmation_data=read.table("C:/Users/admin/Videos/stoch/2020-ncov-master/2020-ncov-master/stoch_model_V2_paper/world confirmation data5.csv",header=T,sep=",") 
confirmation=array(0,dim=c(length(datetime),length(citylist)),dimnames=list(datetime,citylist))
for (ii in 1:length(citylist)){
  for(tt in 1: length(datetime)){
    print(ii)
    print(tt)
    
    if(length(filter(filter(confirmation_data,date==datetime[tt]),country==citycode[ii])$total.confirmation)==0){
      confirmation[tt,ii]=0}
    else{
      confirmation[tt,ii]=filter(filter(confirmation_data,date==datetime[tt]),country==citycode[ii])$total.confirmation
    }
  }
}
