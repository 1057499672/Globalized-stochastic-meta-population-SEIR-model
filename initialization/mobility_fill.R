mobility_fill=function(){
  #Mobility data from 20191210 to 20200326, as total 108 days' mobility data are acquired.
  ######################
  for (day in 23:108){
    for (city1 in 1:length(citylist)){
      for (city2 in 1:length(citylist)){
        if (city1==city2&city1<=32){
          mobility4[city2,city1,day]=0
        }else if(city2==2&city1>=3&city1<=32){
          K=filter(filter(filter(data,origin==as.numeric(citycode[city1])),destination==as.numeric(citycode[city2])),date==datetime[day])$people
          print(K)
          print(city1)
          print(city2)
          mobility4[city2,city1,day]=K
        }else if(city1<=32&city2<=32&nrow(filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])),date==datetime[day]))==1){
          K=filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])),date==datetime[day])$people
          print(K)
          print(city1)
          print(city2)
          mobility4[city2,city1,day]=K
        }else if(city1>=33&!is.na(match(city2,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])))$people
        }else if(city2>=33&!is.na(match(city1,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])))$people
        }else if(city1>=33&is.na(match(city2,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=0
        }else if(city2>=33&is.na(match(city1,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=0
        }else if(city1<=32&city2<=32&nrow(filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])),date==datetime[day]))==0){
          mobility4[city2,city1,day]=0
        }
      }
    }
  }
  
  #Mobility data from 20200327 to 20200501, in which mobility between Chinese regions are assumed to be the same with 20200326
  for (day in 109:143){
    for (city1 in 1:length(citylist)){
      for (city2 in 1:length(citylist)){
        if (city1==city2&city1<=32){
          mobility4[city2,city1,day]=0
        }else if(city2==2&city1>=3&city1<=32){
          K=filter(filter(filter(data,origin==as.numeric(citycode[city1])),destination==as.numeric(citycode[city2])),date==datetime[108])$people
          print(K)
          print(city1)
          print(city2)
          mobility4[city2,city1,day]=K
        }else if(city1<=32&city2<=32&nrow(filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])),date==datetime[108]))==1){
          K=filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])),date==datetime[108])$people
          print(K)
          print(city1)
          print(city2)
          mobility4[city2,city1,day]=K
        }else if(city1>=33&!is.na(match(city2,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])))$people
        }else if(city2>=33&!is.na(match(city1,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])))$people
        }else if(city1>=33&is.na(match(city2,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=0
        }else if(city2>=33&is.na(match(city1,foreign_contact_chinesecity))){
          mobility4[city2,city1,day]=0
        }else if(city1<=32&city2<=32&nrow(filter(filter(filter(data,origin==as.numeric(citycode[city2])),destination==as.numeric(citycode[city1])),date==datetime[23]))==0){
          mobility4[city2,city1,day]=0
        }
      }
    }
  }
}
