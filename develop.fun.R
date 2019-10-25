develop<-function(Tmax, Tmin, startDay, startStage, insect){
  # arguments:
  # Tmax: a raster stack or list of daily max temperatures for 365 days [365][D]
  # Tmin: a raster stack or list of daily min temperatures for 365 days [365][D]
  # startStage: a vector specifying the stage of development at each deme [D]
  # startDay: a scalar specifying the day developmnetal stages were observed [1]
  
  # output
  # a list containing:
  # 'name': the name of the bug
  # 'dev.funs': temperature dependent temperature function for each stage
  # 'life': a life-history category (egg,immature,pupa,adult) for each stage

  # some checks
  if(length(startDay)!=1)stop('startDay must have length 1')
  if(length(Tmax[[1]])!=length(startStage))stop('number of locations must match length of startStage')
  if(length(names(Tmax))!=length(names(Tmin)))stop('size of climate data Tmin and Tmax must match')
  
  # some useful variables
  D <- length(Tmax[[1]])
  S <- length(insect$dev.funs)
  
  # hourly temp as a function of Tmin, Tmax, time (of day),d
  hrtemp<-function(TMIN,TMAX,hr){
    # Tmax: daily maximum temp, C
    # Tmin: daily minimum temp, C
    # hr: hour of day, h
    (TMAX-TMIN)/2*sin(2*pi*(hr - 8)/24 )  + (TMAX +TMIN)/2
  }
  
  hr.dev <- function(yearHour, TMIN, TMAX, curStage, dev.funs){
    # calculate hourly development accross all demes
    hr<-yearHour%%24
    if(length(hr)>1)stop('more than one hour supplied')
    if(all(c(length(TMIN), length(TMAX)) != length(curStage)) )stop('size of TMAX, TMIN, curStage must match')
    S <- length(dev.funs) # stages
    D <- length(TMIN)
    
    # get temp for given hr accross all demes
    temp<-hrtemp(TMIN,TMAX,hr)
    # intialise dev matrix 
    dev<-rep(0,D)
    # iterate through stages
    for(s in 1:S){
      # stage specific function for temp dependence of dev. rate
      dev[curStage==s]<-dev.funs[[s]](temp[curStage==s])/24 # increment dev during hr for stage
    }
    return(dev) # return hourly development
  }
  
  curStage<-startStage
  curDev  <-rep(0, D)
  # create blank data frame for output data
  myCols<-c('Time_start', 'Time_end', 'Stage_duration')
  data<-array(NA,c(D,S, length(myCols)),
              dimnames=list(NULL, paste0('stage',1:S), 
                            myCols)) # 
  
  # set start and end times for intial conditions
  for(i in 1:D){
    data[i,curStage[i],  'Time_start'] <- (startDay-1)*24
    if(curStage[i]>1) data[i,curStage[i]-1,'Time_end'] <- (startDay-1)*24
  }
  
  # forward simulation
  h = ((startDay-1)*24)
  day = startDay # intialise day
  while(day<730&any(curStage!=0)){
    TMIN<-Tmin[[ifelse(day%%365==0,365,day%%365)]][] # index cannot be 0
    TMAX<-Tmax[[ifelse(day%%365==0,365,day%%365)]][]
    for (dayhour in 0:23){
      h<-(day-1)*24+dayhour
      curDev<-curDev+hr.dev(h, TMIN, TMAX, curStage, insect$dev.funs) # increment dev
      curDev[is.na(curDev)]<-0 # na's to 0
      curStage[curDev>1]<-curStage[curDev>1] + 1 # curStage
      D_st <- which(curDev>1) # demes with stage transitions
      S_st <- curStage[curDev>1] # new stages 
      curDev[curDev>1]<-0 # if new stage, set dev to zero
      curStage[curStage==(S+1)]<-0 # if dead, set to zero
      if(length(S_st)>0){
        for(i in 1:length(S_st)){
          if(S_st[i]<(S+1)){# if stage transition is not mortality
            data[D_st[i],S_st[i],  'Time_start'] <- h 
            data[D_st[i],S_st[i]-1,'Time_end']   <- h 
          }else if(S_st[i]==(S+1)){
            data[D_st[i],S_st[i]-1,'Time_end']   <- h # mortality
          }
        }
      }
    }
    if(D>1&&day>364) break
    day = day+1 # increment day
  }
  
  curStage<-startStage
  curDev  <-rep(0, D)
  # backward simulation
  day = startDay-1 # initialise
  while(day>-730&any(curStage!=0)){
    TMIN<-Tmin[[ifelse(day%%365==0,365,day%%365)]][] # index cannot be 0
    TMAX<-Tmax[[ifelse(day%%365==0,365,day%%365)]][]
    for (dayhour in 23:0){
      h<-(day-1)*24+dayhour
      curDev<-curDev-hr.dev(h, TMIN, TMAX, curStage, insect$dev.funs) # decrement dev
      curDev[is.na(curDev)]<-0 # na's to 0
      curStage[curDev<0]<-curStage[curDev<0] - 1 # curStage
      D_st <- which(curDev<0) # demes with stage transitions
      S_st <- curStage[curDev<0] # new stages
      curDev[curDev<0]<-1 # if new stage, set dev to zero
      if(length(S_st)>0){
        for(i in 1:length(S_st)){
          data[D_st[i],S_st[i]+1  ,'Time_start'] <- h
          data[D_st[i],S_st[i],'Time_end']   <- h
        }
      }
    }
    print(day)
    if(D>1&&day<1) break
    day = day-1 # decrement day
  }
  data[,,'Stage_duration']<-(data[,,'Time_end']-data[,,'Time_start'])/24 # convert hours to days
  data[,,'Time_start'] <- data[,,'Time_start']/24 # convert hours to days
  data[,,'Time_end'] <- data[,,'Time_end']/24 # convert hours to days
  return(data)
}