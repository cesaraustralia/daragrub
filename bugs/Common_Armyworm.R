Common_Armyworm<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Common Armyworm'
  sciname<-'Mythimna convecta'
  source <- 'McDonald, G., Bryceson, K., & Farrow, R. 1990. The Development of the 1983 Outbreak of the Common Armyworm, Mythimna convecta, in Eastern Australia. Journal of Applied Ecology, 27(3), 1001-1019. doi:10.2307/2404392'
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 94.5                   
    TUPPER = 33.2
    TLOWER =  6.1
    TMIN   =  7.3
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 48.4               
    TUPPER = 33.7
    TLOWER =  6.7
    TMIN   = 8.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 43.2               
    TUPPER = 32.8
    TLOWER = 6.4
    TMIN   = 7.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  =  44.6                   
    TUPPER =  34.3
    TLOWER = 6.8
    TMIN   = 8.5
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  =  47.0                   
    TUPPER = 32.8
    TLOWER = 6.0
    TMIN   = 7.5
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L5=function(temp){
    DDREQ  =  123.9                   
    TUPPER = 33.3
    TLOWER = 6.1
    TMIN   = 9.5
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L6=function(temp){
    DDREQ  = 126.4                   
    TUPPER = 33.9
    TLOWER = 6.0
    TMIN   = 7.2
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  pupa=function(temp){
    DDREQ  =264.1                   
    TUPPER = 34.0
    TLOWER =  5.9
    TMIN   =  7.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  adult=function(temp){
    DDREQ  = 26.8                   
    TUPPER = 34.0
    TLOWER =  6.0
    TMIN   =  8.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,L6=L6,
                 pupa=pupa,adult=adult)
  
  life<-c('egg','immature','immature','immature','immature','immature','immature','pupa','adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}