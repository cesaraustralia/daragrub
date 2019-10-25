Native_Budworm<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Native Budworm'
  sciname<-'Helicoverpa punctigera'
  source <- 'Kobelt, A., 1988. Estimation of preliminary economic thresholds for Helicoverpa punctigera (Lepidoptera: Noctuidae) on field pea, using development and consumption rates. Honours (B.Sc) thesis, La Trobe University.'
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 80.3               
    TUPPER = 35.0
    TLOWER =  8.0
    TMIN   =  9.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 46.3               
    TUPPER = 35.0
    TLOWER = 12.0
    TMIN   = 13.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 46.3               
    TUPPER = 35.0
    TLOWER = 12.0
    TMIN   = 13.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  =  37.1                   
    TUPPER = 35.0
    TLOWER = 12.0
    TMIN   = 13.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  =  17.3                   
    TUPPER = 35.0
    TLOWER = 12.0
    TMIN   = 13.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L5=function(temp){
    DDREQ  =  28.2                   
    TUPPER = 35.0
    TLOWER = 12.0
    TMIN   = 13.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L6=function(temp){
    DDREQ  = 46.3                   
    TUPPER = 35.0
    TLOWER = 12.0
    TMIN   = 13.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  pupa=function(temp){
    DDREQ  =249.0                   
    TUPPER = 35.0
    TLOWER =  8.0
    TMIN   =  9.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  adult=function(temp){
    DDREQ  = 26.0                   
    TUPPER = 35.0
    TLOWER =  8.0
    TMIN   =  9.0
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