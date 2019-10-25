getBug<-function(bug){
  # returns temperature development function for specified bug
  if(!paste0(bug,'.R')%in%list.files('./bugs')){
    stop('bug temperature data not found in bugs folder')  
  }
  source(paste0('./bugs/',bug,'.R'))
  eval(parse(text=paste0('return(',bug,'())')))
}
  