#' convertdta
#' @description
#' converts files in directory from dta to rda and saves copies. 
#' Among other things, this is a useful function to use if you need 
#' or want to save some harddrive space.
#'@export convertdta
convertdta<-function(dir="./"){
	require(foreign)
  save_method<-switch(save_format,'rda'=save,'rds'=saveRDS)
  ext=paste0('.',save_format,sep='')
	to_proc<-dir(dir,pattern="*.dta")
	to_proc<-data.frame(filename=to_proc,filepath=file.path(dir,to_proc,fsep=''),objname=gsub(to_proc,pattern='\\.dta|-|\\s',replacement=''))
	to_proc$newpath<-gsub(x=to_proc$filepath,pattern='\\.dta',rep=ext)
#	print(gsub(x=to_proc$filepath,pattern='\\.dta',rep='.rda'))
	print(to_proc)
	apply(to_proc,1,function(x){
		eval(substitute({
			obj<-read.dta(filepath)
			save_method(obj,file=newpath)
			},
			env=list(obj=as.name(x['objname']),filepath=x['filepath'],newpath=x['newpath'])))
		})
}


#' convertcsv
#' @description
#' converts files in directory from dta to rda and saves copies.
#' Among other things, this is a useful function to use if you need #' or want to save some harddrive space. 
#'@export convertcsv
convertcsv<-function(dir="./",save_format='rda'){
	require(foreign)
	require(data.table)
  save_method<-switch(save_format,'rda'=save,'rds'=saveRDS)
  ext=paste0('.',save_format,sep='')
	to_proc<-dir(dir,pattern="*.csv")
	to_proc<-data.frame(filename=to_proc,filepath=file.path(dir,to_proc,fsep=''),objname=gsub(to_proc,pattern="\\.csv|-|\\s",replacement=''))
	to_proc$newpath<-gsub(x=to_proc$filepath,pattern='\\.csv',rep=ext)
#	print(gsub(x=to_proc$filepath,pattern='\\.dta',rep='.rda'))
	print(to_proc)
	apply(to_proc,1,function(x){
		eval(substitute({
			obj<-data.table(read.csv(filepath))
			save_method(obj,file=newpath)
			},
			env=list(obj=as.name(x['objname']),filepath=x['filepath'],newpath=x['newpath'])))
		})
}


#' convertxls
#' @description
#' converts files in directory from xls to rda and saves copies. 
#' Among other things, this is a useful function to use if you need #' or want to save some harddrive space.
#' @note 
#' You should/must put a trailing slash on the end of the path you are searching. 
#'@export convertxls

convertxls<-function(mydir="./",save_format='rda'){
  # to_proc<-dir(mydir,pattern="*.xls|*.xlsx",recursive=T,all.files = FALSE)
  save_method<-switch(save_format,'rda'=save,'rds'=saveRDS)
  ext=paste0('.',save_format,sep='')
  # Step 1: get xls files in directory
  to_proc<-dir(
  mydir,pattern="*.xls|*.xlsx",recursive=T,
  all.files = FALSE)
  to_proc<-to_proc[!grepl(to_proc,pattern='~\\$')]  
  print(to_proc)
  
  # step 2 load in workbooks
  workbooks<-list()
  for(i in to_proc){
    ifile<-i
    objname<-gsub(x=i,pattern='\\.xlsx|\\.xls',replacement='')
    objname<-gsub(x=i,pattern='-|\\s|[^a-zA-Z0-9]',replacement='_')
    
    workbooks[[objname]]<-XLConnect::loadWorkbook(filename = file.path(mydir,ifile,fsep=''))
  }
  
  #step 3: get sheets from each object and save
  procsheets<-lapply(seq_along(workbooks),function(w){
    x<-workbooks[[w]]
    sht<- XLConnect::getSheets(x)
    wbk<-
    print(sht)
    print(class(sht))
    print(length(sht))
    print(class(x))
    if(length(sht)>1){
      print("multiple")
    for(i in sht){
            print("forbreak")
      outname<-paste(names(workbooks)[[w]],i,ext,sep='')
      print(outname)
      save_method(x[i],file = file.path(mydir,outname))
    }}else{
      outname<-paste(x,'.RDS',sep='')
      save_method(x,file=file.path(mydir,outname))
    }})
    return()}
    
    
