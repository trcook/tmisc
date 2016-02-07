#' convertdta
#' @description
#' converts files in directory from dta to rda and saves copies. 
#'@export convertdta
convertdta<-function(dir="./"){
	require(foreign)
	to_proc<-dir(dir,pattern="*.dta")
	to_proc<-data.frame(filename=to_proc,filepath=file.path(dir,to_proc,fsep=''),objname=gsub(to_proc,pattern='\\.dta|-|\\s',replacement=''))
	to_proc$newpath<-gsub(x=to_proc$filepath,pattern='\\.dta',rep='.rda')
#	print(gsub(x=to_proc$filepath,pattern='\\.dta',rep='.rda'))
	print(to_proc)
	apply(to_proc,1,function(x){
		eval(substitute({
			obj<-read.dta(filepath)
			save(obj,file=newpath)
			},
			env=list(obj=as.name(x['objname']),filepath=x['filepath'],newpath=x['newpath'])))
		})
}


#' convertcsv
#' @description
#' converts files in directory from dta to rda and saves copies. 
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
#' @note 
#' You should/must put a trailing slash on the end of the path you are searching. 
#'@export convertxls

convertxls<-function(mydir="./"){
  # to_proc<-dir(mydir,pattern="*.xls|*.xlsx",recursive=T,all.files = FALSE)
  
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
    objname<-gsub(x=i,pattern='\\.xlsx|\\.xls|-|\\s|[^a-zA-Z0-9]',replacement='')
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
      outname<-paste(names(workbooks)[[w]],i,'.RDS',sep='')
      print(outname)
      saveRDS(x[i],file = file.path(mydir,outname))
    }}else{
      outname<-paste(x,'.RDS',sep='')
      saveRDS(x,file=file.path(mydir,outname))
    }})
    return()}
    
    
