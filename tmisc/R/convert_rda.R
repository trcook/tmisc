#' convertdta
#' @description
#' converts files in directory from dta to rda and saves copies. 
#'@export convertdta
convertdta<-function(dir="./"){
	require(foreign)
	to_proc<-dir(dir,pattern="*.dta")
	to_proc<-data.frame(filename=to_proc,filepath=file.path(dir,to_proc,fsep=''),objname=gsub(to_proc,pattern='\\.dta',replacement=''))
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
convertcsv<-function(dir="./"){
	require(foreign)
	require(data.table)
	to_proc<-dir(dir,pattern="*.csv")
	to_proc<-data.frame(filename=to_proc,filepath=file.path(dir,to_proc,fsep=''),objname=gsub(to_proc,pattern='\\.csv',replacement=''))
	to_proc$newpath<-gsub(x=to_proc$filepath,pattern='\\.csv',rep='.rda')
#	print(gsub(x=to_proc$filepath,pattern='\\.dta',rep='.rda'))
	print(to_proc)
	apply(to_proc,1,function(x){
		eval(substitute({
			obj<-data.table(read.csv(filepath))
			save(obj,file=newpath)
			},
			env=list(obj=as.name(x['objname']),filepath=x['filepath'],newpath=x['newpath'])))
		})
}

