#' average predicted qi simulation
#' 
#' @description
#' This generates the average predicted effect for quantities of interest using simulations. This is useful for examining the effect of a quantity of interest (iv) in an interaction while still remaining sensitive to the dependence of the interaction effect \eqn{\partial^2 E[y|XB]/\partial x_1 \partial x_2}.
#' @usage
#' To use, input a model, the number of simulations you'd like to run, and the features (i.e. variables ) you'd like to hold constant. 
#' @examples
#' \dontrun{
#' x<-runif(500,0,1)
#' z<-runif(500,0,1)
#' y<-rbinom(500,1,.1*x+.1*z+.5*x*z)
#' mod1<-glm(y~x*z)
#' qi<-avg_pred_qi(mod1,500,x=c(0,1))
#' qi<-avg_pred_qi(mod1,500,x=seq(0,1,.2),z=1)
#' }
#' NULL
#' @export

avg_pred_qi<-function(model=poast_model,n=10,...){
  require(MASS)
betas<-mvrnorm(n,coef(model),vcov(model))
adjust<-expand.grid(list(...))
adjustnext<-adjustinc(adjust)

base.frame<-model.frame(model)

# error check
if(length(setdiff(names(expand.grid(list(...))),names(base.frame)))>0){stop("adjusted variables not in model")}

# the loop needs to have 3 big steps:
# 1. it needs to pull the next row from adjust
# 2. it needs to generate the adjusted model.matrix via setx.frame
# 3. it needs to estimate via qi1
outlist<-c()
m<-adjustnext()
while(!'adjustincend'%in%m){
setx.frame<-setxgen(model = model,m = m,base.frame = base.frame)
outi<-cbind(m,qi1(beta = betas,setxframe = setx.frame,num = n))
outlist<-rbind(outlist,outi)
m<-adjustnext()
}
return(outlist)
}

#' adjustinc
#' this closure gives individual rows of an adjusted grid one at a time, each time it is called.
#' @examples
#' adjustnext<-adjustinc(expand.grid(b=c(0:5)))
#' adjustnext()

adjustinc<-function(adjust){
  limiter<-length(adjust[,1])
  adjustnum<-0
  function(i=adjustnum){
    if(adjustnum<limiter){
    adjustnum<<-adjustnum+1
    out<-data.frame(adjust[i,])
    names(out)<-names(adjust)
    return(out)
    }else{return('adjustincend')}
  }
}

#' setxgen
#' @description
#' This changes the base.frame per the row from adjust.


setxgen<-function(model=model,m,base.frame){
  for(i in 1:length(m)){
    base.frame[,names(m)[i]]<-m[i]
  }
  base.frame<-model.matrix(model$formula,base.frame)
  return(base.frame)
}


#' qi1
#' @description 
#' generates qi for a singular row from adjust

qi1<-function(beta=betas,setxframe=setx.frame,sm=m,num=n){

  yhatout<-sapply(c(1:num),FUN=function(i){
    return(mean(inv.logit(setxframe%*%beta[i,])))
    })
   yhat<-mean(yhatout)
   yhathi<-quantile(yhatout,.975)
   yhatlo<-quantile(yhatout,.025)
  
   outframe<-data.frame(est=yhat,estlo=yhatlo,esthi=yhathi)
   row.names(outframe)<-c()
  return(outframe)  
}



