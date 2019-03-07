#'calculated slope and intercept from object of class lm
#'@param fit An object of class lm
#'@param mode A numeric
#'@param pred.values Numeric. Values of predictor variable
#'@param label A character string
#'@param digits Integer indicating the number of decimal places 
#'@export
#'@examples
#'fit=lm(mpg~wt*hp,data=mtcars)
#'df=calEquation(fit)
calEquation=function(fit,mode=1,pred.values=NULL,label=NULL,digits=2){
        if(is.null(pred.values)) {
             if(mode==1) {
                  pred.values=mean(fit$model[[2]],na.rm=TRUE)+c(-1,0,1)*sd(fit$model[[2]],na.rm=TRUE)   
             } else if(mode==2){
                  pred.values=quantile(fit$model[[2]],probs=c(0.14,0.5,0.86),type=6)
             }
        }
        if(is.null(label)) label=names(fit$model)[2]
        intercept=pred.values*fit$coef[2]+fit$coef[1]
        slope=pred.values*fit$coef[4]+fit$coef[3]
        labels=paste0(label,"=",round(pred.values,digits))
        df=data.frame(intercept,slope,label=labels)
        df
}


#'Add lines with labels to pre-existing ggplot
#'@param p An object of class ggplot 
#'@param df A data.frame. Required columns are slope, intercept and label
#'@param xpos A numeric. Relative horizontal position
#'@param add.coord.fixed Logical. Whether or not add coord_fixed() function
#'@param lty line type 
#'@param color line color
#'@param size line size
#'@param ... Further arguments to be passed to geom_text
#'@importFrom ggplot2 ggplot stat_function
#'@export 
#'@examples
#'require(ggplot2)
#'fit=lm(mpg~wt*hp,data=mtcars)
#'df=calEquation(fit)
#'p=ggplot(data=mtcars)+geom_point(aes(x=hp,y=mpg))
#'add_lines(p,df)
#'add_lines(p,df,lty=1:3,color=1:3,size=1)+theme_bw()
add_lines=function(p,df,xpos=0.3,add.coord.fixed=TRUE,lty=1,color="black",size=0.5,...){
    
     count=nrow(df)
     info=getAspectRatio(p)
     ratio=info$ratio
          df$slope2=df$slope*ratio
     df$radian=atan(df$slope2)
     df$angle=df$radian*180/pi
     if(!is.null(df$xpos)) xpos=df$xpos
     x=info$xmin+(info$xmax-info$xmin)*xpos
     if(length(x)==1) x=rep(x,nrow(df))
     df$x=x
     df$y=df$x*df$slope+df$intercept
     if(is.null(df$vjust)) df$vjust=c(rep(-0.5,count-1),1.5)
     if(is.null(df$lty)) df$lty=lty
     if(is.null(df$color)) df$color=color
     if(is.null(df$size)) df$size=size
     df
     fun=list()
     statfun=list()
     for(i in 1:count){
        fun[[i]]=local({
             j<-i
             function(x){
               df$slope[j]*x+df$intercept[j]
             }
             })
        statfun[[i]]=local({
             j<-i
             stat_function(fun=fun[[j]],lty=df$lty[j],color=df$color[j],size=size)
        })

     }
     for(i in 1:count){
       p<-p+statfun[[i]]
     }
     p<-p+geom_text(data=df,aes_string(x="x",y="y",label="label",angle="angle",vjust="vjust"),color=df$color,...)
     if(add.coord.fixed) p<-p + coord_fixed(ratio=ratio)
     p
}




