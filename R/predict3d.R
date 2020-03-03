#' Generate regular sequences of desired length between minimum and maximal values
#' @param x a numeric vector
#' @param length desired length of the sequence
#' @export
myseq=function(x,length=20){

   seq(min(x,na.rm=T),max(x,na.rm=T),length=length)
}

#'Rank a numeric vector using proportional table and returns a new ordinal vector
#'
#'@param x a numeric vector
#'@param k	a integer specifies how many groups you want to classify. default value is 4
#'
#'@export
rank2group2=function(x,k=4){
   # x=mtcars$cyl;k=9
   temp=cumsum(prop.table(table(x)))
   res=c()
   count=length(unique(x))
   k1=ifelse(k>count,count,k)

   for(i in 1:(k1-1)){
      result=which.min(abs(temp-(i/k1)))
      res=c(res,result)
   }
   res=as.numeric(names(res))
   res=c(min(x,na.rm=TRUE)-0.01,res,max(x,na.rm=TRUE))
   res
   temp=cut(x,breaks=res)
   temp=as.numeric(temp)
   temp
   if(k>count) temp[temp==count]=k
   temp
}

#'Rank a numeric vector using proportional table and returns character vector of names of color using palette
#'@param x A numeric vector
#'@param palette Name of the color palette
#'@param reverse Logical. Whether or not reverse the order of the color palette
#'@param color Default color when palette is NULL
#'@importFrom ggiraphExtra palette2colors
#'@export
#'@examples
#'rank2colors(mtcars$wt,palette="Blues")
rank2colors=function(x,palette="Blues",reverse=TRUE,color="red"){
    if(is.null(palette)){
      result=rep(color,length(x))
   } else{
   k=length(ggiraphExtra::palette2colors(palette,reverse=reverse))
   group=rank2group2(x,k=k)
   result=ggiraphExtra::palette2colors(palette,reverse=reverse)[group]
   }
   result
}


#' Draw 3d predict plot using package `rgl`
#'
#' @param fit A model object for which prediction is desired.
#'@param pred The name of predictor variable
#'@param modx Optional. The name of moderator variable
#'@param mod2 Optional. The name of second moderator variable
#'@param dep Optional. The name of dependent variable
#'@param xlab  x-axis label.
#'@param ylab y-axis label.
#'@param zlab z-axis label.
#' @param width the width of device
#' @param colorn An integer giving the desired number of intervals. Non-integer values are rounded down.
#' @param maxylev Maximal length of unique values of y axis variable to be treated as a categorical variable.
#' @param se Logical. Whether or not show se. Only effective when the y-axis variable is a categorical one.
#' @param show.summary Logical. Whether or not show statistical summary
#' @param overlay Logical. Whether or not overlay plots
#' @param show.error Logical. Whether or not show error
#' @param show.legend Logical. Whether or not show legend
#' @param bg Character. Background color of plot
#' @param type For the default method, a single character indicating the type of item to plot. Supported types are: 'p' for points, 's' for spheres, 'l' for lines, 'h' for line segments from z = 0, and 'n' for nothing. For the mesh3d method, one of 'shade', 'wire', or 'dots'. Partial matching is used.
#' @param radius The size of sphere
#' @param palette Name of color palette
#' @param palette.reverse Logical. Whether or not reverse the palette order
#' @param color Default color. Color is used when the palette is NULL
#' @param show.subtitle Logical. If true, show regression call as subtitle
#' @param show.plane Logical. If true, show regression plane
#' @param plane.color Name of color of regression plane
#' @param plane.alpha Transparency scale of regression plane
#' @param summarymode  An integer indicating method of extracting typical value of variables. If 1, typical() is used.If 2, mean() is used.
#' @param ... additional parameters which will be passed to plot3d
#'
#' @importFrom rgl open3d next3d surface3d plot3d lines3d mfrow3d bg3d legend3d rglwidget axis3d par3d rgl.bg segments3d rgl.bringtotop rgl.clear
#' @importFrom grDevices xyz.coords
#' @importFrom tidyr spread
#' @importFrom dplyr select
#' @importFrom plyr dlply "."
#' @importFrom reshape2 dcast
#' @importFrom stringr str_detect
#' @export
#' @examples
#'fit=lm(mpg~hp*wt,data=mtcars)
#'predict3d(fit,show.error=TRUE)
#'fit=lm(log(mpg)~hp*wt,data=mtcars)
#'predict3d(fit,dep=mpg)
#'\donttest{
#'fit=lm(Sepal.Length~Sepal.Width*Species,data=iris)
#'predict3d(fit,radius=0.05)
#'require(TH.data)
#'fit=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
#'predict3d(fit)
#' mtcars$engine=ifelse(mtcars$vs==0,"V-shaped","straight")
#' fit=lm(mpg~wt*engine,data=mtcars)
#' predict3d(fit,radius=0.5)
#'fit=loess(mpg~hp*wt,data=mtcars)
#'predict3d(fit,radius=4)
#'}
predict3d=function (fit, pred=NULL,modx=NULL,mod2=NULL,dep=NULL,
                    xlab=NULL,ylab=NULL,zlab=NULL,
                    width=640,colorn = 20, maxylev=6, se = FALSE,
          show.summary = FALSE, overlay=NULL,show.error=FALSE,
          show.legend=FALSE,bg=NULL,type="s",radius=2,palette=NULL,palette.reverse=TRUE,
          color="red",show.subtitle=TRUE,
          show.plane=TRUE,plane.color="steelblue",plane.alpha=0.5,summarymode=1,...)
{

     # pred=NULL;modx=NULL;mod2=NULL;dep=NULL
     # xlab=NULL;ylab=NULL;zlab=NULL
     # width=640;colorn = 20; maxylev=6; se = FALSE
     # show.summary = FALSE; overlay=NULL;show.error=FALSE
     # show.legend=FALSE;bg=NULL;type="s";radius=2
     # palette=NULL;palette.reverse=TRUE;color="red"
     # show.plane=TRUE;plane.color="steelblue";plane.alpha=0.5;
     # show.subtitle=TRUE;summarymode=1


     myradius=radius

     method=class(fit)[1]
     if(method=="loess"){
          yname=attr(attr(fit$terms,"factors"),"dimnames")[[1]][1]
     } else{
          yname=names(fit$model)[1]
     }

     if(method=="loess"){
          rawdata=cbind(fit$y,data.frame(fit$x))
          colnames(rawdata)[1]=yname
     } else {
          rawdata=fit$model
     }


     checkVarname=FALSE
     xname <- quo_name(enexpr(pred))
     if(xname=="NULL"){
          pred<-NULL
          xname=names(rawdata)[2]
          checkVarname=TRUE
     } else{
          pred=enquo(pred)
     }
     colorname <- quo_name(enexpr(modx))
     modx<-enquo(modx)
     if(colorname=="NULL"){
          if(checkVarname & ncol(rawdata)>2){
               colorname=names(rawdata)[3]

          } else{
               modx<-NULL
               colorname<-NULL
          }
     }
     colorFactor=str_detect(colorname,"factor")
     colorname=restoreNames(colorname)

     facetname <- quo_name(enexpr(mod2))
     mod2<-enquo(mod2)
     if(facetname=="NULL"){
          if(checkVarname & ncol(rawdata)>3){
               facetname=names(rawdata)[4]
               facetname=restoreNames(facetname)
          } else{
               mod2<-NULL
               facetname<-NULL
          }
     }
     depc<-quo_name(enexpr(dep))
     if(depc!="NULL"){
          yname=depc
     }

     predictors=c(xname,colorname,facetname)
     if(checkVarname){
          predictors=unique(restoreNames(predictors))
          xname<-colorname<-facetname<-NULL
          xname<-predictors[1]
          if(length(predictors)>1){
               colorname=predictors[2]
          } else{
               colorname=names(fit$model)[5]
          }
          if(length(predictors)>2){
               facetname=predictors[3]
          }
          predictors=c(xname,colorname,facetname)
          # cat("predictors=",predictors,"\n")
     }


     data=restoreData(rawdata)
     data=restoreData2(data)
     data$yhat=predict(fit,newdata=data)
     # str(data)


   if(is.numeric(data[[colorname]])& !colorFactor){
        data$color=rank2colors(data[[colorname]],palette=palette,reverse=palette.reverse,color=color)
   } else{
          colorFactor=TRUE
          if(is.character(data[[colorname]])) {
               data$colorf=factor(data[[colorname]])
               data$color=as.numeric(data$colorf)
               data$colorn=data$color
               if(min(data$color)==0) data$colorn=data$color+1
          } else if(!is.factor(data[[colorname]])) {
               data$colorf=factor(data[[colorname]])
               data$color=as.numeric(as.character(data$colorf))
               data$colorn=data$color
               if(min(data$color)==0) data$colorn=data$color+1
          }  else{
               data$colorf=data[[colorname]]
               data$color=as.numeric(data[[colorname]])
               data$colorn=data$color
               if(min(data$color)==0) data$colorn=data$color+1
          }
   }


   newdata2=fit2newdata(fit,predictors,mode=3,colorn=colorn,maxylev=maxylev,summarymode=summarymode)
  newdata2
   colorcount = length(unique(newdata2[[colorname]]))
   facetcount = ifelse(is.null(facetname),0,length(unique(newdata2[[facetname]])))
   newx=unique(newdata2[[xname]])
   newcolor=unique(newdata2[[colorname]])




   open3d()


   # par3d(scale=c(1,1,0.2),cex=.6)

   if(is.null(overlay)){
       if(facetcount==0) overlay=TRUE
       else overlay=FALSE
   }

   (facetcount>0)&&(!overlay)
   if((facetcount>0)&&(!overlay)){
       nc=2
       nr=facetcount%/%nc
       if((facetcount%%nc)>0) nr=nr+1
       mfrow3d(nr,nc,sharedMouse=TRUE)
   }

   if((facetcount==0)&&(!is.numeric(data[[colorname]]))&&(!overlay)){
      nc=2
      nr=colorcount%/%nc
      if((colorcount%%nc)>0) nr=nr+1
      mfrow3d(nr,nc,sharedMouse=TRUE)
   }
   par3d(windowRect = 50 + c( 0, 0, width, width ) )
   rgl.bringtotop()
   if(!is.null(bg)) rgl.bg(color = bg)
   rgl.clear(type = c("shapes", "bboxdeco"))
   rgl.bringtotop()



   mylim=function(x){
      if(is.character(x)) x=as.numeric(factor(x))
      else if(is.factor(x)) x=as.numeric(x)
      range(x,na.rm=TRUE)+c(-1,1)*max(min(x,na.rm=TRUE)*0.1,0.2)

   }

   (myxlim=mylim(data[[xname]]))
   (myylim=mylim(data[[colorname]]))
   (myzlim=mylim(data[[yname]]))

   subtitle=ifelse(show.subtitle,
                   ifelse(is.null(attr(newdata2,"caption")),
                          Reduce(paste0,deparse(fit$call)),
                          paste0("Analysis assuming ",attr(newdata2,"caption"))),"")

   if(is.null(xlab))  xlab=xname
   if(is.null(ylab))  ylab=colorname
   if(is.null(zlab))  zlab=yname

   if(is.null(facetname)) {
         if(!colorFactor){  ## Numeric

            plot3d(data[[xname]],data[[colorname]],data[[yname]],col=data$color,
                   type=type,radius=myradius,
                   xlab=xlab,ylab=ylab,zlab=zlab,xlim=myxlim,ylim=myylim,zlim=myzlim,
                   sub=subtitle)

            if(show.error){
                 segments3d(as.vector(t(data[c(xname,xname)])),
                            as.vector(t(data[c(colorname,colorname)])),
                            as.vector(t(data[c(yname,"yhat")])),col="red",lwd=1)
            }
            newdata2=newdata2[order(newdata2[[colorname]],newdata2[[xname]]),]
            #temp= paste0("dcast(newdata2[1:3],",xname,"~",colorname,",value.var='",yname,"')[-1]")
            temp=paste0("newdata2[c('",xname,"','",colorname,"','",yname,"')] %>% spread(",
                        colorname,",`",yname,"`) %>% select(-1)")
            newdata3=eval(parse(text=temp))
            #surface3d(newx,sort(newcolor),as.matrix(newdata3),col="blue",alpha=.5)
            if(show.plane) surface3d(newx,newcolor,as.matrix(newdata3),col=plane.color,alpha=plane.alpha,
                                     front="lines",back="lines")

            # if(show.plane) surface3d(x.pred,y.pred,z.pred,
            #                          col=plane.color,alpha=plane.alpha,
            #                          front="lines",back="lines")

         } else{

             # head(data)
             addylabel=ifelse(is.numeric(data[[colorname]]),FALSE,TRUE)

            if(overlay) {
            plot3d(data[[xname]],data$color,data[[yname]],col=data$colorn,
                   type=type,radius=myradius,
                   xlab=xlab,ylab=ylab,zlab=zlab,xlim=myxlim,ylim=myylim,zlim=myzlim,
                   sub=subtitle,...)
                 # plot3d(data[[xname]],data$color,data[[yname]],col=data$colorn,
                 #        type=type,radius=myradius,
                 #        xlab=xname,ylab=colorname,zlab=yname,xlim=myxlim,ylim=myylim,zlim=myzlim,
                 #        sub=subtitle)

                 if(show.error){
                      segments3d(as.vector(t(data[c(xname,xname)])),
                                 as.vector(t(data[c("color","color")])),
                                 as.vector(t(data[c(yname,"yhat")])),col="red",lwd=1)
                 }

                 if(addylabel) {
                      levelcount= length(levels(data$colorf))
                      axis3d('y',at=1:levelcount,labels=levels(data$colorf))
                 }
                 data

                 colorname
                    # x.pred<-myseq(data[[xname]],colorn)
                    # y.pred<-unique(sort(data[[colorname]]))
                    # xy<-expand.grid(x=x.pred,y=y.pred)
                    # colnames(xy)=c(xname,colorname)
                    # z.pred=matrix(predict(fit,newdata=xy),nrow=colorn,ncol=colorcount)
                    # y.pred<-sort(unique(data$color))
                    # newdata2
                    # if(show.plane) surface3d(x.pred,y.pred,z.pred,
                    #                          col=plane.color,alpha=plane.alpha,
                    #                          front="lines",back="lines")
                    # newdata2
                    newdata2=newdata2[order(newdata2[[colorname]],newdata2[[xname]]),]

                    temp=paste0("newdata2[c('",xname,"','",colorname,"','",yname,"')] %>% spread(",
                                colorname,",",yname,") %>% select(-1)")
                    temp
                    newdata3=eval(parse(text=temp))
                    #surface3d(newx,sort(newcolor),as.matrix(newdata3),col="blue",alpha=.5)
                    if(show.plane){
                         surface3d(newx,newcolor,as.matrix(newdata3),col=plane.color,alpha=plane.alpha,
                                                  front="lines",back="lines")
                    }
            }

            df=expand.grid(x=myseq(data[[xname]]),
                           y=unique(data[[colorname]]))
            colnames(df)=c(xname,colorname)
            df
            df=restoreData2(df)
            result=predict(fit,newdata=df,type="response",se.fit=TRUE)
            result
            df[[yname]]=result$fit
            df$min=result$fit-result$se.fit
            df$max=result$fit+result$se.fit


            for(i in 1:length(unique(data[[colorname]]))){
               if((i>1)&&(!overlay)){
                  next3d()
                  # par3d(scale=myscale)
                  if(!is.null(bg)) bg3d(bg)
               }
               if(!overlay) {
                  data1=data[data[[colorname]]==sort(unique(data[[colorname]]))[i],]
                  plot3d(data1[[xname]],data1$color,data1[[yname]],
                         xlab=xlab,ylab=ylab,zlab=zlab,
                         type=type,col=i,radius=myradius,
                         xlim=myxlim,ylim=myylim,zlim=myzlim,
                         sub=paste0(colorname,":",sort(unique(data[[colorname]]))[i]),...)
                  if(show.error){
                       segments3d(as.vector(t(data1[c(xname,xname)])),
                                  as.vector(t(data1[c("color","color")])),
                                  as.vector(t(data1[c(yname,"yhat")])),col="red",lwd=1)
                  }

                  # x.pred<-myseq(data1[[xname]],colorn)
                  # y.pred<-unique(sort(data1[[colorname]]))
                  # xy<-expand.grid(x=x.pred,y=y.pred)
                  # colnames(xy)=c(xname,colorname)
                  # z.pred=matrix(predict(fit,newdata=xy),nrow=colorn,ncol=colorcount)
                  # y.pred<-sort(unique(data1$color))
                  # if(!overlay){
                  #      if(show.plane) surface3d(x.pred,y.pred,z.pred,
                  #                               col=plane.color,alpha=plane.alpha,
                  #                               front="lines",back="lines")
                  # }
                  newdata2=newdata2[order(newdata2[[colorname]],newdata2[[xname]]),]
                  temp=paste0("newdata2[c('",xname,"','",colorname,"','",yname,"')] %>% spread(",
                              colorname,",",yname,") %>% select(-1)")
                  newdata3=eval(parse(text=temp))
                  #surface3d(newx,sort(newcolor),as.matrix(newdata3),col="blue",alpha=.5)
                  if(!overlay){
                       if(show.plane) surface3d(newx,newcolor,as.matrix(newdata3),col=plane.color,alpha=plane.alpha,
                                                front="lines",back="lines")
                  }
                }
               df1=df[df[[colorname]]==sort(unique(data[[colorname]]))[i],]
               if(!is.numeric(df1[[colorname]])) df1[[colorname]]=i
               df1

               # print(df1)
               #
               lines3d(xyz.coords(as.matrix(df1[c(xname,colorname,yname)])),col=i,lwd=2)

               if(se) lines3d(xyz.coords(as.matrix(df1[c(xname,colorname,"ymax")])),col=i,lwd=0.5)
               if(se) lines3d(xyz.coords(as.matrix(df1[c(xname,colorname,"ymin")])),col=i,lwd=0.5)




            }


         }


         if(show.legend) legend3d("bottomright",legend=sort(unique(data[[colorname]])),pch=21,pt.bg=1:colorcount)

   } else{


      if(overlay)  {
         plot3d(data[[xname]],data[[colorname]],data[[yname]],col=data$color,
                type=type,radius=myradius,
                xlab=xlab,ylab=ylab,zlab=zlab,xlim=myxlim,ylim=myylim,zlim=myzlim,
                sub=subtitle,...)
           if(show.error){
                segments3d(as.vector(t(data[c(xname,xname)])),
                           as.vector(t(data[c(colorname,colorname)])),
                           as.vector(t(data[c(yname,"yhat")])),col="red",lwd=1)
           }
      }
      mysummary=function(df){
          temp=paste0("dcast(df,",xname,"~",colorname,",value.var='",yname,"')[-1]")
          # temp=paste0("df %>% spread(",colorname,",",yname,") %>% select(-1)")
         temp
         eval(parse(text=temp))
      }

         result=eval(parse(text=paste0("dlply(newdata2,.(",facetname,"),mysummary)")))
         result

         for(i in 1:facetcount){
            if((i>1)&&(!overlay)){
               next3d()
               # par3d(scale=myscale)
               if(!is.null(bg)) bg3d(bg)
            }
            if(!overlay){
            data1=data[data[[facetname]]==unique(data[[facetname]])[i],]


            plot3d(data1[[xname]],data1[[colorname]],data1[[yname]],
                   xlab=xlab,ylab=ylab,zlab=zlab,
                   type=type,col=data1$color,radius=myradius,
                   xlim=myxlim,ylim=myylim,zlim=myzlim,
                   sub=paste0(facetname,":",unique(data[[facetname]])[i]),...)
            if(show.error){
                 segments3d(as.vector(t(data1[c(xname,xname)])),
                            as.vector(t(data1[c(colorname,colorname)])),
                            as.vector(t(data1[c(yname,"yhat")])),col="red",lwd=1)
            }
            }

            z=as.matrix(result[[as.character(unique(data[[facetname]])[i])]])
            newx
            newcolor
            z

            if(show.plane) surface3d(newx,newcolor,z,col=i,alpha=plane.alpha,
                                     front="lines",back="lines")



         }

      if(show.legend) legend3d("bottomright",legend=unique(data[[facetname]]),pch=21,pt.bg=1:facetcount,cex=1)

   }



}

