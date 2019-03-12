#' get opposite arithmetic operator
#' @param operator A character
revOperator=function(operator){
     result=operator
     if(operator=="+") result="-"
     else if(operator=="-") result="+"
     else if(operator=="*") result="/"
     else if(operator=="/") result="*"
     result
}

#'Restore data from arithmetic operator
#'@param df A data.frame
#'@param changeLabel logical
#'@importFrom stringr str_extract
#'@export
#'@examples
#'fit=lm(2^mpg~hp*wt,data=mtcars)
#'summary(fit)
#'restoreData3(fit$model)
restoreData3=function(df,changeLabel=FALSE){

     pattern="/|-|\\+|\\*|\\^"
     select1=which(str_detect(names(df),pattern))
     select2=which(str_detect(names(df),"I\\("))
     select=setdiff(select1,select2)

     addgroup=FALSE
     if(changeLabel){
             changelabel=TRUE
             if(str_detect(df$label[1],"\\|")) {
                     addgroup=TRUE
                     df$label3=str_extract(df$label,"\\|.*")
             }
             df$label=str_replace(df$label,"\\|.*","")
     }
     for(i in seq_along(select)){
          tempname=names(df)[select[i]]
          temp=str_replace_all(tempname," ","")
          temp
          operator=str_extract(temp,pattern)
          operator
          temp1=unlist(strsplit(temp,pattern))
          temp1
          if(sum(beNumeric(temp1))!=1) next
          number=temp1[beNumeric(temp1)]
          varname=temp1[!beNumeric(temp1)]
          pos=which(beNumeric(temp1))
          number
          varname
          pos
          if(pos==1){
               if(operator %in% c("/","-")){
                    eq=paste0(number,operator,"df[['",tempname,"']]")
                    if(changeLabel) df$label=paste0(number,operator,"(",df$label,")")
               } else if(operator=="^"){
                    eq=paste0("log(df[['",tempname,"']])/log(",number,")")
                    if(changeLabel) df$label=paste0("log(",df$label,")/log(",number,")")
               } else{
                    operator=revOperator(operator)
                    eq=paste0("df[['",tempname,"']]",operator,number)
                    if(changeLabel) df$label=paste0("(",df$label,")",operator,number)
               }
          } else{
               if(operator=="^"){
                    eq=paste0("df[['",tempname,"']]^(1/",number,")")
                    if(changeLabel) df$label=paste0("(",df$label,")","^(1/",number,")")
               } else{
               operator=revOperator(operator)
               eq=paste0("df[['",tempname,"']]",operator,number)
               if(changeLabel) df$label=paste0("(",df$label,")",operator,number)
               }
          }
          eq
          df
          df[[varname]]=eval(parse(text=eq))
     }
     if(changeLabel) {if(addgroup) df$label=paste0(df$label,df$label3)}
     df
}


