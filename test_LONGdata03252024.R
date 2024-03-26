

library(shiny)
library (ggplot2)
library (stringr)
#library (grid) 
library (gridBase)
library (gridExtra)
# library (gridExtra) https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
library (RColorBrewer)
library (viridis)
library (shinythemes)
library (textclean)

n_cohort2 <- 4
cohortSizes2 <- c(20,20,30,20)

IClist2 <- c("Control","IC1","IC2","Sustain")
n_IC2 <- length(IClist)
IC_lens2 <- c(1,2,2,1)



#Note: init_df() account for head to head or ABA designs.
#Note: init_df() does not customize Condition Durations.


f_init_df <- function(n_cohort=n_cohort2,
                    cohortSizes=cohortSizes2,
                    IClist=IClist2,
                    n_IC=n_IC2,
                    IC_lens=IC_lens2) {
  
  df_test <- data.frame(CohortID = rep(1:n_cohort,n_IC),
                        CohortSize = rep(cohortSizes,n_IC),
                        yStart = rep(n_cohort:1,n_IC),
                        yEnd = rep(n_cohort:1,n_IC))
  
  templist <- templist2 <- templist3 <- c()
  
  for (c in 1:n_IC){ 
    
    templist <- c(templist, rep(IC_lens[c],n_cohort)) 
    templist2 <- c(templist2, rep(c,n_cohort)) 
    templist3 <- c(templist3, rep(IClist[c],n_cohort))
    
  }
  
  df_test$ConditionID <- templist2
  df_test$ConditionName <- templist3
  df_test$ConditionDuration <- templist
  
  
  #Will want to optimize the for-loop with dplyr later, but wasn't working
  xStart <- xEnd <- c()
  
  for (r in 1:nrow(df_test)){
    if (df_test$ConditionID[r]==1) { 
      xStart <- c(xStart,0) 
      xEnd <- c(xEnd, sum(df_test$CohortID[r],-2,df_test$ConditionDuration[r]) ) 
      
    } else if (df_test$ConditionID[r]==n_IC) { 
      xEnd <- c(xEnd, sum(-2,n_cohort,IC_lens) ) 
      xStart <- c(xStart, sum(df_test$CohortID[r],-1,IC_lens[2:df_test$ConditionID[r]]) )
    } else {
      xStart <- c(xStart, sum(df_test$CohortID[r],-2,IC_lens[2:df_test$ConditionID[r]]) )
      xEnd <- c(xEnd, sum(df_test$CohortID[r],-2,IC_lens[1:df_test$ConditionID[r]]) )
    }
    
  }
  
  df_test$xStart <- xStart
  df_test$xEnd <- xEnd
  
  return(df_test %>% arrange(CohortID))
  
}


main_df <- f_init_df(); main_df

customizations_byCohortIC <- data.frame(CohortID=main_df$CohortID,
                                        ConditionName=main_df$ConditionName,
                                        ConditionDurationChange=rep(0,nrow(main_df)),
                                        ConditionDurationStartDelay=rep(1,nrow(main_df))  )


customizations_byCohortIC #User customizes; will update main_df.


f_ggplot_df <- function(IC_lens=IC_lens2,
                        df_test=main_df,
                        df_cust=customizations_byCohortIC) {
  
  #To account for changed Condition Duration, merge with df_test then create updated columns for xStart and xEnd
  df_test1 <- left_join(df_test,df_cust,by=c('CohortID','ConditionName'))
  
  #Will want to optimize the for-loop with dplyr later, but wasn't working
  xStart <- xEnd <- c()
  
  for (r in 1:nrow(df_test1)){
    
    final_timeunit <- sum(n_cohort,
                          IC_lens,
                          sum((df_cust %>% filter(CohortID==df_test1$CohortID[r]))$ConditionDurationChange),
                          sum((df_cust %>% filter(CohortID==df_test1$CohortID[r]))$ConditionDurationStartDelay), -2)
    
    #Doesn't check that change still keeps Duration at >=1
    if (df_test1$ConditionID[r]==1) { 
      xStart <- c(xStart,df_test1$ConditionDurationStartDelay[r]) 
      
      curr_xEnd = sum(df_test1$ConditionDurationStartDelay[r],
                       df_test1$ConditionDurationStartDelay[r+1],
                       df_test1$ConditionDurationChange[r],
                       IC_lens[1],
                       df_test1$CohortID[r],-2)
      
      xEnd <- c(xEnd,curr_xEnd)
      
    } else if (df_test1$ConditionID[r]==n_IC) { 
      
      xStart <- c(xStart, (xEnd[length(xEnd)]+1))
      
      xEnd <- c(xEnd, final_timeunit )
     
      
    } else {
      xStart <- c(xStart, (xEnd[length(xEnd)]+1) )
      
      curr_xEnd2 = sum(df_test1$ConditionDurationChange[r],
                       df_test1$ConditionDurationStartDelay[r+1],
                       IC_lens[df_test1$ConditionID[r]],
                       xEnd[length(xEnd)])
      
      xEnd <- c(xEnd, curr_xEnd2 )
      
    }
    
  }
  
  df_test1$xStart2 <- xStart
  df_test1$xEnd2 <- xEnd
  
  
  #assign colors across Conditions
  colorsbyCondition <- viridis::turbo(n_IC)
  
  df_test2 <- df_test1 %>%
    mutate(arrange.colors = colorsbyCondition[ConditionID])  %>%
    arrange(CohortID)
  
  #make df for ggplot
  dfNoNA <- df_test2 %>%
    rename(theIntervention = ConditionName,
           width = CohortSize) %>%
    select(theIntervention,xStart2,xEnd2,yStart,yEnd,arrange.colors,width)
  
  return(dfNoNA)
  
}


dfNoNA2 <- f_ggplot_df()

dfNoNA2







#Seems like Hendricks used a df like this for the ggplot and created it using the matrix. 
#Maybe we can skip the middle step.
# dfNoNA2 <- data.frame(xStart = rep(0,n_cohort*n_IC), #int
#                       xEnd = rep(1,n_cohort*n_IC), #int
#                       yStart = rep(0,n_cohort*n_IC), #int
#                       yEnd = rep(1,n_cohort*n_IC), #int
#                       arrange.colors = rep(viridis::turbo(n_IC),n_cohort), #use gradient
#                       width = rep(20,n_cohort*n_IC), #int
#                       theIntervention = rep(IClist,n_cohort) #name
#                       )








# 
# b <- ggplot( dfNoNA2   ) +
#   theme_classic ( base_size = 15 )  + labs ( title = "the.main" , x = "xlabText" , y = "ylabText" ) +
#   geom_segment(data = dfNoNA2 , mapping = aes(  x = xStart , y = yStart, xend = xEnd, yend = yEnd , col = theIntervention  , 
#                                                size =  width  ) , lineend = "butt" ) +
#   geom_segment ( data = VerticalGuidesNoNA , aes ( x = xStartb , y = yStartb, xend = xEndb, yend = yEndb ),
#                  linetype = 3 ) +
#   theme ( plot.title = element_text ( hjust = 0.5 , size = 15 ) ,
#           axis.title.x = element_text ( size = 15 ) ,
#           axis.title.y = element_text ( size = 15 )  ) +
#   scale_y_continuous ( breaks = yAxis ) +
#   scale_x_continuous ( breaks = xAxis  ) +
#   theme(legend.position=  "right" ) +
#   
#   # the following doesn't produce legend, there is one when left out
#   
#   theme(legend.key.width = unit(3 ,"cm")) +
#   # 
#   # scale_color_manual(values= the.colors [ 1 : length ( uniqueInterventions)]) +
#   # scale_size("size" , guide = "none") +
#   # 
#   # theme(legend.text=element_text(size= BiggerSize )) +
#   # theme(legend.direction='vertical') + 
#   # labs( color = 'Implementation Conditions'  ) + 
#   # guides(colour = guide_legend(override.aes = 
#   #                                list(color = 
#   #                                       factor  (the.colors [ 1 : length ( uniqueInterventions)])  ,
#   #                                     linetype = 1 , size = 4 )))
# 
# b



# df_default <- data.frame(date_start = rep(date(),n_cohort*n_IC),
#                          len_ic = rep(1,n_cohort*n_IC),
#                          len_delayStart = rep(0,n_cohort*n_IC),
#                          len_delayEnd = rep(0,n_cohort*n_IC),
#                          isHeadtoHead = rep(F,n_cohort*n_IC)
#                          )
# 
# templist <- templist2 <- templist3 <- c()
# 
# for (c in 1:n_cohort){ 
#   
#   templist <- c(templist, rep(as.character(c),n_IC)) 
#   templist2 <- c(templist2, as.character(c(1:n_IC))) 
#   templist3 <- c(templist3, IClist)
#   
# }
# 
# df_default$cohort_ID <- templist
# df_default$ic_ID <- templist2
# df_default$ic_name <- df_orig$theIntervention <- templist3

