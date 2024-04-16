

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

####Variable Inputs####
n_cohort2 <- 4
cohortSizes2 <- c(20,20,30,20)

IClist2 <- c("Control","IC1","IC2","Sustain")
n_IC2 <- length(IClist2)
IC_lens2 <- c(1,2,2,1)
isH2H_input <- FALSE
H2H_IClist2 <- c(2,3)
########


createSimpleDF <- function(n_cohort=n_cohort2,
                    cohortSizes=cohortSizes2,
                    IClist=IClist2,
                    n_IC=n_IC2,
                    IC_lens=IC_lens2) {
  
  IClist <- trimws(IClist) #clean whitespace
  
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


main_df <- createSimpleDF(); main_df %>% arrange(ConditionID,CohortID)

####Editable Table####
customizations_byCohortIC <- data.frame(CohortID=main_df$CohortID,
                                        ConditionName=main_df$ConditionName,
                                        ConditionDurationChange=rep(0,nrow(main_df)),
                                        ConditionDurationStartDelay=rep(0,nrow(main_df))  )


customizations_byCohortIC #User customizes; will update main_df.
########

# Account for time duration changes, delayed starts, and H2H designs.

createH2HDF <- function(df_test1,
                        IClist=IClist2,
                        H2H_IClist=H2H_IClist2) {
  
  n_H2H <- length(H2H_IClist)
    
    #Note: Of all the H2H Conditions, only keep the first one because we'll load all the H2H items at once.
    IC_ids <- 1:n_IC
    ICs_toloop <- IC_ids[!(IC_ids %in% H2H_IClist[-1])]
    df_test2 <- df_test1[0,]
    
    for (ic in ICs_toloop) {
      df_testH2H <- df_test1[0,]
      
      subset_torbind <- df_test1 %>% filter(ConditionID==ic)
      
      for (i in 1:(n_H2H)) {  df_testH2H <- rbind(df_testH2H,subset_torbind)   }
      
      df_testH2H <- df_testH2H %>% arrange(CohortID)
        
      if (ic %in% H2H_IClist) { 
        yVals <- condIDs <- condNames <- c()
        for (i in 0:(n_H2H-1)) { 
          yVals <- c(yVals,seq((n_cohort*n_H2H)-i,1,-(n_H2H))) 
          condIDs <- c(condIDs,rep(H2H_IClist[i+1],n_cohort))
          condNames <- c(condNames,rep(IClist[H2H_IClist[i+1]],n_cohort))
          }
        
        df_testH2H$ConditionID <- condIDs; df_testH2H$ConditionName <- condNames
        
      } else { yVals <- (n_cohort*n_H2H):1 }
      
      df_testH2H$yStart <- yVals
      df_testH2H$yEnd <- yVals
      
      df_testH2H %>%
        select(CohortID,ConditionID,xStart,xEnd,yStart,yEnd) 
      
      df_test2 <- rbind(df_test2,df_testH2H)
      
      
    }
    
    return(df_test2 %>% arrange())
    
}

createCustomizedDF <- function(n_cohort=n_cohort2,
                        cohortSizes=cohortSizes2,
                        IClist=IClist2,
                        n_IC=n_IC2,
                        IC_lens=IC_lens2,
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
                          sum((df_cust %>% filter(CohortID==df_test1$CohortID[r]))$ConditionDurationStartDelay), -1)
    
    #Doesn't check that change still keeps Duration at >=1
    if (df_test1$ConditionID[r]==1) { 
      xStart <- c(xStart,df_test1$ConditionDurationStartDelay[r]) 
      
      curr_xEnd = sum(df_test1$ConditionDurationStartDelay[r],
                       df_test1$ConditionDurationStartDelay[r+1],
                       df_test1$ConditionDurationChange[r],
                       IC_lens[1],
                       df_test1$CohortID[r],-1)
      
      xEnd <- c(xEnd,curr_xEnd)
      
    } else if (df_test1$ConditionID[r]==n_IC) { 
      
      xStart <- c(xStart, (xEnd[length(xEnd)]))
      
      xEnd <- c(xEnd, final_timeunit )
     
      
    } else {
      xStart <- c(xStart, (xEnd[length(xEnd)]) )
      
      curr_xEnd2 = sum(df_test1$ConditionDurationChange[r],
                       df_test1$ConditionDurationStartDelay[r+1],
                       IC_lens[df_test1$ConditionID[r]],
                       xEnd[length(xEnd)])
      
      xEnd <- c(xEnd, curr_xEnd2 )
      
    }
    
  }
  
  df_test1$xStart2 <- xStart
  df_test1$xEnd2 <- xEnd
  
  
  return(df_test1)
  
}

createGGPlotDF <- function(df_test1=df_test1.1) {
  
  #create df of properties by unique ICs
  
  IClist_unique <- unique(trimws( df_test1$ConditionName ))
  colorsbyCondition <- viridis::turbo(length(IClist_unique))
  df_ICproperties <- data.frame(ConditionName = IClist_unique,
                                ConditionID2 = length(IClist_unique),
                                arrange.colors = colorsbyCondition)
  
  #merge properties with df
  df_test2 <- left_join(df_test2,df_ICproperties,by='ConditionName')
  
  df_test2 <- df_test1 %>%
    left_join(df_ICproperties)  %>%
    arrange(CohortID)
  
  #make df for ggplot
  dfNoNA <- df_test2 %>%
    rename(theIntervention = ConditionName,
           width = CohortSize) %>%
    select(theIntervention,xStart2,xEnd2,yStart,yEnd,arrange.colors,width)
  
  dfNoNA <- dfNoNA %>%
    rename(xStart=xStart2,
           xEnd=xEnd2)
    
  return(dfNoNA)
  
}


####RUN CODE####


if(isH2H_input) { #if H2H
  df_test1.1 <- createCustomizedDF()
  df_test1.2 <- createH2HDF(df_test1.1, IClist2, H2H_IClist2) %>% arrange(ConditionID)
  dfNoNA <- createGGPlotDF(df_test1.2) %>% arrange(theIntervention)
  
} else {
  df_test1.1 <- createCustomizedDF() %>% arrange(ConditionID)
  dfNoNA <- createGGPlotDF(df_test1.1) %>% arrange(theIntervention)
  
}

VerticalGuidesNoNA <- data.frame ( xStartb = dfNoNA$xStart ,
                                   xEndb = dfNoNA$xEnd ,
                                   yStartb = dfNoNA$yStart ,
                                   yEndb = dfNoNA$yEnd )

###########

#### GGPLOT SCRIPT FROM APP.R ####

b <- ggplot() +
  theme_classic ( base_size = 15 )  + labs ( title = "the.main" , x = "xlabText" , y = "ylabText" ) +
  geom_segment(data = dfNoNA , mapping = aes(  x = xStart , y = yStart, xend = xEnd, yend = yEnd , col = theIntervention  ,
                                               size =  width  ) , lineend = "butt" ) +
   # geom_segment ( data = VerticalGuidesNoNA , aes ( x = xStartb , y = yStartb, xend = xEndb, yend = yEndb ),
  #                linetype = 3 )

  theme ( plot.title = element_text ( hjust = 0.5 , size = 15 ) ,
          axis.title.x = element_text ( size = 15 ) ,
          axis.title.y = element_text ( size = 15 )) +
  scale_y_continuous ( breaks = 0:max(dfNoNA$yEnd) ) +
  scale_x_continuous ( breaks = 0:max(dfNoNA$xEnd)  ) +
  scale_color_manual(values= viridis::viridis(length(unique(trimws( df_test1$ConditionName ))))) +
  scale_size(range = c(10,20), guide="none") +
  theme(legend.direction = "vertical",
        legend.position= "bottom" )
  
b

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
b
