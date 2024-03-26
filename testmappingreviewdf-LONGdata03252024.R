library(shiny)
library (ggplot2)
library (stringr)
#library (grid) 
library (gridBase)
library (gridExtra)
# library (gridExtra) https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
library (shinythemes)
library (textclean)

n_cohort <- 4
IClist <- c("Control","Intervention","Sustain")
n_IC <- length(IClist)

df_default <- data.frame(date_start = rep(date(),n_cohort*n_IC),
                         len_ic = rep(1,n_cohort*n_IC),
                         len_delayStart = rep(0,n_cohort*n_IC),
                         len_delayEnd = rep(0,n_cohort*n_IC),
                         isHeadtoHead = rep(F,n_cohort*n_IC)
                         )

templist <- templist2 <- templist3 <- c()

for (c in 1:n_cohort){ 
  
  templist <- c(templist, rep(as.character(c),n_IC)) 
  templist2 <- c(templist2, as.character(c(1:n_IC))) 
  templist3 <- c(templist3, IClist)
  
}

df_default$cohort_ID <- templist
df_default$ic_ID <- templist2
df_default$ic_name <- templist3

