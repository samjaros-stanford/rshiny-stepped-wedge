
# Server
server <- function(input, output) {  ### Open server 
  
  # OutputPlot test8e.R
  # set tabsetPanel appropriately for validate input$WhichDesignTabPanel ) > 0 && 
  #           input$WhichDesignTabPanel  == "DesignNumbersTabPanel" )
  
  # CONSTANTS
  
  InputValueFileStem <- "inputVals"
  InputValueFileType <- ".RData" # Default Settings for the Assignment Plot
  
  
  LineWidth <- 20  # standard when all the steps have same number of entities
  the.colors <- c( "blue" , "green" , "yellow" , "orange" , "pink" , "red" , "purple" , sample ( colors( distinct = FALSE ) , size = length (colors( distinct = FALSE ) )))
  stem.InterventionName <- "Implementation Conditions"
  defaultTimeElementName <- "Time Unit"
  defaultEntityName <- "Assigned Units"
  defaultLanguage <- "English"
  default.maxXLabelValues <- 12
  default.WhereToPutLegend <- "bottom"
  
  # Initialize 
  Checks <- list ()
  inputCheck <- list ()
  
  
  # Initialize and provide constants
  
  All.Intervention.Conditions <- NULL
  
  
  
  
  lastChar <- function(x){
    rslt <- NULL
    if (length(x) > 0 && nchar(x) > 0) {
      ending <- nchar(x)
      rslt <- substring(x,ending,ending)
    }
    return(rslt)
  }
  
  not.Present <- function ( x ) {
    # return TRUE if a value is not available
    if (  length ( x ) == 0 || is.null ( x )  || trimws (x) == "")
      rslt <- TRUE
    else 
      rslt <- FALSE
    return ( rslt )
  }
  
  pr <- function ( Unquoted ) {
    # 
    Quoted = substitute ( Unquoted)
    print ( Quoted )
    print ( Unquoted )
  }
  
  makeFileName_reactive <- reactive (  {
    if ( trimws (input$AssignmentPlotDirectory) == "" )
      slash <- ""
    else
      slash <- "/"
    
    if ( input$AddDateAndTime ) 
      AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
    else
      AddDateAndTime <- ""
    
    paste0 ( input$AssignmentPlotDirectory , slash , input$AssignmentPlotFileName , AddDateAndTime , ".pdf" )
    
  } )
  
  count.reactive <- eventReactive ( input$SaveAssignmentPlot , { input$SaveAssignmentPlot })
  
  # reactive expression
  fullFileName_reactive <- eventReactive ( input$SaveAssignmentPlot , {
    if ( input$AddDateAndTime  )
      AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
    else
      AddDateAndTime <- ""
    
    if ( trimws (input$AssignmentPlotDirectory) == "" )
      slash <- ""
    else {
      if ( lastChar (input$AssignmentPlotDirectory ) != .Platform$file.sep )
        slash <- .Platform$file.sep
    }
    
    
    if ( input$AddDateAndTime ) 
      AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
    else
      AddDateAndTime <- ""
    
    AssignPltTitle <- input$AssignmentPlotTitle 
    if (! is.null ( AssignPltTitle ) || trimws (AssignPltTitle) != "")
      AssignPltTitle <- paste( "." , AssignPltTitle , ".")
    
    paste0 ( input$AssignmentPlotDirectory , slash ,
             input$AssignmentPlotFileName , 
             AddDateAndTime , 
             input$AssignmentPlotTitle ,
             ".",
             input$PlotFileType )
    
  })
  
  savePlot_reactive <- eventReactive( input$SaveAssignmentPlot, {
    
    makeFileName <- function ( AddDateAndTime , 
                               AssignmentPlotDirectory,
                               FileNameStem  ,
                               FileDescription ,
                               FileType ,
                               ReplaceSpace 
    ) {
      if ( AddDateAndTime  )
        AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
      else
        AddDateAndTime <- ""
      
      if ( trimws ( AssignmentPlotDirectory) == "" )
        slash <- ""
      else {
        if ( lastChar (AssignmentPlotDirectory ) != .Platform$file.sep )
          slash <- .Platform$file.sep
      }
      
      FileDescr <- FileDescription
      if ( ! not.Present ( FileDescr ))
        FileDescr <- paste0( "." , FileDescr )
      if ( ReplaceSpace ) { 
        FileDescr <- gsub ( " " , "_" , FileDescr)
        FileDescr <- gsub ( "\n" , "_" , FileDescr)
        FileDescr <- gsub ( "\t" , "_" , FileDescr)
      }
      
      
      rslt <- paste0 ( AssignmentPlotDirectory , slash , FileNameStem , AddDateAndTime , FileDescr , FileType )
      return ( rslt )
      
    }
    
    validate ( need ( length (input$AssignmentPlotDirectory) == 0 || 
                        trimws (input$AssignmentPlotDirectory) == "" ||
                        ( length(input$AssignmentPlotDirectory) > 0  ) && 
                        dir.exists (input$AssignmentPlotDirectory )  , 
                      paste ("Error:" , input$AssignmentPlotDirectory ,  
                             "Directory does not exist or cannot write to it") 
    ) 
    ) 
    
    ggsave ( fullFileName_reactive ( ))
    
    LatestInputValueFile <- makeFileName ( TRUE , 
                                           input$AssignmentPlotDirectory,
                                           FileNameStem =   "inputVals" ,
                                           FileDescription = input$AssignmentPlotTitle ,
                                           FileType = InputValueFileType , 
                                           ReplaceSpace = TRUE )
    
    save (inputCheck , file = LatestInputValueFile )
    
  })
  
  output$FilePrintedOK <- renderText({
    
    savePlot_reactive () 
    
    if (file.exists ( fullFileName_reactive () ))
      paste ( "Saved Assessment Plot Number" , count.reactive ( ) , fullFileName_reactive() )
    else
      paste ( "File " , fullFileName_reactive() , "not saved" )
  })
  
  
  
  output$InputEntityName <- renderUI({  #  output$InputEntityName ####
    print ("input$EntityName =" )
    print (input$EntityName )
    
    GetExistingInputCheck <- list( )
    StartingInputValueFilename <- "inputVals.Rdata"
    
    
    if ( file.exists ( StartingInputValueFilename )) {
      load  (file = StartingInputValueFilename )
      GetExistingInputCheck <- inputCheck
    } 
    
    
    SeparateStringByCommas <- function ( string , single.character = "," ) {
      g <-SeparateStringByCharacter  ( string , single.character )
      if (is.null ( g ))
        g <- ""
      return ( g )
    }
    
    SeparateStringByCharacter  <- function ( string , single.character ) {
      # return a character vector
      # cat ( "***" , string , "\n")
      if ( not.Present ( string ))
        the.vector <- NULL
      else 
        the.vector <- unlist ( strsplit (as.character (unlist ( string )) , single.character ))
      return ( the.vector )
    }
    
    
    
    getName <- function ( a , Default , plural = TRUE ) {
      # if a is missing then use Default
      # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
      # otherwise just add an s
      if ( length ( a ) == 0 || trimws (a) == "") {
        rslt <- Default 
      } else
        rslt <- a 
      
      if ( plural ) {
        the.size <- nchar ( rslt )
        the.last.char <- substr ( rslt , the.size, the.size)
        
        
        if (the.last.char != tolower ( the.last.char)   ) {
          rslt <- toupper ( make_plural ( rslt ) )
        } else
          rslt <- make_plural ( rslt )
        
      }
      return ( rslt )
    }
    
    
    if ( length (GetExistingInputCheck$EntitiesPerStep) == 0 ) { 
      NbrEntitiesDefault <- 1
    } else
      NbrEntitiesDefault <- GetExistingInputCheck$EntitiesPerStep 
    
    tagList (
      textInput("EntitiesPerStep",
                paste0("Number of ", getName ( unlist (input$EntityName) , "Entity" , plural = TRUE ) , " for each Step \n(If different across Steps separate with ',')" ),
                value = NbrEntitiesDefault ) 
    )
  })
  
  outputOptions(output,"InputEntityName",suspendWhenHidden = TRUE)
  
  output$UseTimeElementNameForTimeElementsPerPeriod <- renderUI({ # output$UseTimeElementNameForTimeElementsPerPeriod ----
    
    GetExistingInputCheck <- list( )
    StartingInputValueFilename <- "inputVals.Rdata"
    
    
    if ( file.exists ( StartingInputValueFilename )) {
      load  (file = StartingInputValueFilename )
      GetExistingInputCheck <- inputCheck
    } 
    
    
    
    
    pr (input$WhichDesignTabPanel )
    
    
    getName <- function ( a , Default , plural = TRUE ) {
      # if a is missing then use Default
      # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
      # otherwise just add an s
      if ( length ( a ) == 0 || trimws (a) == "") {
        rslt <- Default 
      } else
        rslt <- a 
      
      if ( plural ) {
        the.size <- nchar ( rslt )
        the.last.char <- substr ( rslt , the.size, the.size)
        
        
        if (the.last.char != tolower ( the.last.char)   ) {
          rslt <- toupper ( make_plural ( rslt ) )
        } else
          rslt <- make_plural ( rslt )
        
      }
      return ( rslt )
    }
    
    
    
    
    if ( length (GetExistingInputCheck$TimeElementsPerPeriod ) == 0 ) { 
      NbrTimeElementsDefault <- 1
    } else
      NbrTimeElementsDefault <- GetExistingInputCheck$TimeElementsPerPeriod 
    
    tagList (
      textInput("TimeElementsPerPeriod",
                paste0("ADJUST TIME SCALE: Number of ", getName ( input$TimeElementName, "Time Element" , plural = TRUE ) , " Between Time Periods\n(If different across Time Periods separate by ',')" ),
                value = NbrTimeElementsDefault ) 
    )
  })
  
  output$UseTimeElementNameForNumberPeriodsEachCondition <- renderUI({ # output$UseTimeElementNameForNumberPeriodsEachCondition ----
    
    print ("input$TimeElementName =" )
    print (input$TimeElementName )
    
    GetExistingInputCheck <- list( )
    StartingInputValueFilename <- "inputVals.Rdata"
    
    
    if ( file.exists ( StartingInputValueFilename )) {
      load  (file = StartingInputValueFilename )
      GetExistingInputCheck <- inputCheck
    } 
    
    
    
    getName <- function ( a , Default , plural = TRUE ) {
      # if a is missing then use Default
      # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
      # otherwise just add an s
      if ( length ( a ) == 0 || trimws (a) == "") {
        rslt <- Default 
      } else
        rslt <- a 
      
      if ( plural ) {
        the.size <- nchar ( rslt )
        the.last.char <- substr ( rslt , the.size, the.size)
        
        
        if (the.last.char != tolower ( the.last.char)   ) {
          rslt <- toupper ( make_plural ( rslt ) )
        } else
          rslt <- make_plural ( rslt )
        
      }
      return ( rslt )
    }
    
    if ( length (GetExistingInputCheck$NumberPeriodsEachCondition ) == 0 ) { 
      NbrPeriodsEachConditionDefault  <- 1
    } else
      NbrPeriodsEachConditionDefault <- GetExistingInputCheck$NumberPeriodsEachCondition
    
    SeparateStringByCommas <- function ( string , single.character = "," ) {
      g <-SeparateStringByCharacter  ( string , single.character )
      if (is.null ( g ))
        g <- ""
      return ( g )
    }
    
    SeparateStringByCharacter  <- function ( string , single.character ) {
      # return a character vector
      # cat ( "***" , string , "\n")
      if ( not.Present ( string ))
        the.vector <- NULL
      else 
        the.vector <- unlist ( strsplit (as.character (unlist ( string )) , single.character ))
      return ( the.vector )
    }
    
    moreThanOne <- length (  SeparateStringByCommas (input$InterventionConditions ) )
    
    tagList (
      
      textInput("NumberPeriodsEachCondition",
                paste0("Number of Time Periods for:  " ,  
                       getName ( input$InterventionConditions  , "Intervention/Implementation Conditions(s)" , plural = FALSE ) , "\n(If different across Interventions separate by ',')") ,
                value =  NbrPeriodsEachConditionDefault  )
    )
  })
  
  ###  COMMENT ALL THIS SECTION OUT, NOT NEEDED
  # output$UseTimeElementNameForAddonBaselineTimeElements <- renderUI({ # output$UseTimeElementNameForAddonBaselineTimeElements  ----
  #   
  #   print ("input$TimeElementName =" )
  #   print (input$TimeElementName )
  
  
  
  #   tagList (
  
  #     textInput("AddonBaselineTimeElements",
  #               paste0("Adjust Starting Times for Steps in Terms of ", getName ( input$TimeElementName  , "Time Element") , "s\n(If different across Steps separate by ',')") ,
  #               value = "1")
  #   )
  # })
  
  
  #  outputOptions(output)
  
  #  outputOptions(output,"UseTimeElementNameForTimeElementsPerPeriod",suspendWhenHidden = FALSE)
  
  
  
  output$AssignmentPlot <- renderPlot({  ###  output$AssignmentPlot ----
    #  Enter text??.R below
    
    
    # set tabsetPanel appropriately for validate input$WhichDesignTabPanel ) > 0 && 
    #           input$WhichDesignTabPanel  == "DesignNumbersTabPanel" )
    
    
    saveFile <- eventReactive( input$SaveAssignmentPlot , {
      ggsave( fileName )
    })
    
    # Functions
    ### copy from here into server 
    
    lastChar <- function ( x ) {
      rslt <- NULL
      if ( length ( x ) > 0 && nchar ( x ) > 0 ) {
        ending <- nchar ( x )
        rslt <- substring ( x , ending , ending )
      }
      
      return (rslt )
    }
    
    pr <- function ( Unquoted ) {
      # 
      Quoted = substitute ( Unquoted)
      print ( Quoted )
      print ( Unquoted )
    }
    
    # Functions
    
    
    
    # new 2020 01 31
    ReplaceColonCommaPositions <- function ( string , replacement , default = 1 ) {
      # Expand or Contract replacement, which can have colons and commas, to same order as string
      # string:  text string with : and , separators, all have to be non-blank
      # replacement: either starts with a non-null character or takes default value
      #  both of these input criteria are tested 
      # find colon and comma locations
      # return replacement text string that has same : and , positions as string does
      
      BlocksInString <- SeparateStringByCommas ( string )
      
      if ( length ( BlocksInString) == 1 && trimws ( BlocksInString ) == "" ) {
        rebuild <- NULL  # this should not happen
      } else { 
        
        if ( !is.null ( replacement)) {
          BlocksInReplacement <- SeparateStringByCommas ( replacement )
        } else
          BlocksInReplacement <- default
        
        
        # look at each block separated by commas
        
        lastReplacement <- 1
        CurrentString <- list ( )
        CurrentReplacement <- list ( )
        for ( i in 1 : length ( BlocksInString )) {
          cat ( i , " t1\n")
          CurrentString [[ i ]] <- SeparateStringByCharacter( BlocksInString [ i ] , ":")
          if (! is.na ( BlocksInReplacement [ i ] ) ) {
            cat ( i , " t2\n")
            CurrentReplacement [[ i ]] <- SeparateStringByCharacter( BlocksInReplacement [ i ] , ":")
          } else {
            cat ( i , " t3\n")
            CurrentReplacement [[ i ]] <- 1
          }
          
          
          lngth <- length ( CurrentString [[ i ]])
          if ( lngth <= length ( CurrentReplacement [[ i ]])) {
            cat ( i , " t4\n" )
            CurrentReplacement [[ i ]] <- CurrentReplacement [[ i ]] [ 1 : lngth ]
          } else { # need to expand CurrentReplacement
            if ( length ( CurrentReplacement [[ i ]] ) == 0 ) { 
              cat ( i , " t5\n" )
              CurrentReplacement [[ i ]] <- 1
            }
            
            
            
            # make same length as CurrentString [[ i ]]
            lngthRepl <- length ( CurrentReplacement [[ i ]] )
            lastOne <- CurrentReplacement [[ i ]] [ lngthRepl ]
            nbrToAdd <- lngth - lngthRepl
            cat ( i , " t6\n")
            CurrentReplacement [[ i ]] <- c ( CurrentReplacement [[ i ]] , rep ( lastOne , nbrToAdd ) )
          } # end need to expand CurrentReplacement
        } # end for loop
        
        
        rebuild <- paste ( CurrentReplacement [[ 1 ]] , collapse = ":")
        if ( length ( BlocksInString ) > 1 ) 
          for ( i in 2 : length ( BlocksInString )) {
            newpart <- paste0 (CurrentReplacement [[ i ]] , collapse = ":")
            rebuild <- paste0 ( rebuild , "," , newpart  )
          }
      } 
      return ( rebuild )
    }
    
    
    SeparateStringByBoth <- function ( string  ) {
      #  string -  text string surrounded by quotes
      #  characters - , characters = c("," , ":")
      # return a vector of strings
      
      
      characters = c("," , ":") 
      
      rslt <- unlist ( strsplit (as.character (unlist ( string )) , characters[1]))
      rslt <- unlist ( strsplit ( rslt , characters[ 2 ]) )
      
      return ( rslt )
    }
    
    
    not.Present <- function ( x ) {
      # return TRUE if a value is not available
      if (  length ( x ) == 0 || is.null ( x )  || trimws (x) == "")
        rslt <- TRUE
      else 
        rslt <- FALSE
      return ( rslt )
    }
    
    
    
    BlocksOfTextsSeparatedbyComma <- function ( Separated ) {
      rslt <- unlist ( Separated$first)
      return ( rslt)
    }
    
    
    getName <- function ( a , Default , plural = TRUE ) {
      # if a is missing then use Default
      # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
      # otherwise just add an s
      if ( length ( a ) == 0 || trimws (a) == "") {
        rslt <- Default 
      } else
        rslt <- a 
      
      if ( plural ) {
        the.size <- nchar( rslt )
        the.last.char <- substr ( rslt , the.size, the.size)
        
        
        if (the.last.char != tolower ( the.last.char)   ) {
          rslt <- toupper ( make_plural ( rslt ) )
        } else
          rslt <- make_plural ( rslt )
        
      }
      return ( rslt )
    }
    
    
    is.wholenumber <- function (x, tol = .Machine$double.eps^0.5)  {
      # is.wholenumber :  adapted from library ( FRACTION )
      if ( is.character (x ) || ! is.vector ( x ) ) {
        rslt <- FALSE
      } else 
        rslt <- all ( abs(x - round(x)) < tol )
      return ( rslt )
    }
    
    
    not.Present <- function ( x ) {
      # return TRUE if a value is not available
      if (  length ( x ) == 0 || is.null ( x )  || trimws ( x ) == "" || is.na (x ))
        rslt <- TRUE
      else 
        rslt <- FALSE
      return ( rslt )
    }
    
    
    SeparateStringByCharacter  <- function ( string , single.character ) {
      # return a character vector
      # cat ( "***" , string , "\n")
      if ( not.Present ( string ))
        the.vector <- NULL
      else 
        the.vector <- unlist ( strsplit (as.character (unlist ( string )) , single.character ))
      return ( the.vector )
    }
    
    SeparateStringByCommas <- function ( string , single.character = "," ) {
      g <-SeparateStringByCharacter  ( string , single.character )
      if (is.null ( g ))
        g <- ""
      return ( g )
    }
    
    # handle input$values that are not set
    copyValue <- function ( x , default  ) {
      if ( is.null (x ) || length ( x ) == 0 || ( trimws (as.character (x) ) == "")) {
        y <- default
      } else 
        y <- x
      return ( y )
    }
    
    checkAllNumericNonNegativeIntegerWsumPositive <- function ( x ) {
      # check a text vector separated by commas is all non-negative numeric when commas are removed and at least one is positive
      # don't give any warning about non-numeric input values
      old.warn <- getOption ( "warn")
      options(warn = -1)  # don't print out 
      y <- as.numeric (SeparateStringByCommas ( x ))
      rslt <- ( all ( ! is.na ( y ) ) & is.wholenumber ( y ) & sum ( y ) > 0 )
      options(warn = old.warn ) # return "warn" to initial state
      return ( rslt )
    } 
    
    checkAllPositiveIntegers <- function ( x ) {
      # check a text vector separated by commas is all positive integers when commas are removed
      # don't give any warning about non-numeric input values
      old.warn <- getOption ( "warn")
      options(warn = -1)  # don't print out 
      y <- as.numeric (SeparateStringByCommas ( x ))
      rslt <- ( all ( ! is.na ( y ) ) & is.wholenumber ( y ) & all ( y ) > 0 )
      options(warn = old.warn ) # return "warn" to initial state
      return ( rslt )
    } 
    
    is.NonNegWholenumber <- function ( x ) {
      rslt <- ( all ( ! is.na ( x )) && is.wholenumber( x ) && all ( x >= 0 ))
      return ( rslt )
    }
    
    is.PositiveWholenumber <- function ( x ) {
      rslt <- ( all ( ! is.na ( x )) && is.wholenumber( x ) && all ( x > 0 ))
      return ( rslt )
    }
    checkNonNegWholenumber <- function ( x ) {
      old.warn <- getOption ( "warn")
      options(warn = -1)  # don't print out 
      y <- as.numeric (SeparateStringByCommas ( x ))
      rslt <- ( length ( y ) >= 1 & is.NonNegWholenumber ( y ) )
      options(warn = old.warn )
      return ( rslt )
    }
    
    checkUnique <- function ( x ) {
      return ( length ( x ) == length ( unique ( x )))
    }
    
    not.Completed <- function ( text , char = ":") {
      rslt <- FALSE
      if ( lastCharacter ( text ) == char )
        rslt <- TRUE
      return ( rslt )
    }
    
    ## new revised function 2020 01 31
    not.Appropriate <- function ( x ) {
      # not present OR not a valid non neg interger w at least 1 positive
      # cat ( "  Entered not.Appropriate section B\n")
      rslt <- (  ! checkAllNumericNonNegativeIntegerWsumPositive ( x ) )
      return ( rslt )
    } 
    
    #   new got rid of revised function 2020 01 31 also renamed from not.Right.Lengths  
    not.Matching.Lengths <- function ( x , y ) {
      # lengths of x and y when separated by commas are not the same
      # first argument is input$InterventionConditions which now need not have unique elements!
      x1 <- trimws (SeparateStringByCommas ( x ))
      y1 <- SeparateStringByCommas ( y )
      return (  not.Present ( x ) || ! checkUnique  ( x1 ) || not.Present ( y ) || length ( x1 ) != length ( y1 ))
    }
    
    ExpandOrContract <- function ( x , len ) {
      # expand a vector to len by copying last element or contract by truncating
      rslt <- x
      if ( length ( rslt ) < len ) {
        last <- rslt [ length ( rslt )]
        rslt <- c ( rslt , rep ( last , len - length ( rslt )  ))
      }
      if ( length ( rslt ) > len ) {
        rslt <- rslt [ 1 :  len ]
      }
      
      if ( len <= 0 ) 
        rslt <- NULL
      
      return ( rslt )
    }
    
    labelWithPluralName <- function ( text , Default ) {
      if ( length ( text ) == 0 ) {
        val <- paste0 ( Default , "s")
      } else 
        val <- paste0 ( text , "s")
      
      the.label <- paste ( "Please enter numeric", val , "Per Period\n(If different across Steps separate by ',')" )
      return ( the.label )
    }
    
    #  this isn't complete yet but need for general way to 
    #  pass truth status and text message to validate()
    
    TruthOf <- function ( obj , Quoted , conjunction = "and") {
      # evaluate the truth of whether obj satisfies Checks[[Quoted]]
      # use as first element of input to validate ()
      # input:  obj  input$SOMETHING
      #      :  Quoted     "SOMETHING"
      if ( conjunction == "and") {
        rslt <- TRUE  # 
      } else # this means "OR" so start with FALSE
        rslt <- FALSE # 
      
      
      for ( i in seq ( along = Checks [[ Quoted ]]))
        if ( conjunction == "and") {
          rslt <- rslt  & Clecks [[ Quoted ]][[ i ]] ( obj ) # check this!
        } else
          rslt <- rslt |  Clecks [[ Quoted ]][[ i ]] ( obj )
        
        return ( rslt ) 
        
    }
    
    makeFileName <- function ( AddDateAndTime , 
                               AssignmentPlotDirectory,
                               FileNameStem  ,
                               FileDescription ,
                               FileType ,
                               ReplaceSpace 
    ) {
      if ( AddDateAndTime  )
        AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
      else
        AddDateAndTime <- ""
      
      if ( trimws ( AssignmentPlotDirectory) == "" )
        slash <- ""
      else {
        if ( lastChar (AssignmentPlotDirectory ) != .Platform$file.sep )
          slash <- .Platform$file.sep
      }
      
      FileDescr <- FileDescription
      if ( ! not.Present ( FileDescr ))
        FileDescr <- paste0( "." , FileDescr )
      if ( ReplaceSpace ) { 
        FileDescr <- gsub ( " " , "_" , FileDescr)
        FileDescr <- gsub ( "\n" , "_" , FileDescr)
        FileDescr <- gsub ( "\t" , "_" , FileDescr)
      }
      
      
      rslt <- paste0 ( AssignmentPlotDirectory , slash , FileNameStem , AddDateAndTime , FileDescr , FileType )
      return ( rslt )
      
    }
    
    SimpleConcat <- function ( StartQuote = "" , obj , EndQuote = "" , sep = " " ) {
      # concatenate Start Quote, value of an object, EndQuote
      # use in validate ( , ?? )
      return ( paste ( StartQuote , obj , EndQuote , sep = "" ))
    }
    
    ValidationMessage <- function ( Quoted , Location = ValidMessage , Language = defaultLanguage ) {
      # Quoted - object name in quotes
      # Location - list that provides 
      What <- Location[[ Language ]][[ Quoted ]]$What # what information is needed
      Valid <- Location[[ Language ]][[ Quoted ]]$Valid # provide text describing valid value
      Where <- Location[[ Language ]][[ Quoted ]]$Where # where do you find this info
      
      return ( paste ( c (What , Valid, Where) , collapse = "\n"))
    }
    
    
    not.AllUnique <- function ( string  ) {
      cat ( paste ("Entered not.AllUnique string = ", string , "\n"))
      the.names <- SeparateStringByBoth( string )
      return ( ! all.unique ( the.names ))
    }
    
    has.DuplicateBeforeEnding <- function ( string ) {
      the.names <- SeparateStringByBoth( string )
      rslt <- FALSE
      if ( length ( the.names) > 1 && not.AllUnique ( string )) {
        dropLastName <- the.names [ - length ( the.names )]
        the.names2 <- paste ( dropLastName , collapse = ",")
        if ( not.AllUnique ( the.names2 ))
          rslt <- TRUE
      }
      return ( rslt )
    }
    
    CombineSteps <- function ( x , Default , n ) {
      # returns numeric vector of length n
      ans <- ExpandOrContract (as.numeric (SeparateStringByCommas ( copyValue ( x , Default ) )) , n )
      return( ans )
    }
    
    CombineStepsText <- function ( x , Default , n ) {
      # returns  vector of length n
      ans <- ExpandOrContract (SeparateStringByCommas ( copyValue ( x , Default ) ) , n )
      return( ans )
    }
    
    CombineSteps2 <- function ( x , Default , n ) {
      # returns NUMERIC vector of length n, separating out both : ,
      ans <- ExpandOrContract (as.numeric  ( SeparateStringByBoth ( copyValue ( x , Default ) )) , n )
      return( ans )
    }
    
    
    CombineStepsChar <- function ( x , Default , n ) {
      # returns character vector of length n, separating out : ,
      ans <- ExpandOrContract ( SeparateStringByBoth ( copyValue ( x , Default ) ) , n )
      return( ans )
    }
    
    
    StaggerStepsStart <- function ( x , Default , n ) {
      # return values of first columns nonblank for each step
      lowerX <- tolower (  SeparateStringByCommas( (x) ) ) 
      if ( length ( x ) == 0 || trimws (lowerX ) == "" ) { # default is stagger
        rslt <- 1 : n
      } else {  # length ( x ) > 0
        
        if ( length ( lowerX ) == 1 ) {  
          if ( lowerX %in% c( "no" , "n" , "t" , "true" , "0")) {
            rslt <- rep ( 1 , n )
            
          } else { 
            rslt <- 1 : n
          }
          
        } else {  # length is more than 1
          rslt <- CombineSteps ( lowerX , 1 , n )
        }
      }
      return ( rslt )
    }
    
    StaggerStepsCatchup <- function ( x , Default , StaggerInitialSteps, n ) {
      # 
      # return number of columns to leave blank at the end
      #  output:  0,0,...,0 -- fill out to the end
      #           set of numbers - fill out to these values
      #           NULL - don't do anything
      # gently allow all reasonable numeric values, yes, no true, false
      # if not.Present ( x )
      #   return NULL
      # if any positive numerical values, e.g., 3,2
      #   expand or contract to n and return
      # if length(x) == 1 and x == TRUE or 1 then the ending is staggerered  
      #   if StaggerInitialSteps not all 1 (no beginning stagger)
      #        nothing to do - return NULL  and don't do anything after this call
      #   else  return ( (n-1):0)
      # if length(x) == 1 and x == FALSE
      #   return rep(0,n) - fill in to the end
      
      # 
      
      lowerX <- tolower (  SeparateStringByCommas( (x) ) ) 
      
      if ( not.Present ( lowerX )) {
        rslt <- NULL
      } else {
        
        if ( length ( lowerX ) == 1 ) {  
          if ( lowerX %in% c( "no" , "n" , "f" , "false" , "0")) { # no staggering
            rslt <- rep ( 0 , n )
            
          } else {  # not a false or no value customized stagger
            if ( ! is.numeric ( lowerX )) {  # then it's true yes or blank  
              rslt <- ( (n - 1 ) :  0 )
            } else {  # length is more than 1 
              rslt <- CombineSteps ( lowerX , n , n )
            }
          }
        }
      }
      return ( rslt )
    }
    
    
    
    
    sortFixNull <- function ( x ) {
      z <- x [ x > 0 ]
      if ( length ( z ) > 0 ) {
        rslt <- sort( z )
      } else
        rslt <- NULL
      return ( rslt )
    }
    
    all.unique <- function ( x ) {
      rslt <- TRUE
      if ( length ( x ) > 0 )
        rslt <- ( length ( x ) == length ( unique( x )))
      return ( rslt)
    }
    
    lastCharacter <- function ( string ) {
      n <- str_length ( string )
      rslt <- str_sub ( string , start = n , end = n )
      return ( rslt )
    }
    
    NumberBlockCombinations <- function ( conditions, BlockLocations , onlyConditionsWithinBlocks ) {
      # return total product of number of block combinations
      nr <- 1  # number of rows as the product of all these
      # message ( paste ( "conditions = " , paste ( conditions, collapse = "\t") , "\n"))
      BlockSizes <- NumberBlockSizes ( conditions, BlockLocations , onlyConditionsWithinBlocks )
      for ( k in BlockLocations) {
        nr <- nr * BlockSizes [ k ]
      }
      return ( nr )
    }
    
    NumberBlockSizes <- function ( conditions, BlockLocations , onlyConditionsWithinBlocks ) {
      BlockSizes <- rep ( 1 , length ( conditions ))
      for ( k in BlockLocations) {
        
        BlockSizes [ k ] <- length ( onlyConditionsWithinBlocks [[ k ]])
        
      }
      return ( BlockSizes )
    }
    
    
    Repeat <- function ( conditions , periods , HasBlocks , BlockLocations ,
                         onlyConditionsWithinBlocks  )  {
      # input: conditions - only 1 value for each block, so a character string separated by ,
      # periods:  number of periods for each intervention Block
      # HasBlock: t/f
      # BlockLocations: which places start blocks with more than 1 value
      # onlyConditionsWithinBlocks: what are the intervention conditions within blocks w more than 1 value
      # 
      # output: returns either a vector or a matrix
      #         number of columns = sum ( periods )
      #         number of rows = product ( length (onlyConditionsWithinBlocks[j]))
      # do special work if HasBlocks
      # This requires conditions and periods to be same lengths
      if ( ! HasBlocks ) {
        rslt <- NULL
        for ( j in 1 : length (conditions) ) {
          rslt <- c ( rslt , rep ( conditions [ j ] , periods [ j ] ))
        }
        rslt <- matrix ( rslt , nrow = 1 , ncol = length ( rslt ) )
      } else {  # has blocks
        nr <- NumberBlockCombinations  ( conditions, BlockLocations , onlyConditionsWithinBlocks )
        BlockSizes <- NumberBlockSizes  ( conditions, BlockLocations , onlyConditionsWithinBlocks )
        
        # for ( uu in 1 : length ( BlockSizes ))
        #   message  ( paste ( "** uu =" , uu , "BlockSizes [ uu ] = ", BlockSizes [ uu ] , "\n") )  
        rslt <- matrix ( "" , nrow = nr  , ncol = sum ( periods ))
        
        startColumn <- 0
        endColumn <- 0
        for ( k in 1 : length ( conditions )) {  # actually over Blocks 
          
          startColumn <- endColumn + 1
          endColumn <- startColumn + periods [ k ] - 1
          
          
          if (! (k %in% BlockLocations )) {
            message  ( paste ( "** k =" , k , "\n") )  
            rslt [  , startColumn : endColumn  ] <-   rep ( conditions [ k ] , periods [ k ] )
            
          } else {  # k involves a block of interventions
            for ( m in 1 : length ( onlyConditionsWithinBlocks [[ k ]] ) ) { 
              message  ( paste ( "&~~ k =" , k , "m = " , m , "nr = " , nr , "BlockSizes [ k ] = " , BlockSizes [ k ] , "\n"))
              message  ( paste ( " WhichExpandedRows = ", paste ( WhichExpandedRows ( k , m , nr , BlockSizes ) , collapse = "\t" ) , "\n") )  
              WhichRows <- WhichExpandedRows ( k , m , nr , BlockSizes  )
              message  ( paste ( "~~**WhichRows =" , paste ( WhichRows , collapse = "\t" ) , "onlyConditionsWithinBlocks value = " , onlyConditionsWithinBlocks[[ k ]][ m ] , "periods value = " , periods [ k ] ,"\n") ) 
              rslt [ WhichRows , startColumn : endColumn  ] <-   rep ( onlyConditionsWithinBlocks [[ k ]][ m ] , periods [ k ] )
              
              
            } # end over m
          } # end over else -- k is a block
        }  # end over k
      }  # end HasBlock
      
      return ( rslt )
    }
    
    WhichExpandedRows <- function ( k , m , nr , BlockSizes ) {
      # From a subblock m within a Block or step k, return all the values of 
      #  rows needed to be filled across all intervention conditions
      # k - which Block
      # m - which value within kth Block
      
      
      NumberRepetitions <- nr  / BlockSizes [ k ]
      
      if ( k > 1 ) {
        AdjacentRepeats <- prod (  BlockSizes [ 1 : ( k -1 )] )
        
        skips <- AdjacentRepeats *  BlockSizes [ k ]
        
        if ( k == length (BlockSizes )) {
          NonAdjacentRepeats <- 0
        } else {
          NonAdjacentRepeats <- prod ( BlockSizes [ k : length ( BlockSizes )])
          
        }
        
        
        startRow <-  AdjacentRepeats * ( m - 1 ) + 1
        endRow <- startRow + AdjacentRepeats - 1
        
        
        beginFillin <- seq ( startRow , endRow )
        
        nbrReps <- NumberRepetitions / length ( beginFillin)
        
        rslt <- NULL
        startSeq <- beginFillin
        for ( mm in 1 : nbrReps ) {
          startSeq <- beginFillin + skips * ( mm - 1 )
          rslt <- c ( rslt , startSeq )
        }
        
        # checkLengthOK <- length ( rslt ) * BlockSizes [ k ] / nr  # should be 1
        
        
      } else { # k = 1
        
        Z <- nr / BlockSizes [ 1 ]
        rslt <- m + ( BlockSizes [ 1 ] ) * ( 0 : ( Z - 1 ))
      }
      
      
      return ( rslt ) 
    }
    
    FindTimePeriods <- function ( x ) {
      # x is vector of non-neg time element durations
      # convert these to time Periods and return a vector with the length being the largest time Period
      # if x [ j ] = 0 then return 0
      #  exampleFindTimePeriods  
      #  x =       2 5 0 4
      #  return    1 3 0 2
      MaxPeriod <- rank ( x, ties.method = "min" ) - sum ( x == 0 )
      MaxPeriod <- ifelse ( MaxPeriod < 0 , 0 , MaxPeriod )
      
      return ( MaxPeriod )
    }
    
    TimeElementsForPeriods <- function ( x ) {
      # x a vector of non neg time element durations, not necessary in any sequence
      # return how many time elements for each period ordered 
      #  example
      #  x =       2 5 0 4
      #  return    2 2 1, e.g., 2nd element goes through 3 time periods 
      y <- unique ( sort ( x [ x > 0 ]) )
      NumberTimeElements <- rep ( 0 , length ( y ))
      the.sum <- 0
      for ( i in 1 : length ( y )) {
        NumberTimeElements [ i ] <- y [ i ] - the.sum
        the.sum <- y [ i ]
      }
      
      return ( NumberTimeElements )
    }
    
    FirstBlankAfterLastCondition <- function ( x , condn ) {
      u <- ( x == condn )
      lastCondition <- max ( ( 1 : length ( x ) ) [ u ] )
      if ( lastCondition < length ( x ) && x [ lastCondition + 1 ] == "" ) {
        w <- lastCondition + 1 
      } else
        w <- 0  # error
      
      return ( w )
    }
    
    FillIn <- function ( firstPart , laterPart ) {
      #  change tx1 tx1 - - tx1 to tx1 tx1 tx1 tx1 tx1
      combine <- cbind ( firstPart , laterPart )
      if ( ncol ( combine ) >= 2 ) {
        for ( r in 1 : nrow ( combine )) {
          # check for pattern
          start <- 0
          end <- 0
          for ( k in 2 : ( ncol ( combine ) - 1 ) ) {
            if ( start == 0 & combine [ r , k ] == "" ) {
              start <- k
            }
            if ( start >= 2 &&  (combine [ r , k ] == combine [ r , (start - 1) ]) ) {
              end <- k - 1
            }
            
            if ( start >= 2 & end > start )
              combine [ r , start : end ] <- combine [ r , start - 1 ]
          }
        }
        
        return ( combine )
      }
      
    }
    
    ExpandSingleStep <- function ( tau , nr ) {
      rows.start <- nr * ( tau - 1 ) + 1
      rows.end <- nr * tau 
      return ( rows.start : rows.end )
    }
    
    RowsAccountingFor0Entities <- function ( NumberOfSteps , nr , EntitiesEveryStep ) {
      # returns matrix expanded rows for all steps
      rslt <- data.frame ( matrix  ( 0 , nrow = NumberOfSteps * nr , ncol = 3 ) )
      names( rslt ) <- c( "Step" , "Keep" , "SeqNumber" ) 
      
      rslt [ , "Keep"] <- ( EntitiesEveryStep > 0 )
      rslt [ , "SeqNumber"] <- cumsum( rslt [ , "Keep"])
      
      for ( tau in 1 : NumberOfSteps )
        
        the.Rows <- ExpandSingleStep ( tau , nr )
      rslt [  the.Rows , "Step" ] <- tau
      
    }
    
    ExpandBlockToSubblock <- function ( NumberOfSteps , nr , InterventionBlocksWhereStepsOccur , AssignmentMatrix , maxPeriods ) {
      # expand rows in assignment matrix to account for all possible combinations of subblocks
      #  nr is product of number of subblocks 
      #  note, some of these may have 0 units assigned, account for this later
      theRows <- nr * NumberOfSteps 
      if ( is.na (maxPeriods) | is.null ( maxPeriods ) )
        maxPeriods <- 0
      theCols <- max  ( c ( ncol ( AssignmentMatrix ) +  sum ( InterventionBlocksWhereStepsOccur) , maxPeriods ) )
      rslt <- matrix ( "", nrow = theRows , ncol = theCols )
      for ( tau in 1 : NumberOfSteps ) {
        cat ("tau = ",tau, "\n")
        startPeriod <- cumsum ( InterventionBlocksWhereStepsOccur) [ tau ] + 1 
        pr(startPeriod)
        endingPeriod <- startPeriod + ncol ( AssignmentMatrix ) - 1
        pr(endingPeriod)
        rslt [ ExpandSingleStep ( tau , nr ) , startPeriod :  endingPeriod ] <- AssignmentMatrix 
        pr(rslt )
      }
      
      
      return ( rslt )
    }
    
    ExpandBlockToSubblock0 <- function ( NumberOfSteps , nr , InterventionBlocksWhereStepsOccur , AssignmentMatrix  ) {
      # expand rows in assignment matrix to account for all possible combinations of subblocks
      #  nr is product of number of subblocks 
      #  note, some of these may have 0 units assigned, account for this later
      theRows <- nr * NumberOfSteps 
      
      theCols <-  ncol ( AssignmentMatrix ) +  sum ( InterventionBlocksWhereStepsOccur) 
      rslt <- matrix ( "", nrow = theRows , ncol = theCols )
      for ( tau in 1 : NumberOfSteps ) {
        pr( tau )
        startPeriod <- cumsum ( InterventionBlocksWhereStepsOccur) [ tau ] + 1 
        pr ( startPeriod )
        endingPeriod <- startPeriod + ncol ( AssignmentMatrix ) - 1
        pr ( endingPeriod )
        
        rslt [ ExpandSingleStep ( tau , nr ) , startPeriod :  endingPeriod ] <- AssignmentMatrix 
        pr ( rslt )
      }
      
      return ( rslt )
    }
    
    makeYlabel <- function ( the.name ,  values ) {
      
      if ( length ( values ) > 1  ) {
        if ( var ( values ) > 0 ) {
          zz <- paste0 ( "[" , paste ( values , collapse = ",") , "]"  )
        } else
          zz <-  paste ( length(values) , "*" , values[ 1 ] ,"For"  )
      } else
        zz <- values [ 1 ]
      the.text <- paste ( the.name , " (", zz , " Each Step)", collapse = "," )
      return ( the.text )
    }
    
    getNonBlank <- function ( matrix  , fctn = min ) {
      firstNonBlankIntvn <- rep (NA , nrow ( matrix ))
      for ( i in 1 : nrow ( matrix )) {
        all.nonBlank <- ( matrix [ i , ] != "")
        whichCol <- fctn (( 1 : ncol ( matrix ) )[ all.nonBlank ])
        firstNonBlankIntvn [ i ] <- matrix [ i , whichCol ]
      }
      return ( firstNonBlankIntvn )
    }
    
    LocateNonBlank <- function ( matrix , fctn ) {
      # return column location of first or last nonblank
      #  fctn is either min for first or max for last
      whichCol <- rep (NA , nrow ( matrix ))
      for ( i in 1 : nrow ( matrix )) {
        all.nonBlank <- ( matrix [ i , ] != "")
        whichCol [ i ] <- fctn (( 1 : ncol ( matrix ) )[ all.nonBlank ])
      }
      return ( whichCol )
    }
    
    BlankColumns <- function ( matrix , StartEnd  ) {
      # return list of size nrow(matrix) of columns with blank values either at beginning or end
      # StartEnd = "start" use for stagger start
      # StartEnd = "end" use for catch up at the end
      if ( StartEnd == "start") {
        NonBlank <- getNonBlank ( matrix , min )
        FirstNonBlankLocation <- LocateNonBlank ( matrix , min )
      } else {
        NonBlank <- getNonBlank ( matrix , max )
        LastNonBlankLocation <- LocateNonBlank ( matrix , max )
      }
      
      
      blanksCols <- list ()
      
      for ( i in 1 : nrow ( matrix )) {
        
        blanksCols [[ i ]]  <- ( 1 : ncol ( matrix ) )[ matrix [ i , ] == ""]
        if ( StartEnd == "start")   {
          blanksCols [[ i ]] <- blanksCols [[ i ]] [ blanksCols [[ i ]] < FirstNonBlankLocation [ i ] ] 
        } else
          blanksCols [[ i ]] <- blanksCols [[ i ]] [ blanksCols [[ i ]] > LastNonBlankLocation [ i ] ] 
      }
      
      return ( blanksCols)
    }
    
    
    # ValidMessage.English
    
    
    
    ValidMessage <- list ( )
    ValidMessage [[ "English"]] <- list ()
    ValidMessage [[ "English"]] [[ "NumberOfSteps"]] <- list()
    
    ValidMessage [[ "English"]] [[ "NumberOfSteps"]]$What <- "Please enter Number of Steps "
    
    ValidMessage [[ "English"]] [[ "NumberOfSteps"]]$Valid <- "Integer >= 2"
    
    ValidMessage [[ "English"]] [[ "NumberOfSteps"]]$Where <- "(Input for Design Figure/Numbers in the Design/Total Number of Steps...)"
    
    ValidMessage [[ "Spanish" ]] <- list ()
    ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]] <- list()
    
    ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]]$What <- "(Spanish) Please enter Number of Steps "
    
    ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]]$Valid <- "Integer >= 2"
    
    ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]]$Where <- "(Input for Design Figure/Numbers in the Design/Total Number of Steps...)"
    
    
    ValidMessage [[ "English"]] [[ "InterventionConditions"]] <- list()
    
    ValidMessage [[ "English"]] [[ "InterventionConditions"]]$What <- "Required: Please enter Name(s) of Intervention/Implementation Condition(s) in order of appearance"
    
    ValidMessage [[ "English"]] [[ "InterventionConditions"]]$Valid <- "Enter as Text; if more than 1 separate names by ','"
    
    ValidMessage [[ "English"]] [[ "InterventionConditions"]]$Where <- "Location ASSIGN: Names/ Name(s) of Intervention...)"
    
    
    ValidMessage [[ "Spanish" ]] <- list ()
    ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]] <- list()
    
    ValidMessage [[ "Spanish" ]] [[ "InterventionConditions"]]$What <- "(Spanish ) Required: Please enter Name(s) of Intervention(s)/Implementation(s) in order of appearance"
    
    ValidMessage [[ "Spanish" ]] [[ "InterventionConditions"]]$Valid <- "Enter as: Text(s), if more than 1 then separate names by ','"
    
    ValidMessage [[ "Spanish" ]] [[ "InterventionConditions"]]$Where <- "Location: (Input for Design Figure/Setup Design Names/Name(s) of Intervention...)"
    
    
    # Run first validations
    
    validate(
      need(  ! is.null (input$NumberOfSteps) && input$NumberOfSteps >= 2 & is.wholenumber (input$NumberOfSteps ) , 
             ValidationMessage ( "NumberOfSteps" , Language = copyValue  (input$Language, defaultLanguage ) ) )
    )
    
    
    
    NumberOfSteps <- copyValue (input$NumberOfSteps, 4 )  # detaults to 4 if missing
    
    
    
    
    
    
    # Note, default number of time periods is NumberOfSteps
    
    default.NumberOfTimePeriodsDuringRollout <- NumberOfSteps
    
    ## Retrieve Intervention Names
    
    validate(
      need( ! not.Present (input$InterventionConditions), 
            ValidationMessage ( "InterventionConditions" , Language = copyValue  (input$Language, defaultLanguage ) ) )
    )
    
    validate(
      need( ! not.Present (input$InterventionConditions) && 
              length ( SeparateStringByBoth (input$InterventionConditions ))  <= length ( the.colors ), 
            "Too many interventions, shorten or extend number of colors under Controls" )
    )
    
    InterventionConditions <- input$InterventionConditions
    Intervention.Blocks <- SeparateStringByCommas (input$InterventionConditions )
    All.Intervention.Conditions <- SeparateStringByBoth (input$InterventionConditions ) 
    
    validate(
      need( not.Present (input$NumberPeriodsEachCondition) ||
              ( ! not.Present (input$NumberPeriodsEachCondition) && 
                  ! not.Appropriate ( SeparateStringByBoth  ( input$NumberPeriodsEachCondition  ) ) )  , 
            "Please enter Non-Negative Number(s) of Time Periods for Each Condition\n(Single Numeric or Separate by Commas and Colons for Blocking)")
    )
    
    
    # Expand or Contract NumberPeriodsEachCondition to match InterventionConditions
    NumberPeriodsEachCondition.String <- ReplaceColonCommaPositions  ( InterventionConditions , input$NumberPeriodsEachCondition , 1 )
    NumberPeriodsEachCondition <- as.numeric (SeparateStringByBoth (NumberPeriodsEachCondition.String))
    
    sepBlocks <- SeparateStringByCommas ( NumberPeriodsEachCondition.String )
    firstWithinBlock <- list ()
    the.Block.Lengths <- list ()
    the.Block <- list ( )
    Numeric.Intervention.Blocks <- NULL
    lastN <- 0
    for ( nblock in 1 : length ( sepBlocks)) {
      the.Block [[ nblock ]] <- SeparateStringByCharacter ( sepBlocks [ nblock ] , ":")
      the.Block.Lengths [[ nblock ]] <- length ( the.Block [[ nblock ]] )
      firstWithinBlock [[ nblock ]] <-  the.Block [[ nblock ]] [ 1 ]
      numeric.text <- paste0 ( ((1 : the.Block.Lengths[[ nblock ]]) + lastN) , collapse = ":")
      Numeric.Intervention.Blocks <-  c ( Numeric.Intervention.Blocks , numeric.text )
      lastN <- lastN + the.Block.Lengths[[ nblock ]]
    }
    NumberPeriodsEachBlock <- as.numeric ( unlist ( firstWithinBlock ))
    
    #Replace all the names with numbers, ignoring ones that are repetitions
    Numeric.All.Intervention.Conditions <- 1 : length ( All.Intervention.Conditions ) # needed if names not all unique e.g. aba design
    
    
    
    
    
    NumberOfInterventionBlocks <- length( Intervention.Blocks )
    NumberOfAll.Intervention.Conditions <- length ( All.Intervention.Conditions )
    
    
    onlyConditionsWithinBlocks <- list()  # initialize, keep blank if no blocks of conditions
    NumericonlyConditionsWithinBlocks <- list ( )
    BlockLocations <- NULL  # vector of all intervention.block locations where there is a block of more than 1 condition
    NumberOfBlockContrasts <- 0
    productOfBlockSizes <- 1
    if ( NumberOfAll.Intervention.Conditions == NumberOfInterventionBlocks ) {
      HasBlocks <- FALSE
      BlockLocations <- NULL
      for ( i in 1 : length ( Intervention.Blocks ))
        onlyConditionsWithinBlocks [[ i ]] <- ""
    } else {
      HasBlocks <- TRUE
      
      for ( i in 1 : length ( Intervention.Blocks)) {
        splitUpBlock <- trimws (SeparateStringByCharacter ( Intervention.Blocks [ i ] , ":"))
        NumericSplitUpBlock <- SeparateStringByCharacter ( Numeric.Intervention.Blocks [ i ] , ":")
        if ( length ( splitUpBlock ) > 1 ) {
          NumberOfBlockContrasts <- NumberOfBlockContrasts  + 1
          BlockLocations <- c( BlockLocations , i )
          onlyConditionsWithinBlocks [[ i ]] <- splitUpBlock
          NumericonlyConditionsWithinBlocks [[ i ]] <- NumericSplitUpBlock
          productOfBlockSizes <- productOfBlockSizes * length ( splitUpBlock )
        }
      }
      
    }
    
    nr <- NumberBlockCombinations  ( Intervention.Blocks, BlockLocations , onlyConditionsWithinBlocks )
    
    
    validate(
      need( not.Present (input$EntitiesPerStep) || ( ! not.Present (input$EntitiesPerStep)  && 
                                                       (! not.Appropriate ( SeparateStringByBoth  ( input$EntitiesPerStep   ) ) ) )  , 
            "Please enter Non-Negative Number(s) of Entities Per Step\n(Single Numeric or Separate by Commas and Colons for Blocking)")
    )
    
    # Make this into a vector of the number of entities in every step
    EntitiesEveryStep <- CombineSteps2  ( input$EntitiesPerStep , 1 , NumberOfSteps * productOfBlockSizes )
    
    
    TimeElementName <- copyValue ( input$TimeElementName , defaultTimeElementName )
    
    
    
    validate(
      need( not.Present (input$TimeElementsPerPeriod) ||
              (!not.Present ( input$TimeElementsPerPeriod ) &&
                 length (input$TimeElementsPerPeriod ) == 1 ) , 
            labelWithPluralName (TimeElementName , "Time Element" ) )
    )
    
    # need to wait until later to correctly assign TimeElementsPerPeriod 
    
    
    
    validate(
      need( checkNonNegWholenumber ( as.numeric ( SeparateStringByCommas  ( input$InterventionBlocksWhereStepsOccur  ) ) )  , 
            "Please enter Number(s) of Time Periods Between Steps\n(Single Numeric >= 0 or Separate by Commas)")
    )
    
    
    
    
    
    validate(
      need( trimws ( input$StaggerInitialSteps )  == "" ||
              length ( SeparateStringByCommas  ( input$StaggerInitialSteps  ) ) == 1 || 
              ( is.numeric ( SeparateStringByCommas  ( input$StaggerInitialSteps  ) ) && 
                  checkNonNegWholenumber ( as.numeric ( SeparateStringByCommas  ( input$StaggerInitialSteps  ) ) ) )  , 
            "Error, Are Start Periods Staggered...: Please Enter 'y/n' or Numerics >= 0 Separated by Commas for each Step)")
    )
    
    StaggerInitialSteps <- ExpandOrContract ( StaggerStepsStart (input$StaggerInitialSteps , "yes" , NumberOfSteps ) , NumberOfSteps )
    
    validate(
      need( not.Present ( input$CatchUpEndingSteps)  ||
              length ( SeparateStringByCommas  ( input$CatchUpEndingSteps  ) ) == 1 || 
              ( is.numeric ( SeparateStringByCommas  ( input$CatchUpEndingSteps ) ) &&
                  checkNonNegWholenumber ( as.numeric ( SeparateStringByCommas  ( input$CatchUpEndingSteps  ) ) ) )  , 
            "Error, ADJUST ENDING: Are Ending Periods Staggered...:Please Enter 'y/n' or Numerics >= 0 Separated by Commas for each Step)")
    )
    
    
    CatchUpEndingSteps <- StaggerStepsCatchup (input$CatchUpEndingSteps , "yes" , StaggerInitialSteps , NumberOfSteps )
    
    
    validate ( need ( not.Present (input$MaxTimePeriodsInStudy)  ||
                        (! is.na (as.numeric ( input$MaxTimePeriodsInStudy)) &
                           as.numeric ( input$MaxTimePeriodsInStudy) >= 1) ,
                      "Error, enter the Last Time Period to display on assignment plot, either blank or > 0" ) )
    
    pr (! not.Present (input$MaxTimePeriodsInStudy) )
    pr ( !is.na ( as.numeric (input$MaxTimePeriodsInStudy ) ) )
    if ( !  not.Present (input$MaxTimePeriodsInStudy) && !is.na ( as.numeric (input$MaxTimePeriodsInStudy ) ) ) {
      MaxTimePeriodsInStudy <- as.numeric (input$MaxTimePeriodsInStudy)
      validate ( need ( MaxTimePeriodsInStudy >= 1 ,
                        "Error, enter Last Time Period >= 1"))
    } else
      MaxTimePeriodsInStudy <- NA
    
    pr ( MaxTimePeriodsInStudy )
    
    validate ( need ( not.Present (input$MinStartTimePeriodInStudy) || 
                        ! is.na (as.numeric ( input$MinStartTimePeriodInStudy) ) ,
                      "Error, enter Starting Time Period either blank or numeric (may be negative)" ) )
    
    
    if ( not.Present( input$MinStartTimePeriodInStudy) ) {
      MinStartTimePeriodInStudy <- 0
    } else
      MinStartTimePeriodInStudy <- as.numeric ( input$MinStartTimePeriodInStudy)
    
    if ( !not.Present ( MaxTimePeriodsInStudy) && !not.Present  (MinStartTimePeriodInStudy  ))
      validate ( need (MaxTimePeriodsInStudy > MinStartTimePeriodInStudy ,
                       "Last Time Period to Display Must be > First Time Period to Display (ASSIGN Save Plot/---Change Plot Labels and Dimensions/Set the Last First ...)"))
    ##
    #   Periods - chronological time sequence, width in Time Elements can vary
    #      Total.Periods = PeriodsDuringRollout  + PeriodsDuringStaggerBaseline +
    #                     PeriodsDuringCatchUpEnding 
    #      Period.TimeElementSizes -- vector of length Total.Periods 
    #                       = TimeElementsPerPeriod + ...
    #      Total.TimeElements <- sum ( Period.TimeElementSizes )
    ##
    #   Periods and Time Elements, work outwards from the Steps to assign interventions
    #    
    #    Stagger Beginning                                    Steps        Catch Up Ending     
    #Times                        TimeElementsEveryStep                 
    #        max(StaggerInitialSteps)                                             max(CatchUpEndingSteps)
    # Periods # pos incrs              NumberPeriodsEachBlock      0/1           Periods # pos incrs   
    #  #unique (StaggerInitialSteps )
    
    ##     interventions c , d , e 
    
    #     (staggerStart == FALSE)   step    {StaggerCatch up == FALSE}         
    #    c  c   c    d  d  d  e  e   e   e  {e} {e} {e}    
    #   (c)  c  c    c  d  d  d  e   e   e  {e} {e} {e} 
    #   (c) (c) c    c  c  d  d  d   e   e   e  {e} {e}
    #   (c) (c) (c)  c  c  c  d  d   d   e   e   e   e   
    
    
    #  
    # Start with Step Section and work outwards
    
    #  NumberPeriodsEachBlock - minimum number of each block of intervention condition (during step section )
    
    lastCondition <- Intervention.Blocks [ length ( Intervention.Blocks ) ]
    NumericlastCondition <- Numeric.Intervention.Blocks [ length ( Numeric.Intervention.Blocks ) ]
    
    if ( NumberOfSteps > 1 ) {
      
      # start at 0
      InterventionBlocksWhereStepsOccur <- c  ( 0 , CombineSteps( input$InterventionBlocksWhereStepsOccur , 1 , NumberOfSteps - 1 ))# 
    } else  
      InterventionBlocksWhereStepsOccur <- 0 
    
    pr( InterventionBlocksWhereStepsOccur )
    PeriodsDuringRollout <- sum ( NumberPeriodsEachBlock ) + sum(InterventionBlocksWhereStepsOccur)  # how many periods for step section to complete
    
    
    
    TimeElementsPerPeriod <- CombineSteps ( input$TimeElementsPerPeriod , 1 , NumberOfSteps ) # expand/contract to periods during rollout
    pr ( TimeElementsPerPeriod )
    #   
    #  1. Calculate Periods during Rollout 
    #
    
    
    
    #   This is a count of all different rows with non-zero entities
    NumberOfNonVacuousRows <- sum ( EntitiesEveryStep > 0 )
    
    # run through creation of CombinedRollout matrix with numbers referring to intervention conditions
    AssignmentMatrix <- Repeat ( Numeric.Intervention.Blocks , NumberPeriodsEachBlock , HasBlocks , BlockLocations ,
                                 NumericonlyConditionsWithinBlocks  )
    
    AssignmentRollout <- ExpandBlockToSubblock0 ( NumberOfSteps , nrow ( AssignmentMatrix) , InterventionBlocksWhereStepsOccur , AssignmentMatrix  ) 
    pr("ExpandBlockToSubblock0")
    pr(AssignmentRollout)
    
    EntitiesEveryNonZeroStep <- EntitiesEveryStep [ EntitiesEveryStep > 0 ] # use later
    
    NumberFinalrows <-  ( EntitiesEveryStep > 0 ) # 
    
    FirstNonBlankIntvn <- getNonBlank ( AssignmentRollout , fctn = min )
    LastNonBlankIntvn <- getNonBlank ( AssignmentRollout , fctn = max )
    
    
    CombinedRollout <- AssignmentRollout
    
    totalRows <- nrow ( CombinedRollout  )
    
    #  2. Calculate Periods Stagger Baseline  -- 
    
    if ( length ( StaggerInitialSteps ) > 1 ||
         ! ( length ( StaggerInitialSteps ) == 1 && 
             StaggerInitialSteps[ 1 ]   %in% c( "yes" , "y" , "t" , "true" ) ) )  {
      
      
      startPeriod.Step <-  StaggerInitialSteps 
      
      theBlankCols <- BlankColumns ( CombinedRollout , "start"  )  # list of initial blanks
      
      for ( tau in 1 : NumberOfSteps ) {
        the.Rows <- ExpandSingleStep ( tau , nr )
        
        ### fixed
        starting <- max ( startPeriod.Step[ tau ] , 1 ) 
        
        LastBeginningBlank <- 0
        if ( length ( theBlankCols [[ ( tau - 1 ) * nr + 1 ]] ) > 0 )
          LastBeginningBlank <-  max ( theBlankCols [[ ( tau - 1 ) * nr + 1 ]])
        
        if ( starting  > 0 && LastBeginningBlank  > 0 && starting <= LastBeginningBlank )
          CombinedRollout [ the.Rows , (starting : LastBeginningBlank )  ]  <- FirstNonBlankIntvn [ the.Rows  ]
        
        
      }
      
    }
    
    
    #  get time elements and periods
    
    
    
    TimeElementsPerPeriod <- ExpandOrContract ( TimeElementsPerPeriod , PeriodsDuringRollout )
    pr ( PeriodsDuringRollout)
    pr(TimeElementsPerPeriod)
    
    Total.Time.Periods <- PeriodsDuringRollout   # number of periods in total design
    
    Total.TimeElements <- sum ( TimeElementsPerPeriod ) # number of time elements in total design
    
    Period.TimeElementSizes <- TimeElementsPerPeriod
    
    # calculate number of time elements for each step
    TimeElementsEveryStep <- ExpandOrContract( Period.TimeElementSizes  , Total.Time.Periods  )
    
    
    
    #  3. Calculate Periods catch up
    
    # if any positive values: 
    
    pr ( CatchUpEndingSteps )
    
    if ( length ( CatchUpEndingSteps ) > 1 ||
         ! ( length ( CatchUpEndingSteps ) == 1 && 
             CatchUpEndingSteps[ 1 ]  %in% c( "yes" , "y" , "t" , "true" ) ) )  { 
      
      
      endPeriod.Step <-  ncol ( CombinedRollout ) -   CatchUpEndingSteps 
      
      
      endPeriod.Step <- ifelse ( endPeriod.Step < 1 , 1 , endPeriod.Step )
      
      pr ( endPeriod.Step)
      
      theBlankCols <- BlankColumns ( CombinedRollout , "end"  )  # list of initial blanks
      
      pr ( theBlankCols )
      for ( tau in 1 : NumberOfSteps ) {
        the.Rows <- ExpandSingleStep ( tau , nr )
        
        ending <-  endPeriod.Step [ tau ] 
        
        FirstEndingBlank <- Total.Time.Periods + 1 
        if ( length ( theBlankCols [[ ( tau - 1 ) * nr + 1  ]] ) > 0 ) 
          FirstEndingBlank <- min ( theBlankCols [[ ( tau - 1 ) * nr + 1  ]]) 
        
        
        if ( FirstEndingBlank  <= Total.Time.Periods &&  ending  <= Total.Time.Periods &&  FirstEndingBlank  <= ending ) {
          pr ( tau )
          pr ( FirstEndingBlank)
          pr ( ending)
          pr (LastNonBlankIntvn [ the.Rows  ] )
          CombinedRollout [ the.Rows , (FirstEndingBlank  : ending )  ]  <- LastNonBlankIntvn [ the.Rows  ]
          pr ( CombinedRollout )
        } else {
          if ( ( FirstEndingBlank == Total.Time.Periods + 1 )  && ending < Total.Time.Periods ) {
            CombinedRollout [ the.Rows , ( ( ending + 1)  : Total.Time.Periods )  ]  <- ""
          }
          
        }
        
      }
    }
    
    #  4. Get time Periods and Time Elements, allowing expansion or contraction
    #     depending on MaxTimePeriodsInStudy
    
    if ( ! not.Present ( MaxTimePeriodsInStudy) ) {
      if ( ncol ( CombinedRollout) > MaxTimePeriodsInStudy ) { 
        CombinedRollout <- CombinedRollout [ , 1 : MaxTimePeriodsInStudy ]
        Period.TimeElementSizes <- Period.TimeElementSizes[1 : MaxTimePeriodsInStudy ]
        cutcolumns <- "cut columns"
        pr (cutcolumns)
        pr ( CombinedRollout )
      }
      else { 
        nbr <- MaxTimePeriodsInStudy - ncol ( CombinedRollout)
        if ( nbr > 0 ) {
          
          extraBlank <- matrix ( "" , nrow = nrow ( CombinedRollout ) ,
                                 ncol = nbr  )
          CombinedRollout <- cbind ( CombinedRollout , extraBlank )
          
          
        }
      }
      lineNbr <- 2152
      pr(lineNbr)
      pr(CombinedRollout)
      # recalculate
      PeriodsDuringRollout <- ncol ( CombinedRollout )
      pr (PeriodsDuringRollout )
      TimeElementsPerPeriod <- CombineSteps ( input$TimeElementsPerPeriod , 1 , PeriodsDuringRollout ) # expand/contract to periods during rollout
      pr (TimeElementsPerPeriod )
      Total.TimeElements <- sum ( TimeElementsPerPeriod )
    }
    
    #5.  Add Additional Steps if this is chosen - fill in this code later.
    #    Split Blocks, making separate rows with different interventions but same timing
    ##  Need to revise EntitiesEveryStep when there is a split
    NumberOfDifferentRows <- nrow ( CombinedRollout) 
    
    #6. Produce matrix of time elements w interventions, expanding from periods to time elements
    Condition.Time.Entities <- matrix ( "" , nrow = totalRows  , ncol = Total.TimeElements )
    pr (  Period.TimeElementSizes)
    
    for ( tt in 1 : totalRows ) {
      startVal <- 1
      pr ( tt )
      pr ( startVal)
      for ( k in 1 : length ( Period.TimeElementSizes ) ) {
        pr ( k )
        pr ( Period.TimeElementSizes)
        
        Condition.Time.Entities [  tt , ( startVal : ( startVal + Period.TimeElementSizes [ k ] - 1 ))] <- CombinedRollout [ tt , k ] 
        startVal <- startVal + Period.TimeElementSizes [ k ]
        pr (startVal )
      }
    }
    
    
    pr ( Condition.Time.Entities )
    # Text in the plot
    
    xAxis <- 0 :  Total.TimeElements 
    
    validate ( need ( is.null (input$maxXLabelValues ) ||
                        trimws ( input$maxXLabelValues ) == "" ||
                        is.PositiveWholenumber (input$maxXLabelValues)  ,
                      "Error: max number of X label values must be > 0 ") )
    
    if ( length (input$maxXLabelValues ) > 0 && trimws ( input$maxXLabelValues ) != "") {
      maxXLabelValues <- input$maxXLabelValues
    } else
      maxXLabelValues <- default.maxXLabelValues
    
    theXlength <- min ( Total.TimeElements + 1 , maxXLabelValues )
    xAxis <- round (seq ( 0 , Total.TimeElements , length.out = theXlength))
    
    xAxis.labelVals <- xAxis
    
    theTimeNames <- SeparateStringByCommas ( input$NamesOfEachTimePoint  )
    if ( any ( trimws (theTimeNames) != "" ) ) {
      theTimeNames.length <- length ( theTimeNames )
      if ( theTimeNames.length < theXlength )
        xAxis.labelVals <- c ( theTimeNames , xAxis [ (theTimeNames.length + 1) : theXlength  ])
    } 
    
    
    
    yAxis <- nr * ( 1 : NumberOfSteps ) 
    
    
    yAxis.labelVals <-  ( NumberOfSteps : 1 ) 
    NamesOfEachStep <- SeparateStringByCommas ( input$NamesOfEachStep )
    nFill <- NumberOfSteps - length (NamesOfEachStep )
    if ( any (NamesOfEachStep != "") &&  nFill <= 0 ) {
      yAxis.labelVals <- rev ( NamesOfEachStep [ 1 : NumberOfSteps ] )
    } else {
      if ( any (NamesOfEachStep != "") && nFill > 0   )
        yAxis.labelVals <- rev ( c( NamesOfEachStep , rep ( "" , nFill )) )
    }   
    
    
    if ( input$AssignmentPlotYaxisLabel != "" ) {
      ylabText <- input$AssignmentPlotYaxisLabel
    } else
      ylabText <- makeYlabel (copyValue ( input$EntityName , defaultEntityName ) , EntitiesEveryStep )
    
    
    if ( all (TimeElementsPerPeriod == 1) ) {
      xlabText <- paste ( "Time," , TimeElementsPerPeriod [ 1 ] , TimeElementName, "Per Time Period" )
    } else {
      if ( var ( TimeElementsPerPeriod ) > 0 ) {
        rest <- paste ( TimeElementsPerPeriod , collapse = ",")
      } else
        rest <- TimeElementsPerPeriod [ 1 ]
      xlabText <- paste ( "Time," , rest , paste0 (TimeElementName, "s"), "Per Period" )
    }
    
    
    the.main <- paste ( "Rollout Design\n" , NumberOfSteps , "Steps in" , Total.TimeElements )
    if ( length ( TimeElementName) > 0 ) {
      the.main <- paste ( the.main  , paste0 (TimeElementName, "s"))
    } else
      the.main <- paste ( the.main , "Times" )
    
    
    UseGGPlot <- TRUE 
    if ( ! UseGGPlot ) { 
      
      plot ( xAxis , xAxis , ylim = range ( yAxis ),  type = "n" , axes = FALSE , 
             main = the.main  ,
             xlab = xlabText , 
             ylab = makeYlabel (copyValue ( input$EntityName , defaultEntityName) , 
                                CombineStepsChar (  input$EntitiesPerStep , 1, NumberOfSteps ) )
      )
      
      axis ( 1 , at = xAxis , labels = xAxis.labelVals , pos = NA )
      axis ( 2 , at = yAxis , labels = yAxis.labelVals , pos = NA )
    }
    
    
    TimesCondition <- list()
    minVal <- matrix ( NA , nrow = totalRows , ncol = NumberOfAll.Intervention.Conditions  )
    maxVal <- matrix ( NA , nrow = totalRows , NumberOfAll.Intervention.Conditions )
    
    
    for ( tau in 1 :  NumberOfSteps ) {
      for ( nrCount in 1 : nr ) {
        indx <- ( tau - 1 ) * nr + nrCount
        TimesCondition [[ indx ]] <- list ()
        
        for ( j in 1 :  NumberOfAll.Intervention.Conditions ) {
          
          TimesCondition [[ indx ]] [[ j ]] <- ( 1 :  Total.TimeElements ) [ Condition.Time.Entities [ indx , ] == as.character(Numeric.All.Intervention.Conditions [ j ] ) ]
          if ( length (TimesCondition [[ indx ]] [[ j ]]) > 0 ) {
            minVal[ indx , j ] <- min (TimesCondition [[ indx ]] [[ j ]] , na.rm = TRUE) 
            maxVal [ indx , j ] <- max (TimesCondition [[ indx ]] [[ j ]] , na.rm = TRUE) 
          } 
          
          
          if ( ! is.na ( minVal [ indx , j ] )) {
            
            if ( EntitiesEveryStep [ indx ] > 0 )
              if ( ! UseGGPlot )
                lines ( c ( minVal [ indx , j ] -1  , maxVal [ indx , j ]  )  , rep ( totalRows - indx + 1 , 2 ) , col = the.colors [ j ] , 
                        lwd = LineWidth * EntitiesEveryStep [ indx ] / mean (EntitiesEveryStep  ) , lend = 2 )
          }
        }
      } # end InterventionConditions
      
    } # end Steps
    
    
    # helpful vertical lines to emphasize time changes
    
    for ( tau in 1 :  NumberOfSteps ) {
      if ( NumberOfAll.Intervention.Conditions >= 2 ) {
        for ( j in 1 :  ( NumberOfAll.Intervention.Conditions )  ) {
          if ( ! UseGGPlot )
            lines ( c( maxVal [ tau , j ]   , maxVal [ tau , j ]  ) , c( 0 , NumberOfSteps - tau + 1 ) , lty = 3 )
        }
      }
    }
    
    # legend (  c(5,8) , c( length(All.Intervention.Conditions) -2 , -2 ) , 
    #        legend = All.Intervention.Conditions ,
    #         fill =  the.colors [ 1 : length ( All.Intervention.Conditions )] , bty = "n")
    
    
    if (  UseGGPlot ) {
      
      
      # Simple Design ggplot.R
      # provides legend with lines at the bottom by default
      
      xStart <- as.vector ( minVal -1 )
      yStart <- as.vector ( NumberOfDifferentRows - row ( minVal ) + 1 ) 
      xEnd <- as.vector ( maxVal )
      yEnd <- yStart 
      arrange.colors <-  as.vector ( the.colors [ col ( minVal ) ])
      the.width <- EntitiesEveryStep [ as.vector  ( row ( minVal ) ) ] * LineWidth / mean ( EntitiesEveryStep)
      
      qq <- All.Intervention.Conditions [ col (minVal)]
      
      pr ( xEnd)
      
      
      # if MaxTimePeriodsInStudy < number of periods, then chop off these from plot
      
      if ( !is.na (MaxTimePeriodsInStudy) ) {
        # add last dashed line but remove values if they are > MaxTimePeriodsInStudy
        smallerThan <- ( xEnd <=  MaxTimePeriodsInStudy )
      } else 
        smallerThan <- rep ( TRUE , length ( xEnd ))
      
      xStartA <- xStart [ smallerThan ]
      yStartA <- yStart [ smallerThan ]
      xEndA <- xEnd [ smallerThan ]
      yEndA <- yEnd [ smallerThan ]
      
      arrange.colorsA <- arrange.colors [ smallerThan ]
      
      the.widthA <- the.width [ smallerThan ]
      
      qqA <- factor ( qq [ smallerThan ] , levels = unique ( qq [ smallerThan ] ) )
      
      pr ( MaxTimePeriodsInStudy )
      pr(xStartA)
      pr ( xStart )
      pr ( xEnd)
      pr ( xEndA )
      pr ( arrange.colorsA )
      pr ( the.widthA )
      pr ( qqA )
      
      uniqueInterventions <- unique ( All.Intervention.Conditions )
      
      pr ( unique ( All.Intervention.Conditions ) )
      
      mapToUniqueInterventionNumber <- match ( All.Intervention.Conditions , uniqueInterventions  )
      
      df.Interventions <- data.frame ( xStart = xStartA , 
                                       yStart = yStartA ,
                                       xEnd = xEndA  ,
                                       yEnd =  yEndA ,
                                       arrange.colors = arrange.colorsA ,
                                       width = the.widthA ,
                                       theIntervention = qqA )
      
      
      if ( input$AssignmentPlotXaxisLabel != "" ) {
        xlabText <- input$AssignmentPlotXaxisLabel 
      } else { 
        if ( all (TimeElementsPerPeriod == 1) ) {
          xlabText <- paste ( "Time," , TimeElementsPerPeriod [ 1 ] , TimeElementName, "Per Time Period" )
        } else {
          if ( var ( TimeElementsPerPeriod ) > 0 ) {
            rest <- paste ( TimeElementsPerPeriod , collapse = ",")
          } else
            rest <- TimeElementsPerPeriod [ 1 ]
          
          xlabText <- paste ( "Time," , rest , paste0 (TimeElementName, "s"), "Per Period" )
        }
      }
      
      
      if ( input$AssignmentPlotTitle != "" ) {
        the.main <- input$AssignmentPlotTitle
      } else {
        
        the.main <- paste ( "Rollout Design\n" , NumberOfSteps , "Steps in" , Total.TimeElements )
        if ( length ( TimeElementName) > 0 ) {
          the.main <- paste ( the.main  , paste0 (TimeElementName, "s"))
        } else
          the.main <- paste ( the.main , "Times" )
      }
      
      # remove any intervention.conditions that have missing start or end values
      pr ( df.Interventions )
      
      Missing <- is.na ( df.Interventions [ , c ( "xStart" , "yStart" , "xEnd" , "yEnd")] ) 
      
      
      MissingVector <- apply ( Missing , 1 , any ) | df.Interventions [ , "width"] == 0
      dfNoNA <- df.Interventions [  ! MissingVector ,  ] 
      
      pr ( dfNoNA )
      
      if ( not.Present ( input$WhereToPutLegend )) {
        WhereToPutLegend <- default.WhereToPutLegend
      } else 
        WhereToPutLegend <- input$WhereToPutLegend
      
      
      #  draw vertical guides 
      
      xStartb <- NULL
      xEndb <- NULL
      yStartb <- NULL
      yEndb <- NULL
      
      NumberofNonZeroEntities <- sum ( EntitiesEveryStep > 0)
      
      
      indx2 <- 0
      for ( tau in 1 :  NumberOfSteps ) {
        for ( nrCount in 1 : nr ) {
          indx <- ( tau - 1 ) * nr + nrCount
          if ( EntitiesEveryStep [ indx ] > 0 )  {
            
            indx2 <- indx2 + 1 
            for ( j in 1 :  ( NumberOfAll.Intervention.Conditions)  ) {
              xStartb <- c( xStartb , maxVal [ indx , j ] )
              xEndb <- c( xEndb , maxVal [ indx , j ]  )
              yStartb <- c ( yStartb , 0 )
              yEndb <- c ( yEndb , NumberofNonZeroEntities - indx2 + 1 )
            }
          }
        }
      }
      
      pr(MaxTimePeriodsInStudy)
      if ( !is.na (MaxTimePeriodsInStudy) ) {
        # add last dashed line but remove values if they are > MaxTimePeriodsInStudy
        smallerThan <- ( xEndb <=  MaxTimePeriodsInStudy )
        pr ( smallerThan )
        xStartb <- c( xStartb [smallerThan ] , MaxTimePeriodsInStudy )
        xEndb <- c( xEndb [smallerThan ] , MaxTimePeriodsInStudy  )
        yStartb <- c ( yStartb [smallerThan ] , 0 )
        yEndb <- c ( yEndb [smallerThan ] , NumberOfSteps )
      }
      
      if ( !is.na (MinStartTimePeriodInStudy) ) {
        # add last dashed line
        xStartb <- c( MinStartTimePeriodInStudy , xStartb  )
        xEndb <- c( MinStartTimePeriodInStudy , xEndb   )
        yStartb <- c ( 0 , yStartb  )
        yEndb <- c ( NumberOfSteps  , yEndb )
      }
      
      pr(xStartb)
      pr(xEndb)
      pr (yStartb)
      pr (yEndb)
      
      
      VerticalGuides <- data.frame ( xStartb = xStartb ,
                                     xEndb = xEndb ,
                                     yStartb = yStartb ,
                                     yEndb = yEndb )
      
      pr ( VerticalGuides)
      
      MsgVert <- is.na ( VerticalGuides [ , c( "xStartb" , "xEndb")] )
      MissingVecVert <- apply ( MsgVert , 1 , any )
      
      VerticalGuidesNoNA <- VerticalGuides [ ! MissingVecVert , ]
      
      pr( VerticalGuidesNoNA)
      
      BiggerSize <- 15
      
      
      b <- ggplot( dfNoNA   ) +
        theme_classic ( base_size = BiggerSize )  + labs ( title = the.main , x = xlabText , y = ylabText ) +
        geom_segment(data = dfNoNA , mapping = aes(  x = xStart , y = yStart, xend = xEnd, yend = yEnd , col = theIntervention  , 
                                                     size =  width  ) , lineend = "butt" ) +
        geom_segment ( data = VerticalGuidesNoNA , aes ( x = xStartb , y = yStartb, xend = xEndb, yend = yEndb ), 
                       linetype = 3 ) +
        theme ( plot.title = element_text ( hjust = 0.5 , size = BiggerSize ) ,
                axis.title.x = element_text ( size = BiggerSize ) ,
                axis.title.y = element_text ( size = BiggerSize )  ) +
        scale_y_continuous ( labels =  yAxis.labelVals  , breaks = yAxis ) +
        scale_x_continuous ( labels = xAxis.labelVals , breaks = xAxis  ) +
        theme(legend.position=  WhereToPutLegend ) +
        
        # the following doesn't produce legend, there is one when left out
        
        theme(legend.key.width = unit(3 ,"cm")) +
        
        scale_color_manual(values= the.colors [ 1 : length ( uniqueInterventions)]) +
        scale_size("size" , guide = "none") +
        
        theme(legend.text=element_text(size= BiggerSize )) +
        theme(legend.direction='vertical') + 
        labs( color = 'Implementation Conditions'  ) + 
        guides(colour = guide_legend(override.aes = 
                                       list(color = 
                                              factor  (the.colors [ 1 : length ( uniqueInterventions)])  ,
                                            linetype = 1 , size = 4 ))) 
      
      
      print ( b )
      
      
      
      
      
    }
    
    
    
    
    pr ( input$StaggerInitialSteps )
    pr ( StaggerInitialSteps )
    
    pr ( input$CatchUpEndingSteps )
    pr ( CatchUpEndingSteps )
    
    
    pr ( AssignmentRollout)
    pr ( CombinedRollout )
    pr ( Total.TimeElements )
    pr ( Condition.Time.Entities )
    
    
    ### run simulation here with input values 
    
    ### display result matrix 
    
    # save current values of input
    
    inputCheck <- list ()
    for ( i in names ( input )) {
      inputCheck [[ i ]] <- input [[ i ]]
    }
    
    LatestInputValueFile <- makeFileName ( TRUE , 
                                           input$AssignmentPlotDirectory,
                                           FileNameStem =   InputValueFileStem ,
                                           FileDescription = input$AssignmentPlotTitle ,
                                           FileType = InputValueFileType , 
                                           ReplaceSpace = TRUE )
    
    #  save (inputCheck , file = LatestInputValueFile )
    
    
    
    
  })  ###:  Close AssignmentPlot ----
  
  
  
}  ### Close server ----

################################################################################
# New Server
#  For now, just prints out the list provided by the input

server <- function(input, output){
  # --- Tab Functionality ----------
  ## --- Next
  observeEvent(input$tab1_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  observeEvent(input$tab1a_next, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  ## --- Back
  observeEvent(input$tab1a_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1")
  })
  observeEvent(input$tab2_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1")
  })
  observeEvent(input$tab2a_back, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2")
  })
  ## --- Advanced
  observeEvent(input$advanced_input, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab1a")
  })
  observeEvent(input$advanced_viz, {
    updateTabsetPanel(inputId = "input_tabs", selected = "tab2a")
  })
  
  # --- Dynamic UI Elements ----------
  output$intervention_names <- renderUI({
    n_int = as.integer(input$n_interventions)
    if(is.na(n_int) | n_int<1){
      return(helpText("Number of interventions is invalid"))
    }
    lapply(1:as.integer(input$n_interventions),
           function(i) {
             textInput(
               inputId = paste0("intervention_name_", i),
               label = paste0("Name of intervention ", i),
               value = NA_character_,
               placeholder = paste0("Intervention ", i)
             )
           })
  })
  # --- Temp for Testing ----------
  # Returns a text of all input values
  output$input_list <- renderText({
    input_names = names(reactiveValuesToList(input))
    out_string = ""
    for(name in input_names){
      out_string = paste0(out_string, name, ": ", input[[name]], "\n")
    }
    out_string
  })
}
