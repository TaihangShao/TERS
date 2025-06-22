options(encoding = "UTF-8")
options (warn = -1)
server <- function(input, output, session) {
  
  
  ### Move to the tabs from the first tab
  observeEvent(input$FCSubmit, {
    updateTabsetPanel(session, "tabs", selected = "import")
  })
  
  ## load example data
  # observeEvent(input$FCvi, {
  #   updateTabsetPanel(session, "tabs", selected = "sde8")
  # })
  
  ### copy citation
  
  # observeEvent(input$FCcitation,{
  #   write_clip("")
  # })
  
  ###### All the helpers
  observe_helpers(help_dir = "help_files")
  
  ###### Stop R if Shiny window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ###### Dynamic variables
  values <- reactiveValues()
  # values <- reactiveValues(task_id=0)
  task_id <- reactiveVal(0)
  task_ssm <- reactiveVal(0)
  task_fp <- reactiveVal(0)
  task_rcs <- reactiveVal(0)
  task_rp <- reactiveVal(0)
  task_gam <- reactiveVal(0)
  task_pmm <- reactiveVal(0)
  task_mcm <- reactiveVal(0)
  
  
  ## Menu
  output$task_menu <- renderMenu({
    dropdownMenu(type = "tasks", badgeStatus = "success", headerText="Running progress", .list = list(
      # 使用task_value()作为taskItem的值
      taskItem(value = task_id(), text = "Input data", color = "aqua"),
      taskItem(value = task_ssm(), text = "Standard survival model", color = "red"),
      taskItem(value = task_fp(), text = "Fractional polynomials", color = "orange"),
      taskItem(value = task_rcs(), text = "Restricted cubic splines", color = "yellow"),
      taskItem(value = task_rp(), text = "Royston-Parmar models", color = "green"),
      taskItem(value = task_gam(), text = "Generalized additive models", color = "aqua"),
      taskItem(value = task_pmm(), text = "Parametric mixture models", color = "yellow"),
      taskItem(value = task_mcm(), text = "Mixture cure models", color = "red")
    ))
  })
  
  ###### Import page
  
  # Upload button
  observe({
    infile <- input$upload
    if (!is.null(infile)){
      tryCatch({
        values$sheet1 <- read_excel(infile$datapath, sheet = 1)[,1:3]
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      #   output$errorlog <- renderText({
      #   print("There is one or more errors in the Excel file. Try modifying the data and re-importing it.")
      # })
      )
    }
  })
  
  observeEvent(input$FCexample, {
    # Load example file
    wd <- getwd()
    infile <- c()
    infile$datapath <- paste0(wd,"/www/files/example.xlsx")

    if (!is.null(infile)){
      tryCatch({
        values$sheet1 <- read_excel(infile$datapath, sheet = 1)[,1:3]
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }

  })
  
  # Sheet selector
  observeEvent(input$upload, {
    output$dropdown <- renderUI({
      selectInput("ddm", "Select data",
                  choices = c("IPD input" = "sheet1"
                  )) 
    })
  })
  
  # Sheet selector
  observeEvent(input$FCexample, {
    output$dropdown <- renderUI({
      selectInput("ddm", "Select data",
                  choices = c("IPD input" = "sheet1"
                  )
      )
    })
  })
  
  #Show the dataframes
  observeEvent(input$ddm,{
    output$upl_outDT <- renderDT(datatable(eval(parse(text = paste0("values$", input$ddm))), editable = FALSE, rownames = FALSE, 
                                           options = list(pageLength = 10)
    ))
  })
  
  # Save the changes made to the survival data
  observeEvent(input$upl_outDT_cell_edit, {
    if (!is.null(input$ddm)){
      row <- input$upl_outDT_cell_edit$row
      col <- input$upl_outDT_cell_edit$col+1
      temp <- eval(parse(text = paste0("values$", input$ddm)))
      x <- input$upl_outDT_cell_edit$value
      temp[row, col] <- as.numeric(x)
      values$sheet1 <- temp
    }
  })
  
  #### process the data --- processdatabutton
  observeEvent(input$FCexample, {
    output$processdatabutton <- renderUI({
      actionButton("ready3", "Process the data", icon("gears"), 
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### process the data button
  observeEvent(input$upload, {
    output$processdatabutton <- renderUI({
      actionButton("ready3", "Process the data", icon("gears"), 
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### process
  observeEvent(input$ready3, {
    gef<-values$sheet1
    gef<-data.frame(gef$event,gef$time)
    gef<-rename(gef,"censrec"="gef.event","recyrs"="gef.time")
    gef$recyrs<-gef$recyrs/12
    gef$rectime<-gef$recyrs*365
    gef$rectime2 <- as.integer(gef$rectime/(365.24/12)) + 1
    ltgef <- lifeTable(gef, timeColumn = "rectime2", eventColumn = "censrec")
    ltHaz <- data.frame(hazKM = ltgef$Output$hazard, Time = (seq(1:length(ltgef$Output[,1]))-0.5)/12,
                        AtRisk = ltgef$Output$atRisk, Events = ltgef$Output$events)
    ltHaz$hazLT = ltHaz$Events / (ltHaz$AtRisk - ltHaz$Events/2)
    ltHaz$lnTime <- log(ltHaz$Time)
    ltHaz$MyId <- 1:dim(ltHaz)[1] # Generate id variable
    ltHaz$EventsL <- lag(ltHaz$Events)
    ltHaz$EventsL[1] <- 0
    ltHaz$surv <- ltgef$Output$S
    ltHaz$timedelta<-ltHaz$Time[2]-ltHaz$Time[1]
    
    
    ##data for sd
    sd_gef<-data.frame("time"=gef$recyrs,"event"=gef$censrec)
    values$sd<-sd_gef
    
    output$folup <- renderText(round(length(ltHaz$Events)/12,2))
    output$lthaz <- renderDT(datatable(ltHaz, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    # output$Newtime <- renderDT(datatable(Newtime, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    values$ltHaz<-ltHaz
    values$folup<-round(length(ltHaz$Events)/12,2)
    values$ssm_input<-gef
    disable("ready3")
    
    task_id(50)
    
    # output$task_info <- renderUI({
    #   value <- values$task_id
    #   taskItem(value = value, text = "Input data", color = "blue")
    # })

  })
  ### text input
  observeEvent(input$ready3, {
    output$WarningMyTHinput<-renderUI({
      html_string <- paste("Warning! This number should be higher than: <span style='color: red;'>", values$folup, "</span>")
      HTML(html_string)})
    output$MyTHinput <- renderUI({
      textInput("MyTH", "Time horizon (years)", value=10)
    })
    output$MyStepinput <- renderUI({
      textInput("MyStep", "Number of observations per year (Suggest value: 12)",value=12)
    })
  })
  ### limit input
  observeEvent(input$MyTH,{
    MyTH =  as.numeric(input$MyTH)
    if (MyTH>values$folup){
      enable("updatedata")
    }else{
      disable("updatedata")}
  })
  ### updata button
  observeEvent(input$ready3, {
    output$updatedata1 <- renderUI({
      actionButton("updatedata", "Update", icon("arrow-up"), 
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### update data
  observeEvent(input$updatedata, {
    MyTH =  as.numeric(input$MyTH)    # Time Horizon (years)
    MyStep =  as.numeric(input$MyStep)    # Number of obs. per year
    ltHaz<-values$ltHaz
    follow_up <- length(ltHaz$Events)
    MyN <- MyTH*MyStep # Total time points (observed & extrapolated)
    values$MyN<-MyN
    
    Newtime <- data.frame(Time = seq(from=1/MyStep, to=MyTH, by=1/MyStep), AtRisk = 1)
    Newtime$MyId <- 1:dim(Newtime)[1]
    Newtime$MyId <- ifelse(Newtime$MyId > follow_up, follow_up, Newtime$MyId)  # Random effects: Using last observed ID for extrapolation
    Newtime$EventsL <- 0
    Newtime$EventsL[1:follow_up] <- lag(ltHaz$Events)
    Newtime$EventsL[1] <- 0
    Newtime$EventsL <- ifelse(Newtime$MyId > follow_up, 0, Newtime$EventsL) # AR: Using last observed event count for extrapolation
    Newtime$timedelta<-Newtime$Time[2]-Newtime$Time[1]
    Newtime$lnTime<-log(Newtime$Time)
    values$Newtime<-Newtime
    output$Newtime <- renderDT(datatable(values$Newtime, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    shinyalert(title = "Data has been updated", type = "success")
    
    Newtime2 <- data.frame(Time = seq(from=1/MyStep, to=30, by=1/MyStep), AtRisk = 1)
    Newtime2$MyId <- 1:dim(Newtime2)[1]
    Newtime2$MyId <- ifelse(Newtime2$MyId > follow_up, follow_up, Newtime2$MyId)  # Random effects: Using last observed ID for extrapolation
    Newtime2$EventsL <- 0
    Newtime2$EventsL[1:follow_up] <- lag(ltHaz$Events)
    Newtime2$EventsL[1] <- 0
    Newtime2$EventsL <- ifelse(Newtime2$MyId > follow_up, 0, Newtime2$EventsL) # AR: Using last observed event count for extrapolation
    Newtime2$timedelta<-Newtime2$Time[2]-Newtime2$Time[1]
    Newtime2$lnTime<-log(Newtime2$Time)
    values$Newtime2<-Newtime2
    
    task_id(100)
    
    # output$task_info <- renderUI({
    #   value <- values$task_id
    #   taskItem(value = task_id(), text = "Input data", color = "blue")
    # })
    
  })
  
  

  
  ### ssm button
  observeEvent(input$updatedata, {
    output$goonbutton1 <- renderUI({
      actionButton("ready1", "1.Standard survival model", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to ssm
  observeEvent(input$ready1, {
    updateTabItems(session, "tabs", "sde1")
  })
  ### fp button
  observeEvent(input$updatedata, {
    output$goonbutton2 <- renderUI({
      actionButton("ready2", "2.Fractional polynomials", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to fp
  observeEvent(input$ready2, {
    updateTabItems(session, "tabs", "sde2")
  })
  ### RCS button
  observeEvent(input$updatedata, {
    output$goonbutton3 <- renderUI({
      actionButton("readyrcs", "3.Restricted cubic splines", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to RCS
  observeEvent(input$readyrcs, {
    updateTabItems(session, "tabs", "sde3")
  })
  
  ### RP button
  observeEvent(input$updatedata, {
    output$goonbutton4 <- renderUI({
      actionButton("readyrp", "4.Royston-Parmar models", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to RP
  observeEvent(input$readyrp, {
    updateTabItems(session, "tabs", "sde4")
  })
  
  ### GAM button
  observeEvent(input$updatedata, {
    output$goonbutton5 <- renderUI({
      actionButton("readygam", "5.Generalized additive models", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to GAM
  observeEvent(input$readygam, {
    updateTabItems(session, "tabs", "sde5")
  })
  
  ### PMM button
  observeEvent(input$updatedata, {
    output$goonbutton6 <- renderUI({
      actionButton("readypmm", "6.Parametric mixture models", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to PMM
  observeEvent(input$readypmm, {
    updateTabItems(session, "tabs", "sde6")
  })
  
  ### MCM button
  observeEvent(input$updatedata, {
    output$goonbutton7 <- renderUI({
      actionButton("readymcm", "7.Mixture cure models", icon("play"),
                   style="color: #fff;
                       background-color: #218838; border-color: #2e6da4")
    })
  })
  ### move to MCM
  observeEvent(input$readymcm, {
    updateTabItems(session, "tabs", "sde7")
  })
  
  ### VI button
  observeEvent(input$updatedata, {
    output$goonbutton8 <- renderUI({
      actionButton("readyvi", "8.Visual inspection", icon("play"),
                   style="color: black;
                       background-color: darkorange; border-color: #2e6da4")
    })
  })
  ### move to VI
  observeEvent(input$readyvi, {
    updateTabItems(session, "tabs", "sde8")
  })
  
  #### survival model ##
  #----
  ### ssm run model button
  observeEvent(input$updatedata, {
    output$runmodelssm1 <- renderUI({ 
      actionButton("runmodelssm", label="Run the model",icon ("play"),style="color: #fff;
                       background-color: skyblue; border-color: #2e6da4")
    })
  })
  ### run ssm
  
  observeEvent(input$runmodelssm, {
    mystep<-as.numeric(input$MyStep)
    if (input$ssm_gf == TRUE){
      nmd<-7
      MyDists <- list("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma")
      
    } else{
      nmd<-8
      MyDists <- list("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","genf")
      
    }
    dfGOF <- data.frame(matrix(nrow=nmd, ncol=6))
    colnames(dfGOF) <- c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC")
    # myLnL <- array(dim=nmd)
    # myAIC <- array(dim=nmd)
    dfHazEst <- array(dim=c(nmd, values$MyN))
    ltHaz<-values$ltHaz
    dfHazEst2 <- array(dim=c(nmd, length(ltHaz$Time)))
    dfHazEst_ssmsum <- array(dim=c(nmd, mystep*30))
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    llCons <- sum(ltHaz$Events*log(ltHaz$AtRisk) - log(factorial(ltHaz$Events)))
    values$llCons<-llCons
    temp_input<-values$ssm_input
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running Standard Survival Model",
      "Please wait while running the model...",
      footer = NULL
    ))
    for (i in 1:nmd){
      glmTemp <- flexsurvreg(Surv(recyrs, censrec) ~ 1, data = temp_input, dist = MyDists[[i]])
      dfHazEst[i,] <- summary(glmTemp, t=exp(Newtime$lnTime), type="hazard")[[1]]$est/mystep
      dfHazEst2[i,] <- summary(glmTemp, t=ltHaz$Time, type="hazard")[[1]]$est/mystep
      dfHazEst_ssmsum[i,] <- summary(glmTemp, t=exp(Newtime2$lnTime), type="hazard")[[1]]$est/mystep
      # myLnL[i] <- glmTemp$loglik
      # myAIC[i] <- glmTemp$AIC
      dfGOF[i,1] <- MyDists[[i]]
      dfGOF[i,2] <- round(sum(ltHaz$Events*log(dfHazEst2[i,]) - dfHazEst2[i,]*ltHaz$AtRisk) + llCons,2)
      dfGOF[i,3] <- glmTemp$npars
      coeff_temp<-as.data.frame(glmTemp$coefficients)
      t1<-"Param"
      for (j in 1:length(glmTemp$coefficients)) {
        t0<-paste(rownames(coeff_temp)[j],round(coeff_temp[j,1],2),sep = "=")
        t1<-paste(t1,t0,sep = ";")
      }
      dfGOF[i,4] <- t1
      progress$set(value = ((i) / nmd))
      Sys.sleep(0.5)
    }
    progress$close()
    removeModal()
    
    dfGOF$AIC <- -2*dfGOF$'Log-Likelihood' + 2*dfGOF$Parameters
    dfGOF$BIC <- -2*dfGOF$'Log-Likelihood' + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters
    
    SRVres <- dfGOF
    SRVres <- arrange(SRVres, AIC)
    best_ssm_AIC<-SRVres$Model[[1]]
    SRVres <- arrange(SRVres, BIC)
    best_ssm_BIC<-SRVres$Model[[1]]

    output$best_ssm1<-renderUI({
      html_best_ssm1 <- paste("The standard survival model with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", best_ssm_AIC, "</span>")
      HTML(html_best_ssm1)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_ssm2<-renderUI({
      html_best_ssm2 <- paste("The standard survival model with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", best_ssm_BIC, "</span>")
      HTML(html_best_ssm2)
    })

    # dfGOF<-values$dfGOF_ssm
    output$ssm_aic <- renderDT(datatable(dfGOF, editable = FALSE, rownames = FALSE,
                                         colnames = c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC"),
                                         options = list(dom = 'tp')))
    values$ssm_dfhazard1<-dfHazEst
    values$ssm_dfhazard2<-dfHazEst2
    values$dfHazEst_ssmsum<-dfHazEst_ssmsum
    shinyalert(title = "Complete!", type = "success")
    
    output$downloadtable_ssm1 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfGOF, file,row.names = FALSE)
      }
    )
    
    task_ssm(50)
    
  })
  ### draw plot button
  observeEvent(input$runmodelssm, {
    output$plotssm1 <- renderUI({ 
      actionButton("plotssm", label="Draw the plot",icon ("play"),style="color: #fff;
                       background-color: skyblue; border-color: #2e6da4")
    })
  })
  ### ssm plot
  observeEvent(input$plotssm, {
    mystep<-as.numeric(input$MyStep)
    if (input$ssm_gf == TRUE){
      nmd<-7
    } else{
      nmd<-8
    }
    Newtime2<-values$Newtime2
    Newtime<-values$Newtime
    ltHaz<-values$ltHaz
    dfHazEst<- as.data.frame(values$ssm_dfhazard1)
    dfHazEst_ssmsum<- as.data.frame(values$dfHazEst_ssmsum)
    
    dfHaz <- t(dfHazEst)
    dfHaz_ssmsum <- t(dfHazEst_ssmsum)
    
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz_ssmsum <- cbind(data.frame(Newtime2$lnTime), dfHaz_ssmsum)
    
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz_ssmsum$Time <- exp(as.numeric(dfHaz_ssmsum$Newtime2.lnTime))
    
    dfHaz <- as.data.frame(dfHaz[,-1])
    dfHaz_ssmsum <- as.data.frame(dfHaz_ssmsum[,-1])
    
    if (input$ssm_gf == TRUE){
      colnames(dfHaz) <- c("Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Time")
      colnames(dfHaz_ssmsum) <- c("Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Time")
    } else{
      colnames(dfHaz) <- c("Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Generalized F","Time")
      colnames(dfHaz_ssmsum) <- c("Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Generalized F","Time")
    }
    
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = (nmd+1)))
    dfSurv[[1]]<-dfHaz$Time
    for ( i in 2:(nmd+1) ) {
      dftemp<-data.frame(dfHaz$Time,dfHaz[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv[[i]]<-dftemp[[4]]
    }
    
    dfSurv_ssmsum<-as.data.frame(matrix(nrow = length(dfHaz_ssmsum$Time),ncol = (nmd+1)))
    dfSurv_ssmsum[[1]]<-dfHaz_ssmsum$Time
    for ( i in 2:(nmd+1) ) {
      dftemp<-data.frame(dfHaz_ssmsum$Time,dfHaz_ssmsum[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv_ssmsum[[i]]<-dftemp[[4]]
    }
    
    if (input$ssm_gf == TRUE){
      colnames(dfSurv)<-c("Time","Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma")
      colnames(dfSurv_ssmsum)<-c("Time","ssm_Exponential","ssm_Weibull","ssm_Gamma","ssm_Log-Normal","ssm_Gompertz","ssm_Log-Logistic","ssm_Generalized Gamma")
    } else{
      colnames(dfSurv)<-c("Time","Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Generalized F")
      colnames(dfSurv_ssmsum)<-c("Time","ssm_Exponential","ssm_Weibull","ssm_Gamma","ssm_Log-Normal","ssm_Gompertz","ssm_Log-Logistic","ssm_Generalized Gamma","ssm_Generalized F")
    }
    dfFigSurv = dfSurv %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    dfFigSurv_ssmsum = dfSurv_ssmsum %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    values$ssm_dfHaz<-dfHaz
    values$ssm_dfSurv<-dfSurv
    values$ssm_dfFigSurv<-dfFigSurv
    values$ssm_dfFigSurv_in<-dfFigSurv_in
    values$dfFigSurv_ssmsum<-dfFigSurv_ssmsum
    
    output$plot_ssm<-renderPlot({
      
      if (input$time1 == 1 & input$model1 == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          # facet_wrap(~Model,nrow=4)+
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$time1 == 2 & input$model1 == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          # facet_wrap(~Model,nrow=4)+
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$time1 == 1 & input$model1 %in% c("Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Generalized F")){
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Model==input$model1), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          # facet_wrap(~Model,nrow=4)+
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$time1 == 2 & input$model1 %in% c("Exponential","Weibull","Gamma","Log-Normal","Gompertz","Log-Logistic","Generalized Gamma","Generalized F")){
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Model==input$model1), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          # facet_wrap(~Model,nrow=4)+
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }
      f_surv1<-f_surv1 + ggtitle(input$ggtitle)
      print(f_surv1)
      values$graph1<-f_surv1
    })
    
    output$downloadtable_dfhaz_ssm <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_dfsurv_ssm <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    task_ssm(100)

  })
  ### download plot
  observeEvent(input$dwld, {
    output$titlebut <- renderUI({
      textInput("ggtitle", "Title", value = input$tipo)
    })
  })
  
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld, {
    output$sizebut <- renderUI({
      bt <- tagList()
      bt[[1]] <- numericInput("height", "Height (px)", value = 1600)
      bt[[2]] <- numericInput("width", "Width (px)", value = 2800)
      bt
    })
  })
  
  # Download back-end
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph1, width = as.numeric(input$width),
             height = as.numeric(input$height),
             units = "px")
    }
  )
  
  #----
  ### fp ###
  #----
  ### run fp button
  observeEvent(input$updatedata, {
    output$runmodelfp1 <- renderUI({ 
      actionButton("runmodelfp", label="Run the model",icon ("play"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")
    })
  })  
  ### run fp
  observeEvent(input$runmodelfp,{
    mystep<-as.numeric(input$MyStep)
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime    
    Newtime2<-values$Newtime2
    dfHazEst_fpsum <- array(dim=c(44, mystep*30))
    
    #-----FP1 -----
    myLnL <- array(dim=8)
    myAIC <- array(dim=8)
    myBIC <- array(dim=8)
    fpindex=1
    MyPowers <- list(c(-2,-1,-0.5,0.5,1,2,3))
    for (i in 1:7){
      glmTemp <- glm (cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
      myLnL[i] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
      myAIC[i] <- extractAIC(glmTemp)[2]
      myBIC[i] <- -2*myLnL[i] + log(length(ltHaz$Time))*extractAIC(glmTemp)[1]
      dfHazEst_fpsum[fpindex,]<-predict(glmTemp, newdata=Newtime2, type="response")
      fpindex=fpindex+1
    }
    ### run for 0
    glmTemp <- glm (cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    myLnL[8] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[8] <- extractAIC(glmTemp)[2]
    myBIC[8] <- -2*myLnL[8] + log(length(ltHaz$Time))*extractAIC(glmTemp)[1]
    dfHazEst_fpsum[fpindex,]<-predict(glmTemp, newdata=Newtime2, type="response")
    fpindex=fpindex+1
    
    FP1res <- data.frame(c("-2","-1","-0.5","0.5","1","2","3","0"))
    FP1res <- cbind(FP1res,round(myLnL,2),round(myAIC,2),round(myBIC,2))
    colnames(FP1res) <- c("Powers","LnL","AIC","BIC")
    FP1res <-arrange(FP1res,AIC)
    FP1_pow1 <- as.numeric(FP1res[1,1])
    FP1res <-arrange(FP1res,BIC)
    FP1_pow2 <- as.numeric(FP1res[1,1])
    
    output$best_fp11<-renderUI({
      html_best_fp11 <- paste("The FP1 with the minimum AIC: power =  <span style='color: red;border-color: #2e6da4'>", FP1_pow1, "</span>")
      HTML(html_best_fp11)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_fp12<-renderUI({
      html_best_fp12 <- paste("The FP1 with the minimum BIC: power =  <span style='color: red;border-color: #2e6da4'>", FP1_pow2, "</span>")
      HTML(html_best_fp12)
    })
    
    #-----FP2 -----
    myLnL <- array(dim=36)
    myAIC <- array(dim=36)
    MyPowers <- list(c(-2,-1,-0.5,0.5,1,2,3))
    index <- 1
    for (i in 1:7){
      for (j in 1:7){
        if (j > i) {
          glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^MyPowers[[1]][j])+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
          myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
          myAIC[index] <- extractAIC(glmTemp)[2]
          myBIC[index] <- -2*myLnL[index] + log(length(ltHaz$Time))*extractAIC(glmTemp)[1]
          index <- index + 1
          dfHazEst_fpsum[fpindex,]<-predict(glmTemp, newdata=Newtime2, type="response")
          fpindex=fpindex+1
        }
      }
    }
    for (i in 1:7) {
      glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^MyPowers[[1]][i]*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
      myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
      myAIC[index] <- extractAIC(glmTemp)[2]
      myBIC[index] <- -2*myLnL[index] + log(length(ltHaz$Time))*extractAIC(glmTemp)[1]
      index <- index + 1
      dfHazEst_fpsum[fpindex,]<-predict(glmTemp, newdata=Newtime2, type="response")
      fpindex=fpindex+1
    }
    
    for (i in 1:7) {
      glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^0*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
      myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
      myAIC[index] <- extractAIC(glmTemp)[2]
      myBIC[index] <- -2*myLnL[index] + log(length(ltHaz$Time))*extractAIC(glmTemp)[1]
      index <- index + 1
      dfHazEst_fpsum[fpindex,]<-predict(glmTemp, newdata=Newtime2, type="response")
      fpindex=fpindex+1
    }
    
    glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + I(Time^0*lnTime*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
    myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[index] <- extractAIC(glmTemp)[2]
    myBIC[index] <- -2*myLnL[index] + log(length(ltHaz$Time))*extractAIC(glmTemp)[1]
    dfHazEst_fpsum[fpindex,]<-predict(glmTemp, newdata=Newtime2, type="response")
    fpindex=fpindex+1
    
    FP2res <- data.frame(c("-2,-1","-2,-0.5","-2,0.5","-2,1","-2,2","-2,3","-1,-0.5","-1,0.5","-1,1","-1,2","-1,3","-0.5,0.5","-0.5,1","-0.5,2","-0.5,3",
                           "0.5,1","0.5,2","0.5,3","1,2","1,3","2,3","-2,-2","-1,-1","-0.5,-0.5","0.5,0.5","1,1","2,2","3,3",
                           "-2,0","-1,0","-0.5,0","0.5,0","1,0","2,0","3,0","0,0"))
    FP2res <- cbind(FP2res,round(myLnL,2),round(myAIC,2),round(myBIC,2))
    colnames(FP2res) <- c("Powers","LnL","AIC","BIC")
    FP2res <-arrange(FP2res,AIC)
    FP2_pow_temp1 <- FP2res[1,1]
    FP2res <-arrange(FP2res,BIC)
    FP2_pow_temp2 <- FP2res[1,1]
    
    ### all data
    dfHaz_fpsum <- t(na.omit(dfHazEst_fpsum))
    dfHaz_fpsum <- cbind(data.frame(Newtime2$lnTime), dfHaz_fpsum)
    dfHaz_fpsum$Time <- exp(as.numeric(dfHaz_fpsum$Newtime2.lnTime))
    dfHaz_fpsum <- as.data.frame(dfHaz_fpsum[,-1])
    dfSurv_fpsum<-as.data.frame(matrix(nrow = length(dfHaz_fpsum$Time),ncol = 2))
    dfSurv_fpsum[[1]]<-dfHaz_fpsum$Time
    for ( i in 2:45 ) {
      dftemp<-data.frame(dfHaz_fpsum$Time,dfHaz_fpsum[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv_fpsum[[i]]<-dftemp[[4]]
    }
    colnames(dfSurv_fpsum)<-c("Time",paste0("fp_",c("-2","-1","-0.5","0.5","1","2","3","0","-2,-1","-2,-0.5","-2,0.5","-2,1","-2,2","-2,3","-1,-0.5","-1,0.5","-1,1","-1,2","-1,3","-0.5,0.5","-0.5,1","-0.5,2","-0.5,3",
                              "0.5,1","0.5,2","0.5,3","1,2","1,3","2,3","-2,-2","-1,-1","-0.5,-0.5","0.5,0.5","1,1","2,2","3,3",
                              "-2,0","-1,0","-0.5,0","0.5,0","1,0","2,0","3,0","0,0")))
    dfFigSurv_fpsum = dfSurv_fpsum %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    
    values$dfFigSurv_fpsum<-dfFigSurv_fpsum
    values$FP1res<-FP1res
    values$FP2res<-FP2res
    output$FP1restb <- renderDT(datatable(values$FP1res, editable = FALSE, rownames = FALSE,
                                          colnames = c("Powers","LnL","AIC","BIC")))
    output$FP2restb <- renderDT(datatable(values$FP2res, editable = FALSE, rownames = FALSE,
                                          colnames = c("Powers","LnL","AIC","BIC")))
    
    
    output$best_fp21<-renderUI({
      html_best_fp21 <- paste("The FP2 with the minimum AIC: power =  <span style='color: red;border-color: #2e6da4'>", FP2_pow_temp1, "</span>")
      HTML(html_best_fp21)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_fp22<-renderUI({
      html_best_fp22 <- paste("The FP2 with the minimum BIC: power =  <span style='color: red;border-color: #2e6da4'>", FP2_pow_temp2, "</span>")
      HTML(html_best_fp22)
    })
    
    shinyalert(title = "Complete!", type = "success")
    
    output$downloadtable_fp1 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(FP1res, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_fp2 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(FP2res, file,row.names = FALSE)
      }
    )
    
    task_fp(100)
    
  })
  ### power input
  observeEvent(input$runmodelfp,{
    output$fp1p <- renderUI({
      textInput("FP1_pow", "Power input of FP1",value = 1)
    })
    output$fp2p1 <- renderUI({
      textInput("FP2_pow_1", "Power input 1 of FP2",value = 1)
    })
    output$fp2p2 <- renderUI({
      textInput("FP2_pow_2", "Power input 2 of FP2",value = 2)
    })
    
    
    output$runmodelfp2 <- renderUI({
      actionButton("runmodelfp21", label="Run the FP1",icon ("play"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")
    })
    
    output$runmodelfp3 <- renderUI({
      actionButton("runmodelfp31", label="Run the FP2",icon ("play"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")
    })
  })
  ### limit input fp
  observeEvent(input$FP1_pow,{  
    values$FP1_pow<-as.numeric(input$FP1_pow)
    if (as.numeric(values$FP1_pow) %in% c("-2","-1","-0.5","0","0.5","1","2","3")){
      enable("runmodelfp21")
    } else {
      shinyalert("Warning!", "The value should be one of the following: -2,-1,-0.5,0,0.5,1,2,3", type = "error")
      disable("runmodelfp21")
    }})
  ### limit input fp
  observeEvent(input$FP2_pow_1,{
    values$FP2_pow_1<-as.numeric(input$FP2_pow_1)
    if (values$FP2_pow_1 %in% c("-2","-1","-0.5","0","0.5","1","2","3")){
      enable("runmodelfp31")
    } else {
      shinyalert("Warning!", "The value should be one of the following: -2,-1,-0.5,0,0.5,1,2,3", type = "error")
      disable("runmodelfp31")
    }})
  ### limit input fp
  observeEvent(input$FP2_pow_2,{
    values$FP2_pow_2<-as.numeric(input$FP2_pow_2)
    if (values$FP2_pow_2 %in% c("-2","-1","-0.5","0","0.5","1","2","3")){
      enable("runmodelfp31")
    } else {
      shinyalert("Warning!", "The value should be one of the following: -2,-1,-0.5,0,0.5,1,2,3", type = "error")
      disable("runmodelfp31")
    }})
  ### specific fp1
  observeEvent(input$runmodelfp21,{
    mystep<-as.numeric(input$MyStep)
    FP1_pow<-as.numeric(values$FP1_pow)
    dfHazEst <- array(dim=c(1, values$MyN))
    ltHaz<-values$ltHaz
    dfHazEst2 <- array(dim=c(1, length(ltHaz$Time)))
    Newtime<-values$Newtime
    
    if(FP1_pow==0){
      modFP1 <- glm (cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    }else{modFP1 <- glm(cbind(Events,AtRisk-Events) ~ I(Time^FP1_pow)+ offset(log(timedelta)) , family=binomial(link=cloglog), data=ltHaz)}
    
    dfHazEst[1,] <- predict(modFP1, newdata=Newtime, type="response") # Extrapolated
    # dfHazEst2[1,] <- predict(modFP1, newdata=ltHaz, type="response")  # Within-sample
    dfHaz <- t(na.omit(dfHazEst))
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz <- as.data.frame(dfHaz[,-1])
    # colnames(as.data.frame(dfHaz)) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","genf")
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 2))
    dfSurv[[1]]<-dfHaz$Time
    dftemp<-data.frame(dfHaz$Time,dfHaz[[1]])
    dftemp<-dftemp %>%
      dplyr::arrange(dftemp[[1]]) %>%
      dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
      dplyr::mutate(survProp = exp(-1*cumhaz))
    dfSurv[[2]]<-dftemp[[4]]
    colnames(dfSurv)<-c("Time","FP1")
    
    dfFigSurv = dfSurv %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    output$plot_fp1_1<-renderPlot({
      f_surv1= ggplot() +
        geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
        geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
        scale_color_discrete(name="Model")+
        expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
        # facet_wrap(~Model,nrow=4)+
        scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
        ylab("Survival Probability") +
        xlab("Time(Years)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f_surv1<-f_surv1 + ggtitle(input$ggtitle1)
      print(f_surv1)
      values$graph2<-f_surv1
    })
    output$plot_fp1_2<-renderPlot({
      f_surv2= ggplot() +
        geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
        geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
        scale_color_discrete(name="Model")+
        expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
        # facet_wrap(~Model,nrow=4)+
        scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
        ylab("Survival Probability") +
        xlab("Time(Years)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f_surv2<-f_surv2 + ggtitle(input$ggtitle2)
      print(f_surv2)
      values$graph3<-f_surv2
    })
    values$modFP1<-modFP1
    updateCheckboxInput(session, "show_figure1", value = FALSE)
    
    output$downloadtable_dfhaz_fp1 <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_dfsurv_fp1 <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    shinyalert(title = "Complete!", type = "success")
    
  })
  
  ###    download 1
  
  observeEvent(input$dwld_fp11, {
    output$titlebut_fp11 <- renderUI({
      textInput("ggtitle1", "Title", value = input$tipo)
    })
  })
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_fp11, {
    output$sizebut_fp11 <- renderUI({
      bt1 <- tagList()
      bt1[[1]] <- numericInput("height1", "Height (px)", value = 1600)
      bt1[[2]] <- numericInput("width1", "Width (px)", value = 2800)
      bt1
    })
  })
  # Download back-end
  output$downloadPlot_fp11 <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph2, width = as.numeric(input$width1),
             height = as.numeric(input$height1),
             units = "px")
    })
  
  ###    download 2
  
  observeEvent(input$dwld_fp12, {
    output$titlebut_fp12 <- renderUI({
      textInput("ggtitle2", "Title", value = input$tipo)
    })
  })
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_fp12, {
    output$sizebut_fp12 <- renderUI({
      bt2 <- tagList()
      bt2[[1]] <- numericInput("height2", "Height (px)", value = 1600)
      bt2[[2]] <- numericInput("width2", "Width (px)", value = 2800)
      bt2
    })
  })
  # Download back-end
  output$downloadPlot_fp12 <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph3, width = as.numeric(input$width2),
             height = as.numeric(input$height2),
             units = "px")
    })
  ### specific fp2
  observeEvent(input$runmodelfp31,{
    mystep<-as.numeric(input$MyStep)
    FP2_pow_1<-as.numeric(values$FP2_pow_1)
    FP2_pow_2<-as.numeric(values$FP2_pow_2)
    if (FP2_pow_1 == 0){
      FP2_pow_1<-FP2_pow_2
      FP2_pow_2<-0
    }
    dfHazEst <- array(dim=c(1, values$MyN))
    ltHaz<-values$ltHaz
    dfHazEst2 <- array(dim=c(1, length(ltHaz$Time)))
    Newtime<-values$Newtime
    
    if(FP2_pow_1 == FP2_pow_2 & FP2_pow_2 != 0){
      modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^FP2_pow_1) + I(Time^FP2_pow_2*lnTime) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    }else if(FP2_pow_1==0 & FP2_pow_2== 0){
      modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + I(Time^0*lnTime*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    }else if(FP2_pow_2==0 & FP2_pow_1 != 0){
      modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^0*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    }else{
      modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^FP2_pow_1) + I(Time^FP2_pow_2) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    }
    
    dfHazEst[1,] <- predict(modFP2, newdata=Newtime, type="response") # Extrapolated
    # dfHazEst2[1,] <- predict(modFP1, newdata=ltHaz, type="response")  # Within-sample
    dfHaz <- t(na.omit(dfHazEst))
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz <- as.data.frame(dfHaz[,-1])
    # colnames(as.data.frame(dfHaz)) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","genf")
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 2))
    dfSurv[[1]]<-dfHaz$Time
    dftemp<-data.frame(dfHaz$Time,dfHaz[[1]])
    dftemp<-dftemp %>%
      dplyr::arrange(dftemp[[1]]) %>%
      dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
      dplyr::mutate(survProp = exp(-1*cumhaz))
    dfSurv[[2]]<-dftemp[[4]]
    colnames(dfSurv)<-c("Time","FP2")
    
    dfFigSurv = dfSurv %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    output$plot_fp2_1<-renderPlot({
      f_surv1= ggplot() +
        geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
        geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
        scale_color_discrete(name="Model")+
        expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
        # facet_wrap(~Model,nrow=4)+
        scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
        ylab("Survival Probability") +
        xlab("Time(Years)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f_surv1<-f_surv1 + ggtitle(input$ggtitle3)
      print(f_surv1)
      values$graph4<-f_surv1
    })
    output$plot_fp2_2<-renderPlot({
      f_surv2= ggplot() +
        geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
        geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
        scale_color_discrete(name="Model")+
        expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
        # facet_wrap(~Model,nrow=4)+
        scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
        ylab("Survival Probability") +
        xlab("Time(Years)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f_surv2<-f_surv2 + ggtitle(input$ggtitle4)
      print(f_surv2)
      values$graph5<-f_surv2
    })
    updateCheckboxInput(session, "show_figure2", value = FALSE)
    values$modFP2<-modFP2
    output$downloadtable_dfhaz_fp2 <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_dfsurv_fp2 <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    shinyalert(title = "Complete!", type = "success")
    
  })
  
  ###    download 3
  
  observeEvent(input$dwld_fp21, {
    output$titlebut_fp21 <- renderUI({
      textInput("ggtitle3", "Title", value = input$tipo)
    })
  })
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_fp21, {
    output$sizebut_fp21 <- renderUI({
      bt3 <- tagList()
      bt3[[1]] <- numericInput("height1", "Height (px)", value = 1600)
      bt3[[2]] <- numericInput("width1", "Width (px)", value = 2800)
      bt3
    })
  })
  # Download back-end
  output$downloadPlot_fp21 <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph4, width = as.numeric(input$width1),
             height = as.numeric(input$height1),
             units = "px")
    })
  
  ###    download 4
  
  observeEvent(input$dwld_fp22, {
    output$titlebut_fp22 <- renderUI({
      textInput("ggtitle4", "Title", value = input$tipo)
    })
  })
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_fp22, {
    output$sizebut_fp22 <- renderUI({
      bt4 <- tagList()
      bt4[[1]] <- numericInput("height2", "Height (px)", value = 1600)
      bt4[[2]] <- numericInput("width2", "Width (px)", value = 2800)
      bt4
    })
  })
  # Download back-end
  output$downloadPlot_fp22 <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph5, width = as.numeric(input$width2),
             height = as.numeric(input$height2),
             units = "px")
    })
  ### choose model button
  observeEvent(input$fp_choose_model_1,{
    shinyalert(title = "Caution",text = "If you want to choose the order of FP, please input the power of best FP models (both FP1 and FP2) in the 'Run specific FP model' section. If you continue without finishing this procedure, you may get the wrong results", type = "info")
  })

  ### choose model
  observeEvent(input$fp_choose_model_2,{
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    FP1_pow<-as.numeric(values$FP1_pow)
    FP2_pow_1<-as.numeric(values$FP2_pow_1)
    FP2_pow_2<-as.numeric(values$FP2_pow_2)
    modFP2 <- values$modFP2
    modFP1 <- values$modFP1
    modLin <- glm(cbind(Events,AtRisk-Events) ~ I(Time)+ offset(log(timedelta)) , family=binomial(link=cloglog), data=ltHaz)
    modNULL <- glm(cbind(Events,AtRisk-Events) ~ 1 + offset(log(timedelta)) , family=binomial(link=cloglog), data=ltHaz)
    ### df null
    dfHazest1 <- array(dim=c(1, values$MyN))
    dfHazest1[1,] <- predict(modNULL, newdata=Newtime, type="response") # Extrapolated
    dfHaz1 <- t(na.omit(dfHazest1))
    dfHaz1 <- cbind(data.frame(Newtime$lnTime), dfHaz1)
    dfHaz1$Time <- exp(as.numeric(dfHaz1$Newtime.lnTime))
    dfHaz1 <- as.data.frame(dfHaz1[,-1])
    # colnames(as.data.frame(dfHaz1)) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","genf")
    dfSurv1<-as.data.frame(matrix(nrow = length(dfHaz1$Time),ncol = 2))
    dfSurv1[[1]]<-dfHaz1$Time
    dftemp<-data.frame(dfHaz1$Time,dfHaz1[[1]])
    dftemp<-dftemp %>%
      dplyr::arrange(dftemp[[1]]) %>%
      dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
      dplyr::mutate(survProp = exp(-1*cumhaz))
    dfSurv1[[2]]<-dftemp[[4]]
    colnames(dfSurv1)<-c("Time","FP NUll")
    dfFigSurv1 = dfSurv1 %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    values$dfFigSurv_fpnull<-dfFigSurv1
    
    ### df lin
    dfHazest2 <- array(dim=c(1, values$MyN))
    dfHazest2[1,] <- predict(modLin, newdata=Newtime, type="response") # Extrapolated
    dfHaz2 <- t(na.omit(dfHazest2))
    dfHaz2 <- cbind(data.frame(Newtime$lnTime), dfHaz2)
    dfHaz2$Time <- exp(as.numeric(dfHaz2$Newtime.lnTime))
    dfHaz2 <- as.data.frame(dfHaz2[,-1])
    # colnames(as.data.frame(dfHaz2)) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","genf")
    dfSurv2<-as.data.frame(matrix(nrow = length(dfHaz2$Time),ncol = 2))
    dfSurv2[[1]]<-dfHaz2$Time
    dftemp<-data.frame(dfHaz2$Time,dfHaz2[[1]])
    dftemp<-dftemp %>%
      dplyr::arrange(dftemp[[1]]) %>%
      dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
      dplyr::mutate(survProp = exp(-1*cumhaz))
    dfSurv2[[2]]<-dftemp[[4]]
    colnames(dfSurv2)<-c("Time","FP Linear")
    dfFigSurv2 = dfSurv2 %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

    values$dfFigSurv_fplin<- dfFigSurv2
    
    
    # Overall association of the outcome with time (Sig result = include time)
    test_time<-anova(modNULL, modFP2, test="LRT") 
    p_value1<-test_time$`Pr(>Chi)`[2]
    if(p_value1 < 0.05) {
      fpcm_massage1<-"Outcome is associated with time!"
      disable("fpnull_haz")
      disable("fpnull_surv")
    }  else {
      fpcm_massage1<-"Outcome is not associated with time!"
      output$fpnullbt<-renderUI({
        bt4 <- tagList()
        bt4[[1]] <- p("")
        bt4[[2]] <- p("FP model with no association with time")
        bt4[[3]] <- fluidRow(
          column(4,downloadButton("fpnull_haz", "Hazard Data", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
          column(4, downloadButton("fpnull_surv", "Survival Data", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
          column(4, actionButton("fpnull_plot", label="Survival Plot",icon ("chart-area"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")))
        bt4
      })
    }
   
    # Evidence for non-linearity (Sig result = non-linear model)
    test_linear<-anova(modLin, modFP2, test="LRT") 
    p_value2<-test_linear$`Pr(>Chi)`[2]
    if(p_value2 < 0.05) {
      fpcm_massage2<-"Non-linearity model is OK!"
      disable("fplin_haz")
      disable("fplin_surv")
    }  else {
      fpcm_massage2<-"There is no evidence for non-linearity model!"
      output$fplinbt<-renderUI({
        bt5 <- tagList()
        bt5[[1]] <- p("")
        bt5[[2]] <- p("Linear FP model")
        bt5[[3]] <- fluidRow(
          column(4, downloadButton("fplin_haz", "Hazard Data", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
          column(4, downloadButton("fplin_surv", "Survival Data", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
          column(4, actionButton("fplin_plot", label="Survival Plot",icon ("chart-area"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")))
        bt5
      })
    }
    # Simpler or more complex non-linear model?  (Sig result = FP2, else FP1)
    test_simple<-anova(modFP1, modFP2, test="LRT") 
    p_value3<-test_simple$`Pr(>Chi)`[2]
    if(p_value3 < 0.05) {
      fpcm_massage3<-"Better choose FP2!"
    }  else {
      fpcm_massage3<-"Better choose FP1!"
    }
    fpcm_massage_final<-HTML(paste(
      paste("Test[1]:",HTML("<strong>",fpcm_massage1,"</strong>"),",P 1=",signif(p_value1,3)),
      paste("Test[2]:",HTML("<strong>",fpcm_massage2,"</strong>"),",P 2=",signif(p_value2,3)),
      paste("Test[3]:",HTML("<strong>",fpcm_massage3,"</strong>"),",P 3=",signif(p_value3,3)),sep = "<br>"
      ))
    shinyalert(title = "Caution",text = fpcm_massage_final, type = "info",html = TRUE)
    
    output$fpnull_haz <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz1, file,row.names = FALSE)
      }
    )
    
    output$fpnull_surv <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv1, file,row.names = FALSE)
      }
    )
    
    output$fplin_haz <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz2, file,row.names = FALSE)
      }
    )
    
    output$fplin_surv <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv2, file,row.names = FALSE)
      }
    )
    })
  
  observeEvent(input$fpnull_plot,{
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    dfFigSurv1<-values$dfFigSurv_fpnull
    output$plot_fpnull<-renderPlot({
      f_surv1= ggplot() +
        geom_line(data=dfFigSurv1, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
        geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
        scale_color_discrete(name="Model")+
        expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
        # facet_wrap(~Model,nrow=4)+
        scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
        ylab("Survival Probability") +
        xlab("Time(Years)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f_surv1<-f_surv1 + ggtitle(input$ggtitle_null)
      print(f_surv1)
      values$graph6<-f_surv1
    })
  })   
  
  observeEvent(input$fplin_plot,{
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    dfFigSurv2<-values$dfFigSurv_fplin
    output$plot_fplin<-renderPlot({
      f_surv2= ggplot() +
        geom_line(data=dfFigSurv2, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
        geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
        scale_color_discrete(name="Model")+
        expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
        # facet_wrap(~Model,nrow=4)+
        scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
        ylab("Survival Probability") +
        xlab("Time(Years)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f_surv2<-f_surv2 + ggtitle(input$ggtitle_lin)
      print(f_surv2)
      values$graph7<-f_surv2
    })
  })

  ### download null
  observeEvent(input$dwld_fpnull, {
    output$titlebut_fpnull <- renderUI({
      textInput("ggtitle_null", "Title", value = input$tipo)
    })
  })
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_fpnull, {
    output$sizebut_fpnull <- renderUI({
      btnull <- tagList()
      btnull[[1]] <- numericInput("height_null", "Height (px)", value = 1600)
      btnull[[2]] <- numericInput("width_null", "Width (px)", value = 2800)
      btnull
    })
  })
  # Download back-end
  output$downloadPlot_fpnull <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph6, width = as.numeric(input$width_null),
             height = as.numeric(input$height_null),
             units = "px")
    })
  
  ### download lin
  observeEvent(input$dwld_fplin, {
    output$titlebut_fplin <- renderUI({
      textInput("ggtitle_lin", "Title", value = input$tipo)
    })
  })
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_fplin, {
    output$sizebut_fplin <- renderUI({
      btlin <- tagList()
      btlin[[1]] <- numericInput("height_lin", "Height (px)", value = 1600)
      btlin[[2]] <- numericInput("width_lin", "Width (px)", value = 2800)
      btlin
    })
  })
  # Download back-end
  output$downloadPlot_fplin <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph7, width = as.numeric(input$width_lin),
             height = as.numeric(input$height_lin),
             units = "px")
    })
  #----
  ### RCS
  #----
  observeEvent(input$updatedata, {
    output$runmodelrcs1 <- renderUI({ 
      actionButton("runmodelrcs", label="Run the model",icon ("play"),style="color: black;
                       background-color: yellow; border-color: #2e6da4")
    })
  })
  
  observeEvent(input$runmodelrcs,{
    mystep<-as.numeric(input$MyStep)
    dfGOF <- data.frame(matrix(, nrow=5, ncol=6))
    colnames(dfGOF) <- c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC")
    dfHazEst <- array(dim=c(5, values$MyN))
    dfHazEst2 <- array(dim=c(5, mystep*30))
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    bc2<-values$ssm_input
    bc2<-subset(bc2, censrec==1)

    for (i in 1:5){
      tryCatch({
        glmTemp <- gam(cbind(Events,AtRisk-Events) ~ s(Time, bs="cr", k=i+2, fx=TRUE) + offset(log(timedelta)), knots=list(Time=quantile(bc2$recyrs, seq(from=0, to=1, by=1/(1+i))), length=i+2), family=binomial(link=cloglog), data=ltHaz)
        dfGOF[i,1] <- i
        dfGOF[i,2] <- round((extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5),2)
        dfGOF[i,3] <- extractAIC(glmTemp)[1]
        coeff_temp<-as.data.frame(glmTemp$coefficients)
        t1<-"Param"
        for (j in 1:length(glmTemp$coefficients)) {
          t0<-paste(rownames(coeff_temp)[j],round(coeff_temp[j,1],2),sep = "=")
          t1<-paste(t1,t0,sep = ";")
        }
        dfGOF[i,4] <- t1
        dfHazEst[i,] <- predict(glmTemp, newdata=Newtime, type="response")
        dfHazEst2[i,] <- predict(glmTemp, newdata=Newtime2, type="response")
      }, error = function(e) {
        shinyalert("Warning!", paste("An error occurred:", e$message), type = "error")
      })
    }
    
    dfGOF$AIC <- -2*dfGOF$'Log-Likelihood' + 2*dfGOF$Parameters
    dfGOF$BIC <- -2*dfGOF$'Log-Likelihood' + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters
    RCSres<-dfGOF
    RCSres<-arrange(RCSres,AIC)
    RCS_k1<-as.numeric(RCSres[1,1])
    RCSres<-arrange(RCSres,BIC)
    RCS_k2<-as.numeric(RCSres[1,1])
    
    output$best_rcs1<-renderUI({
      html_best_rcs1 <- paste("The restricted cubic spline model with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",RCS_k1), "</span>")
      HTML(html_best_rcs1)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_rcs2<-renderUI({
      html_best_rcs2 <- paste("The restricted cubic spline model with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",RCS_k2), "</span>")
      HTML(html_best_rcs2)
    })
    
    
    
    output$rcs_aic <- renderDT(datatable(dfGOF, editable = FALSE, rownames = FALSE,
                                         colnames = c("Knots","Log-Likelihood","Parameters","Coefficients","AIC","BIC"),
                                         options = list(dom = 'tp')))
    values$rcs_dfhazard1<-dfHazEst
    values$rcs_dfhazard2<-dfHazEst2

    shinyalert(title = "Complete!", type = "success")
    
    output$downloadtable_rcs1 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfGOF, file,row.names = FALSE)
      }
    )
    task_rcs(50)
  })
 
  observeEvent(input$runmodelrcs, {
    output$plotrcs1 <- renderUI({ 
      actionButton("plotrcs", label="Draw the plot",icon ("play"),style="color: black;
                       background-color: yellow; border-color: #2e6da4")
    })
  })
  
  ### rcs plot
  observeEvent(input$plotrcs, {
    mystep<-as.numeric(input$MyStep)
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    ltHaz<-values$ltHaz
    dfHazEst<-as.data.frame(values$rcs_dfhazard1)
    dfHaz <- t(na.omit(dfHazEst))
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz <- as.data.frame(dfHaz[,-1])
    colnames(dfHaz) <- c("1","2","3","4","5","Time")
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 6))
    dfSurv[[1]]<-dfHaz$Time
    
    for ( i in 2:6 ) {
      dftemp<-data.frame(dfHaz$Time,dfHaz[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv)<-c("Time","1","2","3","4","5")
    dfFigSurv = dfSurv %>%
      gather(key = "Knots", value = "survProp", -Time) %>% mutate(Knots = factor(Knots))
    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Knots", value = "survProp", -Time) %>% mutate(Knots = factor(Knots))
    
    
    dfHazEst2<-as.data.frame(values$rcs_dfhazard2)
    dfHaz_rcssum <- t(na.omit(dfHazEst2))
    dfHaz_rcssum <- cbind(data.frame(Newtime2$lnTime), dfHaz_rcssum)
    dfHaz_rcssum$Time <- exp(as.numeric(dfHaz_rcssum$Newtime2.lnTime))
    dfHaz_rcssum <- as.data.frame(dfHaz_rcssum[,-1])
    colnames(dfHaz_rcssum) <- c("1","2","3","4","5","Time")
    dfSurv_rcssum<-as.data.frame(matrix(nrow = length(dfHaz_rcssum$Time),ncol = 6))
    dfSurv_rcssum[[1]]<-dfHaz_rcssum$Time
    
    for ( i in 2:6 ) {
      dftemp<-data.frame(dfHaz_rcssum$Time,dfHaz_rcssum[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv_rcssum[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv_rcssum)<-c("Time","rcs_1","rcs_2","rcs_3","rcs_4","rcs_5")
    dfFigSurv_rcssum = dfSurv_rcssum %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    # output$rcssum<-renderDT(datatable(dfFigSurv_rcssum, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    

    values$dfFigSurv_rcssum<-dfFigSurv_rcssum
    values$rcs_dfHaz<-dfHaz
    values$rcs_dfSurv<-dfSurv
    values$rcs_dfFigSurv<-dfFigSurv
    values$rcs_dfFigSurv_in<-dfFigSurv_in
    
    output$plot_rcs<-renderPlot({
      
      if (input$timercs == 1 & input$rcs_knot == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timercs == 2 & input$rcs_knot == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timercs == 1 & input$rcs_knot %in% c("1","2","3","4","5")){
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Knots==input$rcs_knot), aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timercs == 2 & input$rcs_knot %in% c("1","2","3","4","5")){
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Knots==input$rcs_knot), aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }
      f_surv1<-f_surv1 + ggtitle(input$ggtitle_rcs)
      print(f_surv1)
      values$graph_rcs<-f_surv1
    })
    
    output$downloadtable_dfhaz_rcs <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_dfsurv_rcs <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    task_rcs(100)
    
  })
  
  ### download plot rcs
  observeEvent(input$dwld_rcs, {
    output$titlebut_rcs <- renderUI({
      textInput("ggtitle_rcs", "Title", value = input$tipo)
    })
  })
  
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_rcs, {
    output$sizebut_rcs <- renderUI({
      bt_rcs <- tagList()
      bt_rcs[[1]] <- numericInput("height_rcs", "Height (px)", value = 1600)
      bt_rcs[[2]] <- numericInput("width_rcs", "Width (px)", value = 2800)
      bt_rcs
    })
  })
  
  # Download back-end
  output$downloadPlot_rcs <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph_rcs, width = as.numeric(input$width_rcs),
             height = as.numeric(input$height_rcs),
             units = "px")
    }
  )
  
#----
  ### RP
#----
  observeEvent(input$updatedata, {
    output$runmodelrp1 <- renderUI({ 
      actionButton("runmodelrp", label="Run the model",icon ("play"),style="color: black;
                       background-color: orange; border-color: #2e6da4")
    })
  })
  
  observeEvent(input$runmodelrp,{
    mystep<-as.numeric(input$MyStep)
    dfGOF <- data.frame(matrix(, nrow=18, ncol=6))
    colnames(dfGOF) <- c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC")
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    llCons<-sum(ltHaz$Events*log(ltHaz$AtRisk) - log(factorial(ltHaz$Events)))
    temp_data_rp<-values$ssm_input
    MyAIC <- array(dim=c(6,3))
    MyBIC <- array(dim=c(6,3))
    MyScale <- list("hazard","odds","normal")
    dfHazEst <- array(dim=c(18, values$MyN))
    dfHazEst2 <- array(dim=c(18, length(ltHaz$Time)))
    dfHazEst3 <- array(dim=c(18, mystep*30))
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running Royston-Parmar models",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    for (i in 1:3){
      for (j in 0:5){
        tryCatch({
          MyTemp <- flexsurvspline(Surv(recyrs, censrec) ~ 1, data = temp_data_rp, k = j, scale = MyScale[[i]])
          dfHazEst[(i-1)*6+1+j,] <- summary(MyTemp, t=Newtime$Time, type="hazard")[[1]]$est/mystep
          dfHazEst2[(i-1)*6+1+j,] <- summary(MyTemp, t=ltHaz$Time, type="hazard")[[1]]$est/mystep
          dfHazEst3[(i-1)*6+1+j,] <- summary(MyTemp, t=Newtime2$Time, type="hazard")[[1]]$est/mystep
          dfGOF$Model[(i-1)*6+1+j] <- paste("Knot=",j,";Scale=",MyScale[[i]])
          dfGOF$'Log-Likelihood'[(i-1)*6+1+j] <- round(sum(ltHaz$Events*log(dfHazEst2[(i-1)*6+1+j,]) - dfHazEst2[(i-1)*6+1+j,]*ltHaz$AtRisk) + llCons,2)
          dfGOF$Parameters[(i-1)*6+1+j] <- MyTemp$npars
          coeff_temp<-as.data.frame(MyTemp$coefficients)
          t1<-"Param"
          for (ij in 1:length(MyTemp$coefficients)) {
            t0<-paste(rownames(coeff_temp)[ij],round(coeff_temp[ij,1],2),sep = "=")
            t1<-paste(t1,t0,sep = ";")
          }
          dfGOF$Coefficients[(i-1)*6+1+j] <- t1
          MyAIC[[(i-1)*6+1+j]] <- (-2*dfGOF$'Log-Likelihood'[(i-1)*6+1+j] + 2*dfGOF$Parameters[(i-1)*6+1+j])
          MyBIC[[(i-1)*6+1+j]] <- (-2*dfGOF$'Log-Likelihood'[(i-1)*6+1+j] + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters[(i-1)*6+1+j])
        }, error = function(e) {
          shinyalert("Warning!", paste("An error occurred:", e$message), type = "error")
        })
      }
      progress$set(value = ((i-1)*6+1+j) / 18)
      Sys.sleep(0.5)
    }
    progress$close()
    removeModal()
    
    dfGOF$AIC <- -2*dfGOF$'Log-Likelihood' + 2*dfGOF$Parameters
    dfGOF$BIC <- -2*dfGOF$'Log-Likelihood' + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters
    
    MyAICResults <- as.data.frame(cbind(seq(1:6)-1,MyAIC))
    colnames(MyAICResults) <- c("Knots","Hazard","Odds","Normal")
    best_rp_hazard1<-data.frame(MyAICResults$Knots,MyAICResults$Hazard)
    colnames(best_rp_hazard1)<-c("knots","AIC")
    best_rp_odds1<-data.frame(MyAICResults$Knots,MyAICResults$Odds)
    colnames(best_rp_odds1)<-c("knots","AIC")
    best_rp_normal1<-data.frame(MyAICResults$Knots,MyAICResults$Normal)
    colnames(best_rp_normal1)<-c("knots","AIC")
    best_rp_hazard1<-arrange(best_rp_hazard1,AIC)
    best_rp_odds1<-arrange(best_rp_odds1,AIC)
    best_rp_normal1<-arrange(best_rp_normal1,AIC)
    
    MyBICResults <- as.data.frame(cbind(seq(1:6)-1,MyBIC))
    colnames(MyBICResults) <- c("Knots","Hazard","Odds","Normal")
    best_rp_hazard2<-data.frame(MyBICResults$Knots,MyBICResults$Hazard)
    colnames(best_rp_hazard2)<-c("knots","BIC")
    best_rp_odds2<-data.frame(MyBICResults$Knots,MyBICResults$Odds)
    colnames(best_rp_odds2)<-c("knots","BIC")
    best_rp_normal2<-data.frame(MyBICResults$Knots,MyBICResults$Normal)
    colnames(best_rp_normal2)<-c("knots","BIC")
    best_rp_hazard2<-arrange(best_rp_hazard2,BIC)
    best_rp_odds2<-arrange(best_rp_odds2,BIC)
    best_rp_normal2<-arrange(best_rp_normal2,BIC)
    
  #
    output$best_rp_h1<-renderUI({
      html_best_rp_h1 <- paste("The Royston-Parmar models (Scale = Hazard) with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",best_rp_hazard1$knots[1]), "</span>")
            HTML(html_best_rp_h1)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_rp_h2<-renderUI({
      html_best_rp_h2 <- paste("The Royston-Parmar models (Scale = Hazard) with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",best_rp_hazard2$knots[1]), "</span>")
      HTML(html_best_rp_h2)
    })
    output$best_rp_o1<-renderUI({
      html_best_rp_o1 <- paste("The Royston-Parmar models (Scale = Odds) with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",best_rp_odds1$knots[1]), "</span>")
      HTML(html_best_rp_o1)
    })
    output$best_rp_o2<-renderUI({
      html_best_rp_o2 <- paste("The Royston-Parmar models (Scale = Odds) with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",best_rp_odds2$knots[1]), "</span>")
      HTML(html_best_rp_o2)
    })
    output$best_rp_n1<-renderUI({
      html_best_rp_n1 <- paste("The Royston-Parmar models (Scale = Normal) with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",best_rp_normal1$knots[1]), "</span>")
      HTML(html_best_rp_n1)
    })
    output$best_rp_n2<-renderUI({
      html_best_rp_n2 <- paste("The Royston-Parmar models (Scale = Normal) with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",best_rp_normal2$knots[1]), "</span>")
      HTML(html_best_rp_n2)
    })

    output$rp_aic <- renderDT(datatable(dfGOF, editable = FALSE, rownames = FALSE,
                                         colnames = c("Knots","Log-Likelihood","Parameters","Coefficients","AIC","BIC"),
                                         options = list(dom = 'tp')))
    values$rp_dfhazard1<-dfHazEst
    values$rp_dfhazard2<-dfHazEst3
    shinyalert(title = "Complete!", type = "success")

    output$downloadtable_rp1 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfGOF, file,row.names = FALSE)
      }
    )
    
    task_rp(50)
    
  })

  observeEvent(input$runmodelrp, {
    output$plotrp1 <- renderUI({
      actionButton("plotrp", label="Draw the plot",icon ("play"),style="color: black;
                       background-color: orange; border-color: #2e6da4")
    })
  })
  ### rp plot
  observeEvent(input$plotrp, {
    mystep<-as.numeric(input$MyStep)
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    ltHaz<-values$ltHaz
    dfHazEst<-as.data.frame(values$rp_dfhazard1)
    dfHaz <- t(dfHazEst)
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz <- as.data.frame(dfHaz[,-1])
    colnames(dfHaz) <- c("hazard0","hazard1","hazard2","hazard3","hazard4","hazard5",
                         "odds0","odds1","odds2","odds3","odds4","odds5",
                         "normal0","normal1","normal2","normal3","normal4","normal5","Time")
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 19))
    dfSurv[[1]]<-dfHaz$Time

    for ( i in 2:19 ) {
      dftemp<-data.frame(dfHaz$Time,dfHaz[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv[[i]]<-dftemp[[4]]
    }

    colnames(dfSurv)<- c("Time","hazard0","hazard1","hazard2","hazard3","hazard4","hazard5",
                                           "odds0","odds1","odds2","odds3","odds4","odds5",
                                           "normal0","normal1","normal2","normal3","normal4","normal5")
    dfFigSurv = dfSurv %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    
    dfHazEst2<-as.data.frame(values$rp_dfhazard2)
    dfHaz_rpsum <- t(dfHazEst2)
    dfHaz_rpsum <- cbind(data.frame(Newtime2$lnTime), dfHaz_rpsum)
    dfHaz_rpsum$Time <- exp(as.numeric(dfHaz_rpsum$Newtime2.lnTime))
    dfHaz_rpsum <- as.data.frame(dfHaz_rpsum[,-1])
    colnames(dfHaz_rpsum) <- c("hazard0","hazard1","hazard2","hazard3","hazard4","hazard5",
                         "odds0","odds1","odds2","odds3","odds4","odds5",
                         "normal0","normal1","normal2","normal3","normal4","normal5","Time")
    dfSurv_rpsum<-as.data.frame(matrix(nrow = length(dfHaz_rpsum$Time),ncol = 19))
    dfSurv_rpsum[[1]]<-dfHaz_rpsum$Time
    
    for ( i in 2:19 ) {
      dftemp<-data.frame(dfHaz_rpsum$Time,dfHaz_rpsum[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv_rpsum[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv_rpsum)<- c("Time",paste0("rp_",c("hazard0","hazard1","hazard2","hazard3","hazard4","hazard5",
                                           "odds0","odds1","odds2","odds3","odds4","odds5",
                                           "normal0","normal1","normal2","normal3","normal4","normal5")))
    dfFigSurv_rpsum = dfSurv_rpsum %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    values$dfFigSurv_rpsum<-dfFigSurv_rpsum
    values$rp_dfHaz<-dfHaz
    values$rp_dfSurv<-dfSurv
    values$rp_dfFigSurv<-dfFigSurv
    values$rp_dfFigSurv_in<-dfFigSurv_in

    output$plot_rp<-renderPlot({
      ltHaz<-values$ltHaz
      Newtime<-values$Newtime
      scale_temp<-c("hazard","odds","normal")
      knot_temp<-c("0","1","2","3","4","5")

      if (input$timerp == 1 & input$rp_knot == "Combination" & input$rp_scale == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }  else if (input$timerp == 1 & input$rp_knot %in% knot_temp & input$rp_scale == "Combination"){
        input_temp = paste0(scale_temp,input$rp_knot)
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Model==input_temp), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timerp == 1 & input$rp_knot == "Combination" & input$rp_scale %in% scale_temp){
        input_temp = paste0(input$rp_scale,knot_temp)
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Model==input_temp), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timerp == 1 & input$rp_knot %in% knot_temp & input$rp_scale %in% scale_temp){
        input_temp = paste0(input$rp_scale,input$rp_knot)
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Model==input_temp), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timerp == 2 & input$rp_knot == "Combination" & input$rp_scale == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }  else if (input$timerp == 2 & input$rp_knot %in% knot_temp & input$rp_scale == "Combination"){
        input_temp = paste0(scale_temp,input$rp_knot)
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Model==input_temp), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timerp == 2 & input$rp_knot == "Combination" & input$rp_scale %in% scale_temp){
        input_temp = paste0(input$rp_scale,knot_temp)
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Model==input_temp), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timerp == 2 & input$rp_knot %in% knot_temp & input$rp_scale %in% scale_temp){
        input_temp = paste0(input$rp_scale,input$rp_knot)
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Model==input_temp), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }
      f_surv1<-f_surv1 + ggtitle(input$ggtitle_rp)
      print(f_surv1)
      values$graph_rp<-f_surv1
    })

    output$downloadtable_dfhaz_rp <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )

    output$downloadtable_dfsurv_rp <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    task_rp(100)
    
  })

  ### download plot rp
  observeEvent(input$dwld_rp, {
    output$titlebut_rp <- renderUI({
      textInput("ggtitle_rp", "Title", value = input$tipo)
    })
  })

  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_rp, {
    output$sizebut_rp <- renderUI({
      bt_rp <- tagList()
      bt_rp[[1]] <- numericInput("height_rp", "Height (px)", value = 1600)
      bt_rp[[2]] <- numericInput("width_rp", "Width (px)", value = 2800)
      bt_rp
    })
  })

  # Download back-end
  output$downloadPlot_rp <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph_rp, width = as.numeric(input$width_rp),
             height = as.numeric(input$height_rp),
             units = "px")
    }
  )
  
  #----
  ### GAM
  #----
  observeEvent(input$updatedata, {
    output$runmodelgam1 <- renderUI({ 
      actionButton("runmodelgam", label="Run the model",icon ("play"),style="color: white;
                       background-color: violet; border-color: #2e6da4")
    })
  })
  
  observeEvent(input$runmodelgam,{
    mystep<-as.numeric(input$MyStep)
    dfGOF <- data.frame(matrix(, nrow=10, ncol=6))
    colnames(dfGOF) <- c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC")
    dfHazEst <- array(dim=c(10, values$MyN))
    dfHazEst2 <- array(dim=c(10, mystep*30))
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    bc2<-values$ssm_input
    bc2<-subset(bc2, censrec==1)
   
    for (i in 1:10){
      tryCatch({
        glmTemp <-gam(cbind(Events,AtRisk-Events) ~ s(Time, bs="cr", k=i+2, fx=FALSE) + offset(log(timedelta)),
                      knots=list(Time=quantile(bc2$recyrs, seq(from=0, to=1, by=1/(1+i))), length=i+2), family=binomial(link=cloglog), data=ltHaz)
        dfGOF[i,1] <- i
        dfGOF[i,2] <- round((extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5),2)
        dfGOF[i,3] <- round(extractAIC(glmTemp)[1],2)
        coeff_temp<-as.data.frame(glmTemp$coefficients)
        t1<-"Param"
        for (j in 1:length(glmTemp$coefficients)) {
          t0<-paste(rownames(coeff_temp)[j],round(coeff_temp[j,1],2),sep = "=")
          t1<-paste(t1,t0,sep = ";")
        }
        smooth_parm<-as.data.frame(glmTemp$sp)
        t2<-"Smooth_Param"
        for (ij in 1:length(smooth_parm[,1])) {
          t0<-paste(rownames(smooth_parm)[ij],round(smooth_parm[ij,1],2),sep = "=")
          t2<-paste(t2,t0,sep = ";")
        }
        dfGOF[i,4] <- paste(paste(t1,paste("k=",i),sep = ";"),t2,sep = ";")
        dfHazEst[i,] <- predict(glmTemp, newdata=Newtime, type="response")
        dfHazEst2[i,] <- predict(glmTemp, newdata=Newtime2, type="response")
      }, error = function(e) {
        shinyalert("Warning!", paste("An error occurred:", e$message), type = "error")
        })
    }
    
    dfGOF$AIC <- -2*dfGOF$'Log-Likelihood' + 2*dfGOF$Parameters
    dfGOF$BIC <- -2*dfGOF$'Log-Likelihood' + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters
    
    GAMres<-dfGOF
    GAMres<-arrange(GAMres,AIC)
    GAM_k1<-as.numeric(GAMres[1,1])
    GAMres<-arrange(GAMres,BIC)
    GAM_k2<-as.numeric(GAMres[1,1])
    
    output$best_gam1<-renderUI({
      html_best_gam1 <- paste("The generalized additive model with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",GAM_k1), "</span>")
      HTML(html_best_gam1)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_gam2<-renderUI({
      html_best_gam2 <- paste("The generalized additive model with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", paste("Knots=",GAM_k2), "</span>")
      HTML(html_best_gam2)
    })

    output$gam_aic <- renderDT(datatable(dfGOF, editable = FALSE, rownames = FALSE,
                                         colnames = c("Knots","Log-Likelihood","Parameters","Coefficients","AIC","BIC"),
                                         options = list(dom = 'tp',scrollX = TRUE)))
    values$gam_dfhazard1<-dfHazEst
    values$gam_dfhazard2<-dfHazEst2
    
    shinyalert(title = "Complete!", type = "success")
    
    output$downloadtable_gam1 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfGOF, file,row.names = FALSE)
      }
    )
    
    task_gam(50)
    
  })
  
  observeEvent(input$runmodelgam, {
    output$plotgam1 <- renderUI({ 
      actionButton("plotgam", label="Draw the plot",icon ("play"),style="color: white;
                       background-color: violet; border-color: #2e6da4")
    })
  })
  ### gam plot
  observeEvent(input$plotgam, {
    mystep<-as.numeric(input$MyStep)
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    ltHaz<-values$ltHaz
    dfHazEst<-as.data.frame(values$gam_dfhazard1)
    dfHaz <- t(na.omit(dfHazEst))
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz <- as.data.frame(dfHaz[,-1])
    colnames(dfHaz) <- c("1","2","3","4","5","6","7","8","9","10","Time")
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 11))
    dfSurv[[1]]<-dfHaz$Time
    
    for ( i in 2:11 ) {
      dftemp<-data.frame(dfHaz$Time,dfHaz[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv)<-c("Time","1","2","3","4","5","6","7","8","9","10")
    dfFigSurv = dfSurv %>%
      gather(key = "Knots", value = "survProp", -Time) %>% mutate(Knots = factor(Knots))
    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Knots", value = "survProp", -Time) %>% mutate(Knots = factor(Knots))
    
    
    dfHazEst2<-as.data.frame(values$gam_dfhazard2)
    dfHaz_gamsum <- t(na.omit(dfHazEst2))
    dfHaz_gamsum <- cbind(data.frame(Newtime2$lnTime), dfHaz_gamsum)
    dfHaz_gamsum$Time <- exp(as.numeric(dfHaz_gamsum$Newtime2.lnTime))
    dfHaz_gamsum <- as.data.frame(dfHaz_gamsum[,-1])
    colnames(dfHaz_gamsum) <- c("1","2","3","4","5","6","7","8","9","10","Time")
    dfSurv_gamsum<-as.data.frame(matrix(nrow = length(dfHaz_gamsum$Time),ncol = 11))
    dfSurv_gamsum[[1]]<-dfHaz_gamsum$Time
    
    for ( i in 2:11 ) {
      dftemp<-data.frame(dfHaz_gamsum$Time,dfHaz_gamsum[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv_gamsum[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv_gamsum)<-c("Time",paste0("gam_",c("1","2","3","4","5","6","7","8","9","10")))
    dfFigSurv_gamsum = dfSurv_gamsum %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    values$dfFigSurv_gamsum<-dfFigSurv_gamsum
    values$gam_dfHaz<-dfHaz
    values$gam_dfSurv<-dfSurv
    values$gam_dfFigSurv<-dfFigSurv
    values$gam_dfFigSurv_in<-dfFigSurv_in
    
    output$plot_gam<-renderPlot({
      
      if (input$timegam == 1 & input$gam_knot == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timegam == 2 & input$gam_knot == "Combination"){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timegam == 1 & input$gam_knot %in% c("1","2","3","4","5","6","7","8","9","10")){
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Knots==input$gam_knot), aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timegam == 2 & input$gam_knot %in% c("1","2","3","4","5","6","7","8","9","10")){
        f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Knots==input$gam_knot), aes(x=Time, y=survProp, group=Knots, colour=Knots), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Knots")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }
      f_surv1<-f_surv1 + ggtitle(input$ggtitle_gam)
      print(f_surv1)
      values$graph_gam<-f_surv1
    })
    
    output$downloadtable_dfhaz_gam <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_dfsurv_gam <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    task_gam(100)
    
  })
  
  ### download plot gam
  observeEvent(input$dwld_gam, {
    output$titlebut_gam <- renderUI({
      textInput("ggtitle_gam", "Title", value = input$tipo)
    })
  })
  
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_gam, {
    output$sizebut_gam <- renderUI({
      bt_gam <- tagList()
      bt_gam[[1]] <- numericInput("height_gam", "Height (px)", value = 1600)
      bt_gam[[2]] <- numericInput("width_gam", "Width (px)", value = 2800)
      bt_gam
    })
  })
  
  # Download back-end
  output$downloadPlot_gam <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph_gam, width = as.numeric(input$width_gam),
             height = as.numeric(input$height_gam),
             units = "px")
    }
  )
  
  #----
  source("function/PMM.R")
  ### PMM
  #----
  observeEvent(input$updatedata, {
    output$runmodelpmm1 <- renderUI({ 
      actionButton("runmodelpmm", label="Run the model",icon ("play"),style="color: black;
                       background-color: snow; border-color: #2e6da4")
    })
  })
  
  observeEvent(input$runmodelpmm,{
    mystep<-as.numeric(input$MyStep)
    gef_MM<-values$ssm_input
    survpmm <- Surv(time=gef_MM$recyrs , event=gef_MM$censrec==1)
    dfGOF <- data.frame(matrix(, nrow=28, ncol=6))
    colnames(dfGOF) <- c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC")
    ltHaz<-values$ltHaz
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    llCons<-sum(ltHaz$Events*log(ltHaz$AtRisk) - log(factorial(ltHaz$Events)))
    dfHazEst <- array(dim=c(28, values$MyN))
    dfHazEst2 <- array(dim=c(28, length(ltHaz$Time)))
    dfHazEst3 <- array(dim=c(28, mystep*30))
    # dists list #
    dists_MM<-as.data.frame(matrix(nrow=28,ncol=2))
    colnames(dists_MM)<-c("dist1",'dist2')
    distlist<-c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma")
    index1<-1
    for (i in 1:7) {
      for (j in i:7){
        dists_MM$dist1[index1]<- distlist[i]
        dists_MM$dist2[index1]<- distlist[j]
        index1<-index1+1
      }
    }
    myAIC <- array(dim=28)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running Parametric mixture models",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    for (i in 1:28) {
      tryCatch({
        mixfit <- flexsurvmixture(survpmm ~ 1, dists = c(dists_MM$dist1[i],dists_MM$dist2[i]), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
        dfHazEst[i,] <- summary(mixfit, t=exp(Newtime$lnTime), type="hazard")[[1]]$est/mystep
        dfHazEst2[i,] <- summary(mixfit, t=ltHaz$Time, type="hazard")[[1]]$est/mystep
        dfHazEst3[i,] <- summary(mixfit, t=exp(Newtime2$lnTime), type="hazard")[[1]]$est/mystep
        dfGOF[i,1] <- paste(dists_MM$dist1[i],dists_MM$dist2[i])
        dfGOF[i,2] <- round(sum(ltHaz$Events*log(dfHazEst2[i,]) - dfHazEst2[i,]*ltHaz$AtRisk) + llCons,2)
        dfGOF[i,3] <- mixfit$npars
        coeff_temp<-as.data.frame(mixfit$coefficients)
        t1<-"Param"
        for (j in 1:length(mixfit$coefficients)) {
          t0<-paste(rownames(coeff_temp)[j],round(coeff_temp[j,1],2),sep = "=")
          t1<-paste(t1,t0,sep = ";")
        }
        dfGOF[i,4] <- t1
      }, error = function(e) {
        shinyalert("Warning!", paste("An error occurred:", e$message), type = "error")
        })
      progress$set(value = (i / 28))
      Sys.sleep(0.5)
    }
    progress$close()
    removeModal()
    
    dfGOF$AIC <- -2*dfGOF$'Log-Likelihood' + 2*dfGOF$Parameters
    dfGOF$BIC <- -2*dfGOF$'Log-Likelihood' + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters
    
    MMres<-cbind(dists_MM,dfGOF$AIC,dfGOF$BIC)
    colnames(MMres)<-c("dist1","dist2","AIC","BIC")
    MMres1<-arrange(MMres,AIC) 
    MMres2<-arrange(MMres,BIC)

    output$best_pmm1<-renderUI({
      html_best_pmm1 <- paste("The Parametric mixture models with the minimum AIC: <span style='color: red;border-color: #2e6da4'>", paste("Dist:",MMres1$dist1[1],"and",MMres1$dist2[1]), "</span>")
      HTML(html_best_pmm1)%>%
        helper(type = "markdown", content = "bestmodel")
    })
    output$best_pmm2<-renderUI({
      html_best_pmm2 <- paste("The Parametric mixture models with the minimum BIC: <span style='color: red;border-color: #2e6da4'>", paste("Dist:",MMres2$dist1[1],"and",MMres2$dist2[1]), "</span>")
      HTML(html_best_pmm2)
    })

    
    output$pmm_aic <- renderDT(datatable(dfGOF, editable = FALSE, rownames = FALSE,
                                        colnames = c("Knots","Log-Likelihood","Parameters","Coefficients","AIC","BIC"),
                                        options = list(dom = 'tp')))
    values$pmm_dfhazard1<-dfHazEst
    values$pmm_dfhazard2<-dfHazEst3
    shinyalert(title = "Complete!", type = "success")
    
    output$downloadtable_pmm1 <- downloadHandler(
      filename = function() {
        paste("data_1", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfGOF, file,row.names = FALSE)
      }
    )
    
    task_pmm(50)
    
  })
  
  observeEvent(input$runmodelpmm, {
    output$plotpmm1 <- renderUI({
      actionButton("plotpmm", label="Draw the plot",icon ("play"),style="color: black;
                       background-color: snow; border-color: #2e6da4")
    })
  })
  
  ### pmm plot
  observeEvent(input$plotpmm, {
    mystep<-as.numeric(input$MyStep)
    Newtime<-values$Newtime
    Newtime2<-values$Newtime2
    ltHaz<-values$ltHaz
    dfHazEst<-as.data.frame(values$pmm_dfhazard1)
    dfHaz <- t(dfHazEst)
    dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
    dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
    dfHaz <- as.data.frame(dfHaz[,-1])
    colnames(dfHaz) <- c("exp_exp","exp_weibull","exp_gamma","exp_lnorm","exp_gompertz","exp_llogis","exp_gengamma",
                         "weibull_weibull","weibull_gamma","weibull_lnorm","weibull_gompertz","weibull_llogis","weibull_gengamma",
                         "gamma_gamma","gamma_lnorm","gamma_gompertz","gamma_llogis","gamma_gengamma",
                         "lnorm_lnorm","lnorm_gompertz","lnorm_llogis","lnorm_gengamma","gompertz_gompertz","gompertz_llogis","gompertz_gengamma",
                         "llogis_llogis","llogis_gengamma","gengamma_gengamma","Time")
    dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 29))
    dfSurv[[1]]<-dfHaz$Time
    
    for ( i in 2:29 ) {
      dftemp<-data.frame(dfHaz$Time,dfHaz[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv)<- c("Time","exp_exp","exp_weibull","exp_gamma","exp_lnorm","exp_gompertz","exp_llogis","exp_gengamma",
                         "weibull_weibull","weibull_gamma","weibull_lnorm","weibull_gompertz","weibull_llogis","weibull_gengamma",
                         "gamma_gamma","gamma_lnorm","gamma_gompertz","gamma_llogis","gamma_gengamma",
                         "lnorm_lnorm","lnorm_gompertz","lnorm_llogis","lnorm_gengamma","gompertz_gompertz","gompertz_llogis","gompertz_gengamma",
                         "llogis_llogis","llogis_gengamma","gengamma_gengamma")
    dfFigSurv = dfSurv %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    dfSurv_in<-dfSurv[1:round(length(ltHaz$Time)/12*mystep,0),]
    dfFigSurv_in = dfSurv_in %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    
    dfHazEst2<-as.data.frame(values$pmm_dfhazard2)
    dfHaz_pmmsum <- t(dfHazEst2)
    dfHaz_pmmsum <- cbind(data.frame(Newtime2$lnTime), dfHaz_pmmsum)
    dfHaz_pmmsum$Time <- exp(as.numeric(dfHaz_pmmsum$Newtime2.lnTime))
    dfHaz_pmmsum <- as.data.frame(dfHaz_pmmsum[,-1])
    colnames(dfHaz_pmmsum) <- c("exp_exp","exp_weibull","exp_gamma","exp_lnorm","exp_gompertz","exp_llogis","exp_gengamma",
                                "weibull_weibull","weibull_gamma","weibull_lnorm","weibull_gompertz","weibull_llogis","weibull_gengamma",
                                "gamma_gamma","gamma_lnorm","gamma_gompertz","gamma_llogis","gamma_gengamma",
                                "lnorm_lnorm","lnorm_gompertz","lnorm_llogis","lnorm_gengamma","gompertz_gompertz","gompertz_llogis","gompertz_gengamma",
                                "llogis_llogis","llogis_gengamma","gengamma_gengamma","Time")
    dfSurv_pmmsum<-as.data.frame(matrix(nrow = length(dfHaz_pmmsum$Time),ncol = 29))
    dfSurv_pmmsum[[1]]<-dfHaz_pmmsum$Time
    
    for ( i in 2:29 ) {
      dftemp<-data.frame(dfHaz_pmmsum$Time,dfHaz_pmmsum[[(i-1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      dfSurv_pmmsum[[i]]<-dftemp[[4]]
    }
    
    colnames(dfSurv_pmmsum)<- c("Time",paste0("pmm_",c("exp_exp","exp_weibull","exp_gamma","exp_lnorm","exp_gompertz","exp_llogis","exp_gengamma",
                                "weibull_weibull","weibull_gamma","weibull_lnorm","weibull_gompertz","weibull_llogis","weibull_gengamma",
                                "gamma_gamma","gamma_lnorm","gamma_gompertz","gamma_llogis","gamma_gengamma",
                                "lnorm_lnorm","lnorm_gompertz","lnorm_llogis","lnorm_gengamma","gompertz_gompertz","gompertz_llogis","gompertz_gengamma",
                                "llogis_llogis","llogis_gengamma","gengamma_gengamma")))
    dfFigSurv_pmmsum = dfSurv_pmmsum %>%
      gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
    
    values$dfFigSurv_pmmsum<-dfFigSurv_pmmsum
    values$pmm_dfHaz<-dfHaz
    values$pmm_dfSurv<-dfSurv
    values$pmm_dfFigSurv<-dfFigSurv
    values$pmm_dfFigSurv_in<-dfFigSurv_in
    
    output$plot_pmm<-renderPlot({
      ltHaz<-values$ltHaz
      Newtime<-values$Newtime
      dfFigSurv<-values$pmm_dfFigSurv
      dfFigSurv_in<-values$pmm_dfFigSurv_in
      knot_temp<-c("exp_exp","exp_weibull","exp_gamma","exp_lnorm","exp_gompertz","exp_llogis","exp_gengamma",
                   "weibull_weibull","weibull_gamma","weibull_lnorm","weibull_gompertz","weibull_llogis","weibull_gengamma",
                   "gamma_gamma","gamma_lnorm","gamma_gompertz","gamma_llogis","gamma_gengamma",
                   "lnorm_lnorm","lnorm_gompertz","lnorm_llogis","lnorm_gengamma","gompertz_gompertz","gompertz_llogis","gompertz_gengamma",
                   "llogis_llogis","llogis_gengamma","gengamma_gengamma")
      input_temp_1 = paste(input$pmm_d1,input$pmm_d2,sep = "_")
      input_temp_2 = paste(input$pmm_d2,input$pmm_d1,sep = "_")
      if (input_temp_1 %in% knot_temp) {
        input_temp_pmm=input_temp_1
      } else if (input_temp_2 %in% knot_temp){
        input_temp_pmm=input_temp_2
      } else if (input_temp_2 %in% knot_temp & (input_temp_1 %in% knot_temp)){
        input_temp_pmm=input_temp_1
      }
      
      if (input$timepmm == 1 & (input$pmm_d1 == "Combination")){
        f_surv1= ggplot() +
          geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }  else if (input$timepmm == 1 & (input$pmm_d1 != "Combination")){
          f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv_in,Model==input_temp_pmm), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } else if (input$timepmm == 2 & (input$pmm_d1 == "Combination")){
          f_surv1= ggplot() +
          geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      }  else if (input$timepmm == 2 & (input$pmm_d1 != "Combination")){
          f_surv1= ggplot() +
          geom_line(data=filter(dfFigSurv,Model==input_temp_pmm), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
          scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
          ylab("Survival Probability") +
          xlab("Time(Years)") +
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
      } 
      f_surv1<-f_surv1 + ggtitle(input$ggtitle_pmm)
      print(f_surv1)
      values$graph_pmm<-f_surv1
    })
    
    
    output$plot_pmm1<-renderPlot({
      Newtime<-values$Newtime
      p_time<-round(as.numeric(input$MyTH),0)
      gef_MM<-values$ssm_input
      survpmm <- Surv(time=gef_MM$recyrs , event=gef_MM$censrec==1)
      knot_temp<-c("exp_exp","exp_weibull","exp_gamma","exp_lnorm","exp_gompertz","exp_llogis","exp_gengamma",
                   "weibull_weibull","weibull_gamma","weibull_lnorm","weibull_gompertz","weibull_llogis","weibull_gengamma",
                   "gamma_gamma","gamma_lnorm","gamma_gompertz","gamma_llogis","gamma_gengamma",
                   "lnorm_lnorm","lnorm_gompertz","lnorm_llogis","lnorm_gengamma",
                   "gompertz_gompertz","gompertz_llogis","gompertz_gengamma",
                   "llogis_llogis","llogis_gengamma",
                   "gengamma_gengamma")
      input_temp_1 = paste(input$pmm_d1,input$pmm_d2,sep = "_")
      input_temp_2 = paste(input$pmm_d2,input$pmm_d1,sep = "_")
      if (input_temp_1 %in% knot_temp) {
        d1<-input$pmm_d1
        d2<-input$pmm_d2
      } else if (input_temp_2 %in% knot_temp){
        d1<-input$pmm_d2
        d2<-input$pmm_d1
      } else if (input_temp_2 %in% knot_temp & (input_temp_1 %in% knot_temp)){
        d1<-input$pmm_d1
        d2<-input$pmm_d2
      }
      
      mixfit <- flexsurvmixture(survpmm ~ 1, dists = c(d1,d2), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
      pmm_df_ex<-summary(mixfit, t = Newtime$Time)[[1]]
      pmm_df3<-data.frame(Time=pmm_df_ex$time,
                      Surv=pmm_df_ex$est)
      fit<-survfit(Surv(recyrs,censrec==1)~1,data=gef_MM)
      km_pmm<-data.frame(Time=fit$time,
                         Surv=fit$surv,ymin=fit$lower,ymax=fit$upper)
      km_ribbon<-data.frame(ymin=fit$lower,ymax=fit$upper)

      if (d1 == "exp" & d2 %in% c("weibull","gamma","lnorm","gompertz","llogis")) {
        pmm_df1<-data.frame(Time=Newtime$Time,
                        Surv=1-pexp(Newtime$Time, mixfit$res[2]))
          if (d2 == "weibull"){
            pmm_df2<-data.frame(Time=Newtime$Time,
                                Surv=1-pweibull(Newtime$Time, mixfit$res[3], mixfit$res[4]))
          } else if (d2 == "gamma") {
            pmm_df2<-data.frame(Time=Newtime$Time,
                                Surv=1-pgamma(Newtime$Time, mixfit$res[3], mixfit$res[4]))  
          } else if (d2 == "lnorm") {
            pmm_df2<-data.frame(Time=Newtime$Time,
                                Surv=1-plnorm(Newtime$Time, mixfit$res[3], mixfit$res[4]))  
          } else if (d2 == "gompertz") {
            pmm_df2<-data.frame(Time=Newtime$Time,
                                Surv=1-pgompertz(Newtime$Time, mixfit$res[3], mixfit$res[4]))  
          } else if (d2 == "llogis") {
            pmm_df2<-data.frame(Time=Newtime$Time,
                                Surv=1-pllogis(Newtime$Time, mixfit$res[3], mixfit$res[4]))  
          }
      } else if (d1 == "exp" & d2 == "gengamma") {
        pmm_df1<-data.frame(Time=Newtime$Time,
                            Surv=1-pexp(Newtime$Time, mixfit$res[2]))
        pmm_df2<-data.frame(Time=Newtime$Time,
                            Surv=1-pgengamma(Newtime$Time, mixfit$res[3], mixfit$res[4], mixfit$res[5]))
      } else if (d1 == "exp" & d2 == "exp") {
        pmm_df1<-data.frame(Time=Newtime$Time,
                            Surv=1-pexp(Newtime$Time, mixfit$res[2]))
        pmm_df2<-data.frame(Time=Newtime$Time,
                            Surv=1-pexp(Newtime$Time, mixfit$res[3]))
      } else if ((d1 %in% c("weibull","gamma","lnorm","gompertz","llogis")) & (d2 %in% c("weibull","gamma","lnorm","gompertz","llogis"))) {
        if (d1 == "weibull"){
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pweibull(Newtime$Time, mixfit$res[2], mixfit$res[3]))
        } else if (d1 == "gamma") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pgamma(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        } else if (d1 == "lnorm") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-plnorm(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        } else if (d1 == "gompertz") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pgompertz(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        } else if (d1 == "llogis") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pllogis(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        }
        if (d2 == "weibull"){
          pmm_df2<-data.frame(Time=Newtime$Time,
                              Surv=1-pweibull(Newtime$Time, mixfit$res[4], mixfit$res[5]))
        } else if (d2 == "gamma") {
          pmm_df2<-data.frame(Time=Newtime$Time,
                              Surv=1-pgamma(Newtime$Time, mixfit$res[4], mixfit$res[5]))  
        } else if (d2 == "lnorm") {
          pmm_df2<-data.frame(Time=Newtime$Time,
                              Surv=1-plnorm(Newtime$Time, mixfit$res[4], mixfit$res[5]))  
        } else if (d2 == "gompertz") {
          pmm_df2<-data.frame(Time=Newtime$Time,
                              Surv=1-pgompertz(Newtime$Time, mixfit$res[4], mixfit$res[5]))  
        } else if (d2 == "llogis") {
          pmm_df2<-data.frame(Time=Newtime$Time,
                              Surv=1-pllogis(Newtime$Time, mixfit$res[4], mixfit$res[5]))  
        }
      } else if ((d1 %in% c("weibull","gamma","lnorm","gompertz","llogis")) & (d2 == "gengamma")) {
        if (d1 == "weibull"){
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pweibull(Newtime$Time, mixfit$res[2], mixfit$res[3]))
        } else if (d1 == "gamma") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pgamma(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        } else if (d1 == "lnorm") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-plnorm(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        } else if (d1 == "gompertz") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pgompertz(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        } else if (d1 == "llogis") {
          pmm_df1<-data.frame(Time=Newtime$Time,
                              Surv=1-pllogis(Newtime$Time, mixfit$res[2], mixfit$res[3]))  
        }
        pmm_df2<-data.frame(Time=Newtime$Time,
                            Surv=1-pgengamma(Newtime$Time, mixfit$res[4], mixfit$res[5], mixfit$res[6]))
      } else if ((d1 == "gengamma") & (d2 == "gengamma")) {
        pmm_df1<-data.frame(Time=Newtime$Time,
                            Surv=1-pgengamma(Newtime$Time, mixfit$res[2], mixfit$res[3], mixfit$res[4]))
        pmm_df2<-data.frame(Time=Newtime$Time,
                            Surv=1-pgengamma(Newtime$Time, mixfit$res[5], mixfit$res[6], mixfit$res[7]))
      }
      
      f_pmm_plot<-ggplot(data = km_pmm,aes(x=Time,y=Surv))+
                    geom_line(data = km_pmm,aes(x=Time,y=Surv),color="black",size=1)+
                    geom_ribbon(aes(ymin=ymin,ymax=ymax),alpha=0.1,color="black",linetype=2,fill="pink")+
                    geom_line(data=pmm_df1,aes(x=Time,y=Surv),color="skyblue",size=1)+
                    geom_line(data=pmm_df2,aes(x=Time,y=Surv),color="green",size=1)+
                    geom_line(data=pmm_df3,aes(x=Time,y=Surv),color="red",size=1)+
                    expand_limits(y=c(0,1),x=c(0,p_time)) + 
                    ylab("Survival rate") +
                    xlab("Time(Years)") +
                    theme(legend.position = "bottom") + 
                    theme_bw()      
      f_pmm_plot<-f_pmm_plot + ggtitle(input$ggtitle_pmm1)
      print(f_pmm_plot)
      values$graph_pmm1<-f_pmm_plot
    })
    
    output$downloadtable_dfhaz_pmm <- downloadHandler(
      filename = function() {
        paste("data_haz", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfHaz, file,row.names = FALSE)
      }
    )
    
    output$downloadtable_dfsurv_pmm <- downloadHandler(
      filename = function() {
        paste("data_surv", ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfSurv, file,row.names = FALSE)
      }
    )
    
    task_pmm(100)
    
  })
  
  ### download plot pmm
  observeEvent(input$dwld_pmm, {
    output$titlebut_pmm <- renderUI({
      textInput("ggtitle_pmm", "Title", value = input$tipo)
    })
  })
  
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_pmm, {
    output$sizebut_pmm <- renderUI({
      bt_pmm <- tagList()
      bt_pmm[[1]] <- numericInput("height_pmm", "Height (px)", value = 1600)
      bt_pmm[[2]] <- numericInput("width_pmm", "Width (px)", value = 2800)
      bt_pmm
    })
  })
  
  # Download back-end
  output$downloadPlot_pmm <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph_pmm, width = as.numeric(input$width_pmm),
             height = as.numeric(input$height_pmm),
             units = "px")
    }
  ) 
  
  ### download plot pmm1
  observeEvent(input$dwld_pmm1, {
    output$titlebut_pmm1 <- renderUI({
      textInput("ggtitle_pmm1", "Title", value = input$tipo)
    })
  })
  
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_pmm1, {
    output$sizebut_pmm1 <- renderUI({
      bt_pmm1 <- tagList()
      bt_pmm1[[1]] <- numericInput("height_pmm1", "Height (px)", value = 1600)
      bt_pmm1[[2]] <- numericInput("width_pmm1", "Width (px)", value = 2800)
      bt_pmm1
    })
  })
  
  # Download back-end
  output$downloadPlot_pmm1 <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph_pmm1, width = as.numeric(input$width_pmm1),
             height = as.numeric(input$height_pmm1),
             units = "px")
    }
  )  
  #----
  ### MCM
  #----
  observeEvent(input$updatedata, {
    output$runmodelmcm1 <- renderUI({ 
      actionButton("runmodelmcm", label="Run the model",icon ("play"),style="color: black;
                       background-color: royalblue; border-color: #2e6da4")%>%
        helper(type = "markdown", content = "rm_mcm")
    })
  })
  
   observe({
    infile <- input$upload_mcm
    if (!is.null(infile)){
      tryCatch({
        values$sheet_mcm <- read_excel(infile$datapath, sheet = 1)[,1:5]
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    output$sheet_mcm<-renderDT(datatable(values$sheet_mcm, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    updateCheckboxInput(session,"show_table_mcm1",value=TRUE)
  })
  
  observeEvent(input$loadtemplate_mcm, {
    wd <- getwd()
    infile <- c()
    infile$datapath <- paste0(wd,"/www/files/example_mcm.xlsx")
    if (!is.null(infile)){
      tryCatch({
        values$sheet_mcm <- read_excel(infile$datapath, sheet = 1)[,1:5]
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    output$sheet_mcm<-renderDT(datatable(values$sheet_mcm, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    updateCheckboxInput(session,"show_table_mcm1",value=TRUE)
  }) 
  
    observeEvent(input$updatedata, {
      output$mcm_age1 <- renderUI({
        numericInput("mcm_age", "Average age of the population", value=64)
      })
    })
   
    haz_rate = function(x, t, out = "prob"){ 
      tmp  = t - lag(t, default = 0)
      if (out == "rate"){
        y = case_when(x == 1 ~ 1, TRUE ~ - (log(1 - x)) / tmp)
      } else if (out == "prob") {
        y = 1 - exp(- x * tmp)
      } else {
        "error!"
      }
      return (y)
    }
    
    observeEvent(input$runmodelmcm,{
      mystep<-as.numeric(input$MyStep)
      ltHaz<-values$ltHaz
      Newtime<-values$Newtime
      Newtime2<-values$Newtime2
      gef_in<-values$ssm_input 
      llCons <- sum(ltHaz$Events*log(ltHaz$AtRisk) - log(factorial(ltHaz$Events)))
      df_cure<-data.frame(as.numeric(values$folup),gef_in$censrec,gef_in$recyrs)
      colnames(df_cure)<-c("Follow_Up","Censor","Obs_surv")
      mcm_haz_data<-values$sheet_mcm
      mcm_bh = filter(mcm_haz_data, Year == mcm_haz_data$Year[1]) %>% mutate(tmp = Age,
                                                          Age = as.numeric(tmp)[tmp],
                                                          Hazard = haz_rate(qx, 1, "rate"),  # All observations 1 unit apart
                                                          Surv = (lx - dx) / lx[1],
                                                          Years = Age - input$mcm_age)
      b_haz<-mcm_bh
      b_haz<-filter(b_haz,Age>=input$mcm_age)
      values$b_haz<-b_haz
      
      tmp = select(mcm_bh, Year, Age, Hazard)
      my_df_ipd<-df_cure
      my_df_ipd=my_df_ipd %>% mutate(Year = mcm_haz_data$Year[1], Age = floor(input$mcm_age + Obs_surv))
      my_df_ipd<-left_join(my_df_ipd, tmp, by = c("Year", "Age"))
      
      MyDists <- list("exp","weibull","lnorm","llogis")
      dfGOF <- data.frame(matrix(, nrow=4, ncol=6))
      colnames(dfGOF) <- c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC")
      dfHazEst <- array(dim=c(4, values$MyN))
      dfHazEst2 <- array(dim=c(4, length(ltHaz$Time)))
      dfHazEst3 <- array(dim=c(4, mystep*30))
      for (i in 1:4){
        glmTemp <- flexsurvcure(Surv(Obs_surv, Censor) ~ 1, data = my_df_ipd, link="logistic", bhazard = Hazard,mixture=T, dist = MyDists[[i]])
        dfHazEst[i,] <- summary(glmTemp,type="hazard",t=exp(Newtime$lnTime))[[1]]$est/mystep
        dfHazEst2[i,] <- summary(glmTemp,type="hazard",t=ltHaz$Time)[[1]]$est/mystep        
        dfHazEst3[i,] <- summary(glmTemp,type="hazard",t=exp(Newtime2$lnTime))[[1]]$est/mystep
        dfGOF[i,1] <- MyDists[[i]]
        dfGOF[i,2] <- round(sum(ltHaz$Events*log(dfHazEst2[i,]) - dfHazEst2[i,]*ltHaz$AtRisk) + llCons,2)
        dfGOF[i,3] <- glmTemp$npars
        coeff_temp<-as.data.frame(glmTemp$res[,1])
        t1<-"Param"
        for (j in 1:length(glmTemp$res[,1])) {
          t0<-paste(rownames(coeff_temp)[j],round(coeff_temp[j,1],2),sep = "=")
          t1<-paste(t1,t0,sep = ";")
        }
        dfGOF[i,4] <- t1
      }
      dfGOF$AIC <- -2*dfGOF$'Log-Likelihood' + 2*dfGOF$Parameters
      dfGOF$BIC <- -2*dfGOF$'Log-Likelihood' + round(log(length(ltHaz$Time)),2)*dfGOF$Parameters
      # dfGOF<-values$dfGOF_ssm
      output$mcm_aic <- renderDT(datatable(dfGOF, editable = FALSE, rownames = FALSE,
                                           colnames = c("Model","Log-Likelihood","Parameters","Coefficients","AIC","BIC"),
                                           options = list(dom = 'tp')))
      values$mcm_dfhazard1<-dfHazEst
      values$dfHazEst_mcmsum<-dfHazEst3
      
      shinyalert(title = "Complete!", type = "success")
      
      output$downloadtable_mcm1 <- downloadHandler(
        filename = function() {
          paste("data_1", ".csv", sep="")
        },
        content = function(file) {
          write.csv(dfGOF, file,row.names = FALSE)
        }
      )
      
      task_mcm(50)

    })
    ### draw plot button
    observeEvent(input$runmodelmcm, {
      output$plotmcm1 <- renderUI({ 
        actionButton("plotmcm", label="Draw the plot",icon ("play"),style="color: black;
                       background-color: royalblue; border-color: #2e6da4")
      })
    })
    
    ### mcm plot
    observeEvent(input$plotmcm, {
      Newtime2<-values$Newtime2
      Newtime<-values$Newtime
      ltHaz<-values$ltHaz
      dfHazEst<- as.data.frame(values$mcm_dfhazard1)
      dfHazEst_mcmsum<- as.data.frame(values$dfHazEst_mcmsum)
      
      dfHaz <- t(dfHazEst)
      dfHaz_mcmsum <- t(dfHazEst_mcmsum)
      
      dfHaz <- cbind(data.frame(Newtime$lnTime), dfHaz)
      dfHaz_mcmsum <- cbind(data.frame(Newtime2$lnTime), dfHaz_mcmsum)
      
      dfHaz$Time <- exp(as.numeric(dfHaz$Newtime.lnTime))
      dfHaz_mcmsum$Time <- exp(as.numeric(dfHaz_mcmsum$Newtime2.lnTime))
      
      dfHaz <- as.data.frame(dfHaz[,-1])
      dfHaz_mcmsum <- as.data.frame(dfHaz_mcmsum[,-1])
      
      colnames(dfHaz) <- c("Exponential","Weibull","Log-Normal","Log-Logistic","Time")
      colnames(dfHaz_mcmsum) <- c("Exponential","Weibull","Log-Normal","Log-Logistic","Time")

      dfSurv<-as.data.frame(matrix(nrow = length(dfHaz$Time),ncol = 5))
      dfSurv[[1]]<-dfHaz$Time
      for ( i in 2:5 ) {
        dftemp<-data.frame(dfHaz$Time,dfHaz[[(i-1)]])
        dftemp<-dftemp %>%
          dplyr::arrange(dftemp[[1]]) %>%
          dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
          dplyr::mutate(survProp = exp(-1*cumhaz))
        dfSurv[[i]]<-dftemp[[4]]
      }
      
      dfSurv_mcmsum<-as.data.frame(matrix(nrow = length(dfHaz_mcmsum$Time),ncol = 5))
      dfSurv_mcmsum[[1]]<-dfHaz_mcmsum$Time
      for ( i in 2:5 ) {
        dftemp<-data.frame(dfHaz_mcmsum$Time,dfHaz_mcmsum[[(i-1)]])
        dftemp<-dftemp %>%
          dplyr::arrange(dftemp[[1]]) %>%
          dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
          dplyr::mutate(survProp = exp(-1*cumhaz))
        dfSurv_mcmsum[[i]]<-dftemp[[4]]
      }

      colnames(dfSurv)<-c("Time","Exponential","Weibull","Log-Normal","Log-Logistic")
      colnames(dfSurv_mcmsum)<-c("Time","mcm_Exponential","mcm_Weibull","mcm_Log-Normal","mcm_Log-Logistic")
      
      dfFigSurv = dfSurv %>%
        gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
      dfSurv_in<-dfSurv[1:round(length(ltHaz$Time),0),]
      dfFigSurv_in = dfSurv_in %>%
        gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
      
      dfFigSurv_mcmsum = dfSurv_mcmsum %>%
        gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))
      
      values$mcm_dfHaz<-dfHaz
      values$mcm_dfSurv<-dfSurv
      values$mcm_dfFigSurv<-dfFigSurv
      values$mcm_dfFigSurv_in<-dfFigSurv_in
      values$dfFigSurv_mcmsum<-dfFigSurv_mcmsum
      
      output$plot_mcm<-renderPlot({
        
        if (input$time_mcm == 1 & input$model_mcm == "Combination"){
          f_surv1= ggplot() +
            geom_line(data=dfFigSurv_in, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
            geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
            scale_color_discrete(name="Model")+
            expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
            # facet_wrap(~Model,nrow=4)+
            scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
            ylab("Survival Probability") +
            xlab("Time(Years)") +
            guides(color = guide_legend(ncol = 1))  +
            theme(legend.position = "bottom") +
            theme_bw()
        } else if (input$time_mcm == 2 & input$model_mcm == "Combination"){
          f_surv1= ggplot() +
            geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
            geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
            scale_color_discrete(name="Model")+
            expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
            # facet_wrap(~Model,nrow=4)+
            scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
            ylab("Survival Probability") +
            xlab("Time(Years)") +
            guides(color = guide_legend(ncol = 1))  +
            theme(legend.position = "bottom") +
            theme_bw()
        } else if (input$time_mcm == 1 & input$model_mcm %in% c("Exponential","Weibull","Log-Normal","Log-Logistic")){
          f_surv1= ggplot() +
            geom_line(data=filter(dfFigSurv_in,Model==input$model_mcm), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
            geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
            scale_color_discrete(name="Model")+
            expand_limits(y=c(0,1),x=c(0,as.numeric(values$folup))) +
            # facet_wrap(~Model,nrow=4)+
            scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(values$folup),by = 1))) +
            ylab("Survival Probability") +
            xlab("Time(Years)") +
            guides(color = guide_legend(ncol = 1))  +
            theme(legend.position = "bottom") +
            theme_bw()
        } else if (input$time_mcm == 2 & input$model_mcm %in% c("Exponential","Weibull","Log-Normal","Log-Logistic")){
          f_surv1= ggplot() +
            geom_line(data=filter(dfFigSurv,Model==input$model_mcm), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
            geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
            scale_color_discrete(name="Model")+
            expand_limits(y=c(0,1),x=c(0,as.numeric(input$MyTH))) +
            # facet_wrap(~Model,nrow=4)+
            scale_x_continuous(breaks = c(seq(from=0, to=as.numeric(input$MyTH),by = 1))) +
            ylab("Survival Probability") +
            xlab("Time(Years)") +
            guides(color = guide_legend(ncol = 1))  +
            theme(legend.position = "bottom") +
            theme_bw()
        }
        f_surv1<-f_surv1 + ggtitle(input$ggtitle_mcm)
        print(f_surv1)
        values$graph_mcm<-f_surv1
        
      })
      
      output$downloadtable_dfhaz_mcm <- downloadHandler(
        filename = function() {
          paste("data_haz", ".csv", sep="")
        },
        content = function(file) {
          write.csv(dfHaz, file,row.names = FALSE)
        }
      )
      
      output$downloadtable_dfsurv_mcm <- downloadHandler(
        filename = function() {
          paste("data_surv", ".csv", sep="")
        },
        content = function(file) {
          write.csv(dfSurv, file,row.names = FALSE)
        }
      )
      
      task_mcm(100)
      
    })
    ### download plot
    observeEvent(input$dwld_mcm, {
      output$titlebut_mcm <- renderUI({
        textInput("ggtitle_mcm", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$dwld_mcm, {
      output$sizebut_mcm <- renderUI({
        bt_mcm <- tagList()
        bt_mcm[[1]] <- numericInput("height_mcm", "Height (px)", value = 1600)
        bt_mcm[[2]] <- numericInput("width_mcm", "Width (px)", value = 2800)
        bt_mcm
      })
    })
    
    # Download back-end
    output$downloadPlot_mcm <- downloadHandler(
      filename = function() { paste0('survival_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_mcm, width = as.numeric(input$width_mcm),
               height = as.numeric(input$height_mcm),
               units = "px")
      }
    )  
   #----
   ### VI
   #----    

  observeEvent(input$loaddata_vi,{
    df_vi_ssm<-values$dfFigSurv_ssmsum
    df_vi_fp<-values$dfFigSurv_fpsum
    df_vi_rcs<-values$dfFigSurv_rcssum
    df_vi_rp<-values$dfFigSurv_rpsum
    df_vi_gam<-values$dfFigSurv_gamsum
    df_vi_pmm<-values$dfFigSurv_pmmsum
    df_vi_mcm<-values$dfFigSurv_mcmsum
    
    if (is.null(df_vi_ssm) | is.null(df_vi_fp) | is.null(df_vi_rcs) | is.null(df_vi_rp) | is.null(df_vi_gam) | is.null(df_vi_pmm) | is.null(df_vi_mcm)){
      shinyalert("Warning!", "You should run all the models and draw the plots before drawing the plots!", type = "error")
    } else {
      shinyalert(title = "Complete!", type = "success")
    }
    
    df_vi<-rbind(df_vi_ssm, df_vi_fp,df_vi_rcs,df_vi_rp,df_vi_gam,df_vi_pmm,df_vi_mcm)
    values$sheet_vi<-df_vi
    output$sheet_vi<-renderDT(datatable(values$sheet_vi, editable = FALSE, rownames = FALSE,options = list(pageLength = 10)))
    output$downloadtable_df_vi <- downloadHandler(
      filename = function() {
        paste("data_surv_vi", ".csv", sep="")
      },
      content = function(file) {
        write.csv(df_vi, file,row.names = FALSE)
      }
    )
    
  })
  
  output$plot_vi<-renderPlot({
    f_surv1= ggplot() +
      geom_line(data=values$ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
      expand_limits(y=c(0,1))+
      theme(legend.position = "bottom") +
      theme_bw()
      print(f_surv1)
  })
  
  observeEvent(input$vi_plotbt,{
    ltHaz<-values$ltHaz
    b_haz<-values$b_haz
    b_haz$Age<-b_haz$Age-input$mcm_age
    if (is.null(values$sheet_vi)){
      shinyalert("Warning!", "You should run all the models and draw the plots before drawing the plots!", type = "error")
    } else {
      if (input$plot_vi_x == 5){
        values$df_vi_p<-filter(values$sheet_vi,Time<=5)
        b_haz<-filter(b_haz,Age<=5)
      } else if (input$plot_vi_x == 10){
        values$df_vi_p<-filter(values$sheet_vi,Time<=10)
        b_haz<-filter(b_haz,Age<=10)
      } else if (input$plot_vi_x == 15){
        values$df_vi_p<-filter(values$sheet_vi,Time<=15)
        b_haz<-filter(b_haz,Age<=15)
      } else if (input$plot_vi_x == 20){
        values$df_vi_p<-filter(values$sheet_vi,Time<=20)
        b_haz<-filter(b_haz,Age<=20)
      } else if (input$plot_vi_x == 25){
        values$df_vi_p<-filter(values$sheet_vi,Time<=25)
        b_haz<-filter(b_haz,Age<=25)
      } else if (input$plot_vi_x == 30){
        values$df_vi_p<-filter(values$sheet_vi,Time<=30)
        b_haz<-filter(b_haz,Age<=30)
      }
    }

    df_all_model<-data.frame(Model=c("ssm_Exponential","ssm_Weibull","ssm_Gamma","ssm_Log-Normal","ssm_Gompertz","ssm_Log-Logistic","ssm_Generalized Gamma","ssm_Generalized F",
                                     "fp_-2","fp_-1","fp_-0.5","fp_0.5","fp_1","fp_2","fp_3","fp_0",
                                     "fp_-2,-1","fp_-2,-0.5","fp_-2,0.5","fp_-2,1","fp_-2,2","fp_-2,3",
                                     "fp_-1,-0.5","fp_-1,0.5","fp_-1,1","fp_-1,2","fp_-1,3",
                                     "fp_-0.5,0.5","fp_-0.5,1","fp_-0.5,2","fp_-0.5,3","fp_0.5,1","fp_0.5,2","fp_0.5,3",
                                     "fp_1,2","fp_1,3","fp_2,3","fp_-2,-2","fp_-1,-1","fp_-0.5,-0.5","fp_0.5,0.5",
                                     "fp_1,1","fp_2,2","fp_3,3","fp_-2,0","fp_-1,0","fp_-0.5,0","fp_0.5,0","fp_1,0","fp_2,0","fp_3,0",
                                     "fp_0,0","rcs_1","rcs_2","rcs_3","rcs_4","rcs_5",
                                     "rp_hazard0","rp_hazard1","rp_hazard2","rp_hazard3","rp_hazard4","rp_hazard5",
                                     "rp_odds0","rp_odds1","rp_odds2","rp_odds3","rp_odds4","rp_odds5",
                                     "rp_normal0","rp_normal1","rp_normal2","rp_normal3","rp_normal4","rp_normal5",
                                     "gam_1","gam_2","gam_3","gam_4","gam_5","gam_6","gam_7","gam_8","gam_9","gam_10",
                                     "pmm_exp_exp","pmm_exp_weibull","pmm_exp_gamma","pmm_exp_lnorm","pmm_exp_gompertz","pmm_exp_llogis","pmm_exp_gengamma",
                                     "pmm_weibull_weibull","pmm_weibull_gamma","pmm_weibull_lnorm","pmm_weibull_gompertz","pmm_weibull_llogis","pmm_weibull_gengamma",
                                     "pmm_gamma_gamma","pmm_gamma_lnorm","pmm_gamma_gompertz","pmm_gamma_llogis",
                                     "pmm_gamma_gengamma","pmm_lnorm_lnorm","pmm_lnorm_gompertz","pmm_lnorm_llogis","pmm_lnorm_gengamma",
                                     "pmm_gompertz_gompertz","pmm_gompertz_llogis","pmm_gompertz_gengamma","pmm_llogis_llogis","pmm_llogis_gengamma",
                                     "pmm_gengamma_gengamma","mcm_Exponential","mcm_Weibull","mcm_Log-Normal","mcm_Log-Logistic"),choose=array(dim = 117))
    
    # if (input$ssm_Exponential == TRUE){df_all_model$choose[1]=1} else {df_all_model$choose[1]=0}
    if (input$ssm_Exponential == TRUE){df_all_model$choose[1]=1} else {df_all_model$choose[1]=0}
    if (input$ssm_Weibull == TRUE){df_all_model$choose[2]=1} else {df_all_model$choose[2]=0}
    if (input$ssm_Gamma == TRUE){df_all_model$choose[3]=1} else {df_all_model$choose[3]=0}
    if (input$'ssm_Log-Normal' == TRUE){df_all_model$choose[4]=1} else {df_all_model$choose[4]=0}
    if (input$'ssm_Gompertz' == TRUE){df_all_model$choose[5]=1} else {df_all_model$choose[5]=0}
    if (input$'ssm_Log-Logistic' == TRUE){df_all_model$choose[6]=1} else {df_all_model$choose[6]=0}
    if (input$'ssm_Generalized Gamma' == TRUE){df_all_model$choose[7]=1} else {df_all_model$choose[7]=0}
    if (input$'ssm_Generalized F' == TRUE){df_all_model$choose[8]=1} else {df_all_model$choose[8]=0}
    if (input$'fp_-2' == TRUE){df_all_model$choose[9]=1} else {df_all_model$choose[9]=0}
    if (input$'fp_-1' == TRUE){df_all_model$choose[10]=1} else {df_all_model$choose[10]=0}
    if (input$'fp_-0.5' == TRUE){df_all_model$choose[11]=1} else {df_all_model$choose[11]=0}
    if (input$'fp_0.5' == TRUE){df_all_model$choose[12]=1} else {df_all_model$choose[12]=0}
    if (input$'fp_1' == TRUE){df_all_model$choose[13]=1} else {df_all_model$choose[13]=0}
    if (input$'fp_2' == TRUE){df_all_model$choose[14]=1} else {df_all_model$choose[14]=0}
    if (input$'fp_3' == TRUE){df_all_model$choose[15]=1} else {df_all_model$choose[15]=0}
    if (input$'fp_0' == TRUE){df_all_model$choose[16]=1} else {df_all_model$choose[16]=0}
    if (input$'fp_-2,-1' == TRUE){df_all_model$choose[17]=1} else {df_all_model$choose[17]=0}
    if (input$'fp_-2,-0.5' == TRUE){df_all_model$choose[18]=1} else {df_all_model$choose[18]=0}
    if (input$'fp_-2,0.5' == TRUE){df_all_model$choose[19]=1} else {df_all_model$choose[19]=0}
    if (input$'fp_-2,1' == TRUE){df_all_model$choose[20]=1} else {df_all_model$choose[20]=0}
    if (input$'fp_-2,2' == TRUE){df_all_model$choose[21]=1} else {df_all_model$choose[21]=0}
    if (input$'fp_-2,3' == TRUE){df_all_model$choose[22]=1} else {df_all_model$choose[22]=0}
    if (input$'fp_-1,-0.5' == TRUE){df_all_model$choose[23]=1} else {df_all_model$choose[23]=0}
    if (input$'fp_-1,0.5' == TRUE){df_all_model$choose[24]=1} else {df_all_model$choose[24]=0}
    if (input$'fp_-1,1' == TRUE){df_all_model$choose[25]=1} else {df_all_model$choose[25]=0}
    if (input$'fp_-1,2' == TRUE){df_all_model$choose[26]=1} else {df_all_model$choose[26]=0}
    if (input$'fp_-1,3' == TRUE){df_all_model$choose[27]=1} else {df_all_model$choose[27]=0}
    if (input$'fp_-0.5,0.5' == TRUE){df_all_model$choose[28]=1} else {df_all_model$choose[28]=0}
    if (input$'fp_-0.5,1' == TRUE){df_all_model$choose[29]=1} else {df_all_model$choose[29]=0}
    if (input$'fp_-0.5,2' == TRUE){df_all_model$choose[30]=1} else {df_all_model$choose[30]=0}
    if (input$'fp_-0.5,3' == TRUE){df_all_model$choose[31]=1} else {df_all_model$choose[31]=0}
    if (input$'fp_0.5,1' == TRUE){df_all_model$choose[32]=1} else {df_all_model$choose[32]=0}
    if (input$'fp_0.5,2' == TRUE){df_all_model$choose[33]=1} else {df_all_model$choose[33]=0}
    if (input$'fp_0.5,3' == TRUE){df_all_model$choose[34]=1} else {df_all_model$choose[34]=0}
    if (input$'fp_1,2' == TRUE){df_all_model$choose[35]=1} else {df_all_model$choose[35]=0}
    if (input$'fp_1,3' == TRUE){df_all_model$choose[36]=1} else {df_all_model$choose[36]=0}
    if (input$'fp_2,3' == TRUE){df_all_model$choose[37]=1} else {df_all_model$choose[37]=0}
    if (input$'fp_-2,-2' == TRUE){df_all_model$choose[38]=1} else {df_all_model$choose[38]=0}
    if (input$'fp_-1,-1' == TRUE){df_all_model$choose[39]=1} else {df_all_model$choose[39]=0}
    if (input$'fp_-0.5,-0.5' == TRUE){df_all_model$choose[40]=1} else {df_all_model$choose[40]=0}
    if (input$'fp_0.5,0.5' == TRUE){df_all_model$choose[41]=1} else {df_all_model$choose[41]=0}
    if (input$'fp_1,1' == TRUE){df_all_model$choose[42]=1} else {df_all_model$choose[42]=0}
    if (input$'fp_2,2' == TRUE){df_all_model$choose[43]=1} else {df_all_model$choose[43]=0}
    if (input$'fp_3,3' == TRUE){df_all_model$choose[44]=1} else {df_all_model$choose[44]=0}
    if (input$'fp_-2,0' == TRUE){df_all_model$choose[45]=1} else {df_all_model$choose[45]=0}
    if (input$'fp_-1,0' == TRUE){df_all_model$choose[46]=1} else {df_all_model$choose[46]=0}
    if (input$'fp_-0.5,0' == TRUE){df_all_model$choose[47]=1} else {df_all_model$choose[47]=0}
    if (input$'fp_0.5,0' == TRUE){df_all_model$choose[48]=1} else {df_all_model$choose[48]=0}
    if (input$'fp_1,0' == TRUE){df_all_model$choose[49]=1} else {df_all_model$choose[49]=0}
    if (input$'fp_2,0' == TRUE){df_all_model$choose[50]=1} else {df_all_model$choose[50]=0}
    if (input$'fp_3,0' == TRUE){df_all_model$choose[51]=1} else {df_all_model$choose[51]=0}
    if (input$'fp_0,0' == TRUE){df_all_model$choose[52]=1} else {df_all_model$choose[52]=0}
    if (input$rcs_1 == TRUE){df_all_model$choose[53]=1} else {df_all_model$choose[53]=0}
    if (input$rcs_2 == TRUE){df_all_model$choose[54]=1} else {df_all_model$choose[54]=0}
    if (input$rcs_3 == TRUE){df_all_model$choose[55]=1} else {df_all_model$choose[55]=0}
    if (input$rcs_4 == TRUE){df_all_model$choose[56]=1} else {df_all_model$choose[56]=0}
    if (input$rcs_5 == TRUE){df_all_model$choose[57]=1} else {df_all_model$choose[57]=0}
    if (input$rp_hazard0 == TRUE){df_all_model$choose[58]=1} else {df_all_model$choose[58]=0}
    if (input$rp_hazard1 == TRUE){df_all_model$choose[59]=1} else {df_all_model$choose[59]=0}
    if (input$rp_hazard2 == TRUE){df_all_model$choose[60]=1} else {df_all_model$choose[60]=0}
    if (input$rp_hazard3 == TRUE){df_all_model$choose[61]=1} else {df_all_model$choose[61]=0}
    if (input$rp_hazard4 == TRUE){df_all_model$choose[62]=1} else {df_all_model$choose[62]=0}
    if (input$rp_hazard5 == TRUE){df_all_model$choose[63]=1} else {df_all_model$choose[63]=0}
    if (input$rp_odds0 == TRUE){df_all_model$choose[64]=1} else {df_all_model$choose[64]=0}
    if (input$rp_odds1 == TRUE){df_all_model$choose[65]=1} else {df_all_model$choose[65]=0}
    if (input$rp_odds2 == TRUE){df_all_model$choose[66]=1} else {df_all_model$choose[66]=0}
    if (input$rp_odds3 == TRUE){df_all_model$choose[67]=1} else {df_all_model$choose[67]=0}
    if (input$rp_odds4 == TRUE){df_all_model$choose[68]=1} else {df_all_model$choose[68]=0}
    if (input$rp_odds5 == TRUE){df_all_model$choose[69]=1} else {df_all_model$choose[69]=0}
    if (input$rp_normal0 == TRUE){df_all_model$choose[70]=1} else {df_all_model$choose[70]=0}
    if (input$rp_normal1 == TRUE){df_all_model$choose[71]=1} else {df_all_model$choose[71]=0}
    if (input$rp_normal2 == TRUE){df_all_model$choose[72]=1} else {df_all_model$choose[72]=0}
    if (input$rp_normal3 == TRUE){df_all_model$choose[73]=1} else {df_all_model$choose[73]=0}
    if (input$rp_normal4 == TRUE){df_all_model$choose[74]=1} else {df_all_model$choose[74]=0}
    if (input$rp_normal5 == TRUE){df_all_model$choose[75]=1} else {df_all_model$choose[75]=0}
    if (input$gam_1 == TRUE){df_all_model$choose[76]=1} else {df_all_model$choose[76]=0}
    if (input$gam_2 == TRUE){df_all_model$choose[77]=1} else {df_all_model$choose[77]=0}
    if (input$gam_3 == TRUE){df_all_model$choose[78]=1} else {df_all_model$choose[78]=0}
    if (input$gam_4 == TRUE){df_all_model$choose[79]=1} else {df_all_model$choose[79]=0}
    if (input$gam_5 == TRUE){df_all_model$choose[80]=1} else {df_all_model$choose[80]=0}
    if (input$gam_6 == TRUE){df_all_model$choose[81]=1} else {df_all_model$choose[81]=0}
    if (input$gam_7 == TRUE){df_all_model$choose[82]=1} else {df_all_model$choose[82]=0}
    if (input$gam_8 == TRUE){df_all_model$choose[83]=1} else {df_all_model$choose[83]=0}
    if (input$gam_9 == TRUE){df_all_model$choose[84]=1} else {df_all_model$choose[84]=0}
    if (input$gam_10 == TRUE){df_all_model$choose[85]=1} else {df_all_model$choose[85]=0}
    if (input$pmm_exp_exp == TRUE){df_all_model$choose[86]=1} else {df_all_model$choose[86]=0}
    if (input$pmm_exp_weibull == TRUE){df_all_model$choose[87]=1} else {df_all_model$choose[87]=0}
    if (input$pmm_exp_gamma == TRUE){df_all_model$choose[88]=1} else {df_all_model$choose[88]=0}
    if (input$pmm_exp_lnorm == TRUE){df_all_model$choose[89]=1} else {df_all_model$choose[89]=0}
    if (input$pmm_exp_gompertz == TRUE){df_all_model$choose[90]=1} else {df_all_model$choose[90]=0}
    if (input$pmm_exp_llogis == TRUE){df_all_model$choose[91]=1} else {df_all_model$choose[91]=0}
    if (input$pmm_exp_gengamma == TRUE){df_all_model$choose[92]=1} else {df_all_model$choose[92]=0}
    if (input$pmm_weibull_weibull == TRUE){df_all_model$choose[93]=1} else {df_all_model$choose[93]=0}
    if (input$pmm_weibull_gamma == TRUE){df_all_model$choose[94]=1} else {df_all_model$choose[94]=0}
    if (input$pmm_weibull_lnorm == TRUE){df_all_model$choose[95]=1} else {df_all_model$choose[95]=0}
    if (input$pmm_weibull_gompertz == TRUE){df_all_model$choose[96]=1} else {df_all_model$choose[96]=0}
    if (input$pmm_weibull_llogis == TRUE){df_all_model$choose[97]=1} else {df_all_model$choose[97]=0}
    if (input$pmm_weibull_gengamma == TRUE){df_all_model$choose[98]=1} else {df_all_model$choose[98]=0}
    if (input$pmm_gamma_gamma == TRUE){df_all_model$choose[99]=1} else {df_all_model$choose[99]=0}
    if (input$pmm_gamma_lnorm == TRUE){df_all_model$choose[100]=1} else {df_all_model$choose[100]=0}
    if (input$pmm_gamma_gompertz == TRUE){df_all_model$choose[101]=1} else {df_all_model$choose[101]=0}
    if (input$pmm_gamma_llogis == TRUE){df_all_model$choose[102]=1} else {df_all_model$choose[102]=0}
    if (input$pmm_gamma_gengamma == TRUE){df_all_model$choose[103]=1} else {df_all_model$choose[103]=0}
    if (input$pmm_lnorm_lnorm == TRUE){df_all_model$choose[104]=1} else {df_all_model$choose[104]=0}
    if (input$pmm_lnorm_gompertz == TRUE){df_all_model$choose[105]=1} else {df_all_model$choose[105]=0}
    if (input$pmm_lnorm_llogis == TRUE){df_all_model$choose[106]=1} else {df_all_model$choose[106]=0}
    if (input$pmm_lnorm_gengamma == TRUE){df_all_model$choose[107]=1} else {df_all_model$choose[107]=0}
    if (input$pmm_gompertz_gompertz == TRUE){df_all_model$choose[108]=1} else {df_all_model$choose[108]=0}
    if (input$pmm_gompertz_llogis == TRUE){df_all_model$choose[109]=1} else {df_all_model$choose[109]=0}
    if (input$pmm_gompertz_gengamma == TRUE){df_all_model$choose[110]=1} else {df_all_model$choose[110]=0}
    if (input$pmm_llogis_llogis == TRUE){df_all_model$choose[111]=1} else {df_all_model$choose[111]=0}
    if (input$pmm_llogis_gengamma == TRUE){df_all_model$choose[112]=1} else {df_all_model$choose[112]=0}
    if (input$pmm_gengamma_gengamma == TRUE){df_all_model$choose[113]=1} else {df_all_model$choose[113]=0}
    if (input$mcm_Exponential == TRUE){df_all_model$choose[114]=1} else {df_all_model$choose[114]=0}
    if (input$mcm_Weibull == TRUE){df_all_model$choose[115]=1} else {df_all_model$choose[115]=0}
    if (input$'mcm_Log-Normal' == TRUE){df_all_model$choose[116]=1} else {df_all_model$choose[116]=0}
    if (input$'mcm_Log-Logistic' == TRUE){df_all_model$choose[117]=1} else {df_all_model$choose[117]=0}
    # 
    temp_vi<-filter(df_all_model,choose==1)
    model_vi<-temp_vi$Model
 
    if (input$vi_bh) {
      output$plot_vi<-renderPlot({
        f_surv1= ggplot() +
          geom_line(data=filter(values$df_vi_p,Model==model_vi), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          geom_point(data=b_haz, aes(x=Age, y=Surv,colour="BH"), colour="Black")+
          scale_color_discrete(name="Model")+
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
        f_surv1<-f_surv1 + ggtitle(input$ggtitle_vi)
        f_surv1<-f_surv1 + ylab(input$aes_y_vi)
        f_surv1<-f_surv1 + xlab(input$aes_x_vi)
        f_surv1<-f_surv1 + expand_limits(y=c(0,1),x=c(0,round(as.numeric(input$plot_vi_x),0)))
        print(f_surv1)
        values$graph_vip<-f_surv1
      })
    } else {
      output$plot_vi<-renderPlot({
        f_surv1= ggplot() +
          geom_line(data=filter(values$df_vi_p,Model==model_vi), aes(x=Time, y=survProp, group=Model, colour=Model), size=1) +
          geom_line(data=ltHaz, aes(x=Time, y=surv,colour="KM"), size=1,colour="Black")+
          scale_color_discrete(name="Model")+
          guides(color = guide_legend(ncol = 1))  +
          theme(legend.position = "bottom") +
          theme_bw()
        f_surv1<-f_surv1 + ggtitle(input$ggtitle_vi)
        f_surv1<-f_surv1 + ylab(input$aes_y_vi)
        f_surv1<-f_surv1 + xlab(input$aes_x_vi)
        f_surv1<-f_surv1 + expand_limits(y=c(0,1),x=c(0,round(as.numeric(input$plot_vi_x),0)))
        print(f_surv1)
        values$graph_vip<-f_surv1
      })
    }
    output$titlebut_vi <- renderUI({
      textInput("ggtitle_vi", "Title", value = "Survival Plot")
    })
    output$aesbut_vi <- renderUI({
      bt_vi <- tagList()
      bt_vi[[1]] <- textInput("aes_y_vi", "Name of Y-axis", value = "Survival rate")
      bt_vi[[2]] <- textInput("aes_x_vi", "Name of X-axis", value = "Survival Probability")
      bt_vi
    })
  })
  
  # Buttons to customize and download the plot (h and w)
  observeEvent(input$dwld_vip, {
    output$sizebut_vip <- renderUI({
      bt_vip <- tagList()
      bt_vip[[1]] <- numericInput("height_vip", "Height (px)", value = 1600)
      bt_vip[[2]] <- numericInput("width_vip", "Width (px)", value = 2800)
      bt_vip
    })
  })
  
  # Download back-end
  output$downloadPlot_vip <- downloadHandler(
    filename = function() { paste0('survival_plot_1.png') },
    content = function(file) {
      ggsave(file,values$graph_vip, width = as.numeric(input$width_vip),
             height = as.numeric(input$height_vip),
             units = "px")
    }
  ) 
  
### final line 
}


##----
### no use

# downloadButton("downloadtable_df_vi_fp", "fp", icon = icon("download")),
# downloadButton("downloadtable_df_vi_rcs", "rcs", icon = icon("download")),                              
# downloadButton("downloadtable_df_vi_rp", "rp", icon = icon("download")),               
# downloadButton("downloadtable_df_vi_gam", "gam", icon = icon("download")),               
# downloadButton("downloadtable_df_vi_pmm", "pmm", icon = icon("download")),               
# downloadButton("downloadtable_df_vi_mcm", "mcm", icon = icon("download")), 



# output$downloadtable_df_vi_ssm <- downloadHandler(
#   filename = function() {
#     paste("data_surv_ssm", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_ssm, file,row.names = FALSE)
#   }
# )
# output$downloadtable_df_vi_fp <- downloadHandler(
#   filename = function() {
#     paste("data_surv_fp", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_fp, file,row.names = FALSE)
#   }
# )
# output$downloadtable_df_vi_rcs <- downloadHandler(
#   filename = function() {
#     paste("data_surv_rcs", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_rcs, file,row.names = FALSE)
#   }
# )
# output$downloadtable_df_vi_rp <- downloadHandler(
#   filename = function() {
#     paste("data_surv_rp", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_rp, file,row.names = FALSE)
#   }
# )
# output$downloadtable_df_vi_gam <- downloadHandler(
#   filename = function() {
#     paste("data_surv_gam", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_gam, file,row.names = FALSE)
#   }
# )
# output$downloadtable_df_vi_pmm <- downloadHandler(
#   filename = function() {
#     paste("data_surv_pmm", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_pmm, file,row.names = FALSE)
#   }
# )
# output$downloadtable_df_vi_mcm <- downloadHandler(
#   filename = function() {
#     paste("data_surv_mcm", ".csv", sep="")
#   },
#   content = function(file) {
#     write.csv(df_vi_mcm, file,row.names = FALSE)
#   }
# )


# observeEvent(input$dwld_vi,{
#   output$sibut_vi <- renderUI({
#     sliderInput(inputId = "plot_vi_x",
#                 label = "Survial Time (Years):",
#                 min = 5,
#                 max = 30,
#                 value = 10,
#                 step = 5)    
#     })
#   if (input$plot_vi_x == 5){
#     values$df_vi_p<-filter(values$sheet_vi,Time<5)
#   } else if (input$plot_vi_x == 10){
#     values$df_vi_p<-filter(values$sheet_vi,Time<10)
#   } else if (input$plot_vi_x == 15){
#     values$df_vi_p<-filter(values$sheet_vi,Time<15)
#   } else if (input$plot_vi_x == 20){
#     values$df_vi_p<-filter(values$sheet_vi,Time<20)
#   } else if (input$plot_vi_x == 25){
#     values$df_vi_p<-filter(values$sheet_vi,Time<25)
#   } else if (input$plot_vi_x == 30){
#     values$df_vi_p<-filter(values$sheet_vi,Time<30)
#   }
#   
# })

# ### download plot
# observeEvent(input$dwld_vi, {
# 
# })
# 
# # Buttons to customize and download the plot (h and w)
# observeEvent(input$dwld_vi, {
# 
# })
