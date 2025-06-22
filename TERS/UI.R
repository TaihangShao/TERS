options(encoding = "UTF-8")
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(magrittr)))
options(encoding = "UTF-8")


# package
#----
suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(writexl)))
suppressWarnings(suppressMessages(library(readxl)))
suppressWarnings(suppressMessages(library(DT)))
suppressWarnings(suppressMessages(library(heemod)))
suppressWarnings(suppressMessages(library(flexsurv)))
suppressWarnings(suppressMessages(library(shinycssloaders)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(slickR)))
# suppressWarnings(suppressMessages(library(IPDfromKM)))
suppressWarnings(suppressMessages(library(survival)))
suppressWarnings(suppressMessages(library(rhandsontable)))
suppressWarnings(suppressMessages(library(shinyWidgets)))
suppressWarnings(suppressMessages(library(shinyhelper)))
suppressWarnings(suppressMessages(library(shinytitle)))
suppressWarnings(suppressMessages(library(clipr)))
suppressWarnings(suppressMessages(library(rmarkdown)))
suppressWarnings(suppressMessages(library(shinyjs)))
suppressWarnings(suppressMessages(library(shinyalert)))
suppressWarnings(suppressMessages(library(markdown)))
suppressWarnings(suppressMessages(library(markdownInput)))
suppressWarnings(suppressMessages(library(shinymanager)))


suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(gridExtra)))
suppressWarnings(suppressMessages(library(survHE)))
suppressWarnings(suppressMessages(library(discSurv)))
suppressWarnings(suppressMessages(library(mgcv)))
suppressWarnings(suppressMessages(library(survminer)))
suppressWarnings(suppressMessages(library(flexsurvcure)))

cat("\n Welcome to this APP! \n")

# Side bar tabs
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("house")),
              menuItem("Import data", tabName = "import", icon = icon("file-import")),
              menuItem("Survival data extrapolation", tabName = "survival", icon = icon("bars"),
                       menuSubItem("1.Standard survival model",icon = icon("chart-area"), tabName = "sde1"),
                       menuSubItem("2.Fractional polynomials",icon = icon("chart-area"), tabName = "sde2"),
                       menuSubItem("3.Restricted cubic splines",icon = icon("chart-area"), tabName = "sde3"),
                       menuSubItem("4.Royston-Parmar models",icon = icon("chart-area"), tabName = "sde4"),
                       menuSubItem("5.Generalized additive models",icon = icon("chart-area"), tabName = "sde5"),
                       menuSubItem("6.Parametric mixture models",icon = icon("chart-area"), tabName = "sde6"),
                       menuSubItem("7.Mixture cure models",icon = icon("chart-area"), tabName = "sde7"),
                       menuSubItem("8.Visual inspection",icon = icon("eye"), tabName = "sde8")
              )
  )
)

# source("uibody.r")


# Interface
dashboardPage(
  skin = "blue",
  # Title with name of the app
  dashboardHeader(title = "TERS",
                  dropdownMenu(type = "messages", badgeStatus = "success",	
                               messageItem(from = "Taihang Shao",	
                                           message = "Welcome to my Github to find more!",	
                                           time = "Dec 18th, 2023",	
                                           href = "https://github.com/TaihangShao"	
                               ),
                               messageItem(from = "Taihang Shao",	
                                           message = "Help when the APP crashes",	
                                           time = "Dec 18th, 2023",	
                                           href = "problem.html"	
                               )
                    ),
  dropdownMenu(type = "notifications", badgeStatus = "warning",	
               notificationItem(icon = icon("users"), status = "info",	
                                text =  "Current version: 2.0",	
               ),	
               notificationItem(icon = icon("warning"), status = "danger",	
                                text = "Import data first!"	
               ),	
               notificationItem(icon = icon("envelope"), status = "info",	
                                text = "BugReports: cpu_sth@stu.cpu.edu.cn"	
               )	
  ),
  dropdownMenuOutput("task_menu")
  # ,
  # dropdownMenu(type = "tasks", badgeStatus = "success",
  #              .list = list(taskItem(value = NULL, text = "Input data", color = "blue", uiOutput("task_info"))),
  #              # taskItem(value = uiOutput(task_id), color = "aqua",	
  #              #          "Input data"	
  #              # ),
  #              taskItem(value = 55, color = "aqua",	
  #                       "Standard survival model"	
  #              ),	
  #              taskItem(value = 45, color = "green",	
  #                       "Fractional polynomials"	
  #              ),	
  #              taskItem(value = 75, color = "yellow",	
  #                       "Restricted cubic splines"	
  #              ),	
  #              taskItem(value = 90, color = "red",	
  #                       "Royston-Parmar models"	
  #              )	
  # )
                  ),
  sidebar,
  dashboardBody(
    withMathJax(),
    useShinyjs(),
    useShinyalert(force = TRUE),
    # tags$style(type="text/css",
    #            ".shiny-progress .progress {height: 40px; width: 400px; position: fixed; top: 50%; left: 50%; margin-top: -20px; margin-left: -200px;}"
    # ),
    tags$head(
      tags$style(HTML("
      .scrollable-panel {
        max-height: 200px;
        overflow-y: auto;
        background-color: white; 
        border-radius: 5px;
        padding: 10px;
      }
    "))
    ),
    tags$head(
      tags$style(HTML("
      .dashboard-title {
        background-color: white; 
        border-radius: 5px;
        padding: 10px;
      }
    "))
    ),
    tags$head(
      tags$style(HTML('dashboard .content-wrapper {background-color:red;}'))
    ),
    tabItems(
      ### Dashboard tab
      tabItem(tabName = "dashboard",
              # tags$head(
              #   tags$style(HTML("#dashboard .content-wrapper {background-color: red !important;}"))
              # ),
              #----
              column(width = 12,
                     wellPanel(
                       class = "dashboard-title",
                       HTML("<h1>Welcome to the Tool for Extrapolation of Reconstructed Survival data (TERS) "),
                       HTML("<h4>This tool provides an open-access tool powered by R that can be used to extrapolate the 
                     survival data with standard survival models and some flexible techniques.
                     Please note that not all existing extrapolation methods are included in this app, so please be careful when using this APP (You can find more in the references).
                     Under many real-world situations, researchers do not have the IPD. Therefore, 
                     this tool focuses on <em>reconstructed survival data</em>, covariates can not be included in this APP.
                     If you have survival data with covariates, we do not recommend you to use this APP, but methods used in this APP can still be applied. 
                     Please refer to the references in this app for relevant methodology and codes.
                     You can get started by pressing the button below or using the sidebar button.
                     <b>Please note that you need to import the data first before proceeding with any further operations; 
                     and Please run all models before conducting visual inspection.</b></h4>")
                     ),
                     fluidRow(
                       column(width = 12, align ="center",
                              actionButton("FCSubmit", label="Get start!",style="color: black;
                       background-color: white; border-color: black; font-size: 20px; font-weight: bold; height: 50px; width: 200px;"),
                              # actionButton("FCvi", label="Visual inspection"),
                              # actionButton("downloadmanual", "Download manual",
                              #              onclick = "location.href='files/manual.rar'"),
                              # actionButton("FCcitation", label="Copy citation"),
                       )
                     ),
                     p("")
              ),
              column(width = 12,
                     # height = 1,
                     # wellPanel(
                     #   class = "custom-well",
                       HTML("<h3><b>ABOUT</b></h3>"),
                     # ),
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("<h5>BugReports: <u>cpu_sth@stu.cpu.edu.cn</u></h5>"),
                       HTML("<h5>This tool has been developed by:<br><br> [1] Taihang Shao <br> <em> -- From Center for Pharmacoeconomics and Outcome Research, China Pharmaceutical University </em>
                       <br><br> [2] Mingye Zhao <br> <em> -- From Center for Pharmacoeconomics and Outcome Research, China Pharmaceutical University </em>
                       <br><br> [3] Fenghao Shi <br> <em> -- From International Research Center for Medicinal Administration, Peking University </em>
                       <br><br> [4] Mingjun Rui <br> <em> -- From School of Pharmacy, Chinese University of Hong Kong </em>
                       <br><br> [5] Wenxi Tang <br> <em> -- From Center for Pharmacoeconomics and Outcome Research, China Pharmaceutical University </em>
                          </h5>")
                     ),
              ),
              column(width = 12,
                     # wellPanel(
                       HTML("<h3><b>LOG</b></h3>")
                     # ),
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("<h5><em> October 17, 2023 </em>: Version 1.0 (NMA is now under construction.)</h5>"),
                       HTML("<h5><em> December 17, 2023 </em>: Version 2.0 (NMA will be constructed in another APP. Some improvements are made in this APP.)</h5>")
                     ),
              ),
              column(width = 12,
                     # wellPanel(
                       HTML("<h3><b>USEFUL REFERENCE</b></h3>"),
                       HTML("<h4>If you use this tool, these references will be helpful to you:</h4>")
                     # )
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("
                          <p>[1]Shao T, Zhao M, Liang L, Shi L, Tang W. Impact of Extrapolation Model Choices on the Structural Uncertainty in Economic Evaluations for Cancer Immunotherapy: A Case Study of Checkmate 067. Pharmacoecon Open. 2023;7(3):383-392. doi:10.1007/s41669-023-00391-5</p>
                          <p>[2]Shao T, Zhao M, Liang L, Tang W. A systematic review and network meta-analysis of first-line immune checkpoint inhibitor combination therapies in patients with advanced non-squamous non-small cell lung cancer. Front Immunol. 2022;13:948597. Published 2022 Oct 26. doi:10.3389/fimmu.2022.948597</p>
                          <p>[3]Guyot P, Ades AE, Ouwens MJ, Welton NJ. Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves. BMC Med Res Methodol. 2012;12:9 doi:10.1186/1471-2288-12-9</p>
                          <p>[4]Djalalov S, Beca J, Ewara EM, Hoch JS. A Comparison of Different Analysis Methods for Reconstructed Survival Data to Inform CostEffectiveness Analysis. Pharmacoeconomics. 2019;37(12):1525-1536 doi:10.1007/s40273-019-00830-4</p>
                          <p>[5]Gibson E, Koblbauer I, Begum N et al. Modelling the Survival Outcomes of Immuno-Oncology Drugs in Economic Evaluations: A Systematic Approach to Data Analysis and Extrapolation. Pharmacoeconomics. 2017;35(12):1257-1270 doi:10.1007/s40273-017-0558-5</p>
                          <p>[6]Palmer S, Borget I, Friede T et al. A Guide to Selecting Flexible Survival Models to Inform Economic Evaluations of Cancer Immunotherapies. Value Health. 2022; doi:10.1016/j.jval.2022.07.009</p>
                          <p>[7]Kearns B, Stevenson MD, Triantafyllopoulos K, Manca A. Generalized Linear Models for Flexible Parametric Modeling of the Hazard Function. Med Decis Making. 2019;39(7):867-878 doi:10.1177/0272989X19873661</p>
                          <p>[8]Kearns B, Stevenson MD, Triantafyllopoulos K, Manca A. The Extrapolation Performance of Survival Models for Data With a Cure Fraction: A Simulation Study. Value Health. 2021;24(11):1634-1642 doi:10.1016/j.jval.2021.05.009</p>
                          <p>[9]Kearns B, Stevenson MD, Triantafyllopoulos K, Manca A. Comparing current and emerging practice models for the extrapolation of survival data: a simulation study and case-study. BMC Med Res Methodol. 2021;21(1) doi:10.1186/s12874-021-01460-1</p>
                          <p>[10]Klijn SL, Fenwick E, Kroep S et al. What Did Time Tell Us? A Comparison and Retrospective Validation of Different Survival Extrapolation Methods for Immuno-Oncologic Therapy in Advanced or Metastatic Renal Cell Carcinoma. Pharmacoeconomics. 2021;39(3):345-356 doi:10.1007/s40273-020-00989-1</p>
                          <p>[11]Royston P, Parmar MK. Flexible parametric proportional-hazards and proportional-odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. Stat Med. 2002;21(15):2175-97 doi:10.1002/sim.1203</p>
                          <p>[12]Royston P, Sauerbrei W. A new approach to modelling interactions between treatment and continuous covariates in clinical trials by using fractional polynomials. Stat Med. 2004;23(16):2509-25 doi:10.1002/sim.1815</p>
                          <p>[13]Ishak KJ, Kreif N, Benedict A, Muszbek N. Overview of parametric survival analysis for health-economic applications. Pharmacoeconomics. 2013;31(8):663-75 doi:10.1007/s40273-013-0064-3</p>
                          <p>[14]Rutherford MJ, Crowther MJ, Lambert PC. The use of restricted cubic splines to approximate complex hazard functions in the analysis of time-to-event data: a simulation study. J Stat Comput Simul. 2015;85(4-6):777-793</p>
                          <p>[15]Grant TS, Burns D, Kiff C, Lee D. A Case Study Examining the Usefulness of Cure Modelling for the Prediction of Survival Based on Data Maturity. Pharmacoeconomics. 2020;38(4):385-395 doi:10.1007/s40273-019-00867-5</p>
                          <p>[16]Jakobsen LH, Bogsted M, Clements M. Generalized parametric cure models for relative survival. Biom J. 2020;62(4):989-1011 doi:10.1002/bimj.201900056</p>
                          <p>[17]Federico PV, Kurt M, Zhang L et al. Heterogeneity in Survival with Immune Checkpoint Inhibitors and Its Implications  for Survival Extrapolations: A Case Study in Advanced Melanoma. MDM Policy Pract. 2022;7(1):23814683221089659 doi:10.1177/23814683221089659</p>
                          <p>[18]Cooper M, Smith S, Williams T, Aguiar-Ibanez R. How accurate are the longer-term projections of overall survival for cancer immunotherapy for standard versus more flexible parametric extrapolation methods? J Med Econ. 2022;25(1):260-273 doi:10.1080/13696998.2022.2030599</p>
                          <p>[19]The National Institute for Health and Care Excellence. NICE DSU technical support document 14: Survival analysis for economic evaluations alongside clinical trials – extrapolation with patient-level data. https://www.sheffield.ac.uk/sites/default/files/2022-02/TSD14-Survival-analysis.updated-March-2013.v2.pdf. Accessed 17 October, 2023</p>
                          <p>[20]The National Institute for Health and Care Excellence. NICE DSU technical support document 21: Flexible Methods for Survival Analysis. https://www.sheffield.ac.uk/sites/default/files/2022-02/TSD21-Flex-Surv-TSD-21_Final_alt_text.pdf. Accessed 17 October, 2023</p>
                          ")
                     )
              )
       ),
      # End of dashboard
      #----
      #Import data tab
      tabItem(tabName = "import",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Import Data"))%>%
                    helper(type = "markdown", content = "idreadme",icon = "triangle-exclamation"),
                  p(""),
                  fileInput("upload", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload"),
                  p(""),
                  actionButton("downloadtemplate", "Download example data",icon("download"),
                               onclick = "location.href='files/example.xlsx'"),
                  p(""),
                  actionButton("FCexample", label="Load example data",icon("gear")),
                  hr(),
                  uiOutput("dropdown"),
                  hr(),
                  h4(em("You should process the data first!")),
                  p(""),
                  uiOutput("processdatabutton"),
                  p(""),
                  hr(),
                  h4(em("Then you should generate the data which can be used in the extrapolation models!")),
                  p(""),
                  uiOutput("MyTHinput"),
                  htmlOutput("WarningMyTHinput"),
                  hr(),
                  p(""),
                  uiOutput("MyStepinput"),
                  conditionalPanel(
                    condition = "input.ready3 == true",
                    HTML("Tips: If you want to set the cycle length as 21 days, you can input 17 here.")
                  ),
                  p(""),
                  uiOutput("updatedata1"),
                  hr(),
                  h4(em("Now you can move to the Survival Data Extrapolation!")),
                  fluidRow(
                    column(6, p(""),
                           uiOutput("goonbutton1"),
                           p(""),
                           uiOutput("goonbutton2"),
                           p(""),
                           uiOutput("goonbutton3"),
                           p(""),
                           uiOutput("goonbutton4")),
                    column(6, p(""),
                           uiOutput("goonbutton5"),
                           p(""),
                           uiOutput("goonbutton6"),
                           p(""),
                           uiOutput("goonbutton7"),
                           p(""),
                           uiOutput("goonbutton8"))
                  ),
                ),
                mainPanel(
                  checkboxInput("show_table1", strong("Show Reconstructed IPD data"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table1 == true",
                    DTOutput("upl_outDT")),
                  
                  checkboxInput("show_table2", strong("Do not Show KM Estimated and Life-table Estimated HR"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table2 == false",
                    DTOutput("lthaz")),
                  
                  checkboxInput("show_table3", strong("Do not Show Data Input in the Extrapolation Model"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table3 == false",
                    DTOutput("Newtime")),
                )
              )
              
      ), 
      # End of Import data tab
      #----
      #### Start of survival page
      #Standard Survival Model
      tabItem(tabName = "sde1",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Standard Survival Model")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("Standard survival models include Exponential, Weibull, Gompertz, Lognormal, Loglogistic Gamma, Generalized Gamma, 
                  and Generalized F distributions. Details of the methodologies had been reported by previous published articles. 
                  Some useful links can be found here:"),
                  p(""),
                  tags$a(href = "https://devinincerti.com/2019/06/18/parametric_survival.html#introduction", "Parametric survival modeling", target = "_blank"),br(),
                  tags$a(href = "https://github.com/chjackson/flexsurv", "flexsurv: Flexible Parametric Survival and Multi-State Models", target = "_blank"),br(),
                  tags$a(href = "SSM.html", "Survival function of eight models", target = "_blank"),br(),
                  p(""),
                  hr(),
                  p(strong("You should run the model before the plot can be generated or updated!")),
                  hr(),
                  uiOutput("runmodelssm1"),
                  p(),
                  # conditionalPanel(
                  # condition="input.updatedata",
                  checkboxInput("ssm_gf", strong("Run without Generalized F distribution"), value = FALSE)%>%
                    helper(type = "markdown", content = "ssm_gf"),
                  # ),
                  p(),
                  hr(),
                  htmlOutput("best_ssm1"),
                  htmlOutput("best_ssm2"),
                  hr(),
                  p(),
                  selectInput("time1", "Time", choices = c("Original Time Horizon" = 1, 
                                                           "Input Time Horizon" = 2),selected = 1),
                  
                  selectInput("model1", "Model", choices = c("Exponential", 
                                                             "Weibull",
                                                             "Gamma", 
                                                             "Log-Normal",
                                                             "Gompertz",
                                                             "Log-Logistic",
                                                             "Generalized Gamma",
                                                             "Generalized F",
                                                             "Combination"),selected = "Exponential"),
                  hr(),
                  p(),
                  uiOutput("plotssm1"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_ssm",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_ssm", "Download modeled Hazard Data", icon = icon("download"),style="color: #fff;
                       background-color: skyblue; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_ssm", "Download modeled Survival Data", icon = icon("download"),style="color: #fff;
                       background-color: skyblue; border-color: #2e6da4"))
                    )),
                  
                ),
                mainPanel(
                  DTOutput("ssm_aic"),
                  conditionalPanel(
                    condition = "output.ssm_aic",
                    downloadButton("downloadtable_ssm1", "Download This Table", icon = icon("download"),style="color: #fff;
                       background-color: skyblue; border-color: #2e6da4")
                  ),
                  p(),
                  plotOutput("plot_ssm", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_ssm",
                    p(),
                    actionButton("dwld", "Download plot", icon("download"), 
                                 style="color: #fff;
                       background-color: skyblue; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld",
                    p(),
                    uiOutput("titlebut"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut")
                      )
                    ),
                    downloadButton("downloadPlot", "", icon = icon("download"))
                  )
                  
                )
              )
      ),
      #----
      #Fractional polynomials
      tabItem(tabName = "sde2",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Fractional polynomials")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("FP model, which was developed and put into use in the survival analysis by Royston and Altman. 
                  It was as an extension of polynomial functions that can be applied under the assumption of non-linearity of the hazard. 
                  FP could be divided into one-order FP and two-order FP with 8 optional powers {-2, -1, -0.5, 0, 0.5, 1, 2, 3}. 
                  Thus, there were eight one-order FP and 36 two-order FP. Function of FP can be found in the first link.
                  The second link can provide some useful information about FP. 
                  In this APP, glm function was used here, binomial was selected as the distribution and cloglog was selected as the link function."),
                  p(""),
                  tags$a(href = "FP.html", "Function of fractional polynomials", target = "_blank"),br(),
                  tags$a(href = "https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Fractional_Polynomial_Regression.pdf", 
                         "Fractional Polynomial Regression (From NCSS)", target = "_blank"),br(),
                  p(""),
                  hr(),
                  p(strong("You should run the model before the plot can be generated or updated!")),
                  hr(),
                  uiOutput("runmodelfp1"),
                  p(),
                  hr(),
                  htmlOutput("best_fp11"),
                  htmlOutput("best_fp12"),
                  p(),
                  htmlOutput("best_fp21"),
                  htmlOutput("best_fp22"),
                  p(),
                  hr(),
                  h4("Run specific FP model"),
                  p(),
                  uiOutput("fp1p"),
                  p(),
                  uiOutput("runmodelfp2"),
                  p(),
                  uiOutput("fp2p1"),
                  p(),
                  uiOutput("fp2p2"),
                  p(),
                  uiOutput("runmodelfp3"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_fp1_1",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_fp1", "Hazard Data of FP1", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_fp1", "Survival Dataof FP1", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4"))
                    )),
                  conditionalPanel(
                    condition = "output.plot_fp2_1",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_fp2", "Hazard Data of FP2", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_fp2", "Survival Dataof FP2", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4"))
                    )),
                  hr(),
                  p(""),
                  conditionalPanel(
                    condition = "output.plot_fp1_1 && output.plot_fp2_1",
                    p(""),
                    h4("Choose the order of FP models")%>%
                      helper(type = "markdown", content = "choose_order_fp",icon = "triangle-exclamation"),
                    p(""),
                    fluidRow(
                      column(6, actionButton("fp_choose_model_1", label="Choose Order",icon ("gear"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
                      column(6, conditionalPanel(
                        condition = "input.fp_choose_model_1",
                        actionButton("fp_choose_model_2", label="Run the model",icon ("gear"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")
                      )
                      )
                    )
                  ),
                  uiOutput("fpnullbt"),
                  uiOutput("fplinbt")
                ),
                mainPanel(
                  checkboxInput("show_table4", strong("Do not Show Model performance of FP1"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table4 == false",
                    DTOutput("FP1restb"),
                    conditionalPanel(
                      condition = "output.FP1restb",
                      downloadButton("downloadtable_fp1", "Download This Table", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")
                    )),
                  checkboxInput("show_table5", strong("Do not Show Model performance of FP2"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table5 == false",
                    DTOutput("FP2restb"),
                    conditionalPanel(
                      condition = "output.FP2restb",
                      downloadButton("downloadtable_fp2", "Download This Table", icon = icon("download"),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")
                    )),
                  
                  checkboxInput("show_figure1", strong("Do not Show Survival plot for selected FP1"), value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.show_figure1 == false",
                    fluidRow(
                      column(6, "Survival plot with Original Time Horizon"),
                      column(6, "Survival plot with Input Time Horizon")
                    ),
                    fluidRow(
                      column(6, plotOutput("plot_fp1_1", width = "480px", height = "360px")),
                      column(6, plotOutput("plot_fp1_2", width = "480px", height = "360px"))
                    )),
                  conditionalPanel(
                    condition = "output.plot_fp1_1",
                    p(),
                    fluidRow(
                      column(6, actionButton("dwld_fp11", "Download plot", icon = icon("download", verify_fa = FALSE),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
                      column(6, actionButton("dwld_fp12", "Download plot", icon = icon("download", verify_fa = FALSE),style="color: #fff;
                       background-color: pink; border-color: #2e6da4"))
                    )),
                  
                  fluidRow(
                    column(6, conditionalPanel(
                      condition = "input.dwld_fp11",
                      p(),
                      uiOutput("titlebut_fp11"),
                      uiOutput("sizebut_fp11"),
                      downloadButton("downloadPlot_fp11", "", icon = icon("download")))),
                    
                    column(6, conditionalPanel(
                      condition = "input.dwld_fp12",
                      p(),
                      uiOutput("titlebut_fp12"),
                      uiOutput("sizebut_fp12"),
                      downloadButton("downloadPlot_fp12", "", icon = icon("download"))))
                  ),
                  
                  checkboxInput("show_figure2", strong("Do not Show Survival plot for selected FP2"), value = TRUE),
                  
                  conditionalPanel(
                    condition = "input.show_figure2 == false",
                    fluidRow(
                      column(6, "Survival plot with Original Time Horizon"),
                      column(6, "Survival plot with Input Time Horizon")
                    ),
                    fluidRow(
                      column(6, plotOutput("plot_fp2_1", width = "480px", height = "360px")),
                      column(6, plotOutput("plot_fp2_2", width = "480px", height = "360px"))
                    )),
                  conditionalPanel(
                    condition = "output.plot_fp2_1",
                    p(),
                    fluidRow(
                      column(6, actionButton("dwld_fp21", "Download plot", icon = icon("download", verify_fa = FALSE),style="color: #fff;
                       background-color: pink; border-color: #2e6da4")),
                      column(6, actionButton("dwld_fp22", "Download plot", icon = icon("download", verify_fa = FALSE),style="color: #fff;
                       background-color: pink; border-color: #2e6da4"))
                    )),
                  
                  fluidRow(
                    column(6, conditionalPanel(
                      condition = "input.dwld_fp21",
                      p(),
                      uiOutput("titlebut_fp21"),
                      uiOutput("sizebut_fp21"),
                      downloadButton("downloadPlot_fp21", "", icon = icon("download")))),
                    
                    column(6, conditionalPanel(
                      condition = "input.dwld_fp22",
                      p(),
                      uiOutput("titlebut_fp22"),
                      uiOutput("sizebut_fp22"),
                      downloadButton("downloadPlot_fp22", "", icon = icon("download"))))
                  ),
                  conditionalPanel(
                    condition = "input.fpnull_plot",
                    p(),
                    p("Survival Plot: FP model with no association with time"),
                    fluidRow(
                      column(6,plotOutput("plot_fpnull", width = "480px", height = "360px")),
                      column(6,p(""),
                             p(""),
                             p(""),
                             actionButton("dwld_fpnull", "Download plot", icon = icon("download", verify_fa = FALSE),style="color: #fff;
                       background-color: pink; border-color: #2e6da4"),
                             uiOutput("titlebut_fpnull"),
                             uiOutput("sizebut_fpnull"),
                             downloadButton("downloadPlot_fpnull", "", icon = icon("download")) )
                    )),
                  conditionalPanel(
                    condition = "input.fplin_plot",
                    p(),
                    p("Survival Plot: Linear FP model"),
                    fluidRow(
                      column(6,plotOutput("plot_fplin", width = "480px", height = "360px")),
                      column(6,p(""),
                             p(""),
                             p(""),
                             actionButton("dwld_fplin", "Download plot", icon = icon("download", verify_fa = FALSE),style="color: #fff;
                       background-color: pink; border-color: #2e6da4"),
                             uiOutput("titlebut_fplin"),
                             uiOutput("sizebut_fplin"),
                             downloadButton("downloadPlot_fplin", "", icon = icon("download")) )
                    )
                  )
                )        
              )
      ),
      #----
      #Restricted cubic splines
      tabItem(tabName = "sde3",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Restricted cubic splines")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("In splines, the range of values of the independent variable is split up, with “knots” defining the end of one segment and the start of the next. 
                Separate curves are fit to each segment. 
                Overall, the splines are defined so that the resulting fitted curve is smooth and continuous.
                Commonly, the boundary interval of cubic spline model was cubic polynomial, which could also be restricted to linear functions. 
                The latter was called restricted spline model. 
                In this APP, gam function was used here, binomial was selected as the distribution and cloglog was selected as the link function.
                  Some useful links can be found here:"),
                  p(""),
                  tags$a(href = "https://support.sas.com/resources/papers/proceedings16/5621-2016.pdf", "Restricted Cubic Spline Regression: A Brief Introduction", target = "_blank"),br(),
                  tags$a(href = "RCS.html", "Function of Restricted Cubic Spline models", target = "_blank"),br(),
                  p(""),
                  hr(),
                  uiOutput("runmodelrcs1"),
                  p(),
                  hr(),
                  htmlOutput("best_rcs1"),
                  htmlOutput("best_rcs2"),
                  hr(),
                  p(),
                  selectInput("timercs", "Time", choices = c("Original Time Horizon" = 1, 
                                                             "Input Time Horizon" = 2),selected = 1),
                  selectInput("rcs_knot", "Plot based on Knots", choices = c("1","2","3","4","5","Combination"),selected = "1"),
                  hr(),
                  p(),
                  uiOutput("plotrcs1"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_rcs",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_rcs", "Download modeled Hazard Data", icon = icon("download"),style="color: black;
                       background-color: yellow; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_rcs", "Download modeled Survival Data", icon = icon("download"),style="color: black;
                       background-color: yellow; border-color: #2e6da4"))
                    )),
                ),
                mainPanel(
                  DTOutput("rcs_aic"),
                  # DTOutput("rcssum"),
                  conditionalPanel(
                    condition = "output.rcs_aic",
                    downloadButton("downloadtable_rcs1", "Download This Table", icon = icon("download"),style="color: black;
                       background-color: yellow; border-color: #2e6da4")
                  ),
                  p(),
                  plotOutput("plot_rcs", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_rcs",
                    p(),
                    actionButton("dwld_rcs", "Download plot", icon("download"), 
                                 style="color: black;
                       background-color: yellow; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_rcs",
                    p(),
                    uiOutput("titlebut_rcs"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_rcs")
                      )
                    ),
                    downloadButton("downloadPlot_rcs", "", icon = icon("download"))
                  )
                )        
              )
      ),
      #----
      #Royston-Parmar models
      tabItem(tabName = "sde4",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Royston-Parmar models")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("In splines, the range of values of the independent variable is split up, with “knots” defining the end of one segment and the start of the next. 
                Separate curves are fit to each segment. 
                Overall, the splines are defined so that the resulting fitted curve is smooth and continuous.
                Commonly, the boundary interval of cubic spline model was cubic polynomial, which could also be restricted to linear functions. 
                The latter was called restricted spline model.Some useful links can be found here:"),
                  p(""),
                  tags$a(href = "https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.1203", "Royston P, Parmar MK. Stat Med. 2002;21:2175–97.doi:10.1002/sim.1203", target = "_blank"),br(),
                  tags$a(href = "https://github.com/chjackson/flexsurv", "flexsurv: Flexible Parametric Survival and Multi-State Models", target = "_blank"),br(),
                  tags$a(href = "RP.html", "Function of Royston-Parmar models", target = "_blank"),br(),
                  p(""),
                  hr(),
                  uiOutput("runmodelrp1")%>%
                    helper(type = "markdown", content = "rp_error"),
                  p(),
                  hr(),
                  htmlOutput("best_rp_h1"),
                  htmlOutput("best_rp_h2"),
                  htmlOutput("best_rp_o1"),
                  htmlOutput("best_rp_o2"),
                  htmlOutput("best_rp_n1"),
                  htmlOutput("best_rp_n2"),
                  hr(),
                  p(),
                  selectInput("timerp", "Time", choices = c("Original Time Horizon" = 1, 
                                                            "Input Time Horizon" = 2),selected = 1),
                  selectInput("rp_knot", "Plot based on Knots", choices = c("1","2","3","4","5","Combination"),selected = "1"),
                  selectInput("rp_scale", "Plot based on Scale", choices = c("hazard","odds","normal","Combination"),selected = "1"),
                  hr(),
                  p(),
                  uiOutput("plotrp1"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_rp",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_rp", "Download modeled Hazard Data", icon = icon("download"),style="color: black;
                       background-color: orange; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_rp", "Download modeled Survival Data", icon = icon("download"),style="color: black;
                       background-color: orange; border-color: #2e6da4"))
                    )),
                ),
                mainPanel(                
                  DTOutput("rp_aic"),
                  conditionalPanel(
                    condition = "output.rp_aic",
                    downloadButton("downloadtable_rp1", "Download This Table", icon = icon("download"),style="color: black;
                       background-color: orange; border-color: #2e6da4")
                  ),
                  p(),
                  plotOutput("plot_rp", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_rp",
                    p(),
                    actionButton("dwld_rp", "Download plot", icon("download"), 
                                 style="color: black;
                       background-color: orange; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_rp",
                    p(),
                    uiOutput("titlebut_rp"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_rp")
                      )
                    ),
                    downloadButton("downloadPlot_rp", "", icon = icon("download"))
                  )
                )        
              )
      ),
      #----
      #Generalized additive models
      tabItem(tabName = "sde5",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Generalized additive models")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("Generalized Additive Models (GAMs) are flexible models that extend the linear model framework to 
                  allow for non-linear relationships between your predictors or features and your response variable.
                  In this APP, gam function was used here, binomial was selected as the distribution and cloglog was selected as the link function.
                  Some useful links can be found here:
                  "),
                  p(""),
                  tags$a(href = "https://www.wallstreetmojo.com/generalized-additive-model/", "What Is A Generalized Additive Model (GAM)?", target = "_blank"),br(),
                  tags$a(href = "https://projecteuclid.org/journals/statistical-science/volume-1/issue-3/Generalized-Additive-Models/10.1214/ss/1177013604.full",
                         "Trevor Hastie. Robert Tibshirani. Statist. Sci. 1 (3) 297 - 310", target = "_blank"),br(),
                  tags$a(href = "GAM.html", "Function of Generalized Additive Models", target = "_blank"),br(),
                  p(""),
                  hr(),
                  uiOutput("runmodelgam1"),
                  p(),
                  hr(),
                  htmlOutput("best_gam1"),
                  htmlOutput("best_gam2"),
                  hr(),
                  p(),
                  selectInput("timegam", "Time", choices = c("Original Time Horizon" = 1, 
                                                             "Input Time Horizon" = 2),selected = 1),
                  selectInput("gam_knot", "Plot based on Knots", choices = c("1","2","3","4","5","6","7","8","9","10","Combination"),selected = "1"),
                  hr(),
                  p(),
                  uiOutput("plotgam1"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_gam",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_gam", "Download modeled Hazard Data", icon = icon("download"),style="color: white;
                       background-color: violet; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_gam", "Download modeled Survival Data", icon = icon("download"),style="color: white;
                       background-color: violet; border-color: #2e6da4"))
                    )),
                ),
                mainPanel(                
                  DTOutput("gam_aic"),
                  conditionalPanel(
                    condition = "output.gam_aic",
                    downloadButton("downloadtable_gam1", "Download This Table", icon = icon("download"),style="color: white;
                       background-color: violet; border-color: #2e6da4")
                  ),
                  p(),
                  plotOutput("plot_gam", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_gam",
                    p(),
                    actionButton("dwld_gam", "Download plot", icon("download"), 
                                 style="color: white;
                       background-color: violet; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_gam",
                    p(),
                    uiOutput("titlebut_gam"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_gam")
                      )
                    ),
                    downloadButton("downloadPlot_gam", "", icon = icon("download"))
                  ) 
                )        
              )
      ),
      #----
      #Parametric mixture models
      tabItem(tabName = "sde6",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Parametric mixture models")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("Mixture models are defined as a mixture of two (or more) latent subgroups, 
                  each with a different survival curve characterized by a parametric distribution. 
                  Given the weights (p and 1−p) and the survival function of 
                  each subgroup (S1(t) and S2(t)), the OS distribution can be written as 
                  the weighted average of the survival from the two latent subgroups:
                  $$S(t)=(pS_1(t)=(1-p)S_2(t))$$
                  In this APP, 28 potential combinations of exponential, weibull,log-logistic, 
                  log normal, gompertz, gamma and generalized gamma distributions are considered.",
                    strong("Note: This APP cite the code of 'Klijn, S.L., Fenwick, E., Kroep, S. et al. PharmacoEconomics 39, 345–356 (2021).' for Parametric mixture models."),
                    "The reference can be found here:"),
                  p(""),
                  tags$a(href = "https://link.springer.com/article/10.1007/s40273-020-00989-1#citeas", 
                         "Klijn, S.L., Fenwick, E., Kroep, S. et al. PharmacoEconomics 39, 345–356 (2021)", target = "_blank"),br(),
                  p(""),
                  hr(),
                  uiOutput("runmodelpmm1")%>%
                    helper(type = "markdown", content = "pmm_error"),###补充
                  p(),
                  hr(),
                  htmlOutput("best_pmm1"),
                  htmlOutput("best_pmm2"),
                  hr(),
                  p(),
                  selectInput("timepmm", "Time", choices = c("Original Time Horizon" = 1, 
                                                             "Input Time Horizon" = 2),selected = 1),
                  selectInput("pmm_d1", "Input distribution 1", choices = c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","Combination"),selected = "1"),
                  selectInput("pmm_d2", "Input distribution 2", choices = c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma"),selected = "1"),
                  hr(),
                  p(),
                  uiOutput("plotpmm1"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_pmm",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_pmm", "Download modeled Hazard Data", icon = icon("download"),style="color: black;
                       background-color: snow; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_pmm", "Download modeled Survival Data", icon = icon("download"),style="color: black;
                       background-color: snow; border-color: #2e6da4"))
                    )),
                ),
                mainPanel(                
                  DTOutput("pmm_aic"),
                  conditionalPanel(
                    condition = "output.pmm_aic",
                    downloadButton("downloadtable_pmm1", "Download This Table", icon = icon("download"),style="color: black;
                       background-color: snow; border-color: #2e6da4")
                  ),
                  p(),
                  plotOutput("plot_pmm", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_pmm",
                    p(),
                    actionButton("dwld_pmm", "Download plot", icon("download"), 
                                 style="color: black;
                       background-color: snow; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_pmm",
                    p(),
                    uiOutput("titlebut_pmm"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_pmm")
                      )
                    ),
                    downloadButton("downloadPlot_pmm", "", icon = icon("download"))
                  ),
                  p(),
                  plotOutput("plot_pmm1", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_pmm1",
                    p(),
                    p("Through this plot, you can see how two distributions construct the mixture model. Green and Blue lines represent two distributions. Red line shows the mixture model."),
                    p("Warning! Choosing COMBINATION as the input distribution will not bring any changes."),
                    actionButton("dwld_pmm1", "Download plot", icon("download"), 
                                 style="color: black;
                       background-color: snow; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_pmm1",
                    p(),
                    uiOutput("titlebut_pmm1"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_pmm1")
                      )
                    ),
                    downloadButton("downloadPlot_pmm1", "", icon = icon("download"))
                  ),
                )        
              )
      ),
      #----
      #Mixture cure models
      tabItem(tabName = "sde7",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Mixture cure models")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  p("Mixture cure models (MCMs) are a type of survival analysis model used when a certain portion of the study population is 'cured' and therefore will never experience the event of interest.
                  In MCMs, the population is considered to be a mixture of susceptible individuals who may experience the event, and non-susceptible or 'cured' individuals who will not. 
                  The survival function is then a weighted average of the survival functions for the susceptible and non-susceptible subpopulations.
                  Some useful links can be found here:"),
                  p(""),
                  tags$a(href = "https://github.com/jrdnmdhl/flexsurvcure", "flexsurvcure,Parametric Cure Models", target = "_blank"),br(),
                  tags$a(href = "https://github.com/LasseHjort/cuRe", "cuRe: An R-package for parametric cure model estimation", target = "_blank"),br(),
                  tags$a(href = "MCM.html", "Function of Mixture cure models", target = "_blank"),br(),
                  p("This APP use the 'flexsurvcure' as the basic package to generate MCMs. The distributions can be selected include:Exponential,Weibull,Lognormal,Log-Logistic"),
                  p(""),
                  p(strong("Tips:MCM may not be applicable to all data."),"You should conduct several tests before using a cure model:
                  (1)sufficient follow-up;(2)the presence of a cure fraction.etc. In addition, non-mixture cure models can also be considered(Not considered in this app). 
                  Through the following article, you may have a knowledge of what should do when using a MCM."),
                  tags$a(href = "https://www.annualreviews.org/doi/10.1146/annurev-statistics-031017-100101", "Cure Models in Survival Analysis", target = "_blank"),br(),
                  p(""),
                  hr(),
                  fileInput("upload_mcm", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload_mcm"),
                  p(""),
                  actionButton("downloadtemplate_mcm", "Download example data",icon("download"),
                               onclick = "location.href='files/example_mcm.xlsx'"),
                  actionButton("loadtemplate_mcm", "Load example data",icon("gear")),
                  hr(),
                  uiOutput("mcm_age1"),
                  uiOutput("runmodelmcm1"),
                  p(),
                  hr(),
                  selectInput("time_mcm", "Time", choices = c("Original Time Horizon" = 1, 
                                                              "Input Time Horizon" = 2),selected = 1),
                  selectInput("model_mcm", "Model", choices = c("Exponential", 
                                                                "Weibull",
                                                                "Log-Normal",
                                                                "Log-Logistic",
                                                                "Combination"
                  ),selected = "Exponential"),
                  hr(),
                  p(),
                  uiOutput("plotmcm1"),
                  p(),
                  hr(),
                  h4("Modeled Data Download"),
                  p(),
                  conditionalPanel(
                    condition = "output.plot_mcm",
                    p(),
                    fluidRow(
                      column(6, downloadButton("downloadtable_dfhaz_mcm", "Download modeled Hazard Data", icon = icon("download"),style="color: black;
                       background-color: royalblue; border-color: #2e6da4")),
                      column(6, downloadButton("downloadtable_dfsurv_mcm", "Download modeled Survival Data", icon = icon("download"),style="color: black;
                       background-color: royalblue; border-color: #2e6da4"))
                    )),
                ),
                mainPanel(                
                  checkboxInput("show_table_mcm1", strong("Show Input Background Hazard"), value = FALSE),
                  conditionalPanel(
                    condition = "input.show_table_mcm1 == true",
                    DTOutput("sheet_mcm")),
                  DTOutput("mcm_aic"),
                  conditionalPanel(
                    condition = "output.mcm_aic",
                    p("Note: Theta means the cure fraction.")),
                  conditionalPanel(
                    condition = "output.mcm_aic",
                    downloadButton("downloadtable_mcm1", "Download This Table", icon = icon("download"),style="color: black;
                       background-color: royalblue; border-color: #2e6da4")
                  ),
                  p(),
                  plotOutput("plot_mcm", width = "800px", height = "600px"),
                  # Show download button when the curves appears
                  conditionalPanel(
                    condition = "output.plot_mcm",
                    p(),
                    actionButton("dwld_mcm", "Download plot", icon("download"), 
                                 style="color: black;
                       background-color: royalblue; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_mcm",
                    p(),
                    uiOutput("titlebut_mcm"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_mcm")
                      )
                    ),
                    downloadButton("downloadPlot_mcm", "", icon = icon("download"))
                  )
                )        
              )    
      ),
      #----
      #Visual inspection
      tabItem(tabName = "sde8",
              #----
              sidebarLayout(
                sidebarPanel(
                  width = 5,
                  h3(em("Visual inspection")),
                  p(""),
                  hr(),
                  h4("Brief Introduction"),
                  p(""),
                  HTML("In this section, we allow you to put all the models together and conduct the visual inspection (Survival Plot). You can run all the models in the app and directly 
                import the resulting data. 
                    <b>You can check the <u>Running Progress</u> on the upper right corner to see whether all the needed steps are completed.</b>
                    "),
                  p(""),
                  hr(),
                  # p(""),
                  # fileInput("upload_vi", "Load Excel file (.xlsx)", accept = c(".xlsx")),
                  # p(""),
                  # actionButton("downloadtemplate_vi", "Download example data",icon("download"),
                  #              onclick = "location.href='files/example_vi.xlsx'",
                  #              style="color: white;
                  #        background-color: cadetblue; border-color: #2e6da4"),
                  p(""),
                  actionButton("loaddata_vi", "Load modeled data",icon("gear"), 
                               style="color: white;
                       background-color: cadetblue; border-color: #2e6da4"),
                  # downloadButton("downloadtable_df_vi", "vi", icon = icon("download")),
                  p(""),
                  hr(),
                  p(""),
                  actionButton("vi_plotbt", "Draw or Update the plot",icon("gear"), 
                               style="color: white;
                          background-color: cadetblue; border-color: #2e6da4"),
                  p(""),
                  hr(),
                  p(""),
                  h4("Choose the model to display"),
                  p("Only applicable to users who load the modeled data! (You should also run all the models and draw the plots in this APP.) 
                  If you choose to upload your own data, please neglect this part."),
                  checkboxInput("vi_choose_model", strong("Enable the model selecting function"), value = FALSE)%>%
                    helper(type = "markdown", content = "aes_ud_vi"),
                  conditionalPanel(
                    condition = "input.vi_choose_model == true",
                    downloadButton("downloadtable_df_vi", "Download the aggregated modeled data.", icon = icon("download"),style="color: white;
                       background-color: cadetblue; border-color: #2e6da4"),
                    checkboxInput("vi_choose_model_ssm", strong("Standard survival models"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_ssm == true",
                      fluidRow(
                        column(width = 6, 
                               checkboxInput("ssm_Exponential", "__ssm_Exponential", value = TRUE),
                               checkboxInput("ssm_Weibull", "__ssm_Weibull", value = FALSE),
                               checkboxInput("ssm_Gamma", "__ssm_Gamma", value = FALSE),
                               checkboxInput("ssm_Log-Normal", "__ssm_Log-Normal", value = FALSE)),
                        column(width = 6, 
                               checkboxInput("ssm_Gompertz", "__ssm_Gompertz", value = FALSE),
                               checkboxInput("ssm_Log-Logistic", "__ssm_Log-Logistic", value = FALSE),
                               checkboxInput("ssm_Generalized Gamma", "__ssm_Generalized Gamma", value = FALSE),
                               checkboxInput("ssm_Generalized F", "__ssm_Generalized F", value = FALSE),)
                      ),
                    ),
                    checkboxInput("vi_choose_model_fp", strong("Fractional polynomials"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_fp==true",
                      p("FP1"),
                      checkboxInput("fp_-2", "__fp_-2", value = FALSE),
                      checkboxInput("fp_-1", "__fp_-1", value = FALSE),
                      checkboxInput("fp_-0.5", "__fp_-0.5", value = FALSE),
                      checkboxInput("fp_0.5", "__fp_0.5", value = FALSE),
                      checkboxInput("fp_1", "__fp_1", value = FALSE),
                      checkboxInput("fp_2", "__fp_2", value = FALSE),
                      checkboxInput("fp_3", "__fp_3", value = FALSE),
                      checkboxInput("fp_0", "__fp_0", value = FALSE),
                      p("FP2"),
                      fluidRow(
                        column(width = 4,
                               checkboxInput("fp_-2,-1", "__fp_-2,-1", value = FALSE),
                               checkboxInput("fp_-2,-0.5", "__fp_-2,-0.5", value = FALSE),
                               checkboxInput("fp_-2,0.5", "__fp_-2,0.5", value = FALSE),
                               checkboxInput("fp_-2,1", "__fp_-2,1", value = FALSE),
                               checkboxInput("fp_-2,2", "__fp_-2,2", value = FALSE),
                               checkboxInput("fp_-2,3", "__fp_-2,3", value = FALSE),
                               checkboxInput("fp_-1,-0.5", "__fp_-1,-0.5", value = FALSE),
                               checkboxInput("fp_-1,0.5", "__fp_-1,0.5", value = FALSE),
                               checkboxInput("fp_-1,1", "__fp_-1,1", value = FALSE),
                               checkboxInput("fp_-1,2", "__fp_-1,2", value = FALSE),
                               checkboxInput("fp_-1,3", "__fp_-1,3", value = FALSE),
                               checkboxInput("fp_-0.5,0.5", "__fp_-0.5,0.5", value = FALSE),
                        ),
                        column(width = 4,
                               checkboxInput("fp_-0.5,1", "__fp_-0.5,1", value = FALSE),
                               checkboxInput("fp_-0.5,2", "__fp_-0.5,2", value = FALSE),
                               checkboxInput("fp_-0.5,3", "__fp_-0.5,3", value = FALSE),
                               checkboxInput("fp_0.5,1", "__fp_0.5,1", value = FALSE),
                               checkboxInput("fp_0.5,2", "__fp_0.5,2", value = FALSE),
                               checkboxInput("fp_0.5,3", "__fp_0.5,3", value = FALSE),
                               checkboxInput("fp_1,2", "__fp_1,2", value = FALSE),
                               checkboxInput("fp_1,3", "__fp_1,3", value = FALSE),
                               checkboxInput("fp_2,3", "__fp_2,3", value = FALSE),
                               checkboxInput("fp_-2,-2", "__fp_-2,-2", value = FALSE),
                               checkboxInput("fp_-1,-1", "__fp_-1,-1", value = FALSE),
                               checkboxInput("fp_-0.5,-0.5", "__fp_-0.5,-0.5", value = FALSE),
                        ),
                        column(width = 4,
                               checkboxInput("fp_0.5,0.5", "__fp_0.5,0.5", value = FALSE),
                               checkboxInput("fp_1,1", "__fp_1,1", value = FALSE),
                               checkboxInput("fp_2,2", "__fp_2,2", value = FALSE),
                               checkboxInput("fp_3,3", "__fp_3,3", value = FALSE),
                               checkboxInput("fp_-2,0", "__fp_-2,0", value = FALSE),
                               checkboxInput("fp_-1,0", "__fp_-1,0", value = FALSE),
                               checkboxInput("fp_-0.5,0", "__fp_-0.5,0", value = FALSE),
                               checkboxInput("fp_0.5,0", "__fp_0.5,0", value = FALSE),
                               checkboxInput("fp_1,0", "__fp_1,0", value = FALSE),
                               checkboxInput("fp_2,0", "__fp_2,0", value = FALSE),
                               checkboxInput("fp_3,0", "__fp_3,0", value = FALSE),
                               checkboxInput("fp_0,0", "__fp_0,0", value = FALSE),
                        )
                      ),
                    ),
                    checkboxInput("vi_choose_model_rcs", strong("Restricted cubic splines"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_rcs == true",
                      checkboxInput("rcs_1", "__rcs_1", value = FALSE),
                      checkboxInput("rcs_2", "__rcs_2", value = FALSE),
                      checkboxInput("rcs_3", "__rcs_3", value = FALSE),
                      checkboxInput("rcs_4", "__rcs_4", value = FALSE),
                      checkboxInput("rcs_5", "__rcs_5", value = FALSE),
                    ),
                    checkboxInput("vi_choose_model_rp", strong("Royston-Parmar models"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_rp == true",
                      fluidRow(
                        column(width = 4,
                               checkboxInput("rp_hazard0", "__rp_hazard0", value = FALSE),
                               checkboxInput("rp_hazard1", "__rp_hazard1", value = FALSE),
                               checkboxInput("rp_hazard2", "__rp_hazard2", value = FALSE),
                               checkboxInput("rp_hazard3", "__rp_hazard3", value = FALSE),
                               checkboxInput("rp_hazard4", "__rp_hazard4", value = FALSE),
                               checkboxInput("rp_hazard5", "__rp_hazard5", value = FALSE),
                        ),
                        column(width = 4,
                               checkboxInput("rp_odds0", "__rp_odds0", value = FALSE),
                               checkboxInput("rp_odds1", "__rp_odds1", value = FALSE),
                               checkboxInput("rp_odds2", "__rp_odds2", value = FALSE),
                               checkboxInput("rp_odds3", "__rp_odds3", value = FALSE),
                               checkboxInput("rp_odds4", "__rp_odds4", value = FALSE),
                               checkboxInput("rp_odds5", "__rp_odds5", value = FALSE),
                        ),
                        column(width = 4,
                               checkboxInput("rp_normal0", "__rp_normal0", value = FALSE),
                               checkboxInput("rp_normal1", "__rp_normal1", value = FALSE),
                               checkboxInput("rp_normal2", "__rp_normal2", value = FALSE),
                               checkboxInput("rp_normal3", "__rp_normal3", value = FALSE),
                               checkboxInput("rp_normal4", "__rp_normal4", value = FALSE),
                               checkboxInput("rp_normal5", "__rp_normal5", value = FALSE),
                        ),
                      ),
                    ),
                    checkboxInput("vi_choose_model_gam", strong("Generalized additive models"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_gam == true",
                      fluidRow(
                        column(width = 6,
                               checkboxInput("gam_1", "__gam_1", value = FALSE),
                               checkboxInput("gam_2", "__gam_2", value = FALSE),
                               checkboxInput("gam_3", "__gam_3", value = FALSE),
                               checkboxInput("gam_4", "__gam_4", value = FALSE),
                               checkboxInput("gam_5", "__gam_5", value = FALSE),
                        ),
                        column(width = 6,
                               checkboxInput("gam_6", "__gam_6", value = FALSE),
                               checkboxInput("gam_7", "__gam_7", value = FALSE),
                               checkboxInput("gam_8", "__gam_8", value = FALSE),
                               checkboxInput("gam_9", "__gam_9", value = FALSE),
                               checkboxInput("gam_10", "__gam_10", value = FALSE),
                        )
                      ),
                    ),
                    checkboxInput("vi_choose_model_pmm", strong("Parametric mixture models"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_pmm == true",
                      fluidRow(
                        column(width = 4,
                               checkboxInput("pmm_exp_exp", "__pmm_exp_exp", value = FALSE),
                               checkboxInput("pmm_exp_weibull", "__pmm_exp_weibull", value = FALSE),
                               checkboxInput("pmm_exp_gamma", "__pmm_exp_gamma", value = FALSE),
                               checkboxInput("pmm_exp_lnorm", "__pmm_exp_lnorm", value = FALSE),
                               checkboxInput("pmm_exp_gompertz", "__pmm_exp_gompertz", value = FALSE),
                               checkboxInput("pmm_exp_llogis", "__pmm_exp_llogis", value = FALSE),
                               checkboxInput("pmm_exp_gengamma", "__pmm_exp_gengamma", value = FALSE),
                               checkboxInput("pmm_weibull_weibull", "__pmm_weibull_weibull", value = FALSE),
                               checkboxInput("pmm_weibull_gamma", "__pmm_weibull_gamma", value = FALSE),
                               checkboxInput("pmm_weibull_lnorm", "__pmm_weibull_lnorm", value = FALSE),
                        ),
                        column(width = 4,
                               checkboxInput("pmm_weibull_gompertz", "__pmm_weibull_gompertz", value = FALSE),
                               checkboxInput("pmm_weibull_llogis", "__pmm_weibull_llogis", value = FALSE),
                               checkboxInput("pmm_weibull_gengamma", "__pmm_weibull_gengamma", value = FALSE),
                               checkboxInput("pmm_gamma_gamma", "__pmm_gamma_gamma", value = FALSE),
                               checkboxInput("pmm_gamma_lnorm", "__pmm_gamma_lnorm", value = FALSE),
                               checkboxInput("pmm_gamma_gompertz", "__pmm_gamma_gompertz", value = FALSE),
                               checkboxInput("pmm_gamma_llogis", "__pmm_gamma_llogis", value = FALSE),
                               checkboxInput("pmm_gamma_gengamma", "__pmm_gamma_gengamma", value = FALSE),
                               checkboxInput("pmm_lnorm_lnorm", "__pmm_lnorm_lnorm", value = FALSE),
                        ),
                        column(width = 4,
                               checkboxInput("pmm_lnorm_gompertz", "__pmm_lnorm_gompertz", value = FALSE),
                               checkboxInput("pmm_lnorm_llogis", "__pmm_lnorm_llogis", value = FALSE),
                               checkboxInput("pmm_lnorm_gengamma", "__pmm_lnorm_gengamma", value = FALSE),
                               checkboxInput("pmm_gompertz_gompertz", "__pmm_gompertz_gompertz", value = FALSE),
                               checkboxInput("pmm_gompertz_llogis", "__pmm_gompertz_llogis", value = FALSE),
                               checkboxInput("pmm_gompertz_gengamma", "__pmm_gompertz_gengamma", value = FALSE),
                               checkboxInput("pmm_llogis_llogis", "__pmm_llogis_llogis", value = FALSE),
                               checkboxInput("pmm_llogis_gengamma", "__pmm_llogis_gengamma", value = FALSE),
                               checkboxInput("pmm_gengamma_gengamma", "__pmm_gengamma_gengamma", value = FALSE),
                        )
                      ),
                    ),
                    checkboxInput("vi_choose_model_mcm", strong("Mixture cure models"), value = FALSE),
                    conditionalPanel(
                      condition = "input.vi_choose_model_mcm == true",
                      fluidRow(
                        column(width = 6,
                               checkboxInput("mcm_Exponential", "__mcm_Exponential", value = FALSE),
                               checkboxInput("mcm_Weibull", "__mcm_Weibull", value = FALSE),
                        ),
                        column(width = 6,
                               checkboxInput("mcm_Log-Normal", "__mcm_Log-Normal", value = FALSE),
                               checkboxInput("mcm_Log-Logistic", "__mcm_Log-Logistic", value = FALSE),
                        )
                      ),
                    ),
                  )
                ),
                mainPanel(
                  width = 7,
                  checkboxInput("vi_showdata", strong("Show the data input"), value = FALSE),
                  conditionalPanel(
                    condition = "input.vi_showdata == true",
                    DTOutput("sheet_vi"),
                  ),
                  h4("Survival Plot"),
                  plotOutput("plot_vi"),
                  p("Black line refers to the KM curve of input data. Colorful lines refer to the curves of models. The scatter points represent the background survival."),
                  p(),
                  fluidRow(

                    column(width = 5,
                           checkboxInput("vi_bh", strong("Show the background survival (input from the MCM model)"), value = FALSE, width = '100%')%>%
                             helper(type = "markdown", content = "vi_bh"),
                           textInput("ggtitle_vi", "Title", value = "Survival Plot",width = "100%"),
                           textInput("aes_y_vi", "Name of Y-axis", value = "Survival rate",width = "100%"),
                           textInput("aes_x_vi", "Name of X-axis", value = "Survival time",width = "100%"),
                           sliderInput(inputId = "plot_vi_x",
                                       label = "Survial Time (Years):",
                                       min = 5,
                                       max = 30,
                                       value = 10,
                                       step = 5,
                                       width = "100%") %>%
                             helper(type = "markdown", content = "aes_ud_vi"),
                    )
                  ),
                  conditionalPanel(
                    condition = "output.plot_vi",
                    p(),
                    actionButton("dwld_vip", "Download plot", icon("download"), 
                                 style="color: white;
                       background-color: cadetblue; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.dwld_vip",
                    p(),
                    uiOutput("titlebut_vip"),
                    fluidRow(
                      column(width = 5,
                             uiOutput("sizebut_vip")
                      )
                    ),
                    downloadButton("downloadPlot_vip", "", icon = icon("download"))
                  ),
                  # downloadButton("downloadPlot_vi", "", icon = icon("download"))
                  # ),
                )        
              )
      )
      ### not to modify here           
    )
  )
)



# vi
# SSM   dfFigSurv_ssmsum
# FP  dfFigSurv_fpsum
# RCS dfFigSurv_rcssum
# RP dfFigSurv_rpsum
# GAM dfFigSurv_gamsum
# PMM dfFigSurv_pmmsum
# MCM 
#
#
#

