library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(dplyr)
library(datasets)
library(colorspace)
library(RColorBrewer)
library(viridisLite)
library(viridis)
library(plotly)
library(plyr)
library(dplyr)
library(htmlTable)
library(RCurl)
library(htmlwidgets)
library(htmlTable)

ui = dashboardPage(skin = "purple",
                   dashboardHeader(title="Dashboard"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Project", tabName = "Project", icon=icon("chart-line")),
                       menuItem("Users", tabName = "Users", icon=icon("users")),
                       menuItem("Delays", tabName = "Delays", icon=icon("clock")),
                       menuItem("Errors", tabName = "Errors", icon=icon("fas fa-exclamation-triangle")),
                       fileInput("file1", "Import your file",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                       )
                     )

                   ),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "Project",
                               mainPanel(
                                 tabsetPanel(type = "tabs", 
                                             tabPanel(strong("Discipline"), 
                                                      fluidRow(
                                                        box(title = "Nº of Issues by Discipline",
                                                            status = "primary",
                                                            solidHeader = T,
                                                            width=NULL,
                                                            height=NULL,
                                                            plotlyOutput("grafica1",  height = 300, width = 600)),
                                                            ),
                                                      fluidRow(
                                                        valueBoxOutput("approvalBox", width = NULL),
                                                        ),
                                                  ), 
                                             
                                             tabPanel(strong("Project"), 
                                                      fluidRow(
                                                        box(title = "Nº of Issues by Project",
                                                            status = "primary",
                                                            solidHeader = T,
                                                            width=NULL,
                                                            height=NULL,
                                                            plotlyOutput("grafica2",  height = 300, width = 600)))), 
                                             tabPanel(strong("Priority"), 
                                                      fluidRow(
                                                        box(title = "Nº of issues by Priority",
                                                            status = "primary",
                                                            solidHeader = T,
                                                            width=NULL,
                                                            height=NULL,
                                                            plotlyOutput("grafica3",  height = 300, width = 600)))), 
                                             tabPanel(strong("Blocks"), 
                                                      fluidRow(
                                                        box(title = "Nº of Blocks by Discipline",
                                                            status = "primary",
                                                            solidHeader = T,
                                                            width=NULL,
                                                            height=NULL,
                                                            plotlyOutput("grafica4",  height = 300, width = 600),
                                                            downloadButton("downloadBloqueos", "Download")))),
                                             tabPanel(strong("Completion"), 
                                                      fluidRow(
                                                        box(title = "Project Deadline",
                                                            status = "primary",
                                                            solidHeader = T,
                                                            width=NULL,
                                                            height=NULL,
                                                           plotlyOutput("grafica5",  height = 300, width = 600)
                                                            )))
                                             
                                             
                                 )
                               )),
                               tabItem(tabName = "Users", mainPanel(
                                       (tabsetPanel(type = "tabs",  
                                                     tabPanel(strong("Assignment"),
                                                              fluidRow(
                                                                box(title = "Issues assigned to a user",
                                                                    status = "success",
                                                                    solidHeader = T,
                                                                    width=NULL,
                                                                    height=NULL,
                                                                    sidebarPanel(
                                                                      selectInput("usuario", "Users:", choices=c("User01", "User02","User03","User04","User05","User06","User07"))
                                                                    ),

                                                                    mainPanel(
                                                                      plotlyOutput("userPlot", width = "400px", height = "180px"),
                                                                      DT::dataTableOutput("tableIssues", height = NULL, width = NULL),
                                                                      downloadButton("downloadAsignadas", "Download")
                                                                    )
                                                                )
                                                                    )),
                                                               
                                                     tabPanel(strong("Date"), 
                                                              fluidRow(
                                                                box(title = "Issues assigned on a Date to a User",
                                                                    status = "success",
                                                                    solidHeader = T,
                                                                    width=NULL,
                                                                    height=NULL,
                                                                    sidebarPanel( width = 5,
                                                                      dateRangeInput('dateRange',
                                                                                     label = 'Fecha',
                                                                                     start = Sys.Date() - 2, end = Sys.Date() + 2
                                                                      ),
                                                                    ),
                                                                    mainPanel(
                                                                      plotlyOutput("fechaPlot", height = 270, width = 400),
                                                                      downloadButton("downloadFechas", "Download")
                                                                    ),
                                                                ))
                                                              
                                                              
                                                              
                                                              )
                                                    ))
                                       )),
                               tabItem(tabName = "Delays",
                                       mainPanel(
                                         (tabsetPanel(type = "tabs",  
                                                      tabPanel(strong("User Date"),
                                                               fluidRow(
                                                                 box(title = "Issues assigned to Users whose end date has been exceeded",
                                                                     status = "warning",
                                                                     solidHeader = T,
                                                                     width=NULL,
                                                                     height=NULL,
                                                                     mainPanel(
                                                                     DT::dataTableOutput("tableUsuario", height = 270, width = 400),
                                                                     downloadButton("downloadUsuarioFechaKO", "Download"),
                                                                     ),          
                                                                                         
                                                                 ))
                                                      ), 
                                                      tabPanel(strong("Project Date"), 
                                                               fluidRow(
                                                                 box(title = "Issues of Projects whose completion date has been exceeded",
                                                                     status = "warning",
                                                                     solidHeader = T,
                                                                     width=NULL,
                                                                     height=NULL,
                                                                     mainPanel(
                                                                       DT::dataTableOutput("tableProyecto", height = 270, width = 400),
                                                                       downloadButton("downloadProyectoFechaKO", "Download"),
                                                                     ), 
                                                                    
                                                                 ))
                                                      ),
                                                      tabPanel(strong("Execution Time"),
                                                               fluidRow(
                                                                 box(title = "Issues assigned to Users whose execution time has been exceeded",
                                                                     status = "warning",
                                                                     solidHeader = T,
                                                                     width=NULL,
                                                                     height=NULL,
                                                                     mainPanel(
                                                                       DT::dataTableOutput("tableTiempo", height = 270, width = 400),
                                                                       downloadButton("downloadUsuarioTiempoKO", "Download"),
                                                                     ),          
                                                                     
                                                                 ))
                                                      )
                                         ))
                                       )

                                       ),
                       tabItem(tabName = "Errors",
                               mainPanel(
                                 (tabsetPanel(type = "tabs",  
                                              tabPanel(strong("State Errors"),
                                                       fluidRow(
                                                         box(title = "Issues 100% and not closed",
                                                             status = "danger",
                                                             solidHeader = T,
                                                             width=NULL,
                                                             height=NULL,
                                                             mainPanel(
                                                              DT::dataTableOutput("tableNoCerrada", height = 270, width = 400),
                                                               downloadButton("downloadNoCerrada", "Download"),
                                                             ),          
                                                             
                                                         ))
                                              ), 
                                              tabPanel(strong("Percentage Errors"), 
                                                       fluidRow(
                                                         box(title = "Issues in progress y %=0",
                                                             status = "danger",
                                                             solidHeader = T,
                                                             width=NULL,
                                                             height=NULL,
                                                             mainPanel(
                                                               DT::dataTableOutput("tableInProgress", height = 270, width = 400),
                                                               downloadButton("downloadNoDone", "Download"),
                                                             ), 
                                                             
                                                         ))
                                              ),
                                              tabPanel(strong("Assignment Errors"), 
                                                       fluidRow(
                                                         box(title = "Unassigned Issues",
                                                             status = "danger",
                                                             solidHeader = T,
                                                             width=NULL,
                                                             height=NULL,
                                                             mainPanel(
                                                               DT::dataTableOutput("tableNoAsignada", height = 270, width = 400),
                                                               downloadButton("downloadNoAsignada", "Download"),
                                                             ), 
                                                             
                                                         ))
                                              )
                                 ))
                               )
                               
                       )
                       
                       
                     )
                   )
)
