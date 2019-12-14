#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(fluidPage(
    withMathJax(),
    titlePanel("Demographics and Fatal Police Shootings in the US"),
    tabsetPanel(
        tabPanel("Information Page",
                 mainPanel(
                     h3("Desrciption"),
                     htmlOutput("text1"),
                     h3("Datasets"),
                     htmlOutput("text2"),
                     h3("Link to Data Files on Kaggle"),
                     htmlOutput("htmlLink")
                 )
        ),
        tabPanel("Fatal Shootings (Bar Graph and Odds)", fluid = TRUE,
                 sidebarLayout(
                    sidebarPanel(
                        selectizeInput("year", 
                                       h5("Year"), 
                                       selected = "2105", 
                                       choices = c("2015", 
                                                   "2016", 
                                                   "2017")
                                       ),
                        br(),
                        selectizeInput("reg", 
                                       h5("Choose a region to see percentages and odds for the selected year."),
                                       selected = "West",
                                       choices = c("Midwest",
                                                   "Northeast",
                                                   "South", 
                                                   "West")
                                       ),
                        br(),
                        radioButtons("butt1",
                                     h5("Choose the file type to download the bar graph."),
                                     choices = list("png",
                                                    "pdf")
                                     ),
                        br(),
                        downloadButton("downloadPlot1", "Download Button")
                    ),
                    mainPanel(fluidRow(
                        plotOutput("barPlot1"),
                        br(),
                        h4(textOutput("text3"), align = "center"),
                        br(),
                        DT::dataTableOutput("table1")
                    )),
                 )
        ),
        tabPanel("Fatal Shootings (Data Tables)", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("year1", 
                                        h5("Year"), 
                                        selected = "2015", 
                                        choices = c("2015", 
                                                    "2016", 
                                                    "2017")
                                        ),
                         br(),
                         downloadButton("downloadTable2", "Download Button")
                     ),
                     mainPanel(fluidRow(
                         br(),
                         DT::dataTableOutput("table2"),
                         br()
                     ))
                 )
         ),
        tabPanel("Principal Components Analysis of Demographic Data", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("selectbox",
                                     h5("Choose components to use with median income and poverty rate"),
                                     choices = c("Percent Black",
                                                 "Percent White",
                                                 "Percent Black and Percent White"),
                                     selected = "")
                         ),
                     mainPanel(fluidRow(
                         plotOutput("pcaPlot1"),
                         br()
                     ))
                 )
        ),
        tabPanel("Models", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("radio",
                                      h5("Chose the variable to compare a linear model to a tree model."),
                                      choices = c("Poverty Rate",
                                                  "Percent White",
                                                  "Percent Black",
                                                  "Percent Hispanic",
                                                  "Percent Completed High School"),
                                      selected =  "Poverty Rate")
                         ),
                     mainPanel(fluidRow(
                         uiOutput("text4"),
                         verbatimTextOutput("lmModel"),
                         br(),
                         h5("Tree Model"),
                         plotOutput("treeModelPlot"),
                         br(),
                         verbatimTextOutput("treeModel"),
                         br(),
                         h5("Predict Using the Linear Model"),
                         br(),
                         verbatimTextOutput("predictModel")
                         ))
                 )
        ),
        tabPanel("Plots", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("scatter", 
                                        h5("Choose ademographic category to plot against median household income for all US Sates"),
                                        selected = "Percent White",
                                        choices = c("Percent Asian",
                                                    "Percent Black",
                                                    "Percent Hispanic", 
                                                    "Percent Native American",
                                                    "Percent White",
                                                    "Percent Graudated from High School",
                                                    "Percent Living in Poverty")
                                        )
                         ),
                     mainPanel(fluidRow(
                         plotOutput("scatterPlot1", 
                                    brush = "brushPoints"),
                         br(),
                         h5("Table of Selected Data Points"),
                         tableOutput("table3"),
                         br()
                      ))
                 )
        )
    )
))