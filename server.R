#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(DT)
library(formattable)
library(rpart)

# US regions
west      <- c("WA", "OR", "CA", "NV", "AZ", "ID", "MT", "WY", "CO", "NM", "UT")
south     <- c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY", "GA", "FL", "SC", "NC", "VA", "WV")
midwest   <- c("KS", "NE", "SD", "ND", "MN", "MO", "IA", "IL", "IN", "MI", "WI", "OH")
northeast <- c("ME", "NH", "NY", "MA", "RI", "VT", "PA", "NJ", "CT", "DE", "MD", "DC")

shinyServer(function(input, output, session){
    
    getDataDemo <- reactive({
        newDataDemo <- read.csv(file = "./datasets/project3Dataset.csv", 
                         header = TRUE, 
                         sep = ",", 
                         strip.white = TRUE,
                         na.strings = c(" ", "-", "(X)", ""),
                         stringsAsFactors = FALSE)
        newDataDemo <- mutate(newDataDemo, 
                              region = ifelse(state %in% south, "South",
                                                   ifelse(state %in% northeast, "Northeast", 
                                                   ifelse(state %in% west, "West", "Midwest"))))
        newDataDemo <- na.omit(newDataDemo)
    })
    
    getDataKbp <- reactive({
        newDataKbp <- read.csv(file = "./datasets/PoliceKillingsUS.csv",
                               header = TRUE, 
                               sep = ",", 
                               strip.white = TRUE,
                               na.strings = c(" "),
                               stringsAsFactors = FALSE)
        newDataKbp$id <- NULL
        
        # Add region and year to the dataset
        newDataKbp  <- mutate(newDataKbp, region = ifelse(state %in% south, "South",
                                            ifelse(state %in% northeast, "Northeast", 
                                                   ifelse(state %in% west, "West",
                                                          "Midwest"))))
        year <- paste0("20", substring(as.character(newDataKbp$date),
                                       nchar(newDataKbp$date)-2+1,
                                       nchar(as.character((newDataKbp$date)))))
        newDataKbp <- cbind(newDataKbp, year)
        newDataKbp <- newDataKbp %>% filter(race %in% c("A", "B", "H", "N", "W"))
        newDataKbp <- na.omit(newDataKbp)
    })
    
#    values <- reactiveValues(toHighlight = rep(FALSE, length(x$Freq)), 
#                             selectedBar = NULL)
    
#    observeEvent(eventExpr = input$barPlot1Click, {
#        values$selectedBar <- x$Freq[input$barPlot1Click$x]
#        values$toHighligth <- x$Freq %in% values$selectedBar
#    })
    
    plotGraph1 <- reactive({
        newDataKbp <- getDataKbp()
        x <- as.data.frame(table(newDataKbp$region, newDataKbp$year, newDataKbp$race))
        if(input$year == "2015"){
            x <- as.data.frame(x[x$Var2 == levels(x$Var2)[1], ])
        } else if (input$year == "2016"){
            x <- as.data.frame(x[x$Var2 == levels(x$Var2)[2], ])
        } else {
            x <- as.data.frame(x[x$Var2 == levels(x$Var2)[3], ])
        }
        ggplot(x, aes(x = Var1, y = Freq, fill = factor(Var3))) +
            geom_bar(stat = "identity", 
                     position = "dodge", 
                     color = "black") + 
            labs(x = "US Region", 
                 y = NULL) + 
            theme(plot.title = element_text(hjust = 0.5, 
                                            size = 18), 
                                            #face = "bold"),
                  axis.text = element_text(size = 12),
                  legend.title = element_text(size = 14), 
                                              #face = "bold"),
                  legend.text = element_text(size = 12)) + 
            scale_fill_discrete(name = "Race", 
                                labels = c("Asian",
                                           "Black",
                                           "Hispanic",
                                           "Native American",
                                           "White")) + 
            ggtitle(paste("Number of People in the US Killed by the Police in ", input$year))
    })
    
    dataTable2 <- reactive({
        newDataKbp <- getDataKbp()
        x <- newDataKbp %>% select(year, 
                                   manner_of_death,
                                   armed,
                                   age,
                                   gender,
                                   race,
                                   threat_level,
                                   flee,
                                   city,
                                   state,
                                   region)
        if(input$year1 == "2015"){
            x <- x %>% filter(year == "2015")
        } else if(input$year1 == "2016"){
            x <- x %>% filter(year == "2016")
        } else {
            x <- x %>% filter(year == "2017")
        }
        names(x) <- c("Year",
                      "Manner of Death", 
                      "Armed", 
                      "Age",
                      "Gender",
                      "Race",
                      "Threat Level",
                      "Fleeing",
                      "City",
                      "State",
                      "Region")
        formattable(x, align = c("r", rep(ncol(x)))) 
    })
    
    dataTable3 <- reactive({
        newDataKbp <- getDataKbp()
        x <- newDataKbp %>% select(race, region, threat_level, flee)
        tm <- rpart(race ~ ., data = x)
        y <- data.frame(tm["splits"])
        y
        #return(as.datatable(formattable(y, align = c("r", rep(ncol(y))))))
    })
    
    output$lmModel <- renderPrint({
        newDataDemo <- getDataDemo()
        x <- newDataDemo
        if(input$radio == "Poverty Rate"){
            mlm <- lm(medIncome ~ povertyRate, data = x)
        } else if(input$radio == "Percent Black"){
            mlm <- lm(medIncome ~ perBlack, data = x)
        } else if(input$radio == "Percent White"){
            mlm <- lm(medIncome ~ perWhite, data = x)
        } else if(input$radio == "Percent Hispanic"){
            mlm <- lm(medIncome ~ perHis, data = x)
        } else {
            mlm <- lm(medIncome ~ perCompHighSchool, data = x)
        }
        summary(mlm)
    })

    output$pcaPlot1 <- renderPlot({
        newDataDemo <- getDataDemo()
        if(is.null(input$checkbox)){
            x <- newDataDemo %>% select(medIncome, 
                                        povertyRate)
        } else if(input$checkbox %in% c("Percent Black")){
            x <- newDataDemo %>% select(medIncome,
                                        povertyRate,
                                        perBlack)
        } else if(input$checkbox %in% c("Percent White")){
            x <- newDataDemo %>% select(medIncome,
                                        povertyRate,
                                        perWhite)
        } else if(input$checkbox %in% c("Percent White", "Percent Black")){
            x <- newDataDemo %>% select(medIncome,
                                        povertyRate,
                                        perWhite, 
                                        perBlack)
        } else {
            x <- newDataDemo %>% select(medIncome, povertyRate)
        }
        pc <- prcomp(x, center = TRUE, scale = TRUE)
        biplot(pc)
    })
    
    output$scatterPlot1 <- renderPlot({
        newDataDemo <- getDataDemo()
        x <- newDataDemo %>% filter(perAsian,
                                    perBlack, 
                                    perHis, 
                                    perNativeAm, 
                                    perWhite,
                                    povertyRate,
                                    perCompHighSchool,
                                    medIncome)
        if(input$scatter == "Percent Asian"){
            ggplot(x, aes(x = perAsian, y = medIncome)) + geom_point() +
                labs(x = "Percent of Asian Residents in the County", 
                     y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                          ggtitle(paste(input$scatter))
        } else if(input$scatter == "Percent Black"){
            ggplot(x, aes(x = perBlack, y = medIncome)) + geom_point() +
                labs(x = "Percent of Black Residents in the County", 
                     y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter))
        } else if(input$scatter == "Percent Hispanic"){
            ggplot(x, aes(x = perHis, y = medIncome)) + geom_point() +
                labs(x = "Percent of Hispanic Residents in the County", 
                     y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter))
        } else if(input$scatter == "Percent Native American"){
            ggplot(x, aes(x = perNativeAm, y = medIncome)) + geom_point() +
                labs(x = "Percent of Native American Residents in the County", 
                     y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter))
        } else if(input$scatter == "Percent White"){
            ggplot(x, aes(x = perWhite, y = medIncome)) + geom_point() +
                labs(x = "Percent of White Residents in the County", 
                     y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter))
        } else if(input$scatter == "Percent Living in Poverty"){
            ggplot(x, aes(x = povertyRate, y = medIncome)) + geom_point() +
                labs(x = "Percent of Residents Living in Poverty in the County", 
                     y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter))
        } else {
                ggplot(x, aes(x = perCompHighSchool, y = medIncome)) + geom_point() +
                    labs(x = "Percent of Residents the Completed High School in the County", 
                         y = "Median Income") +
                    theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                    ggtitle(paste(input$scatter))
        }
    })
    
    output$barPlot1 <- renderPlot({
        plotGraph1()
    })
    
    output$table1 <- DT::renderDataTable({
        newDataDemo <- getDataDemo()
        newDataKbp <- getDataKbp()
        
        if(input$reg == "Midwest"){
            x <- newDataKbp %>% filter(region == "Midwest")
            y <- newDataDemo %>% filter(region == "Midwest")
        } else if(input$reg == "Northeast"){
            x <- newDataKbp %>% filter(region == "Northeast")
            y <- newDataDemo %>% filter(region == "Northeast")
        } else if(input$reg == "South"){
            x <- newDataKbp %>% filter(region == "South")
            y <- newDataDemo %>% filter(region == "South")
        } else {
            x <- newDataKbp %>% filter(region == "West")
            y <- newDataDemo %>% filter(region == "West")
        }
        
        x <- x %>% filter(year == input$year)
        x <- x %>% filter(race %in% c("B", "H", "W"))
        x <- as.data.frame(table(x$race), stringsAsFactors = FALSE)
        x <- mutate(x, perKilled = round(Freq/sum(Freq)*100, 2))
        
        pb <- round(mean(y$perBlack), 2)
        ph <- round(mean(y$perHis), 2)
        pw <- round(mean(y$perWhite), 2)
        r  <-  c(pb, ph, pw)
        xr <- cbind(x, r)

        ppb <- round(xr[1, 2]/pb, 2)
        pph <- round(xr[2, 2]/ph, 2)
        pw  <- round(xr[3, 2]/pw, 2)
        p   <- c(ppb, pph, ppw)
        xrp <- cbind(xr, p)
        
        xrp[, 1] <- c("Black", "Hispanic", "White")
        names(xrp) <- c("Race", 
                        "Number Killed", 
                        "Percent of Total Killed", 
                        "Percent of US Population (2015)", 
                        "Proportion for Each Race (2015)")
        return(as.datatable(formattable(xrp, align = c("r", rep(ncol(xrp))))))
    })
    
    output$table2 <- DT::renderDataTable({
        dataTable2()
    })
    
    output$table3 <- DT::renderDataTable({
        dataTable3()
    })
    
    output$text1 <- renderText({
        p1 <- ("The Washington Post has a dataset of fatal shooting in the US by a police officer in the line of duty. The dataset contains the city, state, race, age and gender of the deceased. This information was gathered from law enforcement websites, local new reports, social media, and by monitoring independent databases such as 'Killed by police' and 'Fatal Encounters'.")
})

    output$text2 <- renderText({
        p1 <- ("There are six datasets used for this app. five datasets contain demographic information from the 2015 US Census and one dataset contains information on police shootings from 2015-2017.")
        HTML(paste(p1, sep = '<br/> <br/>'), "<br/> <br/>",
        "<ul>
        <li>Poverty Rate</li>
        <li>High School Graduation Rate</li>
        <li>High School Graduation Rate</li>
        <li>Median Household Income</li>
        <li>Racial Demographics</li>
        <li>Police Killings in the US</li>
        </ul>")
    })

    output$text3 <- renderText({
        paste("Odds of Being Killed by a Police Officer in the ", input$reg, "Region")
    })
    
    output$text4 <- renderUI({
        #paste("Summary from Linear Model with One Variable (", input$radio, ")")
        withMathJax(helpText("Summary from Linear Model with One Variable $$y=mx+b$$"))
    })
    
    output$downloadPlot1 <- downloadHandler(
        filename = function(){
            paste("barplot", input$butt, sep = ".")
        },
        content = function(file){
            if(input$butt == "png"){
                ggsave(file, plot = plotGraph1(), device = "png")
            } else {
                ggsave(file, plot = plotGraph1(), device = "pdf", width = 11, height = 8.5)
            }
            dev.off()
        }
    )
    
    output$downloadTable2 <- downloadHandler(
        filename = function(){
            paste("policeKillings", input$year1, ".csv", spe = "")
        },
        content = function(file){
            write.csv(dataTable2(), file)
        }
    )
})