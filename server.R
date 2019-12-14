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
library(rpart.plot)

# US regions
west      <- c("WA", "OR", "CA", "NV", "AZ", "ID", "MT", "WY", "CO", "NM", "UT")
south     <- c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY", "GA", "FL", "SC", "NC", "VA", "WV")
midwest   <- c("KS", "NE", "SD", "ND", "MN", "MO", "IA", "IL", "IN", "MI", "WI", "OH")
northeast <- c("ME", "NH", "NY", "MA", "RI", "VT", "PA", "NJ", "CT", "DE", "MD", "DC")

shinyServer(function(input, output, session){
    
    # Read demographic csv file
    getDataDemo <- reactive({
        newDataDemo <- read.csv(file = "./datasets/project3Dataset.csv",
                                header = TRUE, 
                                sep = ",", 
                                strip.white = TRUE,
                                na.strings = c(" ", "-", "(X)", ""),
                                stringsAsFactors = FALSE
                                )
        newDataDemo <- mutate(newDataDemo, 
                              region = ifelse(state %in% south, "South",  
                                              ifelse(state %in% northeast, "Northeast", 
                                                     ifelse(state %in% west, "West", "Midwest")
                                                     )
                                              )
                              )
        newDataDemo <- mutate(newDataDemo,
                              incomeLevel = ifelse(medIncome < 50000, "Below Average", "Above Average")
                              )
        newDataDemo <- na.omit(newDataDemo)
    })
    
    # Get police killing csv file
    getDataKbp <- reactive({
        newDataKbp <- read.csv(file = "./datasets/PoliceKillingsUS.csv",
                               header = TRUE, 
                               sep = ",", 
                               strip.white = TRUE,
                               na.strings = c(" "),
                               stringsAsFactors = FALSE
                               )
        newDataKbp$id <- NULL
        
        # Add region and year to the dataset
        newDataKbp  <- mutate(newDataKbp, 
                              region = ifelse(state %in% south, "South",
                                              ifelse(state %in% northeast, "Northeast", 
                                                     ifelse(state %in% west, "West", "Midwest")
                                                     )
                                              )
                              )
        year <- paste0("20", substring(as.character(newDataKbp$date),
                                       nchar(newDataKbp$date)-2+1,
                                       nchar(as.character((newDataKbp$date)))
                                       )
                       )
        newDataKbp <- cbind(newDataKbp, year)
        newDataKbp <- newDataKbp %>% filter(race %in% c("A", "B", "H", "N", "W"))
        newDataKbp <- na.omit(newDataKbp)
    })
    

    # Create reactive scatterplot
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
        
        # Creat plot object and add layers
        ggplot(x, aes(x = Var1, y = Freq, fill = factor(Var3))) +
            geom_bar(stat = "identity", 
                     position = "dodge", 
                     color = "black") + 
            labs(x = "US Region", 
                 y = NULL) + 
            theme(plot.title = element_text(hjust = 0.5, size = 18), 
                  axis.text = element_text(size = 12),
                  legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 12)) + 
            scale_fill_discrete(name = "Race", 
                                labels = c("Asian",
                                           "Black",
                                           "Hispanic",
                                           "Native American",
                                           "White")
                                ) + 
            ggtitle(paste("Number of People in the US Killed by the Police in ", input$year))
    })
    
    # Creat plot object  for biplot
    plotGraph2 <- reactive({
        newDataDemo <- getDataDemo()
        
        if(length(input$selectbox) == 0){
            x <- newDataDemo %>% select(medIncome, povertyRate)
        } 
        if(input$selectbox == "Percent Black"){
            x <- newDataDemo %>% select(medIncome,
                                        povertyRate,
                                        perBlack)
        }
        if(input$selectbox == "Percent White"){
            x <- newDataDemo %>% select(medIncome,
                                        povertyRate,
                                        perWhite)
        }
        if(input$selectbox == "Percent Black and Percent White"){
            x <- newDataDemo %>% select(medIncome,
                                        povertyRate,
                                        perBlack,
                                        perWhite)
        }
        pc <- prcomp(x, center = TRUE, scale = TRUE)
        biplot(pc)
    })
    
    # Create table to display police killing information
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
    
    # Create the linear model
    output$lmModel <- renderPrint({
        newDataDemo <- getDataDemo()
        x <- newDataDemo
        if(input$radio == "Poverty Rate"){
            lrm <- lm(medIncome ~ povertyRate, data = x)
        } else if(input$radio == "Percent Black"){
            lrm <- lm(medIncome ~ perBlack, data = x)
        } else if(input$radio == "Percent White"){
            lrm <- lm(medIncome ~ perWhite, data = x)
        } else if(input$radio == "Percent Hispanic"){
            lrm <- lm(medIncome ~ perHis, data = x)
        } else {
            lrm <- lm(medIncome ~ perCompHighSchool, data = x)
        }
        summary(lrm)
    })
    
    # Create tree model
    output$treeModel <- renderPrint({
        newDataDemo <- getDataDemo()
        x <- newDataDemo
        if(input$radio == "Poverty Rate"){
            tm <- rpart(medIncome ~ povertyRate, data = x)
        } else if(input$radio == "Percent Black"){
            tm <- rpart(medIncome ~ perBlack, data = x)
        } else if(input$radio == "Percent White"){
            tm <- rpart(medIncome ~ perWhite, data = x)
        } else if(input$radio == "Percent Hispanic"){
            tm <- rpart(medIncome ~ perHis, data = x)
        } else {
            tm <- rpart(medIncome ~ perCompHighSchool, data = x)
        }
        summary(tm)
    })
    
    # Create the tree model plot
    output$treeModelPlot <- renderPlot({
        newDataDemo <- getDataDemo()
        x <- newDataDemo 
        if(input$radio == "Poverty Rate"){
            tm <- rpart(medIncome ~ povertyRate, data = x)
        } else if(input$radio == "Percent Black"){
            tm <- rpart(medIncome ~ perBlack, data = x)
        } else if(input$radio == "Percent White"){
            tm <- rpart(medIncome ~ perWhite, data = x)
        } else if(input$radio == "Percent Hispanic"){
            tm <- rpart(medIncome ~ perHis, data = x)
        } else {
            tm <- rpart(medIncome ~ perCompHighSchool, data = x)
        }
        rpart.plot(tm)
    })
    
    output$scatterPlot1 <- renderPlot({
        newDataDemo <- getDataDemo()
        x <- newDataDemo
        if(input$scatter == "Percent Asian"){
            ggplot(x, aes(x = perAsian, y = medIncome)) + geom_point() +
                labs(x = "Percent of Asian Residents", y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        } else if(input$scatter == "Percent Black"){
            ggplot(x, aes(x = perBlack, y = medIncome)) + geom_point() +
                labs(x = "Percent of Black Residents", y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        } else if(input$scatter == "Percent Hispanic"){
            ggplot(x, aes(x = perHis, y = medIncome)) + geom_point() +
                labs(x = "Percent of Hispanic Residents", y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        } else if(input$scatter == "Percent Native American"){
            ggplot(x, aes(x = perNativeAm, y = medIncome)) + geom_point() +
                labs(x = "Percent of Native American Residents", y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        } else if(input$scatter == "Percent White"){
            ggplot(x, aes(x = perWhite, y = medIncome)) + geom_point() +
                labs(x = "Percent of White Residents", y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        } else if(input$scatter == "Percent Living in Poverty"){
            ggplot(x, aes(x = povertyRate, y = medIncome)) + geom_point() +
                labs(x = "Percent of Residents Living in Poverty", y = "Median Income") +
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        } else {
                ggplot(x, aes(x = perCompHighSchool, y = medIncome)) + geom_point() +
                    labs(x = "Percent of Residents that Completed High School", y = "Median Income") + 
                theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
                ggtitle(paste(input$scatter, "(", input$year, ")"))
        }
    })
    
    output$barPlot1 <- renderPlot({
        plotGraph1()
    })
    
    output$pcaPlot1 <- renderPlot({
        plotGraph2()
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
    
    output$table3 <- renderTable({
        newDataDemo <- getDataDemo()
        x <- newDataDemo
        
        if(input$scatter == "Percent Asian"){
            x <- x %>% select(state, perAsian, medIncome)
        } else if(input$scatter == "Percent Black"){
            x <- x %>% select(state, perBlack, medIncome)
        } else if(input$scatter == "Percent Hispanic"){
                x <- x %>% select(state, perHis, medIncome)
        } else if(input$scatter == "Percent Native American"){
            x <- x %>% select(state, perNativeAm, medIncome)
        } else if(input$scatter == "Percent White"){
            x <- x %>% select(state, perWhite, medIncome)
        } else if(input$scatter == "Pverty Rate"){
            x <- x %>% select(state, povertyRate, medIncome)
        } else {
            x <- x %>% select(state, perCompHighSchool, medIncome)
        }
        n = nrow(brushedPoints(x, brush = input$brushPoints))
        if(n == 0){
            return()
        } else {
            brushedPoints(x, brush = input$brushPoints)
        }
    })
    
    output$htmlLink <- renderUI({
        a("Kaggle Police Shootings Dataset", href = "https://www.kaggle.com/kwullum/fatal-police-shootings-in-the-us") 
    })
    
    output$text1 <- renderText({
        p1 <- ("The Washington Post has a dataset of fatal shootings in the US by police officers in the line of duty. The dataset contains the city, state, race, age, gender of the deceased as well as other information concerning the circumstances of the incident. The csv files were taken from . According to the owner, this information was gathered from law enforcement websites, local new reports, social media, and by monitoring independent databases such as 'Killed by police' and 'Fatal Encounters'.")
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
        paste("Odds of Being Killed by a Police Officer in the ", input$reg, "Region (", input$year,")" )
    })
    
    output$text4 <- renderUI({
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