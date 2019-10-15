##Project
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
data <- read.csv("wine1.csv")
grade_data <- data %>% mutate(grade = ifelse(points > 91,"Good",ifelse(points >86,"Average","Bad")))

# Define UI for application that draws a histogram
shinyUI(navbarPage("Wine Analysis",
                   
                   #First tab: Description
                   tabPanel("Description",h2("Wine Introduction and Analysis from all over the world"),
                            h5(textOutput("text")),
                            hr(),
                            tags$img(src = 'wine.png',width = "1250px")
                            ),
                  
                   #tabPanel("Image",tags$img(src = 'wine.png')),
                   tabPanel("Table", 
                            fluidRow(
                              column(4,
                                      selectInput("country","Select Country",c('All',unique(as.character(data$country))),selected = 'All')
                              ),
                              column(4,
                                     selectInput("grade","Select Grade",c('All',unique(as.character(grade_data$grade))),selected = 'All')
                                     )
                            ),
                            hr(),
                            tableOutput("table")
                            ),
                   
                   
                   #Second tab: two bar plots
                   tabPanel("Plots",
                            sidebarPanel(selectInput("priceOrPoints",label = "Select By Price or By Points",
                                                     choices = c("By Point","By Price"),selected = "By Point")
                            ),
                            mainPanel(plotOutput("plot_two"),hr(),plotOutput("plot_hist_two"))
                   ),
                   
                   #Third tab: two Word Cloud
                   navbarMenu("Most popular apperance",
                              tabPanel("By Wine Making Province",wordcloud2Output("wineProvince")),
                              tabPanel("By Wine Variety",wordcloud2Output("wineVar",width = "100%"))
                   )
                   
                   
))
