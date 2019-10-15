#Version2.0

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


shinyServer(function(input, output) {
    
    output$text <- renderText({
        
        "Wine is an alcohol beverage made with grapes. The textures, taste and smell of the wine depends on the grape planted. 
        A great variety of wines are selling in the market. They come from different place all over the world as most countries are making their own wine. It’s hard to make the decision among a large amount of wines. 
        Therefore, this data set provides a brief introduction about the spread of the wine made from different countries. Also, the data set presents the distribution of the quality of wines and average price of wine. 
        Moreover, the most popular wine making province and wine variety are shown. Hope this introduction and analysis can offer great help choosing a great wine."
        
    })
    
    output$table <- renderTable({
        
        
        table_data <- grade_data %>% select(country,points,price,province,variety,winery,grade) %>% head(n = 300)

        if(input$country != 'All'){
            if(input$grade != 'All'){
                table_data %>% filter(country == input$country & grade == input$grade)
            }
            else if(input$grade == 'All'){
                table_data %>% filter(country == input$country)
            }

        }
        else if(input$country == 'All'){
            if(input$grade != 'All'){
                table_data %>% filter(grade == input$grade)
            }
            else if(input$grade == 'All')(
                tbl_df(table_data)
            )
        }
       
    })
    
    
    
    output$plot_two <- renderPlot({
  
        good_points <-data %>% filter(points >91) %>%
            group_by(country)%>% drop_na(points,country)  %>% summarise(n = n()) %>%arrange(desc(n)) %>% head(n = 15)
        average_price <- data %>% select(country,points,price) %>%
            group_by(country) %>% drop_na(price,country )%>% summarize(avg_price = mean(price)) %>% arrange(desc(avg_price)) %>% head(n=30) 
        
        
        if(input$priceOrPoints == 'By Point'){
            
            good_points %>%  ggplot(aes(x =reorder(country,n), y =  n )) + geom_bar(stat='identity',colour="white", fill = c("#FF6666")) +
                    labs(x = 'Country', y = 'Number of Good Wines', title = 'Good Wine Producing Country Based on Point') +
                    coord_flip()
        }
        else if(input$priceOrPoints == 'By Price'){
            
            average_price %>%   ggplot(aes(x =reorder(country,avg_price), y =  avg_price )) +
                geom_bar(stat='identity',colour="white", fill = c("#FF6666")) +
                labs(x = 'Country', y = 'Average Price of Wine', title = 'Average Price of Wine for different country') + coord_flip()
        }
    })
    
    output$plot_hist_two <- renderPlot({
        
  
        
        if(input$priceOrPoints == "By Point"){
            ggplot(data, aes(x=points)) + geom_histogram(binwidth = 1, color= 'white',fill = c('#FF6666')) + 
                coord_cartesian(xlim = c(75, 100))+
                labs(title ="Points Distribution", x = "Points Graded", y = "Number  of Reviews") +
                scale_x_continuous(breaks=seq(75,100, by = 1))
        }
        else if(input$priceOrPoints == "By Price"){
            ggplot(data, aes(x=price)) + geom_histogram(binwidth = 5, color= 'white',fill = c('#FF6666')) + 
                coord_cartesian(xlim = c(0, 100))+
                labs(title ="Price Distribution ", x = "Price in Dollars", y = "Number  of Reviews") +
                 scale_x_continuous(breaks=seq(0,100, by = 10))
        }
    })
    

    
    output$wineProvince <- renderWordcloud2({
        
  
        
        temp <- data %>% group_by(province) %>% summarise(n = n())
        
        wordcloud2(data = temp,size = 1,shape = "star")
        
    })
    
    output$wineVar <- renderWordcloud2({
 
        
        temp1 <- data %>% group_by(variety) %>% summarise(n = n())
        
        wordcloud2(data = temp1,shape = "star")
        
    })
    
    
})
