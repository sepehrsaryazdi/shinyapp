#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(tidyverse) # Importing tidyverse, standard preprocessing library
library(janitor) # Importing janitor, standard preprocessing library
library(kableExtra) # Import kableExtra, standard table library
library(gridExtra) # Import gridExtra, standard grid controller
library(latex2exp) # LaTeX expression library
library(shiny)
library(dplyr)
library(ggthemes)
library(plotly)
library(ggplot2) # Importing ggplot2, standard plotting library
data = read.csv('DATA2x02_survey.csv') # Importing dataset
data = data %>% clean_names() # Cleaning names

covid=na.omit(data["in_the_past_2_months_how_many_times_have_you_had_a_covid_test"])
lambda_hat = mean(as.numeric(covid[[1]]))




ui <- dashboardPage(skin = "red",
  
 
  
  
  dashboardHeader(title = "Student Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("COVID-19 Tests", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )),
  dashboardBody(align="center",
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              titlePanel("Modelling Proportion of Number of COVID-19 Tests Per Student in Past 2 Months"),
              column(12,
                     box(
                       title = "Select Comparative Distribution",align="left",
                       selectInput("dist_input", "Distribution",c("Poisson","Chi-squared", "Exponential")),
                       
                       tags$h4("This analysis uses a Goodness of Fit test on the COVID-19 testing data by students in DATA2X02. This is done by using a Chi-squared approximation. Continuous distributions are continuity corrected and binned into integer positions using their cumulative distributive functions."),
                       
                       uiOutput("description"),
                       uiOutput("parameterTitle"),
                       
                       sliderInput("dist_param",label="",1,100,lambda_hat),
                       tableOutput('table1')),
                     box(plotlyOutput("plot1")),
                     
                     
                     
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
  
    
    
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    
    if(input$dist_input == "Poisson") {
      dist = round(rpois(1000,input$dist_param))
      output$parameterTitle = renderUI({
        withMathJax(paste("Adjust Parameter","\\(\\left(\\lambda\\right)\\)"))
      })
      
      output$description = renderUI({
        str1 <- withMathJax(paste("\\(H_0\\): ","The data came from a ", "\\(\\text{Pois}(\\lambda)\\)", "distribution with parameter ", "\\(\\lambda = ",input$dist_param, "\\)."));
        str2 <- withMathJax(paste("\\(H_A\\): ","The data did not come from a ", "\\(\\text{Pois}(\\lambda)\\)", "distribution with parameter ", "\\(\\lambda = ",input$dist_param, "\\)."));
        HTML(paste(str1,str2, sep = '<br/>'))
      })
      
      
    }
    
    if(input$dist_input == "Chi-squared"){
      dist = round(rchisq(1000,input$dist_param))
      output$parameterTitle = renderUI({
        withMathJax(paste("Adjust Parameter","\\(\\left(k\\right)\\)"))
      })
      
      output$description = renderUI({
        str1 <- withMathJax(paste("\\(H_0\\): ","The data came from a ", "\\(\\chi^2_{k}\\)", "distribution with parameter ", "\\(k = ",input$dist_param, "\\)."));
        str2 <- withMathJax(paste("\\(H_A\\): ","The data did not come from a ", "\\(\\chi^2_{k}\\)", "distribution with parameter ", "\\(k = ",input$dist_param, "\\)."));
        HTML(paste(str1,str2, sep = '<br/>'))
      })
      
    } 
    
    if(input$dist_input == "Exponential"){ 
      dist = round(rexp(1000,input$dist_param))
      output$parameterTitle = renderUI({
        withMathJax(paste("Adjust Parameter","\\(\\left(\\lambda\\right)\\)"))
      })
      
      
      output$description = renderUI({
        str1 <- withMathJax(paste("\\(H_0\\): ","The data came from an ", "\\(\\text{Exp}(\\lambda)\\)", "distribution with parameter ", "\\(\\lambda = ",input$dist_param, "\\)."));
        str2 <- withMathJax(paste("\\(H_A\\): ","The data did not come from an ", "\\(\\text{Exp}(\\lambda)\\)", "distribution with parameter ", "\\(\\lambda = ",input$dist_param, "\\)."));
        HTML(paste(str1,str2, sep = '<br/>'))
      })
      
    }
    ggplotly(ggplot()+
               geom_histogram(aes(x=dist,y=..count../sum(..count..),color=paste(input$dist_input,'Distribution')),alpha=0.5,fill='white')+
               geom_histogram(aes(x=covid[[1]],y=..count../sum(..count..),color='Data Distribution'),alpha=0.5,fill='white')+
               scale_y_continuous(label=scales::percent)+
               labs(x="Number of COVID-19 Tests", y="Proportion of Students", color="Distribution")+ ggtitle("Prop of No. of COVID-19 Tests Per Student (2 Month Interval)")+
               theme_gdocs(),tooltip="text")
  })
  
  output$table1 <- function(){
    
    covid = factor(covid$in_the_past_2_months_how_many_times_have_you_had_a_covid_test,levels=0:max(covid))
    covid_table =table(covid)
    
    if(input$dist_input == "Poisson") probabilities =dpois(0:10,input$dist_param) 
    
    if(input$dist_input == "Chi-squared") probabilities = pchisq(1:11-0.5,input$dist_param)-pchisq(0:10-0.5,input$dist_param)
    
    if(input$dist_input == "Exponential") probabilities = pexp(1:11-0.5,input$dist_param)-pexp(0:10-0.5,input$dist_param)
    
    probabilities[length(probabilities)] = 1-sum(probabilities[1:length(probabilities)-1])
    expectations = sum(covid_table)*probabilities
    
    fixed_expectations = c(expectations[expectations>5],sum(expectations[expectations<=5]))
    
    covid_fixed = as.numeric(c(covid_table[expectations>5],sum(covid_table[expectations<=5])))
    
    t = sum(abs(covid_fixed-fixed_expectations)^2/fixed_expectations)
    
    p_val =data.frame(c(t),c(1-pchisq(t,df=length(covid_fixed)-2)),c(0.05), c("Null Hypothesis Rejected"))
    
    colnames(p_val) = c("Test Statistic","P-Value","Significance Level","Result")
    rownames(p_val) = c("Data vs. Poisson Distribution Chi-squared Test")
    
    
    kable(p_val,caption='<strong>Hypothesis Test Results</strong>',, color="black",digits=12,round=4,escape = FALSE,format.args = list(big.mark = ",",
                                                                                                                                       scientific = FALSE))%>%
      kable_styling(bootstrap_options = c("striped", "hover"))
  }
    
}

# Run the application 
shinyApp(ui = ui, server = server)
