#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Data"),
   
   fluidRow(
        column(4,
         sliderInput("initial",
                     "Initial Amount:",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500,
                     pre = "$", 
                     sep = ",")),
        
        column(4,
               sliderInput("return_rate",
                           "Return Rate (in %)",
                           min = 0,
                           max = 20,
                           value = 5,
                           step = 0.1)),
        column(4,
               sliderInput("year",
                           "Years",
                           min = 0,
                           max = 50,
                           value = 10,
                           step = 1)),
         column(4,
                sliderInput("annual_contribution",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     value=2000,
                     step = 500,
                     pre = "$", 
                     sep = ",")),
         column(4,
                sliderInput("growth_rate",
                     "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 2,
                     step = 0.1)),
         column(4,
                selectInput("facet",
                     "Facet?",
                     choices = c("No","Yes")
                     ))
         
      ),
      
      # Show a plot of the generated distribution
   fluidRow(
     h4("Timelines"),
     column(12,
            plotOutput("savings_plot"))),
   fluidRow(
     h4("Balances"),
     column(12,
            dataTableOutput("summary")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$savings_plot <- renderPlot({
     init<-input$initial
     an_cont<-input$annual_contribution
     ret_rate<-input$return_rate/100
     gr_rate<-input$growth_rate/100
     yr<-input$year
     fac<-input$facet
     
     
     #' @title future_value
     #' @description calculates future value of an investment
     #' @param amount (initial invested amount)
     #' @param rate (annual rate of return)
     #' @param years (number of years)
     #' @return future value of amount
     future_value<-function(amount,rate,years) {
       return(amount*(1+rate)^years)
     }
     
     #' @title annuity
     #' @description computes the future value of annuity
     #' @param contrib (contributed amount)
     #' @param rate (annual rate of return)
     #' @param years (number of years)
     #' @return future value of annuity (using the 3 different parameters)
     annuity<-function(contrib,rate,years) {
       return (contrib*(((1+rate)^years-1)/rate))
     }
     
     #' @title growing_annuity
     #' @description computes the future value of growing annuity
     #' @param contrib (contributed amount)
     #' @param rate (annual rate of return)
     #' @param growth (annual growth rate)
     #' @param years (number of years)
     #' @return future value of growing annuity (using the 4 different parameters)
     growing_annuity<-function(contrib,rate,growth,years) {
       numerator<-(1+rate)^years-(1+growth)^years
       denominator<-rate-growth
       return (contrib*(numerator/denominator))
     }
     

     #' @title savings_function
     #' @description computes savings values
     #' @param init initial invested amount
     #' @param ret_rat annual rate of return
     #' @param an_cont contributed amount
     #' @param gr_rate annual growth rate
     #' @return savings values for no contribution, fixed contribution, and growing contribution for each of the years in a data frame
     savings_function<-function(init,ret_rat,an_cont,gr_rate) {
       no_cont <- rep(init,yr)
       fixed_cont <- rep(init,yr)
       growing_cont <- rep(init,yr)
       for (i in c(1:yr+1)) {
         no_cont[i]<- future_value(amount = init, rate = ret_rat, years = i-1)
         fixed_cont[i]<- future_value(amount = init, rate = ret_rat, years = i-1) +
           annuity(contrib = an_cont, rate = ret_rat, years = i-1)
         growing_cont[i]<- future_value(amount = init, rate = ret_rat, years = i-1) +
           growing_annuity(contrib = an_cont, rate = ret_rat, growth = gr_rate, years = i-1)
       }
       return(data.frame(year=c(0:yr),no_contrib=no_cont,fixed_contrib=fixed_cont,growing_contrib=growing_cont))
     }
     
     
     modalities <- savings_function(init, ret_rate, an_cont, gr_rate)
     
     facet_modalities<-melt(data = modalities, id = c("year"))
     
     if (fac=="No") {
       ggplot(data=modalities)+
         geom_line(aes(x=year, y=no_contrib,color="no_contrib"), show.legend = T) +
         geom_point(aes(x=year, y=no_contrib,color="no_contrib")) +
         geom_line(aes(x=year, y=fixed_contrib,color="fixed_contrib"), show.legend = T) +
         geom_point(aes(x=year, y=fixed_contrib,color="fixed_contrib")) +
         geom_line(aes(x=year, y=growing_contrib,color="growing_contrib"), show.legend = T) +
         geom_point(aes(x=year, y=growing_contrib,color="growing_contrib")) +
         labs(x="Year",y="Dollars ($)") +
         ggtitle("Three modes of investing")
     } else {
       fno<-subset(facet_modalities, variable=="no_contrib")
       ffixed<-subset(facet_modalities, variable=="fixed_contrib")
       fgrowing<-subset(facet_modalities, variable=="growing_contrib")
       ggplot(data=facet_modalities)+
         geom_area(aes(x=year, y=value,fill=variable), alpha=0.5, show.legend = T) +
         geom_line(data = fno, aes(x=year, y=value),color="red", alpha = 0.5) +
         geom_point(data=fno, aes(x=year, y=value), color = "red", alpha=0.5) +
         geom_line(data = ffixed, aes(x=year, y=value),color="green", alpha = 0.5) +
         geom_point(data=ffixed, aes(x=year, y=value), color = "green", alpha=0.5) +
         geom_line(data = fgrowing, aes(x=year, y=value) ,color="blue", alpha = 0.5) +
         geom_point(data=fgrowing, aes(x=year, y=value), color = "blue", alpha=0.5) +
         labs(x="Year",y="Dollars ($)") +
         ggtitle("Three modes of investing") +
         theme_bw() +
         facet_wrap(~variable)
     }

   })
   output$summary<- renderDataTable({
     init<-input$initial
     an_cont<-input$annual_contribution
     ret_rate<-input$return_rate/100
     gr_rate<-input$growth_rate/100
     yr<-input$year
     fac<-input$facet
     
     #' @title future_value
     #' @description calculates future value of an investment
     #' @param amount (initial invested amount)
     #' @param rate (annual rate of return)
     #' @param years (number of years)
     #' @return future value of amount
     future_value<-function(amount,rate,years) {
       return(amount*(1+rate)^years)
     }
     
     #' @title annuity
     #' @description computes the future value of annuity
     #' @param contrib (contributed amount)
     #' @param rate (annual rate of return)
     #' @param years (number of years)
     #' @return future value of annuity (using the 3 different parameters)
     annuity<-function(contrib,rate,years) {
       return (contrib*(((1+rate)^years-1)/rate))
     }
     
     #' @title growing_annuity
     #' @description computes the future value of growing annuity
     #' @param contrib (contributed amount)
     #' @param rate (annual rate of return)
     #' @param growth (annual growth rate)
     #' @param years (number of years)
     #' @return future value of growing annuity (using the 4 different parameters)
     growing_annuity<-function(contrib,rate,growth,years) {
       numerator<-(1+rate)^years-(1+growth)^years
       denominator<-rate-growth
       return (contrib*(numerator/denominator))
     }
     
     #' @title savings_function
     #' @description computes savings values
     #' @param init initial invested amount
     #' @param ret_rat annual rate of return
     #' @param an_cont contributed amount
     #' @param gr_rate annual growth rate
     #' @return savings values for no contribution, fixed contribution, and growing contribution for each of the years in a data frame
     savings_function<-function(init,ret_rat,an_cont,gr_rate) {
       no_cont <- rep(init,yr)
       fixed_cont <- rep(init,yr)
       growing_cont <- rep(init,yr)
       for (i in c(1:yr+1)) {
         no_cont[i]<- future_value(amount = init, rate = ret_rat, years = i-1)
         fixed_cont[i]<- future_value(amount = init, rate = ret_rat, years = i-1) +
           annuity(contrib = an_cont, rate = ret_rat, years = i-1)
         growing_cont[i]<- future_value(amount = init, rate = ret_rat, years = i-1) +
           growing_annuity(contrib = an_cont, rate = ret_rat, growth = gr_rate, years = i-1)
       }
       return(data.frame(year=c(0:yr),no_contrib=no_cont,fixed_contrib=fixed_cont,growing_contrib=growing_cont))
     }
     
     modalities <- savings_function(init, ret_rate, an_cont, gr_rate)
     modalities
   })
}

# Run the application
shinyApp(ui = ui, server = server)

