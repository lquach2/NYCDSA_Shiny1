library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(maps)
require(usmap)
require(ggplot2)
library(tidyverse)
library(plotly)


setwd("~/Desktop/ShinyApp")

csv_files <- list.files(pattern = ".csv")

df_deg <- read_csv("./degrees-that-pay-back.csv",
                   col_names = c("major", "start_med_slry", "mid_car_slry",
                                 "percent_chng", "mid_car_10th", "mid_car_25th",
                                 "mid_car_75th", "mid_car_90th"),  
                   col_types = "cnndnnnn", 
                   skip = 1)  

df_col <- read_csv("./salaries-by-college-type.csv",
                   col_names = c("school_name", "school_type", "start_med_slry",
                                 "mid_car_slry", "mid_car_10th", "mid_car_25th",
                                 "mid_car_75th", "mid_car_90th"),
                   col_types = "ccnnnnnn", skip = 1)

df_reg <- read_csv("./salaries-by-region.csv",
                   col_names = c("school_name", "region", "start_med_slry",
                                 "mid_car_slry", "mid_car_10th", "mid_car_25th",
                                 "mid_car_75th", "mid_car_90th"),
                   col_types = "ccnnnnnn", skip = 1)


df_state <- read_csv("./states_by_region.csv", col_names = c("state", "abbreviation","region"), skip = 1)

df_state_reg <- merge(df_state, df_reg, by='region')


df3 <- df_state_reg %>% 
  group_by(., state, abbreviation) %>% 
  summarise(.,avgStart= (mean(start_med_slry)), 
            avgMid=mean(mid_car_slry), color=mean(mid_car_slry)/max(mid_car_slry))

df4 <- df_state_reg %>% 
    group_by(., state) %>% 
    summarise(.,avgStart= (mean(start_med_slry)), avgMid=(mean(mid_car_slry)))

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  titlePanel("Application Title"),
  
  navlistPanel(
    "Header",
    tabPanel("Plot", 
             tabsetPanel(
               tabPanel("Plot", p("hello"), plotOutput("plot1")), 
               tabPanel("Distribution by Region", 
                        p(selectizeInput(inputId = "region",
                                         label = "Region",
                                         choices = unique(df_reg$region))), 
                        p(plotOutput("plot2"))),
               
               tabPanel("All", plotOutput("plot3")), 
               tabPanel("Corr", plotOutput("plot4")), 
               tabPanel("Summary Table", dataTableOutput("table2")),
               #tabPanel("Summary", verbatimTextOutput("summary")), 
               tabPanel("Table", dataTableOutput("table1")),
               tabPanel("Map", plotlyOutput("map1")),
               p("hello")
               
             )),
    tabPanel("Second"),
    tabPanel("Third")
  )
)


# Define server logic required to draw a histogram

server <- function(input, output) { 
  
    output$plot1 <- renderPlot(
            ggplot(df_reg, aes(region)) +
            geom_bar(color = 'black', alpha = 0.8)
     ) 
    output$plot2 <- renderPlot(
        df_reg %>%
        filter(region == input$region) %>%
        ggplot() + 
        geom_density(aes(x=start_med_slry, color='starting median salary')) + 
        geom_density(aes(x=mid_car_slry, color='mid career salary')) +
        theme(legend.position='right') +
        scale_color_manual(values = c('mid career salary' = 'red', 'starting median salary' = 'blue')) +
        ggtitle("Salaries by Region") +
        theme(axis.text.x=element_text(angle=45)) +
        xlab("Salary (USD$)") +
        #scale_x_continuous(labels=dollar)
        scale_x_continuous(breaks=seq(5000, 300000, 5000))
    ) 

    output$table1 <- renderDataTable(
        df_reg #%>% 
            #select(., school_name, region, start_med_slry, mid_car_slry)
    )
    
    
    df3$hover <- with(df3, paste(state, '<br>', 
                                 "Average Mid Career Salary", format(avgMid, digits=2), '<br>',
                                 "Average Med Starting Salary", format(avgStart,digits=2)))
    
    l <- list(color = toRGB("white"), width = 2)

    
    output$map1 <- renderPlotly(
        plot_geo(df3, locationmode = 'USA-states') %>%
        add_trace(
          z = ~avgStart, text = ~hover, locations = ~abbreviation,
          color = ~I(color), colors = 'Blues') %>%
        colorbar(title = "USD") %>%
        layout(
          title = 'Median Starting and Mid Career Salaries by State',
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white'))
        )
    )

    output$plot3 <- renderPlot(
        df_reg %>% 
            ggplot(aes(x=region, y=start_med_slry)) +
            geom_boxplot(aes(fill=region)) +
            geom_jitter() +
            #facet_wrap( ~ region, scales="free") +
            xlab("Region") + ylab("Salary") + ggtitle("Distribution of Salaries") +
            coord_flip()
    ) 
    output$plot4 <- renderPlot(
        df_reg %>% 
            ggplot(aes(start_med_slry, mid_car_slry)) +
            geom_point(alpha = 0.5) +
            geom_jitter() + geom_smooth() +
            xlab("Starting Median Salary (USD$)") +
            ylab("Mid Career Salary (USD$)")
            #scale_x_continuous(labels=dollar)
        #strong correlation although, it looks like its almost saturating as start med salary increases
    ) 
    
    output$table2 <- renderDataTable(
        df4 #%>% 
        #select(., school_name, region, start_med_slry, mid_car_slry)
    
    )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
