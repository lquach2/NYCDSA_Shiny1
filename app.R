library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(maps)
require(usmap)
require(ggplot2)
library(tidyverse)
library(plotly)
library(grid)

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

df_nat <- read_csv("./national_universities_rankings1.csv",
                   col_names = c("school_name", "rank",
                                  "tuition_and_fees", "in_state"), 
                   col_types = "ccnnnnnn", skip = 1)

df_state <- read_csv("./states_by_region.csv", col_names = c("state", "abbreviation","region"), skip = 1)

df_state_reg <- merge(df_state, df_reg, by='region')

df_col_nat <- merge(df_col, df_nat, by="school_name")
df_col_natf <- df_col_nat %>% 
  select(-rank, -X5, -X6, -X7, -X8)

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  titlePanel("Guide to Choosing a College"),
  
  navlistPanel(
    "Table of Contents",
    tabPanel("Overview",
             p("Choosing a college can be difficult, let alone choosing a major which can ultimately define your entire career and your paycheck. 
             Will knowing how the possible outcome of each decision, make your decision easier?" , style="font-size:19px"),
             br(),
             p("1. Does it matter which name college I attend?", style="font-size:19px"),  
             p("2. Which major will yield the highest salary? Highest potential?", style="font-size:19px"),
             p("3. Does it matter in what region?", style="font-size:19px"),
             p("4. Will the type of college impact salary?", style="font-size:19px"),
             p("5. Is paying for an Ivy League education really worth it in regards to salary?", style="font-size:19px"),
             br(),
             p("Firstly, let's look and see if there a correlation between starting median salary and mid career salary", style="font-size:17px"),
             plotOutput("plot4"),
             p("There is a strong positive correlation between starting median salary and mid career salary, but as starting median salary increases, you begin to see a saturation, suggesting a possible
               plateau in mid career salary -- in which the starting median salary may increase but there is no increase in mid career salary.")),
    
    tabPanel("Salaries by College",
             tabsetPanel(
               tabPanel("Salaries by College",
                        p("Trends in Starting and Mid Career Salaries by College", style="font-size:17px"), 
                        br(),
                        p("Does school name impact your starting and mid career salary?"),
                        p("Depending on the school attended, there is a difference in starting and mid career salaries"),
                        plotOutput(
                          outputId = "chart1",
                          height = "4800px"))
               
             )),
    
    
    
    
    tabPanel("Salaries by Major",
             tabsetPanel(
               tabPanel("Salaries by Major",
                        p("Do starting and mid career salaries differ between majors?", style="font-size:17px"),
                        p("-Majoring in engineering or math-related subjects have an overall higher starting and mid career salary. There are a few
                          majors with higher starting salaries (i.e. Physician Assistant) do not necessarily have higher mid career salaries and vice versa (i.e. Economics, which starts out lower, but has higher mid career salary."),
                        splitLayout(cellWidths=c("50%", "50%"),
                                    plotOutput(
                                      outputId ="chart2",
                                      height = "1200px"),
                                    plotOutput(
                                      outputId = "chart3",
                                      height = "1200px"))),
               tabPanel("Salary Distribution by Major",
                        p("Distribution of salaries/major (10th, 25th, 75th, 90th) percentile mid career"),
                        p("Which majors have the most potential in regards to salary?"),
                        p("-Engineering/math-related and finance/economics majors have a wide range of salary distribution and higher median salaries mid career."), 
                        p("-Majors such as religion, spanish, education, nutrition, health care admin and nursing has a less likelihood of earning ~100K mid career, with the median being a little over $50K."),
                        p("-Majors such as Physician Assistant guarantees a higher baseline pay mid career, but the range is short and max. salary is ~125K mid career."),
                        p("-Majoring in economics yields the largest distribution of salary mid career, and the most potential of earning over $200K."),
                        p("-Different majors such as history, agriculture, journalism, communications film, and biology do not offer much of a difference in salary mid career."),
                        plotOutput(
                          outputId="chart4",
                          height="1200px"))
            )),
    
    
    
    
    tabPanel("Salaries by Region", 
             tabsetPanel(
               tabPanel("Distribution by Region", 
                        p("What is the distribution of salaries by college region?", style="font-size:17px"), plotOutput("plot1"),
                        p("There are more reported salaries by colleges in the Northeastern region than in any other region."),
                        br(),
                        p("What does the distribution of starting salaries and mid career salaries look like across regions?", style="font-size:17px"),
                        p(selectizeInput(inputId = "region",
                                         label = "Region",
                                         choices = unique(df_reg$region))), 
                        br(),
                        p(plotOutput("plot2")), 
                        p(plotOutput("plot3")),
                        p("California has the highest starting and mid career salary, with Northeast as second and also has the widest range. Western, Southern
                          and Mideastern regions are more similar in respect to starting and mid career salaries.")),
               
               tabPanel("Map and Table", p("Salaries by Region"),plotlyOutput("map1"), dataTableOutput("table1"))

               
             )),
    


    tabPanel("Salaries by College Type",
             tabsetPanel(
               tabPanel("College Types", 
                        p("Distribution of Salaries by College Type"),
                        plotOutput(outputId="chart7", height="500px"),
                        p("Majority of the data available/represented are from state colleges.")),
               tabPanel("Salaries by College Type", 
                        p("Does the type of college I attend matter?"),
                        plotOutput(outputId="chart5", height="500px"),
                        p("1. Going to an Ivy League will you give you the best chances of higher pay. Ivy League has less range in the starting salary and you're almost guaranteed ~$60K upon starting."),
                        p("2. Engineering schoos also gives you a good chance of higher starting and mid career salary."),
                        p("3. Going to a party school may have a higher minimum starting salary over going to a liberal arts or state school. However, a liberal arts college will have a slight advantage in median mid career salary over a party or state school."),
                        p("4. A state school will yield the lowest median starting and mid career salary but may have a higher max. mid career salary over a party school.")),
               tabPanel("Salaries vs Tuition Cost",
                        p("Is it worth going to an Ivy League?"),
                        plotOutput(outputId = "chart6", height="500px"),
                        p("Starting salaries upon graduating from an Ivy League is similar to an engineering school. However, to get the best
                          'bang for your buck', go to an engineering school in-state and pay in-state tuition. The starting salary is similar and you end up with less debt.")))
            ),
  
             tabPanel("Observations",
                      p("Observations/summary of dataset", style="font-size:20px"), br(),
                      p("Does it matter which name college I attend?", style="font-size:20px"),
                      p("Attending an Ivy League school is likely to have higher starting and mid career salaries as well as an engineering school."), br(),
                      p("Which major will yield the highest salary? Highest potential?", style ="font-size:20px"),
                      p("Majors in engineering/math-related subjects have higher starting and mid career salaries, 
                        whereas majoring in physician assistant will likely only have a higher starting salary, but not mid career salary when compared to other majors."),
                      p("The highest potential for earning in the top 90th percentile is majoring in economics/finance or chemical engineering."), br(),
                      p("Will the type of college impact salary?", style="font-size:20px"),
                      p("Ivy league and engineering school will most likely yield higher salaries overall."), br(),
                      p("Is paying for an Ivy League education really worth it in regards to salary?", style="font-size:20px"),
                      p("Going to an engineering school will yield similar starting salaries compared to an ivy league, with similar tuition. However, going to a state engineering school and 
                        paying in state tuition will yield similar starting salary as going to an out of state engineering school, but only paying 3x less in tuition cost"),
                      p("Best scenario, go to an ivy league college and major in engineering/math related subjects or economics, if tuition isn't a factor. 
                        If tuition is a factor, go to an in state engineering school and major in engineering.")
            
          ))

)


# Define server logic required to draw a histogram

server <- function(input, output) { 
  
    output$plot1 <- renderPlot(
            ggplot(df_reg, aes(region)) +
            geom_bar(color = 'slategray')+
            xlab(NULL) +
            ggtitle("Distribution of Salaries by Region")
     ) 
    
    df_reg1 <- df_reg %>% 
      select(., region, start_med_slry, mid_car_slry) %>% 
      gather(time_in_career, salary, start_med_slry:mid_car_slry)
    
    output$plot2 <- renderPlot(
        df_reg1 %>%
        filter(region == input$region) %>%
        ggplot(aes(x=salary)) + 
        geom_density(aes(color=time_in_career)) + 
        theme(legend.position='right') +
        scale_fill_manual(values = c('tomato', 'lightseagreen'))+        
        ggtitle("Salaries by Region") +
        theme(axis.text.x=element_text(angle=45)) +
        xlab("Salary") +
        scale_x_continuous(labels=dollar)

        
    ) 

    output$table1 <- renderDataTable(
        df_reg
    )
    
    df3 <- df_state_reg %>% 
      group_by(., state, abbreviation) %>% 
      summarise(.,avgStart= (mean(start_med_slry)), 
                avgMid=mean(mid_car_slry), color=mean(mid_car_slry)/max(mid_car_slry))
    
    df3$hover <- with(df3, paste(state, '<br>'))
    
    l <- list(color = toRGB("white"), width = 2)

    
    output$map1 <- renderPlotly(
        plot_geo(df3, locationmode = 'USA-states') %>%
        add_trace(
          z = ~avgStart, text = ~hover, locations = ~abbreviation,
          color = ~I(color), colors = c("steelblue1","steelblue")) %>%
        colorbar(title = "USD") %>%
        layout(
          title = 'Starting Salaries by State',
          geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white'))
        )
    )
    
    df_reg_dis <- df_reg %>% 
      select(region, start_med_slry, mid_car_slry) %>% 
      gather(time_in_career, salary, start_med_slry:mid_car_slry) 
    
    output$plot3 <- renderPlot(
        df_reg_dis %>% 
            ggplot(aes(x= region, y=salary)) +
            geom_boxplot(aes(fill=time_in_career)) +
            geom_jitter(aes(color=time_in_career), alpha = 0.3) +
            scale_fill_manual(values = c('tomato', 'lightseagreen'))+
            xlab(NULL) + ylab("Salary") + ggtitle("Distribution of Starting and Mid Career Salaries") +
            scale_y_continuous(labels=dollar)+
            coord_flip()
    ) 
    output$plot4 <- renderPlot(
        df_reg %>% 
            ggplot(aes(start_med_slry, mid_car_slry)) +
            geom_point(alpha = 0.5) +
            geom_jitter() + geom_smooth(se=FALSE) +
            xlab("Starting Salary") +
            ylab("Mid Career Salary") +
            scale_x_continuous(labels=dollar) +
            scale_y_continuous(labels=dollar)
    ) 
    
    df4 <- df_state_reg %>% 
      group_by(., state) %>% 
      summarise(.,avgStart= (mean(start_med_slry)), avgMid=(mean(mid_car_slry)))
    
    output$table2 <- renderDataTable(
        df4 
    
    )
    
    df_reg_gather<- df_reg %>% 
      select(school_name, region, start_med_slry, mid_car_slry) %>% 
      gather(region, salary, start_med_slry:mid_car_slry) %>% 
      arrange(desc(salary))
    
    output$chart1 <- renderPlot(
      df_reg_gather %>% 
        ggplot(aes(reorder(school_name, salary), salary, fill = region)) +
        geom_col() +
        scale_y_continuous(labels=dollar) +
        xlab(NULL) +
        theme(legend.position = "top") +
        geom_text(aes(label=dollar(salary)), hjust=1.8, size = 3, color='black') +
        coord_flip() 
    )
    
    df_deg_slry<- df_deg %>% 
      select(major, start_med_slry, mid_car_slry) %>% 
      arrange(desc(start_med_slry))
    
    output$chart2 <- renderPlot(
      df_deg_slry %>% 
        ggplot(aes(x=reorder(major, start_med_slry), start_med_slry)) +
        geom_col(fill='lightseagreen') +
        scale_y_continuous(labels=dollar) +
        geom_text(aes(label=dollar(start_med_slry)), size = 3.5, hjust=1.1, color='black') +
        xlab(NULL) +
        ggtitle("major by starting salary") +
        coord_flip() 
    )
    
    
    output$chart3 <- renderPlot(
      df_deg_slry %>% 
        ggplot(aes(x=reorder(major, mid_car_slry), mid_car_slry)) +
        geom_col(fill='tomato') +
        scale_y_continuous(labels=dollar) +
        xlab(NULL) +
        geom_text(aes(label=dollar(mid_car_slry)), size = 3.5, hjust=1.2, color='black') +
        ggtitle("major by mid career salary") +
        coord_flip() 
    )
    
    df_deg_dis <- df_deg %>% 
      select(-start_med_slry, -mid_car_slry, -percent_chng) %>% 
      gather(percentile, salary, mid_car_10th:mid_car_90th)
    
    output$chart4 <- renderPlot(
      df_deg_dis %>% 
        ggplot(aes(x=reorder(major, salary), salary)) +
        geom_boxplot(fill='slategray') +
        xlab(NULL) +
        scale_y_continuous(labels=dollar) +
        ggtitle("Percentile (10th, 25th, 75th, 90th) of Salaries by Major") +
        coord_flip() 
    )
    
    
    df_col_type <- df_col %>% 
      select(school_type, start_med_slry, mid_car_slry) %>% 
      gather(type, salary, start_med_slry:mid_car_slry)
    
    
    output$chart5 <- renderPlot(
      df_col_type %>% 
        ggplot(aes(x=reorder(school_type, salary), salary)) +
        geom_boxplot(aes(fill=type)) +
        geom_jitter(aes(color=type), alpha = 0.3)+
        scale_fill_manual(values = c('tomato', 'lightseagreen'))+
        xlab(NULL) +
        ylab('Salary')+
        ggtitle("Staring and Mid Career Salaries by School Type") +
        scale_y_continuous(labels=dollar) +
        coord_flip()
    )   

    

    df_col_ivy <- df_col_natf %>% 
      select(school_type, tuition_and_fees, in_state, start_med_slry, mid_car_slry) %>% 
      mutate(tuition=tuition_and_fees*4) %>% 
      mutate(tuitionState=in_state*4) #%>% 

    my_text <- "In State Tuition"
    my_text1 <- "Out of State Tuition"
    my_grob = grid.text(my_text, x=0.18,  y=0.05, gp=gpar(col="black", fontsize=14, fontface="bold"))
    my_grob1 = grid.text(my_text1, x=0.75,  y=0.05, gp=gpar(col="black", fontsize=14, fontface="bold"))
    
    output$chart6 <- renderPlot(
      df_col_ivy %>% 
        ggplot() +
        geom_boxplot(aes(x=tuition, y=start_med_slry, fill=school_type)) +
        geom_boxplot(aes(x=tuitionState, y=start_med_slry, fill=school_type)) +
        xlab('Tuition per 4 years') +
        ylab('Starting Salary')+
        ggtitle("Four-year Tuition vs. Starting Salary by School Type") +
        scale_y_continuous(labels=dollar) +
        scale_x_continuous(labels=dollar) +
        annotation_custom(my_grob) +
        annotation_custom(my_grob1)
      
        
    )
    
    output$chart7 <- renderPlot(
        ggplot(df_col, aes(school_type)) +
        geom_bar(color = 'slategray')+
        xlab(NULL) +
        ggtitle("Distribution of College Types")
    )
 
    
}


# Run the application 
shinyApp(ui = ui, server = server)
