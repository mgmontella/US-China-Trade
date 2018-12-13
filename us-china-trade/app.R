#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)
library(plotly)

data <- read_rds("services copy.rds")
select_goods <- read_rds("goods2 copy.rds")
export_comparison <- read_rds("export_comparison2 copy.rds")

# Define UI for application 
ui <- navbarPage(
  title = "US-China Trade",
  theme = shinytheme("sandstone"),
  # first tab (overview of trade war)
  tabPanel("Trade War Overview",
           htmlOutput("summary")
  ),
  # second tab (what this project hopes to provide)
  tabPanel("Mission",
           htmlOutput("summary_b")
  ),
  # third tab (services exchange)
  tabPanel(
    title = "US-China Services Exchange",
    sidebarLayout(
      sidebarPanel(
        selectInput("X1_06", "Choose Type of Service:",
                    choices = c("All Services" = "Services",
                                "Intellectual Property" = "Charges for the use of intellectual property n.i.e.",
                                "Maintenance and Repair" = "Maintenance and repair services n.i.e.",
                                "Air Freight Transport" = "Freight (Air)",
                                "Financial Services" = "Financial services",
                                "Travel" = "Travel",
                                "Education-related" = "Education-related",
                                "Construction" = "Construction",
                                "Telecommunications" = "Telecommunications services",
                                "Computer Services" = "Computer services",
                                "Information Services" = "Information services",
                                "Research and Development" = "Research and development services",
                                "Professional and Management Consulting" = "Professional and management consulting services",
                                "Commercial" = "Commercial services",
                                "Other Commercial" = "Other commercial services",
                                "Other Services" = "Other services",
                                "Operating Leasing" = "Operating leasing services",
                                "Legal" = "Legal services",
                                "Goods-related" = "Goods-related services",
                                "Governement Goods & Services" = "Government goods and services n.i.e.",
                                "Total Transport" = "Transport",
                                "Sea Transport" = "Sea transport",
                                "Air Transport" = "Air transport",
                                "Passenger Transport" = "Passenger (Air)",
                                "Other Modes of Transport" = "Other modes of transport",
                                "Insurance & Pension" = "Insurance and pension services",
                                "Direct Insurance" = "Direct insurance",
                                "Reinsurance" = "Reinsurance",
                                "Licenses for Research & Development" = "Licences for the use of outcomes of research and development",
                                "Licenses for Computer Software" = "Licences to reproduce and/or distribute computer software",
                                "Licenses for Other Products" = "Licences to reproduce and/or distribute other products",
                                "Franchises and Trademarks Licensing" = "Franchises and trademarks licensing fees"),
                    selected = "All Services",
                    multiple = FALSE),
        h6("Source: World Trade Organization")),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Export-Import Comparison",
            plotlyOutput("lineplot"))
        )
      )
    )
  ),
  # fourth tab panel (goods exports to China)
  tabPanel(
    title = "US Goods Exports",
    sidebarLayout(
      sidebarPanel(
        selectInput("Sector", "Choose sectors of goods to compare:",
                    choices = c("Total Merchandise" = "Total Merchandise",
                                "Fish" = "Fish",
                                "Other Food" = "Other Food",
                                "Raw Materials" = "Raw Materials",
                                "Ores and Other Minerals" = "Ores and Other Minerals",
                                "Fuels" = "Fuels",
                                "Non-ferrous Metals" = "Non-ferrous Metals",
                                "Iron and Steel" = "Iron and Steel",
                                "Pharmaceuticals" = "Pharmaceuticals",
                                "Other Chemicals" = "Other Chemicals",
                                "Other Semi-manufactures" = "Other Semi-manufactures",
                                "Electronic data processing and office equipment" = "Electronic data processing and office equipment",
                                "Telecommunications Equipment" = "Telecommunications Equipment",
                                "Integrated circuits and electronic components" = "Integrated circuits and electronic components",
                                "Automotive products" = "Automotive products",
                                "Other transport equipment" = "Other transport equipment",
                                "Other machinery" = "Other machinery",
                                "Textiles" = "Textiles",
                                "Clothing" = "Clothing",
                                "Personal and household goods" = "Personal and household goods",
                                "Scientific and controlling instruments" = "Scientific and controlling instruments",
                                "Miscellaneous manufactures" = "Miscellaneous manufactures"),
                    selected = "Total Merchandise",
                    multiple = TRUE),
        h6("Source: World Trade Organization")),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "US Goods Exports to China",
            plotlyOutput("lineplot_b")
          )
        )
      )
    )
  ),
  tabPanel(
    title = "US Exports Compared",
    sidebarLayout(
      sidebarPanel(h4("US Exports More Goods than Services to China"), h5("Source: World Trade Organization")),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "US Exports to China",
            plotlyOutput("lineplot_c")
          )
        )
      )
    )
  )
)


###########################
## Define the server logic ##
###########################

server <- function(input, output) { 
  
  output$summary <- renderUI({
    str1 <- ("Trade Tensions Have Been Escalating")
    str2 <- paste("In his 2016 presidential campaign, President Donald J. Trump railed against many of the trade agreements 
                  the United States of America is enrolled, in addition to campaigning to rectify 
                  the unequal terms of trade and business between the United States 
                  and many of its global trading partners, notably China.")
    str3 <- paste("Following through on campaign promises, President Trump began confronting China on issues relating 
                  to trade and business on January 22, 2018, when he imposed safeguard restrictions for US solar panel 
                  and washing machine companies. These safeguard restrictions imposed tariffs on $8.5 billion in solar 
                  panel imports and $1.8 billion in washing machine imports, and they were met with challenge by 
                  South Korea and China, who claimed such restrictions violated the terms of the World Trade Organization.")
    str4 <- paste("In potential retaliation on April 17, China levied duties of 178.6 percent on sorghum imports from the United States; 
                  these duties were removed about a month later.")
    str5 <- paste("On March 1, the Trump Administration announced a 25 percent tariff on all steel imports and a 10 percent tariff on all 
                  aluminum imports. This step in President's Trump trade agenda—tariffs on steel and aluminum—was ostensibly not directed at China, 
                  as only 6 percent of imports covered derived from China. However, as the Trump Administration negotiated exemptions with its NAFTA 
                  and EU partners, among other countries, China was not exempted when the tariffs went into effect on March 23.")
    str6 <- paste("On April 2, China retaliated with tariffs on aluminum waste and scrap, pork, fruits and nuts, together worth $2.4 billion in export value.")
    str7 <- paste("On April 3, President Trump then threatened to impose 25 percent tariffs on $50 billion of Chinese exports to the United States; 
                  the main industries listed are machinery, mechanical appliances, and electrical equipment. China reacts by threatening an equal measure: 
                  25 percent tariffs on $50 billion of US exports to China. The main industries threatened are auto, aircraft and agriculture; 
                  in particular, US vehicle, aircraft, vessel and soybean exports.")
    str8 <- paste("On July 6, these threats became actions, with each nation reciprocating 25 percent tariffs on $34 billion in goods.")
    str9 <- paste("On July 10, the United States announced its plan to impose 10 percent tariffs on an additional $200 billion in Chinese goods, 
                  and President Trump even threatened to impose tariffs on all imports from China.")
    str10 <- paste("By August 23, both the United States and China had imposed 25 percent tariffs on $50 billion in imports.")
    str11 <- paste(" On September 24, 10 percent tariffs on an additional $200 billion in Chinese goods took effect; China responded by taxing an additional 
                   $60 billion in US goods. At this point, 12 percent of US imports were taxed and 8 percent of US exports faced tariffs abroad.")
    str12 <- paste("On December 1, at the G-20 meeting in Buenos Aires, President Trump and President Xi agreed to halt the escalation of trade war as they negotiate over trade concerns.")
    str13 <- paste("Source: https://piie.com/system/files/documents/trump-trade-war-timeline.pdf")
    
    HTML(paste(h1(str1), p(str2), p(str3), p(str4), p(str5), p(str6), p(str7), p(str8), p(str9), p(str10), p(str11), p(str12), h6(str13)))
  })
  
  output$summary_b <- renderUI({
    str1 <- ("The Goal of This Project")
    str2 <- paste("The escalating trade tension between the United States and China surfaces many questions: who will win this trade war? 
                  How will this trade war affect Americans and Chinese? 
                  And how will this trade war affect each nation’s economy and various sectors within each economy?")
    str3 <- paste("Rather than trying to answer these questions, this project intends to provide context for analysis 
                  and speculation relating to the US-China trade war. ")
    str4 <- paste("The context provided includes a breakdown of the services exchange between the United States and China, 
                  as well as a representation of the United States’ goods exports to China. ")
    str5 <- paste("Data From:")
    str6 <- paste("All data used is taken from the website of the World Trade Organization.")
    
    HTML(paste(h2(str1), p(str2), p(str3), p(str4), h2(str5), p(str6)))
  })
  
  output$lineplot <- renderPlotly({
    
    lineplot <- data %>%
      filter(X1_06 == input$X1_06) %>%
      ggplot(aes(x = Year, y = Value, color = X1_08, group = X1_08)) + 
      geom_point() + 
      geom_line() + 
      labs(title = "US-China Trade in Services over Time") + 
      ylab("Millions of USD") + 
      xlab("Year") + 
      theme(text = element_text(family = "Times New Roman", size = 14), panel.background = element_blank()) + 
      theme(legend.title=element_blank())
    
    ggplotly(lineplot)
  })
  
  output$lineplot_b <- renderPlotly({
    
    lineplot_b <- select_goods %>%
      filter(Sector == input$Sector) %>%
      ggplot(aes(x = Year, y = Value, color = Sector, group = Sector)) + 
      geom_point() + 
      geom_line() + 
      labs(title = "US Goods Exported to China") + 
      ylab("Millions of USD") + 
      xlab("Year") + 
      theme(text = element_text(family = "Times New Roman", size = 14), panel.background = element_blank()) + 
      theme(legend.title=element_blank())
    
    ggplotly(lineplot_b)
  })
  
  output$lineplot_c <- renderPlotly({
    
    lineplot_c <- export_comparison %>%
      ggplot(aes(x = Year, y = Value, color = Sector, group = Sector)) + 
      geom_point() + 
      geom_line() + 
      labs(title = "US Exports to China") + 
      ylab("Millions of USD") + 
      xlab("Year") + 
      theme(text = element_text(family = "Times New Roman", size = 14), panel.background = element_blank()) + 
      theme(legend.title=element_blank()) 
    
    ggplotly(lineplot_c)
  })
}

###########################
## Run the application ##
###########################

shinyApp(ui = ui, server = server)

