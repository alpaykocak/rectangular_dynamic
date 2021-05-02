library(shiny)
library(tidyverse)
library(gapminder)
library(treemapify)
dattum <- 
ui <-fluidPage(
    titlePanel("Rectangular Presentation of Gapminder Indicators"),
    sidebarLayout(      
        sidebarPanel(
            selectInput("continent", "Continent:", 
                        choices = c("Asia" = "Asia", "Europe" = "Europe", "Africa" = "Africa", "Americas" = "Americas", "Okyanusya" = "Oceania")),
            hr(),
            selectInput("indicator", "Gösterge:", 
                        choices = c("Total Real GDP" = "totgdp", "Population"  ="pop", "Real GDP per capita" = "gdpPercap")),
            helpText("Gapminder Data")
        ),
        mainPanel(
            plotOutput("rectPlot")  
        )
        
    ),
    fluidRow(
        shiny::column(4, offset = 4,
                      sliderInput("year", "Year",
                                  min = min(gapminder$year), max = max(gapminder$year),
                                  value = min(gapminder$year), animate = TRUE,
                                  step = 5,sep = "")
        )
    )
)
server <- function(input, output) {
    n <- reactive({
        data <- gapminder %>% mutate(totgdp = gdpPercap*pop/1000000000, pop = round(gapminder$pop/1000000,1)) %>% filter(continent == input$continent & year == input$year) %>%
        select(country, continent, year, input$indicator)
    })
    name <- reactive({
        if (input$indicator == "pop") {
        
            paste0("Population by year (Millions)")
        } else if (input$indicator == "gdpPercap") {
            paste0("Real GDP per capit by year ($)")
        } else {
            paste0("Total Real GDP (Billion $)")
        }
    })
    output$rectPlot <- renderPlot({
        veri <- as.data.frame(n())
        ggplot(veri, aes(area = veri[,4],fill = veri[,1], label = paste0(veri[,1],"-",round(veri[,4])))) +
            geom_treemap(show.legend = F) +
            geom_treemap_text() +
            labs(title = name(),
                 caption  = "Data source: Gapminder & Author's calculation") 
    })
}
# Run the application 
shinyApp(ui = ui, server = server)