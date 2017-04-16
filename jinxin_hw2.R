#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)

life_exp <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4)
fertility <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4)
pop <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv")
meta <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv")

life_exp <- life_exp[c(1, 2, 5:(length(life_exp) - 3))]
fertility <- fertility[c(1, 2, 5:(length(fertility) - 3))]
pop <- pop[c(1, 2, 5:(length(pop) - 3))]
meta <- meta[1:2]

life_exp_meta <- merge(life_exp, meta, by = "Country.Code")
fertility_meta <- merge(fertility, meta, by = "Country.Code")
pop_meta <- merge(pop, meta, by = "Country.Code")

life_exp_long <- melt(life_exp_meta, id.vars = c("Country.Code", "Country.Name", "Region"))
life_exp_long <- arrange(life_exp_long, Country.Code, Country.Name, Region)
colnames(life_exp_long) <- c("country.code", "country.name", "region", "year", "life.exp")

fertility_long <- melt(fertility_meta, id.vars = c("Country.Code", "Country.Name", "Region"))
fertility_long <- arrange(fertility_long, Country.Code, Country.Name, Region)
colnames(fertility_long) <- c("country.code", "country.name", "region", "year", "fertility")

pop_long <- melt(pop_meta, id.vars = c("Country.Code", "Country.Name", "Region"))
pop_long <- arrange(pop_long, Country.Code, Country.Name, Region)
colnames(pop_long) <- c("country.code", "country.name", "region", "year", "pop")

all_data <- merge(life_exp_long, fertility_long, 
                  by = c("country.code", "country.name", "region", "year"))
all_data <- merge(all_data, pop_long, 
                  by = c("country.code", "country.name", "region", "year"))

all_data$year <- lapply(all_data$year, function(x) substr(x, 2, nchar(as.character(x)) ))
all_data$year <- as.numeric(all_data$year)
all_data <- subset(all_data, region != "")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Life Expectancy VS. Fertility Rate"),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year:",
                     min = 1960,
                     max = 2014,
                     value = 1960,
                     animate=animationOptions(interval=800)),

         sliderInput("population",
                     "Population Dot Size:",
                     min = 1,
                     max = 5,
                     value = 1),
         checkboxGroupInput("region", label = "Select Region", choices = c("South Asia","Europe & Central Asia",
                                                                            "Middle East & North Africa", "East Asia & Pacific",
                                                                            "Sub-Saharan Africa",
                                                                            "Latin America & Caribbean",
                                                                            "North America"))
         ),
      
      mainPanel(
        div(
          style = "position:relative",
          plotOutput("lineplot", 
                     hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
          uiOutput("hover_info")
          )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   sliderValues <- reactive({input$year})
   sliderPop <- reactive({input$population})
   regionValues <- reactive({input$region})
   
   output$lineplot <- renderPlot({
       cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
       
       temp <- subset(all_data, year %in% sliderValues())
       temp <- na.omit(temp)
       
       if (is.null(regionValues()))
       {
         plt <- ggplot(data = temp)
         plt <- plt + geom_point(aes(x = life.exp, y = fertility, colour = region, size = pop))
         plt <- plt + scale_size(range = c(1, sliderPop() * 3), guide = "none") 
         plt <- plt + xlim(0, 90) + ylim(0, 9) + scale_colour_manual(values=cbPalette)
         plt <- plt + xlab("Fertility") + ylab("Life Expectancy")
         plt <- plt + theme_bw() + theme(axis.title = element_text(size = 20),
                                         legend.text = element_text(size = 15),
                                         legend.title = element_blank())
         plt <- plt + guides(colour = guide_legend(override.aes = list(shape = 15, size = 5)))
         plt            
       } else
       {
         temp1 <- subset(temp, region %in% regionValues())
         temp2 <- subset(temp, !(region %in% regionValues()))
         plt <- ggplot()
         plt <- plt + geom_point(data = temp1,
                                 aes(x = life.exp, y = fertility, colour = region, size = pop))
         plt <- plt + geom_point(data = temp2,
                                 aes(x = life.exp, y = fertility, alpha = 0.0001, size = pop), color = 'grey')
         plt <- plt + scale_size(range = c(1, sliderPop() * 3), guide = "none") + scale_alpha(guide = "none")
         plt <- plt + xlim(0, 90) + ylim(0, 9) 
         plt <- plt + xlab("Fertility") + ylab("Life Expectancy") 
         plt <- plt + theme_bw() + theme(axis.title = element_text(size = 20),
                                         legend.text = element_text(size = 15),
                                         legend.title = element_blank())
         plt <- plt + guides(colour = guide_legend(override.aes = list(shape = 15, size = 5)))
         plt  
       }
    
   })
   
   output$hover_info <- renderUI({
     hover <- input$plot_hover
     point <- nearPoints(all_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
     if (nrow(point) == 0) return(NULL)
     
     # calculate point position INSIDE the image as percent of total dimensions
     # from left (horizontal) and from top (vertical)
     left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
     top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
     
     # calculate distance from left and bottom side of the picture in pixels
     left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
     top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
     
     # create style property fot tooltip
     # background color is set so tooltip is a bit transparent
     # z-index is set so we are sure are tooltip will be on top
     style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                     "left:", left_px + 2, "px; top:", top_px + 2, "px;")
     
     # actual tooltip created as wellPanel
     wellPanel(
       style = style,
       p(HTML(paste0("<b> Country: </b>", point$country.name, "<br/>",
                     "<b> Life Expectancy: </b>", round(point$life.exp, 2), "<br/>",
                     "<b> Fertility Rate: </b>", point$fertility, "<br/>",
                     "<b> Population: </b>", point$pop)))
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

