#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(animation)
library(ggplot2)
ani.options(outdir = getwd())

# antenna <- lapply(agent <- 1000:1010, function(x) data.frame(
#     x = rep(runif(1, min = 9, max = 11), 100),
#     y = rep(runif(1, min = 9, max = 11), 100),
#     t = seq(1, 100, 1),
#     pid = rep(x, 100),
#     type = "A"))
# 
# antenna <- do.call("rbind", antenna)
# 
# mydata <- lapply( agent <- 1:3, function(x) data.frame(
#   x = rep(10, 100) - runif(n = 100, -1, 1),
#   y = rep(10, 100) - runif(n = 100, -1, 1),
#   t = seq(1, 100, 1),
#   pid = rep(x, 100),
#   type = "P")
# )
# 
# mydata <- do.call("rbind", mydata)
# 
# mydata <- rbind(mydata, antenna)
# 
# write.csv(mydata, "/home/heaviside/Desktop/test.csv", row.names = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fluidRow( class = "plotRow",
            
# TODO height
  column(class = "myplot", width = 12, 
      plotOutput(outputId = "particle"))
  ),
  
  fluidRow( class = "controlRow",
    style = 'height:10vh',
    column(width = 3, 
                  fileInput("file1", "Choose CSV File w/ persons",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
                  ),
    column(width = 3,
                  fileInput("file2", "Choose CSV File w/ antenna's",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values, text/plain",
                              ".csv"))
                  
                  ),

    column(width = 3,
                  uiOutput("sliders")
                  ),

    column(width = 3,
                  downloadButton(outputId = "savegif",
                                 label = "Save plot to gif"))

          ),
  
  tags$head(tags$style("
                        .plotRow{height:700px;}
                        .myplot{height:600px}
                        .controlRow{height:100px;}
                       ")
            )
)
  # sidebarLayout(
  #   sidebarPanel(
  #     fileInput("file1", "Choose CSV File",
  #               accept = c(
  #                 "text/csv",
  #                 "text/comma-separated-values,text/plain",
  #                 ".csv")
  #     ),
  #     tags$hr(),
  #     checkboxInput("header", "Header", TRUE),
  #     uiOutput("sliders"),
  #     downloadButton(outputId = "savegif", label = "Save plot to gif")
  #   ),
  #   mainPanel(
  #     plotOutput("particles")
  #   )
  # )


# Define server logic 
server <- function(input, output) {
  
  # set file size limi
  options(shiny.maxRequestSize = 100*1024^2)
  
  #################### read data from csv ########################
  # agents
  agents <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, skip = 2, stringsAsFactors = FALSE)
    
    return(tbl)
  })
  
  # antenna's
  antenna <- reactive({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, skip = 2, stringsAsFactors = FALSE)
    
    return(tbl)
  })

####################################################################  
  
  
#################### create UI elements ############################  
  
  output$sliders <- renderUI(
    if (!is.null(agents)) {
      
    sliders <- sliderInput("time", "TIME:", min = min(agents()[,1]),
                            max(agents()[,1]), value = min(agents()[,1]), step = 1,
                           animate = animationOptions(interval = 500, loop = TRUE))
    }
  )
  
####################################################################
  part_anim <- reactive({
  
    df1 <- agents()[agents()[,1] == input$time,]
    df2 <- antenna()[]
    
    p <- ggplot(data = df1[]) + geom_point(aes(x = df1[,3], y = df1[,4], color = df1[,2]))  
    p <- p + xlim(0,10) + ylim(0,10)
    p <- p + geom_point(data = df2[], aes(x = df2[,1], y = df2[,2]), colour = "#CC0000")
    p <- p + xlab(label = "Longitude") + ylab("Latitude")
    p

 })


  
  output$particle <- renderPlot({
  part_anim()
  })
  
  output$savegif <- downloadHandler(
    filename = "test.gif",
    content = function(file){
      req(part_anim())
      saveGIF(
        part_anim(),
        movie.name = "test.gif"
      )
    file.rename("test.gif", file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

