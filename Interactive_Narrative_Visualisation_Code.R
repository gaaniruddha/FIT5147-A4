# Code for the Narrative Visualisation 

# Loading the required libraries
library(shiny)
#library(datasets)
library(ggplot2)
#library(scales)
library(gridExtra)

# Loading the dataset into R
youtube_data <- read.csv("youtube_formatted_data.csv")

# defing the ui code 
ui <- fluidPage(
    
  titlePanel("TRENDING YOUTUBE VIDEO ANALYSIS!"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      p("Youtube is no doubt the most used video website around the world for more than the last ten or more years. This narrative visualisation is mainly 
          for those aspiring young youtubers, who want to derive insights from the existing youtube videos and release videos which are most likely
          to become a trend. Here, in our analysis we have narrowed it down to four main factors which affect the popularity of a video. Now, you will know the best of -
          WHICH DAY, WHAT TIMES of the day, VIDEO LENGTH title and the VIDEO CATEGORY your video should revolve around, for it to become a trend! 
         "),
      
      # Defining the input for the publishing day
      # Here, we can view the output for single and multiple publishing days 
      h3("1) Publishing Day"),
      p("You can now select a specific day/days in order to view the number of trending videos released on those days."),
      selectInput("day", "Select Publishing Days:", c( "Monday" = "Mon",
                                                       "Tuesday" = "Tue",
                                                       "Wednesday" = "Wed",
                                                       "Thursday" = "Thu",
                                                       "Friday" = "Fri",
                                                       "Saturday" = "Sat",
                                                       "Sunday" = "Sun"), multiple = TRUE),
      
      # Defining the input for the publishing hour 
      # Here, we can view the output for single and multiple publishing hours
      h3("2) Publishing Hour"),
      p("You can now select a particular time in order to view the number of trending videos released at the selected times in a given day."),
      selectInput("hour", "Select Publishing Hours:", c( "12 AM" = "0", "1 AM" = "1", "2 AM" = "2", "3 AM" = "3", "4 AM" = "4",
                                                         "5 AM" = "5", "6 AM" = "6", "7 AM" = "7", "8 AM" = "8", "9 AM" = "9",
                                                         "10 AM" = "10","11 AM" = "11","12 AM" = "12","13 AM" = "13","14 AM" = "14",
                                                         "3 PM" = "15", "4 PM" = "16", "5 PM" = "17", "6 PM" = "18", "7 PM" = "19",
                                                         "8 PM" = "20","9 PM" = "21", "10 PM" = "22", "11 PM" = "23"), multiple = TRUE),
      
      # Defining the input for video title length
      # Here, we can view the output for various video title lengths 
      h3("3) Video Title Length"),
      p("You can now select the time in order to view the number of trending videos released at the selected times in a given day."),
      # Defining the input for the number of characters in the video title 
      sliderInput("characters", label = "Select Number of Characters in the Video Title:",
                  min = 5, max = 95, value = c(0, 100)),
      
      h3("4) Video Category"),
      p("You can now select a particular video category in order to view the number of trending videos released at the selected times in a given day."),
      # Defining the input for the categories
      # Here, we can view the output for single and multiple categories 
      selectInput("category", "Select Category:", c(   "Comedy" = "Comedy",
                                                       "Education" = "Education",
                                                       "Entertainment" = "Entertainment",
                                                       "Film and Animation" = "Film & Animation",
                                                       "Gaming" = "Gaming",
                                                       "Music" = "Music",
                                                       "News & Politics" = "News & Politics",
                                                       "People & Blogs" = "People & Blogs",
                                                       "Pets & Animals" = "Pets & Animals",
                                                       "Science & Technology" = "Science & Technology",
                                                       "Sports" = "Sports",
                                                       "Travel & Events" = "Travel & Events" ), multiple = TRUE)
      
      
    ), 
    
    # Outputs of ggplot
    mainPanel(
      h2("Videos according to Publishing Day and Publishing Hour"),
      fluidRow(splitLayout(cellWidths = c("50%","50%"),
      plotOutput("my_plot1"),
      plotOutput("my_plot2"))),
      
      h2("Videos according to Video Title Length and Category"),
      fluidRow(splitLayout(cellWidths = c("50%","50%"),
      plotOutput("my_plot3"),
      plotOutput("my_plot4")))
    )
  )
  
)
# Defining the server code 
server <- function(input, output) {
  
  # PLOT 1 
  # Plotting the Videos According to the Publishing Day 
  output$my_plot1 <- renderPlot({
    
    # Taking only the needed data according to the type selected     
    
    y1_data <- youtube_data[youtube_data$publishing_day == input$day,]
    
    # Plotting using ggplot2 
    
      ggplot(data = y1_data, aes(x=publishing_day) ) +
      geom_bar(aes(fill=publishing_day)) +  
      ggtitle("Trending Videos according to Publishing Days") +
      ylab(label = "Video Count") +
      xlab(label = "Publishing Day") + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
      }, height = 400, width = 600) 
      
      
  # PLOT 2 
  # Plotting the Videos According to the Publishing Hour 
   output$my_plot2 <- renderPlot({
    
    # Taking only the needed data according to the type selected     
    y2_data <- youtube_data[youtube_data$publishing_hour == input$hour,]
    
    # Plotting using ggplot2 
    ggplot(data = y2_data, aes(x=publishing_hour,fill = factor(publishing_hour) ) )  +
      geom_bar() +  
      ggtitle("Trending Videos according to Publishing Hours") +
      ylab(label = "Publishing Hour") +
      xlab(label = "Video Count") + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
      }, height = 400, width = 600)
   
   
   # PLOT 3 
   # Plotting the Videos According to the Video Title Length 
   output$my_plot3 <- renderPlot({
     
     # Taking only the needed data according to the type selected     
     y3_data <- youtube_data[youtube_data$title_length == input$characters,]
     
     
     # Plotting using ggplot2
     ggplot(data = y3_data, aes(y=title_length, fill=factor(title_length))) +
       geom_bar() +  
       ggtitle("Trending Videos according to Video Title Length") +
       ylab(label = "Video Count") +
       xlab(label = "Video Title Length") + 
       theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
       coord_polar("y") 
       } , height = 400, width = 600)
   
   # PLOT 4 
   # Plotting the Videos According to the Category
   output$my_plot4 <- renderPlot({
     
     # Taking only the needed data according to the type selected     
     y4_data <- youtube_data[youtube_data$category_name == input$category,]
     
     # Plotting using ggplot2
    
     ggplot(data = y4_data, aes(x=category_name,y = days_to_trending)) +
       geom_line(color="grey") + 
       geom_point(aes(color = factor(category_name))) +  
       ggtitle("Trending Videos according to Video Category") +
       ylab(label = "Video Count") +
       xlab(label = "Video Category") + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1))
   }, height = 400, width = 600)
 
}

# Running the shiny application 
shinyApp(ui, server)
