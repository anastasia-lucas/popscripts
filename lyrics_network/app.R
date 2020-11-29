library(shinydashboard)
library(dashboardthemes)
library(dplyr)
library(ggplot2)

filepath <- "taylorswift_lyrics.txt"

df <- read.delim(filepath, stringsAsFactors = FALSE)
levels(df$album) <- c("Fearless", "Red" , "1989",
                      "Reputation", "Lover", "Folklore")

#### UI
ui <- dashboardPage(
  dashboardHeader(title="Song Lyrics"),
  dashboardSidebar(disable = TRUE),
  #dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue-gradient"
    ),
    fluidPage(
      fluidRow(
        column(width=8,
               box(
                 plotOutput("plot1", height = 500), 
                 title="Most Common Words per Album",
                 solidHeader = FALSE,
                 status = "info",
                 numericInput("selectn", "Top N", 
                              value = 10, min = 3, max = 20, step=1),
                 width=NULL
               ))
      ))))
### Server
server <- function(input, output) { 
  output$plot1 <- renderPlot({
    
    topn_album <- df %>% 
      group_by(album, word) %>% 
      summarise(n_album=sum(n)) %>% 
      arrange(desc(n_album)) %>% 
      group_by(album) %>% 
      slice(1:input$selectn) %>%
      # labels
      mutate(name = reorder(word, n_album)) %>%
                    group_by(album, word) %>% 
                    arrange(desc(n_album)) %>% 
                    ungroup() %>% 
                    mutate(word = factor(paste(word, album, sep = "__"), 
                                         levels = rev(paste(word, album, sep = "__"))))
    
    topn_album$album <- factor(topn_album$album, levels = c("Fearless", "Red" , "1989",
                                                            "Reputation", "Lover", "Folklore"))
    
    ggplot(topn_album, aes(x=word, y=n_album, fill=n_album)) +
      geom_col() +
      scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) + 
      ylab("count") +
      coord_flip() +
      facet_wrap(.~album, scales = "free_y") +
      theme_minimal() + 
      scale_fill_gradient(high = "#f6a97a", low="#ca3c97") +
      ggtitle(paste("Top", input$selectn, "frequently used words"))

  })
}

shinyApp(ui, server)
