library(shiny)
require(XML)
require(httr)
require(mosaic)
library(dplyr)
require(tidyr)
require(rvest)
require(wordcloud)

require(tm)
require(lubridate)
require(stringr)
require(ggplot2)
library(SnowballC) 
library(reshape2)
#library(dplyr)
#library(tidytext)


#Create a grief corpus 
load("NatureCorpus.Rda")
load("ReligionCorpus.Rda")
load("GriefCorpus.Rda")
load("WarCorpus.Rda")
load("LoveCorpus.Rda")
load("FameCorpus.Rda")
load("sentiments1.Rda")
sent <- load("sentimentPlot.Rda")
load("gutenberg.Rda")

#setwd("~/ShinyApps/EmilyDickinsonApp")
#setwd("~")

ui<-navbarPage("Emily Dickinson's Poetry",
               tabPanel("Visualizing Emily's Poetry Corpus",
                        sidebarLayout(
                          sidebarPanel(
                            
                            sliderInput("freq",
                                        "Minimum Frequency:",
                                        min = 1,  max = 50, value = 15),
                            sliderInput("max",
                                        "Maximum Number of Words:",
                                        min = 1,  max = 300,  value = 100),
                            strong("Specify the minimum frequency of words to
                                   display (words with at least the specified frequency are shown)
                                   and/or specify the maximum number of words to print.")
                            ),
                          mainPanel(
                            plotOutput("plot2", height = "900px")
                          )
                            )
               ),
               tabPanel("Thematizing Emily's Poetry",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("select", label = h3("Select a Theme"),
                                        choices = list("Nature", "Religion",
                                                       "Love", "Grief", "War", "Fame"),
                                        selected = "Nature"),
                            fluidRow(column(3, verbatimTextOutput("value")),
                                     sliderInput("slider1", label = h4("How many words?"), min = 0,
                                                 max = 20, value = 10),
                                     actionButton("start", "Show frequency!"),
                                     hr(),
                                     sliderInput("slider2", label = h4("How many Poems?"), min = 0,
                                                 max = 20, value = 5),
                                     actionButton("pickpoem", "Pick a poem!")),
                            hr(),
                            strong("Specify a theme to display the frequency of synonyms and antonyms in that theme found in Emily's work and/or
                                   generate a random Emily poem fitting that thematic category.")),
                          mainPanel(plotOutput("plot"),
                                    verbatimTextOutput("displaypoem"))
                            )
                        )
               )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) { 
  location <- "gutenberg"
  #mycorpus <- VCorpus(DirSource(location))
  #mycorpus[[1]]$content use this to view the content of the corpus file 
  
  mycorpus <- tm_map(mycorpus, stripWhitespace)
  mycorpus <- tm_map(mycorpus, removePunctuation) #tm_map(mycorpus, removePunctuation)
  mycorpus <- tm_map(mycorpus, content_transformer(tolower))
  mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
  
  mycorpus <- tm_map(mycorpus, stemDocument) #remove extraneous endings (like ly, ing, extra s)
  #save(mycorpus, file = "mycorpusEmily.Rda")
  
  freq_plot<-eventReactive(input$start,{
    
    ds1<-switch(input$select,
                "Nature" = NatureCorpus,
                "Religion" = ReligionCorpus,
                "Love" = LoveCorpus,
                "Grief" = GriefCorpus,
                "War" = WarCorpus,
                "Fame" = FameCorpus) 
    
    commonWords <- read.table("https://raw.githubusercontent.com/first20hours/google-10000-english/master/google-10000-english-usa-no-swears-short.txt")
    dataCW <- as.data.frame(commonWords)
    ds1 <- subset(ds1, !(ds1 %in% dataCW$V1))
    wordmatrix1<-as.data.frame(as.matrix(DocumentTermMatrix(mycorpus, list(dictionary = ds1))))
    
    ColSums <- colSums(wordmatrix1)
    NatureSort <- sort(ColSums, decreasing = TRUE)
    matrixD <- as.data.frame(NatureSort)
    nam <- rownames(matrixD) #retrieves all the words 
    datfin <- cbind(nam, matrixD)
    topData <- head(datfin, input$slider1)
    
    gp1<-ggplot(data=topData, aes(x=reorder(nam, -NatureSort), y=NatureSort)) +
      geom_bar(stat="identity") +
      ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) + 
      xlab(paste(c("Top", input$slider1, "words"), collapse = " "))
    gp1
    
  })
  
  ######################Sentiment Emotion Analysis
  # 
  # freq_plot2 <- eventReactive(input$start2,{
  #   #wordmatrix2<-as.data.frame(as.matrix(DocumentTermMatrix(mycorpus)))
  #   #ColSums2 <- colSums(wordmatrix2)
  #   #NatureSort2 <- sort(ColSums2, decreasing = TRUE)
  #   #matrixD2 <- as.data.frame(NatureSort2)
  #   #nam2 <- rownames(matrixD2) #retrieves all the words 
  #   #datfin2 <- cbind(nam2, matrixD2) #about 4191 observations 
  #   #save(datfin2, file = "sentimentPlot.Rda")
  #   sadness <- subset(sentiments, sentiment == "sadness")
  #   join <- semi_join(datfin, sadness, by = c("nam" = "word")) #166 words with sentiment of sad
  #   update <- join %>% arrange(desc(NatureSort))
  #   top <- head(update, input$slider3)
  #   #head(update, 10)
  #   #return(top)
  #   #return(top)
  #   #top <- head(update, 10)
  #   
  #   gp2<-ggplot(data=top, aes(x=reorder(nam, -NatureSort), y=NatureSort)) +
  #     geom_bar(stat="identity", fill = "#FFFF66")
  #   gp2
  # })
  
  ################  Word Cloud ###############  
  corpus=Corpus(VectorSource(mycorpus))
  corpus=tm_map(corpus,tolower)
  corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))
  corpus=tm_map(corpus,PlainTextDocument)
  col=brewer.pal(6,"Dark2")
  
  wm<-eventReactive(input$pickpoem,{
    ds<-switch(input$select,
               "Nature" = NatureCorpus,
               "Religion" = ReligionCorpus,
               "Love" = LoveCorpus,
               "Grief" = GriefCorpus,
               "War" = WarCorpus,
               "Fame" = FameCorpus) 
    
    ds1 <- LoveCorpus
    wordmatrix<-as.data.frame(as.matrix(DocumentTermMatrix(mycorpus, list(dictionary = ds1))))
    files <- list.files(location)  
    n <- length(files)   
    
    word_freq<-list()  
    for(i in 1:n){
      word_freq[i]<-sum(wordmatrix[i,])
    }
    
    wordfreq_summ<-as.data.frame(cbind(files,word_freq)) 
    
    wordfreq_summ<-wordfreq_summ %>% 
      mutate(word_freq=as.numeric(word_freq)) %>%
      arrange(desc(word_freq)) 
    
    ordered_raw<-unlist(strsplit(as.character(wordfreq_summ$files),"xt"))
    ordered_id<-t(matrix(ordered_raw, ncol=n)) [,2]
    ordered_id<-as.numeric(ordered_id)
    ordered_freq<-wordfreq_summ[,2]
    ordered_list<-as.data.frame(cbind(ordered_id,ordered_freq))
    
    randomp<-sample(ordered_list[c(1:input$slider2),1],1) #pick a poem that has the most words related to our theme
    
    lines <- readLines(paste(location, "/", files[randomp], sep="")) #displays poem
    for (i in 1:length(lines)) {
      cat(paste(lines[i], "\n"))
    }
    
    poem_freq<-filter(ordered_list,ordered_id==randomp)["ordered_freq"][1,]
    cat(paste("Poem ID: ", randomp, "      ", "Number of ", input$select, " words:  ",poem_freq))
    
  })  
  output$displaypoem <- renderPrint({
    wm()
  })
  
  output$plot <- renderPlot({
    freq_plot()
  })
  
  output$plot2 <- renderPlot({
    corpus=Corpus(VectorSource(mycorpus))
    corpus=tm_map(corpus,tolower)
    corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))
    corpus=tm_map(corpus,PlainTextDocument)
    corpus <- Corpus(VectorSource(corpus))
    col=brewer.pal(6,"Dark2")
    
    wordcloud(corpus, min.freq=input$freq, scale=c(3,2), rot.per = 0,
              random.color=T, max.words=input$max, random.order=F,colors=col)
  })
  
  #output$view <- renderDataTable({
  # freq_plot2()
  #})
  
  # output$plotSent <- renderPlot({
  #   freq_plot2()
  # })
  
})

# Run the application 
shinyApp(ui = ui, server = server)  
