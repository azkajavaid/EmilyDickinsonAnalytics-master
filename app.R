library(shiny)
require(mosaic)
library(plyr)
library(dplyr)
require(tidyr)
require(rvest)
require(wordcloud)
require(tm)
require(stringr)
require(ggplot2)
library(SnowballC)
library(tidytext)
library(reshape2)
library(visNetwork)
library(igraph)
library(ggraph)
library(data.table)
library(lubridate)

load("join21.Rda")
load("bigramCounts.Rda")
load("NatureCorpus.Rda")
load("ReligionCorpus.Rda")
load("GriefCorpus.Rda")
load("WarCorpus.Rda")
load("LoveCorpus.Rda")
load("FameCorpus.Rda")
load("sentiments1.Rda")
load("sentimentPlot.Rda")
load("gutenberg.Rda")


ui <- navbarPage(
  "Emily Dickinson's Poetry",

  tabPanel(
    "Welcome",
    img(src = "emily.png", align = "left", style = "margin:10px 10px"),

    h4(p("Emily Dickinson's Poetry application explores Emily's work from the Project Gutenberg.
                                    This App outlines semantic and thematic trends in her work by performing natural language processing (NLP) and
                                     text mining techniques like bigram and sentiment analysis.")),
    HTML("<br/>"),

    hr(),

    HTML("<br/>"),
    h4(p(
      strong("Word Cloud"), "visualizes
                                     Emily's corpus by frequency.", strong("Sentiment Analysis"), "explores anger, anticipation, 
                               disgust, fear, joy, sadness, surprise and trust sentiments in Emily's work.", strong("Positive-Negative Analysis"),
      "displays specified number of positive and negative words in bar plot and word cloud formats.", strong("Bigram Network"),
      "analyzes relationships between pairs of words in Emily's work in both dynamic and static plots.", strong("Thematic Analysis"),
      "explores themes of Nature, Religion, Love, Grief, War and Fame in Emily's work. 
                               This feature also displays an Emily's poem fitting the specified theme."
    )),

    HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
    uiOutput("urlLink")
  ),

  tabPanel(
    "Word Cloud",
    sidebarLayout(
      sidebarPanel(
        strong("Specify the minimum frequency of words to
                                     display (words with at least the specified frequency are shown)
                                     and/or specify the maximum number of words to print."),
        hr(),

        sliderInput(
          "freq",
          "Minimum Frequency:",
          min = 1, max = 50, value = 15
        ),
        sliderInput(
          "max",
          "Maximum Number of Words:",
          min = 1, max = 300, value = 100
        )
      ),
      mainPanel(
        plotOutput("plot2", height = "900px")
      )
    )
  ),

  tabPanel(
    "Sentiment Analysis",

    tags$head(
      tags$style(HTML("
                                            body {
                                            background-color: #BF4D6A;
                                            color: #000000; 
                                            }
                                            "))
    ),

    sidebarLayout(
      sidebarPanel(
        strong("Choose a sentiment and number of associated words to display from Emily Dickinson's poetry anthology"),
        HTML("<br><br>"),
        radioButtons(
          "variable", "Select sentiments:",
          c(
            "Anger" = "anger",
            "Anticipation" = "anticipation",
            "Disgust" = "disgust",
            "Fear" = "fear",
            "Joy" = "joy",
            "Sadness" = "sadness",
            "Surprise" = "surprise",
            "Trust" = "trust"
          )
        ),
        fluidRow(
          column(4, verbatimTextOutput("value2")),
          sliderInput(
            "slider4", label = NULL, min = 0,
            max = 25, value = 10
          ),
          actionButton("start3", "Show Plot", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;")
        )
      ),
      mainPanel(
        plotOutput("plotSecond")
      )
    )
  ),

  tabPanel(
    "Positive-Negative Analysis",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(3, verbatimTextOutput("value")),
          sliderInput(
            "slider3", label = h5("Specify number of positive/negative word display"), min = 0,
            max = 40, value = 10
          ),
          actionButton("start2", "Show plot", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;"),
          div(style = "display:inline-block", actionButton("start4", "Show cloud", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;"))
        )
      ),
      mainPanel(
        plotOutput("plotSent"),
        plotOutput("plotCloud")
      )
    )
  ),

  tabPanel(
    "Bigram Network",
    sidebarLayout(
      sidebarPanel(
        strong("Choose to display either a dynamic or static word plot of bigram relationships in Emily Dickinson's poetry.
                                     All bigrams with at least one occurrence are shown. Zoom in the dynamic cloud to observe bigrams 
                                     or select a word from the id drop down"),
        HTML("<br><br>"),
        actionButton("start5", "Show dynamic bigram", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;"),
        div(style = "display:inline-block", actionButton("start6", "Show static bigram", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;"))
      ),
      mainPanel(
        visNetworkOutput("network", height = "700px", width = "700px"),
        plotOutput("plotCloud4", height = "700px", width = "700px")
      )
    )
  ),

  tabPanel(
    "Thematic Analysis",
    sidebarLayout(
      sidebarPanel(
        strong("Specify a theme to display the frequency of synonyms and antonyms in that theme found in Emily's work and/or
                                       generate a random Emily poem fitting that thematic category."),

        hr(),
        selectInput(
          "select", label = h5("Select a Theme"),
          choices = list(
            "Nature", "Religion",
            "Love", "Grief", "War", "Fame"
          ),
          selected = "Nature"
        ),
        fluidRow(
          sliderInput(
            "slider1", label = h5("How many words?"), min = 0,
            max = 20, value = 10
          ),
          actionButton("start", "Show frequency", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;"),
          sliderInput(
            "slider2", label = h5("How many Poems?"), min = 0,
            max = 20, value = 5
          ),
          actionButton("pickpoem", "Pick a poem", style = "color: #ffffff;background-color: #BF4D6A;margin: 4px;")
        )
      )
      ,
      mainPanel(
        plotOutput("plot"),
        fluidRow(
          column(
            9,
            verbatimTextOutput("displaypoem")
          ),
          column(
            2,
            dataTableOutput("poemtable")
          )
        )
      )
    )
  ),
  tabPanel(
    "References",
    verbatimTextOutput("Reference")
  )
)


server <- function(input, output) {
  url <- a("teenreads", href = "https://www.teenreads.com/features/side-by-side/side-by-side-emily-dickinson")

  output$urlLink <- renderUI({
    tagList("Image courtesy of:", url)
  })

  freq_plot2 <- eventReactive(input$start2, {
    join3 <- filter(join2, sentiment %in% c("positive", "negative"))
    join4 <- join3 %>% mutate(score = ifelse(sentiment == "negative", -1 * NatureSort, NatureSort))
    joinNRC <- join4

    joinNRC %>%
      top_n(input$slider3, abs(score)) %>%
      mutate(word = reorder(nam, score)) %>%
      ggplot(aes(word, score, fill = score > 0)) +
      geom_bar(stat = "identity", show.legend = FALSE) + theme_bw() + scale_fill_manual(values = c("#98CF82", "#D77D67")) +
      coord_flip()
  })


  sentiment <- eventReactive(input$start3, {
    join3 <- filter(join2, sentiment %in% c(
      "anticipation", "joy", "anger", "fear",
      "sadness", "trust", "surprise", "disgust"
    ))
    join4 <- subset(join3, sentiment == c(input$variable))
    join4 <- subset(join4, lexicon == "nrc")

    tru <- head(join4, input$slider4)
    tru <- plyr::rename(tru, c("NatureSort" = "WordCount"))

    if (input$variable == "anticipation") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#E39B1B")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else if (input$variable == "joy") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#98DAC1")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else if (input$variable == "anger") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#730A20")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else if (input$variable == "fear") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#9B192B")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else if (input$variable == "sadness") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#8185F6")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else if (input$variable == "trust") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#F5C255")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else if (input$variable == "surprise") {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#F9686E")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }

    else {
      gg <- ggplot(data = tru, aes(x = reorder(nam, -WordCount), y = WordCount, fill = sentiment)) +
        geom_bar(stat = "identity") + scale_fill_manual(values = ("#58A46B")) + theme_bw() + xlab("") +
        facet_wrap(~sentiment, ncol = 2, scales = "free_x")
      return(gg)
    }
  })

  sentCloud <- eventReactive(input$start4, {
    join3 <- filter(join2, sentiment %in% c("positive", "negative"))
    join3 %>%
      arrange(desc(NatureSort)) %>%
      acast(nam ~ sentiment, value.var = "NatureSort", fill = 0) %>%
      comparison.cloud(
        colors = c("#98CF82", "#D77D67"),
        max.words = input$slider3
      )
  })


  df_subsetVis <- eventReactive(input$start5, {
    bigram_graph <- bigram_counts %>%
      filter(n > 1) %>%
      graph_from_data_frame()
    return(bigram_graph)
  })

  df_subsetVis2 <- eventReactive(input$start6, {
    bigram_graph <- bigram_counts %>%
      filter(n > 1) %>%
      graph_from_data_frame()
    return(bigram_graph)
  })

  output$network <- renderVisNetwork({
    dat <- df_subsetVis()
    visIgraph(dat, idToLabel = TRUE) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })

  output$plotCloud4 <- renderPlot({
    dat <- df_subsetVis2()
    ggraph(dat, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = NULL) +
      geom_node_point(color = "#7E9ADD", size = 6) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  })


  output$plotSent <- renderPlot({
    freq_plot2()
  })
  output$plotSecond <- renderPlot({
    sentiment()
  })
  output$plotCloud <- renderPlot({
    sentCloud()
  })

  location <- "gutenberg"
  mycorpus <- VCorpus(DirSource(location))

  mycorpus <- tm_map(mycorpus, stripWhitespace)
  mycorpus <- tm_map(mycorpus, removePunctuation)
  mycorpus <- tm_map(mycorpus, content_transformer(tolower))
  mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))

  mycorpus <- tm_map(mycorpus, stemDocument)

  freq_plot <- eventReactive(input$start, {
    withProgress(message = "Loading application", value = 0, {
      ds1 <- switch(input$select,
        "Nature" = NatureCorpus,
        "Religion" = ReligionCorpus,
        "Love" = LoveCorpus,
        "Grief" = GriefCorpus,
        "War" = WarCorpus,
        "Fame" = FameCorpus
      )

      incProgress(0.3, detail = "Finding common words")
      commonWords <- read.table("https://raw.githubusercontent.com/ajav17/google-10000-english/master/google-10000-english-usa-no-swears-short.txt")
      dataCW <- as.data.frame(commonWords)
      ds1 <- subset(ds1, !(ds1 %in% dataCW$V1))
      wordmatrix1 <- as.data.frame(as.matrix(DocumentTermMatrix(mycorpus, list(dictionary = ds1))))

      ColSums <- colSums(wordmatrix1)
      NatureSort <- sort(ColSums, decreasing = TRUE)
      matrixD <- as.data.frame(NatureSort)
      nam <- rownames(matrixD)
      datfin <- cbind(nam, matrixD)
      topData <- head(datfin, input$slider1)

      incProgress(0.5, detail = "Plotting")

      if (input$select == "Nature") {
        gp1 <- ggplot(data = topData, aes(x = reorder(nam, -NatureSort), y = NatureSort)) +
          geom_bar(stat = "identity", fill = "#66B533") +
          ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) +
          xlab(paste(c("Top", input$slider1, "words"), collapse = " ")) + theme_bw()
        return(gp1)
      }

      else if (input$select == "Religion") {
        gp1 <- ggplot(data = topData, aes(x = reorder(nam, -NatureSort), y = NatureSort)) +
          geom_bar(stat = "identity", fill = "#3F5ABC") +
          ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) +
          xlab(paste(c("Top", input$slider1, "words"), collapse = " ")) + theme_bw()
        return(gp1)
      }

      else if (input$select == "Love") {
        gp1 <- ggplot(data = topData, aes(x = reorder(nam, -NatureSort), y = NatureSort)) +
          geom_bar(stat = "identity", fill = "red") +
          ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) +
          xlab(paste(c("Top", input$slider1, "words"), collapse = " ")) + theme_bw()
        return(gp1)
      }

      else if (input$select == "Grief") {
        gp1 <- ggplot(data = topData, aes(x = reorder(nam, -NatureSort), y = NatureSort)) +
          geom_bar(stat = "identity", fill = "#318A84") +
          ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) +
          xlab(paste(c("Top", input$slider1, "words"), collapse = " ")) + theme_bw()
        return(gp1)
      }

      else if (input$select == "War") {
        gp1 <- ggplot(data = topData, aes(x = reorder(nam, -NatureSort), y = NatureSort)) +
          geom_bar(stat = "identity", fill = "#730A20") +
          ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) +
          xlab(paste(c("Top", input$slider1, "words"), collapse = " ")) + theme_bw()
        return(gp1)
      }

      else {
        gp1 <- ggplot(data = topData, aes(x = reorder(nam, -NatureSort), y = NatureSort)) +
          geom_bar(stat = "identity", fill = "#E1701D") +
          ylab((paste(c("Frequency of ", input$select, "Words"), collapse = " "))) +
          xlab(paste(c("Top", input$slider1, "words"), collapse = " ")) + theme_bw()
        return(gp1)
      }
      incProgress(0.2, detail = "Finishing...")
    })
  })


  location <- "gutenberg"
  mycorpus <- VCorpus(DirSource(location))
  corpus <- Corpus(VectorSource(mycorpus))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, PlainTextDocument)

  wm <- eventReactive(input$pickpoem, {
    ds1 <- switch(input$select,
      "Nature" = NatureCorpus,
      "Religion" = ReligionCorpus,
      "Love" = LoveCorpus,
      "Grief" = GriefCorpus,
      "War" = WarCorpus,
      "Fame" = FameCorpus
    )

    wordmatrix <- as.data.frame(as.matrix(DocumentTermMatrix(mycorpus, list(dictionary = ds1))))
    word_impute <- data.frame(t(wordmatrix))

    location <- "gutenberg"
    files <- list.files(location)
    n <- length(files)

    word_freq <- list()

    for (i in 1:n) {
      word_freq[i] <- sum(wordmatrix[i, ])
    }

    wordfreq_summ <- as.data.frame(cbind(files, word_freq))

    wordfreq_summ <- wordfreq_summ %>%
      mutate(word_freq = as.numeric(word_freq)) %>%
      arrange(desc(word_freq))

    ordered_raw <- unlist(strsplit(as.character(wordfreq_summ$files), "xt"))

    ordered_id <- t(matrix(ordered_raw, ncol = n)) [, 2]
    ordered_id <- as.numeric(ordered_id)
    ordered_freq <- wordfreq_summ[, 2]
    ordered_list <- as.data.frame(cbind(ordered_id, ordered_freq))

    randomp <- sample(ordered_list[c(1:input$slider2), 1], 1)
    combo <- list(randomp = randomp, ordered_list = ordered_list, word_impute = word_impute)
    return(combo)
  })


  output$poemtable <- renderDataTable({
    withProgress(message = "Loading application", value = 0, {
      combo <- wm()
      randomp <- combo$randomp
      ordered_list <- combo$ordered_list
      word_impute <- combo$word_impute
      incProgress(0.6, detail = "Preparing Corpus")

      if (nchar(randomp) == 1) {
        randomp1 <- as.character(paste("00", randomp, sep = ""))
      }
      else if (nchar(randomp) == 2) {
        randomp1 <- as.character(paste("0", randomp, sep = ""))
      }
      else {
        randomp1 <- randomp
      }

      incProgress(0.3, detail = "Printing poem")
      column <- paste("gutenberg3.txt", randomp1, sep = "")
      word_data <- setDT(data.frame(word_impute), keep.rownames = TRUE)[]

      column_data <- word_data %>% dplyr::select(rn, column)

      x <- names(column_data)[2]
      filter_words <- column_data %>% dplyr::filter_(paste(x, ">", 0))

      incProgress(0.1, detail = "Finishing...")
      colnames(filter_words) <- c("word", "count")

      filter_words <- data.frame(filter_words)
      return(filter_words)
    })
  })


  output$displaypoem <- renderPrint({
    withProgress(message = "Loading application", value = 0, {
      incProgress(0.2, detail = "Processing")
      location <- "gutenberg"
      files <- list.files(location)
      combo <- wm()
      randomp <- combo$randomp
      ordered_list <- combo$ordered_list
      incProgress(0.5, detail = "Printing")
      lines <- readLines(paste(location, "/", files[randomp], sep = ""))

      for (i in 1:length(lines)) {
        cat(paste(lines[i], "\n"))
      }

      poem_freq <- filter(ordered_list, ordered_id == randomp)["ordered_freq"][1, ]
      incProgress(0.3, detail = "Finishing...")
      cat(paste("Poem ID: ", randomp, "      ", "Number of ", input$select, " words: ", poem_freq))
    })
  })

  output$plot <- renderPlot({
    freq_plot()
  })

  output$plot2 <- renderPlot({
    withProgress(message = "Loading application", value = 0, {
      incProgress(0.7, detail = "Making word cloud")
      corpus <- Corpus(VectorSource(mycorpus))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, function(x) removeWords(x, stopwords()))
      corpus <- tm_map(corpus, PlainTextDocument)
      corpus <- Corpus(VectorSource(corpus))
      pal <- brewer.pal(9, "RdPu")
      pal <- pal[-(1:4)]
      incProgress(0.3, detail = "Finishing...")
      wordcloud(
        corpus, min.freq = input$freq, scale = c(3, 2), rot.per = 0,
        max.words = input$max, random.order = F, colors = pal
      )
    })
  })
  output$Reference <- renderPrint({
    sessionInfo()
  })
}

shinyApp(ui = ui, server = server)