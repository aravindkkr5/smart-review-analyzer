#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(rvest)
products <<- list("APPLE IPHONE 12 PRO MAX",
                "Samsung Galaxy Note 10" ,
                  "Xiaomi Redmi Note 9 Pro"
                  )
######################################################################################
ui<-fluidPage(
    titlePanel("SMART REVIEW ANALYZER"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selection", "Choose a product:",
                        choices = products),
            actionButton("update", "Change"),
            hr(),
            
            sliderInput("max",
                        "Maximum Number of Words:Wordcloud",
                        min = 1,  max = 300,  value = 200)
        ),
        mainPanel(
            textOutput("text1"),
            tabsetPanel(
                tabPanel("HISTOGRAM", plotOutput("hist")),
                tabPanel("WORDCLOUD", plotOutput("plot"))
            ) 
        )))

##########################################################################################

server <- function(input, output){
    
    crawl<-(function(forcasthtml1,forcasthtml2){
        
        
        forcast1<-html_text(forcasthtml1)
        forcast1
        
        if(missing(forcasthtml2)){
            forcast <- html_text(forcasthtml1)
            forcast
        }else{
            forcast2<-html_text(forcasthtml2)
            forcast2
            forcast<-rbind(forcast1,forcast2)
        }
        
        #install.packages("tm")
        #R package for text mining
        library(tm)
        #load your texts into R.
        
        doc.vec <- VectorSource(forcast)
        doc.corpus <- VCorpus(doc.vec)
        summary(doc.corpus)
        
        ##First we convert all of the text to lowercase
        doc.corpus <- tm_map(doc.corpus, tolower)
        
        ## remove punctuation
        doc.corpus <- tm_map(doc.corpus, removePunctuation)
        
        ##numbers
        doc.corpus <- tm_map(doc.corpus, removeNumbers)
        
        ## common English stopwords.
        doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
        
        # read your documents
        inspect(doc.corpus)
        ##stemming, which removes affixes from words.
        ##(so, for example, "run", "runs" and "running" all become "run").
        #install.packages("SnowballC")
        library(SnowballC)
        
        doc.corpus <- tm_map(doc.corpus, stemDocument) 
        
        doc.corpus <- tm_map(doc.corpus, stripWhitespace)
        inspect(doc.corpus)
        
        doc.corpus <- tm_map(doc.corpus, PlainTextDocument)
        dtm <- DocumentTermMatrix(doc.corpus)
        dtm2 <- as.matrix(dtm)
        frequency <- colSums(dtm2)
        frequency <- sort(frequency,decreasing = TRUE)
        head(frequency)
        
        #install.packages('wordcloud')
        words <- names(frequency)
        #   library(wordcloud)
        #   #wordcloud(words, frequency)
        pos <- scan("positive-words.txt",what="character",comment.char=";")
        neg <- scan("negative-words.txt",what="character",comment.char=";")
        source("sentiment.R")
        
        analysis <- score.sentiment(words,pos,neg)
        table(analysis$score)
        neutral <- length(which(analysis$score == 0))
        positive <- length(which(analysis$score > 0))
        negative <- length(which(analysis$score < 0))
        Sentiment <- c("Negative","Neutral","Positive")
        Count <- c(negative,neutral,positive)
        output <- as.data.frame(Sentiment,Count)

        library(ggplot2)
        #ggplot(data=analysis, aes(analysis$score)) + geom_histogram(col="black",binwidth=0.5,aes(fill=..count..)) +scale_fill_gradient("Count", low = "aquamarine", high = "aquamarine4") + xlab("Polarity of words") +ylab("Count of words")
         qplot(factor(score), data=analysis, geom="bar", fill=factor(score))+scale_fill_discrete(name = "Reviews", labels = c("Negative", "Neutral", "Positive"))+xlab("Polarity of words") + ylab("Count of words") + ggtitle("Review Analysis from Amazon and Flipkart")
        })
    
    output$text1 <- renderText({
        paste("Product: ", input$selection," - Amazon and Flipkart")
    })
    
    
    ##########################################################
    
    ######  HISTOGRAM  #########
    
    ##########################################################
    
    output$hist=renderPlot({
        
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")})
            
            ######################################################
            if(input$selection=="Samsung Galaxy Note 10")
            {
                
                amazonhtmlpage <- read_html("https://www.amazon.in/Samsung-Galaxy-Storage-Additional-Exchange/dp/B07PQ7DK2N/ref=sr_1_2?dchild=1&keywords=samsung+galaxy+note+10&qid=1610260272&sr=8-2")
                amazonforcasthtml <- html_nodes(amazonhtmlpage,".a-expander-partial-collapse-content span")
                flipkarthtmlpage <- read_html("https://www.flipkart.com/search?q=samsung+galaxy+m51&sid=tyy%2C4io&as=on&as-show=on&otracker=AS_QueryStore_OrganicAutoSuggest_1_18_na_na_na&otracker1=AS_QueryStore_OrganicAutoSuggest_1_18_na_na_na&as-pos=1&as-type=RECENT&suggestionId=samsung+galaxy+m51%7CMobiles&requestId=0b7b2cb9-8047-4871-ae2b-1ca8b17b3d77&as-searchtext=samsung%20galaxy%20m51")
                flipkartforcasthtml <- html_nodes(flipkarthtmlpage,".t-ZTKy div")
                crawl(amazonforcasthtml,flipkartforcasthtml)
                
            }
            else if(input$selection=="Xiaomi Redmi Note 9 Pro")
                
            {
                amazonhtmlpage <- read_html("https://www.amazon.in/Test-Exclusive-550/dp/B077Q7GW9V/ref=sr_1_1?dchild=1&keywords=redmi+note+9+pro&qid=1609856185&sr=8-1")
                amazonforcasthtml <- html_nodes(amazonhtmlpage,".a-expander-partial-collapse-content span")
                flipkarthtmlpage <- read_html("https://www.flipkart.com/redmi-note-9-pro-interstellar-black-128-gb/p/itm0418537d115ba?pid=MOBFUZYNSYP64TQK&lid=LSTMOBFUZYNSYP64TQKRIXIXF&marketplace=FLIPKART&srno=s_1_1&otracker=search&otracker1=search&fm=SEARCH&iid=ec3cb7f6-208c-40e3-8303-66c3f564b791.MOBFUZYNSYP64TQK.SEARCH&ppt=sp&ppn=sp&ssid=y441qhmugw0000001609856175688&qH=2eeee788c4b5011c")
                flipkartforcasthtml <- html_nodes(flipkarthtmlpage,".t-ZTKy div")
                crawl(amazonforcasthtml,flipkartforcasthtml)
            }
            else if(input$selection=="APPLE IPHONE 12 PRO MAX")
            {
                amazonhtmlpage <- read_html("https://www.amazon.in/Apple-iPhone-Pro-Max-128GB/dp/B08L5WS5JB/ref=sr_1_1_sspa?dchild=1&keywords=Apple+iPhone+12+Pro+Max&qid=1609856280&sr=8-1-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUEzRzFYS1U5SzVHVzY0JmVuY3J5cHRlZElkPUEwMjEwNTE4STdZSkNURUI1Sk4xJmVuY3J5cHRlZEFkSWQ9QTAyNjc3OTMySDA1VEtPRTBPS1JGJndpZGdldE5hbWU9c3BfYXRmJmFjdGlvbj1jbGlja1JlZGlyZWN0JmRvTm90TG9nQ2xpY2s9dHJ1ZQ==")
                amazonforcasthtml <- html_nodes(amazonhtmlpage,".a-expander-partial-collapse-content span")
                flipkarthtmlpage <- read_html("https://www.flipkart.com/apple-iphone-12-pro-max-gold-256-gb/p/itm1e0354f5bbc8c?pid=MOBFWBYZZPW8JHQF&lid=LSTMOBFWBYZZPW8JHQF9DJLT1&marketplace=FLIPKART&srno=s_1_1&otracker=AS_Query_HistoryAutoSuggest_1_23_na_na_na&otracker1=AS_Query_HistoryAutoSuggest_1_23_na_na_na&fm=SEARCH&iid=125c007f-bcb9-4c58-8e3f-7ecac519a4a8.MOBFWBYZZPW8JHQF.SEARCH&ppt=sp&ppn=sp&ssid=y7vj4tw6v40000001609856069315&qH=d6aaa1cff1d16f4a")
                flipkartforcasthtml <- html_nodes(flipkarthtmlpage,".t-ZTKy div")
                crawl(amazonforcasthtml,flipkartforcasthtml)
            }
            
            
        })
    })
    ##########################################################
    
    ######  WORDCLOUD  #########
    
    ##########################################################
    
    wcloud<-(function(forcasthtml1,forcasthtml2){
        
        forcast1<-html_text(forcasthtml1)
        forcast1
        
        if(missing(forcasthtml2)){
            forcast <- html_text(forcasthtml1)
            forcast
        }else{
            forcast2<-html_text(forcasthtml2)
            forcast2
            forcast<-rbind(forcast1,forcast2)
            
        }
        
        #install.packages("tm")
        #R package for text mining
        library(tm)
        #load your texts into R.
        
        #Corpus is a list of documents.
        #VectorSource() function creates a corpus of character vectors.
        
        ###  docs <- Corpus(DirSource(cname))
        doc.vec <- VectorSource(forcast)
        doc.corpus <- VCorpus(doc.vec)
        summary(doc.corpus)
        
        ##First we convert all of the text to lowercase
        doc.corpus <- tm_map(doc.corpus, tolower)
        
        ## remove punctuation
        doc.corpus <- tm_map(doc.corpus, removePunctuation)
        
        ##numbers
        doc.corpus <- tm_map(doc.corpus, removeNumbers)
        
        ## common English stopwords.
        doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
        
        # Remove your own stop word
        # specify your stopwords as a character vector
        doc.corpus <- tm_map(doc.corpus, removeWords, c("phone","phones", "samsung","mobile","galaxy","ultra","xiaomi","note","pro","iphone","max",'will','can','isnt','lets','due','even','also'))
        
        # read your documents
        inspect(doc.corpus)
        
        ##stemming, which removes affixes from words.
        ##(so, for example, "run", "runs" and "running" all become "run").
        #install.packages("SnowballC")
        library(SnowballC)
        
        # doc.corpus <- tm_map(doc.corpus, stemDocument)
        doc.corpus <- tm_map(doc.corpus, stripWhitespace)
        inspect(doc.corpus)
        ####writeLines(as.character(doc.corpus))
        #parseddata = paste(doc.corpus, collapse = " ")
        #parseddata
        doc.corpus <- tm_map(doc.corpus, PlainTextDocument)
        dtm <- DocumentTermMatrix(doc.corpus)
        dtm2 <- as.matrix(dtm)
        frequency <- colSums(dtm2)
        frequency <- sort(frequency,decreasing = TRUE)
        head(frequency)
        
        #install.packages('wordcloud')
        words <- names(frequency)
        library(wordcloud)
        #wordcloud(words, frequency)
        d <- data.frame(word = names(frequency),freq=frequency)
        head(d, 10)
        
        #####The importance of words can be illustrated as a word cloud as follow :
        set.seed(1234)
        wordcloud(words = d$word, freq = d$freq, min.freq = 2,
                  max.words=input$max, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))
        
        
        
    })
    
    output$plot <- renderPlot({
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")})
            #################################################################
            if(input$selection=="Samsung Galaxy Note 10")
            {
                amazonhtmlpage <- read_html("https://www.amazon.in/Samsung-Galaxy-Storage-Additional-Exchange/dp/B07PQ7DK2N/ref=sr_1_2?dchild=1&keywords=samsung+galaxy+note+10&qid=1610260272&sr=8-2")
                amazonforcasthtml <- html_nodes(amazonhtmlpage,".a-expander-partial-collapse-content span")
                flipkarthtmlpage <- read_html("https://www.flipkart.com/search?q=samsung+galaxy+m51&sid=tyy%2C4io&as=on&as-show=on&otracker=AS_QueryStore_OrganicAutoSuggest_1_18_na_na_na&otracker1=AS_QueryStore_OrganicAutoSuggest_1_18_na_na_na&as-pos=1&as-type=RECENT&suggestionId=samsung+galaxy+m51%7CMobiles&requestId=0b7b2cb9-8047-4871-ae2b-1ca8b17b3d77&as-searchtext=samsung%20galaxy%20m51")
                flipkartforcasthtml <- html_nodes(flipkarthtmlpage,".t-ZTKy div")
                wcloud(amazonforcasthtml,flipkartforcasthtml)
                
            }else if(input$selection=="Xiaomi Redmi Note 9 Pro")
                
            {
                amazonhtmlpage <- read_html("https://www.amazon.in/Test-Exclusive-550/dp/B077Q7GW9V/ref=sr_1_1?dchild=1&keywords=redmi+note+9+pro&qid=1609856185&sr=8-1")
                amazonforcasthtml <- html_nodes(amazonhtmlpage,".a-expander-partial-collapse-content span")
                flipkarthtmlpage <- read_html("https://www.flipkart.com/redmi-note-9-pro-interstellar-black-128-gb/p/itm0418537d115ba?pid=MOBFUZYNSYP64TQK&lid=LSTMOBFUZYNSYP64TQKRIXIXF&marketplace=FLIPKART&srno=s_1_1&otracker=search&otracker1=search&fm=SEARCH&iid=ec3cb7f6-208c-40e3-8303-66c3f564b791.MOBFUZYNSYP64TQK.SEARCH&ppt=sp&ppn=sp&ssid=y441qhmugw0000001609856175688&qH=2eeee788c4b5011c")
                flipkartforcasthtml <- html_nodes(flipkarthtmlpage,".t-ZTKy div")
                wcloud(amazonforcasthtml,flipkartforcasthtml)
                
            }else if(input$selection=="APPLE IPHONE 12 PRO MAX")
            {
                amazonhtmlpage <- read_html("https://www.amazon.in/Apple-iPhone-Pro-Max-128GB/dp/B08L5WS5JB/ref=sr_1_1_sspa?dchild=1&keywords=Apple+iPhone+12+Pro+Max&qid=1609856280&sr=8-1-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUEzRzFYS1U5SzVHVzY0JmVuY3J5cHRlZElkPUEwMjEwNTE4STdZSkNURUI1Sk4xJmVuY3J5cHRlZEFkSWQ9QTAyNjc3OTMySDA1VEtPRTBPS1JGJndpZGdldE5hbWU9c3BfYXRmJmFjdGlvbj1jbGlja1JlZGlyZWN0JmRvTm90TG9nQ2xpY2s9dHJ1ZQ==")
                amazonforcasthtml <- html_nodes(amazonhtmlpage,".a-expander-partial-collapse-content span")
                flipkarthtmlpage <- read_html("https://www.flipkart.com/apple-iphone-12-pro-max-gold-256-gb/p/itm1e0354f5bbc8c?pid=MOBFWBYZZPW8JHQF&lid=LSTMOBFWBYZZPW8JHQF9DJLT1&marketplace=FLIPKART&srno=s_1_1&otracker=AS_Query_HistoryAutoSuggest_1_23_na_na_na&otracker1=AS_Query_HistoryAutoSuggest_1_23_na_na_na&fm=SEARCH&iid=125c007f-bcb9-4c58-8e3f-7ecac519a4a8.MOBFWBYZZPW8JHQF.SEARCH&ppt=sp&ppn=sp&ssid=y7vj4tw6v40000001609856069315&qH=d6aaa1cff1d16f4a")
                flipkartforcasthtml <- html_nodes(flipkarthtmlpage,".t-ZTKy div")
                wcloud(amazonforcasthtml,flipkartforcasthtml)
                
            }
            
        })
    })
    outputOptions(output,"plot",suspendWhenHidden = FALSE)
}

shinyApp(ui=ui, server=server)
