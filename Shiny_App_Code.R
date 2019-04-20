# Read in libraries
library(shiny)
library(tm)
library(shinydashboard)
library(text2vec)
library(wordcloud)
library(htmltools)

# Read in Data
cols <- c('user_id', 'Date', 'Day', 'screen_name', 'text', 'stripped_text', 'location', 'account_lang', 'terms')
tweets_data <- read.csv("DATA_4_SHINY.csv")[,cols]

# Segement data
tweets_data_day_before <- tweets_data[tweets_data['Day'] == 2,]
tweets_data_during <- tweets_data[tweets_data['Day'] == 3,]
tweets_data_next_day <- tweets_data[tweets_data['Day'] == 4,]
tweets_data_week_after <- tweets_data[tweets_data['Day'] == 10,]

#### Tokenise and parse text

# Day before SuperBowl
day_before_tokens <- word_tokenizer(tweets_data_day_before$stripped_text)

stop_words <- stopwords("english")
add_words <- c('commercial','commercials', 'drink', 'drinking', 'anheuser-busch', 'budweiser','budweisers','budwiser','budlight','bud light','stella artois','bon & viv','bon and viv','bon + viv','bon+viv','bon&viv','bonandviv','michelob ultra','michelob','yellow tail','yellow','tail','stella artois','stella','artois','bud','just','uf','im', 'superbowl', 'super', 'bowl', 'beer','ad')

all_stop_words <- union(stop_words, add_words)

it_day_before <- itoken(day_before_tokens)
bi_it_day_before <- itoken(day_before_tokens)
tri_it_day_before <- itoken(day_before_tokens)

vocab_day_before <- create_vocabulary(it_day_before,  stopwords = all_stop_words) 

bi_vocab_day_before <- create_vocabulary(bi_it_day_before,  stopwords = all_stop_words, ngram = c(ngram_min=2, ngram_max=2), sep_ngram = " ")

tri_vocab_day_before <- create_vocabulary(tri_it_day_before,  stopwords = all_stop_words, ngram = c(ngram_min=3, ngram_max=3), sep_ngram = " ")

# Day after SuperBowl
day_after_tokens <- word_tokenizer(tweets_data_next_day$stripped_text)

it_day_after <- itoken(day_after_tokens)
bi_it_day_after <- itoken(day_after_tokens)
tri_it_day_after <- itoken(day_after_tokens)

vocab_day_after <- create_vocabulary(it_day_after,  stopwords = all_stop_words) 

bi_vocab_day_after <- create_vocabulary(bi_it_day_after,  stopwords = all_stop_words, ngram = c(ngram_min=2, ngram_max=2), sep_ngram = " ")

tri_vocab_day_after <- create_vocabulary(tri_it_day_after,  stopwords = all_stop_words, ngram = c(ngram_min=3, ngram_max=3), sep_ngram = " ")

# One week after SuperBowl
week_after_tokens <- word_tokenizer(tweets_data_week_after$stripped_text)

it_week_after <- itoken(week_after_tokens)
bi_it_week_after <- itoken(week_after_tokens)
tri_it_week_after <- itoken(week_after_tokens)

vocab_week_after <- create_vocabulary(it_week_after,  stopwords = all_stop_words) 

bi_vocab_week_after <- create_vocabulary(bi_it_week_after,  stopwords = all_stop_words, ngram = c(ngram_min=2, ngram_max=2), sep_ngram = " ")

tri_vocab_week_after <- create_vocabulary(tri_it_week_after,  stopwords = all_stop_words, ngram = c(ngram_min=3, ngram_max=3), sep_ngram = " ")


#### Define UI for application

# Using the dashboard layout
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("ReadMe", tabName = "ReadMe", icon = icon("book")),
            menuItem("WordCloud", tabName = "WordCloud", icon = icon("cloud")),
            menuItem("Bigram", tabName = "Bigram", icon = icon("cloud")),
            menuItem("Trigram", tabName = "Trigram", icon = icon("cloud"))
        )
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "ReadMe",
                    h2("About this Application"),
                    textOutput("readme")),
            
            tabItem(tabName = "WordCloud",
                    fluidRow(
                        box(title = "One Day before the Superbowl", plotOutput("wordcloud1", height = 500, width = 500)),
                        box(title = "One Day after the Superbowl", plotOutput("wordcloud2", height = 500, width = 500)),
                        box(title = "One Week after the Superbowl", plotOutput("wordcloud3", height = 500, width = 500)))),
            
            tabItem(tabName = "Bigram",
                    fluidRow(
                        box(title = "One Day before the Superbowl", plotOutput("bigram1", height = 500, width = 500)),
                        box(title = "One Day after the Superbowl", plotOutput("bigram2", height = 500, width = 500)),
                        box(title = "One Week after the Superbowl", plotOutput("bigram3", height = 500, width = 500)))),
            
            tabItem(tabName = "Trigram",
                    fluidRow(
                        box(title = "One Day before the Superbowl", plotOutput("trigram1", height = 500, width = 500)),
                        box(title = "One Day after the Superbowl", plotOutput("trigram2", height = 500, width = 500)),
                        box(title = "One Week after the Superbowl", plotOutput("trigram3", height = 500, width = 500))))
        )
    )
)

# Define server logic 
server <- function(input, output) {
    output$wordcloud1 <- renderPlot(
        wordcloud(vocab_day_before$term, vocab_day_before$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.05))
        
    )
    output$wordcloud2 <- renderPlot(  
        wordcloud(vocab_day_after$term, vocab_day_after$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.15))
        
    )
    output$wordcloud3 <- renderPlot(
        wordcloud(vocab_week_after$term, vocab_week_after$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.15))
    )  
    
    output$bigram1 <- renderPlot(
        wordcloud(bi_vocab_day_before$term, bi_vocab_day_before$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.05))
        
    )
    output$bigram2 <- renderPlot(
        wordcloud(bi_vocab_day_after$term, bi_vocab_day_after$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.25))
        
    )
    output$bigram3 <- renderPlot(
        wordcloud(bi_vocab_week_after$term, bi_vocab_week_after$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.15))
        
    )
    output$trigram1 <- renderPlot(
        wordcloud(tri_vocab_day_before$term, tri_vocab_day_before$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.05))
        
    )
    output$trigram2 <- renderPlot(
        wordcloud(tri_vocab_day_after$term, tri_vocab_day_after$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.15))
        
    )
    output$trigram3 <- renderPlot(
        wordcloud(tri_vocab_week_after$term, tri_vocab_week_after$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"), scale=c(3.5,0.15))
        
    )
    output$readme <- renderText(
        
        {
            
            HTML("This application uses data from Twitter to analyze beer alcohol commmercials shown before, immediately after, and one week after the Super Bowl. The data pertains only to companies that showed commercials during the Super Bowl. These companies include; Budweiser, Stella Artios, Bon & Viv, Michelob Ultra, Yellow Tail. Please enjoy the app!"
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
