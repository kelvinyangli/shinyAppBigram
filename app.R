#

# This is a Shiny web application. You can run the application by clicking

# the 'Run App' button above.

#

# Find out more about building applications with Shiny here:

#

#    http://shiny.rstudio.com/

#

library(shiny)
source("library.R")


ui <- fluidPage(

  sidebarLayout(

    sidebarPanel(

      radioButtons(inputId = 'inputformat',

                   label = 'File type',

                   choices = c('Excel' = 'excel', 'CSV' = 'csv')),



      fileInput("file1", "Choose file",

                accept = c(

                  "text/csv",

                  "text/comma-separated-values,text/plain",

                  ".csv",

                  ".xlsx")

      ),

      tags$hr(),

      sliderInput("topN", "Top n bigrams:",

                  min = 1, max = 30, value = 15),

      # checkboxInput("header", "Header", TRUE),

      fluidRow(

        column(4, actionButton("draw", "Make plot", icon("paper-plane"),

                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

      )

    ),

    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----

      tabsetPanel(type = "tabs",

                  tabPanel("Summary", verbatimTextOutput("summary"),

                           fluidRow(

                             column(8, verbatimTextOutput("count")),

                             tags$hr(),

                             column(8, verbatimTextOutput("NPSGroupsCount"))

                           )),

                  # tabPanel("NPS Groups Count", verbatimTextOutput("NPSGroupsCount")),

                  tabPanel("Data", DT::dataTableOutput("table")),

                  tabPanel("Plot", plotOutput("plot")),

                  tabPanel("Top detractor bigrams", verbatimTextOutput("topDetractor")),

                  tabPanel("Top passive bigrams", verbatimTextOutput("topPassive")),

                  tabPanel("Top promoter bigrams", verbatimTextOutput("topPromoter"))

      )



      # tableOutput("contents")

    )

  )

)







server <- function(input, output) {







  dt = reactive({

    inFile <- input$file1



    if (is.null(inFile))

      return(NULL)



    if (input$inputformat == "csv") {

      read.csv(inFile$datapath, header = input$header)

    } else if (input$inputformat == "excel") {

      read_excel(inFile$datapath)

    }







  })



  dt2 = reactive({

    temp = dt()

    temp = temp[,c(1,2)]

    colnames(temp) = c("NPSGroups", "NPSVerbatim")

    temp = dplyr::filter(temp, !is.na(NPSGroups))

    temp = dplyr::filter(temp, !is.na(NPSVerbatim))

    temp

  })



  dt3 = eventReactive(input$draw, {



    withProgress(message="Making plot", value = 0, {





      temp = dt2()



      nps_bigrams = temp %>%

        unnest_tokens(bigram, NPSVerbatim, token = "ngrams", n = 2, collapse = FALSE)



      bigrams_separated = nps_bigrams %>%

        separate(bigram, c("word1", "word2"), sep = " ")



      bigrams_filtered = bigrams_separated %>%

        filter(!word1 %in% stop_words$word) %>%

        filter(!word2 %in% stop_words$word)



      bigrams_united = bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")



      bigrams_tf_idf = bigrams_united %>%

        count(NPSGroups, bigram) %>%

        bind_tf_idf(bigram, NPSGroups, n) %>%

        arrange(desc(tf_idf))



      bigrams_tf_idf



    })

  })







  output$table <- DT::renderDataTable({



    dt2()



  })



  # Generate a summary of the data ----

  output$count <- renderPrint({



    cat("Number of records:", nrow(dt2()))



  })



  output$NPSGroupsCount = renderPrint({



    table(dt2()[,1])



  })



  # # Generate a summary of the data ----

  # output$NPSGroupsCount <- renderPrint({

  #

  #   table(dt2()[,1])

  #

  # })





  output$plot <- renderPlot({



    dt3() %>%

      arrange(desc(tf_idf)) %>%

      mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%

      group_by(NPSGroups) %>%

      top_n(input$topN) %>%

      ungroup() %>%

      ggplot(aes(bigram, tf_idf, fill = NPSGroups)) +

      geom_col(show.legend = F) +

      labs(x = NULL, y = "tf-idf") +

      facet_wrap (~NPSGroups, ncol = 3, scales = "free") +

      coord_flip() +

      ggtitle("Bigrams")





  })



  output$topDetractor = renderPrint({



    dt3()[dt3()$NPSGroups == "Detractor",] %>% print(n = input$topN)



  })



  output$topPassive = renderPrint({



    dt3()[dt3()$NPSGroups == "Passive",] %>% print(n = input$topN)



  })



  output$topPromoter = renderPrint({



    dt3()[dt3()$NPSGroups == "Promoter",] %>% print(n = input$topN)



  })



}


# increase the max upload file size
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

shinyApp(ui, server)

