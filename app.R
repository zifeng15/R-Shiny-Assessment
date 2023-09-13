library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(DT)
library(rsconnect)

####Define UI####
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Import claims file", accept = c(".xlsx")),
      h5("Example Excel Input Format:"),
      img(src = "example_input_format.png", 
          height = 150, 
          width = 235, 
          style = "margin-bottom: 20px;"),
      numericInput("tail_factor", "Tail Factor", value = 0, step = 0.1),
      actionButton("calculate", "Calculate", style = "margin-bottom: 10px;"),
      div(downloadButton("download_table", "Download Table")),
      div(downloadButton("download_plot", "Download Plot"))
    ),
    
    mainPanel(
      navbarPage(title = "Stimulation Results",
                 tabPanel(
                   "Cumulative Paid Claims Table",
                   fluidRow(
                     column(12, dataTableOutput("table"))
                   )
                 ),
                 tabPanel(
                   "Cumulative Paid Claims Plot",
                   fluidRow(
                     column(12, plotOutput("plot"))
                   )
                 )
      )
    )
  )
)

####Define server logic####
server <- function(input, output, session) {
  
  ####Create reactive for loss years####
  loss_years <- reactive({
    req(input$file)
    claims_table <- read_xlsx(input$file$datapath) %>%
      rename("Loss Year" = "Loss Year", 
             "Development Year" = "Development Year",
             "Amount of Claims Paid ($)" = "Amount of Claims Paid ($)")
    
    unique_loss_years <- unique(claims_table$`Loss Year`)
    return(unique_loss_years)
  })
  
  ####Create eventReactive for stimulation####
  stimulation_data <- eventReactive(input$calculate, {
    req(input$file)
    claims_table <- read_xlsx(input$file$datapath)
    colnames(claims_table) <- c("Loss Year", 
                                "Development Year", 
                                "Amount of Claims Paid ($)")
    
    paid_claims <- matrix(0, 
                          nrow = length(loss_years()), 
                          ncol = length(loss_years()) + 1)
    
    cumulative_claims <- matrix(0, 
                                nrow = length(loss_years()), 
                                ncol = length(loss_years()) + 1)
    
    for (i in 1:length(loss_years())) {
      AoP <- 1
      for (j in 1:nrow(claims_table)) {
        if (claims_table$'Loss Year'[j] == loss_years()[i]) {
          paid_claims[i, AoP] <- claims_table$'Amount of Claims Paid ($)'[j]
          AoP <- AoP + 1
        } else {
          AoP <- 1
        }
      }
      
      for (j in 1:length(loss_years())) {
        if (paid_claims[i, j] != 0) {
          if (j == 1) {
            cumulative_claims[i, j] <- paid_claims[i, j]
          } else {
            cumulative_claims[i, j] <- sum(paid_claims[i, 1:j])
          }
        }
      }
    }
    
    ####Calculate development factor and projected claims####
    dev_factor <- matrix(1, nrow = 1, ncol = length(loss_years()) + 1)
    for (i in 1:length(loss_years())) {
      df1 <- 0
      df2 <- 0
      if (i != 1) {
        df1 <- sum(cumulative_claims[1:(length(loss_years()) + 1 - i), i - 1])
        df2 <- sum(cumulative_claims[1:(length(loss_years()) + 1 - i), i])
        dev_factor[1, i] <- df2 / df1
      }
    }
    
    dev_factor[1, length(loss_years()) + 1] <- input$tail_factor
    
    projected_claims <- cumulative_claims
    for (i in 1:length(loss_years())) {
      for (j in 1:length(loss_years()) + 1) {
        if (projected_claims[i, j] == 0) {
          projected_claims[i, j] <- projected_claims[i, j - 1] * dev_factor[j]
        }
      }
    }
    round(projected_claims, 0)
  })
  
  output$table_title <- renderText({
    "Cumulative Claims Table"
  })
  
  output$plot_title <- renderText({
    "Cumulative Claims Plot"
  })
  
  output$table <- renderDataTable({
    table_data <- as.data.frame(stimulation_data())
    dev_year_list <- c()
    for (i in 1:(length(loss_years()) + 1)){
      dev_year_list <- append(dev_year_list, 
                              paste("Develpoment Year ", i, sep = ""))
    }
    table_data <- cbind(loss_years(), table_data)
    colnames(table_data) <- c("Loss Year", dev_year_list)
    table_data
  })
  
  output$plot <- renderPlot({
    plot_data <- as.data.frame(stimulation_data())
    
    development_years <- c()
    for (i in (1:(length(loss_years()) + 1))){
      development_years <- append(development_years, i)
    }
    plot_data <- rbind(development_years, plot_data)
    transposed_plot_data <- as.data.frame(t(plot_data))
    colnames(transposed_plot_data) <- c("Development Year", loss_years())
    
    ####Create line####
    p <- ggplot() + labs(x = "Development Year", y = "Cumulative Claims ($)")
    for (i in 1:length(loss_years())){
      aes <- aes_string(x = transposed_plot_data[,1],
                        y = transposed_plot_data[,(i+1)],
                        color = factor(loss_years()[i]))
      p <- p + geom_smooth(aes,
                           method = "loess",
                           se = FALSE,
                           linewidth = 0.5)
      p <- p + geom_text(transposed_plot_data,
                         mapping = aes,
                         label = paste(transposed_plot_data[,(i+1)]),
                         size = 3,
                         vjust = -1,
                         show.legend = FALSE)
    }
    p
  })
  
  #### Function to generate a CSV for the table ####
  generateTableCSV <- function() {
    write.csv(stimulation_data(), "table.csv", row.names = TRUE)
  }
  
  #### Function to generate a PNG for the plot ####
  generatePlotPNG <- function() {
    png("plot.png", width = 800, height = 600)
    print(output$plot)
    dev.off()
  }
  
  #### Report download handler for table CSV ####
  output$download_table <- downloadHandler(
    filename = function() {
      "table.csv"
    },
    content = function(file) {
      generateTableCSV()
      file.copy("table.csv", file)
    }
  )
  
  #### Report download handler for plot ####
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      plot_data <- as.data.frame(stimulation_data())
      
      development_years <- c()
      for (i in (1:(length(loss_years()) + 1))){
        development_years <- append(development_years, i)
      }
      plot_data <- rbind(development_years, plot_data)
      transposed_plot_data <- as.data.frame(t(plot_data))
      colnames(transposed_plot_data) <- c("Development Year", loss_years())
      
      #### Create line plot ####
      p <- ggplot() + labs(x = "Development Year", y = "Cumulative Claims ($)")
      for (i in 1:length(loss_years())){
        aes <- aes_string(x = transposed_plot_data[,1],
                          y = transposed_plot_data[,(i+1)],
                          color = factor(loss_years()[i]))
        p <- p + geom_smooth(aes,
                             method = "loess",
                             se = FALSE,
                             linewidth = 0.5)
        p <- p + geom_text(transposed_plot_data,
                           mapping = aes,
                           label = paste(transposed_plot_data[,(i+1)]),
                           size = 3,
                           vjust = -1,
                           show.legend = FALSE)
      }
      ggsave(file, p, width = 8, height = 6, dpi = 300)
    }
  )
}

shinyApp(ui, server)


