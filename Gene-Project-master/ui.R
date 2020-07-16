fluidPage(
  titlePanel("Gene Expression"),
  theme = shinytheme("yeti"),
  fluidRow(
    column(4, wellPanel(
      sliderInput("n", "Number of Genes:",
                  min = 11, max = 1000, value = 200, step = 10),
      br(),
      actionButton("goButton", "Go!")
    )),
    column(8,
           h4("Accuracy Summary"),
           h4("Min.", HTML('&emsp;') ,HTML('&emsp;'),"1st Qu.", HTML('&emsp;'),"Median.", HTML('&emsp;'),  "Mean.", HTML('&emsp;'), "3rd Qu.", HTML('&emsp;'),  "Max. "),
           verbatimTextOutput("summary"),
           h4("Box Plot"),
           plotOutput("plot1", width = 400, height = 300),
           h4("Genes Used In The Model"),
           textOutput("summary2")
    )
  )
)