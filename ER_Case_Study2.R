# These are all of the packages that are required to make the app run
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

# This will read in the data if it doesn't already exist within R's 
# environment.
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

# This retrieves the the product code which will be used to filter the data.
prod_codes <- setNames(products$prod_code, products$title)


ui <- fluidPage(
  h1("ER Case Study App"),
  tags$head(tags$style('h1 {color:red; font-style: italic}')),
  p("Please select a mode of injury"),
  fluidRow(
    column(8,
           # This allows the user to select from the list of products based
           # on the product codes.
           selectInput("code", "Mode of Injury",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # This will allow the user to select the number of rows they would like
    # to see displayed in the tables. I changes this to be a slider rather than
    # a drop down selection.
    column(2, sliderInput("rows", "Number of Rows",
                           min = 1, max = 10, value = 5)),
    # This displays the choice of Y-axis: rate (per 10,000 people) or count.
    # I changed this to show up as radio buttons rather than a drop down 
    # selection.
    column(2, radioButtons("y", "Y axis", c("rate" = "rate", "count" = "count"),
                           selected = "rate"))
  ),
  # This outputs the diagnosis, body part, and location data from the 
  # server in the form of tables, one for each variable.
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    # This outputs a plot from the server based on age and sex.
    column(12, plotOutput("age_sex"))
  ),
  
  # This adds two action buttons that allow the user to cycle forwards and
  # backwards through all of the narratives.
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)



# This function reduces the size of the output tables by only showing the first 
# 5 rows of the data and then combining the remaining rows into an "other" 
# category.
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
  # This reactive is used to select the product code that is input by the user
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # This determines the maximum number of rows for each category.
  max_no_rows <- reactive(
    max(length(unique(selected()$diag)),
        length(unique(selected()$body_part)),
        length(unique(selected()$location)))
  )
  
  # This will update the maximum number of rows value based on the user input.
  observeEvent(input$code, {
    updateNumericInput(session, "rows", max = max_no_rows())
  })
  
  table_rows <- reactive(input$rows - 1)
  
  # Output table for the diagnosis variable. Added n = table_rows() to allow
  # user to select desired number of rows. Changed labels.
  output$diag <- renderTable({
    count_top(selected(), diag, n = table_rows())
    diagTable <- count_top(selected(), var = diag, n = table_rows())
    colnames(diagTable) <- c("Diagnosis", "Number of Cases")
    diagTable
    
}, width = "100%")
  
  # Output table for the body_part variable. Added n = table_rows() to allow
  # user to select desired number of rows. Changed labels.
  output$body_part <- renderTable({
    bodyTable <- count_top(selected(), body_part, n = table_rows())
    colnames(bodyTable) <- c("Body Part Injured", "Number of Cases")
    bodyTable
    
}, width = "100%")
  
  # Output table for the location variable. Added n = table_rows() to allow
  # user to select desired number of rows.
  output$location <- renderTable({
    locTable <- count_top(selected(), location, n = table_rows())
    colnames(locTable) <- c("Location of Injury", "Number of Cases")
    locTable
    
}, width = "100%")
  
  # This reactive gets the raw and and sex count (n) and calculates the rate 
  # of injury per 10,000
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # Plot output depicting age vs injuries per 10,000 people
  output$age_sex <- renderPlot({
    # This is used if count is selected for the Y-axis. I changed the color
    # and line type.
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, group = sex)) +
        geom_line(aes(color=sex, linetype=sex)) +
        labs(y = "Estimated number of injuries") +
        scale_linetype_manual(values=c("twodash", "dotted"))+
        scale_color_manual(values=c('purple','blue'))
      
      # This is used if rate is selected for the Y-axis. I changed the color
      # and line type.
    } else {
      summary() %>%
        ggplot(aes(age, rate, group = sex)) +
        geom_line(aes(color=sex, linetype=sex), na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")+
        scale_linetype_manual(values=c("twodash", "dotted"))+
        scale_color_manual(values=c('purple','blue'))
    }
  }, res = 96)
  
  
  # This creates a reactive that stores the maximum possible number of stories.
  max_no_stories <- reactive(length(selected()$narrative))
  
  # This creates a reactive that saves the users current position in the 
  # narrative list.
  story <- reactiveVal(1)
  
  # This resets the story counter if the user changes the product code. 
  observeEvent(input$code, {
    story(1)
  })
  
  # This advances the narrative by 1 when the user clicks the "Next Story" 
  # button.
  # The mod function (%%) is keeping the current position within this interval.
  observeEvent(input$next_story, {
    story((story() %% max_no_stories()) + 1)
  })
  
  # This decreases the narrative by 1 when the user clicks the "Previous Story" 
  # button.
  # The mod function (%%) is keeping the current position within this interval.
  observeEvent(input$prev_story, {
    story(((story() - 2) %% max_no_stories()) + 1)
  })
  
  output$narrative <- renderText({
    selected()$narrative[story()]
  })
}

shinyApp(ui, server)
