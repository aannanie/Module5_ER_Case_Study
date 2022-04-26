# These are all of the packages that are required to make the app run
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

# This will read in the data if it doesn't already exist within R's 
# environment. Had to add the "neiss/" so R knew where to look for the data.
if (!exists("injuries")) {
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
}

# This retrieves the the product code which will be used to filter the data
prod_codes <- setNames(products$prod_code, products$title)


ui <- fluidPage(
  fluidRow(
    column(8,
           # This allows the user to select from the list of products based
           # on the product codes.
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # This displays the choice of Y-axis: rate (per 10,000 people) or count.
    column(2, selectInput("y", "Y axis", c("rate", "count")))
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
  
  fluidRow(
    # This creates an action button that allows the user to randomly generate
    # a narrative (based on the specified product code) each time the button is
    # pressed. Added play button icon.
    column(2, actionButton("story", "Tell me a story", 
                           icon = icon("play-circle"))),
    column(10, textOutput("narrative"))
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
  
  # Output table for the diagnosis variable
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  
  # Output table for the body_part variable
  output$body_part <- renderTable(count_top(selected(), body_part), width = 
                                    "100%")
  # Output table for the location variable
  output$location <- renderTable(count_top(selected(), location), width = 
                                   "100%")
  
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
  
  
  # Randomly samples 5 narratives from selected input field when the action 
  # button is pressed.
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(5)
  )
  # Outputs the randomly generated narrative.
  output$narrative <- renderText(narrative_sample())
  
}

shinyApp(ui, server)
