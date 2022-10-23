# Análisis gráfico de los cultivos en la Unión Europea

library(shiny)
library("shinyWidgets")
library(rsconnect)

shinyUI(fluidPage(
  # Application title
  titlePanel("Crop analysis in the European Union"),
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(12,
           fluidRow(
             column(12,
                    wellPanel(
                      selectInput("Crop", label = "Choose the crop to see how its production has evolved", 
                                  choices = list("Barley" = "Barley",
                                                 "Hops" = "Hops",
                                                 "Maize" = "Maize",
                                                 "Olive oil" = "Olive oil",
                                                 "Raspberries" = "Raspberries",
                                                 "Rye" = "Rye",
                                                 "Sorghum" = "Sorghum",
                                                 "Strawberries" = "Strawberries",
                                                 "Triticale" = "Triticale",
                                                 "Walnuts" = "Walnuts"), 
                                  selected = 1))
                    )),
           fluidRow(
             column(12,
                    wellPanel(radioButtons("selectcountry",label = "Do you want to compare the evolution of crop production in the country with the highest production and in another country?",
                                       choices = c("Compare with another country","Do not compare with another country"), 
                                       selected = "Do not compare with another country")),
                    )),
            fluidRow(column(12,
                            conditionalPanel(condition = "input.selectcountry == 'Compare with another country'",
                                                        wellPanel(
                                                          textInput("secondCountry", label = "Then, write the chosen country. If the country has not produced the crop it will not be shown in the graph", value = "",
                                                          )),
                                                 ),
                               )
                      )
                    
           )),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Does the production of crops with high prices VS cheap differ much?", fluidRow(column(12,
                                             fluidRow(
                                                  column(6,
                                                    plotOutput("plot1", 
                                                       dblclick = "plot1_dblclick",
                                                       brush = brushOpts(
                                                         id = "plot1_brush",
                                                         resetOnNew = TRUE)
                                                              )
                                                        ),
                                                  column(6, plotOutput("plot2"))
                                                      )
                                      ))
        ),
        tabPanel("How crop yields have evolved over the years?", fluidRow(
                                                                            column(12, 
                                                                              plotOutput("plot3"), 
                                                                         fluidRow(
                                                                            column(12,
                                                                              plotOutput("plot4")))
                                                                            ))
        )
      ))
  ))
