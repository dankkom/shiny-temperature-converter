
library(shiny)


ui <- fluidPage(

    includeCSS("styles.css"),

    titlePanel("Temperature converter"),

    sidebarLayout(
        sidebarPanel(
            numericInput(
                inputId = "temp",
                value = 0,
                label = "Temperature"),
            radioButtons(
                inputId = "unit",
                label = "Unit",
                choices = c("Celsius", "Fahrenheit", "Kelvin"),
                selected = "Celsius")
        ),

        mainPanel(
            textOutput("o"),
            div(
                htmlOutput("converted_temp1", inline = TRUE),
                htmlOutput("converted_unit1", class = "unit")
            ),
            hr(),
            div(
                htmlOutput("converted_temp2", inline = TRUE),
                htmlOutput("converted_unit2", class = "unit")
            )
        )
    )
)


server <- function(input, output) {

    output$o <- renderText({
        paste(
            "Temperature of",
            format(input$temp, digits = 2, nsmall = 2),
            input$unit,
            "equals to:"
        )
    })

    output$converted_temp1 <- renderText({
        temp <- input$temp
        if (input$unit == "Celsius") {
            val <- (temp * 9 / 5) + 32
            unit <- "°F"
        } else if (input$unit == "Fahrenheit") {
            val <- (temp - 32) * 5 / 9
            unit <- "°C"
        } else if (input$unit == "Kelvin") {
            val <- temp - 273.15
            unit <- "°C"
        }
        format(val, digits = 2, nsmall = 2)
    })

    output$converted_unit1 <- renderText({
        temp <- input$temp
        if (input$unit == "Celsius") {
            unit <- "°F"
        } else if (input$unit == "Fahrenheit") {
            unit <- "°C"
        } else if (input$unit == "Kelvin") {
            unit <- "°C"
        }
        unit
    })

    output$converted_temp2 <- renderText({
        temp <- input$temp
        if (input$unit == "Celsius") {
            val <- temp + 273.15
            unit <- "°K"
        } else if (input$unit == "Fahrenheit") {
            val <- (temp - 32) * 5 / 9 + 273.15
            unit <- "°K"
        } else if (input$unit == "Kelvin") {
            val <- (temp - 273.15) * 9 / 5 + 32
            unit <- "°F"
        }
        format(val, digits = 2, nsmall = 2)
    })

    output$converted_unit2 <- renderText({
        temp <- input$temp
        if (input$unit == "Celsius") {
            unit <- "°K"
        } else if (input$unit == "Fahrenheit") {
            unit <- "°K"
        } else if (input$unit == "Kelvin") {
            unit <- "°F"
        }
        unit
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
