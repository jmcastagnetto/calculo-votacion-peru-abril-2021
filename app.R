#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel(
        "Estimación del número de mesas necesarias -- Elecciones Generales 2021, Perú"
    ),
    fluidRow(
        column(
        2,
        wellPanel(
            markdown("**Porcentaje en la población adulta**"),
            numericInput(
                inputId = "pct_adultos_mayores",
                label = "... de adultos mayores",
                value = 18.4,
                min = 10,
                max = 30,
                step = 0.1
            ),
            numericInput(
                inputId = "pct_gestantes",
                label = "... de gestantes",
                value = 3.5,
                min = 2,
                max = 10,
                step = 0.1
            ),
            markdown("*****

                     **Tiempo para votar promedio (en minutos)**"),
            numericInput(
                inputId = "tiempo_adultos_mayores",
                label = "... para adultos mayores",
                value = 8,
                min = 2,
                max = 15
            ),
            numericInput(
                inputId = "tiempo_gestantes",
                label = "... para gestantes",
                value = 7,
                min = 2,
                max = 15
            ),
            numericInput(
                inputId = "tiempo_resto",
                label = "... para el resto de votantes",
                value = 4,
                min = 2,
                max = 15
            )
        )
    ),
    column(
        6,
        markdown("**Escenario 1**: Si se cumple el esquema propuesto por ONPE"),
        #tableOutput("tablaGrupos"),
        tableOutput("tablaEscenario1"),
        markdown("

                 **Escenario 2**: Los grupos de riesgo disponen de 12 horas para votar, el resto sólo 10 horas"),
        tableOutput("tablaEscenario2"),
        markdown("

                 **Escenario 3**: Si todos los grupos disponen de 12 horas para votar"),
        tableOutput("tablaEscenario3")

    ),
    column(4, includeMarkdown("descripcion.md"))
    )
)

server <- function(input, output) {
    output$tablaEscenario1 <- renderTable({
        # Datos de ONPE/JNE
        votantes_2021 <- 24290921
        actas_2021 <- 86488
        n_adultos_mayores <-
            ceiling(input$pct_adultos_mayores / 100 * votantes_2021)
        n_gestantes <-
            ceiling(input$pct_gestantes / 100 * votantes_2021)
        n_adultos_menor_60 <-
            votantes_2021 - n_adultos_mayores - n_gestantes
        pph_adultos_mayores <- 60 / input$tiempo_adultos_mayores
        pph_gestantes <- 60 / input$tiempo_gestantes
        pph_resto <- 60 / input$tiempo_resto
        df2 <- data_frame(
            Grupo = c(
                "Adultos Mayores",
                "Gestantes",
                "Adultos < 60 años"
            ),
            "Número de horas de votación" = c("2h", "2h", "10h"),
            votantes = c(
                n_adultos_mayores,
                n_gestantes,
                n_adultos_menor_60
            ),
            "Personas atendidads por mesa y hora" = c(
                sprintf("%.1f personas/hora", pph_adultos_mayores),
                sprintf("%.1f personas/hora", pph_gestantes),
                sprintf("%.1f personas/hora", pph_resto)
            ),
            mesas = c(
                ceiling(n_adultos_mayores / (pph_adultos_mayores * 2)),
                ceiling(n_gestantes / (pph_gestantes * 2)),
                ceiling(n_adultos_menor_60 / (pph_resto * 10))
            )
        ) %>%
        janitor::adorn_totals() %>%
        mutate(
            "Número de votantes" = format(votantes, big.mark = ","),
            "Mesas requeridas" = format(mesas, big.mark = ",")
        ) %>%
        select(-votantes, -mesas)
    }, striped = TRUE, hover = TRUE, align = "lccrr")

    output$tablaEscenario2 <- renderTable({
        # Datos de ONPE/JNE
        votantes_2021 <- 24290921
        actas_2021 <- 86488
        n_adultos_mayores <-
            ceiling(input$pct_adultos_mayores / 100 * votantes_2021)
        n_gestantes <-
            ceiling(input$pct_gestantes / 100 * votantes_2021)
        n_adultos_menor_60 <-
            votantes_2021 - n_adultos_mayores - n_gestantes
        pph_adultos_mayores <- 60 / input$tiempo_adultos_mayores
        pph_gestantes <- 60 / input$tiempo_gestantes
        pph_resto <- 60 / input$tiempo_resto
        df2 <- data_frame(
            Grupo = c(
                "Adultos Mayores",
                "Gestantes",
                "Adultos < 60 años"
            ),
            "Número de horas de votación" = c("12h", "12h", "10h"),
            votantes = c(
                n_adultos_mayores,
                n_gestantes,
                n_adultos_menor_60
            ),
            "Personas atendidads por mesa y hora" = c(
                sprintf("%.1f personas/hora", pph_adultos_mayores),
                sprintf("%.1f personas/hora", pph_gestantes),
                sprintf("%.1f personas/hora", pph_resto)
            ),
            mesas = c(
                ceiling(n_adultos_mayores / (pph_adultos_mayores * 12)),
                ceiling(n_gestantes / (pph_gestantes * 12)),
                ceiling(n_adultos_menor_60 / (pph_resto * 10))
            )
        ) %>%
            janitor::adorn_totals() %>%
            mutate(
                "Número de votantes" = format(votantes, big.mark = ","),
                "Mesas requeridas" = format(mesas, big.mark = ",")
            ) %>%
            select(-votantes, -mesas)
    }, striped = TRUE, hover = TRUE, align = "lccrr")

    output$tablaEscenario3 <- renderTable({
        # Datos de ONPE/JNE
        votantes_2021 <- 24290921
        actas_2021 <- 86488
        n_adultos_mayores <-
            ceiling(input$pct_adultos_mayores / 100 * votantes_2021)
        n_gestantes <-
            ceiling(input$pct_gestantes / 100 * votantes_2021)
        n_adultos_menor_60 <-
            votantes_2021 - n_adultos_mayores - n_gestantes
        pph_adultos_mayores <- 60 / input$tiempo_adultos_mayores
        pph_gestantes <- 60 / input$tiempo_gestantes
        pph_resto <- 60 / input$tiempo_resto
        df2 <- data_frame(
            Grupo = c(
                "Adultos Mayores",
                "Gestantes",
                "Adultos < 60 años"
            ),
            "Número de horas de votación" = c("12h", "12h", "12h"),
            votantes = c(
                n_adultos_mayores,
                n_gestantes,
                n_adultos_menor_60
            ),
            "Personas atendidads por mesa y hora" = c(
                sprintf("%.1f personas/hora", pph_adultos_mayores),
                sprintf("%.1f personas/hora", pph_gestantes),
                sprintf("%.1f personas/hora", pph_resto)
            ),
            mesas = c(
                ceiling(n_adultos_mayores / (pph_adultos_mayores * 12)),
                ceiling(n_gestantes / (pph_gestantes * 12)),
                ceiling(n_adultos_menor_60 / (pph_resto * 12))
            )
        ) %>%
            janitor::adorn_totals() %>%
            mutate(
                "Número de votantes" = format(votantes, big.mark = ","),
                "Mesas requeridas" = format(mesas, big.mark = ",")
            ) %>%
            select(-votantes, -mesas)
    }, striped = TRUE, hover = TRUE, align = "lccrr")
}

# Run the application
shinyApp(ui = ui, server = server)
