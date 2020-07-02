
library(shiny)
library(viztools)
library(tools)
library(purrr)
library(ggplot2)
library(dplyr)
library(UpSetR)
library(waffle)
library(glue)
library(assertthat)
library(scales)
library(forcats)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("viztools"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "file1",
                "Choose a CSV or XLSX File",
                accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                    ".xlsx"
                )
            ),
            radioButtons(
                inputId = "type",
                "Plot to create:",
                choices = c(
                    "UpSet" = "upset",
                    "Int. Degree" = "intdeg",
                    "Heatmap" = "heatmap",
                    "Waffle" = "waffle"
                )
            ),
            actionButton("startButton", "Go"),
            br(), br(),
            downloadButton("downloadPDF", label = "Download PDF"),
            downloadButton("downloadPNG", label = "Download PNG"),
            downloadButton("downloadSVG", label = "Download SVG")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(
                "outputPlot",
                width = 800,
                height = 500
            )
        )

    )
)

# Define server logic
server <- function(input, output) {

    observeEvent(
        input$startButton,
        {

            {
                inFile <- input$file1

                if (file_ext(inFile$datapath) == "csv") {

                    inputsheet <-
                        readr::read_csv(inFile$datapath)

                } else if (file_ext(inFile$datapath) == "xlsx") {

                    inputsheet <-
                        readxl::read_xlsx(inFile$datapath)

                }

                plotter <-
                    switch(
                        input$type,
                        upset = make_UpSet_plot,
                        intdeg = make_intersection_degree_plot,
                        heatmap = make_heatmap,
                        waffle = waffle_iron,
                        make_UpSet_plot
                    )

                outplot <-
                    plotter(inputsheet)
            }

            output$outputPlot <-
                renderPlot(
                    outplot
                )


            output$downloadPDF <-
                downloadHandler(
                    filename = "plot.pdf",
                    content = function(file) {
                        pdf(
                            file = file,
                            width = 8,
                            height = 5,
                            bg = "transparent",
                            useDingbats = FALSE
                        )
                        print(outplot)
                        dev.off()
                    }
                )

            output$downloadPNG <-
                downloadHandler(
                    filename = "plot.png",
                    content = function(file) {
                        png(
                            file = file,
                            width = 8,
                            height = 5,
                            units = "in",
                            bg = "white",
                            res = 300
                        )
                        print(outplot)
                        dev.off()
                    }
                )

            output$downloadSVG <-
                downloadHandler(
                    filename = "plot.svg",
                    content = function(file) {
                        svg(
                            file = file,
                            width = 8,
                            height = 5,
                            bg = "transparent"
                        )
                        print(outplot)
                        dev.off()
                    }
                )

        }
    )
}

# Run the application

shinyApp(ui = ui, server = server)
