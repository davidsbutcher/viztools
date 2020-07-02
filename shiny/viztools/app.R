
library(shiny)
library(shinyWidgets)
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


parameter_tabs <-
    tabsetPanel(
        id = "params",
        type = "hidden",
        tabPanel(
            "upset",
            selectInput(
                "upset_name",
                "UpSet type",
                choices = c("Protein", "Proteoform")
            )
        ),
        tabPanel(
            "intdeg",
            selectInput(
                "intdeg_name",
                "Int. Deg. type",
                choices = c("Protein", "Proteoform")
            ),
            sliderInput(
                "intdeg_yrange",
                "Y range",
                0,
                100,
                100,
                step = 1
            ),
            textInput(
                "intdeg_fillcolor",
                "Fill Color",
                "#4C4184"
            )
        ),
        tabPanel(
            "heatmap",
            selectInput(
                "heatmap_name",
                "Heatmap type",
                choices = c("Protein", "Proteoform")
            ),
            selectInput(
                "heatmap_orientation",
                "Orientation",
                choices = c("h", "v")
            ),
            sliderInput(
                "heatmap_binsize",
                "Bin size",
                500,
                10000,
                1000,
                step = 500
            ),
            textInput(
                "heatmap_masscol",
                "Mass column name",
                value = "mass"
            ),
            textInput(
                "heatmap_fractioncol",
                "Fraction column name",
                value = "fraction"
            ),
            numericRangeInput(
                "heatmap_axisrange",
                "Axis range",
                NULL
            ),
            numericRangeInput(
                "heatmap_countrange",
                "Count range",
                NULL
            )
        ),
        tabPanel(
            "waffle",
            selectInput(
                "waffle_name",
                "Waffle type",
                choices = c("Protein", "Proteoform")
            ),
            textInput(
                "waffle_fractioncol",
                "Fraction column name",
                value = "fraction"
            )
        )
    )


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("shiny viztools"),

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
            selectInput(
                inputId = "type",
                "Plot to create:",
                choices = c(
                    "UpSet" = "upset",
                    "Int. Degree" = "intdeg",
                    "Heatmap" = "heatmap",
                    "Waffle" = "waffle"
                )
            ),
            parameter_tabs,
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
server <- function(input, output, session) {

    observeEvent(
        input$type,
        {
            updateTabsetPanel(session, "params", selected = input$type)
        }
    )

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
                    switch(
                        input$type,
                        upset =
                            make_UpSet_plot(
                                inputsheet,
                                plotType = input$upset_name
                            ),
                        intdeg =
                            make_intersection_degree_plot(
                                inputsheet,
                                Yrange = c(0, as.integer(input$intdeg_yrange)),
                                plotType = input$intdeg_name,
                                fillColor = input$intdeg_fillcolor
                            ),
                        heatmap =
                            make_heatmap(
                                inputsheet,
                                plotType = input$heatmap_name,
                                orientation = input$heatmap_orientation,
                                binSize = input$heatmap_binsize,
                                massColname = input$heatmap_masscol,
                                fractionColname = input$heatmap_fractioncol,
                                axisRange = input$heatmap_axisrange,
                                countRange = input$heatmap_countrange
                            ),
                        waffle =
                            waffle_iron(
                                inputsheet,
                                fraction_colname = input$waffle_fractioncol,
                                waffleType = input$waffle_name
                            ),
                        make_UpSet_plot(
                            inputsheet,
                            plotType = input$upset_name
                        )
                    )

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
