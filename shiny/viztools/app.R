
library(viztools)
library(tools)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(UpSetR)
library(waffle)
library(glue)
library(assertthat)
library(scales)
library(markdown)
library(forcats)
library(shiny)
library(shinyWidgets)

# Type-dependent parameters ------------------------------------------

parameter_tabs <-
    tabsetPanel(
        id = "params",
        type = "hidden",
        tabPanel(
            "upset",
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                selectInput(
                    "upset_name",
                    "UpSet type",
                    choices = c("Protein", "Proteoform")
                )
            ),
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                textInput(
                    "upset_barcolor",
                    "Bar color",
                    "#4C4184"
                )
            )
        ),
        tabPanel(
            "intdeg",
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                selectInput(
                    "intdeg_name",
                    "Int. Deg. type",
                    choices = c("Protein", "Proteoform")
                )
            ),
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                textInput(
                    "intdeg_fillcolor",
                    "Fill color",
                    "#4C4184"
                )
            ),
            sliderInput(
                "intdeg_yrange",
                "Y range",
                0,
                100,
                100,
                step = 1
            )
        ),
        tabPanel(
            "heatmap",
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                selectInput(
                    "heatmap_name",
                    "Heatmap type",
                    choices = c("Protein", "Proteoform")
                )
            ),
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                selectInput(
                    "heatmap_orientation",
                    "Orientation",
                    choices = c("h", "v")
                )
            ),
            sliderInput(
                "heatmap_binsize",
                "Bin size (Da)",
                500,
                5000,
                1000,
                step = 500
            ),
            hr(),
            h5("Leave ranges blank for automatic sizing"),
            numericRangeInput(
                "heatmap_axisrange",
                "Mass axis range (kDa)",
                NULL
            ),
            numericRangeInput(
                "heatmap_countrange",
                "Count range",
                NULL
            ),
            hr(),
            h5("These should match column names in spreadsheet"),
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                textInput(
                    "heatmap_masscol",
                    "Mass column",
                    value = "mass"
                )
            ),
            div(
                style="display: inline-block;vertical-align:top; width: 150px;",
                textInput(
                    "heatmap_fractioncol",
                    "Fraction column",
                    value = "fraction"
                )
            )
        ),
        tabPanel(
            "waffle",
            selectInput(
                "waffle_name",
                "Waffle type",
                choices = c("Protein", "Proteoform")
            ),
            h5("This should match column name in spreadsheet"),
            textInput(
                "waffle_fractioncol",
                "Fraction column name",
                value = "fraction"
            )
        )
    )



# UI elements -------------------------------------------------------------

ui <-
    fixedPage(

        tags$head(
            tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}"))
        ),

        # Application title
        titlePanel("shiny viztools"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                tabsetPanel(
                    id = "main",
                    type = "tabs",
                    tabPanel(
                        "Make Plot",
                        br(),
                        fileInput(
                            "file1",
                            "Upload a CSV or XLSX file",
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
                            "Plot type",
                            choices = c(
                                "UpSet" = "upset",
                                "Int. Degree" = "intdeg",
                                "Heatmap" = "heatmap",
                                "Waffle" = "waffle"
                            )
                        ),
                        hr(),
                        parameter_tabs,
                        hr(),
                        actionBttn(
                            "startButton",
                            "Update Preview"
                        ),
                        br(),
                        br(),
                        downloadButton("downloadPDF", label = "Download PDF"),
                        downloadButton("downloadSVG", label = "Download SVG"),
                        downloadButton("downloadPNG", label = "Download PNG")
                    ),
                    tabPanel(
                        "Image Settings",
                        br(),
                        div(
                            style="display: inline-block;vertical-align:top; width: 150px;",
                            selectInput(
                                "download_font",
                                "Plot font",
                                choices = c(
                                    "sans",
                                    "serif",
                                    "mono"
                                )
                            )
                        ),
                        div(
                            style="display: inline-block;vertical-align:top; width: 150px;",
                            radioGroupButtons(
                                inputId = "download_unit",
                                label = "Size unit",
                                choices =
                                    c("cm", "inch"),
                                justified = TRUE
                            )
                        ),
                        numericInput(
                            "download_width",
                            "Image width",
                            value = 20,
                            step = 0.5
                        ),
                        numericInput(
                            "download_height",
                            "Image height",
                            value = 12.5,
                            step = 0.5
                        )
                        ,
                        sliderInput(
                            "download_dpi",
                            "Image DPI (PNG only)",
                            min = 50,
                            max = 600,
                            value = 300,
                            step = 50
                        )
                    ),
                    tabPanel(
                        "About",
                        br(),
                        includeMarkdown("about.md")
                    )
                ),
                width = 4
            ),
            # Show a plot of the generated distribution
            mainPanel(
                tableOutput(
                    "inputSheet"
                ),
                textOutput("truncnote"),
                plotOutput(
                    "outputPlot"
                )
            )

        )
    )

# Server function ---------------------------------------------------------

server <- function(input, output, session) {

    # Isolate input params so plot is not created until startButton is clicked

    isolate(input$file1)
    isolate(input$download_font)

    # Create expression used to generate plots on-demand

    plotExpression <-
        expr(
            isolate(
                switch(
                    input$type,
                    upset =
                        make_UpSet_plot(
                            inputsheet,
                            plotType = input$upset_name,
                            barColor = input$upset_barcolor
                        ),
                    intdeg =
                        make_intersection_degree_plot(
                            inputsheet,
                            Yrange = c(0, as.integer(input$intdeg_yrange)),
                            plotType = input$intdeg_name,
                            fillColor = input$intdeg_fillcolor,
                            fontFamily = input$download_font
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
                            countRange = input$heatmap_countrange,
                            fontFamily = input$download_font
                        ),
                    waffle =
                        waffle_iron(
                            inputsheet,
                            fraction_colname = input$waffle_fractioncol,
                            waffleType = input$waffle_name,
                            fontFamily = input$download_font
                        ),
                    make_UpSet_plot(
                        inputsheet,
                        plotType = input$upset_name
                    )
                )
            )
        )

    observeEvent(
        input$type,
        {
            updateTabsetPanel(session, "params", selected = input$type)
        }
    )

    observeEvent(
        input$file1,
        {
            validate(
                need(
                    is.null(input$file1) == FALSE,
                    'No input file'
                )
            )

            inFile <- input$file1

            if (file_ext(inFile$datapath) == "csv") {

                inputsheet <-
                    readr::read_csv(inFile$datapath)

            } else if (file_ext(inFile$datapath) == "xlsx") {

                inputsheet <-
                    readxl::read_xlsx(inFile$datapath)

            }

            output$truncnote <-
                if(nrow(inputsheet) > 6 | length(inputsheet) > 10) renderText("(input spreadsheet truncated)")

            output$inputSheet <-
                renderTable(
                    {
                        head(select(inputsheet, any_of(1:10)))
                    }
                )
        }
    )

    observeEvent(
        input$startButton,
        {
            {

                validate(
                    need(
                        is.null(input$file1) == FALSE,
                        'No input file'
                    )
                )

                inFile <- input$file1

                if (file_ext(inFile$datapath) == "csv") {

                    inputsheet <-
                        readr::read_csv(inFile$datapath)

                } else if (file_ext(inFile$datapath) == "xlsx") {

                    inputsheet <-
                        readxl::read_xlsx(inFile$datapath)

                }

            }

            output$outputPlot <-
                renderPlot(
                    {
                        validate(
                            need(
                                file_ext(input$file1) == "csv" | file_ext(input$file1) == "xlsx",
                                'Input file should be csv or xlsx'
                            )
                        )

                        eval(plotExpression)

                    },
                    width = 800,
                    height=800*(input$download_height/input$download_width)
                )

            output$downloadPDF <-
                downloadHandler(
                    filename = glue::glue(
                        "{format(Sys.time(), '%Y%m%d_%H%M%S')}_{input$type}_plot.pdf"
                    ),
                    content = function(file) {
                        pdf(
                            file = file,
                            width =
                                switch(
                                    input$download_unit,
                                    inch = input$download_width,
                                    cm = input$download_width/2.54
                                ),
                            height =
                                switch(
                                    input$download_unit,
                                    inch = input$download_height,
                                    cm = input$download_height/2.54
                                ),
                            bg = "transparent",
                            useDingbats = FALSE
                        )
                        print(eval(plotExpression))
                        dev.off()
                    }
                )

            output$downloadPNG <-
                downloadHandler(
                    filename = glue::glue(
                        "{format(Sys.time(), '%Y%m%d_%H%M%S')}_{input$type}_plot.png"
                    ),
                    content = function(file) {
                        png(
                            file = file,
                            width =
                                switch(
                                    input$download_unit,
                                    inch = input$download_width,
                                    cm = input$download_width/2.54
                                ),
                            height =
                                switch(
                                    input$download_unit,
                                    inch = input$download_height,
                                    cm = input$download_height/2.54
                                ),
                            units = "in",
                            bg = "white",
                            res = input$download_dpi
                        )
                        print(eval(plotExpression))
                        dev.off()
                    }
                )

            output$downloadSVG <-
                downloadHandler(
                    filename = glue::glue(
                        "{format(Sys.time(), '%Y%m%d_%H%M%S')}_{input$type}_plot.svg"
                    ),
                    content = function(file) {
                        svg(
                            file = file,
                            width =
                                switch(
                                    input$download_unit,
                                    inch = input$download_width,
                                    cm = input$download_width/2.54
                                ),
                            height =
                                switch(
                                    input$download_unit,
                                    inch = input$download_height,
                                    cm = input$download_height/2.54
                                ),
                            bg = "transparent"
                        )
                        print(eval(plotExpression))
                        dev.off()
                    }
                )


        }
    )

}

# Run the application

shinyApp(ui = ui, server = server)
