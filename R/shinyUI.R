#' data viewer
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.dataviewer <- function(bench.proj) {

  data = bench.proj$dataset()

  shiny::shinyApp(
    ui = shiny::fluidPage(

      shiny::titlePanel('Data set'),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("select", label = shiny::h3("Select data set"),
                            choices = data$name,
                            selected = 1)
        ),
        shiny::mainPanel(
          shiny::tableOutput(outputId="data.info")
        )
      ),
      shiny::hr(),

      shiny::headerPanel('snmf'),
      shiny::sidebarPanel(
        shiny::numericInput('K', 'ancestral population number', 2, step = 1, min = 1),
        shiny::actionButton("snmf", label = "run sNMF")
      ),
      shiny::mainPanel(
        shiny::plotOutput('snmf.plot')
      ),

      shiny::hr(),

      shiny::headerPanel('PCA'),
      shiny::sidebarPanel(
        shiny::actionButton("pca", label = "run prcomp"),
        shiny::selectInput('pca.x', 'X Variable', c("PC1", "PC2","PC3")),
        shiny::selectInput('pca.y', 'Y Variable', c("PC1", "PC2","PC3"))
      ),
      shiny::mainPanel(
        shiny::plotOutput('pca.plot')
      )

    ),
    server = function(input, output,session) {

      #data.name = input$select
      output$data.info <- shiny::renderTable({ dplyr::select( dplyr::filter(data, name==input$select), name, description )})

      # snmf
      shiny::observeEvent(input$snmf, {
        t = bench.snmf(bench.proj, input$select,K = input$K, again = TRUE)
      })
      output$snmf.plot <- shiny::renderPlot({
        # Take a dependency on input$snmf
        input$snmf

        data.exploration = bench.getdata.exploration(bench.proj, input$select)
        if(!is.null(data.exploration$snmf)){
          # barplot(t(data.exploration$snmf$Q))
          K = ncol(data.exploration$snmf$Q)
          toplot = data.frame(data.exploration$snmf$Q)
          names(toplot) = 1:K
          toplot = dplyr::add_rownames(toplot)
          toplot = tidyr::gather(toplot, "cluster", "coef", 2:(K+1))
          ggplot2::ggplot(toplot, ggplot2::aes(x=rowname, y = coef, color = cluster, fill =cluster)) +
            ggplot2::geom_bar(stat="identity") + ggplot2::xlab("Indiv")
        }
      })


      # PCA
      observeEvent(input$pca, {
        t = bench.pca(bench.proj, input$select, again = TRUE)
      })
      output$pca.plot <- renderPlot({
        # Take a dependency on input$snmf
        input$pca

        data.exploration = bench.getdata.exploration(bench.proj, input$select)
        if(!is.null(data.exploration$pca)){
          ggplot2::ggplot(data.frame(data.exploration$pca$x), ggplot2::aes(x = PC1, y = PC2)) +
            ggplot2::geom_point()
        }
      })


    },
    options = list(height = 1250)
  )


}


#' result viewer
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.resultviewer <- function(bench.proj) {

  data = bench.proj$dataset()
  methods = bench.proj$methods()

  shiny::shinyApp(
    ui = shiny::fluidPage(

      shiny::titlePanel('Result'),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("data", label = shiny::h3("Select data set"),
                             choices = data$name,
                             selected = 1),
          shiny::selectInput("method", label = shiny::h3("Select methods"),
                            choices = methods$name,
                            selected = 1)
        ),
        shiny::mainPanel(
          shiny::tableOutput(outputId="data.info")
        )
      ),
      shiny::hr(),

      shiny::headerPanel('Result'),
      shiny::sidebarPanel(
        shiny::textInput("parameter", label = "Parameter", value = "parameter = list()"),
        shiny::actionButton("run", label = "run"),
        shiny::hr(),
        shiny::selectInput('plot', 'Plot', c("histogram of pvalue", "manhattan plot","size effect"))
      ),
      shiny::mainPanel(
        shiny::plotOutput('result.plot')
      )


    ),
    server = function(input, output,session) {

      #data.name = input$select
      output$data.info <- shiny::renderTable({ dplyr::filter(bench.proj$summary(), data==input$data, method==input$method ) })

      # plot result
      shiny::observeEvent(input$run, {
        eval(parse(text = input$parameter))
        bench.run(bench.proj, data.name = input$data, method.name = input$method, again = TRUE,parameter = parameter)
        #data.name = input$select
        output$data.info <- shiny::renderTable({ dplyr::filter(bench.proj$summary(), data==input$data, method==input$method ) })
      })
      output$result.plot <- shiny::renderPlot({
        # Take a dependency on input$snmf
        input$run

        plots = bench.plotres(bench.proj, data.name = input$data, method.name = input$method)
        if(input$plot=="histogram of pvalue") {
          plots$mplot
        } else if(input$plot=="manhattan plot") {
          plots$histpvalue
        } else if(input$plot=="size effect") {
          plots$b
        }

      })


    },
    options = list(height = 860)
  )


}

#' result viewer
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.adddataviewer <- function(bench.proj) {

  shiny::shinyApp(
    ui = shiny::fluidPage(

      shiny::headerPanel('Add/Remove Data Set'),
      shiny::sidebarPanel(
        shiny::h2("Add Data Set"),
        shiny::fileInput("file.geno", label = "Genotype"),
        shiny::fileInput("file.env", label = "Environmental Variable"),
        shiny::fileInput("file.outlier", label = "Outlier"),
        shiny::hr(),
        shiny::h3("Remove Data Set"),
        shiny::selectInput('plot', 'Plot', c("histogram of pvalue", "manhattan plot","size effect"))
      ),
      shiny::mainPanel(
        shiny::plotOutput('result.plot')
      )


    ),
    server = function(input, output,session) {

      NULL
    }
  )


}


#' Comparaison viewer
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.comparaisonviewer <- function(bench.proj) {

  methods = bench.proj$methods()

  shiny::shinyApp(
    ui = shiny::fluidPage(

      shiny::headerPanel('Comparaison'),
      shiny::sidebarPanel(
        uiOutput("data"),
        uiOutput("methods"),

        shiny::hr(),
        shiny::selectInput('plot', 'Plot', c("fdr", "power","fdr power", "F1 score", "correlation")),
        shiny::sliderInput("fdr_lim", label = shiny::h3("Threshold"), min = 0,
                    max = 1.0, value = c(0.0, 0.25))

      ),
      shiny::mainPanel(
        shiny::plotOutput('comp.plot')
      )


    ),
    server = function(input, output,session) {

      # select data
      output$data <- renderUI({
        data = bench.proj$dataset()
        shiny::selectInput("data", label = shiny::h3("Select data set"), choices =
                           data$name)
      })

      # select method
      output$methods <- renderUI({
        sum = bench.proj$summary()
        m = dplyr::filter(sum, data == input$data)$method
        if(length(m) == 0) {
         shiny::helpText("No run for this dataset")
        } else {
        shiny::checkboxGroupInput("methods", label = shiny::h3("Methods"), choices = m, selected = input$methods)
        }
      })


      # comparaison plots
      output$comp.plot <- shiny::renderPlot({
        plots=bench.plotfdr_power(bench.proj, input$data, input$methods, input$fdr_lim)
        if(input$plot=="fdr") {
          plots$fdr
        } else if(input$plot=="power") {
          plots$power
        } else if(input$plot=="fdr power") {
          plots$fdr.power
        } else if(input$plot=="F1 score") {
          plots$f1.score
        } else if(input$plot=="correlation") {
          plots = bench.heatmap(bench.proj, input$data, input$methods)
          plots$cor
        }
      })


    },
    options = list(height = 750)
  )


}


#' run sampler viewer
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.samplerviewer <- function(bench.proj) {

  sampler = bench.proj$sampler()

  shiny::shinyApp(
    ui = shiny::fluidPage(

      shiny::headerPanel('Sampler'),
      shiny::sidebarPanel(
        shiny::selectInput("sampler", label = shiny::h3("Select sampler"),
                           choices = sampler$name,
                           selected = 1),

        shiny::hr(),
        uiOutput("param"),
        shiny::actionButton("run", label = "run"),

        shiny::hr(),
        uiOutput("dataSelector"),
        shiny::actionButton("remove", label = "remove")

      ),
      shiny::mainPanel(
        shiny::h3("Sampler"),
        shiny::tableOutput(outputId="sampler.info"),
        shiny::hr(),
        shiny::h3("Dateset"),
        shiny::tableOutput(outputId="data.info")
      )


    ),
    server = function(input, output,session) {



      output$sampler.info <- shiny::renderTable({dplyr::select( dplyr::filter(sampler, name == input$sampler), name, description) })

      output$param <- renderUI({
        shiny::textInput("parameter", label = shiny::h3("Parameter"), value =
                           dplyr::select( dplyr::filter(sampler, name == input$sampler), default_parameter)[1])
      })

      shiny::observeEvent(input$run, {
        eval(parse(text = input$parameter))
        bench.runsampler(bench.proj, sampler.name = input$sampler, repetition = 1, parameter = parameter)


        # data update
        output$data.info <- shiny::renderTable({
          data = bench.proj$dataset()
          data[grep(input$sampler,data$name),c(1,2)]
        })
        output$dataSelector <- renderUI({
          data = bench.proj$dataset()
          names = data[grep(input$sampler,data$name),1]
          shiny::selectInput("dataset", label = shiny::h3("Select dataset"),
                             choices = names,
                             selected = 1)
        })

      })

      # data update
      output$data.info <- shiny::renderTable({
        data = bench.proj$dataset()
        data[grep(input$sampler,data$name),c(1,2)]
      })
      output$dataSelector <- renderUI({
        data = bench.proj$dataset()
        names = data[grep(input$sampler,data$name),1]
        shiny::selectInput("dataset", label = shiny::h3("Select dataset"),
                           choices = names,
                           selected = 1)
      })

      shiny::observeEvent(input$remove, {
        bench.removedataset(bench.proj, input$dataset)

        # data update
        output$data.info <- shiny::renderTable({
          data = bench.proj$dataset()
          data[grep(input$sampler,data$name),c(1,2)]
        })
        output$dataSelector <- renderUI({
          data = bench.proj$dataset()
          names = data[grep(input$sampler,data$name),1]
          shiny::selectInput("dataset", label = shiny::h3("Select dataset"),
                             choices = names,
                             selected = 1)
        })

      })

    },
    options = list(height = 750)
  )


}

