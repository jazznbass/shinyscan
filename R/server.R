



server <- function(input, output, session) {

  upload <- reactive({
    req(input$upload)
    readRDS(input$upload$datapath)
  })

  scdf <- reactive({
    out <- NULL
    if (input$dataset == "input") {
      out <- try({
        values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
        out <- scan::scdf(values, name = input$casename)
      }, silent = TRUE)
      if (inherits(out, "try-error")) out <- NULL
    } else if (input$dataset == "loaded") {
      out <- upload()
    } else if (input$dataset == "active") {
      out <- readRDS(tmp_filename)
    } else {
      out <- paste0("scan::",input$dataset) |> str2lang() |> eval()
    }

    if (input$select != "") {
      args <- paste0("list(", input$select, ")") |> str2lang() |> eval()
      out <- do.call("select_cases", c(list(out), args))
    }

    if (input$phase_A != "" && input$phase_B != "") {
      arg_a <- paste0("c(", input$phase_A, ")") |> str2lang() |> eval()
      arg_b <- paste0("c(", input$phase_B, ")") |> str2lang() |> eval()

      out <- do.call("select_phases", c(list(out), list(A = arg_a, B = arg_b)))
    }
    out
  })


  observeEvent(input$add_case, {
    new <- try({
      values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
      scan::scdf(values, name = input$casename)
    }, silent = TRUE)
    if (!inherits(new, "try-error")) {
      if (input$dataset == "loaded") {
        updateSelectInput(session, "dataset", selected = "active")
        old <- readRDS(input$upload$datapath)
      } else if (input$dataset == "active") {
        old <- readRDS(tmp_filename)
      } else {
        old <- paste0("scan::",input$dataset) |> str2lang() |> eval()
        updateSelectInput(session, "dataset", selected = "active")
      }

      if (!is.null(old)) new <- c(old, new)
      saveRDS(new, tmp_filename)

      output$scdf_html <- renderUI(HTML(export(new)))
      output$scdf_syntax <- renderPrint(do.call("convert", list(new)))

    }

  })

  observeEvent(input$set_case, {
    new <- try({
      values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
      scan::scdf(values, name = input$casename)
    }, silent = TRUE)
    if (!inherits(new, "try-error")) {
      updateSelectInput(session, "dataset", selected = "active")
      saveRDS(new, tmp_filename)

      output$scdf_html <- renderUI(HTML(export(new)))
      output$scdf_syntax <- renderPrint(do.call("convert", list(new)))
    }

  })

  observeEvent(input$remove_case, {

    if (input$dataset == "loaded") {
      updateSelectInput(session, "dataset", selected = "active")
      new <- readRDS(input$upload$datapath)
    } else if (input$dataset == "active") {
      new <- readRDS(tmp_filename)
    } else {
      new <- paste0("scan::",input$dataset) |> str2lang() |> eval()
      updateSelectInput(session, "dataset", selected = "active")
    }

    new <- new[-length(new)]
    saveRDS(new, tmp_filename)
    output$scdf_html <- renderUI(HTML(export(new)))
    output$scdf_syntax <- renderPrint(do.call("convert", list(new)))
  })


  output$save <- downloadHandler(
    filename = function () {
      paste0("my_scdf", ".rds")
    },
    content = function(file) saveRDS(scdf(), file)
  )

  output$scdf <- renderPlot({
    if (input$plot == "scplot") {
      scplot(scdf())
    } else if (input$plot == "plot.scdf") {
      plot(scdf())
    }
  })


  output$startupmessage <- renderText({
    paste0(
      "scan ",utils::packageVersion("scan")," (",utils::packageDate('scan'), ")"
    )
  })

  output$statistic <- renderPrint({
    scdf <- scdf()
    call <- paste0(input$func, "(scdf")
    if (input$func == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$args, ")")
    }
    str2lang(call) |> eval()
  })

  output$funcargs <- renderText({
    get_call(input$func)
  })


  output$scdf_print <- renderPrint({
    if (!is.null(scdf())) print(scdf())
  })

  output$scdf_html <- renderUI({
    if (!is.null(scdf())) HTML(export(scdf()))
  })

  output$scdf_syntax <- renderPrint({
    do.call("convert", list(scdf()))
  })


}
