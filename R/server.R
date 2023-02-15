



server <- function(input, output, session) {

  upload <- reactive({
    req(input$upload)
    readRDS(input$upload$datapath)
  })

  scdf <- reactive({
    out <- NULL
    if (input$dataset == "My scdf") {
      out <- readRDS(tmp_filename)
    } else {
      out <- paste0("scan::",input$dataset) |> str2lang() |> eval()
    }
    out
  })

  transformed <- reactive({
    out <- scdf()
    syntax = "scdf"
    if (input$select_cases != "") {
      args <- list(str2lang(input$select_cases))
      out <- do.call("select_cases", c(list(out), args))
      syntax <- c(syntax, paste0("select_cases(",input$select_cases,")"))

    }

    if (input$select_phases != "") {
      args <- paste0("c(", input$select_phases, ")") |> str2lang() |> eval()
      out <- do.call("select_phases", c(list(out), args))
      syntax <- c(syntax, paste0("select_phases(c(", input$select_phases, "))"))
    }

    if (input$subset != "") {
      args <-  list(str2lang(input$subset))
      out <- do.call("subset", c(list(out),args))
      syntax <- c(syntax, paste0("subset(",  input$subset, ")"))
    }

    if (input$transform != "") {
      arg <- paste0("transform(out,", trim(input$transform),")")
      out <- str2lang(arg) |> eval()
      syntax <- c(
        syntax, paste0("transform(", gsub("\n", ", ", trim(input$transform)),")")
      )
    }

    if (length(syntax)>1) {
      syntax <- syntax[-1]
      syntax <- paste0(
        "scdf %>%\n  ",
        paste0(syntax, collapse = " %>%\n  ")
      )

    }

    output$transform_syntax <- renderPrint(cat(syntax))

    out
  })

  observeEvent(input$add_case, {
    new <- try({
      values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
      scan::scdf(values, name = input$casename)
    }, silent = TRUE)
    if (!inherits(new, "try-error")) {
      if (input$dataset == "My scdf") {
        old <- readRDS(tmp_filename)
      } else {
        old <- paste0("scan::",input$dataset) |> str2lang() |> eval()
        updateSelectInput(session, "dataset", selected = "My scdf")
      }

      if (!is.null(old)) new <- c(old, new)
      saveRDS(new, tmp_filename)

      output$scdf_summary <- renderPrint(do.call("summary", list(new)))
      output$scdf_syntax <- renderPrint(do.call("convert", list(new)))

    }

  })

  observeEvent(input$set_case, {
    new <- try({
      values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
      scan::scdf(values, name = input$casename)
    }, silent = TRUE)
    if (!inherits(new, "try-error")) {
      updateSelectInput(session, "dataset", selected = "My scdf")
      saveRDS(new, tmp_filename)

      output$scdf_summary <- renderPrint(do.call("summary", list(new)))
      output$scdf_syntax <- renderPrint(do.call("convert", list(new)))
    }

  })

  observeEvent(input$remove_case, {

    if (input$dataset == "My scdf") {
      new <- readRDS(tmp_filename)
    } else {
      new <- paste0("scan::",input$dataset) |> str2lang() |> eval()
      updateSelectInput(session, "dataset", selected = "My scdf")
    }

    new <- new[-length(new)]
    saveRDS(new, tmp_filename)
    output$scdf_summary <- renderPrint(do.call("summary", list(new)))
    output$scdf_syntax <- renderPrint(do.call("convert", list(new)))
  })


  output$save <- downloadHandler(
    filename = "my_scdf.rds",
    content = function(file) saveRDS(transformed(), file)
  )

  output$plot_scdf <- renderPlot({
    if (input$plot == "scplot") {
      call <- paste0("scplot(transformed())")
      if (trimws(input$plot_arguments) != "") {
        call <- paste0(
          call, "%>% ", gsub("\n", " %>% ", trimws(input$plot_arguments))
        )
      }

      str2lang(call) |> eval()
    } else if (input$plot == "plot.scdf") {
      call <- paste0(
        "plot(transformed(), ", trim(input$plot_arguments), ")"
      )
      str2lang(call) |> eval()
    }
  })

  output$plot_syntax <- renderPrint({
    if (input$plot == "scplot") {
      call <- paste0("scplot(scdf)")
      if (trimws(input$plot_arguments) != "") {
        call <- paste0(
          call, "%>%\n  ", gsub("\n", " %>%\n  ", trimws(input$plot_arguments))
        )
      }
    } else if (input$plot == "plot.scdf") {
      if (trim(input$plot_arguments) != "") {
        call <- paste0("plot(scdf, ", trim(input$plot_arguments), ")")
      } else {
        call <- "plot(scdf)"
      }
    }
    cat(call)
  })

  output$startupmessage <- renderText({
    paste0(
      "scan ",utils::packageVersion("scan")," (",utils::packageDate('scan'), ")"
    )
  })

  # stats -----
  output$statistic <- renderPrint({
    scdf <- transformed()
    call <- paste0(input$func, "(scdf")
    if (input$args == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$args, ")")
    }
    str2lang(call) |> eval()
  })

  output$stats_syntax <- renderPrint({
    call <- paste0(input$func, "(scdf")
    if (input$args == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$args, ")")
    }
    cat(call)
  })

  output$funcargs <- renderText({
    get_call(input$func)
  })

  # scdf ----
  output$scdf_print <- renderPrint({
    if (!is.null(scdf())) print(scdf())
  })

  #output$scdf_html <- renderUI({
  #  if (!is.null(scdf())) HTML(export(scdf()))
  #})

  output$scdf_summary <- renderPrint({
    do.call("summary", list(scdf()))
  })

  output$scdf_syntax <- renderPrint({
    do.call("convert", list(scdf()))
  })

  output$export_html <- renderUI({
    scdf <- transformed()
    call <- paste0(input$func, "(scdf")
    if (input$func == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$args, ")")
    }
    out <- str2lang(call) |> eval()
    out <- paste0("export(out,", input$exportargs, ")") |>
      str2lang() |> eval()
    HTML(out)
  })


  # transform ----
  output$transform_scdf <- renderPrint({
    print(transformed(), rows = 100)
  })


}
