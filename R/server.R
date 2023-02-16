



server <- function(input, output, session) {

  scdf <- reactive({
    out <- NULL
    if (input$datasource == "My scdf") {#if (input$example == "My scdf") {
      out <- readRDS(tmp_filename)
    } else {
      out <- paste0("scan::",input$example) |> str2lang() |> eval()
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

  # upload ------
  observeEvent(input$upload, {
    ext <- tools::file_ext(input$upload$datapath)
    if (ext == "rds") {
      new <- readRDS(input$upload$datapath)
      syntax <- paste0("scdf <- read_RDS(\"", input$upload$name, "\")")

    } else {
      new <- read_scdf(input$upload$datapath)
      syntax <- paste0("scdf <- read_scdf(\"", input$upload$name, "\")")
    }

    #output$scdf_upload <- renderText(syntax)
    saveRDS(new, tmp_filename)
    updateRadioButtons(session, "datasource", selected = "My scdf")
  })

  observeEvent(input$add_case, {
    new <- try({
      values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
      scan::scdf(values, name = input$casename)
    }, silent = TRUE)
    if (!inherits(new, "try-error")) {
      if (input$datasource == "My scdf") {
        old <- readRDS(tmp_filename)
      } else {
        old <- paste0("scan::",input$example) |> str2lang() |> eval()
        updateRadioButtons(session, "datasource", selected = "My scdf")
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
      updateRadioButtons(session, "datasource", selected = "My scdf")
      saveRDS(new, tmp_filename)

      output$scdf_summary <- renderPrint(do.call("summary", list(new)))
      output$scdf_syntax <- renderPrint(do.call("convert", list(new)))
    }

  })

  observeEvent(input$remove_case, {

    if (input$datasource == "My scdf") {
      new <- readRDS(tmp_filename)
    } else {
      new <- paste0("scan::",input$example) |> str2lang() |> eval()
      updateRadioButtons(session, "datasource", selected = "My scdf")
    }

    new <- new[-length(new)]
    saveRDS(new, tmp_filename)
    output$scdf_summary <- renderPrint(do.call("summary", list(new)))
    output$scdf_syntax <- renderPrint(do.call("convert", list(new)))
  })


  output$save <- downloadHandler(
    filename = function() "my_scdf.rds",
    content = function(file) saveRDS(transformed(), file)
  )

  # plot -----

  observeEvent(input$plot_help, {
    if (input$plot == "scplot") {
      link <- "https://jazznbass.github.io/scplot/reference/index.html"
    } else if (input$plot == "plot.scdf") {
      link <- "https://jazznbass.github.io/scan/reference/plot.scdf.html"
    }
    browseURL(link)
  })

  render_plot <- reactive({
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

  output$plot_scdf <- renderPlot({
    render_plot()
  })

  output$saveplot <- downloadHandler(
    filename = function() "my_scan_plot.png",
    content = function(file) {
      png(file)
      render_plot()
      dev.off()
    }
  )

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

  # stats -----

  output$stats_html <- renderUI({
    scdf <- transformed()
    call <- paste0(input$func, "(scdf")
    if (input$stats_arguments == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$stats_arguments, ")")
    }
    print_args <- input$stats_print_arguments
    if (print_args != "")
      print_args <- paste0(", ", print_args)
    call<- paste0("export(", call, print_args, ")")
    str2lang(call) |> eval() |> HTML()
  })

  output$stats_text <- renderPrint({
    scdf <- transformed()
    first <- paste0(input$func, "(scdf")
    if (input$stats_arguments == "") {
      last <- ")"
    } else {
      last <- paste0(", ", input$stats_arguments, ")")
    }
    call <- paste0(first, last)
    print_args <- input$stats_print_arguments
    if ( print_args != "")
      print_args <- paste0(", ", print_args)
    call<- paste0("print(", call, print_args, ")")
    str2lang(call) |> eval()
  })


  observeEvent(input$stats_help, {
    link <- paste0(
      "https://jazznbass.github.io/scan/reference/", input$func, ".html"
    )
    browseURL(link)
  })

  output$stats_syntax <- renderPrint({
    call <- paste0(input$func, "(scdf")
    if (input$stats_arguments == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$stats_arguments, ")")
    }
    cat(call)
  })

  output$funcargs <- renderText({
    get_call(input$func)
  })

  # scdf ----

  output$scdf_summary <- renderPrint({
    do.call("summary", list(scdf()))
  })

  output$scdf_syntax <- renderPrint({
    do.call("convert", list(scdf()))
  })

  # export html ----
  output$export_html <- renderUI({
    scdf <- transformed()
    call <- paste0(input$func, "(scdf")
    if (input$func == "") {
      call <- paste0(call, ")")
    } else {
      call <- paste0(call, ",", input$stats_arguments, ")")
    }
    out <- str2lang(call) |> eval()
    out <- paste0("export(out,", input$export_arguments, ")") |>
      str2lang() |> eval()
    HTML(out)
  })


  # transform ----
  output$transform_scdf <- renderPrint({
    print(transformed(), rows = 100)
  })


}
