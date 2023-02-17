
server <- function(input, output, session) {

  # scdf -----
  scdf <- reactive({
    out <- NULL
    if (input$datasource == "My scdf") {
      out <- readRDS(tmp_filename)
    } else {
      out <- paste0("scan::",input$example) |> str2lang() |> eval()#
      #get(input$example, "package:scan")
    }
    out
  })

  output$scdf_summary <- renderPrint({
    do.call("summary", list(scdf()))
  })

  output$scdf_syntax <- renderPrint({
    do.call("convert", list(scdf()))
  })

  # scdf: upload ------
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


  # scdf: new cases --------
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

  # transform ----

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

  output$transform_scdf <- renderPrint({
    print(transformed(), rows = 100)
  })

  output$save <- downloadHandler(
    filename = function() "my_scdf.rds",
    content = function(file) saveRDS(transformed(), file)
  )

  # stats -----

  output$stats_html <- renderUI({
    scdf <- transformed()
    call <- get_stats_call()
    print_args <- input$stats_print_arguments
    if (print_args != "")
      print_args <- paste0(", ", print_args)
    call<- paste0("export(", call, print_args, ")")
    str2lang(call) |> eval() |> HTML()
  })

  output$stats_text <- renderPrint({
    call <- get_stats_call()
    scdf <- transformed()
    print_args <- input$stats_print_arguments
    if (print_args != "")
      print_args <- paste0(", ", print_args)
    call<- paste0("print(", call, print_args, ")")
    str2lang(call) |> eval()
  })


  observeEvent(input$stats_help, {
    link <- paste0(
      "https://jazznbass.github.io/scan/reference/", input$func, ".html"
    )
    #js$openURL(link)
    browseURL(link)
  })

  output$stats_syntax <- renderPrint({
    cat(get_stats_call())
  })


  # stats: arguments ------

  stat_arg_names <- reactive({
    args <- names(formals(input$func))
    values <- formals(input$func)

    id <- which(!args %in% c("dvar", "pvar", "mvar", "phases", "meta_method",
                             "data", "scdf", "data.l2", "offset", "lag.max",
                             "graph", "output", "..."))
    args <- args[id]
    values <- values[id]
    list(names = args, values = values)
  })

  output$stats_arguments <- renderUI({
    args <- stat_arg_names()

    out <- vector("list", length(args$names))
    if (length(out) > 0) {
      for (i in 1:length(out)) {
        value <- ""
        if (input$stats_default == "Yes") {
          value <- args$values[[i]]
          if (is.character(value)) {
            value <- paste0("\"", value, "\"")
          } else {
            value <- substitute(value) |> deparse()
          }
        }
        out[[i]] <- textInput(
          args$names[i], args$names[i], value = value
          #isolate(input[[args$names[i]]])
        )
      }
      return(out)
    }
  })

  get_stats_call <- reactive({
    args <- stat_arg_names()
    values <- sapply(args$names, function(name) input[[name]])
    args <- args$names

   id <- which(values != "")

    args <- args[id]
    values <- values[id]

    call <- paste0(
      input$func, "(scdf",
      if (length(args > 0)) {
        paste0(", ",paste0(args, " = ", values, collapse = ", "))
      } else {
        ""
      },
      ")"
    )
    call
  })


  # plot -----

  observeEvent(input$plot_help, {
    if (input$plot == "scplot") {
      link <- "https://jazznbass.github.io/scplot/reference/index.html"
    } else if (input$plot == "plot.scdf") {
      link <- "https://jazznbass.github.io/scan/reference/plot.scdf.html"
    }
    #js$openURL(link)
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


}
