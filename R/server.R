
server <- function(input, output, session) {

  # Startup message
  output$scdf_summary <- renderPrint({
    cat(
      "Welcome to 'shiny scan'!",
      "",
      "You can:",
      "1. create a new case (click 'Add')",
      "2. load a dataset (click 'Load file' to import an rds, csv, or excel file)",
      "3. choose an example scdf (choosse from 'Load example')",
      sep = "\n"
    )
  })

  my_scdf <- reactiveVal()

  scdf_render <- reactive({

    output$scdf_summary <- renderPrint({
      if (inherits(my_scdf(), "scdf")) {
        do.call("summary", list(my_scdf()))
      } else {
        cat(
          "No case has been defined yet.",
          "You can:",
          "1. create a new case (click 'Add')",
          "2. load a dataset (click 'Load file' to import an rds, csv, or excel file)",
          "3. choose an example scdf (choosse from 'Load example')",
          sep = "\n"
        )
      }
    })

    output$scdf_syntax <- renderPrint({
      if (inherits(my_scdf(), "scdf")) {
        do.call("convert", list(my_scdf()))
      }
    })

  })

  observeEvent(input$scdf_example, {
    if (input$scdf_example != "(none)") {
      my_scdf(paste0("scan::", input$scdf_example) |> str2lang() |> eval())
      scdf_render()
    } else {
      my_scdf(NULL)
    }
  })


  # scdf: upload / save ------
  observeEvent(input$upload, {
    ext <- tools::file_ext(input$upload$datapath)
    if (ext == "rds") {
      new <- readRDS(input$upload$datapath)
      #syntax <- paste0("scdf <- read_RDS(\"", input$upload$name, "\")")
    } else {
      new <- read_scdf(input$upload$datapath)
      #syntax <- paste0("scdf <- read_scdf(\"", input$upload$name, "\")")
    }
    my_scdf(new)
    scdf_render()
  })

  output$scdf_save <- downloadHandler(
    filename = function() "my_scdf.rds",
    content = function(file) saveRDS(my_scdf(), file)
  )

  # scdf: new cases --------
  observeEvent(input$add_case, {
    new <- try({
      values <- paste0("c(", input$values, ")") |> str2lang() |> eval()
      dvar <- "values"
      if (inherits(my_scdf(), "scdf")) {
        dvar <- scdf_attr(my_scdf(), "var.values")
      }

      if (input$mt == "") {
        call <- paste0(
          "scdf(", dvar, " = ", deparse(values),
          ", dvar = ", deparse(dvar),
          ", name = ",
          deparse(input$casename), ")"
        )
      } else {
        call <- paste0(
          "scdf(", dvar, " = ", deparse(values),
          ", mt = ", deparse(mt),
          ", dvar = ", deparse(dvar),
          ", name = ", deparse(input$casename), ")"
        )
      }
      new <- call |> str2lang() |> eval()
    }, silent = TRUE)
    if (!inherits(new, "try-error")) {
      if (length(my_scdf()) > 0) new <- c(my_scdf(), new)
      my_scdf(new)
      scdf_render()
    }
  })

  observeEvent(input$remove_case, {
    if (length(my_scdf()) > 1) {
      my_scdf(my_scdf()[-length(my_scdf())])
    } else (my_scdf(NULL))
    scdf_render()
  })

  observeEvent(input$remove_all, {
    my_scdf(NULL)
    scdf_render()
  })

  # transform ----

  transformed <- reactive({
    out <- my_scdf()
    syntax = "scdf"
    if (input$select_cases != "") {
      args <- list(str2lang(input$select_cases))
      out <- do.call("select_cases", c(list(out), args))
      syntax <- c(syntax, paste0("select_cases(",input$select_cases,")"))

    }

    if (input$select_phases != "") {
      out <- paste0("select_phases(out, ", input$select_phases, ")") |>
        str2lang() |> eval()
      syntax <- c(syntax, paste0("select_phases(", input$select_phases, ")"))
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

    if (input$setdvar != "") {
      args <-  list(input$setdvar)
      out <- do.call("set_dvar", c(list(out),args))
      syntax <- c(syntax, paste0("set_dvar(",  deparse(input$setdvar), ")"))
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
    if (inherits(my_scdf(), "scdf")) {
      print(transformed(), rows = 100)
    } else {
      cat(
        "There is no case defined yet.",
        "Please define a case on the 'scdf' tab first.",
        sep = "\n"
      )
    }
  })

  output$transform_save <- downloadHandler(
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
    if (inherits(my_scdf(), "scdf")) {
      call <- get_stats_call()
      scdf <- transformed()
      print_args <- input$stats_print_arguments
      if (print_args != "")
        print_args <- paste0(", ", print_args)
      call<- paste0("print(", call, print_args, ")")
      str2lang(call) |> eval()
    } else {
      cat(
        "There is no case defined yet.",
        "Please define a case on the 'scdf' tab first.",
        sep = "\n"
      )
    }

  })


  observeEvent(input$stats_help, {
    link <- paste0(
      "https://jazznbass.github.io/scan/reference/", input$func, ".html"
    )
    js$openURL(link)
    #browseURL(link)
  })

  output$stats_syntax <- renderPrint({
    cat(get_stats_call())
  })


  # stats: arguments ------

  stat_arg_names <- reactive({
    args <- names(formals(input$func))
    values <- formals(input$func)

    id <- which(!args %in% c(
      "dvar", "pvar", "mvar", "phases", "meta_method",
      "data", "scdf", "data.l2", "offset", "lag.max",
      "graph", "output", "...")
    )
    args <- args[id]
    values <- values[id]
    list(names = args, values = values)
  })

  output$stats_arguments <- renderUI({
    args <- stat_arg_names()

    out <- vector("list", length(args$names))
    if (length(out) > 0) {
      for (i in 1:length(out)) {

        value <- args$values[[i]]
        if (is.character(value)) value <- deparse(value)
        if (isTRUE(is.na(value))) value <- substitute(value) |> deparse()
        if (is.null(value)) value <- substitute(value) |> deparse()
        if (!is.numeric(value) && !is.logical(value) && !is.character(value)) {
          value <- substitute(value) |> deparse()
        }

        if (input$stats_default == "Yes") outvalue <- value else outvalue = NULL

        if (is.numeric(value)) {
          out[[i]] <- numericInput(
            args$names[i], args$names[i], value = outvalue
          )
        } else if (is.logical(value)) {
          out[[i]] <- radioButtons(
            args$names[i], args$names[i], choices = c("empty" = "", "FALSE", "TRUE"),
            inline = TRUE, selected = outvalue
          )
        } else {
          out[[i]] <- textInput(args$names[i], args$names[i], value = outvalue)
        }
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
    js$openURL(link)
  })

  render_plot <- reactive({
    if (inherits(my_scdf(), "scdf")) {
      if (input$plot == "scplot") {
        call <- paste0("scplot(transformed())")
        if (trimws(input$plot_arguments) != "") {
          call <- paste0(
            call, "%>% ", gsub("\n", " %>% ", trimws(input$plot_arguments))
          )
        }
        paste0("print(",call,")") |> str2lang() |> eval()
      } else if (input$plot == "plot.scdf") {
        call <- paste0(
          "plot(transformed(), ", trim(input$plot_arguments), ")"
        )
        str2lang(call) |> eval()
      }
    }
  })

  output$plot_scdf <- renderPlot({
    render_plot()
  })

  output$saveplot <- downloadHandler(
    filename = function() "my_scan_plot.png",
    content = function(file) {
      if (input$plot == "scplot"){
        ggplot2::ggsave(
          file, render_plot(), width = input$width, height = input$height,
          dpi = input$dpi, units = "px",  device = "png"
        )
      }
      if (input$plot == "plot.scdf"){
        grDevices::png(file, width = input$width, height = input$height,
                       res = input$dpi, units = "px")
        call <- paste0(
          "plot(transformed(), ", trim(input$plot_arguments), ")"
        )
        str2lang(call) |> eval()
        grDevices::dev.off()
      }
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
