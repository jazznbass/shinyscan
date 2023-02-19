try(source("resources.R"))

# scdf ------
tab_scdf <-   tabPanel(
  "scdf",
  sidebarLayout(
    sidebarPanel(
      h4("New case"),
      br(),
      textAreaInput(
        "values", "Values", value = "A = 1,2,3,4,3, \nB = 7,6,7,8,7,6"
      ),
      textInput("mt", "Measurement times"),
      textInput("casename", "Case name", value = "Example case"),
      actionButton("add_case", "Add"),
      actionButton("remove_case", "Remove last"),
      actionButton("remove_all", "Remove all"),
      hr(),
      fileInput("upload", NULL, accept = c(".csv", ".rds", ".xlsx", "xls"),
                buttonLabel = "Load file"),
      downloadButton("scdf_save", "Save"),
      hr(),
      selectInput(
        "scdf_example", "Load example", choices = resources$choices$examples
      ),
    ),

    mainPanel(
      verbatimTextOutput("scdf_summary"),
      verbatimTextOutput("scdf_syntax"),
    )
  )
)

# Transform -----
tab_transform <- tabPanel(
  "Transform",
  sidebarLayout(
    sidebarPanel(
      textInput("select_cases", "Select cases", value = ""),
      textInput("select_phases", "Recombine phases", value = ""),
      textInput("subset", "Filter measurments", value = ""),
      textAreaInput("transform", "Transform variables", value = "", rows = 5),
      textInput("setdvar", "Set dependent variable", value = ""),
      downloadButton("transform_save", "Save")
    ),
    mainPanel(
      verbatimTextOutput("transform_syntax"),
      verbatimTextOutput("transform_scdf")
      #htmlOutput("transform_html")
    )
  )
)


# Stats -----
tab_stats <- tabPanel(
  "Stats",
  useShinyjs(),
  extendShinyjs(text = js_code, functions = 'openURL'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "func",
        "Statistic",
        choices = resources$choices$fn_stats
      ),
      radioButtons(
        "stats_default", "Use defaults", choices = c("No", "Yes"), inline = TRUE
      ),

      uiOutput("stats_arguments"),
      hr(),
      actionButton("stats_help", "Open help")
    ),
    mainPanel(
      radioButtons(
        "stats_out", "Output format", c("Text", "Html"), "Text", inline = TRUE
      ),
      textInput("stats_print_arguments", "Output arguments"),
      hr(),
      verbatimTextOutput("stats_syntax"),
      conditionalPanel(
        'input.stats_out == "Text"', verbatimTextOutput("stats_text")
      ),
      conditionalPanel(
        'input.stats_out == "Html"', htmlOutput("stats_html")
      )

    )
  )
)


## Plot -----
tab_plot <- tabPanel(
  "Plot",
  useShinyjs(),
  extendShinyjs(text = js_code, functions = 'openURL'),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", "Plot engine", choices = resources$choices$fn_plot),
      textAreaInput(
        "plot_arguments",
        "Arguments", value = "",rows = 5
      ),
      actionButton("plot_help", "Open help"),
      hr(),
      numericInput("width", "Width", value = 800, min = 100, max = 2000),
      numericInput("height", "Height", value = 600, min = 100, max = 2000),
      numericInput("dpi", "Dpi", value = 100, min = 50, max = 600),
      downloadButton("saveplot", "Save plot"),
    ),
    mainPanel(
      verbatimTextOutput("plot_syntax"),
      plotOutput("plot_scdf", width = 800,height = 600)
    )
  )
)

## About -----

tab_about <- tabPanel(
  "About",
  h4("Running:"),
  h4(paste0(
    "scan ",
    utils::packageVersion("scan")," (",utils::packageDate('scan'), ")"
  )),
  h4(paste0(
    "scplot ",
    utils::packageVersion("scplot")," (",utils::packageDate('scplot'), ")"
  )),
  hr(),
  h4("Please cite as:"),
  h4({x<-citation("scan"); class(x)<-"list"; attributes(x[[1]])$textVersion}),
  hr(),
  h4("(c) Jürgen Wilbert, 2023")
)

## ui ------

ui <- navbarPage(
  title = "Shiny scan",
  #theme = shinythemes::shinytheme("sandstone"),
  #header = shinythemes::themeSelector(),
  tags$header(tags$style("#funcargs{
                         font-size: 16px;
                         font-style: italic;
                         }"
  )),
  tab_scdf,
  tab_transform,
  tab_stats,
  tab_plot,
  #tab_test,
  tab_about
)
