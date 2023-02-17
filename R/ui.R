
## About -----

tab_about <- tabPanel(
  "About",
  h4("Running:"),
  h4(paste0(
    "shinyscan ",
    utils::packageVersion("shinyscan")," (",utils::packageDate('shinyscan'), ")"
  )),
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

## Plot -----
tab_plot <- tabPanel(
  "Plot",
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", "Plot engine", choices = resources$choices$fn_plot),
      textAreaInput(
        "plot_arguments",
        "Arguments", value = "",rows = 5
      ),
      actionButton("plot_help", "Open help"),
      hr(),
      downloadButton("saveplot", "Save plot"),
    ),
    mainPanel(
      verbatimTextOutput("plot_syntax"),
      plotOutput("plot_scdf", width = 800,height = 600)
    )
  )
)

# Transform -----
tab_transform <- tabPanel(
  "Transform",
  sidebarLayout(
    sidebarPanel(
      textInput("select_cases", "Select cases", value = ""),
      textInput("select_phases", "Select phases", value = ""),
      textInput("subset", "Subset", value = ""),
      textAreaInput("transform", "Transform", value = "", rows = 5),
      downloadButton("save", "Save active scdf")
    ),
    mainPanel(
      verbatimTextOutput("transform_syntax"),
      verbatimTextOutput("transform_scdf")
      #htmlOutput("transform_html")
    )
  )
)

# scdf ------
tab_scdf <-   tabPanel(
  "scdf",
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", NULL, accept = c(".csv", ".rds", ".xlsx", "xls"),
                buttonLabel = "Load"),
      radioButtons("datasource", "Source", c("My scdf", "Example"), "Example"),
      selectInput(
        "example",
        "Example",
        choices = resources$choices$examples,
        selected = "exampleAB"
      ),
      hr(),
      h4("New case"),
      br(),
      textAreaInput(
        "values",
        "Values", value = "A = 1,2,3,4,3, \nB = 7,6,7,8,7,6"
      ),
      textInput("casename", "Case name", value = "My case"),
      actionButton("set_case", "Set first"),
      actionButton("add_case", "Add"),
      actionButton("remove_case", "Remove last"),
    ),

    mainPanel(
      verbatimTextOutput("scdf_summary"),
      verbatimTextOutput("scdf_syntax"),
    )
  )
)

# Stats -----
tab_stats <- tabPanel(
  "Stats",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "func",
        "Statistic",
        choices = resources$choices$fn_stats
      ),
      radioButtons(
        "stats_default", "Defaults", choices = c("No", "Yes"), inline = TRUE
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

## Export -----
tab_export <- tabPanel(
  "Export",
  sidebarLayout(
    sidebarPanel(
      textInput("export_arguments", "Arguments")
    ),
    mainPanel(htmlOutput("export_html"))
  )
)




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
