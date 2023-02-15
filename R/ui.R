

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
      fileInput("upload", NULL, accept = c(".csv", ".rds"),
                buttonLabel = "Load"),
      hr(),
      selectInput(
        "dataset",
        "Data source",
        choices = resources$choices$examples,
        selected = "exampleAB"
      ),
      textAreaInput(
        "values",
        "New case (e.g. A = 1,2,3,4,3, B = 7,6,7,8,7,6)", value = ""
      ),
      textInput("casename", "Case name", value = "My case"),
      actionButton("set_case", "Set first"),
      actionButton("add_case", "Add"),
      actionButton("remove_case", "Remove last"),

      textOutput("startupmessage"),
    ), #end sidebarPanel

    mainPanel(
      #conditionalPanel(
      #   condition = "input.plot != 'none'",
      #   plotOutput("scdf")
      # ),
      #verbatimTextOutput("scdf_print"),
      verbatimTextOutput("scdf_summary"),
      verbatimTextOutput("scdf_syntax"),
      #htmlOutput("scdf_html")
    )

  ) # end sidebarLayout

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
      textInput("args", "Arguments"),
      textOutput("funcargs"),
    ),
    mainPanel(
      verbatimTextOutput("stats_syntax"),
      verbatimTextOutput("statistic")
    )
  )
)

## Export -----
tab_export <- tabPanel(
  "Export",
  sidebarLayout(
    sidebarPanel(
      textInput("exportargs", "Arguments")
    ),
    mainPanel(htmlOutput("export_html"))
  )
)


ui <- navbarPage(
  "Shiny scan",
  #theme = shinythemes::shinytheme("superhero"),
  #shinythemes::themeSelector(),
  tags$head(tags$style("#funcargs{color: red;
                             font-size: 20px;
                         font-style: italic;
                         }"
  )),
  tab_scdf,
  tab_transform,
  tab_stats,
  tab_export,
  tab_plot
)
