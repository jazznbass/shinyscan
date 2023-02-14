
ui <- navbarPage(
  "Shiny scan",
  #theme = shinythemes::shinytheme("superhero"),
  #shinythemes::themeSelector(),
  tags$head(tags$style("#funcargs{color: red;
                             font-size: 20px;
                         font-style: italic;
                         }"
  )),
  tabPanel(
    "scdf",
    sidebarLayout(
      sidebarPanel(
        fileInput("upload", NULL, accept = c(".csv", ".rds"),
                  buttonLabel = "Load"),
        downloadButton("save", "Save active scdf"),
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

        textInput("select", "Select cases", value = ""),

        textInput("phase_A", "Select phase A", value = ""),

        textInput("phase_B", "Select phase B", value = ""),

        textOutput("startupmessage"),
      ), #end sidebarPanel

      mainPanel(
       #conditionalPanel(
       #   condition = "input.plot != 'none'",
       #   plotOutput("scdf")
       # ),
       #verbatimTextOutput("scdf_print"),
        verbatimTextOutput("scdf_syntax"),
        htmlOutput("scdf_html")
      )

    ) # end sidebarLayout

  ), # end tabPanel

  ## ---- plot
  tabPanel(
    "plot",
    sidebarLayout(
      sidebarPanel(
        selectInput(
         "plot", "Show plot",
         choices = resources$choices$fn_plot
        ),
      ),
      mainPanel(
       plotOutput("scdf", height = 800)
      )
    )
  ), # end tabpanel

  ## ---- stats
  tabPanel(
    "stats",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "func",
          "Statistic",
          choices = resources$choices$fn_stats
        ),
        textInput(
          "args",
          "Arguments"
        ),
        textOutput("funcargs"),
      ),
      mainPanel(
        verbatimTextOutput("statistic")
      )
    )
  ) # end tabpanel

)
