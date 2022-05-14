
library(shiny)
library(DT)

#flatly simplex journal

shinyUI(
    fluidPage(theme = "readable.min.css",
              HTML("<title>HUG-5 Valuation: MADUF</title>"),
              column(10, offset = 1,
                     br(),br(),
                 fluidRow(column(12, align = "center",
                                 # img(src = "hei_hrm_logo2.png", align = "c", class = "responsive"),
                                  HTML("<h2 style='color:#3F596B'>HUG-5 Valuation: MADUF</h2>"),
                                  hr()
                 )),
                  tags$div(title="Progress",plotOutput("progress_plot", width = "100%", height = "20px"), align = "center", style = "
    position:relative;
    width:100%;
    height:50px; /* Height of the footer */
    color: white;
    padding: 5px;
    background-color: white;
    z-index: 1000;"
                  ),
                  tags$style(".checkbox-inline, .radio-inline, .text-area {
        text-align: left;
        margin-left: 0px;
        margin-right: 0px;
        padding: 0px;
    width: 100%;}"),
        #uiOutput('login_ui'),
                  uiOutput('dash_ui'),
                  uiOutput('brief_ui'),
                  uiOutput('reflect_ui'),
                  uiOutput('vas_ui'),
                  uiOutput('sg_ui'),
                  uiOutput('wtd_ui'),
                  uiOutput('survey_ui')
       ))
)
