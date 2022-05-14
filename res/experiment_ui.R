##### Brief UI
output$brief_ui <- renderUI({
  if (session$userData$view == 1) {
    fluidPage(
      if (session$userData$brief_page == 1) {
        list(
          fluidRow(column(12,
            align = "left",
            h3("Brief"),
            p("Thank you for taking the time to participate in our study. For our study, we are interested in what you prefer when faced with possible health problems. This study is organized by a professor at McMaster University. All information is confidential and anonymous. The questions we ask today are opinions, there are no right or wrong answers. We expect that everyone will have different responses. We will use your responses to assign a value to health problems related to glaucoma. Initially you will learn about glaucoma, what it is, and how it is treated. You will learn about how losing vision impacts quality of life.")
          )),
          tags$li("This study investigates preferences"),
          tags$li("No right or wrong answers"),
          tags$li("You will learn about the impact of disease and treatment on patients with glaucoma"),
          br(),
          fluidRow(column(12, align = "center", actionButton("brief_next", "Next")))
        )
      }
      else if (session$userData$brief_page == 2) {
        list(
          fluidRow(column(12,
            align = "left",
            p("Over the next 15 to 25 minutes, you will be asked to make difficult choices. You will be asked to choose between a risky gamble or a certain health problem. The situations are made up, but the health problems are real. It is important that you take your time on each situation. "),
            br(),
            p("Click 'Next' to proceed to the consent form.")
          )),
          fluidRow(column(12, align = "center", actionButton("brief_next", "Next")))
        )
      }
      else if (session$userData$brief_page == 3) {
        list(
          useShinyalert(),
          fluidRow(column(12, align = "center", h1("<Consent Form>")
          )),
          fluidRow(column(12,
            align = "center",
            actionButton("consent", "I Consent"),
            actionButton("no_consent", "I do not consent")
          )),
          br(),
          br(),
          br(),
          br()
        )
      }
    )
  }
})

#### Reflective framing UI
output$reflect_ui <- renderUI({
  if (session$userData$view == 2) {
    fluidPage(
      # fluidRow(column(8, align = "center", offset = 2, h4("Study Task 1"))),
      if (session$userData$framing_page == 1) {
        fluidRow(
          column(12,
            align = "left",
            column(12, align = "center", HTML("<img src='vision_sim.gif'; alt='vision_loss_simulator'; width=400; style = 'border-radius: 200px; padding: 10px;'/>")),
            hr(),
            h4("What is Glaucoma?"),
            p("Glaucoma is a disease that occurs in one or both eyes. Over time, glaucoma causes permanent damage to how well you can see. Glaucoma tends to be a problem for people around 50 years old. Accidents where the eye or eye socket is damaged can also lead to glaucoma. Scientists and doctors are still learning about glaucoma. The damage to the eye cannot be fixed. Treatments for glaucoma help to prevent the eye from further damage. It is important to catch glaucoma early to reduce damage to the eye and prevent loss of sight. There are many types of glaucoma, the most common is open angle glaucoma. Progressive vision loss from glaucoma is related to increased pressure inside the eye. At every visit to an optometrist, your eye pressure is measured. If it is too high, you could be referred to a specialist eye doctor to check for glaucoma. When the inside of the eye has too much pressure, the optical nerve at the back of the eye is damaged. In most cases this results in losing vision from the outside of the eye to the middle of the eye. The animation shows what that could look like in one eye.")
          ),
          hr(),
          tags$li("No cure for glaucoma."),
          tags$li("Glaucoma usually occurs in adults over 50."),
          tags$li("Treatments prevent vision loss by reducing intraocular pressure."),
          tags$li("Vision loss progresses from peripheral to central."),
          br(),
          column(12, align = "center", actionButton("refl_submit", "Next")),
          br(),
          br(),
          br()
        )
      }
      else if (session$userData$framing_page == 2) {
        fluidRow(
          column(12,
            align = "left",
            h4("How is Glaucoma Treated?"),
            p("The first line of defense against glaucoma is pharmaceutical eye drops. These eye drops are prescribed by a specialist eye doctor. A patient being treated for glaucoma can take up to 5 different eye drops. The number of eye drops depends on the patient. More drops are prescribed if the pressure is not low enough. Patients may be required to use each of these drops once or twice a day. Patients with glaucoma can also be treated with a laser or open eye surgeries. These treatments are only available by highly specialized eye doctors. To have this treatment, the patient has to live near or travel to larger cities.")
          ),
          hr(),
          tags$li("First line of therapy is pharmaceutical eye drops"),
          tags$li("Many patients need to take multiple drops, multiple times a day."),
          tags$li("Second lines of therapy are laser or incisional surgeries."),
          br(),
          column(12, align = "center", actionButton("refl_submit", "Next"))
        )
      }
      else if (session$userData$framing_page == 3) {
        fluidRow(
          column(12,
            align = "left",
            h4("How does Glaucoma Impact Daily Life?"),
            p("We have found that patients with glaucoma are impacted on 5 broad areas of life."),
            p(
              tags$li("Visual Discomfort: sensitive to light, scratching or burning sensation, eye pain."),
              tags$li("Mobility: problems going up or down stairs, tripping or bumping while walking, missing hazards in peripheral vision."),
              tags$li("Daily Life Activities: challenges with close up work, frustration with household chores and errands, reading, and writing."),
              tags$li("Emotional Health: Anxiety relating to losing vision. Worried about being unable to work in the future and provide for their family. Depression related to losing vision."),
              tags$li("Social Activities: Frustration playing sports. Have to avoid social gatherings and crowded places. Embarrassed in front of others."),
              br()
            ),
            hr(),
            h4("Reflective Framing Exercise"),
            p("We are interested in your opinion of what living with glaucoma would mean for you in your future everyday life. Please take your time to answer thoughtfully."),
            wellPanel(
              tags$style(".form-group.shiny-input-container { width: 100%; }"),
              textAreaInput2("txt2", "What typical activities are important to you to maintain as you age? (list 3 activities)", width = "100%", height = "100%")
            ),
            wellPanel(
              tags$style(".form-group.shiny-input-container { width: 100%; }"),
              textAreaInput2("txt3", "Take a moment to think about each of the areas we know are affected by patients living with glaucoma (Visual Discomfort, Mobility, Daily Life Activities, Emotional Health, and Social Activities). What area would be most difficult for you to adapt to? Why? (2 brief sentences)", width = "100%", height = "100%")
            ),
            wellPanel(tags$style(".form-group.shiny-input-container { width: 100%; }"), selectInput("txt4", "How would you rank each area of quality of life from easiest to deal with to most difficult? (select in order from easiest to most difficult)", multiple = TRUE, choices = sample(c("Visual Discomfort", "Mobility", "Daily Life Activities", "Emotional Health", "Social Activities"), 5, replace = F)))
          ),
          br(),
          column(12, align = "center", actionButton("refl_submit", "Submit"))
        )
      }
    )
  }
})

output$vas_ui <- renderUI({
  if (session$userData$view == 3) {
    fluidPage(
      useShinyalert(),
      if (session$userData$vas_page == 1) {
        tagList(
          fluidRow(column(12,
                          align = "left",
                          h4("Visual Analogue Task"),
                          p("For the next few tasks, we ask you to assign values to health problems"),
                          p("First, you be asked to choose which is worse, the",tags$b("Worst Possible Glaucoma"), " health state or ", tags$b("Death"), ". This will define the order of your next few choices, so choose carefully!"),
                          p("Next, you will be shown a ruler with a maximum value of 100 and a minimum value of 0. The ruler will be used to measure how you value each health problem. Health problems will be described on the left. Health problems will have 4 levels of severity: ", tags$b("slight, moderate, very much or severe."),  " Groups of the same health problems that change in severity will stay on the ruler until you finish each group. If health problems are so similar that they should be equal in value, please place the health problem directly on top of the similar health problem."),
                          p("Click 'Begin' to start this task."))),
          br(),
          fluidRow(column(12, align = "center", actionButton("start_vas", "Begin")))
        )
      }
      else if (session$userData$vas_page == 2) {
        tagList(
          fluidRow(column(12, align = "center",
                          p("Which option would you prefer"))),
          fluidRow(
            column(6,
                   align = "center",
                   wellPanel(
                     style = "height:420px;",
                     h1("A"),
                     h4("Worst Possible Glaucoma"),
                     state_table(c(5, 5, 5, 5, 5)), h4("Live for 10 years in this state then die")
                   ),
                   actionButton("choose_pitts", "Choose A")
            ),
            column(6,
                   align = "center",
                   wellPanel(
                     style = "height:420px;", 
                     h1("B"),
                     h4("Immediate death"),
                     column(12, align = "center", HTML("<img src='skull.png'; alt='skull'; width=200; style = 'border-radius: 200px; padding: 10px; color: #000000;'/>"))
                   ),
                   actionButton("choose_death", "Choose B")
            )
          )
        )
      }
      else if (session$userData$vas_page == 3) {
        tagList(
          fluidRow(column(12,
                          align = "left",
                          h4("Visual Analogue Task"),
                          p("Drag the white square and press confirm when the marker is at a value appropriate for the description"))),
        hr(),
        # fixedRow(
        #     ),
        fixedRow(
          column(12, align = "center",
                 uiOutput("thermometer_question")
                 
          )
        ),
        fixedRow(
          # column(1, align = "right",
          #        actionButton("confirm_value", "Confirm")),
          column(12,
                 align = "center",
                 sliderInput("the_slider", "",
                             min = 0, max = 100,
                             value = 50, step = 1, width = "600px"),
                 tags$pre(h4("Death \t \t \t \t \t \t \t \t \t \t \t \t \t Perfect Health")),
                 actionButton("confirm_value", "Select")
                 )
          # column(4),
          # column(2,
          #        align = "right",
          #        br(), br(), 
          #        noUiSliderInput(
          #          inputId = "the_slider",
          #          label = NULL,
          #          min = 0,
          #          max = 100,
          #          orientation = "vertical",
          #          width = "0px",
          #          height = "315px",
          #          direction = "rtl",
          #          inline = F,
          #          tooltips = T,
          #          value = c(100),
          #          step = 1,
          #          behaviour = c("snap"),
          #          color = "green"
          #        )),
          # column(6,
          #        align = "left",
          #        imageOutput("thermometer")
          #        # HTML("<img src='thermometer.png'; alt='thermometer';  style = padding: 40px; '/>")
          # )
          
          #imageOutput("reindeer")
        ))
      }
    )
  }
})

 

#### Standard Gamble UI
output$sg_ui <- renderUI({
  if (session$userData$view == 4) {
    fluidPage(
      useShinyalert(),
      if (session$userData$experiment_page == 1) {
        tagList(
          fluidRow(column(12,
            align = "left",
            h4("Standard Gamble Task"),
            p("For our next task, we would like for you to again consider the choices as if you are 70 years old. You will be shown three different descriptions of health and asked to decide between two choices. If you think the choices are", tags$b("equal"), ", select 'A and B About the same'. If A and B are", tags$b("not equal"), "the risk will change before asking you to choose again. The risk will change until we know the chance of death you would accept to avoid living with that health problem. There are no right or wrong answers."),
            p(tags$b("Choice A "), "is a gamble with a risk of death and perfect health, for you at 70 years old. Think about this choice as a miracle surgery that has a chance to restore you to perfect health (chance of perfect health). The only other possible situation is that you die, sedated on the operating table (chance of immediate death). Choice A is illustrated with a pie chart showing the percent (%) chance of perfect health or death. For the case of 90% chance of perfect health, 90 of 100 people would live their remaining 10 years perfectly healthy after treatment but 10 will die during treatment. No one can know whether they will be one of the 90 or one of the 10. The perfect health will last for your remaining 10 years of life."),
            p(tags$b("Choice B "), "is a guaranteed health problem related to glaucoma at 70 years old. This choice has 100% chance of occurring and persisting for your remaining 10 years of life."),
            hr(),
            tags$li("Pretend you are 70 years old and expected to live for 10 more years..."),
            tags$li("You have glaucoma and it is progressing, it is starting to affect your everyday life."),
            tags$li("A surgery is available that can give you a chance of perfect health for your last 10 years."),
            tags$li("At what point are you willing to risk death to have a chance of perfect health?"),
            hr(),
            tags$li(tags$b("Choose A for the surgery, with a chance of perfect health and death.")),
            tags$li(tags$b("Choose B for living the outlined health concerns for your last 10 years.")),
            tags$li(tags$b("Choose 'About the Same' if the options are equal for you.")),
            hr(),
            p("If you are ready, click 'Begin' to start the choice task.")
          )),
          fluidRow(column(12, align = "center", actionButton("start_exp", "Begin")))
        )
      }
      else if (session$userData$experiment_page == 2) {
        tagList(
          fluidRow(column(12, align = "center", p("Choose your preferred alternative or state they are about equal."))),
          fluidRow(
            column(6,
              align = "center",
              wellPanel(
                style = "height:420px;",
                h2("Novel Treatment"),
                plotOutput("circle", height = 200, width = 577),
                br(),
                h5(paste0(session$userData$A_alts[1], "% Chance of Perfect Health for 10 Years")),
                h5(paste0(session$userData$A_alts[2], "% Chance of Immediate Death"))
              ),
              actionButton("choose_a", "Choose A")
            ),
            column(6,
              align = "center",
              wellPanel(
                style = "height:420px;",
                h2("Certain Health State"),
                suppressWarnings(state_table(session$userData$current_state)),
                br(),
                h5("Living for 10 years in this state from 70 years old.")
              ),
              actionButton("choose_b", "Choose B")
            )
          ),
          fluidRow(
            column(12,
              align = "center",
              actionButton("indifferent", "About the same")
            )
          )
        )
      }
      else if (session$userData$experiment_page == 3) {
        tagList(
          fluidRow(column(12, align = "center", p("Thank you for your responses so far. The next question will ask you to make a very difficult choice between the worst possible state of living with glaucoma and death. One alternative will be certain and another will have a chance of a surgery fully restoring your health"))),
          fluidRow(column(12, align = "center", actionButton("wtd_next", "Next")))
        )
      }
    )
  }
})

#### Worse than dead UI
output$wtd_ui <- renderUI({
  if (session$userData$view == 5) {
    fluidPage(
      useShinyalert(),
        tagList(
          fluidRow(column(12, align = "center", p("Choose your preferred alternative or state they are about equal."))),
          fluidRow(
            column(6,
                   align = "center",
                   wellPanel(
                     style = "height:420px;",
                     h2("Novel Treatment"),
                     plotOutput("circle", height = 200, width = 577),
                     br(),
                     h5(paste0(session$userData$A_alts[1], "% Chance of Perfect Health for 10 Years at 70")),
                     h5(
                       if (session$userData$anchor == "55555") {
                         paste0(session$userData$A_alts[2], "% Chance of WORST POSSIBLE GLAUCOMA (Pits) at 70")
                       }
                       else if (session$userData$anchor == "99999") {
                         paste0(session$userData$A_alts[2], "% Chance of Immediate Death at 70")
                       }
                       )
                   ),
                   actionButton("choose_a", "Choose A")
            ),
            column(6,
                   align = "center",
                   wellPanel(
                     style = "height:420px;",
                     h2("Certain Health State"),
                     if (session$userData$anchor == "99999") {
                       tagList(suppressWarnings(state_table(c(5, 5, 5, 5, 5))),
                               h5("Living for 10 years in this state from 70 years old."))
                     }
                     else if (session$userData$anchor == "55555") {
                       tagList(
                         h4("Immediate death"),
                         br(),
                         column(12, align = "center", HTML("<img src='skull.png'; alt='skull'; width=200; style = 'border-radius: 200px; padding: 10px; color: #000000;'/>"))
                       )
                     }
                    ),
                   actionButton("choose_b", "Choose B")
                   )
            )
          ),
          fluidRow(
            column(12,
                   align = "center",
                   actionButton("indifferent", "About the same")
            )
          )
        )
    
  }
})

#### Survey UI
jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) { window.location = message;});"
output$survey_ui <- renderUI({
  if (session$userData$view == 6) {
    fluidPage(fluidRow(column(12,
      align = "center",
      tags$head(tags$script(jscode)),
      if (session$userData$q == 0) {
        tagList(
          wellPanel(
            tags$style(".form-group.shiny-input-container { width: 100%; }"),
            textAreaInput2("feedback", label = "Please provide any feedback on the experiment, do you believe you could have used more information or support?", width = "100%", height = "100%")
          ),
          hr(),
          p("Finally, please take your time responding to a few questions about you.", align = "center"),
          br(),
          column(12, align = "center", actionButton("begin", "Continue"))
        )
      }
      else if (session$userData$q == 1) {
        tagList(
          column(2,
            offset = 5, align = "left",
            tags$style("#in1 {font-size:15px;height:50px;padding:4px; text-algin:left; width: 100%;}"),
            numericInput("in1", label = "How old are you? \n", value = 18, min = 18, max = 100),
            actionButton("select_resp1", "Select")
          )
        )
      }
      else if (session$userData$q == 2) {
        tagList(
          column(2,
            offset = 5, align = "left",
            textInput("in1", label = "What is the postal code of your current address? \n", value = ""),
            actionButton("select_resp1", "Select")
          )
        )
      }
      else if (session$userData$q > 2 & session$userData$q <= sum(length(questions[, 1]) + 2)) {
        tagList(
          column(2,
            offset = 5, align = "left",
            tags$style("width:400px;"),
            prettyRadioButtons("survey", textOutput("question"), as.character(option.list()), animation = "smooth"),
            actionButton("select_resp", "Select")
          )
        )
      }
    )))
  }
})
