#### Login
####

observeEvent(input$start_vas, {
  session$userData$vas_page <- session$userData$vas_page + 1
  #shinyalert("", "Next you will choose which option is PREFERRED, the better outcome of the two.", type = "info")
})

#### SG
#### Functions

state_randomization <- function() {
  session$userData$states_sequence[2:6,] <- session$userData$states_sequence[sample(2:6, 5),]
  session$userData$states_sequence[7:11,] <- session$userData$states_sequence[sample(7:11, 5),]

  vas_rows <- list(1:4, 5:8, 9:12, 13:16, 17:20)
  vas_order <- sample(1:5, 5)
  vas_vec <- c(vas_rows[[vas_order[1]]], vas_rows[[vas_order[2]]], vas_rows[[vas_order[3]]], vas_rows[[vas_order[4]]], vas_rows[[vas_order[5]]])
  session$userData$vas_sequence[1:20,] <- session$userData$vas_sequence[vas_vec,]
  session$userData$states_vector <- c(session$userData$vas_sequence$state,session$userData$states_sequence$state)
  
  if (session$userData$anchor == "55555") {
    session$userData$states_vector <- c(session$userData$states_vector, "99999")
  } else if (session$userData$anchor == "99999") {
    session$userData$states_vector <- c(session$userData$states_vector, "55555")
  }
}

state_table <- function(vec1) {
  state1 <- as.data.frame(matrix(nrow = 5, ncol = 3))
  colnames(state1) <- c("dim1", "level1", "desc1")
  rownames(state1) <- c(
    "Visual Discomfort", # <span class= 'tooltiptext'>(e.g., uneasy feeling in the eye(s), pain, and no difficulty seeing in darkness)</span>
    "Mobility", # <br> <font size = '-2'> (e.g., feeling cautious when driving or biking, difficulty going up or downstairs, incidents of tripping or bumping when walking) </font>
    "Daily Living", # <br> <font size = '-2'> (e.g., difficulty with any close-up work, household chores or errands) </font>
    "Emotional Well-Being", # <br> <font size = '-2'> (e.g., frustrated with symptoms or treatment, disturbed by frequent thoughts, troubled by worries or fears) </font>
    "Social Discomfort" # <br> <font size = '-2'> (e.g., missing out on things, difficulty playing sports, discomfort attending social gatherings or crowded places, incidents of social embarrassment) </font>
  )
  state1[, 1] <- c(1:5)
  state1[, 2] <- vec1
  for (i in 1:5) {
    state1[i, 3] <- as.character(dce_dim_level_text[which(state1[i, 1] == dce_dim_level_text[, 1] & state1[i, 2] == dce_dim_level_text[, 2]), 3])
  }
  d <- state1[1:5, ] %>%
    mutate(
      Dimension = row.names(.),
      state1 = ifelse(level1 == 1, "",
        ifelse(level1 == 2, "#E5E6FA",
          ifelse(level1 == 3,
            "#E3C4F4",
            ifelse(level1 == 4,
              "#DDA0DC",
              ifelse(level1 == 5,
                "#EE82EF", "Error"
              )
            )
          )
        )
      )
    )
  states <- cbind(rownames(state1), state1[, 3])
  states <- as.data.frame(states)
  colnames(states) <- c(" ", "State")
  cell_color1 <- formatter("td",
    style = x ~ style("color" = ifelse(d[, 5] == "", "#A9A9A9", "black"), "background-color" = d[, 5], "padding-right" = "8px", "padding-left" = "8px")
  )
  f <- formattable(states, list(`State` = cell_color1), format = "pandoc")
  f <- as.htmlwidget(f)
  f <- gsub("&lt;b&gt;", "<b>", f$x)
  f <- gsub("&lt;/b&gt;", "</b>", f)
  f <- gsub("State", "", f)
  f <- HTML(f)
  return(f)
}

experiment_progress <- function(count) {
  max1 <- 70
  par(mar = c(0, 0, 0, 0))
  progress_plot <- plot.new() %>%
    rect(xleft = 0, ybottom = 0, xright = 1, ytop = 1, border = "blue2") %>%
    rect(xleft = 0, ybottom = 0, xright = count / max1, ytop = 1, col = "blue2")
  return(progress_plot)
}

output$progress_plot <- renderPlot({
  experiment_progress(session$userData$progress)
})

plot_waffle <- function() {
  library(waffle)
  x <- c(`Perflect Health` = session$userData$A_alts[1], Death = session$userData$A_alts[2])
  
  if (session$userData$anchor == "99999" | (session$userData$anchor == "55555" & session$userData$end_picks == F)) {
    x <- c(`Perflect Health` = session$userData$A_alts[1], Death = session$userData$A_alts[2])
  } else if (session$userData$anchor == "55555" & session$userData$end_picks == T){
    x <- c(`Perflect Health` = session$userData$A_alts[1], `Worst Glaucoma` = session$userData$A_alts[2])
  }
  
  return(waffle(x, rows = 5, 
         colors = c("green", "black"),
         legend_pos = "bottom") + theme(legend.text = element_text(size=14)))
}

plot_circle <- function(death = T) {
  val_life <- session$userData$A_alts[1]
  val_blind <- session$userData$A_alts[2]

  circle_df <- as.data.frame(matrix(NA, nrow = 2, ncol = 2))
  
  if (session$userData$anchor == "99999" | (session$userData$anchor == "55555" & session$userData$end_picks == F)) {
    circle_df[, 1] <- c("Life", "Death")
    my_cols <- c("#EE82EF", "Green")
  } else if (session$userData$anchor == "55555" & session$userData$end_picks == T){
    circle_df[, 1] <- c("Life", "PITS")
    my_cols <- c("Green","#EE82EF")
  }

  circle_df[, 2] <- c(val_life, val_blind)
  colnames(circle_df) <- c("group", "value")

  start1 <- 0
  if (val_blind == 0) {
    circle_df <- circle_df[-2, ]
    my_cols <- c("Green")
  }
  else if (val_blind == 0.5) {
    circle_df[, 1] <- c("", "")
  }
  if (val_life == 0) {
    circle_df <- circle_df[-1, ]
    my_cols <- c("#EE82EF")
  }
  else if (val_life == 0.5) {
    circle_df[, 1] <- c("", "")
  }

  circle_df <- circle_df %>%
    #arrange(desc(group)) %>%
    mutate(lab.ypos = cumsum(value) - (0.5 * value - 1))

  p <- ggplot(circle_df, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y", start = start1) +
    scale_fill_manual(values = my_cols) +
    geom_text(aes(y = lab.ypos, label = group), color = "black", size = 5.25) +
    blank_theme +
    theme(axis.text.x = element_blank()) +
    guides(fill = "none") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  return(p)
}

textAreaInput2 <- function(inputId, label, value = "", width = NULL, height = NULL,
                           cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) {
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c(
      "both", "none", "vertical",
      "horizontal"
    ))
  }
  style <- paste("max-width: 100%;", if (!is.null(width)) {
    paste0("width: ", validateCssUnit(width), ";")
  }, if (!is.null(height)) {
    paste0("height: ", validateCssUnit(height), ";")
  }, if (!is.null(resize)) {
    paste0("resize: ", resize, ";")
  })
  if (length(style) == 0) {
    style <- NULL
  }
  div(
    class = "form-group",
    tags$label(label, `for` = inputId), tags$textarea(
      id = inputId,
      class = "form-control", placeholder = placeholder, style = style,
      rows = rows, cols = cols, value
    )
  )
}

update_probs <- function() {
  v1 <- session$userData$sg_sequence[which(session$userData$sg_sequence$seq == session$userData$seq2), 2]
  v2 <- session$userData$sg_sequence[which(session$userData$sg_sequence$seq == session$userData$seq2), 3]
  out <- c(v1, v2)
  return(out)
}

current_state <- function(seq) {
  out <- as.numeric(session$userData$states_sequence[seq, c(3:7)])
  return(out)
}

next_state <- function() {
  if (session$userData$seq <= length(session$userData$states_sequence[, 1])) {
    session$userData$seq <- session$userData$seq + 1
    session$userData$progress <- session$userData$progress + 1
    session$userData$current_state <- as.numeric(session$userData$states_sequence[session$userData$seq, c(3:7)])
    session$userData$seq2 <- 1
    session$userData$A_alts <- update_probs()
    session$userData$circle <- plot_waffle()
  }
  # if (session$userData$seq < length(session$userData$states_sequence[,1])) {
  #   shinyalert("Next State", "Please look over choice B, as some attributes have changed.", type = "info")
  # }
  if (all(session$userData$current_state %in% 5) & session$userData$anchor == "99999") {
    session$userData$view <- session$userData$view + 1
    session$userData$progress <- session$userData$progress + 1
  } else if(all(session$userData$current_state %in% 5) & session$userData$anchor == "55555") {
    session$userData$view <- session$userData$view + 1
    session$userData$progress <- session$userData$progress + 1
  } else if (session$userData$seq == sum(length(session$userData$states_sequence[,1]), 1)) {
    session$userData$experiment_page <- session$userData$experiment_page + 1
    session$userData$progress <- session$userData$progress + 1
  }
}

check_response <- function(choice) {
  if (choice == "A") {
    val <- session$userData$sg_sequence$A[which(session$userData$sg_sequence$seq == session$userData$seq2)]
    cont <- TRUE
  }
  else if (choice == "B") {
    val <- session$userData$sg_sequence$B[which(session$userData$sg_sequence$seq == session$userData$seq2)]
    cont <- TRUE
  }
  else if (choice == "indifferent") {
    cont <- FALSE
    shinyalert("Are you sure?", paste0("If living in health state B is worth the same as risking: \n \n", session$userData$A_alts[2], "% chance of immediate death, \n \n type 'next' - if not, type 'back'."),
      type = "input",
      callbackR = function(value) {
        if (value == "next") {
          val <- session$userData$sg_sequence$same[which(session$userData$sg_sequence$seq == session$userData$seq2)]
          cont <- TRUE
          
          session$userData$sg_response <- c(session$userData$sg_response, val)
          session$userData$experiment_times <- c(session$userData$experiment_times, as.numeric(Sys.time()))
          next_state()
          return(val)
        }
        else if (value == "back") {
          return()
        }
      }
    )
  }
  if (cont == TRUE) {
    if (val == "continue") {
      session$userData$seq2 <- session$userData$seq2 + 1
      session$userData$A_alts <- update_probs()
      session$userData$circle <- plot_waffle()
    }
    else if (val == "prompt1") {
      shinyalert("", "It looks like you think it would be better to have some problems despite a 100% chance of no problems - please revisit this question and try again.", type = "error")
    }
    else if (val == "prompt2") {
      shinyalert("", "Do you mean you would prefer to have a 90% chance of dying immediately and a 10% chance of perfect health rather than living in the state described in choice B? (Y/N)",
        type = "input",
        callbackR = function(value) {
          if (value == "Y") {
            shinyalert(text = "What percent chance of the worst possible state in choice A would you accept to avoid choice B? (enter a number from 90 to 100)",
              type = "input",
              callbackR = function(value) {
                if (is.numeric(value) == T | is.numeric(value) == F) {
                  session$userData$sg_response <- c(session$userData$sg_response, value)
                  session$userData$experiment_times <- c(session$userData$experiment_times, as.numeric(Sys.time()))
                  next_state()
                } else {
                  return()
                }
              }
            )
          }
          if (value == "N") {
            return()
          }
        }
      )
    }

    else if ( val >= 0) {
      #session$userData$states_vector[session$userData$seq] <- paste(session$userData$current_state, sep = "", collapse = "")
      session$userData$sg_response <- c(session$userData$sg_response, val)
      session$userData$experiment_times <- c(session$userData$experiment_times, as.numeric(Sys.time()))
      next_state()
      return(val)

    }
    
  }
}

blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

observeEvent(input$no_consent, {
  shinyalert(
    title = "End Survey", text = "As you do not wish to participate in this study, please return your submission on Prolific by selecting the 'Stop without completing' button. \n \n Click 'Ok' to be redirected to Prolific.", type = "info", showCancelButton = T, showConfirmButton = T,
    callbackR = function() {
      url <- "www.google.ca" # /submissions/complete?cc=4FE050AC
      browseURL(url,
        browser = getOption("browser"),
        encodeIfNeeded = FALSE
      )
      stopApp()
    }
  )
})

#### Brief React
observeEvent(input$consent, {

  session$userData$view <- session$userData$view + 1

})

observeEvent(input$brief_next, {
  session$userData$progress <- session$userData$progress + 1
  session$userData$brief_page <- session$userData$brief_page + 1
})

#### Reflective React
observeEvent(input$refl_submit, {
  session$userData$progress <- session$userData$progress + 1
  session$userData$framing_page <- session$userData$framing_page + 1
  if (session$userData$framing_page == 4) {
    session$userData$view <- session$userData$view + 1
    session$userData$text_resp <- c(session$userData$resp_id, input$txt2, input$txt3, paste0(input$txt4, collapse = ", "))
  }
})

## VAS React

observeEvent(input$choose_death, {
  session$userData$anchor <- "55555"
  v1 <- session$userData$vas_sequence[2,1:2]
  session$userData$vas_sequence[2,1:2] <- session$userData$vas_sequence[3,1:2]
  session$userData$vas_sequence[3,1:2] <- v1
  state_randomization()
  session$userData$progress <- session$userData$progress + 1
  session$userData$vas_page <- session$userData$vas_page + 1
  # shinyalert(title = "",text = "Remember, there are no tricks to this. We are interested in how you value these possible health states. Respond intuitively. Don't think too hard about your answers. Answer with whatever comes to your mind naturally! Press 'Confirm' to assign a value to a health state state." , type = "info", size = "l")
})

observeEvent(input$choose_pitts, {
  session$userData$anchor <- "99999"
  state_randomization()
  session$userData$progress <- session$userData$progress + 1
  session$userData$vas_page <- session$userData$vas_page + 1
  # shinyalert(title = "",text = "Remember, there are no tricks to this. We are interested in how you value these possible health states. Respond intuitively. Don't think too hard about your answers. Answer with whatever comes to your mind naturally! Press 'Confirm' to assign a value to a health state state." , type = "info", size = "l")
})

# observeEvent(input$go_back_vas, {
#   session$userData$progress <- 10
#   session$userData$tmpfile <- NA
#   session$userData$seq_pos <- 1
#   session$userData$value1 <- NA
#   unlink(tempdir(), recursive = T)
#   session$userData$base <- NA
# })

### SG React
observeEvent(input$start_exp, {
  session$userData$progress <- session$userData$progress + 1
  session$userData$current_state <- as.numeric(session$userData$states_sequence[session$userData$seq, c(3:7)])
  session$userData$circle <- suppressWarnings(plot_waffle())
  session$userData$experiment_page <- session$userData$experiment_page + 1
  session$userData$oth_times[1] <- as.numeric(Sys.time())
})

observeEvent(input$choose_a, {
  Sys.sleep(sleep_time)
  session$userData$value <- check_response("A")
})

observeEvent(input$choose_b, {
  Sys.sleep(sleep_time)
  session$userData$value <- check_response("B")
})

observeEvent(input$indifferent, {
  Sys.sleep(sleep_time)
  session$userData$value <- check_response("indifferent")
})

observeEvent(input$wtd_next, {
  session$userData$progress <- session$userData$progress + 1
  session$userData$current_state <- c(5, 5, 5, 5, 5)
  session$userData$end_picks <- T
  session$userData$view <- session$userData$view + 1
})

output$circle <- renderPlot(
  {
    if (is.null(session$userData$circle)) {
      return()
    }
    par(mar = c(0, 0, 0, 0))
    session$userData$circle
  },
  bg = "transparent"
)

output$thermometer <- renderImage(
  {
    filename <- normalizePath(file.path("./www", paste0(session$userData$thermometer, ".png")))
    list(
      src = filename,
      alt = "thermometer"
    )
  },
  deleteFile = FALSE
)

output$slider_value <- renderText(input$the_slider)

#### Survey React
observeEvent(input$select_resp1, {
  session$userData$progress <- session$userData$progress + 1
  session$userData$survey_response[session$userData$q] <- input$in1
  session$userData$q <- session$userData$q + 1
})

convert_times <- function() {
  times <- vector()
  times[1] <- sum(as.numeric(session$userData$oth_times[1]) - as.numeric(t_0))
  for (i in 1:length(session$userData$experiment_times)) {
    if (i == 1) {
      times[sum(i + 1)] <- sum(as.numeric(session$userData$experiment_times[i]) - as.numeric(session$userData$oth_times[1]))
    }
    else {
      times[sum(i + 1)] <- sum(as.numeric(session$userData$experiment_times[i]) - as.numeric(session$userData$experiment_times[sum(i - 1)]))
    }
  }
  times[sum(length(times) + 1)] <- as.numeric(session$userData$oth_times[2]) - as.numeric(session$userData$oth_times[1])
  return(times)
}

observeEvent(input$select_resp, {
  session$userData$survey_response[session$userData$q] <- survey_reactive()
  session$userData$progress <- session$userData$progress + 1
  session$userData$q <- session$userData$q + 1
  if (session$userData$q > sum(length(questions[, 1]) + 2)) {

    session$userData$oth_times[2] <- as.numeric(Sys.time())
    durations <- convert_times()

    url <- "https://www.google.ca"
    session$sendCustomMessage("mymessage", url)
  }
})

option.list <- reactive({
  ans_len <- length(which(is.na(questions[sum(session$userData$q - 2), ]) == FALSE))
  qlist <- as.vector(setNames(questions[sum(session$userData$q - 2), 2:ans_len], NULL))
})

output$question <- renderText({
  paste0(
    questions[sum(session$userData$q - 2), 1]
  )
})

observeEvent(input$begin, {

  session$userData$q <- session$userData$q + 1
})

survey_reactive <- eventReactive(input$select_resp, {
  paste(input$survey, collapse = ", ")
})

### VAS Functions
place_words <- function(value = NA, attribute = "", tmpfile = NA) {
  
  attribute <- paste0("< ", attribute)
  
  if(!is.na(tmpfile)){
    thermometer = image_read(tmpfile)
  } else{
    thermometer = session$userData$thermometer_image
  }
  if(!is.na(value)){
    thermometer <- image_draw(thermometer)
    y_coord <- 45+((100 - as.numeric(value)) * 3.12)
    text(130, y_coord, attribute, family = "Times", cex = 1.25, srt = 0, adj = 0)
    dev.off()
  }
  
  # create a temp file
  tmpfile <- thermometer %>%
    image_convert(matte = T) %>%
    image_write(tempfile(fileext='png'), format = 'png')
  return(tmpfile)
}

output$thermometer_question <- renderUI({
  return(HTML(paste0("Please assign a value for: <b><br>",session$userData$vas_sequence[session$userData$seq_pos,2])))
})

observeEvent(input$confirm_value, {
  session$userData$progress <- session$userData$progress + 1
  session$userData$vas_response <- c(session$userData$vas_response, isolate(input$the_slider))
  session$userData$experiment_times <- c(session$userData$experiment_times, as.numeric(Sys.time()))
  if(!is.na(session$userData$tmpfile)){
    session$userData$thermometer_image <- image_read(session$userData$tmpfile)
  }
  session$userData$value1 <- isolate(input$the_slider)
  
  if (session$userData$seq_pos == 3) {
    session$userData$tmpfile <- place_words(value = session$userData$value1, attribute = session$userData$vas_sequence[session$userData$seq_pos,2], tmpfile = session$userData$tmpfile)
    session$userData$base <- session$userData$tmpfile
  } else if (session$userData$seq_pos %in% seq(4, 20, by = 4)) {
    session$userData$tmpfile <- place_words(value = session$userData$value1, attribute = session$userData$vas_sequence[session$userData$seq_pos,2], tmpfile = session$userData$base)
  } else{
    session$userData$tmpfile <- place_words(value = session$userData$value1, attribute = session$userData$vas_sequence[session$userData$seq_pos,2], tmpfile = session$userData$tmpfile)
  }
  session$userData$seq_pos <- session$userData$seq_pos + 1
  if(session$userData$seq_pos > nrow(session$userData$vas_sequence)){
    session$userData$view <- session$userData$view + 1
  }
  Sys.sleep(sleep_time)
  
  updateSliderInput(session = session, "the_slider", value = 50)

  # if(session$userData$seq_pos %in% seq(4, 20, by = 4)) {
  #   shinyalert("", "The next health state is a new health problem that is slightly impacted by glaucoma.", type = "info")
  # }
})

output$thermometer <- renderImage({
  if(is.na(session$userData$tmpfile)){
    out <- paste0("www/", "thermometer2.png")
  } else{
    out <- session$userData$tmpfile
  }
  return(list(src = out,
              alt = "thermometer-",
              contentType = "image/png"))
}, deleteFile = F)