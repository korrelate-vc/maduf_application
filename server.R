require(shiny)
require(shinyWidgets)
require(dplyr)
require(kableExtra)
require(formattable)
require(condformat)
require(lubridate)
require(dbplyr)
require(sodium)
require(shinydashboard)
require(shinyjs)
require(ggplot2)
require(shinyalert)
require(ggforce)
require(gridExtra)
require(scales)
library(shinyBS)
library(DT)
library(zoo)
library(chron)
library(ids)
library(waffle)
library(gganimate)
library(magick)
library(likert)
library(draw)
library(DBI)
library(promises)
library(future)
plan(multiprocess)

shinyServer(function(input, output, session) {
  USER <- reactiveValues(
    Logged = FALSE,
    session = session$user
  )
  
  sleep_time <- 0
  
  questions <- read.csv("dat/questions.csv")
  dce_dim_level_text <- read.csv("dat/dce_dim_level_text.csv")
  sequence3 <- read.csv("dat/thermometer_sequence.csv")
  
  t_0 <- Sys.time()

  #source("res/write_dat.R", local = TRUE)
  source("res/experiment.R", local = TRUE)
  source("res/experiment_ui.R", local = TRUE)
  
  
  session$userData <- reactiveValues(
    view = 1,
    seq = 1,
    seq2 = 1,
    oth_times = c("", ""),
    q = 0,
    survey_response = vector(),
    user_pos = 0,
    iter = 1,
    value = "",
    vas_response = vector(),
    sg_response = vector(),
    current_state = vector(),
    seq2 = 1,
    A_alts = c(100, 0),
    circle = NULL,
    experiment_times = vector(),
    states_vector = NA,
    brief = TRUE,
    brief_page = 1,
    framing_page = 1,
    experiment_page = 1,
    txt_resp = vector(),
    progress = 1,
    resp_id = "",
    vas_page = 1,
    value1 = NA,
    thermometer_image = image_read(paste0("www/", "thermometer2.png")),
    tmpfile = NA, 
    base = NA,
    seq_pos = 1,
    anchor = NA,
    vas_sequence = read.csv("dat/thermometer_sequence.csv"),
    sg_sequence = read.csv("dat/sequence.csv"),
    states_sequence = read.csv("dat/states.csv"),
    end_picks = F
  )
  
  onStop(function(){
      vas_response <- isolate(session$userData$vas_response)
      resp_id <- isolate(session$userData$resp_id)
      survey_response <- c(as.character(Sys.time()),isolate(session$userData$survey_response))
      text_resp <- isolate(session$userData$text_resp)
      states_vector <- isolate(session$userData$states_vector)
      sg_response <- isolate(session$userData$sg_response)
      experiment_times <- isolate(session$userData$experiment_times)
      
      add_na <- function(v1, expect) {
        actual <- length(v1)
        diff <- expect - actual
        if (diff > 0) {
          v1 <- c(v1, rep(" ", diff))
        }
        return(v1)
      }
      
      sg_response <- add_na(sg_response, 12)
      vas_response <- add_na(vas_response, 20)
      text_resp <- add_na(text_resp, 4)
      states_vector <- add_na(states_vector, 32)
      experiment_times <- add_na(experiment_times, 32)
      survey_response <- add_na(survey_response, 29)
      
      compile_dat <- as.data.frame(t(c(
        resp_id,
        survey_response,
        text_resp, 
        states_vector,
        vas_response,
        sg_response,
        experiment_times
      )))
      
      #warning(paste0(compile_dat, collapse = ", "))

      # compile_dat <- jsonlite::toJSON(list(
      #   "resp_id" = isolate(session$userData$resp_id),
      #   "durations" = isolate(session$userData$experiment_times),
      #   "survey_response" = c(as.character(Sys.time()),isolate(session$userData$survey_response)),
      #   "text_resp" = isolate(session$userData$text_resp),
      #   "experiment_response" = isolate(session$userData$experiment_response),
      #   "states_vector" = isolate(session$userData$states_vector),
      #   "end_early" = TRUE
      # ))
      
      #Change to your database (this will add noSQL object)
      # conn <- mongolite::mongo(
      #   collection = "collection",
      #   db = "db",
      #   url = paste0("mongodb://user:", "password", "@ip_address:27017/user")
      # )

      # conn$insert(compile_dat)

      # conn$disconnect()
   
  },session = session)
  
})
