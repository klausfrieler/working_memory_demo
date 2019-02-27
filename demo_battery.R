library(psychTestR)
library(psychTestRCAT)
library(JAJ)
library(mdt)
library(RAT)
library(ggplot2)

MDT_feedback_graph_normal_curve <- function(perc_correct, x_min = 40, x_max = 160, x_mean = 100, x_sd = 15) {
  q <-
    ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = dnorm, args = list(mean = x_mean, sd = x_sd)) +
    ggplot2::stat_function(fun = dnorm, args=list(mean = x_mean, sd = x_sd),
                           xlim = c(x_min, (x_max - x_min) * perc_correct + 40),
                           fill = "lightblue4",
                           geom = "area")
  q <- q + ggplot2::theme_bw()
  #q <- q + scale_y_continuous(labels = scales::percent, name="Frequency (%)")
  q <- q + ggplot2::scale_y_continuous(labels = NULL)
  x_axis_lab <- "RAT Score"
  title <- "Your RAT score"
  fake_IQ <- 100 * perc_correct + 37.5 #(15/24 -> 100)
  main_title <- sprintf("%s: %.0f", title, round(fake_IQ, digits = 0))
  
  q <- q + ggplot2::labs(x = x_axis_lab, y = "")
  q <- q + ggplot2::ggtitle(main_title)
  plotly::ggplotly(q,
                   width = 300,
                   height = 300)
}

MDT_feedback_with_graph <- function() {
  psychTestR::new_timeline(
    c(
      psychTestR::reactive_page(function(state, ...) {
        results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = FALSE)
        results <- attr(as.list(results)$MDT$ability, "metadata")$results
        print(results)
        print(nrow(results))
        
        perc_correct <- (results$ability_WL[nrow(results)] + 2)/4
        sum_score <- sum(results$score)
        num_question <- nrow(results)
        #perc_correct <- sum_score/num_question
        #printf("Sum scores: %d, total items: %d perc_correct: %.2f", sum_score, num_question, perc_correct)
        text_finish <- sprintf("You answered %d out of %d questions correctly", sum_score, num_question)
        norm_plot <- MDT_feedback_graph_normal_curve(perc_correct)
        psychTestR::page(
          ui = shiny::div(
            shiny::p(text_finish),
            norm_plot,
            shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("AMDI_0016_I_0001_1")))
          )
        )
      }
      )),
    dict = mdt::mdt_dict
  )
  
}

num_items <- 1L
take_training <- F
demo <- F

elts <- 
  list(
  text_input_page(
    label = "name", 
    prompt = "What's your name?", 
    validate = function(answer, ...) {
      if (answer == "")
        "Name cannot be left blank."
      else TRUE
    },
    on_complete = function(answer, state, ...) {
      set_global(key = "name", value = answer,
                 state = state)
    }),
  mdt(num_items = num_items, take_training = take_training, feedback = MDT_feedback_with_graph()),
  RAT(num_items = num_items, take_training = take_training, feedback = RAT::RAT_feedback_with_graph()),
  JAJ(num_items = num_items, take_training = take_training, feedback = JAJ::JAJ_feedback_with_graph()),
  elt_save_results_to_disk(complete = TRUE),
  reactive_page(
    function(state, ...) {
      final_page(sprintf("Thank you for participating, %s.", get_global("name", state)))
    }
  ))

run_demo <- function(){
  make_test(
    elts, 
    opt = psychTestR::test_options(title = "Working Memory Tests Demo",
                                   admin_password = "conifer",
                                   researcher_email = "longgold@gold.uc.ak",
                                   demo = demo,
                                   languages = "EN")
  )
}