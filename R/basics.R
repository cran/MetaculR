user_agent <- httr::user_agent("https://gitlab.com/ntrlshrp/metaculr")

#' Retrieve questions from Metaculus API
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#' @param order_by Choose "last_prediction_time", "-activity", "-votes", "-publish_time", "close_time", "resolve_time", "last_prediction_time"
#' @param status Choose "all", "upcoming", "open", "closed", "resolved"
#' @param search Search term(s)
#' @param guessed_by Generally your Metaculus_user_id
#' @param offset Question offset
#' @param pages Number of pages to request
#'
#' @return A list of questions, ordered by last prediction time.
#' @export
#' @family Question Retrieval functions
#'
#' @examples
#' \dontrun{
#' questions_recent_open <-
#'   MetaculR_questions(
#'     order_by = "close_time",
#'     status = "open",
#'     guessed_by = "")
#' }

MetaculR_questions <- function(api_domain = "www", order_by = "last_prediction_time", status = "all", search = "", guessed_by = "", offset = 0, pages = 10) {
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = pages - 1, clear = FALSE, width= 60)
  pb$tick(0)
  #for (i in 1:100) {

  #}

  endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/questions/?")
  extra <- paste0("order_by=", order_by, "&status=", status, "&search=", search, "&guessed_by=", guessed_by, "&limit=20&offset=", offset)

  endpoint <- paste0(endpoint, extra)
  message(endpoint)

  get <- httr::GET(url = endpoint, user_agent)
  data = jsonlite::fromJSON(rawToChar(get$content))
  data_all <- list(data)

  page <- 1
  offset_base <- 20

  while(length(data$results$id) == 20) {
    pb$tick()
    Sys.sleep(0.5)
    #print(paste0(page, " "))

    endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/questions/?")
    extra <- paste0("order_by=", order_by, "&status=", status, "&search=", search, "&guessed_by=", guessed_by, "&limit=20&offset=", offset + offset_base)

    endpoint <- paste0(endpoint, extra)

    get <- httr::GET(url = endpoint, user_agent)
    data <- jsonlite::fromJSON(rawToChar(get$content))
    if(length(data$results) == 0) break
    data_all <- append(data_all, list(data))

    page <- page + 1
    offset_base <- page * 20

    if(page == pages) break
  }

  return(data_all)
}





#' Retrieve questions from Metaculus API (A wrapper for MetaculR_questions())
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#' @param order_by Default is "last_prediction_time"
#' @param status Choose "all", "upcoming", "open", "closed", "resolved"
#' @param search Search term(s)
#' @param guessed_by Generally your Metaculus_user_id
#' @param offset Question offset
#' @param pages Number of pages to request
#'
#' @return A list of questions that I've predicted, ordered by last prediction time.
#' @export
#' @family Question Retrieval functions
#'
#' @examples
#' \dontrun{
#' questions_myPredictions <-
#'   MetaculR_myPredictions(
#'     guessed_by = Metaculus_user_id)
#' }

MetaculR_myPredictions <- function(api_domain = "www", order_by = "last_prediction_time", status = "all", search = "", guessed_by = "", offset = 0, pages = 10) {
  data_all <- MetaculR_questions(api_domain = api_domain, order_by = order_by, status = status, search = search, guessed_by = guessed_by, offset = offset, pages = pages)

  return(data_all)
}





#' Retrieve questions from Metaculus API (A wrapper for MetaculR_questions())
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#' @param order_by Default is "-resolve_time"
#' @param status Default is "resolved"
#' @param search Search term(s)
#' @param guessed_by Generally your Metaculus_user_id
#' @param offset Question offset
#' @param pages Number of pages to request
#'
#' @return A list of questions that I've predicted, ordered by last prediction time, and resolved.
#' @export
#' @family Question Retrieval functions
#'
#' @examples
#' \dontrun{
#' questions_myPredictions_resolved <-
#'   MetaculR_myPredictions_Resolved(
#'     guessed_by = Metaculus_user_id)
#' }

MetaculR_myPredictions_Resolved <- function(api_domain = "www", order_by = "-resolve_time", status = "resolved", search = "", guessed_by = "", offset = 0, pages = 10) {
  data_all <- MetaculR_questions(api_domain = "www", order_by = order_by, status = status, search = "", guessed_by = guessed_by, offset = offset, pages = pages)

  return(data_all)
}





#' Login to Metaculus
#'
#' @param api_domain Use "www" unless you have a custom Metaculus domain
#'
#' @return Your Metaculus_user_ID.
#' @export
#'
#' @examples
#' \dontrun{
#' Metaculus_user_id <-
#'   MetaculR_login()
#' }

MetaculR_login <- function(api_domain = "www") {
  if(Sys.getenv("Metaculus_username") == "") {
    stop("No username in .Renviron!")
  }

  endpoint <- paste0("https://", api_domain, ".metaculus.com/api2/accounts/login/")

  response <- httr::POST(url = endpoint,
                         httr::accept_json(),
                         httr::content_type_json(),
                         body = jsonlite::toJSON(list(username = Sys.getenv("Metaculus_username"), password = Sys.getenv("Metaculus_password")),
                                                 auto_unbox = TRUE),
                         encode = "json")

  return(jsonlite::fromJSON(rawToChar(response$content)))
}





#' Calculate Brier statistics on MetaculR_questions object
#'
#' @param MetaculR_questions A MetaculR_questions object
#' @param me Show my scores alongside Metaculus scores
#' @param thresholds Thresholds to bin questions
#'
#' @return A list of Brier statistics for you and Metaculus.
#' \item{brier_me, brier_Metaculus}{}
#' \item{baseline.tf}{Logical indicator of whether climatology was provided.}
#' \item{bs}{Brier score}
#' \item{bs.baseline}{Brier Score for climatology}
#' \item{ss}{Skill score}
#' \item{bs.reliability}{Reliability portion of Brier score.}
#' \item{bs.resolution}{Resolution component of Brier score.}
#' \item{bs.uncert}{Uncertainty component of Brier score.}
#' \item{y.i}{Forecast bins -- described as the center value of the bins.}
#' \item{obar.i}{Observation bins -- described as the center value of the bins.}
#' \item{prob.y}{Proportion of time using each forecast.}
#' \item{obar}{Forecast based on climatology or average sample observations.}
#' \item{thresholds}{The thresholds for the forecast bins.}
#' \item{check}{ Reliability - resolution + uncertainty should equal brier score.}
#' \item{Other}{}
#' \item{ss_me_Metaculus}{Skill score, me vs. Metaculus.}
#' \item{count_questions}{Number of total questions included.}
#' \item{brier_df: Used for plotting Brier score statistics}{}
#' \item{ID}{Predictor.}
#' \item{name}{Name of value, see above.}
#' \item{value}{Value.}
#' \item{brier_bins_df: Used for plotting histogram and calibration plots.}{}
#' \item{ID}{Predictor.}
#' \item{centers}{y.i, see above.}
#' \item{freqs}{prob.y, see above.}
#' \item{obars}{obar.i, see above.}
#' \item{ideal}{Ideal calibration where centers equals obars.}
#' \item{ci_low}{Low end of 95% confidence interval for obar.i.}
#' \item{ci_high}{High end of 95% confidence interval for obar.i.}
#' @export
#'
#' @examples
#' \dontrun{
#' brier_me <-
#'   MetaculR_brier(
#'     questions_myPredictions_resolved)
#' }

MetaculR_brier <- function(MetaculR_questions, me = TRUE, thresholds = seq(0,1,0.1)) {
  ## no visible binding for global variable solution
  ID <- NULL

  my_predictions <- unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))], function(f) f$x[length(f$x)])}))

  if(is.null(my_predictions) |
     me == FALSE) {
    binary_questions <- data.frame(
      id = unlist(lapply(MetaculR_questions, function(x) x$results$id[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      observed = unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      metaculus_prediction = unlist(lapply(MetaculR_questions, function(x) x$results$metaculus_prediction$full[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))]))
    )

    brier_me <- NULL
    brier_Metaculus <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$metaculus_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)
    ss_me_Metaculus <- NULL

    brier_df <-
      data.frame(ID = c("Me", "Metaculus", "NA"),
                 bs = c(NA, brier_Metaculus$bs, NA),
                 bs.baseline = c(NA, brier_Metaculus$bs.baseline, NA),
                 bs.reliability = c(NA, brier_Metaculus$bs.reliability, NA),
                 bs.resolution = c(NA, brier_Metaculus$bs.resol, NA),
                 bs.uncertainty = c(NA, brier_Metaculus$bs.uncert, NA),
                 skill_baseline = c(NA, brier_Metaculus$ss, NA),
                 obar = c(NA, NA, brier_Metaculus$obar),
                 skill_me_Metaculus = c(NA, NA, NA)) %>%
      tidyr::pivot_longer(cols = -ID)

    bins_df <- data.frame(x = round(brier_Metaculus$obar.i * brier_Metaculus$prob.y * nrow(binary_questions), 0),
                          n = round(brier_Metaculus$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_bins_df <-
      data.frame(centers = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_Metaculus$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_Metaculus$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))
  } else {
    binary_questions <- data.frame(
      id = unlist(lapply(MetaculR_questions, function(x) x$results$id[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      observed = unlist(lapply(MetaculR_questions, function(x) x$results$resolution[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))])),
      metaculus_prediction = unlist(lapply(MetaculR_questions, function(x) x$results$metaculus_prediction$full[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))]))
      )

    binary_questions_me <- data.frame(
      id = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {x$results$id[which(x$results$possibilities$type == "binary" & !is.na(x$results$resolution) & x$results$resolution != -1 & !is.na(x$results$my_predictions$question) & unlist(lapply(x$results$metaculus_prediction$full, function(z) !is.null(z))))]})),
      my_prediction = my_predictions
      )

    binary_questions <- merge(binary_questions,
                              binary_questions_me)

    brier_me <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$my_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)
    brier_Metaculus <-
      verification::brier(
        obs = binary_questions$observed,
        pred = binary_questions$metaculus_prediction,
        thresholds = thresholds) ### c(0, exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) / (exp(seq(from = -4.59512, to = 4.59512, by = 0.919024)) + 1), 1)

    brier_df <-
      data.frame(ID = c("Me", "Metaculus", "NA"),
                 bs = c(brier_me$bs, brier_Metaculus$bs, NA),
                 bs.baseline = c(brier_me$bs.baseline, brier_Metaculus$bs.baseline, NA),
                 bs.reliability = c(brier_me$bs.reliability, brier_Metaculus$bs.reliability, NA),
                 bs.resolution = c(brier_me$bs.resol, brier_Metaculus$bs.resol, NA),
                 bs.uncertainty = c(brier_me$bs.uncert, brier_Metaculus$bs.uncert, NA),
                 skill_baseline = c(brier_me$ss, brier_Metaculus$ss, NA),
                 obar = c(NA, NA, brier_me$obar),
                 skill_me_Metaculus = c(NA, NA, 1 - brier_me$bs / brier_Metaculus$bs)) %>%
      tidyr::pivot_longer(cols = -ID)

    bins_df <- data.frame(x = round(brier_me$obar.i * brier_me$prob.y * nrow(binary_questions), 0),
                          n = round(brier_me$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_me_bins_df <-
      data.frame(ID = "Me",
                 centers = brier_me$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_me$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_me$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_me$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    bins_df <- data.frame(x = round(brier_Metaculus$obar.i * brier_Metaculus$prob.y * nrow(binary_questions), 0),
                          n = round(brier_Metaculus$prob.y * nrow(binary_questions), 0))

    df_binom.test <- apply(bins_df[which(!is.na(bins_df$x)), ],
                           MARGIN = 1,
                           FUN = function(z) stats::binom.test(z[1], z[2]))

    brier_Metaculus_bins_df <-
      data.frame(ID = "Metaculus",
                 centers = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 freqs = brier_Metaculus$prob.y[which(!is.na(bins_df$x))],
                 obars = brier_Metaculus$obar.i[which(!is.na(bins_df$x))],
                 ideal = brier_Metaculus$y.i[which(!is.na(bins_df$x))],
                 ci_low = unlist(lapply(df_binom.test, function(x) x$conf.int[1])),
                 ci_high = unlist(lapply(df_binom.test, function(x) x$conf.int[2])))

    brier_bins_df <- rbind(brier_me_bins_df,
                           brier_Metaculus_bins_df)
  }

  results <- list(brier_me = brier_me,
                  brier_Metaculus = brier_Metaculus,
                  ss_me_Metaculus = 1 - brier_me$bs / brier_Metaculus$bs,
                  count_questions = nrow(binary_questions),
                  brier_df = brier_df,
                  brier_bins_df = brier_bins_df)

  return(results)
}





#' Find important changes within MetaculR_questions object
#'
#' @param MetaculR_questions A MetaculR_questions object
#'
#' @return A dataframe of questions with difference measures (your most recent prediction vs. community's most recent prediction, etc.).
#' \item{id}{Question ID.}
#' \item{title}{Question title.}
#' \item{my_prediction}{My most recent prediction.}
#' \item{community_q2}{Community median.}
#' \item{community_ave}{Community average.}
#' \item{community_q2_pre_me}{Community median immediately prior to my_prediction.}
#' \item{community_ave_pre_me}{Community average immediately prior to my_prediction.}
#' \item{diff_me_q2}{Difference between me and the community median, by logodds.}
#' \item{diff_me_ave}{Difference between me and the community average, by logodds.}
#' \item{diff_comm_q2_pre_me}{Difference between community_q2_pre_me and the community average, by logodds.}
#' \item{diff_comm_ave_pre_me}{Difference between community_ave_pre_me and the community average, by logodds.}
#' \item{diff_me_q2_abs}{Absolute difference between me and the community median, by logodds.}
#' \item{diff_me_ave_abs}{Absolute difference between me and the community average, by logodds.}
#' \item{diff_comm_q2_pre_me_abs}{Absolute difference between community_q2_pre_me and the community average, by logodds.}
#' \item{diff_comm_ave_pre_me_abs}{Absolute difference between community_ave_pre_me and the community average, by logodds.}
#' \item{diff_me_q2_abs_odds}{Absolute difference between me and the community median, by odds.}
#' \item{diff_me_ave_abs_odds}{Absolute difference between me and the community average, by odds.}
#' \item{diff_comm_q2_pre_me_abs_odds}{Absolute difference between community_q2_pre_me and the community average, by odds.}
#' \item{diff_comm_ave_pre_me_abs_odds}{Absolute difference between community_ave_pre_me and the community average, by odds.}
#' @export
#'
#' @examples
#' \dontrun{
#' questions_myPredictions_byDiff <-
#'   MetaculR_myDiff(
#'     questions_myPredictions)
#' }

MetaculR_myDiff <- function(MetaculR_questions) {
  ## no visible binding for global variable solution
  my_prediction <- community_q2 <- community_ave <- community_q2_pre_me <- community_ave_pre_me <- diff_me_q2 <- diff_me_ave <- diff_comm_q2_pre_me <- diff_comm_ave_pre_me <- diff_me_q2_abs <- NULL

  loop_results <- data.frame()
  for(l in 1:length(MetaculR_questions)) {
    if(is.data.frame(MetaculR_questions[[l]]$results$my_predictions)) {
      for(el in which(MetaculR_questions[[l]]$results$possibilities$type == "binary" & is.na(MetaculR_questions[[l]]$results$resolution) & !is.na(MetaculR_questions[[l]]$results$community_prediction$full$q2))) {
    loop_results <-
      rbind(
        loop_results,
        data.frame(
          community_q2_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x1$q2[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t))],
          community_ave_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x2$avg[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t))] ###,
          # community_t_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t))]
        ))
  }}}

  binary_questions <- data.frame(
    id = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {x$results$id[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2))]})),
    title = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {x$results$title[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2))]})),
    my_prediction = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2))], function(f) f$x[length(f$x)])})),
    community_q2 = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {x$results$community_prediction$full$q2[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2))]})),
    community_ave = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & is.na(x$results$resolution) & !is.na(x$results$community_prediction$full$q2))], function(f) f$x2$avg[length(f$x2$avg)])}))
  ) %>%
    cbind(loop_results) %>%
    dplyr::mutate(diff_me_q2 = log(my_prediction / (1 - my_prediction)) - log(community_q2 / (1 - community_q2)),
                  diff_me_ave = log(my_prediction / (1 - my_prediction)) - log(community_ave / (1 - community_ave)),
                  diff_comm_q2_pre_me = log(community_q2 / (1 - community_q2)) - log(community_q2_pre_me / (1 - community_q2_pre_me)),
                  diff_comm_ave_pre_me = log(community_ave / (1 - community_ave)) - log(community_ave_pre_me / (1 - community_ave_pre_me))) %>%
    dplyr::mutate(diff_me_q2_abs = abs(diff_me_q2),
                  diff_me_ave_abs = abs(diff_me_ave),
                  diff_comm_q2_pre_me_abs = abs(diff_comm_q2_pre_me),
                  diff_comm_ave_pre_me_abs = abs(diff_comm_ave_pre_me),
                  diff_me_q2_abs_odds = exp(abs(diff_me_q2)),
                  diff_me_ave_abs_odds = exp(abs(diff_me_ave)),
                  diff_comm_q2_pre_me_abs_odds = exp(abs(diff_comm_q2_pre_me)),
                  diff_comm_ave_pre_me_abs_odds = exp(abs(diff_comm_ave_pre_me))) %>%
    dplyr::arrange(dplyr::desc(diff_me_q2_abs))
}





#' Plot the history of a single question
#'
#' @param MetaculR_questions A MetaculR_questions object
#' @param Metacular_id The ID of the question to plot
#' @param scale_binary Choose "prob", "odds", or "logodds"
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' MetaculR_plot(
#'   MetaculR_questions = questions_myPredictions,
#'   Metacular_id = 10004)
#' }

MetaculR_plot <- function(MetaculR_questions, Metacular_id, scale_binary = "prob") {
  ## no visible binding for global variable solution
  Date <- q1 <- q2 <- q3 <- x <- community_q2_pre_me <- community_ave_pre_me <- NULL

  community <- data.frame(
    cbind(Date = as.POSIXct(unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metacular_id)], function(f) f$t)})), origin = "1970-01-01 00:00.00 UTC"),
    q1 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metacular_id)], function(f) f$x1$q1)})),
    q2 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metacular_id)], function(f) f$x1$q2)})),
    q3 = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {lapply(x$results$community_prediction$history[which(x$results$id == Metacular_id)], function(f) f$x1$q3)})))
  ) %>%
    dplyr::mutate(Date = as.POSIXct(Date, origin = "1970-01-01 00:00.00 UTC"))

  if(is.null(unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$id == Metacular_id)], function(f) f$t)})))) {
    me_predict <- FALSE
  } else {
    me_predict <- TRUE
  }

  if(me_predict == TRUE) {
    me <- data.frame(
      cbind(Date = as.POSIXct(unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$id == Metacular_id)], function(f) f$t)})), origin = "1970-01-01 00:00.00 UTC"),
            x = unlist(lapply(MetaculR_questions, function(x) if(is.data.frame(x$results$my_predictions)) {lapply(x$results$my_predictions$predictions[which(x$results$id == Metacular_id)], function(f) f$x)})))
    ) %>%
      dplyr::mutate(Date = as.POSIXct(Date, origin = "1970-01-01 00:00.00 UTC"))

    loop_results <- data.frame()
    for(l in 1:length(MetaculR_questions)) {
      if(is.data.frame(MetaculR_questions[[l]]$results$my_predictions)) {
        for(el in which(MetaculR_questions[[l]]$results$id == Metacular_id)) {
          loop_results <-
            rbind(
              loop_results,
              data.frame(
                community_q2_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x1$q2[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))],
                community_ave_pre_me = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$x2$avg[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))],
                Date = MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t[max(which(MetaculR_questions[[l]]$results$community_prediction$history[[el]]$t <= MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t[length(MetaculR_questions[[l]]$results$my_predictions$predictions[[el]]$t)]))]
              ))  %>%
            dplyr::mutate(Date = as.POSIXct(Date, origin = "1970-01-01 00:00.00 UTC"))
        }}}

    diff_comm_q2_pre_me_abs_odds <-
      round(exp(abs(log(community$q2[length(community$q2)] / (1 - community$q2[length(community$q2)])) - log(loop_results$community_q2_pre_me / (1 - loop_results$community_q2_pre_me)))), 2)
    diff_me_q2_abs_odds <-
      round(exp(abs(log(community$q2[length(community$q2)] / (1 - community$q2[length(community$q2)])) - log(me$x[length(me$x)] / (1 - me$x[length(me$x)])))), 2)
  }

  ylim <- c(0, 1)

  if(scale_binary == "odds") {
    community <- community %>%
      dplyr::mutate(q1 = ifelse(q1 >= 0.5,
                                q1 / (1- q1),
                                -(1 - q1) / q1),
                    q2 = ifelse(q2 >= 0.5,
                                q2 / (1- q2),
                                -(1 - q2) / q2),
                    q3 = ifelse(q3 >= 0.5,
                                q3 / (1- q3),
                                -(1 - q3) / q3))

    if(me_predict == TRUE) {
      me <- me %>%
        dplyr::mutate(x = ifelse(x >= 0.5,
                                 x / (1 - x),
                                 -(1 - x) / x))

      loop_results <- loop_results %>%
        dplyr::mutate(community_q2_pre_me = ifelse(community_q2_pre_me >= 0.5,
                                                   community_q2_pre_me / (1 - community_q2_pre_me),
                                                   -(1 - community_q2_pre_me) / community_q2_pre_me),
                      community_ave_pre_me = ifelse(community_ave_pre_me >= 0.5,
                                                    community_ave_pre_me / (1 - community_ave_pre_me),
                                                    -(1 - community_ave_pre_me) / community_ave_pre_me))
    }

    ylim <- c(-99, 99)
  }
  if(scale_binary == "logodds") {
    community <- community %>%
      dplyr::mutate(q1 = log(q1 / (1 - q1)),
                    q2 = log(q2 / (1 - q2)),
                    q3 = log(q3 / (1 - q3)))

    if(me_predict == TRUE) {
      me <- me %>%
        dplyr::mutate(x = log(x / (1 - x)))

      loop_results <- loop_results %>%
        dplyr::mutate(community_q2_pre_me = log(community_q2_pre_me / (1 - community_q2_pre_me)),
                      community_ave_pre_me = log(community_ave_pre_me / (1 - community_ave_pre_me)))
    }


    ylim <- c(log(1 / 99), log(99))
  }

  gg <- community %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = Date,
                   y = q2)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = Date,
                   ymin = q1,
                   ymax = q3),
      alpha = 0.1)

  if(me_predict == TRUE) {
    gg <- gg +
    ggplot2::geom_point(
      data = me,
      ggplot2::aes(x = Date,
                   y = x),
      shape = 21
      ) +
    ggplot2::geom_point(
      data = loop_results,
      ggplot2::aes(x = Date,
                   y = community_q2_pre_me),
      shape = 21,
      fill = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = loop_results$Date,
                   y = loop_results$community_q2_pre_me,
                   xend = loop_results$Date,
                   yend = q2[length(q2)],
                   color = "red")) +
    ggplot2::annotate("text",
                      x = loop_results$Date,
                      y = community$q2[length(community$q2)],
                      label = paste0("Odds diff: ", diff_comm_q2_pre_me_abs_odds),
                      size = 3) +
    ggplot2::geom_segment(
      ggplot2::aes(x = Date[length(Date)],
                   y = q2[length(q2)],
                   xend = Date[length(Date)],
                   yend = me$x[length(me$x)],
                   color = "red")) +
    ggplot2::annotate("text",
                      x = me$Date[length(me$Date)], ###community$Date[length(community$Date)],
                      y = me$x[length(me$x)] - (ylim[2] - ylim[1]) * 0.04,
                      label = paste0("Odds diff: ", diff_me_q2_abs_odds),
                      size = 3)
    }

  gg +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(
      ylim = ylim
    ) +
    ggplot2::ggtitle(label = paste0(Metacular_id, ": ", unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$title[which(x$results$id == Metacular_id)]})))) +
    ggplot2::labs(y = "Community prediction") +
    ggplot2::guides(color = FALSE)
}





#' Find exciting questions
#'
#' @param MetaculR_questions A MetaculR_questions object
#'
#' @return A dataframe of questions with excitement measures.
#' \item{id}{Question ID.}
#' \item{title}{Question title.}
#' \item{Total_Change}{Cumulative delta in time period, by probability.}
#' \item{Total_logodds_Change}{Cumulative delta in time period, by logodds.}
#' \item{Total_Change_Even}{Cumulative delta toward even odds in time period, by probability.}
#' \item{Total_logodds_Change_Even}{Cumulative delta toward even odds in time period, by logodds.}
#' @export
#'
#' @examples
#' \dontrun{
#' questions_myPredictions_byExcitement <-
#'   MetaculR_excitement(
#'     questions_myPredictions)
#' }

MetaculR_excitement <- function(MetaculR_questions) {
  ## no visible binding for global variable solution
  x1 <- Date <- q2_delta <- q2_logodds_delta <- q2_delta_even <- q2_logodds_delta_even <- NULL

  binary_questions <- data.frame(
    id = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$id[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - 30 & unlist(lapply(x$results$community_prediction$history, function(z) nrow(z) > 0)))]})),
    title = unlist(lapply(MetaculR_questions, function(x) if(TRUE) {x$results$title[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - 30 & unlist(lapply(x$results$community_prediction$history, function(z) nrow(z) > 0)))]})),
    Total_Change = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - 30)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_delta = x1$q2 - dplyr::lag(x1$q2)) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - 30) %>% dplyr::summarize(Total_Change = sum(abs(q2_delta)))}))),
    Total_logodds_Change = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - 30)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_logodds_delta = log(x1$q2 / (1 - x1$q2)) - log(dplyr::lag(x1$q2) / (1 - dplyr::lag(x1$q2)))) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - 30) %>% dplyr::summarize(Total_logodds_Change = sum(abs(q2_logodds_delta)))}))),
    Total_Change_Even = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - 30)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_delta_even = ifelse(x1$q2 > 0.5 & x1$q2 - dplyr::lag(x1$q2) < 0, x1$q2 - dplyr::lag(x1$q2), ifelse(x1$q2 < 0.5 & x1$q2 - dplyr::lag(x1$q2) > 0, x1$q2 - dplyr::lag(x1$q2), 0))) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - 30) %>% dplyr::summarize(Total_Change_Even = sum(abs(q2_delta_even)))}))),
    Total_logodds_Change_Even = unlist(lapply(MetaculR_questions, function(x) lapply(x$results$community_prediction$history[which(x$results$possibilities$type == "binary" & as.Date(x$results$close_time) >= as.Date(Sys.time()) - 30)], function(z) if(nrow(z) > 0) {z %>% dplyr::mutate(Date = as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC"), q2_logodds_delta_even = ifelse(x1$q2 > 0.5 & x1$q2 - dplyr::lag(x1$q2) < 0, log(x1$q2 / (1 - x1$q2)) - log(dplyr::lag(x1$q2) / (1 - dplyr::lag(x1$q2))), ifelse(x1$q2 < 0.5 & x1$q2 - dplyr::lag(x1$q2) > 0, log(x1$q2 / (1 - x1$q2)) - log(dplyr::lag(x1$q2) / (1 - dplyr::lag(x1$q2))), 0))) %>% dplyr::filter(as.Date(Date) >= as.Date(Sys.time()) - 30) %>% dplyr::summarize(Total_logodds_Change_Even = sum(abs(q2_logodds_delta_even)))}))))
}
