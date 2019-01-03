#' @title Post an arbitrary message to Slack
#' @name PostToSlack
#' @description Posts a message to Slack using Incoming Webhooks
#' @param channelName A string of the form \code{"@username"} or \code{"#channelname"}. When using
#'   \code{"@username"}, log messages will be sent through Slackbot to that user. When using
#'   \code{"#channelname"}, log messages will be posted to that channel as a bot.
#' @param messageToPost A string of the message to post.
#' @param botName A string of the name of the bot that posts. Default is NULL, which means to use
#'   the default bot name that was set up with the Incoming Webhook. See References.
#' @param webhookURL A string of the Slack webhook URL of the form
#'   \code{"https://hooks.slack.com/services/XXX/YYY/ZZZ"}. Default reads the environment variable
#'   \code{SLACK_WEBHOOK}. See References.
#' @return Result of the \code{\link[httr]{POST}} call.
#' @references
#' \href{https://get.slack.help/hc/en-us/articles/115005265063-Incoming-WebHooks-for-Slack}{Slack
#' documentation on setting up an Incoming Webhook}
#' @importFrom assertthat assert_that is.string
#' @importFrom httr POST add_headers
#' @importFrom jsonlite toJSON
#' @importFrom utils URLencode
PostToSlack <- function(
    channelName
    , messageToPost
    , botName = NULL
    , webhookURL = Sys.getenv("SLACK_WEBHOOK")
) {

    # Input checks
    assertthat::assert_that(
        assertthat::is.string(webhookURL)
        , webhookURL != ""
        , webhookURL != "https://hooks.slack.com/services/XXX/YYY/ZZZ"
        , msg = paste(
            "webhookURL seems to be missing. Either provide it to this function, or set the"
            , "SLACK_WEBHOOK env variable. You can add it to your .Renviron or call"
            , "\n\nSys.setenv(SLACK_WEBHOOK = 'https://hooks.slack.com/services/XXX/YYY/ZZZ')\n\n"
            , "in this R session. To set up an Incoming Webhook, see"
            , "https://get.slack.help/hc/en-us/articles/115005265063-Incoming-WebHooks-for-Slack"
        )
    )
    assertthat::assert_that(
        assertthat::is.string(channelName)
        , substr(channelName, 1, 1) %in% c("@", "#")
        , msg = "channelName must be a string starting with '@' or '#'"
    )
    assertthat::assert_that(
        assertthat::is.string(messageToPost)
        , is.null(botName) || assertthat::is.string(botName)
    )

    # Set up the payload
    payloadList <- list(
        channel = channelName
        , text = messageToPost
    )
    if (!is.null(botName)) {
        payloadList <- c(payloadList, list(username = botName))
    }
    payloadJSON <- jsonlite::toJSON(x = payloadList, auto_unbox = TRUE)

    # Make the call
    httr::POST(
        url = webhookURL
        , encode = "form"
        , httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded", Accept = "*/*")
        , body = utils::URLencode(paste0("payload=", payloadJSON))
    )
}
