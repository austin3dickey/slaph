% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ascii.R
\name{PlotToSlack}
\alias{PlotToSlack}
\title{Post ASCII of a ggplot to Slack}
\usage{
PlotToSlack(p, characters = c("codeblock", "emoji"), width = NULL,
  channelName, botName = NULL,
  webhookURL = Sys.getenv("SLACK_WEBHOOK"))
}
\arguments{
\item{p}{A ggplot object. Currently works best if it's monochromatic.}

\item{characters}{One of these two options: \itemize{
  \item{\code{"codeblock"} (default) - Plot in a backtick-surrounded code block. No emojis but
    higher resolution is possible.}
  \item{\code{"emoji"} - Plot in emojis. Lower resolution but more fun.}
}}

\item{width}{The width of the plot in number of characters. Default is 60 for \code{"codeblock"}
and 30 for \code{"emoji"}.}

\item{channelName}{A string of the form \code{"@username"} or \code{"#channelname"}. When using
\code{"@username"}, log messages will be sent through Slackbot to that user. When using
\code{"#channelname"}, log messages will be posted to that channel as a bot.}

\item{botName}{A string of the name of the bot that posts. Default is NULL, which means to use
the default bot name that was set up with the Incoming Webhook. See References.}

\item{webhookURL}{A string of the Slack webhook URL of the form
\code{"https://hooks.slack.com/services/XXX/YYY/ZZZ"}. Default reads the environment variable
\code{SLACK_WEBHOOK}. See References.}
}
\value{
Result of the \code{\link[httr]{POST}} call.
}
\description{
Given a ggplot, rasterize it, turn it into ASCII, and post it to Slack.
}
