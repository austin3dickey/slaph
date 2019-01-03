p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

print(p)

channelName <- "@austin"
botName <- "Austin's Plot Bot"

# Normal text
slaph::PlotToSlack(
    p = p
    , characters = "codeblock"
    , channelName = channelName
    , botName = botName
)

# Emojis
slaph::PlotToSlack(
    p = p
    , characters = "emoji"
    , channelName = channelName
    , botName = botName
)

# Size is adjustable
slaph::PlotToSlack(
    p = p
    , characters = "emoji"
    , width = 15
    , channelName = channelName
    , botName = botName
)


# Data pipelines can publish daily graphs to monitor
monitoringDT <- data.table::fread("~/monitoring.csv")
monitoringDT[, realtime := as.POSIXct(realtime)]

p2 <- ggplot2::ggplot(monitoring, ggplot2::aes(realtime, lag)) +
    ggplot2::geom_line()
print(p2)

slaph::PlotToSlack(
    p = p2
    , characters = "emoji"
    , channelName = channelName
    , botName = botName
)
