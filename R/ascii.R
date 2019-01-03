#' @name PlotToSlack
#' @title Post ASCII of a ggplot to Slack
#' @description Given a ggplot, rasterize it, turn it into ASCII, and post it to Slack.
#' @param p A ggplot object. Currently works best if it's monochromatic.
#' @param characters One of these two options: \itemize{
#'   \item{\code{"codeblock"} (default) - Plot in a backtick-surrounded code block. No emojis but
#'     higher resolution is possible.}
#'   \item{\code{"emoji"} - Plot in emojis. Lower resolution but more fun.}
#' }
#' @param width The width of the plot in number of characters. Default is 60 for \code{"codeblock"}
#'   and 30 for \code{"emoji"}.
#' @inheritParams PostToSlack
#' @return Result of the \code{\link[httr]{POST}} call.
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom ggplot2 theme_void
#' @importFrom grDevices dev.off png
#' @importFrom png readPNG
PlotToSlack <- function(
    p
    , characters = c("codeblock", "emoji")
    , width = NULL
    , channelName
    , botName = NULL
    , webhookURL = Sys.getenv("SLACK_WEBHOOK")
) {
    # Parse arguments
    characters <- match.arg(characters)
    if (is.null(width)) {
        width <- switch(
            characters
            , codeblock = 60
            , emoji = 30
        )
    }
    height <- switch(
        characters
        , codeblock = width/18*7  # codeblock aspect ratios are stretched
        , emoji = width
    )

    # Input checks
    assertthat::assert_that(
        inherits(p, "ggplot")
        , assertthat::is.count(width)
    )

    # Make geom_point output pixels instead of circles
    p$layers <- lapply(p$layers, function(layer) {
        if (inherits(layer$geom, "GeomPoint")) {
            layer$aes_params <- c(layer$aes_params, list(shape = "."))
        }
        layer
    })

    # Rasterize the image to a temporary file
    tmpFile <- tempfile(fileext = ".png")
    grDevices::png(
        filename = tmpFile
        , width = width
        , height = height
    )
    print(p + ggplot2::theme_void())
    grDevices::dev.off()

    # Read it back in as a matrix of pixels
    imageMatrix <- png::readPNG(tmpFile)

    # Collapse each (R,G,B,A) tuple into a character
    chrMatrix <- switch(
        characters
        , codeblock = CollapseCodeblock(imageMatrix)
        , emoji = CollapseEmoji(imageMatrix)
    )

    # String it all together into a message
    message <- MatrixToMessage(
        mat = chrMatrix
        , characters = characters
        , width = width
        , xlab = p$labels$x
        , ylab = p$labels$y
    )

    # Post it
    PostToSlack(
        channelName = channelName
        , messageToPost = message
        , botName = botName
        , webhookURL = webhookURL
    )
}


CollapseCodeblock <- function(imageMatrix) {
    apply(
        X = imageMatrix[, , 1:3]    # not alpha layer
        , MARGIN = c(1, 2)
        , FUN = function(rgb) {
            if (sum(rgb) > 1) " "
            else "0"
        }
    )
}

CollapseEmoji <- function(imageMatrix) {

    # Sum R+G+B to find intensities
    intensities <- as.vector(apply(
        X = imageMatrix[, , 1:3]    # not alpha layer
        , MARGIN = c(1, 2)
        , FUN = sum
    ))

    outMat <- character(length(intensities))
    outMat[intensities == 3] <- ":white_square:"

    n <- min(length(unique(intensities[intensities < 3])), 5)

    if (n == 1) {
        cutInt <- ":black_square:"
    } else {
        cutInt <- cut(
            x = intensities[intensities < 3]
            , breaks = n
            , labels = c(
                ":black_large_square:"
                , ":black_medium_square:"
                , ":white_square_button:"
                , ":black_medium_small_square:"
                , ":black_small_square:"
            )[seq(n)]
        )
    }

    outMat[intensities < 3] <- as.character(cutInt)
    outMat <- matrix(outMat, nrow = nrow(imageMatrix))
    return(outMat)
}

MatrixToMessage <- function(mat, characters, width, xlab, ylab) {
    # Define the border characters
    b <- switch(
        characters
        , codeblock = list(
            tl = "\u250c"
            , lr = "\u2502"
            , bl = "\u2514"
            , tb = "\u2500"
            , br = "\u2518"
            , tr = "\u2510"
        )
        , emoji = list(
            tl = ":border_tl:"
            , lr = ":border_lr:"
            , bl = ":border_bl:"
            , tb = ":border_tb:"
            , br = ":border_br:"
            , tr = ":border_tr:"
        )
    )

    numXLabSpaces <- switch(
        characters
        , codeblock = width + 1 - nchar(xlab)
        , emoji = 5.75 * (width + 1)
    )

    # Paste it all together
    message <- paste0(
        ylab
        , "\n"
        , b$tl, paste(rep(b$tb, width), collapse = ""), b$tr
        , "\n"
        , b$lr, paste(apply(mat, 1, paste, collapse = ""), collapse = paste0(b$lr, "\n", b$lr)), b$lr
        , "\n"
        , b$bl, paste(rep(b$tb, width), collapse = ""), b$br
        , "\n"
        , " ", paste(rep(" ", numXLabSpaces), collapse = ""), xlab
    )

    # Need backticks for codeblocks
    if (characters == "codeblock") {
        message <- paste0("```", message, "```")
    }

    return(message)
}
