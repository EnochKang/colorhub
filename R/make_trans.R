#' @title Produce color transparent
#'
#' @author Enoch Kang
#'
#' @description
#' **MakeTrans()** is a function for simply producing semi-transparent color
#' using color name.
#'
#' @import grDevices
#'
#' @param color STRING for a color name of Hex code.
#' @param percent NUMERIC value between 0 and 1 for transparent.
#'
#' @return
#' **MakeTrans()** returns Hex code.
#'
#' @references
#' Ying Wei (2021). Colors in R. retrieved from:
#' *http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf*
#'
#' @seealso \code{\link{SmrzTrans}}
#'
#' @examples
#' library(colorhub)
#' colorTrans <- MakeTrans("red", percent = 0.7)
#'
#' colorTrans
#'
#' @export MakeTrans

MakeTrans <- function(color, percent = 0) {

  testColor <- try(col2rgb(color), TRUE)
  lgcColor  <- ifelse(isFALSE("try-error" %in% class(testColor)),
                      FALSE, TRUE)
  lgcPrcnt  <- ifelse(percent > 1, TRUE,
                      ifelse(percent < 0, TRUE, FALSE))

  cat("Check variables:\n")

  if (lgcColor) {
    cat(paste(" Color ------------------------------------------------------- X\n",
              ' REQUIRE: Argument "color" must be a valid color name or Hex code.
              Color name can be found as following link:
              http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
              Rules for Hex code: #RRGGBB.'
    ),
    fill = TRUE, sep = "")
  } else {
    cat(paste(" Color ------------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcPrcnt) {
    cat(paste(" Percentage -------------------------------------------------- X\n",
              ' REQUIRE: Argument "percent" must be a value between 0 and 1.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Percentage -------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcColor | lgcPrcnt)
    stop("Try to resolve abovementioned issues.")

  '
  cat(" \n")
  cat("Summary:\n")

  #optColor <- rgb(col2rgb(color)[1]/255, col2rgb(color)[2]/255, col2rgb(color)[3]/255, )

  cat("Hex code: ", rgb(col2rgb(color)[1]/255, col2rgb(color)[2]/255, col2rgb(color)[3]/255, ), "\n",
      "rgb info: \n",
      " Red:    ", col2rgb(color)[1], "\n",
      " Green:  ", col2rgb(color)[2], "\n",
      " Blue:   ", col2rgb(color)[3], "\n",
      " Trans.: ", percent,
      sep = "")

  cat(" \n")
  '

  output <- rgb(col2rgb(color)[1]/255, col2rgb(color)[2]/255, col2rgb(color)[3]/255, percent)

  output         <- as.list(output)
  class(output)  <- "colorhub"
  output$proc    <- "MakeTrans"
  output$summary <- capture.output(cat("Hex code: ", "\n", rgb(col2rgb(color)[1]/255, col2rgb(color)[2]/255, col2rgb(color)[3]/255, ), "\n",
                                       "rgb info: \n",
                                       " Red: ", "\n", col2rgb(color)[1], "\n",
                                       " Green: ", "\n", col2rgb(color)[2], "\n",
                                       " Blue: ", "\n", col2rgb(color)[3], "\n",
                                       " Trans.: ", "\n", percent,
                                       sep = ""))

  optTrans <<- output

  # cat(output[[1]])

  tempTrans <- output[[1]]

}


#' @title Summary of output of MakeTrans
#'
#' @description
#' **SmrzTrans()** is a function for summarizing output of **MakeTrans()**.
#'
#' @import grDevices
#'
#' @param data STRING for output of **MakeTrans()**.
#'
#' @return
#' **SmrzTrans()** returns Hex code.
#'
#' @seealso \code{\link{MakeTrans}}
#'
#' @examples
#' library(colorhub)
#' MakeTrans("red", percent = 0.7)
#'
#' SmrzTrans(optTrans)
#'
#' @export SmrzTrans

SmrzTrans <- function(data) {

  lgcInher <- !inherits(data, "colorhub")

  lgcProc  <- isFALSE(data$proc == "MakeTrans")

  cat("Check object:\n")

  if (lgcInher | lgcProc) {
    cat(paste(" Inherit ----------------------------------------------------- X\n",
              ' REQUIRE: Argument "data" must be an object of class \"colorhub\"
               produced by function **MakeTrans()**.'),
        fill = TRUE, sep = "")
  } else {
    cat(paste(" Inherit ----------------------------------------------------- V"),
        fill = TRUE, sep = "")
  }

  if (lgcInher | lgcProc)
    stop("Try to resolve abovementioned issues.")

  cat(" \n")
  cat("Summary:\n")
  cat(data$summary[1:2], "\n",
      data$summary[3], "\n ",
      data$summary[4], "   ", data$summary[5],"\n ",
      data$summary[6], " ", data$summary[7],"\n ",
      data$summary[8], "  ", data$summary[9],"\n ",
      data$summary[10], data$summary[11],
      sep = "")

  optColor <<- data.frame(sys  = c(data$summary[1], data$summary[3], data$summary[4], data$summary[6], data$summary[8], data$summary[10]),
                          code = c(data$summary[2], "", data$summary[5], data$summary[7], data$summary[9], data$summary[11]))

}

