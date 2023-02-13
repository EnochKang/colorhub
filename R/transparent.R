#' @title Produce color transparent
#'
#' @author Enoch Kang
#'
#' @description
#' **Transparent()** is a function for simply producing semi-transparent color
#' using color name.
#'
#' @import grDevices
#'
#' @param color STRING for a color name of Hex code.
#' @param percent NUMERIC value between 0 and 1 for transparent.
#'
#' @return
#' **Transparent()** returns Hex code.
#'
#' @references
#' Ying Wei (2021). Colors in R. retrieved from:
#' *http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf*
#'
#' @examples
#' library(colorhub)
#' colorTrans <- Transparent("red", percent = 0.7)
#'
#' colorTrans
#'
#' @export Transparent


Transparent <- function(color, percent = 0) {

  testColor <- try(col2rgb(color), TRUE)
  lgcColor  <- ifelse(isFALSE("try-error" %in% class(testColor)),
                      FALSE, TRUE)
  lgcPrcnt  <- ifelse(percent > 1, TRUE,
                      ifelse(percent < 0, TRUE, FALSE))


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

  optColor <- rgb(col2rgb(color)[1]/255, col2rgb(color)[2]/255, col2rgb(color)[3]/255,)


  cat("Hex code:", optColor, "\n",
      "rgb info:\n",
      " Red:    ", col2rgb(color)[1],
      " Green:  ", col2rgb(color)[2],
      " Blue:   ", col2rgb(color)[3],
      " Trans.: ", percent,
        sep = "")

}
