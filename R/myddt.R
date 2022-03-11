#' @title my ddt
#'
#' @param df a data frame
#' @param SPECIES the name of a species
#'
#' @importFrom utils write.csv read.csv
#' @importFrom dplyr filter %>% group_by summarise mutate n
#' @importFrom ggplot2 ggplot geom_point geom_smooth ggtitle aes_string
#'
#' @return a Length vs Weight plot for the given Species and a quadratic curve
#' @return a named list of the original data frame, subsetted data frame, and
#' a relative frequency table of River in the original data frame
#' @export
#'
#' @examples
#' \dontrun{myddt(ddt, SPECIES="CCATFISH")}
#' \dontrun{myddt(ddt, SPECIES="SMBUFFALO")}
myddt <- function(df, SPECIES="CCATFISH") {
  RIVER <- WEIGHT <- LENGTH <- NULL
  d <- df %>% filter(SPECIES=={{SPECIES}})
  write.csv(d, file=paste("LvsWfor", SPECIES, ".csv", sep=""))
  g <- ggplot(d, aes_string(x="WEIGHT", y="LENGTH")) + geom_point(aes_string(color="RIVER")) + geom_smooth(formula = y~x +I(x^2), method = "lm") + ggtitle("Ruyu Liu")
  print(g)

  freq <- df %>% group_by(RIVER) %>% summarise(freq=n()) %>% mutate(freq=freq/sum(freq))
  l <- list(data = df, dataSub = d, riverFreq=freq)
  print(l)
}
