#' Standard Error
#'
#' This function allows you to calculate standard error
#' @param x A single vector.
#' @keywords standard error
#' @export
#' @examples
#' x <- rnorm(100)
#' se(x)

se <- function(x, na.rm = TRUE) {
  ifelse(na.rm == TRUE,
         sd(x, na.rm = TRUE)/sqrt(length(na.omit(x))),
         sd(x)/sqrt(length(x)))
}

#' Cross-Validated Correlation
#'
#' This function allows you to run a correlation using leave-one-out cross-validation.
#' @param x A single vector.
#' @param y A single vector.
#' @return An average correlation coefficient and the p-value for that coefficient.
#' @keywords cross-validation, correlation
#' @export
#' @examples
#' x <- rnorm(n = 25, mean = 0, sd = 1)
#' y <- rnorm(n = 25, mean = 25, sd = 5)
#' corr_cv(x, y)

corr_cv <- function(x, y) {
  
  datset <- data.frame(cbind(x, y))
  datset <- dplyr::filter(datset, !is.na(x) & !is.na(y))
  
  corval <- vector()
  for (i in 1:nrow(datset)) {
    corval[i] <- cor(x[-i], y[-i], use = "pairwise.complete.obs")
  }
  
  mean_corr <- mean(corval)
  p <- psych::r.test(n = nrow(datset), r12 = mean_corr)$p
  
  datset <- data.frame(r = mean_corr, p = p)
  return(datset)
}

#' Group Summary Statistics
#'
#' This function summarizes your data by a given factor(s). It will return a dataframe that includes
#' the mean and standard error of the outcome measure of interest. The summary statistics will be 
#' formatted for plotting using ggplot2.
#' @param data A dataframe that contains the variables to be summarized.
#' @param y A column from the dataframe specified that will be summarized as an outcome measure.
#' @param g1 Grouping variable 1
#' @param g2 Grouping variable 2
#' @return A dataframe with the mean and standard error by grouping factor(s).
#' @keywords summarize, mean, standard error, tidy
#' @export
#' @import dplyr
#' @import magrittr
#' @examples
#' dime <- dime 
#' groupsum(data = dime, y = acc, g1 = modul)

groupsum <- function(data, y, g1, g2 = NULL, g3 = NULL, g4 = NULL) {
  
  #mlab <- paste0("m_", deparse(substitute(y)))
  #selab <- paste0("se_", deparse(substitute(y)))
  
  g1 <- substitute(g1)
  y1 <- substitute(y)
  
  if (missing(g2)) {
    
    datset <- data %>% 
      group_by(!! g1) %>% 
      summarize(mean = mean(!! y1),
                se = se(!! y1))
    return(data.frame(datset))
    
  } else if(missing(g3)) {
    
    g2 <- substitute(g2)
    datset <- data %>% 
      group_by(!! g1, !! g2) %>% 
      summarize(mean = mean(!! y1),
                se = se(!! y1))
    return(data.frame(datset))
    
  } else if(missing(g4)) {
    
    g2 <- substitute(g2)
    g3 <- substitute(g3)
    datset <- data %>%
      group_by(!! g1, !! g2, !! g3) %>% 
      summarize(mean = mean(!! y1),
                se = se(!! y1))
    return(data.frame(datset))
    
  } else {
    
    g2 <- substitute(g2)
    g3 <- substitute(g3)
    g4 <- substitute(g4)
    datset <- data %>% 
      group_by(!! g1, !! g2, !! g3, !! g4) %>% 
      summarize(mean = mean(!! y1),
                se = se(!! y1))
    return(data.frame(datset))
  }
}

#' Melt Lower Triangle
#'
#' This function pulls the lower half of a correlation matrix and imputes NAs for the upper half, 
#' and then melts the matrix so that it can be plotted
#' @param df A dataframe/tibble that will be correlated.
#' @return An melted tibble that has the lower half values of a correlation matrix.
#' @keywords lower triangle, correlation matrix
#' @import reshape2
#' @export
#' @examples
#' cormat_lower(facat[2:4])

cormat_lower <- function(df, diag = TRUE){
  tmp <- cor(df)
  
  ifelse(diag == FALSE,
         tmp[upper.tri(tmp, diag = TRUE)] <- NA,
         tmp[upper.tri(tmp, diag = FALSE)] <- NA)
  
  tmp <- melt(tmp, na.rm = TRUE)
  return(tmp)
}

#' Melt Upper Triangle
#'
#' This function pulls the upper half of a correlation matrix and imputes NAs for the upper half, 
#' and then melts the matrix so that it can be plotted
#' @param df A dataframe/tibble that will be correlated.
#' @return An melted tibble that has the upper half values of a correlation matrix.
#' @keywords lower triangle, correlation matrix
#' @import reshape2
#' @export
#' @examples
#' cormat_uper(facat[2:4])

cormat_upper <- function(df, diag = TRUE){
  tmp <- cor(df)
  
  ifelse(diag == FALSE,
         tmp[lower.tri(tmp, diag = TRUE)] <- NA,
         tmp[lower.tri(tmp, diag = FALSE)] <- NA)
  
  tmp <- melt(tmp, na.rm = TRUE)
  return(tmp)
}