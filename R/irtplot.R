#' Item Response Theory Plotting
#'
#' Functions for plotting results from item response theory models.
#' Currently, only the 1 parameter Rasch model is supported.
#'
#' @param x irtstudy output.
#' @param theta vector of theta values, taken by default from \code{x} in
#' ip_plot and otherwise defaulting to a sequence of values.
#' @param b vector of item location parameters, taken by default from
#' \code{x}.
#' @param groups vector defining person groups, defaulting to one group.
#' @param tests vector defining item groups, i.e., tests, defaulting to one.
#' @param ip data frame of item parameters, taken from \code{x}.
#' @param ... any number of unnamed arguments, each being output from an
#' irtstudy, used to plot multiple test functions.
#' @examples
#'
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
#'   "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
#'   "r458q07", "r458q04")
#' rsitems <- paste0(ritems, "s")
#' pisagbr <- PISA09[PISA09$cnt == "GBR", rsitems]
#' irtgbr <- irtstudy(pisagbr)
#'
#' # Item person map
#' print(ip_plot(irtgbr))
#'
#' # Item response functions
#' print(irf_plot(x))

#' @export
ip_plot <- function(x, theta = x$data$theta, b = x$ip$b,
  groups = rep(1, length(theta)), tests = rep(1, length(b))) {

  df <- dplyr::data_frame(theta = double(), source = character())
  for (p in unique(groups))
    df <- dplyr::bind_rows(df, dplyr::data_frame(theta = theta[groups == p],
      source = paste("group", p)))
  for (i in unique(tests))
    df <- dplyr::bind_rows(df, dplyr::data_frame(theta = b[tests == i],
      source = paste("test", i)))
  if (length(unique(groups)) == 1)
    df$source <- gsub("group 1", "person", df$source)
  if (length(unique(tests)) == 1)
    df$source <- gsub("test 1", "item", df$source)
  out <- ggplot2::ggplot(df, aes(x = theta, fill = source)) +
    ggplot2::geom_density(alpha = 0.25, size = 0) +
    theme(legend.title = element_blank())
  return(out)
}

#' @export
irf_plot <- function(x, ip = x$ip, theta = seq(-4, 4, length = 100)) {
  irf <- rirf(ip, theta = theta)[, -1]
  item <- rep(colnames(irf), each = length(theta))
  df <- dplyr::data_frame(theta = rep(theta, nrow(ip)), p = unlist(irf),
    item = item)
  out <- ggplot2::ggplot(df, aes(x = theta, y = p, color = item)) +
    geom_line()
  return(out)
}

#' @export
tef_plot <- function(..., ip, theta = seq(-4, 4, length = 100)) {
  dots <- list(...)
  if (length(dots)) {
    df <- dplyr::as_data_frame(rtef(ip, theta = theta))
    out <- ggplot2::ggplot(df, aes(x = theta, y = se)) + geom_line()
  } else {
    if (all(unlist(lapply(dots, function(y) "irtstudy" %in% class(y))))) {
      ip <- lapply(dots, "[[", "ip")
      # test_names <- unlist(lapply(dots, "[[", "name"))
    } else {
      ip <- dots
      # test_names
    }
    tef <- do.call("rbind", lapply(ip, rtef, theta = theta))
    test_names <- paste("test", seq_along(ip))
    tef <- data.frame(tef, source = rep(test_names, each = length(theta)))
    df <- dplyr::as_data_frame(tef)
    out <- ggplot2::ggplot(df, aes(x = theta, y = se, color = source)) +
      geom_line()
  }
  return(out)
}
