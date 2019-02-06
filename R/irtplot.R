#' Item Response Theory Plotting
#'
#' Functions for plotting results from item response theory models.
#'
#' @title irtplot
#' @param x irtstudy output.
#' @param theta vector of theta values, taken by default from \code{x} in
#' ip_plot and otherwise defaulting to a sequence of values.
#' @param b vector of item location parameters, taken by default from
#' \code{x}.
#' @param groups vector defining person groups, defaulting to one group.
#' @param tests vector defining tests, i.e., item groups, defaulting to one.
#' @param ip data frame of item parameters, taken from \code{x}.
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
#' print(irf_plot(irtgbr))
#'
#' # Test response functions
#' print(tef_plot(irtgbr))
#'
#' @rdname irtplot
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
  out <- ggplot2::ggplot(df, ggplot2::aes(x = theta, fill = source)) +
    ggplot2::geom_density(alpha = 0.25, size = 0) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  return(out)
}

#' @rdname irtplot
#' @export
irf_plot <- function(x, ip = x$ip, theta = seq(-4, 4, length = 100)) {
  irf <- rirf(ip, theta = theta)[, -1]
  item <- rep(colnames(irf), each = length(theta))
  p <- unlist(irf)
  df <- dplyr::data_frame(theta = rep(theta, nrow(ip)), p = p,
    item = item)
  out <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = p, color = item)) +
    ggplot2::geom_line()
  return(out)
}

#' @rdname irtplot
#' @export
tef_plot <- function(x, ip = x$ip, theta = seq(-4, 4, length = 100)) {
  df <- dplyr::as_data_frame(rtef(ip, theta = theta))
  se <- df$se
  out <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = se)) +
    ggplot2::geom_line()
  return(out)
}

