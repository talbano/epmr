#' PISA 2009 Data
#'
#' A dataset containing demographic, cognitive, and noncognitive variables
#' for a subset of students participating in the 2009 administration of the
#' Programme for International Student Assessment (PISA).
#'
#' Variables include
#' \itemize{
#'  \item cnt: country code factor.
#'  \item schoolid: school ID factor.
#'  \item stidstd: student ID factor.
#'  \item oecd: indicator factor for OECD participation.
#'  \item bookid: booklet ID factor.
#'  \item age: numeric age in years.
#'  \item grade: numeric grade, ranging from 7 to 12
#'  \item From the PISA 2009 student survey:
#'  \itemize{
#'    \item st27q01 through st27q13: items from the approaches to learning scale,
#'      which is separated into three subscales: memorization, elaboration,
#'      and control strategies.
#'    \item st33q01 through st33q04: items from the attitude toward school scale.
#'    \item st41q01 through st41q06: items from the metacognition understanding
#'      and remembering scale.
#'    \item st42q01 through st42q05: items from the metacognition summarizing
#'      scale.
#'    \item memor: memorization strategies scale score.
#'    \item elab: elaboration strategies scale score.
#'    \item cstrat: control strategies scale score.
#'    \item atschl: attitude toward school scale score.
#'    \item undrem: metacognition understanding and remembering scale score.
#'    \item metasum: metacognition summarizing scale score.
#'  }
#'  \item From the PISA 2009 reading assessment:
#'  \itemize{
#'    \item r412q01 through r453q06: unscored responses for 15 items in reading
#'      cluster 6.
#'    \item r412q01s through r453q06s: scored responses for 15 items in reading
#'      cluster 6.
#'  }
#'}
#'
#' @format A data frame with 44506 rows and 73 variables
#' @source \url{https://nces.ed.gov/surveys/pisa/}
"PISA09"