#' PISA 2009 Data
#'
#' A data set containing demographic, cognitive, and noncognitive variables
#' for a subset of students and countries participating in the 2009
#' administration of the Programme for International Student Assessment (PISA).
#'
#' Variables include
#' \itemize{
#'  \item cnt: factor, country code
#'  \item schoolid: factor, school ID
#'  \item stidstd: factor, student ID
#'  \item oecd: factor, OECD participation (no, yes)
#'  \item bookid: factor, booklet ID (which of 13 booklets were administered)
#'  \item age: numeric, age in years
#'  \item escs: numeric, index of economic, social, and cultural status (calculated by PISA)
#'  \item sex: factor, binary sex (f for female, m for male)
#'  \item grade: numeric, grade ranging from 7 to 12
#'  \item homelang: factor, language spoken at home (testlang for same as test language, or other)
#'  \item pared: numeric, highest parent education level in years
#'  \item st27q01 through st27q13: numeric, items from the approaches to learning scale,
#'    which is separated into three subscales: memorization (items st27q01,
#'    st27q03, st27q05, st27q07), elaboration (items st27q04, st27q08,
#'    st27q10, st27q12), and control strategies (items st27q02, st27q06,
#'    st27q09, st27q11, st27q13)
#'  \item st33q01 through st33q04: numeric, items from the attitude toward school scale
#'  \item st41q01 through st41q06: numeric, items from the metacognition understanding
#'    and remembering scale
#'  \item st42q01 through st42q05: numeric, items from the metacognition summarizing
#'    scale
#'  \item memor: numeric, memorization strategies scale score
#'  \item elab: numeric, elaboration strategies scale score
#'  \item cstrat: numeric, control strategies scale score
#'  \item atschl: numeric, attitude toward school scale score
#'  \item undrem: numeric, metacognition understanding and remembering scale score
#'  \item metasum: numeric, metacognition summarizing scale score
#'  \item r414q02 through r458q04: factor, unscored responses for 11 reading test items
#'  \item s428q01 through s415q07t: factor, unscored responses for 17 science test items
#'  \item m420q01t through m559q01: factor, unscored responses for 12 math test items
#'  \item r414q02s through r458q04s: numeric, scored responses for 11 reading items
#'  \item s428q01s through s415q07ts: numeric, scored responses for 17 science items
#'  \item m420q01ts through m559q01s: numeric, scored responses for 12 math items
#'}
#'
#' @format A data frame with 44878 rows and 125 variables
#' @source \url{https://nces.ed.gov/surveys/pisa/}
"PISA09"
