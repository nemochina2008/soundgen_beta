#' Print time
#'
#' Internal soundgen function.
#'
#' Converts time in seconds to time in h:m:s for pretty printing.
#' @param time_s time (s)
#' @return Returns a character string like "1 h 20 min 3 s"
#' @examples
#' time_start = proc.time()
#' Sys.sleep(1.5)
#' time_diff = as.numeric((proc.time() - time_start)[3])
#' soundgen:::convert_sec_to_hms(time_diff)
convert_sec_to_hms = function(time_s) {
  hours = time_s %/% 3600
  minutes = time_s %/% 60 - hours * 60
  seconds = round (time_s %% 60, 0)

  output = ''
  if (hours > 0) output = paste0(output, hours, ' h ')
  if (minutes > 0) output = paste0(output, minutes, ' min ')
  if (seconds > 0) output = paste0(output, seconds, ' s')

  # remove the last space, if any
  if (substr(output, nchar(output), nchar(output)) == ' ') {
    output = substr(output, 1, nchar(output)-1)
  }
  return(output)
}
