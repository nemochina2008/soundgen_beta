## Functions that are only needed for the Shiny UI, but not for using soundgen as a command-line tool, i.e. not needed for calling generateBout()

#' Convert Hz to semitones
#'
#' Internal soundgen function.
#'
#' Converts from Hz to semitones above C0 (~16.4 Hz).
#' @param h vector of frequencies (Hz)
HzToSemitones = function(h) {
  out = sapply(h, function(x)
    log2(x / 16.3516) * 12)
  # this is also the index of the note name in our dictionary notes_dict,
  # so we can simply look it up :))
  return (out)
}

#' Convert semitones to Hz
#'
#' Internal soundgen function.
#'
#' Converts from semitones above C0 (~16.4 Hz) to Hz
#' @param s vector of frequencies (semitones above C0)
semitonesToHz = function(s) {
  out = sapply(s, function(x)
    16.3516 * 2 ^ (x / 12))
  return (out)
}
