#' Find fyllables
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{segment}}.
#'
#' @param envelope downsampled amplitude envelope
#' @param timestep time difference between two points in the envelope (ms)
#' @param threshold all continuous segments above this value are considered to
#'   be syllables
#' @inheritParams segment
#' @param merge_close_syl if TRUE, syllable separated by less than
#'   \code{shortest_pause} will be merged
#' @return Returns a dataframe with timing of syllables.
findSyllables = function(envelope,
                         timestep,
                         threshold,
                         shortest_syl,
                         shortest_pause,
                         merge_close_syl) {
  # find strings of TTTTT
  envelope$aboveThres = ifelse (envelope$value > threshold, 1, 0)
  env_above_thres = data.frame (value = rle(envelope$aboveThres)[[2]],
                                count = rle(envelope$aboveThres)[[1]])
  env_above_thres$idx = 1:nrow(env_above_thres) # a convoluted way of tracing
  # the time stamp in the output of rle
  # exclude segments of length < shortest_syl or below threshold
  env_short = na.omit(env_above_thres[env_above_thres$value == 1 &
                                        env_above_thres$count > ceiling(shortest_syl / timestep), ])
  nSyllables = nrow(env_short)

  # save the time of each syllable for plotting
  if (nSyllables == 0){
    syllables = data.frame(syllable = 0,
                           time_start = NA,
                           time_end = NA,
                           dur = NA)
  } else {
    syllables = data.frame(
      syllable = 1:nSyllables,
      time_start = apply(matrix(1:nSyllables), 1, function(x) {
        sum(env_above_thres$count[1:(env_short$idx[x] - 1)]) * timestep
      }),
      time_end = NA
    )
    if (env_above_thres$value[1] == 1) {   # if the sounds begins with a syllable
      syllables$time_start[1] = 0  # the first syllable begins at zero
    }
    syllables$time_end = syllables$time_start + env_short$count * timestep

    # Optional: merge syllables with very short intervals in between them
    if (merge_close_syl) {
      syllables = mergeSyllables(syllables, shortest_pause)
      syllables$syllable = 1:nrow(syllables)
    }
    syllables$dur = syllables$time_end - syllables$time_start
  }


  return (syllables)
}


#' Merge syllables
#'
#' Internal soundgen function.
#'
#' Merges syllables if they are separated by less than \code{shortest_pause ms}. Called by \code{\link{findSyllables}}.
#' @param syllables a dataframe listing syllables with time_start and time_end
#' @inheritParams segment
mergeSyllables = function (syllables, shortest_pause) {
  i = 1
  while (i < nrow(syllables)) {
    while (syllables$time_start[i + 1] - syllables$time_end[i] < shortest_pause &
           i < nrow(syllables)) {
      syllables$time_end[i] = syllables$time_end[i + 1]
      syllables = syllables[-(i + 1), ]
    }
    i = i + 1
  }
  return (syllables)
}


#' Find bursts
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{segment}}.
#'
#' @inheritParams findSyllables
#' @inheritParams segment
#' @return Returns a dataframe with timing of bursts
findBursts = function(envelope,
                      timestep,
                      interburst_min_ms,
                      peak_to_global_max,
                      peak_to_trough,
                      trough_left = TRUE,
                      trough_right = FALSE) {
  if (!is.numeric(interburst_min_ms)) {
    stop(paste0('interburst_min_ms is weird:', interburst_min_ms))
  }
  if (interburst_min_ms < 0) {
    stop('interburst_min_ms is negative')
  }

  # we're basically going to look for local maxima within ± n
  n = floor(interburst_min_ms / timestep)
  bursts = data.frame(time = 0, ampl = 0)

  for (i in 1:nrow(envelope)) {
    # for each datapoint, compare it with the local minima to the left/right over ± interburst_min ms
    if (i > n) {
      local_min_left = min(envelope$value[(i - n):i])
    } else {
      local_min_left = 0
    }
    # close to the beginning of the file, local_min_left = 0
    if (i < (nrow(envelope) - n - 1)) {
      # lowest ampl over interburst_min ms on the right
      local_min_right = min (envelope$value[i:(i + n)])
    } else {
      # just in case we want to evaluate both sides of a peak
      local_min_right = 0
    }

    # define the window for analysis (differs from ± interburst_min because we have to consider the beginning and end of file)
    if (i > n) {
      limit_left = i - n
    } else {
      limit_left = 1
    }
    if (i < (nrow(envelope) - n - 1)) {
      limit_right = i + n
    } else {
      limit_right = nrow(envelope)
    }

    # DEFINITION OF A BURST FOLLOWS!!!
    # (1) it is a local maximum over ± interburst_min
    cond1 = envelope$value[i] == max(envelope$value[limit_left:limit_right])
    # (2) it is above a certain % of the global maximum
    cond2 = envelope$value[i] / max(envelope$value) > peak_to_global_max
    # (3) it exceeds the local minimum on the LEFT / RIGHT by a factor of peak_to_trough
    cond3_left = ifelse(trough_left,
                        envelope$value[i] / local_min_left > peak_to_trough,
                        TRUE)  # always TRUE if we're not interested in what's left
    cond3_right = ifelse(trough_right,
                         envelope$value[i] / local_min_right > peak_to_trough,
                         TRUE)  # always TRUE if we're not interested in what's right
    if (cond1 && cond2 && cond3_left && cond3_right) {
      bursts = rbind(bursts, c(i * timestep, envelope$value[i]))
    }
  }

  # prepare output
  bursts = bursts[-1, ]  # remove the first empty row
  if (nrow(bursts) > 0) {
    bursts$interburst_int = NA
    if (nrow(bursts) > 1) {
      bursts$interburst_int[1:(nrow(bursts)-1)] = diff(bursts$time)
    }
  }

  return (bursts)
}


