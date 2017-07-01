#' Segment a sound
#'
#' Finds syllables and bursts. Syllables are defined as continous segments with
#' ampiltude above threshold. Bursts are defined as local maxima in amplitude
#' envelope that are high enough both in absolute terms (relative to the global
#' maximum) and with respect to the surrounding region (relative to local
#' mimima).
#'
#' The algorithm is very flexible, but the parameters may be hard to optimize by
#' hand. If you have an annotated sample of the sort of audio you are planning
#' to analyze, with syllables or bursts counted manually, you can use it for
#' automatic optimization of control parameters (see
#' \code{\link{optimizeSegment}}). The defaults are the results of just such
#' optimization against 260 human vocalizations in Anikin, A. & Persson, T.
#' (2017). Non-linguistic vocalizations from online amateur videos for emotion
#' research: a validated corpus. Behavior Research Methods, 49(2): 758-771.
#' @param x path to a .wav file or a vector of amplitudes with specified
#'   samplingRate
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than a .wav file)
#' @param shortest_syl minimum acceptable length of syllables (ms)
#' @param shortest_pause minimum acceptable break between syllables (ms).
#'   Syllables separated by less time are merged. To avoid merging, specify
#'   \code{shortest_pause = NA}
#' @param syl_to_global_mean amplitude threshold for syllable detection (as a
#'   proportion of global mean amplitude of smoothed envelope)
#' @param interburst_min_ms minimum time between two consecutive bursts (ms). If
#'   specified, it overrides \code{interburst_min_idx}
#' @param interburst_min_scale multiplier of the default minimum interburst
#'   interval (median syllable length or, if no syllables are detected, the same
#'   number as \code{shortest_syl}). Only used if \code{interburst_min_ms} is
#'   not specified. Larger values improve detection of unusually broad shallow
#'   peaks, while smaller values improve the detection of sharp narrow peaks
#' @param peak_to_global_max to qualify as a burst, a local maximum has to be at
#'   least \code{peak_to_global_max} time the height of the global maximum of
#'   the entire amplitude envelope
#' @param peak_to_trough to qualify as a burst, a local maximum has to be at
#'   least \code{peak_to_trough}  times the local minimum on the LEFT over
#'   analysis window (which is controlled by \code{interburst_min_ms} or
#'   \code{interburst_min_scale})
#' @param trough_left,trough_right should local maxima be compared to the trough
#'   on the left and/or right of it? TRUE / FALSE
#' @param smooth_ms length of smoothing window (ms). Capped at half the length
#'   of sound. Low values dramatically increase processing time
#' @param smooth_overlap overlap between smoothing windows (%): the higher, the
#'   more accurate, but also slower
#' @param summary if TRUE, returns only a summary of the number and spacing of
#'   syllables and vocal bursts. If FALSE, returns a list containing full stats
#'   on each syllable and bursts (location, duration, amplitude, ...)
#' @param plot should a segmentation plot be plotted? TRUE / FALSE
#' @param savePath full path to the folder in which to save the plots. If you
#'   don't want to save the plots, set \code{savePath} to NA (default)
#' @param ... other graphical parameters
#' @return Returns a dataframe with one row and 8 columns summarizing
#' @export
#' @examples
#' sound = generateBout(nSyl = 8, sylDur_mean = 50, pauseDur_mean = 70,
#'   pitchAnchors = list(time = c(0, 1), value = c(368, 284)), temperature = 0.1,
#'   attackLen = 10, exactFormants = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100),
#'   f2 = list(time = 0, freq = 1600, amp = 30, width = 100),
#'   f3 = list(time = 0, freq = 3100, amp = 30, width = 100),
#'   f4 = list(time = 0, freq = 3900, amp = 30, width = 100)),
#'   breathingAnchors = list(time = c(0, 67, 86, 186), value = c(-45, -47, -89, -120)),
#'   rolloff_breathing = -8, amplAnchors_global = list(time = c(0, 1), value = c(120, 20)))
#' spec (sound, samplingRate = 16000, osc = TRUE)
#'  # playme(sound, samplingRate = 16000)
#'
#' s = segment(sound, samplingRate = 16000, plot = TRUE)
#' # accept quicker and quieter syllables
#' s = segment(sound, samplingRate = 16000, plot = TRUE,
#'   shortest_syl = 25, shortest_pause = 25, syl_to_global_mean = .6)
#' # look for narrower, sharper bursts
#' s = segment(sound, samplingRate = 16000, plot = TRUE,
#'   shortest_syl = 25, shortest_pause = 25, syl_to_global_mean = .6,
#'   interburst_min_scale = 1)
segment = function(x,
                   samplingRate = NULL,
                   shortest_syl = 40,
                   shortest_pause = 40,
                   syl_to_global_mean = 0.9,
                   interburst_min_ms = NULL,
                   interburst_min_scale = 1,
                   peak_to_global_max = 0.075,
                   peak_to_trough = 3,
                   trough_left = TRUE,
                   trough_right = FALSE,
                   smooth_ms = 40,
                   smooth_overlap = 80,
                   summary = FALSE,
                   plot = FALSE,
                   savePath = NA,
                   ...) {
  merge_close_syl = ifelse(is.null(shortest_pause) || is.na(shortest_pause), F, T)
  if (smooth_ms < 10) {
    warning('smooth_ms < 10 ms is slow and usually not very useful')
  }
  if (smooth_overlap < 0) smooth_overlap = 0
  if (smooth_overlap > 99) smooth_overlap = 99

  ## import a sound
  if (class(x) == 'character') {
    sound = tuneR::readWave(x)
    samplingRate = sound@samp.rate
    sound = sound@left
    plotname = tail(unlist(strsplit(x, '/')), n = 1)
    plotname = substring(plotname, 1, nchar(plotname) - 4)
  }  else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
      plotname = ''
    }
  }

  ## normalize
  if (min(sound) > 0) {
    sound = scale(sound)
  }
  sound = sound / max(abs(max(sound)), abs(min(sound)))
  # plot(sound, type='l')

  ## extract amplitude envelope
  smooth_points = ceiling(smooth_ms * samplingRate / 1000)
  if (smooth_points > length(sound) / 2) {
    smooth_points = length(sound) / 2
  }

  sound_downsampled = seewave::env(
    sound,
    f = samplingRate,
    msmooth = c(smooth_points, smooth_overlap),
    fftw = TRUE,
    plot = FALSE
  )
  timestep = 1000 / samplingRate *
    (length(sound) / length(sound_downsampled)) # time step in the envelope, ms
  envelope = data.frame (time = ( (1:length(sound_downsampled) - 1) * timestep),
                         value = sound_downsampled)
  # plot (envelope, type='l')

  ## find syllables and get descriptives
  threshold = mean(envelope$value) * syl_to_global_mean
  syllables = findSyllables(envelope = envelope,
                            timestep = timestep,
                            threshold = threshold,
                            shortest_syl = shortest_syl,
                            shortest_pause = shortest_pause,
                            merge_close_syl = merge_close_syl)

  ## find bursts and get descriptives
  # calculate the window for analyzing bursts based on the median duration of
  # syllables (if no syllables are detected, just use the specified shortest
  # acceptable syllable length)
  if (is.null(interburst_min_ms)) {
    median_scaled = median(syllables$dur) * interburst_min_scale
    interburst_min_ms = ifelse(!is.na(median_scaled),
                               median_scaled,
                               shortest_syl)
  }
  bursts = findBursts(envelope = envelope,
                      timestep = timestep,
                      interburst_min_ms = interburst_min_ms,
                      peak_to_global_max = peak_to_global_max,
                      peak_to_trough = peak_to_trough,
                      trough_left = trough_left,
                      trough_right = trough_right
  )

  ## prepare a dataframe containing descriptives for syllables and bursts
  result = data.frame(
    nSyllables = nrow(syllables),
    syllableLength_mean = suppressWarnings(mean(syllables$dur)),
    syllableLength_median = ifelse(nrow(syllables) > 0,
                                   median(syllables$dur),
                                   NA),  # otherwise returns NULL
    syllableLength_sd = sd(syllables$dur),
    nBursts = nrow(bursts),
    interBurst_mean = suppressWarnings(mean(bursts$interburst_int, na.rm = TRUE)),
    interBurst_median = ifelse(nrow(bursts) > 0,
                               median(bursts$interburst_int, na.rm = TRUE),
                               NA),  # otherwise returns NULL
    interBurst_sd = sd(bursts$interburst_int, na.rm = TRUE)
  )

  ## plotting (optional)
  if (is.character(savePath)) plot = TRUE
  if (plot) {
    if (is.character(savePath)) {
      jpeg(filename = paste0 (savePath, plotname, ".jpg"), 900, 500)
    }
    plot (envelope$time, envelope$value, type = 'l', col = 'green',
          xlab = 'Time, ms', ylab = 'Amplitude', main = plotname, ...)
    points (bursts, col = 'red', cex = 3, pch = 8)
    for (s in 1:nrow(syllables)) {
      segments( x0 = syllables$time_start[s], y0 = threshold,
                x1 = syllables$time_end[s], y1 = threshold,
                lwd = 2, col = 'blue')
    }
    if (!is.na(savePath)){
      dev.off()
    }
  }

  if (summary) {
    return(result)
  } else {
    return(list(syllables = syllables, bursts = bursts))
  }
}


#' Segment all files in a folder
#'
#' Finds syllables and bursts in all .wav files in a folder.
#'
#' This is just a convenient wrapper for \code{\link{segment}} intended for
#' analyzing the syllables and bursts in a large number of audio files at a
#' time. In verbose mode, it also reports ETA every ten iterations. With default
#' settings, running time should be about a second per minute of audio.
#'
#' @param myfolder full path to target folder
#' @inheritParams segment
#' @param verbose if TRUE, reports progress and estimated time left
#' @return If \code{summary} is TRUE, returns a dataframe with one row per audio
#'   file. If \code{summary} is FALSE, returns a list of detailed descriptives.
#' @export
#' @examples
#' \dontrun{
#' # download 260 sounds from Anikin & Persson (2017)
#' # http://cogsci.se/personal/results/
#' # 01_anikin-persson_2016_naturalistics-non-linguistic-vocalizations/260sounds_wav.zip
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp'  # 260 .wav files live here
#' s = segmentFolder(myfolder, verbose = TRUE)
#'
#' # Check accuracy: import a manual count of syllables (our "key")
#' key = segment_manual  # a vector of 260 integers
#' trial = as.numeric(s$nBursts)
#' cor(key, trial, use = 'pairwise.complete.obs')
#' boxplot(trial ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#' }
segmentFolder = function (myfolder,
                          shortest_syl = 40,
                          shortest_pause = 40,
                          syl_to_global_mean = 0.9,
                          interburst_min_ms = NULL,
                          interburst_min_scale = 1,
                          peak_to_global_max = 0.075,
                          peak_to_trough = 3,
                          trough_left = TRUE,
                          trough_right = FALSE,
                          smooth_ms = 40,
                          smooth_overlap = 80,
                          summary = TRUE,
                          plot = FALSE,
                          savePath = NA,
                          verbose = TRUE,
                          ...) {
  time_start = proc.time()  # timing
  # open all .wav files in folder
  filenames = list.files(myfolder, pattern = "*.wav", full.names = TRUE)
  result = list()

  for (i in 1:length(filenames)) {
    result[[i]] = segment(
        filenames[i],
        shortest_syl = shortest_syl,
        shortest_pause = shortest_pause,
        syl_to_global_mean = syl_to_global_mean,
        interburst_min_ms = interburst_min_ms,
        interburst_min_scale = interburst_min_scale,
        peak_to_global_max = peak_to_global_max,
        peak_to_trough = peak_to_trough,
        trough_left = trough_left,
        trough_right = trough_right,
        smooth_ms = smooth_ms,
        smooth_overlap = smooth_overlap,
        plot = plot,
        savePath = savePath,
        summary = summary
      )

    if (verbose) {
      if (i %% 10 == 0) {
        time_diff = as.numeric((proc.time() - time_start)[3])
        time_left = time_diff / i * (length(filenames) - i)
        time_left_hms = convert_sec_to_hms(time_left)
        print(paste0('Done ', i, ' / ', length(filenames),
                     '; Estimated time left: ', time_left_hms))
      }
    }
  }

  # prepare output
  if (summary == TRUE) {
    output = as.data.frame(t(sapply(result, rbind)))
    output$sound = apply(matrix(1:length(filenames)), 1, function(x) {
      tail(unlist(strsplit(filenames[x], '/')), 1)
    })
    output = output[, c(9, 1:8)]
  } else {
    output = result
    names(output) = filenames
  }

  if (verbose) {
    total_time = as.numeric((proc.time() - time_start)[3])
    total_time_hms = convert_sec_to_hms(total_time)
    print(paste0('Analyzed ', i, ' files in ', total_time_hms))
  }
  return (output)
}
