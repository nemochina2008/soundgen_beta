### UTILITIES FOR SOUND GENERATION ###

## TODO: fine-tune saveme for compatibility with html

#' Report time
#'
#' Internal soudgen function.
#'
#' Based on the current iteration, total number of iterations, and time when the
#' loop started running, prints estimated time left or a summary upon
#' completion.
#' @param i current iteration
#' @param nIter total number of iterations
#' @param time_start time when the loop started running
#' @param jobs vector of length \code{nIter} specifying the relative difficulty
#'   of each iteration. If not NULL, estimated time left takes into account
#'   whether the jobs ahead will take more or less time than the jobs already
#'   completed
#' @examples
#' \dontrun{
#' time_start = proc.time()
#' for (i in 1:5) {
#'   Sys.sleep(i ^ 2 / 10)
#'   reportTime(i = i, nIter = 5, time_start = time_start, jobs = (1:5) ^ 2 / 10)
#' }
#' }
reportTime = function(i, nIter, time_start, jobs = NULL) {
  time_diff = as.numeric((proc.time() - time_start)[3])
  if (i == nIter) {
    time_total = convert_sec_to_hms(time_diff)
    print(paste0('Completed ', i, ' iterations in ', time_total, '.'))
  } else {
    if (is.null(jobs)) {
      # simply count iterations
      time_left = time_diff / i * (nIter - i)
    } else {
      # take into account the expected time for each iteration
      speed = time_diff / sum(jobs[1:i])
      time_left = speed * sum(jobs[min((i + 1), nIter):nIter])
    }
    time_left_hms = convert_sec_to_hms(time_left)
    print(paste0('Done ', i, ' / ', nIter, '; Estimated time left: ', time_left_hms))
  }
}


#' Print time
#'
#' Internal soundgen function.
#'
#' Converts time in seconds to time in hh:mm:ss for pretty printing.
#' @param time_s time (s)
#' @return Returns a character string like "1 h 20 min 3 s"
#' @examples
#' time_start = proc.time()
#' Sys.sleep(1)
#' time_diff = as.numeric((proc.time() - time_start)[3])
#' soundgen:::convert_sec_to_hms(time_diff)
convert_sec_to_hms = function(time_s) {
  hours = time_s %/% 3600
  minutes = time_s %/% 60 - hours * 60
  seconds = round(time_s %% 60, 0)

  output = ''
  if (hours > 0) output = paste0(output, hours, ' h ')
  if (minutes > 0) output = paste0(output, minutes, ' min ')
  output = paste0(output, seconds, ' s')

  # remove the last space, if any
  if (substr(output, nchar(output), nchar(output)) == ' ') {
    output = substr(output, 1, nchar(output)-1)
  }
  return(output)
}


#' Play audio
#'
#' Plays an audio file or a numeric vector. This is a simple wrapper for the
#' functionality provided by tuneR package.
#' @param sound a vector of numbers on any scale or a path to a .wav file
#' @param samplingRate sampling rate (only needed if sound is a vector)
#' @export
#' @examples
#' \dontrun{playme('~/myfile.wav')}
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' playme(sound, 16000)
playme = function(sound, samplingRate = 16000) {
  # input: a vector of numbers on any scale or a path to a .wav file
  if (class(sound) == 'character') {
    soundWave = tuneR::readWave(sound)
  } else if (class(sound) == 'numeric' | class(sound) == 'integer') {
    soundWave = tuneR::Wave(
      left = sound,
      samp.rate = samplingRate,
      bit = 16,
      pcm = TRUE
    )
    soundWave = tuneR::normalize(soundWave, unit = '32') # / 2
  }
  tuneR::play(soundWave, 'play')
  # can't get rid of printed output! sink(), capture.output, invisible don't work!!!
}


#' Prepare a list of formants
#'
#' Internal soundgen function.
#'
#' Takes a string of phonemes entered WITHOUT ANY BREAKS. Recognized phonemes in
#' the human preset dictionary: vowels "a" "o" "i" "e" "u" "0" (schwa);
#' consonants "s" "x" "j".
#' @param phonemeString a string of characters from the dictionary of phoneme
#'   presets, e.g., uaaaaii (short u - longer a - medium-long i)
#' @param speaker name of the preset dictionary to use
#' @return Returns a list of formant values, which can be fed directly into
#'   \code{\link{getSpectralEnvelope}}
#' @examples
#' exactFormants = soundgen:::convertStringToFormants(
#'   phonemeString = 'aaeuiiiii', speaker = 'M1')
convertStringToFormants = function(phonemeString, speaker = 'M1') {
  availablePresets = names(presets[[speaker]]$Formants)
  input_phonemes = strsplit(phonemeString, "")[[1]]
  valid_phonemes = input_phonemes[input_phonemes %in% availablePresets]
  unique_phonemes = unique(valid_phonemes)
  if (length(valid_phonemes) < 1)
    return(list())

  # for each input vowel, look up the corresponding formant values
  # in the presets dictionary and append to exactFormants
  vowels = list()
  formantNames = character()
  for (v in 1:length(unique_phonemes)) {
    vowels[[v]] = presets[[speaker]]$Formants[unique_phonemes[v]][[1]]
    formantNames = c(formantNames, names(vowels[[v]]))
  }
  formantNames = sort(unique(formantNames))
  names(vowels) = unique_phonemes

  # make sure we have filled in info on all formants from the entire sequence of vowels for each individual vowel
  for (v in 1:length(vowels)) {
    absentFormants = formantNames[!formantNames %in% names(vowels[[v]])]
    for (f in absentFormants) {
      closestFreq = unlist(sapply(vowels, function(x) x[f]))
      names_stripped = substr(names(closestFreq),
                              nchar(names(closestFreq)) - 3,
                              nchar(names(closestFreq)))
      closestFreq = closestFreq[which(names_stripped == 'freq')]
      closestFreq = as.numeric(na.omit(closestFreq))[1]
      # NB: instead of the last [1], ideally we should specify some intelligent
      # way to pick up the closest vowel with this missing formant, not just the
      # first one, but that's only a problem in long sequences of vowels with
      # really different numbers of formants (nasalization)
      vowels[[v]] [[f]] = data.frame(
        'time' = 0,
        'freq' = closestFreq,
        'amp' = 0,
        'width' = 100
      )
      # NB: width must be positive, otherwise dgamma crashes in
      # getSpectralEnvelope()
    }
  }

  # initialize a common list of exact formants
  exactFormants = list()
  for (f in 1:length(formantNames)) {
    exactFormants[[f]] = data.frame(
      'time' = vector(),
      'freq' = vector(),
      'amp' = vector(),
      'width' = vector()
    )
  }
  names(exactFormants) = formantNames

  # for each vowel, append its formants to the common list
  for (v in 1:length(valid_phonemes)) {
    vowel = vowels[[valid_phonemes[v]]]
    for (f in 1:length(vowel)) {
      formantName = names(vowel)[f]
      exactFormants[[formantName]] = rbind(exactFormants[[formantName]],
                                           vowel[[f]])
    }
  }

  # specify time stamps by dividing the sound equally into vowels in valid_phonemes
  time_stamps = seq(0, 1, length.out = length(valid_phonemes))
  for (f in 1:length(exactFormants)) {
    if (nrow(exactFormants[[f]]) > 0) {
      exactFormants[[f]]$time = time_stamps
    }
  }

  # remove formants with amplitude 0 at all time points
  all_zeroes = sapply(exactFormants, function(f) {
    sum(f$amp == 0) == length(f) # all values are 0
  })
  exactFormants = exactFormants [which(!all_zeroes), drop = FALSE]
  return (exactFormants)
}



#' Find zero crossing
#'
#' Internal soundgen function.
#'
#' \code{findZeroCrossing} looks for the last negative point before a zero
#' crossing as close as possible to the specified location. Since this is
#' primarily intended for joining waveforms without a click, this function only
#' looks at upward segments of a waveform (see example).
#'
#' @param ampl a vector of amplitudes oscillating around zero, such as a sound
#'   waveform
#' @param location the index indicating the desired location of a zero crossing
#' @return Returns the index of the last negative value before zero crossing
#'   closest to specified location.
#' @examples
#' ampl = sin(1:100/2)
#' plot(ampl, type = 'b')
#' lines(1:100, rep(0,100), lty = 2)
#' zc = vector()
#' for (i in 1:length(ampl)){
#'   zc[i] = soundgen:::findZeroCrossing (ampl, i)
#'   # find zc closest to each of 100 points
#' }
#' for (z in unique(zc)){
#'   points(z, ampl[z], col = 'red', pch = 17)
#'   # only on upward segments
#' }
#' zc # see which zc is closest to each point
findZeroCrossing = function(ampl, location) {
  len = length(ampl)
  if (len < 1 | location < 1 | location > len)
    return (NA)
  if (len == 1 & location == 1)
    return(location)
  zc_left = zc_right = NA

  # left of location
  if (location > 1) {
    i = location
    while (i > 1) {
      if (ampl[i] > 0 && ampl[i - 1] < 0) {
        zc_left = i - 1
        break
      }
      i = i - 1
    }
  }

  # right of location
  if (location < len)
    i = location
  while (i < (len - 1)) {
    if (ampl[i + 1] > 0 && ampl[i] < 0) {
      zc_right = i
      break
    }
    i = i + 1
  }

  if (is.na(zc_left) & is.na(zc_right)) return (NA)
  zc_nearest = which.min(c(abs(zc_left - location), abs(zc_right - location)))
  if (zc_nearest == 1) {
    return (zc_left)
  } else if (zc_nearest == 2) {
    return (zc_right)
  } else {
    return (NA) # zc not found
  }
}


#' Join two waveforms by cross-fading
#'
#' \code{crossFade} joins two input vectors (waveforms) by cross-fading. It
#' truncates both input vectors, so that ampl1 ends with a zero crossing and
#' ampl2 starts with a zero crossing. Then it cross-fades both vectors linearly
#' with an overlap of length_ms or length_points. If the input vectors are too
#' short for the specified length of cross-faded region, the two vectors are
#' concatenated at zero crossings instead of cross-fading. Soundgen uses
#' \code{crossFade} for gluing together epochs in generateSyllable()
#'
#' @param ampl1,ampl2 two numeric vectors (waveforms) to be joined
#' @param length_ms the length of overlap, in ms (doesn't need to be specified
#'   if length_points is not NULL)
#' @param length_points (optional) the length of overlap, in points (defaults to
#'   NULL)
#' @param samplingRate the sampling rate of input vectors, in Hz
#' @export
#' @return Returns the index of the last negative value before zero crossing
#'   closest to specified location.
#' @examples
#' sound1 = sin(1:100 / 9)
#' sound2 = sin(7:107 / 3)
#' plot(c(sound1, sound2), type = 'b') # an ugly discontinuity
#' #  at 100 that will make an audible click
#' sound = crossFade(sound1, sound2, length_points = 5)
#' plot(sound, type = 'b') # a nice, smooth transition
#' length(sound) # but note that cross-fading costs us ~60 points
#' #  because of trimming to zero crossings
crossFade = function (ampl1,
                      ampl2,
                      length_ms = 15,
                      samplingRate = 44100,
                      length_points = NULL) {
  # cut to the nearest zero crossings
  zc1 = findZeroCrossing(ampl1, location = length(ampl1))
  if (!is.na(zc1)) {
    ampl1 = c (ampl1[1:zc1], 0) # up to the last point before the last zero-crossing in sound 1 on the upward curve + one extra zero (to have a nice, smooth transition line: last negative in s1 - zero - first positive in s2)
  }
  zc2 = findZeroCrossing(ampl2, location = 1)
  if (!is.na(zc2)) {
    ampl2 = ampl2[(zc2 + 1):length(ampl2)]
    # from the first positive point on the upward curve. Note the +1 - next
    # point after the first zero crossing in s2
  }

  # check whether there is enough data to cross-fade. Note that ampl1 or ampl2
  # may even become shorter than length_points after we shortened them to the
  # nearest zero crossing
  if (is.null(length_points)) {
    length_points = min(floor(length_ms * samplingRate / 1000),
                        length(ampl1) - 1,
                        length(ampl2) - 1)
  } else {
    length_points = min(length_points, length(ampl1) - 1, length(ampl2) - 1)
  }

  # concatenate or cross-fade
  if (length_points < 2) {
    # for segments that are too short,
    # just concatenate from zero crossing to zero crossing
    ampl = c(ampl1, ampl2)
  } else {
    # for segments that are long enough, cross-fade properly
    multipl = seq(0, 1, length.out = length_points)
    idx1 = length(ampl1) - length_points
    cross = rev(multipl) * ampl1[(idx1 + 1):length(ampl1)] +
            multipl * ampl2[1:length_points]
    ampl = c(ampl1[1:idx1],
             cross,
             ampl2[(length_points + 1):length(ampl2)])
  }
  return (ampl)
}


#' Upsample pitch contour
#'
#' Internal soundgen function.
#'
#' Upsamples a pitch contour to samplingRate through linear interpolation
#' between successive glottal cycles.
#' @param pitch_per_gc a vector of fundamental frequencies per glottal cycle
#' @param samplingRate target sampling rate after upsampling, in Hz
#' @return Returns a list of two vectors: pitch_upsampled (the upsampled version
#'   of the input) and gc_upsampled (new indices of glottal cycles on an
#'   upsampled scale)
#' @examples
#' soundgen:::upsample(pitch_per_gc = c(100, 150, 130), samplingRate = 16000)
upsample = function(pitch_per_gc, samplingRate = 16000) {
  l = length(pitch_per_gc)
  gcLength_points = round(samplingRate / pitch_per_gc)
  c = cumsum(gcLength_points)
  gc_upsampled = c(1, c)

  if (l == 1) {
    pitch_upsampled = rep(pitch_per_gc, gcLength_points)
  } else if (l == 2) {
    pitch_upsampled = seq(pitch_per_gc[1], pitch_per_gc[2], length.out = sum(gcLength_points))
  } else {
    # find time stamps (in gc) corresponding to centers of each pitch value
    t = rep(1, l)
    t[1] = 1  # start at 1
    t[l] = sum(gcLength_points)  # end at total number of gc
    for (i in 2:(l - 1)) {
      t[i] = c[i - 1] + round(gcLength_points[i] / 2)
    }
    pitch_upsampled = spline(x = t,
                             y = pitch_per_gc,
                             n = tail(c, 1))$y
  }
  # plot(pitch_upsampled, type = 'l')
  return (list(pitch = pitch_upsampled, gc = gc_upsampled))
}


#' Fade-in and fade-out
#'
#' Internal soundgen function.
#'
#' Applies linear fade-in and fade-out of length 'length_fade' points to one or
#' both ends of input vector.
#' @param ampl numeric vector such as a waveform
#' @param do_fadeIn,do_fadeOut (logical) perform linear fade-in / fade-out?
#' @param length_fade the length of affected region, in points (expects an
#'   integer > 1, otherwise just returns the original vector with no
#'   modifications)
#' @return Returns a numeric vector of the same length as input
#' @examples
#' ampl = sin(1:1000)
#' plot(soundgen:::fadeInOut(ampl, length_fade = 100), type = 'l')
#' plot(soundgen:::fadeInOut(ampl, length_fade = 300,
#'   do_fadeOut = FALSE), type = 'l')
#' # if the vector is shorter than twice the specified length_fade,
#' # fade-in/out regions overlap
#' plot(soundgen:::fadeInOut(ampl, length_fade = 700), type = 'l')
fadeInOut = function(ampl,
                     do_fadeIn = TRUE,
                     do_fadeOut = TRUE,
                     length_fade = 1000) {
  if ((!do_fadeIn & !do_fadeOut) | length_fade < 2) return(ampl)

  length_fade = min(length_fade, length(ampl))
  fadeIn = seq(0, 1, length.out = length_fade)
  if (do_fadeIn) {
    ampl[1:length_fade] = ampl[1:length_fade] * fadeIn
  }

  if (do_fadeOut) {
    fadeOut = rev(fadeIn)
    ampl[(length(ampl) - length_fade + 1):length(ampl)] =
      ampl[(length(ampl) - length_fade + 1):length(ampl)] * fadeOut
  }

  return (ampl)
}


#' Divide f0 contour into glottal cycles
#'
#' Internal soundgen function.
#'
#' Returns a vector of indices giving the borders between "glottal cycles",
#' assuming that we know the true f0 at each time point (as we do in synthesized
#' sounds) and that maximum amplitude gives us the center of a glottal cycle.
#' The first index is always 1.
#' @param pitch a vector of fundamental frequency values
#' @param samplingRate sampling rate at which f0 values are provided
#' @examples
#' # 100 ms of audio with f0 steadily increasing from 150 to 200 Hz
#' soundgen:::getGlottalCycles (seq(150, 200, length.out = 350),
#'   samplingRate = 3500)
getGlottalCycles = function (pitch, samplingRate = 44100) {
  glottalCycles = numeric()
  i = 1 # the first border is the first time point
  while (i < length(pitch)) {
    glottalCycles = c(glottalCycles, i)
    # take steps proportionate to the current F0
    i = i + max(2, floor(samplingRate / pitch[i]))
  }
  return (glottalCycles)
}


#' Syllable structure of a bout
#'
#' Internal soundgen function.
#'
#' Stochastic generation of syllable structure of a bout. Calls
#' \code{\link{rnorm_bounded}} to vary the duration of each new syllable and of
#' pauses between syllables. Total bout duration will also vary, unless
#' temperature is zero.
#' @param nSyl the desired number of syllables
#' @param sylLen the desired mean syllable duration, in ms
#' @param pauseLen the desired mean pause between syllables, in ms
#' @param sylDur_min,sylDur_max the lower and upper bounds on possible syllable
#'   duration, in ms
#' @param pauseDur_min,pauseDur_max the lower and upper bounds on possible pause
#'   duration, in ms
#' @param temperature a non-negative float regulating the stochasticity of
#'   syllable segmentation; 0 = no stochasticity; 1 = sd of proposals is equal
#'   to sylLen (very strong stochasticity)
#' @param plot produce a plot of syllable structure?
#' @return Returns a matrix with a list of start-end points for syllables
#' @examples
#' soundgen:::divideIntoSyllables (nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0.2, plot = TRUE)
#' soundgen:::divideIntoSyllables (nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0, plot = TRUE)
divideIntoSyllables = function (nSyl,
                                sylLen,
                                pauseLen,
                                sylDur_min = 20,
                                sylDur_max = 10000,
                                pauseDur_min = 20,
                                pauseDur_max = 1000,
                                temperature = 0.025,
                                plot = FALSE) {
  out = matrix(ncol = 2, nrow = 0)
  colnames(out) = c('start', 'end')
  if (nSyl == 1) {
    out = rbind(out, c(0, sylLen))
  } else {
    # generate random lengths while respecting the constraints
    c = 0
    while (nrow(out) < nSyl) {
      duration_ms_loop = rnorm_bounded(
        n = 1,
        mean = sylLen,
        low = sylDur_min,
        high = sylDur_max,
        sd = sylLen * temperature
      )
      pause_ms_loop = rnorm_bounded(
        n = 1,
        mean = pauseLen,
        low = pauseDur_min,
        high = pauseDur_max,
        sd = pauseLen * temperature
      )
      start = 1 + c # start time of syllable, in ms
      end = start + duration_ms_loop # end time of syllable, in ms
      out = rbind(out, c(start, end))
      c = end + pause_ms_loop
    }
  }

  if (plot) {
    # for the UI
    t = 1:max(out)
    plot(t, rep(1, length(t)), type = 'n', xlab = 'Time, ms', ylab = '',
         bty = 'n', yaxt = 'n', ylim = c(0.8, 1.2))
    for (i in 1:nrow(out)) {
      rect(xleft = out[i, 1], xright = out[i, 2], ybottom = .9, ytop = 1.1,
           col = 'blue')
      text(x = mean(c(out[i, 2], out[i, 1])), y = 1,
        col = 'yellow', cex = 5, labels = i)
    }
  }
  return(out)
}
