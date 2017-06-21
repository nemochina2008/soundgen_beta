## basic utilities

## TODO: check upsample (maybe we should not hold pitch constant in the last gc); non-linear cross-fade; fine-tune saveme for compatibility

## conversion from Hz to musical notes (for UI only). See http://www.phy.mtu.edu/~suits/notefreqs.html for reference table. The commented-out section shows how the dictionary was originally compiled
# notes = c('C','C\U266F','D','D\U266F','E','F','F\U266F','G','G\U266F','A','B\U266D','B')
# nOct = 11
# notes_all = paste0(notes,rep(0:(nOct-1),each=12)) # paste0(notes_all, collapse="','")
# c0 = 13.75 * 2^(3/12) # 16.3516 Hz exactly. 440/32 = 13.75 A-1, and C0 is 3 semitones higher
# notes_freq = round ( c0 * 2^(0:(12*nOct-1)/12), 1 ) # paste0(notes_freq, collapse=',')
notes_dict = data.frame (note = c('C0','C♯0','D0','D♯0','E0','F0','F♯0','G0','G♯0','A0','B♭0','B0','C1','C♯1','D1','D♯1','E1','F1','F♯1','G1','G♯1','A1','B♭1','B1','C2','C♯2','D2','D♯2','E2','F2','F♯2','G2','G♯2','A2','B♭2','B2','C3','C♯3','D3','D♯3','E3','F3','F♯3','G3','G♯3','A3','B♭3','B3','C4','C♯4','D4','D♯4','E4','F4','F♯4','G4','G♯4','A4','B♭4','B4','C5','C♯5','D5','D♯5','E5','F5','F♯5','G5','G♯5','A5','B♭5','B5','C6','C♯6','D6','D♯6','E6','F6','F♯6','G6','G♯6','A6','B♭6','B6','C7','C♯7','D7','D♯7','E7','F7','F♯7','G7','G♯7','A7','B♭7','B7','C8','C♯8','D8','D♯8','E8','F8','F♯8','G8','G♯8','A8','B♭8','B8','C9','C♯9','D9','D♯9','E9','F9','F♯9','G9','G♯9','A9','B♭9','B9','C10','C♯10','D10','D♯10','E10','F10','F♯10','G10','G♯10','A10','B♭10','B10'),
                         freq = c(16.4,17.3,18.4,19.4,20.6,21.8,23.1,24.5,26,27.5,29.1,30.9,32.7,34.6,36.7,38.9,41.2,43.7,46.2,49,51.9,55,58.3,61.7,65.4,69.3,73.4,77.8,82.4,87.3,92.5,98,103.8,110,116.5,123.5,130.8,138.6,146.8,155.6,164.8,174.6,185,196,207.7,220,233.1,246.9,261.6,277.2,293.7,311.1,329.6,349.2,370,392,415.3,440,466.2,493.9,523.3,554.4,587.3,622.3,659.3,698.5,740,784,830.6,880,932.3,987.8,1046.5,1108.7,1174.7,1244.5,1318.5,1396.9,1480,1568,1661.2,1760,1864.7,1975.5,2093,2217.5,2349.3,2489,2637,2793.8,2960,3136,3322.4,3520,3729.3,3951.1,4186,4434.9,4698.6,4978,5274,5587.7,5919.9,6271.9,6644.9,7040,7458.6,7902.1,8372,8869.8,9397.3,9956.1,10548.1,11175.3,11839.8,12543.9,13289.8,14080,14917.2,15804.3,16744,17739.7,18794.5,19912.1,21096.2,22350.6,23679.6,25087.7,26579.5,28160,29834.5,31608.5), stringsAsFactors=F)

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
#' playme(sin(2*pi*f0_Hz*(1:16000)/16000), samplingRate=16000)
playme = function(sound, samplingRate = 44100) {
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


#' Random draw from a truncated normal distribution
#'
#' \code{rnorm_bounded} generates random numbers from a normal distribution
#' using rnorm(), but forced to remain within the specified low/high bounds. All
#' proposals outside the boundaries (exclusive) are discarded, and the sampling
#' is repeated until there are enough values within the specified range. Fully
#' vectorized.
#'
#' @param n the number of values to return
#' @param mean the mean of the normal distribution from which values are
#'   generated (vector of length 1 or n)
#' @param sd the standard deviation of the normal distribution from which values
#'   are generated (vector of length 1 or n)
#' @param low,high exclusive lower and upper bounds ((vectors of length 1 or n))
#' @param roundToInteger boolean vector of length 1 or n. If TRUE, the
#'   corresponding value is rounded to the nearest integer.
#' @return A vector of length n.
#' @examples
#' soundgen:::rnorm_bounded (n = 3, mean = 10, sd = 5, low = 7, high = NULL,
#'   roundToInteger = c(TRUE, FALSE, FALSE))
#' soundgen:::rnorm_bounded (n = 3, mean = c(10, 50, 100), sd = c(5, 0, 20),
#'   roundToInteger = TRUE) # vectorized
rnorm_bounded = function(n = 1,
                         mean = 0,
                         sd = 1,
                         low = NULL,
                         high = NULL,
                         roundToInteger = FALSE) {
  if (sum(mean > high | mean < low) > 0) {
    warning('Some of the specified means are outside the low/high bounds!')
  }
  if (sum(sd != 0) == 0) {
    out = rep(mean, n)
    out[roundToInteger] = round (out[roundToInteger], 0)
    return (out)
  }

  if (length(mean) < n) mean = rep(mean[1], n)
  if (length(sd) < n) sd = rep(sd[1], n)

  if (is.null(low) & is.null(high)) {
    out = rnorm(n, mean, sd)
    out[roundToInteger] = round (out[roundToInteger], 0)
    return (out)
  }

  if (is.null(low)) low = rep(-Inf, n)
  if (is.null(high)) high = rep(Inf, n)
  if (length(low) == 1) low = rep(low, n)
  if (length(high) == 1) high = rep(high, n)

  out = rnorm(n, mean, sd)
  out[roundToInteger] = round (out[roundToInteger], 0)
  for (i in 1:n) {
    while (out[i] < low[i] | out[i] > high[i]) {
      out[i] = rnorm(1, mean[i], sd[i]) # repeat until a suitable value is generated
      out[roundToInteger] = round (out[roundToInteger], 0)
    }
  }
  out
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


#' Clump a sequence into large segments
#'
#' Internal soundgen function.
#'
#' \code{clumper} makes sure each homogeneous segment in a sequence is at least
#' minLength long. Called by getBinaryRandomWalk() and getVocalFry(). Algorithm:
#' go through the sequence once. If a short segment is encountered, it is pooled
#' with the previous one (i.e., the currently evaluated segment grows until it
#' is long enough, which may shorten the following segment). Finally, the last
#' segment is checked separately. This is CRUDE - a smart implementation is
#' pending!
#' @keywords internal
#' @param s a vector (soundgen supplies integers, but \code{clumper} also works
#'   on a vector of floats, characters or booleans)
#' @param minLength an integer or vector of integers indicating the desired
#'   length of a segment at each position (can vary with time, e.g., if we are
#'   processing pitch_per_gc values)
#' @return Returns the original sequence s transformed to homogeneous segments
#'   of required length.
#' @examples
#' s = c(1,3,2,2,2,0,0,4,4,1,1,1,1,1,3,3)
#' soundgen:::clumper(s, 2)
#' soundgen:::clumper(s, 3)
#' soundgen:::clumper(s, seq(1, 3, length.out = length(s)))
#' soundgen:::clumper(c('a','a','a','b','b','c','c','c','a','c'), 4)
clumper = function(s, minLength) {
  if (max(minLength) < 2) return(s)
  minLength = round(minLength) # just in case it's not all integers
  if (length(unique(s)) < 2 |
      (length(minLength) == 1 && length(s) < minLength) |
      length(s) < minLength[1]) {
    return(rep(round(median(s)), length(s)))
  }
  if (length(minLength)==1 |length(minLength)!=length(s)) {
    minLength = rep(minLength, length(s)) # upsample minLength
  }

  c = 0
  for (i in 2:length(s)) {
    if (s[i - 1] == s[i]) {
      c = c + 1
    } else {
      if (c < minLength[i]) {
        s[i] = s[i - 1] # grow the current segment until it is long enough
        c = c + 1
      } else {
        c = 1 # terminate the segment and reset the counter
      }
    }
  }

  # make sure the last epoch is also long enough
  idx_min = max((length(s) - tail(minLength, 1) + 1), 2):length(s)
  # these elements have to be homogeneous
  if (sum(s[idx_min] == tail(s, 1)) < tail(minLength, 1)) {
    # if they are not...
    idx = rev(idx_min)
    c = 1
    i = 2
    while (s[idx[i]] == s[idx[i] - 1] & i < length(idx)) {
      # count the number of repetitions for the last element
      c = c + 1
      i = i + 1
    }
    if (c < tail(minLength, 1)) {
      # if this number is insufficient,...
      s[idx] = s[min(idx_min)] # ...pool the final segment and the previous one
    }
  } # plot (s)
  return(s)
  }


#' Random walk
#'
#' Internal soundgen function.
#'
#' Generates a random walk with flexible control over its range, trend, and
#' smoothness. It works by calling \code{\link[stats]{rnorm}} at each step and
#' taking a cumulative sum of the generated values. Smoothness is controlled by
#' initially generating a shorter random walk and upsampling.
#' @param len an integer specifying the required length of random walk. If len
#'   is 1, returns a single draw from a gamma distribution with mean=1 and
#'   sd=rw_range
#' @param rw_range the upper bound of the generated random walk (the lower bound
#'   is set to 0)
#' @param rw_smoothing specifies the amount of smoothing, from 0 (no smoothing)
#'   to 1 (maximum smoothing to a straight line)
#' @param method specifies the method of smoothing: either linear interpolation
#'   ('linear', see \code{\link[stats]{approx}}) or cubic splines ('spline', see
#'   \code{\link[stats]{spline}})
#' @param trend mean of generated normal distribution (vectors are also
#'   acceptable, as long as their length is an integer multiple of len). If
#'   positive, the random walk has an overall upwards trend (good values are
#'   between 0 and 0.5 or -0.5). Trend = c(1,-1) gives a roughly bell-shaped rw
#'   with an upward and a downward curve. Larger absolute values of trend
#'   produce less and less random behavior
#' @return Returns a numeric vector of length len and range from 0 to rw_range.
#' @examples
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 5,
#'   rw_smoothing = .2))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 15,
#'   rw_smoothing = .2, trend = c(.5, -.5)))
#' plot(soundgen:::getRandomWalk(len = 1000, rw_range = 15,
#'   rw_smoothing = .2, trend = c(15, -1)))
getRandomWalk = function(len,
                         rw_range = 1,
                         rw_smoothing = .2,
                         method = c('linear', 'spline')[2],
                         trend = 0) {
  if (len < 2)
    return (rgamma(1, 1 / rw_range ^ 2, 1 / rw_range ^ 2))

  # generate a random walk (rw) of length depending on rw_smoothing, then linear extrapolation to len
  n = floor(max(2, 2 ^ (1 / rw_smoothing)))
  if (length(trend) > 1) {
    n = round(n / 2, 0) * 2 # force to be even
    trend_short = rep(trend, each = n / length(trend))
    # for this to work, length(trend) must be a multiple of n.
    # In practice, specify trend of length 2
  } else {
    trend_short = trend
  }

  if (n > len) {
    rw_long = cumsum(rnorm(len, trend_short)) # just a rw of length /len/
  } else {
    # get a shorter sequence and extrapolate, thus achieving more or less smoothing
    rw_short = cumsum(rnorm(n, trend_short)) # plot(rw_short, type = 'l')
    if (method == 'linear') {
      rw_long = approx(rw_short, n = len)$y
    } else if (method == 'spline') {
      rw_long = spline(rw_short, n = len)$y
    }
  } # plot (rw_long, type = 'l')

  # normalize
  rw_normalized = rw_long - min(rw_long)
  rw_normalized = rw_normalized / max(abs(rw_normalized)) * rw_range
  return (rw_normalized)
}


#' Discrete random walk
#'
#' Internal soudgen function.
#'
#' Takes a continuous random walk and converts it to continuous epochs of
#' repeated values 0/1/2, each at least minLength points long. 0/1/2 correspond
#' to different noise regimes: 0 = no noise, 1 = subharmonics, 2 = subharmonics
#' and jitter/shimmer.
#' @keywords internal
#' @param rw a random walk generated by \code{\link{getRandomWalk}} (expected
#'   range 0 to 100)
#' @param noise_amount a number between 0 to 100: 0 = returns all zeroes; 100 =
#'   returns all twos
#' @param minLength the mimimum length of each epoch
#' @return Returns a vector of integers (0/1/2) of the same length as rw.
#' @examples
#' rw = soundgen:::getRandomWalk(len = 100, rw_range = 100, rw_smoothing = .2)
#' plot (rw, type = 'l')
#' plot (soundgen:::getBinaryRandomWalk(rw, noiseAmount = 75, minLength = 10))
#' plot (soundgen:::getBinaryRandomWalk(rw, noiseAmount = 5, minLength = 10))
getBinaryRandomWalk = function(rw,
                               noiseAmount = 50,
                               minLength = 50) {
  len = length(rw)
  if (noiseAmount == 0) return(rep(0, len))
  if (noiseAmount == 100) return(rep(2, len))

  # calculate thresholds for different noise regimes
  q1 = noiseThresholds_dict$q1[noiseAmount + 1]
  # +1 b/c the rows indices in noiseThresholds_dict start from 0, not 1
  q2 = noiseThresholds_dict$q2[noiseAmount + 1]

  # convert continuous rw to discrete epochs based on q1 and q2 thresholds
  rw_bin = rep(0, len)
  rw_bin[which(rw > q1)] = 1
  rw_bin[which(rw > q2)] = 2   # plot (rw_bin, ylim=c(0,2))

  # make sure each epoch is long enough
  rw_bin = clumper(rw_bin, minLength = minLength)
  # plot (rw_bin, ylim = c(0,2))
  return (rw_bin)
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
#' soundgen:::upsample(c(100, 150, 130), samplingRate = 16000)
upsample = function(pitch_per_gc, samplingRate = 44100) {
  gcLength_points = round (samplingRate / pitch_per_gc)
  gc_upsampled = c(1, cumsum(gcLength_points))
  pitch_upsampled = vector()

  # fill in the missing values in between points through linear interpolation
  for (i in 1:(length(pitch_per_gc) - 1)) {
    pitch_upsampled = c(
      pitch_upsampled,
      seq(pitch_per_gc[i], pitch_per_gc[i + 1], length.out = gcLength_points[i])
    )
  }
  pitch_upsampled = c(pitch_upsampled,
                      rep(tail(pitch_per_gc, 1), tail(gcLength_points, 1)))
  # plot(pitch_upsampled, type = 'l')

  return (list(pitch = pitch_upsampled, gc = gc_upsampled))
}


#' Resize vector to required length
#'
#' Internal soundgen function.
#'
#' Adjusts a vector to match the required length by either trimming one or both
#' ends or padding them with zeros.
#' @param myseq input vector
#' @param len target length
#' @param padDir specifies the affected side. For padding, it is the side on
#'   which new elements will be added. For trimming, this is the side that will
#'   be trimmed. Defaults to 'central'
#' @param padWith if the vector needs to be padded to match the required length,
#'   what should it be padded with? Defaults to 0
#' @return Returns the modified vector of the required length.
#' @examples
#' soundgen:::matchLengths (c(1, 2, 3), len = 5)
#' soundgen:::matchLengths (3:7, len = 3)
#' # trimmed on the left
#' soundgen:::matchLengths (3:7, len = 3, padDir = 'left')
#' # padded with zeroes on the left
#' soundgen:::matchLengths (3:7, len = 30, padDir = 'left')
matchLengths = function(myseq,
                        len,
                        padDir = c('left', 'right', 'central')[3],
                        padWith = 0) {
  #  padDir specifies where to cut/add zeros ('left' / 'right' / 'central')
  if (length(myseq) == len) return (myseq)

  if (padDir == 'central') {
    if (length(myseq) < len) {
      myseq = c(rep(padWith, len), myseq, rep(padWith, len))
      # for padding, first add a whole lot of zeros and then trim using the same
      # algorithm as for trimming
    }
    halflen = len / 2
    center = (1 + length(myseq)) / 2
    start = ceiling(center - halflen)
    myseq = myseq[start:(start + len - 1)]
  } else if (padDir == 'left') {
    if (length(myseq) > len) {
      myseq = myseq [(length(myseq) - len + 1):length(myseq)]
    } else {
      myseq = c(rep(padWith, (len - length(myseq))), myseq)
    }
  } else if (padDir == 'right') {
    if (length(myseq) > len) {
      myseq = myseq [1:(length(myseq) - len)]
    } else {
      myseq = c(myseq, rep(padWith, (len - length(myseq))))
    }
  }
  return (myseq)
}


#' Add overlapping vectors
#'
#' Internal soundgen function.
#'
#' Adds two partly overlapping vectors to produce a longer vector. The location
#' at which vector 2 is pasted is defined by insertionPoint. Algorithm: both
#' vectors are padded with zeroes to match in length and then added. All NA's
#' are converted to 0.
#' @param v1,v2 numeric vectors
#' @param insertionPoint the index of element in vector 1 at which vector 2 will
#'   be insterted (any integer, can also be negative)
#' @examples
#' v1 = 1:6
#' v2 = rep(100, 3)
#' soundgen:::addVectors(v1, v2, insertionPoint = 5)
#' soundgen:::addVectors(v1, v2, insertionPoint = -4)
#' # note the asymmetry: insertionPoint refers to the first arg
#' soundgen:::addVectors(v2, v1, insertionPoint = -4)
#'
#' v3 = rep(100, 15)
#' soundgen:::addVectors(v1, v3, insertionPoint = -4)
#' soundgen:::addVectors(v2, v3, insertionPoint = 7)
addVectors = function(v1, v2, insertionPoint) {
  if (!is.numeric(v1)) stop(paste('Non-numeric v1:', head(v1)))
  if (!is.numeric(v2)) stop(paste('Non-numeric v2:', head(v2)))
  v1[is.na(v1)] = 0
  v2[is.na(v2)] = 0

  # align left ends
  if (insertionPoint > 1) {
    pad_left = insertionPoint - 1
    v2 = c(rep(0, insertionPoint), v2)
  } else if (insertionPoint < 1) {
    pad_left = 1 - insertionPoint
    v1 = c(rep(0, pad_left), v1)
  }

  # equalize lengths
  l1 = length(v1)
  l2 = length(v2)
  len_dif = l2 - l1
  if (len_dif > 0) {
    v1 = c(v1, rep(0, len_dif))
  } else if (len_dif < 0) {
    v2 = c(v2, rep(0, -len_dif))
  }

  return (v1 + v2)
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
#' @param sylDur_mean the desired mean syllable duration, in ms
#' @param pauseDur_mean the desired mean pause between syllables, in ms
#' @param sylDur_min,sylDur_max the lower and upper bounds on possible syllable
#'   duration, in ms
#' @param pauseDur_min,pauseDur_max the lower and upper bounds on possible pause
#'   duration, in ms
#' @param temperature a non-negative float regulating the stochasticity of
#'   syllable segmentation; 0 = no stochasticity; 1 = sd of proposals is equal
#'   to sylDur_mean (very strong stochasticity)
#' @param plot produce a plot of syllable structure?
#' @return Returns a matrix with a list of start-end points for syllables
#' @examples
#' soundgen:::divideIntoSyllables (nSyl = 5, sylDur_mean = 180,
#'   pauseDur_mean = 55, temperature = 0.2, plot = TRUE)
#' soundgen:::divideIntoSyllables (nSyl = 5, sylDur_mean = 180,
#'   pauseDur_mean = 55, temperature = 0, plot = TRUE)
divideIntoSyllables = function (nSyl,
                                sylDur_mean,
                                pauseDur_mean,
                                sylDur_min = 20,
                                sylDur_max = 10000,
                                pauseDur_min = 20,
                                pauseDur_max = 1000,
                                temperature = 0.025,
                                plot = FALSE) {
  out = matrix(ncol = 2, nrow = 0)
  colnames(out) = c('start', 'end')
  if (nSyl == 1) {
    out = rbind (out, c(0, sylDur_mean))
  } else {
    # generate random lengths while respecting the constraints
    c = 0
    while (nrow(out) < nSyl) {
      duration_ms_loop = rnorm_bounded (
        n = 1,
        mean = sylDur_mean,
        low = sylDur_min,
        high = sylDur_max,
        sd = sylDur_mean * temperature
      )
      pause_ms_loop = rnorm_bounded (
        n = 1,
        mean = pauseDur_mean,
        low = pauseDur_min,
        high = pauseDur_max,
        sd = pauseDur_mean * temperature
      )
      start = 1 + c # start time of syllable, in ms
      end = start + duration_ms_loop # end time of syllable, in ms
      out = rbind (out, c(start, end))
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
