# Functions for controlling the spectrum of generated sounds (rolloff and formants).

#' Control rolloff of harmonics
#'
#' Harmonics are generated as separate sine waves. But we don't want each
#' harmonic to be equally strong, so we normally specify some rolloff function
#' that describes the loss of energy in upper harmonics relative to the
#' fundamental frequency (f0). \code{\link{getRolloff}} provides flexible
#' control over this rolloff function, going beyond simple exponential decay
#' (\code{rolloff}). Use quadratic terms to modify the behavior of a few lower
#' harmonics, \code{rolloffAdjust_per_octave} to adjust the rate of decay per
#' octave, and \code{rolloffAdjust_per_kHz} for rolloff correction depending on
#' f0. Plot the output with different parameter values and see examples below
#' and the vignette to get a feel for how to use \code{\link{getRolloff}}
#' effectively.
#' @param pitch_per_gc a vector of f0 per glottal cycle, Hz
#' @param nHarmonics maximum number of harmonics to generate (very weak
#'   harmonics with amplitude < \code{throwaway_dB} will be discarded)
#' @inheritParams soundgen
#' @param rolloff_quadratic_ceiling quadratic adjustment is applied only up to
#'   \code{rolloff_quadratic_ceiling}, Hz. If not NULL, it overrides
#'   \code{rolloffAdjust_quadratic_nHarm}
#' @param baseline_Hz The "neutral" frequency, at which no adjustment of rolloff
#'   takes place regardless of \code{rolloffAdjust_per_kHz}
#' @param samplingRate sampling rate (needed to stop at Nyquist frequency and
#'   for plotting purposes)
#' @param plot if TRUE, produces a plot
#' @return Returns a matrix of amplitude multiplication factors for adjusting
#'   the amplitude of harmonics relative fo f0. Each row of output contains one
#'   harmonic, and each column contains one glottal cycle.
#' @export
#' @examples
#' # steady exponential rolloff of -12 dB per octave
#' rolloff = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffAdjust_per_octave = 0, plot = TRUE)
#' # the rate of rolloff slows down with each octave
#' rolloff = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffAdjust_per_octave = 2, plot = TRUE)
#' # the rate of rolloff increases with each octave
#' rolloff = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffAdjust_per_octave = -2, plot = TRUE)
#'
#' # variable f0: the lower f0, the more harmonics are non-zero
#' rolloff = getRolloff(pitch_per_gc = c(150, 800, 3000),
#'   rolloffAdjust_per_octave = 0, plot = TRUE)
#' # without the correction for f0 (rolloffAdjust_per_kHz),
#'   # high-pitched sounds have the same rolloff as low-pitched sounds,
#'   # producing unnaturally strong high-frequency harmonics
#' rolloff = getRolloff(pitch_per_gc = c(150, 800, 3000),
#'   rolloffAdjust_per_octave = 0, rolloffAdjust_per_kHz = 0, plot = TRUE)
#'
#' # parabolic adjustment of lower harmonics
#' rolloff = getRolloff(pitch_per_gc = 350, rolloffAdjust_quadratic = 0,
#'   rolloffAdjust_quadratic_nHarm = 2, samplingRate = 16000, plot = TRUE)
#' # rolloffAdjust_quadratic_nHarm = 1 affects only f0
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffAdjust_quadratic = 30,
#'   rolloffAdjust_quadratic_nHarm = 1, samplingRate = 16000, plot = TRUE)
#' # rolloffAdjust_quadratic_nHarm=2 or 3 affects only h1
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffAdjust_quadratic = 30,
#'   rolloffAdjust_quadratic_nHarm = 2, samplingRate = 16000, plot = TRUE)
#' # rolloffAdjust_quadratic_nHarm = 4 affects h1 and h2, etc
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffAdjust_quadratic = 30,
#'   rolloffAdjust_quadratic_nHarm = 4, samplingRate = 16000, plot = TRUE)
#' # negative rolloffAdjust_quadratic weakens lower harmonics
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffAdjust_quadratic = -20,
#'   rolloffAdjust_quadratic_nHarm = 7, samplingRate = 16000, plot = TRUE)
#' # only harmonics below 2000 Hz are affected
#' rolloff = getRolloff(pitch_per_gc = c(150, 600),
#'   rolloffAdjust_quadratic = -20, rolloff_quadratic_ceiling = 2000, samplingRate = 16000,
#'   plot = TRUE)
getRolloff = function(pitch_per_gc = c(440),
                      nHarmonics = 100,
                      rolloff = -12,
                      rolloffAdjust_per_octave = -2,
                      rolloffAdjust_quadratic = 0,
                      rolloffAdjust_quadratic_nHarm = 2,
                      rolloff_quadratic_ceiling = NULL,
                      rolloffAdjust_per_kHz = -6,
                      baseline_Hz = 200,
                      throwaway_dB = -120,
                      samplingRate = 44100,
                      plot = FALSE) {
  ## Exponential decay
  deltas = matrix(0, nrow = nHarmonics, ncol = length(pitch_per_gc))
  if (sum(rolloffAdjust_per_octave != 0) > 0) {
    for (h in 2:nHarmonics) {
      deltas[h,] = rolloffAdjust_per_octave * (pitch_per_gc * h - baseline_Hz) / 1000
      # rolloff changes by rolloffAdjust_per_octave per octave for each octave above H2
    }
  }
  # plot(deltas[, 1])

  r = matrix(0, nrow = nHarmonics, ncol = length(pitch_per_gc))
  for (h in 1:nHarmonics) {
    r[h,] = ((rolloff + rolloffAdjust_per_kHz *
        (pitch_per_gc - baseline_Hz) / 1000) * log2(h)) + deltas[h,]
    # note that rolloff is here adjusted as a linear function of
    #   the difference between current f0 and baseline_Hz
    r[h, which(h * pitch_per_gc >= samplingRate / 2)] = -Inf # to avoid
    # aliasing, we discard all harmonics above Nyquist frequency
  }

  ## QUADRATIC term affecting the first rolloffAdjust_quadratic_nHarm harmonics only
  if (rolloffAdjust_quadratic != 0) {
    if (!is.null(rolloff_quadratic_ceiling)) {
      rolloffAdjust_quadratic_nHarm = round(rolloff_quadratic_ceiling / pitch_per_gc)  # vector of
      # length pitch_per_gc specifying the number of harmonics whose amplitude
      # is to be adjusted
    } else {
      rolloffAdjust_quadratic_nHarm = rep(round(rolloffAdjust_quadratic_nHarm), length(pitch_per_gc))
    }
    rolloffAdjust_quadratic_nHarm[rolloffAdjust_quadratic_nHarm == 2] = 3 # will have the effect of boosting
    # H1 (2 * F0)
    # parabola ax^2+bx+c
    # 0 at h=1 and at h=rolloffAdjust_quadratic_nHarm; a parabola up/down in between. We have the following constraints on the parabola: f(1)=0; f(rolloffAdjust_quadratic_nHarm)=0; f'((1+rolloffAdjust_quadratic_nHarm)/2)=0; and f((1+rolloffAdjust_quadratic_nHarm)/2)=rolloffAdjust_quadratic.
    ## Solving for a,b,c
    # f'(middle) = 2a*(1+rolloffAdjust_quadratic_nHarm)/2+b = a*(1+rolloffAdjust_quadratic_nHarm)+b = 0, so b = -a*(1+rolloffAdjust_quadratic_nHarm).
    # f(1) = a+b+c = 0, so c = -a+a*(1+rolloffAdjust_quadratic_nHarm) = a*rolloffAdjust_quadratic_nHarm.
    # f(middle)=rolloffAdjust_quadratic. middle is (1+rolloffAdjust_quadratic_nHarm)/2, and f( (1+rolloffAdjust_quadratic_nHarm)/2 ) = a*(1+rolloffAdjust_quadratic_nHarm)^2/4 + b*(1+rolloffAdjust_quadratic_nHarm)/2 + c = (substituting above expressions for b and c) = a*(1+rolloffAdjust_quadratic_nHarm)^2/4 - a*(1+rolloffAdjust_quadratic_nHarm)*(1+rolloffAdjust_quadratic_nHarm)/2 + a*rolloffAdjust_quadratic_nHarm = -a*(1+rolloffAdjust_quadratic_nHarm)^2/4 + a*rolloffAdjust_quadratic_nHarm = -a/4*(1 + rolloffAdjust_quadratic_nHarm^2 + 2*rolloffAdjust_quadratic_nHarm - 4*rolloffAdjust_quadratic_nHarm) = -a/4*(1-rolloffAdjust_quadratic_nHarm)^2. And we want this to equal rolloffAdjust_quadratic. Solving for a, we have a = -4*rolloffAdjust_quadratic/(rolloffAdjust_quadratic_nHarm-1)^2
    a = -4 * rolloffAdjust_quadratic / (rolloffAdjust_quadratic_nHarm - 1) ^ 2
    b = -a * (1 + rolloffAdjust_quadratic_nHarm)
    c = a * rolloffAdjust_quadratic_nHarm
    # # verify:
    # myf = function(s, a, b, c) {return(a * s^2 + b * s + c)}
    # s = seq(1, rolloffAdjust_quadratic_nHarm[1], by = .5)
    # plot (s, myf(s, a, b, c))

    # for a single affected harmonic, just change the amplitude of F0
    r[1, which(rolloffAdjust_quadratic_nHarm < 3)] =
      r[1, which(rolloffAdjust_quadratic_nHarm < 2)] + rolloffAdjust_quadratic
    # if at least 2 harmonics are to be adjusted, calculate a parabola
    for (i in which(rolloffAdjust_quadratic_nHarm >= 3)) {
      rowIdx = 1:rolloffAdjust_quadratic_nHarm[i]
      r[rowIdx, i] = r[rowIdx, i] + a[i] * rowIdx ^ 2 +
        b[i] * rowIdx + c[i]   # plot (r[, 1])
    }
  }

  # set values under throwaway_dB to zero
  if (is.numeric(throwaway_dB)) {
    # if not null and not NA
    r[r < throwaway_dB] = -Inf
  }

  # normalize so the amplitude of F0 is always 0
  r = apply (r, 2, function(x) x - max(x))

  # plotting
  if (plot) {
    x_max = samplingRate / 2 / 1000
    if (length(pitch_per_gc) == 1 | var(pitch_per_gc) == 0) {
      idx = which(r[, 1] > -Inf)
      plot ( idx * pitch_per_gc[1] / 1000, r[idx, 1],
        type = 'b', xlim = c(0, x_max), xlab = 'Frequency, Hz',
        ylab = 'Amplitude, dB', main = 'Glottal source rolloff')
    } else {
      pitch_min = min(pitch_per_gc)
      pitch_max = max(pitch_per_gc)
      idx_min = which.min(pitch_per_gc)
      idx_max = which.max(pitch_per_gc)
      rows_min = 1:tail(which(r[, idx_min] > -Inf), 1)
      rows_max = 1:tail(which(r[, idx_max] > -Inf), 1)
      freqs_min = rows_min * pitch_min / 1000
      freqs_max = rows_max * pitch_max / 1000
      rolloff_min = r[rows_min, idx_min]
      rolloff_max = r[rows_max, idx_max]
      plot (freqs_min, rolloff_min, type = 'b', col = 'blue',
            xlim = c(0, x_max), xlab = 'Frequency, Hz',
            ylab = 'Amplitude, dB', main = 'Glottal source rolloff')
      text (x = x_max, y = -10, labels = 'Lowest pitch',
            col = 'blue', pos = 2)
      points (freqs_max, rolloff_max, type = 'b', col = 'red')
      text (x = x_max, y = 0, labels = 'Highest pitch',
            col = 'red', pos = 2)
    }
  }

  # convert from dB to linear amplitude multipliers
  r = 2 ^ (r / 10)

  # shorten by discarding harmonics that are 0 throughout the sound
  r = r[which(apply(r, 1, sum) > 0), , drop = FALSE]
  rownames(r) = 1:nrow(r) # helpful for adding vocal fry

  return (r)
}


#' Spectral envelope
#'
#' Internal soundgen function.
#'
#' Prepares a spectral envelope for filtering a sound to add formants, lip
#' radiation, and some stochastic component regulated by temperature.
#' exactFormants is specified as a list containing time, frequency, amplitude,
#' and width values for each formant (see examples). NB: each formant is
#' generated as a gamma distribution with mean = freq and SD = width. Formant
#' bandwidths in soundgen are therefore NOT compatible with formant bandwidths
#' used in Klatt synthesizer and other algorithms that rely on FIR instead of
#' FFT.
#' @param nr the number of frequency bins = windowLength_points/2, where
#'   windowLength_points is the size of window for Fourier transform
#' @param nc the number of time steps for Fourier transform
#' @inheritParams soundgen
#' @param formDrift scale factor regulating the effect of temperature on the depth of random drift of all formants (user-defined and stochastic): the higher, the more formants drift at a given temperature
#' @param formDisp scale factor regulating the effect of temperature on the irregularity of the dispersion of stochastic formants: the higher, the more unevenly stochastic formants are spaced at a given temperature
#' @param speedSound speed of sound in warm air, cm/s. Stevens (2000) "Acoustic phonetics", p. 138
#' @param amplBoost_openMouth_dB amplify the voice when the mouth is open by
#'   \code{amplBoost_openMouth_dB} dB
#' @param mouthOpening_threshold the mouth is considered to be open when its
#'   opening is greater than \code{mouthOpening_threshold}. Defaults to 0
#' @param extraFormants_stochastic the amplitude of additional formants added above
#'   the highest specified formant (only if temperature > 0)
#' @param smoothLinear_factor regulates smoothing of formant anchors (0 to +Inf)
#'   as they are upsampled to the number of fft steps \code{nc}. This is
#'   necessary because the input \code{exactFormants} normally contains fewer
#'   sets of formant values than the number of fft steps.
#'   \code{smoothLinear_factor} = 0: close to default spline; >3: approaches
#'   linear extrapolation
#' @param plot if TRUE, produces a plot of the spectral envelope
#' @param dur_ms duration of the sound, ms (for plotting purposes only)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   or another color theme (e.g. 'heat.colors')
#' @param nCols number of colors in the palette
#' @param xlab,ylab labels of axes
#' @param ... other graphical parameters passed on to \code{image()}
#' @export
#' @return Returns a spectral filter (matrix nr x nc, where nr is the number of
#'   frequency bins = windowLength_points/2 and nc is the number of time steps)
#' @examples
#' # [a] with F1-F4 visible
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   exactFormants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0, samplingRate = 16000)))
#' # some "wiggling" of specified formants plus extra formants on top
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   exactFormants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0.1, extraFormants_stochastic = 10, samplingRate = 16000)))
#' # stronger extra formants
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   exactFormants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0.1, extraFormants_stochastic = 30, samplingRate = 16000)))
#' # a schwa based on the length of vocal tract = 15.5 cm
#' image(t(getSpectralEnvelope(nr = 512, nc = 50, exactFormants = NA,
#'   temperature = .1, vocalTract = 15.5, samplingRate = 16000)))
#'
#' # manual specification of formants
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#' samplingRate = 16000, exactFormants = list(
#'   'f1' = data.frame('time' = 0, 'freq' = 900, 'amp' = 30, 'width' = 120),
#'   'f2' = data.frame('time' = 0, 'freq' = 1300, 'amp' = 30, 'width' = 120),
#'   'f3' = data.frame('time' = 0, 'freq' = 3200, 'amp' = 20, 'width' = 200)))))
getSpectralEnvelope = function(nr,
                               nc,
                               exactFormants = NA,
                               formantDep = 1,
                               rolloff_lipRad = 6,
                               mouthAnchors = NA,
                               mouthOpening_threshold = 0,
                               amplBoost_openMouth_dB = 0,
                               vocalTract = NULL,
                               temperature = 0,
                               formDrift = .3,
                               formDisp = .2,
                               extraFormants_stochastic = 30,
                               smoothLinear_factor = 1,
                               samplingRate = 16000,
                               speedSound = 35400,
                               plot = FALSE,
                               dur_ms = NULL,
                               colorTheme = c('bw', 'seewave', '...')[1],
                               nCols = 100,
                               xlab = 'Time',
                               ylab = 'Frequency, kHz',
                               ...) {
  if (class(exactFormants) == 'character') {
    exactFormants = convertStringToFormants(exactFormants)
  } else if (is.list(exactFormants)) {
    if (is.list(exactFormants[[1]])) {
      exactFormants = lapply(exactFormants, as.data.frame)
    }
  } else if (!is.null(exactFormants) && !is.na(exactFormants)) {
    stop('If not NULL, exactFormants must be a list or a string of characters
          from dictionary presets: a, o, i, e, u, 0 (schwa)')
  }
  if (is.null(vocalTract) & length(exactFormants[[1]]) > 2) {
    # if we don't know vocalTract, but at least one formant is defined,
    # we guess the length of vocal tract
    formantDispersion = mean(diff(unlist(lapply(exactFormants, function(f) f$freq))))
    vocalTract = ifelse (
      is.numeric(formantDispersion),
      speedSound / 2 / formantDispersion,
      speedSound / 4 / exactFormants$f1$freq
    )
  }
  if (length(exactFormants[[1]]) < 2) {
    # ie if is.na(exactFormants) or if there's something wrong with it,
    # we fall back on vocalTract to make a schwa
    freq = speedSound / 4 / vocalTract
    exactFormants = list('f1' = data.frame(
      'time' = 0,
      'freq' = freq,
      'amp' = 30,
      'width' = 50 + (log2(freq) - 5) * 20
    ))
    # freq = 50:5000; a = 50+(log2(freq)-5)*20; plot(freq, a))
  }

  # upsample to the length of fft steps
  nPoints = max(unlist(lapply(exactFormants, nrow)))
  exactFormants_upsampled = lapply(exactFormants, function(f) {
    temp = apply(f, 2, function(y) {
      if (nrow(f) > 1) {
        # just spline produces imprecise, overly smoothed curves. Loess is just
        # too slow for this. So we apply linear extrapolation to formant values
        # first, to get a fairly straight line between anchors, and THEN smooth
        # it out with spline
        out = spline(approx(y, n = nPoints + 2 ^ smoothLinear_factor,
          x = f$time)$y, n = nc)$y
      } else {
        out = rep(y, nc)
      }
      out
    })
    if (class(temp) == 'numeric') {
      # if nc==1, we get numeric instead of
      # matrix and need to convert
      temp = t(as.matrix(temp))
    }
    temp
  }) # check that class(exactFormants_upsampled[[1]]) == 'matrix'

  ## Stochastic part (only for temperature > 0)
  if (temperature > 0) {
    # create a few new, relatively high-frequency "pseudo-formants" moving
    # together with the real formants
    if (is.null(vocalTract) && length(exactFormants) > 1) {
      ff = unlist(lapply(exactFormants, function(x) x$freq[1]))
      formantDispersion = mean(c(ff[1], diff(ff)))
    } else if (!is.null(vocalTract)) {
      formantDispersion = 2 * speedSound / (4 * vocalTract)
    } else {
      formantDispersion = NA # making sdG also NA, ie extra formants not added
    }
    sdG = formantDispersion * temperature * formDisp
    nFormants = length(exactFormants_upsampled)
    freq_max = max(exactFormants_upsampled[[nFormants]][, 'freq'])

    if (!is.na(sdG) && extraFormants_stochastic > 0) {
      while (freq_max < (samplingRate / 2 - 1000)) {
        # don't add extra formants close to Nyquist to avoid artifacts
        rw = getRandomWalk(
          len = nc,
          rw_range = temperature * formDrift,
          rw_smoothing = 0,
          trend = 0
        )
        # for nPoints == 1, returns one number close to 1
        if (length(rw) > 1) {
          rw = rw - mean(rw) + 1
        } # for actual random walks, make sure mean is 1
        temp = data.frame (
          'time' = exactFormants_upsampled[[1]][, 'time'],
           'freq' = exactFormants_upsampled[[nFormants]][, 'freq'] +
                    round(rgamma(1,
                                 formantDispersion ^ 2 / sdG ^ 2,
                                 formantDispersion / sdG ^ 2
                               ) * rw
                             ))
        # rgamma: mean = extraFormants_stochastic, sd = extraFormants_stochastic*temperature
        temp$amp = round(rgamma(
          1,
          (formantDep / temperature) ^ 2,
          extraFormants_stochastic * formantDep /
            (extraFormants_stochastic * temperature) ^ 2 ) * rw)
        temp$width = 50 + (log2(temp$freq) - 5) * 20
        # visualize: freq=50:8000; plot(freq, 50+(log2(freq)-5)*20)
        exactFormants_upsampled[[nFormants + 1]] = temp
        nFormants = nFormants + 1
        freq_max = max(exactFormants_upsampled[[nFormants]]$freq)
      }
    }

    # wiggle both user-specified and stochastically added formants
    for (f in 1:nFormants) {
      for (c in 2:4) {
        # wiggle freq, ampl and bandwidth independently
        rw = getRandomWalk(
          len = nc,
          rw_range = temperature * formDrift,
          rw_smoothing = 0.3,
          trend = rnorm(1)
        )
        # if nc == 1, returns one number close to 1
        if (length(rw) > 1) {
          # for actual random walks, make sure mean is 1
          rw = rw - mean(rw) + 1
        }
        exactFormants_upsampled[[f]][, c] = exactFormants_upsampled[[f]][, c] * rw
      }
    } # end of wiggling existing formants
  } # end of if temperature > 0

  ## Deterministic part
  # convert formant freqs and widths from Hz to bins
  bin_width = samplingRate / 2 / nr # Hz
  bin_freqs = seq(bin_width / 2, samplingRate / 2, length.out = nr) # Hz
  for (f in 1:length(exactFormants_upsampled)) {
    exactFormants_upsampled[[f]][, 'freq'] =
      (exactFormants_upsampled[[f]][, 'freq'] - bin_width / 2) / bin_width + 1
    # frequencies expressed in bin indices (how many bin widths above the
    # central frequency of the first bin)
    exactFormants_upsampled[[f]][, 'width'] =
      exactFormants_upsampled[[f]][, 'width'] / bin_width
  }

  # mouth opening
  if (length(mouthAnchors) < 1 | sum(is.na(mouthAnchors)) > 0) {
    mouthOpening_upsampled = rep(0.5, nc) # defaults to mouth half-open the
    # whole time - sort of hanging loosely agape ;))
    mouthOpen_binary = rep(1, nc)
  } else {
    mouthOpening_upsampled = getSmoothContour(
      len = nc,
      anchors = mouthAnchors,
      value_floor = permittedValues['mouthOpening', 'low'],
      value_ceiling = permittedValues['mouthOpening', 'high'],
      plot = FALSE
    )
    mouthOpening_upsampled[mouthOpening_upsampled < mouthOpening_threshold] = 0
    mouthOpen_binary = ifelse(mouthOpening_upsampled > 0, 1, 0)
  }
  # plot(mouthOpening_upsampled, type = 'l')

  # adjust formants for mouth opening
  if (!is.null(vocalTract) && is.finite(vocalTract)) {
    # is.finite() returns F for NaN, NA, ±inf, etc
    adjustment_hz = (mouthOpening_upsampled - 0.5) * speedSound /
      (4 * vocalTract) # speedSound = 35400 cm/s, speed of sound in warm
    # air. The formula for mouth opening is modified from Moore (2016)
    # "A Real-Time Parametric General-Purpose Mammalian Vocal Synthesiser".
    # mouthOpening = .5 gives no modification (neutral, "default" position)
    adjustment_bins = (adjustment_hz - bin_width / 2) / bin_width + 1
  } else {
    adjustment_bins = 0
  }
  for (f in 1:length(exactFormants_upsampled)) {
    exactFormants_upsampled[[f]][, 'freq'] =
      exactFormants_upsampled[[f]][, 'freq'] + adjustment_bins
    # force each formant frequency to be positive
    exactFormants_upsampled[[f]][, 'freq'] [exactFormants_upsampled[[f]][, 'freq'] < 1] = 1
  }

  # nasalize the parts with closed mouth: see Hawkins & Stevens (1985);
  # http://www.cslu.ogi.edu/tutordemos/SpectrogramReading/cse551html/cse551/node35.html
  nasalizedIdx = which(mouthOpen_binary == 0) # or specify a separate
  # nasalization contour
  if (length(nasalizedIdx) > 0) {
    # add a pole
    exactFormants_upsampled$fnp = exactFormants_upsampled$f1
    exactFormants_upsampled$fnp[, 'amp'] = 0
    exactFormants_upsampled$fnp[nasalizedIdx, 'amp'] =
      exactFormants_upsampled$f1[nasalizedIdx, 'amp'] * 2 / 3
    exactFormants_upsampled$fnp[nasalizedIdx, 'width'] =
      exactFormants_upsampled$f1[nasalizedIdx, 'width'] * 2 / 3
    exactFormants_upsampled$fnp[nasalizedIdx, 'freq'] =
      ifelse(
        exactFormants_upsampled$f1[nasalizedIdx, 'freq'] > 550 / bin_width,
        exactFormants_upsampled$f1[nasalizedIdx, 'freq'] - 250 / bin_width,
        exactFormants_upsampled$f1[nasalizedIdx, 'freq'] + 250 / bin_width
      )
    # 250 Hz below or above F1, depending on whether F1 is above or below
    # 550 Hz

    # add a zero
    exactFormants_upsampled$fnz = exactFormants_upsampled$f1
    exactFormants_upsampled$fnz[, 'amp'] = 0
    exactFormants_upsampled$fnz[nasalizedIdx, 'amp'] =
      -exactFormants_upsampled$f1[nasalizedIdx, 'amp'] * 2 / 3
    exactFormants_upsampled$fnz[nasalizedIdx, 'freq'] =
      (exactFormants_upsampled$fnp[nasalizedIdx, 'freq'] +
         exactFormants_upsampled$f1[nasalizedIdx, 'freq']) / 2  # midway between
    # f1 and fnp
    exactFormants_upsampled$fnz[nasalizedIdx, 'width'] =
      exactFormants_upsampled$fnp[nasalizedIdx, 'width']
    # modify f1
    exactFormants_upsampled$f1[nasalizedIdx, 'amp'] =
      exactFormants_upsampled$f1[nasalizedIdx, 'amp'] * 4 / 5
    exactFormants_upsampled$f1[nasalizedIdx, 'width'] =
      exactFormants_upsampled$f1[nasalizedIdx, 'width'] * 5 / 4
  }

  # create a "spectrum"-shaped filter matrix
  spectralEnvelope = matrix(0, nrow = nr, ncol = nc)
  for (f in 1:length(exactFormants_upsampled)) {
    mg = exactFormants_upsampled[[f]][, 'freq']  # mean of gamma distribution
    # (vector of length nc)
    sdg = exactFormants_upsampled[[f]][, 'width']  # sd of gamma distribution
    # (vector of length nc)
    sdg[sdg == 0] = 1  # otherwise division by 0
    shape = mg ^ 2 / sdg ^ 2
    rate = mg / sdg ^ 2
    formant = matrix(0, nrow = nr, ncol = nc)
    for (c in 1:nc) {
      formant[, c] = dgamma (1:nr, shape[c], rate[c])
      formant[, c] = formant[, c] / max(formant[, c]) *
        exactFormants_upsampled[[f]][c, 'amp']
    }
    spectralEnvelope = spectralEnvelope + formant
  }
  spectralEnvelope = spectralEnvelope * formantDep
  # plot(spectralEnvelope[, 1], type = 'l')

  # add lip radiation when the mouth is open
  lip_dB = rolloff_lipRad * log2(1:nr) # vector of length nr
  for (c in 1:nc) {
    spectralEnvelope[, c] = (spectralEnvelope[, c] +
                            lip_dB * mouthOpen_binary[c]) *
       2 ^ (mouthOpening_upsampled[c] * amplBoost_openMouth_dB / 10)
  }

  # convert from dB to linear multiplier
  spectralEnvelope = 2 ^ (spectralEnvelope / 10)

  if (plot) {
    if (is.numeric(dur_ms)) {
      x = seq(0, dur_ms, length.out = nc)
    } else {
      x = seq(0, 1, length.out = nc)
    }
    if (colorTheme == 'bw') {
      col = gray(seq(from = 1, to = 0, length = nCols))
    } else if (colorTheme == 'seewave') {
      col = seewave::spectro.colors(nCols)
    } else {
      colFun = match.fun(colorTheme)
      col = rev(colFun(nCols))
    }
    image(x = x,
          y = seq(0, samplingRate /2, length.out = nr)/ 1000,
          z = t(spectralEnvelope),
          xlab = xlab,
          ylab = ylab,
          col = col,
          ...)
  }

  return (spectralEnvelope)
}
