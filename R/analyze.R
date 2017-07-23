### MAIN FUNCTIONS FOR ACOUSTIC ANALYSIS ###

#' Analyze sound
#'
#' Acoustic analysis of a single sound file: pitch tracking and basic spectral
#' characteristics. The default values of arguments are optimized for human
#' non-linguistic vocalizations. See the vignette for details.
#'
#' @inheritParams spec
#' @param silence (0 to 1) frames with mean abs amplitude below silence
#'   threshold are not analyzed at all. NB: this number is dynamically updated:
#'   the actual silence threshold may be higher depending on the quietest frame,
#'   but it will never be lower than this specified number.
#' @param cutoff_freq (>0 to Nyquist Hz) repeat the calculation of spectral
#'   descriptives after discarding all info above \code{cutoff_freq}.
#'   Recommended if the original sampling rate varies across different analyzed
#'   audio files
#' @param pitch_methods methods of pitch estimation to consider for determining
#'   pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
#'   'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band
#' @param entropy_threshold pitch tracking is not performed for frames with Weiner
#'   entropy above \code{entropy_threshold}, but other spectral descriptives are
#'   still calculated
#' @param pitch_floor,pitch_ceiling absolute bounds for pitch candidates (Hz)
#' @param prior_mean,prior_sd specifies the mean and sd of gamma distribution
#'   describing our prior knowledge about the most likely pitch values for this
#'   file. Specified in semitones: \code{prior_mean = HzToSemitones(300),
#'   prior_sd = 6} gives a prior with mean = 300 Hz and SD of 6 semitones (half
#'   an octave)
#' @param prior_plot if TRUE, produces a separate plot of the prior
#' @param nCands maximum number of pitch candidates per method (except for
#'   \code{dom}, which returns at most one candidate per frame)
#' @param min_voiced_cands minimum number of pitch candidates that
#'   have to be defined to consider a frame voiced (defaults to 2 if \code{dom}
#'   is among other candidates and 1 otherwise)
#' @param dom_threshold (0 to 1) to find the lowest dominant frequency band, we
#'   do short-term FFT and take the lowest frequency with amplitude at least
#'   dom_threshold
#' @param dom_smoothing the width of smoothing interval (Hz) for finding
#'   \code{dom}
#' @param autocor_threshold,cep_threshold,spec_threshold (0 to 1) separate
#'   voicing thresholds for detecting pitch candidates with three different
#'   methods: autocorrelation, cepstrum, and BaNa algorithm (see Details). Note
#'   that HNR is calculated even for unvoiced frames.
#' @param autocor_smoothing the width of smoothing interval (in bins) for
#'   finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'   rate 44100 and smaller odd numbers for lower values of sampling rate
#' @param cep_smoothing the width of smoothing interval (in bins) for finding
#'   peaks in the cepstrum. Defaults to 31 for sampling rate 44100 and smaller
#'   odd numbers for lower values of sampling rate
#' @param cep_zp zero-padding of the spectrum used for cepstral pitch detection
#'   (final length of spectrum after zero-padding, in points)
#' @param spec_peak,spec_peak_HNRslope when looking for putative harmonics in
#'   the spectrum, the threshold for peak detection is calculated as
#'   \code{spec_peak * (1 - HNR * spec_peak_HNRslope)}
#' @param spec_smoothing the width of window for detecting peaks in the spectrum, Hz
#' @param spec_merge pitch candidates within \code{spec_merge} semitones are
#'   merged with boosted certainty
#' @param spec_singlePeakCert (0 to 1) if F0 is calculated based on a single
#'   harmonic ratio (as opposed to several ratios converging on the same
#'   candidate), its certainty is taken to be \code{spec_singlePeakCert}
#' @param shortest_syl the smallest length of a voiced segment (ms) that
#'   constitutes a voiced syllable (shorter segments will be replaced by NA, as
#'   if unvoiced)
#' @param shortest_pause the smallest gap between voiced syllables (ms) that
#'   means they shouldn't be merged into one voiced syllable
#' @param interpol_window,interpol_tolerance,interpol_cert control the behavior of
#'   interpolation algorithm when postprocessing pitch candidates. To turn off
#'   interpolation, set \code{interpol_window} to NULL. See
#'   \code{\link{pathfinder}} for details.
#' @param pathfinding method of finding the optimal path through pitch
#'   candidates: 'slow' for annealing, 'fast' for a simple heuristic, 'none' for
#'   none. See \code{\link{soundgen:::pathfinder}} for details.
#' @param control_anneal a list of control parameters for postprocessing of
#'   pitch contour with SANN algorithm of \code{\link[stats]{optim}}. This is
#'   only relevant if \code{pathfinding} is 'slow'
#' @param cert_weight (0 to 1) in pitch postprocessing, specifies how much we
#'   prioritize the certainty of pitch candidates vs. pitch jumps / the internal
#'   tension of the resulting pitch curve
#' @param snake_step optimized path through pitch candidates is further
#'   processed to minimize the elastic force acting on pitch contour. To
#'   disable, set \code{snake_step} to NULL
#' @param snake_plot if TRUE, plots the snake
#' @param smooth_idx,smooth_vars if \code{smooth_idx} is a positive number,
#'   contours of the variables in \code{smooth_vars} are smoothed using a
#'   customized version of median smoothing. \code{smooth_idx} of 1 corresponds
#'   to a window of ~100 ms and tolerated deviation of ~4 semitones. To disable,
#'   set \code{smooth_idx} to NULL.
#' @param plot if TRUE, produces a spectrogram with pitch contour overlaid
#' @param savePath if a valid path is specified, a plot is saved in this
#'   folder (defaults to NA)
#' @param plot_spec_pars a list of graphical parameters passed on to
#'   \code{\link{spec}}. Set to \code{NULL} or \code{NA} to suppress plotting
#'   the spectrogram
#' @param plot_pitchCands_pars a list of graphical parameters for displaying
#'   individual pitch candidates. Set to \code{NULL} or \code{NA} to suppress
#' @param plot_pitch_pars a list of graphical parameters for displaying the
#'   final pitch contour. Set to \code{NULL} or \code{NA} to suppress
#' @return Returns a dataframe with one row per FFT frame and one column per
#'   acoustic variable. The best guess at the pitch contour considering all
#'   available information is stored in the variable called "pitch". In
#'   addition, the output contains pitch estimates based on three separate
#'   algorithms: autocorrelation (pitchAutocor), cepstrum (pitchCep), and BaNa
#'   (pitchSpec).
#' @export
#' @examples
#' sound1 = soundgen(sylLen = 900, pitchAnchors = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noiseAnchors = list(time = c(0, 900), value = c(-40, 00)),
#'   temperature = 0)
#' playme(sound1, 16000)
#' a1 = analyze(sound1, samplingRate = 16000, plot = TRUE)
#' # or, to improve the quality of postprocessing:
#' a1 = analyze(sound1, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
#' median(a1$pitch, na.rm = TRUE)  # 614 Hz
#' # (can vary, since postprocessing is stochastic)
#' # compare to the true value:
#' median(getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
#'   value = c(300, 900, 400, 2300)), len = 1000))
#'
#' # the same pitch contour, but harder to analyze b/c of subharmonics and jitter
#' sound2 = soundgen(sylLen = 900, pitchAnchors = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noiseAnchors = list(time = c(0, 900), value = c(-40, 20)),
#'   subDep = 100, jitterDep = 0.5, pitchEffects_amount = 100, temperature = 0)
#' playme(sound2, 16000)
#' a2 = analyze(sound2, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
#' # many pitch candidates are off, but the overall contour and estimate of
#' # median pitch should be pretty accurate:
#' median(a2$pitch, na.rm = TRUE)  # 622 Hz (can vary, since postprocessing is stochastic)
#' median(a2$HNR, na.rm = TRUE)  # HNR of 3-4 dB
#'
#' # Fancy plotting options:
#' a = analyze(sound2, samplingRate = 16000, plot = TRUE,
#'   plot_spec_pars = list(xlab = 'Time, ms', colorTheme = 'seewave', contrast = .8),
#'   plot_pitchCands_pars = list(cex = 3, col = c('gray70', 'yellow', 'purple', 'maroon')),
#'   plot_pitch_pars = list(col = 'black', lty = 3, lwd = 3))
#'
#'# Plot pitch candidates w/o a spectrogram
#' a = analyze(sound2, samplingRate = 16000, plot = TRUE, plot_spec_pars = NA)
analyze = function(x,
                   samplingRate = NULL,
                   silence = 0.04,
                   windowLength = 50,
                   wn = 'gaussian',
                   step = 25,
                   zp = 0,
                   cutoff_freq = 6000,
                   pitch_methods = c('autocor', 'spec', 'dom'),
                   entropy_threshold = 0.6,
                   pitch_floor = 75,
                   pitch_ceiling = 3500,
                   prior_mean = HzToSemitones(300),
                   prior_sd = 6,
                   prior_plot = FALSE,
                   nCands = 1,
                   min_voiced_cands = 'autom',
                   dom_threshold = 0.1,
                   dom_smoothing = 220,
                   autocor_threshold = 0.7,
                   autocor_smoothing = NULL,
                   cep_threshold = 0.3,
                   cep_smoothing = NULL,
                   cep_zp = 0,
                   spec_threshold = 0.3,
                   spec_peak = 0.35,
                   spec_singlePeakCert = 0.4,
                   spec_peak_HNRslope = 0.8,
                   spec_smoothing = 150,
                   spec_merge = 1,
                   shortest_syl = 20,
                   shortest_pause = 60,
                   interpol_window = 3,
                   interpol_tolerance = 0.3,
                   interpol_cert = 0.3,
                   pathfinding = c('none', 'fast', 'slow')[2],
                   control_anneal = list(maxit = 5000, temp = 1000),
                   cert_weight = .5,
                   snake_step = 0.05,
                   snake_plot = FALSE,
                   smooth_idx = 1,
                   smooth_vars = c('pitch', 'dom'),
                   plot = TRUE,
                   savePath = NA,
                   plot_spec_pars = list(
                     contrast = .2,
                     brightness = 0,
                     ylim = c(0, 5)
                   ),
                   plot_pitch_pars = list(
                     col = rgb(0, 0, 1, .75),
                     lwd = 3
                     ),
                   plot_pitchCands_pars = list(
                     levels = c('autocor', 'spec', 'dom', 'cep'),
                     col = c('green', 'red', 'orange', 'violet'),
                     pch = c(16, 2, 3, 7),
                     cex = 2
                   )) {
  ## preliminaries
  # import a sound
  if (class(x) == 'character') {
    sound = tuneR::readWave(x)
    samplingRate = sound@samp.rate
    sound = sound@left
    plotname = tail(unlist(strsplit(x, '/')), n = 1)
    plotname = substring (plotname, first = 1,
                          last = (nchar(plotname) - 4))
  }  else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop('Please specify "samplingRate", eg 44100')
    } else {
      sound = x
      plotname = ''
    }
  }

  # normalize to range from no less than -1 to no more than +1
  if (min(sound) > 0) {
    sound = scale(sound)
  }
  sound = sound / max(abs(max(sound)), abs(min(sound)))

  # some derived pars, defaults
  if (!is.numeric(silence) || silence < 0 || silence > 1) {
    silence = 0.04
    warning('"silence" must be between 0 and 1; defaulting to 0.04')
  }
  if (!is.numeric(entropy_threshold) || entropy_threshold < 0 || entropy_threshold > 1) {
    entropy_threshold = 0.6
    warning('"entropy_threshold" must be between 0 and 1; defaulting to 0.6')
  }
  if (!is.numeric(windowLength) || windowLength <= 0) {
    windowLength = 50
    warning('"windowLength" must be a positive number;
            defaulting to 50 ms')
  }
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  # to ensure that the window length in points is a power of 2, say 2048 or 1024:
  # windowLength_points = 2^round (log(windowLength * samplingRate /1000)/log(2), 0)
  duration = length(sound) / samplingRate
  supported_wn = c('bartlett', 'blackman', 'flattop', 'gaussian',
                   'hamming', 'hanning', 'rectangle')
  if (!wn %in% supported_wn) {
    wn = 'gaussian'
    warning(paste('implemented "wn":', supported_wn, '; defaulting to "gaussian"'))
  }
  if (!is.numeric(zp)) {
    zp = 0
  } else if (zp < 0) {
    zp = 0
    warning('"zp" must be non-negative; defaulting to 0')
  }
  if (!is.numeric(cutoff_freq)) {
    cutoff_freq = samplingRate / 2
  } else if (cutoff_freq <= 0) {
    cutoff_freq = samplingRate / 2
    warning('"cutoff_freq" must be positive;
            defaulting to samplingRate / 2')
  }
  if (!is.numeric(pitch_floor) || pitch_floor <= 0 || pitch_floor > samplingRate / 2) {
    pitch_floor = 1
    warning('"pitch_floor" must be between 0 and pitch_ceiling;
            defaulting to 1 Hz')
    } # 1 Hz ~ 4 octraves below C0
  if (!is.numeric(pitch_ceiling) || pitch_ceiling > samplingRate / 2) {
    pitch_ceiling = samplingRate / 2  # Nyquist
    warning('"pitch_ceiling" must be between 0 and Nyquist;
            defaulting to samplingRate / 2')
  }
  if (pitch_floor > pitch_ceiling) {
    pitch_floor = 1
    pitch_ceiling = samplingRate / 2
    warning('"pitch_floor" cannot be above "pitch_ceiling";
            defaulting to 1 Hz and samplingRate / 2, respectively')
  }
  if (is.numeric(prior_mean) && semitonesToHz(prior_mean) > samplingRate / 2) {
    prior_mean = HzToSemitones(300)
    warning('Your chosen "prior_mean" exceeds Nyquist frequency;
            defaulting to HzToSemitones(300); set to NULL to disable prior')
  }
  if (is.numeric(prior_mean) &&
      (!is.numeric(prior_sd)) || prior_sd <= 0) {
    prior_sd = 6
    warning('"prior_sd" must be positive; defaulting to 6 semitones')
  }
  if (!is.numeric(step) || !(step > 0)) {
    step = windowLength / 2
    warning('step must be positive; defaulting to windowLength / 2')
  }
  if (is.numeric(step) && step > windowLength) {
    warning('"step" should normally not be larger than "windowLength":
            you are skipping parts of the sound!')
  }
  if (!is.numeric(nCands) || nCands < 1) {
    nCands = 1
    warning('"nCands" must be a positive integer; defaulting to 1')
  } else if (!is.integer(nCands)) {
    nCands = round(nCands)
  }

  if (!is.numeric(dom_threshold) || dom_threshold < 0 || dom_threshold > 1) {
    dom_threshold = 0.1
    warning('"dom_threshold" must be between 0 and 1; defaulting to 0.1')
  }
  if (!is.numeric(autocor_threshold) || autocor_threshold < 0 || autocor_threshold > 1) {
    autocor_threshold = 0.7
    warning('"autocor_threshold" must be between 0 and 1; defaulting to 0.7')
  }
  if (!is.numeric(cep_threshold) || cep_threshold < 0 || cep_threshold > 1) {
    cep_threshold = 0.3
    warning('"cep_threshold" must be between 0 and 1; defaulting to 0.3')
  }
  if (!is.numeric(spec_threshold) || spec_threshold < 0 || spec_threshold > 1) {
    spec_threshold = 0.3
    warning('"spec_threshold" must be between 0 and 1; defaulting to 0.3')
  }
  if (!is.numeric(spec_peak) || spec_peak < 0 || spec_peak > 1) {
    spec_peak = 0.35
    warning('"spec_peak" must be between 0 and 1; defaulting to 0.35')
  }
  if (!is.numeric(spec_singlePeakCert) || spec_singlePeakCert < 0 ||
      spec_singlePeakCert > 1) {
    spec_singlePeakCert = 0.4
    warning('"spec_singlePeakCert" must be between 0 and 1; defaulting to 0.4')
  }
  if (!is.numeric(spec_merge) || spec_merge < 0) {
    spec_merge = 1
    warning('"spec_merge" must be non-negative; defaulting to 1 semitone')
  }

  if (!is.numeric(shortest_syl) || shortest_syl < 0) {
    shortest_syl = 0
    warning('shortest_syl must be a non-negative number; defaulting to 0')
  }
  if (!is.numeric(shortest_pause) || shortest_pause < 0) {
    shortest_pause = 0
    warning('shortest_pause must be a non-negative number; defaulting to 0')
  }
  if (shortest_pause > 0 && is.numeric(interpol_window) &&
      interpol_window * step < shortest_pause / 2) {
    interpol_window = ceiling(shortest_pause / 2 / step)
    warning('"interpol_window" reset to ceiling(shortest_pause / 2 / step):
            interpolation must be able to bridge merged voiced fragments')
  }
  if (is.numeric(interpol_window) &
      (!is.numeric(interpol_tolerance) || interpol_tolerance <= 0)) {
    interpol_tolerance = 0.3
    warning('"interpol_tolerance" must be positive; defaulting to 0.3')
  }
  if (is.numeric(interpol_window) &
      (!is.numeric(interpol_cert) || interpol_cert < 0 | interpol_cert > 1)) {
    interpol_cert = 0.3
    warning('"interpol_tolerance" must be between 0 and 1; defaulting to 0.3')
  }
  if (!pathfinding %in% c('none', 'fast', 'slow')) {
    pathfinding = 'fast'
    warning('implemented "pathfinding": "none", "fast", "slow";
            defaulting to "fast"')
  }
  if (!is.numeric(cert_weight) || cert_weight < 0 | cert_weight > 1) {
    cert_weight = 0.5
    warning('"cert_weight" must be between 0 and 1; defaulting to 0.5')
  }

  # Set up filter for calculating pitchAutocor
  filter = ftwindow_modif(2 * windowLength_points, wn = wn) # plot(filter, type='l')
  powerSpectrum_filter = abs(fft(filter)) ^ 2
  autoCorrelation_filter = abs(fft(powerSpectrum_filter, inverse = TRUE)) ^ 2
  autoCorrelation_filter = autoCorrelation_filter[1:windowLength_points]
  autoCorrelation_filter = autoCorrelation_filter / max(autoCorrelation_filter)
  # plot(autoCorrelation_filter, type = 'l')

  ## fft and acf per frame
  if (is.character(savePath)) {
    # make sure the last character of savePath is "/"
    last_char = substr(savePath, nchar(savePath), nchar(savePath))
    if(last_char != '/') savePath = paste0(savePath, '/')
    plot = TRUE
    f = ifelse(plotname == '',
               'sound',
               plotname)
    jpeg(filename = paste0(savePath, f, ".jpg"), 1200, 800)
  }
  frameBank = getFrameBank(
    sound = sound,
    samplingRate = samplingRate,
    windowLength_points = windowLength_points,
    wn = wn,
    step = step,
    zp = zp,
    filter = NULL
  )

  if (plot == TRUE && is.list(plot_spec_pars)) {
    plot_spec = TRUE
  } else {
    plot_spec = FALSE
    plot_spec_pars = list()  # otherwise can't run do.call('spec') below
  }

  s = do.call('spec', c(
    list(
      x = NULL,
      frameBank = frameBank,
      duration = duration,
      samplingRate = samplingRate,
      windowLength = windowLength,
      zp = zp,
      wn = wn,
      step = step,
      main = plotname,
      plot = plot_spec,
      output = 'original'
    ),
    plot_spec_pars
  ))

  # calculate amplitude of each frame
  myseq = seq(1, (length(sound) - windowLength_points), length.out = ncol(s))
  ampl = apply (as.matrix(1:ncol(s)), 1, function(x) {
    # perceived intensity - root mean square of amplitude
    sqrt(mean(sound[myseq[x]:(myseq[x] + windowLength_points)] ^ 2))
  })
  # dynamically adjust silence threshold
  silence = max(silence, min(ampl))

  # calculate entropy of each frame within the most relevant
  # vocal range only: 50 to 6000 Hz
  rowLow = which(as.numeric(rownames(s)) > 0.05)[1] # 50 Hz
  rowHigh = which(as.numeric(rownames(s)) > 6)[1] # 6000 Hz
  entropy = apply (as.matrix(1:ncol(s)), 1, function(x) {
    getEntropy(s[rowLow:rowHigh, x], type = 'weiner')
  })
  # if the frame is too quiet or too noisy, we will not analyze it
  cond_silence = ampl > silence
  cond_entropy = ampl > silence & entropy < entropy_threshold

  # autocorrelation for each frame
  autocorBank = matrix(NA, nrow = length(autoCorrelation_filter),
                       ncol = ncol(frameBank))
  for (i in which(cond_entropy)) {
    autocorBank[, i] = acf(frameBank[, i],
                           windowLength_points,
                           plot = FALSE)$acf / autoCorrelation_filter
  }
  # plot(autocorBank[, 13], type = 'l')
  rownames(autocorBank) = samplingRate / (1:nrow(autocorBank))

  ## spectral analysis of each frame from fft
  # set up an empty nested list to save values in - this enables us to analyze
  # only the non-silent and not-too-noisy frames but still have a consistently
  # formatted output
  frameInfo = rep(list(list(
    'pitch_array' = data.frame (
      'pitchCand' = NA,
      'pitchAmpl' = NA,
      'source' = NA,
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    'summaries' = data.frame(
      'HNR' = NA,
      'dom' = NA,
      'peakFreq' = NA,
      'peakFreq_cut' = NA,
      'meanFreq' = NA,
      'quartile25' = NA,
      'quartile50' = NA,
      'quartile75' = NA,
      'specSlope' = NA
    )
  )), ncol(s))

  for (i in which(cond_silence)) {
    # for each frame that satisfies our condition, do spectral analysis (NB: we
    # do NOT analyze frames that are too quiet, so we only get NA's for those
    # frames, no meanFreq, dom etc!)
    frameInfo[[i]] = analyzeFrame(
      frame = s[, i],
      autoCorrelation = autocorBank[, i],
      samplingRate = samplingRate,
      trackPitch = cond_entropy[i],
      pitch_methods = pitch_methods,
      cutoff_freq = cutoff_freq,
      autocor_threshold = autocor_threshold,
      autocor_smoothing = autocor_smoothing,
      cep_threshold = cep_threshold,
      cep_smoothing = cep_smoothing,
      cep_zp = cep_zp,
      spec_threshold = spec_threshold,
      spec_peak = spec_peak,
      spec_peak_HNRslope = spec_peak_HNRslope,
      spec_smoothing = spec_smoothing,
      spec_merge = spec_merge,
      pitch_floor = pitch_floor,
      pitch_ceiling = pitch_ceiling,
      dom_threshold = dom_threshold,
      dom_smoothing = dom_smoothing,
      spec_singlePeakCert = spec_singlePeakCert,
      nCands = nCands
    )
  }

  # Store the descriptives provided by function analyzeFrame in a dataframe
  result = lapply(frameInfo, function(y) y[['summaries']])
  result = data.frame(matrix(unlist(result), nrow=length(frameInfo), byrow=TRUE))
  colnames(result) = names(frameInfo[[1]]$summaries)
  # NB: sapply allows to do this in 1 line, but then result$HNR returns a list
  # instead of a vector! Annoying
  # result = matrix(t(sapply(frameInfo, function(y) y[['summaries']])))
  result$ampl = ampl
  result$entropy = entropy
  result$time = round(seq(
    windowLength_points / 2 / samplingRate,
    duration,
    length.out = nrow(result)
  ) * 1000,
  0)
  result$duration = duration

  ## postprocessing
  # extract and prepare pitch candidates for the pathfinder algorithm
  pitch_list = lapply(frameInfo, function(y) y[['pitch_array']])
  pitchCands = lapply(pitch_list, function(y) as.data.frame(t(y[['pitchCand']])))
  pitchCands = t(plyr::rbind.fill(pitchCands)) # a matrix of pitch candidates per frame
  pitchCert = lapply(pitch_list, function(y) as.data.frame(t(y[['pitchAmpl']])))
  pitchCert = t(plyr::rbind.fill(pitchCert)) # a matrix of our certainty in pitch candidates
  pitchSource = lapply(pitch_list, function(y) {
    # NB: without StringsAsFactors=FALSE, the first row becomes "1"
    # because of wrong NA recognition
    as.data.frame(t(y[['source']]), stringsAsFactors = FALSE)
  })
  pitchSource = t(plyr::rbind.fill(pitchSource)) # a matrix of the sources of pitch candidates
  pitch_na = which(is.na(pitchCands))
  pitchCert[pitch_na] = NA
  pitchSource[pitch_na] = NA

  # PRIOR for adjusting the estimated pitch certainties. For ex., if primarily
  # working with speech, we could prioritize pitch candidates in the expected
  # pitch range (100-1000 Hz) and dampen candidates with very high or very low
  # frequency as unlikely but still remotely possible in everyday vocalizing
  # contexts (think a soft pitch ceiling)
  if (is.numeric(prior_mean) & is.numeric(prior_sd)) {
    shape = prior_mean ^ 2 / prior_sd ^ 2
    rate = prior_mean / prior_sd ^ 2
    prior_normalizer = max(dgamma(
      seq(HzToSemitones(pitch_floor), HzToSemitones(pitch_ceiling), length.out = 100),
      shape = shape,
      rate = rate
    ))
    pitchCert_multiplier = dgamma(
      HzToSemitones(pitchCands),
      shape = shape,
      rate = rate
    ) / prior_normalizer
    pitchCert = pitchCert * pitchCert_multiplier
  }

  # divide the file into continuous voiced syllables
  if (!is.numeric(min_voiced_cands) || min_voiced_cands < 1 ||
      min_voiced_cands > length(pitch_methods)) {
    if ('dom' %in% pitch_methods && length(pitch_methods) > 1) {
      # since dom is usually defined, we want at least one more pitch candidate
      # (unless dom is the ONLY method that the user wants for pitch tracking)
      min_voiced_cands = 2
    } else {
      min_voiced_cands = 1
    }
  }
  analFrames = findVoicedSegments(
    pitchCands,
    shortest_syl = shortest_syl,
    shortest_pause = shortest_pause,
    min_voiced_cands = min_voiced_cands,
    step = step,
    samplingRate = samplingRate
  )

  # for each syllable, impute NA's and find a nice path through pitch candidates
  pitchFinal = rep(NA, ncol(pitchCands))
  if (nrow(analFrames) > 0) {
    # if we have found at least one putatively voiced syllable
    for (syl in 1:nrow(analFrames)) {
      myseq = analFrames$segmentStart[syl]:analFrames$segmentEnd[syl]
      # compute the optimal path through pitch candidates
      pitchFinal[myseq] = pathfinder(
        pitchCands = pitchCands[, myseq, drop = FALSE],
        pitchCert = pitchCert[, myseq, drop = FALSE],
        cert_weight = cert_weight,
        pathfinding = pathfinding,
        control_anneal = control_anneal,
        interpol_window = interpol_window,
        interpol_tolerance = interpol_tolerance,
        interpol_cert = interpol_cert,
        snake_step = snake_step,
        snake_plot = snake_plot
      )
    }
  }

  # save optimal pitch track and the best candidates separately for
  # autocor, cepstrum and spectral
  result$pitch = pitchFinal # optimal pitch track
  result$pitchAutocor = as.numeric(lapply(pitch_list, function(x) {
    x$pitchCand[x$source == 'autocor'] [which.max(x$pitchAmpl[x$source == 'autocor'])]
  }))
  result$pitchCep = as.numeric(lapply(pitch_list, function(x) {
    x$pitchCand[x$source == 'cep'] [which.max(x$pitchAmpl[x$source == 'cep'])]
  }))
  result$pitchSpec = as.numeric(lapply(pitch_list, function(x) {
    x$pitchCand[x$source == 'spec'] [which.max(x$pitchAmpl[x$source == 'spec'])]
  }))

  ## Median smoothing of specified contours (by default pitch & dom)
  if (is.numeric(smooth_idx) && smooth_idx > 0) {
    points_per_sec = nrow(result) / duration
    # smooth_idx of 1 means that smoothing window is ~100 ms
    smoothing_ww = round (smooth_idx * points_per_sec / 10, 0)
    # the larger smooth_idx, the heavier the smoothing (lower tolerance
    # threshold before values are replaced by median over smoothing window).
    # smooth_idx of 1 gives smoothing_threshold of 4 semitones
    smoothing_threshold = 4 / smooth_idx
    result[smooth_vars] = medianSmoother(result[smooth_vars],
                                         smoothing_ww = smoothing_ww,
                                         smoothing_threshold = smoothing_threshold)
  }

  ## Having decided upon the pitch for each frame, we save certain measurements
  # only for voiced frames (with non-NA pitch)
  voiced_idx = which(!is.na(result$pitch))
  unvoiced_idx = which(is.na(result$pitch))
  result$ampl_voiced = NA
  result$ampl_voiced[voiced_idx] = result$ampl[voiced_idx]
  result[unvoiced_idx, c('quartile25', 'quartile50', 'quartile75')] = NA
  result$voiced = FALSE
  result$voiced[voiced_idx] = TRUE

  # Calculate the % of energy in harmonics based on the final pitch estimates
  threshold = 1.25 * result$pitch / 1000
  result$harmonics = apply(matrix(1:ncol(s)), 1, function(x) {
    ifelse(is.na(threshold[x]),
           NA,
           sum(s[as.numeric(rownames(s)) > threshold[x], x]) / sum(s[, x]))
  })

  # Convert HNR and harmonics to dB
  result$HNR = to_dB(result$HNR)
  result$harmonics = to_dB(result$harmonics)

  ## Add pitch contours to the spectrogram
  if (plot) {
    # if plot_spec is FALSE, we first have to set up an empty plot
    if (plot_spec == FALSE) {
      m = max(pitchCands, na.rm = TRUE) / 1000  # for ylim on the empty plot
      if (is.na(m)) m = samplingRate / 2 / 1000
      plot(x = result$time,
           y = rep(0, nrow(result)),
           type = 'n',
           ylim = c(0, m), xlab = 'Time, ms', ylab = 'Pitch, kHz')
    }
    # add pitch candidates to the plot
    if (nrow(pitchCands) > 0) {
      if (is.list(plot_pitchCands_pars)) {
        if (is.null(plot_pitchCands_pars$levels)) {
          plot_pitchCands_pars$levels = c('autocor', 'cep', 'spec', 'dom')
        }
        if (is.null(plot_pitchCands_pars$col)) {
          plot_pitchCands_pars$col = c('green', 'violet', 'red', 'orange')
        }
        if (is.null(plot_pitchCands_pars$pch)) {
          plot_pitchCands_pars$pch = c(16, 7, 2, 3)
        }
        if (is.null(plot_pitchCands_pars$cex)) {
          plot_pitchCands_pars$cex = 2
        }
        pitchSource_1234 = matrix(match(pitchSource, plot_pitchCands_pars$levels),
                                  ncol = ncol(pitchSource))
        for (r in 1:nrow(pitchCands)) {
          points(
            x = result$time,
            y = pitchCands[r, ] / 1000,
            col = plot_pitchCands_pars$col[pitchSource_1234[r, ]],
            pch = plot_pitchCands_pars$pch[pitchSource_1234[r, ]],
            cex = pitchCert[r, ] * plot_pitchCands_pars$cex
          )
        }
      }
      # add the final pitch contour to the plot
      if (is.list(plot_pitch_pars)) {
        do.call('lines', c(list(
          x = result$time,
          y = result$pitch / 1000
        ),
        plot_pitch_pars)
        )
      }
    }
  }
  if (is.character(savePath)) {
    dev.off()
  }

  # a separate plot of the prior
  if (prior_plot) {
    freqs = seq(1, HzToSemitones(samplingRate / 2), length.out = 1000)
    prior = dgamma(freqs, shape = shape, rate = rate) / prior_normalizer
    plot(semitonesToHz(freqs), prior, type = 'l', xlab = 'Frequency, Hz',
         ylab = 'Multiplier of certainty', main = 'Prior beliefs in pitch values')
  }

  result = result[c('duration', 'time', 'voiced', 'ampl', 'ampl_voiced',
                    'entropy', 'HNR', 'dom', 'meanFreq', 'peakFreq', 'peakFreq_cut',
                    'pitch', 'pitchAutocor', 'pitchCep', 'pitchSpec',
                    'quartile25', 'quartile50', 'quartile75', 'specSlope', 'harmonics'
  )]
  return (result)
}


#' Analyze sound
#'
#' Acoustic analysis of all .wav files in a folder.
#' @param myfolder full path to target folder
#' @inheritParams analyze
#' @param summary if TRUE, summarizes acoustics per file
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
#' s = analyzeFolder(myfolder, pathfinding = 'slow', verbose = TRUE)
#'
#' # Check accuracy: import manually verified pitch values (our "key")
#' key = pitch_manual  # a vector of 260 floats
#' trial = s$pitch_median
#' cor(key, trial, use = 'pairwise.complete.obs')
#' plot(log(key), log(trial))
#' abline(a=0, b=1, col='red')
#' }
analyzeFolder = function(myfolder,
                         samplingRate = NULL,
                         silence = 0.04,
                         windowLength = 50,
                         wn = 'gaussian',
                         step = 25,
                         zp = 0,
                         cutoff_freq = 6000,
                         pitch_methods = c('autocor', 'spec', 'dom'),
                         entropy_threshold = 0.6,
                         pitch_floor = 75,
                         pitch_ceiling = 3500,
                         prior_mean = HzToSemitones(300),
                         prior_sd = 6,
                         prior_plot = FALSE,
                         nCands = 1,
                         min_voiced_cands = 'autom',
                         dom_threshold = 0.1,
                         dom_smoothing = 220,
                         autocor_threshold = 0.7,
                         autocor_smoothing = NULL,
                         cep_threshold = 0.3,
                         cep_smoothing = NULL,
                         cep_zp = 0,
                         spec_threshold = 0.3,
                         spec_peak = 0.35,
                         spec_singlePeakCert = 0.4,
                         spec_peak_HNRslope = 0.8,
                         spec_smoothing = 150,
                         spec_merge = 1,
                         shortest_syl = 20,
                         shortest_pause = 60,
                         interpol_window = 3,
                         interpol_tolerance = 0.3,
                         interpol_cert = 0.3,
                         pathfinding = c('none', 'fast', 'slow')[2],
                         control_anneal = list(maxit = 5000, temp = 1000),
                         cert_weight = .5,
                         snake_step = 0.05,
                         snake_plot = FALSE,
                         smooth_idx = 1,
                         smooth_vars = c('pitch', 'dom'),
                         plot = TRUE,
                         savePath = NA,
                         plot_spec_pars = list(
                           contrast = .2,
                           brightness = 0,
                           ylim = c(0, 5)
                         ),
                         plot_pitch_pars = list(
                           col = rgb(0, 0, 1, .75),
                           lwd = 3
                         ),
                         plot_pitchCands_pars = list(
                           levels = c('autocor', 'cep', 'spec', 'dom'),
                           col = c('green', 'violet', 'red', 'orange'),
                           pch = c(16, 7, 2, 3),
                           cex = 2
                         ),
                         summary = TRUE,
                         verbose = TRUE) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav", full.names = TRUE)
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = apply(as.matrix(filenames), 1, function(x) file.info(x)$size)
  myPars = list(
    samplingRate = samplingRate,
    silence = silence,
    windowLength = windowLength,
    wn = wn,
    step = step,
    zp = zp,
    cutoff_freq = cutoff_freq,
    pitch_methods = pitch_methods,
    entropy_threshold = entropy_threshold,
    pitch_floor = pitch_floor,
    pitch_ceiling = pitch_ceiling,
    prior_mean = prior_mean,
    prior_sd = prior_sd,
    prior_plot = prior_plot,
    nCands = nCands,
    min_voiced_cands = min_voiced_cands,
    dom_threshold = dom_threshold,
    dom_smoothing = dom_smoothing,
    autocor_threshold = autocor_threshold,
    autocor_smoothing = autocor_smoothing,
    cep_threshold = cep_threshold,
    cep_smoothing = cep_smoothing,
    cep_zp = cep_zp,
    spec_threshold = spec_threshold,
    spec_peak = spec_peak,
    spec_singlePeakCert = spec_singlePeakCert,
    spec_peak_HNRslope = spec_peak_HNRslope,
    spec_smoothing = spec_smoothing,
    spec_merge = spec_merge,
    shortest_syl = shortest_syl,
    shortest_pause = shortest_pause,
    interpol_window = interpol_window,
    interpol_tolerance = interpol_tolerance,
    interpol_cert = interpol_cert,
    pathfinding = pathfinding,
    control_anneal = control_anneal,
    cert_weight = cert_weight,
    snake_step = snake_step,
    snake_plot = snake_plot,
    smooth_idx = smooth_idx,
    smooth_vars = smooth_vars,
    plot = plot,
    savePath = savePath,
    plot_spec_pars = plot_spec_pars,
    plot_pitch_pars = plot_pitch_pars,
    plot_pitchCands_pars = plot_pitchCands_pars
  )

  if (summary == FALSE) {
    out = list()
    for (i in 1:length(filenames)) {
      out[[i]] = do.call(analyze, c(filenames[i], myPars))
      if (verbose) {
        reportTime(i = i, nIter = length(filenames),
                   time_start = time_start, jobs = filesizes)
      }
    }
  } else if (summary == TRUE) {
    vars = c('ampl', 'ampl_voiced', 'entropy', 'HNR', 'dom', 'meanFreq', 'peakFreq',
             'peakFreq_cut', 'pitch', 'pitchAutocor', 'pitchCep', 'pitchSpec',
             'quartile25', 'quartile50', 'quartile75', 'specSlope', 'harmonics')
    out = as.data.frame(matrix(
      ncol = 3 + 2 * length(vars),
      nrow = length(filenames)
    ))
    colnames(out)[c(1:3)] = c('file', 'duration', 'voiced')
    for (c in 1:length(vars)) {
      # specify how to summarize pitch etc values for each frame within each file
      # - save median, sd, ... "3+2*c-1": "3" because of file/dur/voiced above,
      # "+2*c" because for each acoustic variable, we save median and sd
      colnames(out)[3 + 2 * c - 1] = paste0(vars[c], '_', 'median')
      colnames(out)[3 + 2 * c] = paste0(vars[c], '_', 'sd')
    }
    # which columns in the output of pitch_per_sound to save as median + sd
    myseq = (1:length(vars)) + 3

    for (i in 1:length(filenames)) {
      temp = do.call(analyze, c(filenames[i], myPars))
      out[i, 1] = tail (unlist (strsplit(filenames[i], '/')), n = 1)
      out[i, 2] = temp[1, 'duration']  # duration, ms
      out[i, 3] = mean(temp[, 'voiced'])  # proportion of voiced frames

      for (v in 1:length(myseq)) {
        myvar = colnames(temp)[myseq[v]]
        out[i, 2 * v + 2] = median(temp[, myvar], na.rm = TRUE)
        out[i, 2 * v + 3] = sd(temp[, myvar], na.rm = TRUE)
      }

      if (verbose) {
        reportTime(i = i, nIter = length(filenames),
                   time_start = time_start, jobs = filesizes)
      }
    }
  }

  return (out)
}
