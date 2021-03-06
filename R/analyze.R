### MAIN FUNCTIONS FOR ACOUSTIC ANALYSIS ###

#' Analyze sound
#'
#' Acoustic analysis of a single sound file: pitch tracking and basic spectral
#' characteristics. The default values of arguments are optimized for human
#' non-linguistic vocalizations. See the vignette on acoustic analysis for
#' details.
#'
#' @inheritParams spectrogram
#' @param silence (0 to 1) frames with mean abs amplitude below silence
#'   threshold are not analyzed at all. NB: this number is dynamically updated:
#'   the actual silence threshold may be higher depending on the quietest frame,
#'   but it will never be lower than this specified number.
#' @param cutFreq (>0 to Nyquist, Hz) repeat the calculation of spectral
#'   descriptives after discarding all info above \code{cutFreq}.
#'   Recommended if the original sampling rate varies across different analyzed
#'   audio files
#' @param nFormants the number of formants to extract per FFT frame. Calls
#'   \code{\link[phonTools]{findformants}} with default settings
#' @param pitchMethods methods of pitch estimation to consider for determining
#'   pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
#'   'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band
#' @param entropyThres pitch tracking is not performed for frames with Weiner
#'   entropy above \code{entropyThres}, but other spectral descriptives are
#'   still calculated
#' @param pitchFloor,pitchCeiling absolute bounds for pitch candidates (Hz)
#' @param priorMean,priorSD specifies the mean and sd of gamma distribution
#'   describing our prior knowledge about the most likely pitch values for this
#'   file. Specified in semitones: \code{priorMean = HzToSemitones(300),
#'   priorSD = 6} gives a prior with mean = 300 Hz and SD of 6 semitones (half
#'   an octave)
#' @param priorPlot if TRUE, produces a separate plot of the prior
#' @param nCands maximum number of pitch candidates per method (except for
#'   \code{dom}, which returns at most one candidate per frame), normally 1...4
#' @param minVoicedCands minimum number of pitch candidates that
#'   have to be defined to consider a frame voiced (defaults to 2 if \code{dom}
#'   is among other candidates and 1 otherwise)
#' @param domThres (0 to 1) to find the lowest dominant frequency band, we
#'   do short-term FFT and take the lowest frequency with amplitude at least
#'   domThres
#' @param domSmooth the width of smoothing interval (Hz) for finding
#'   \code{dom}
#' @param autocorThres,cepThres,specThres (0 to 1) separate
#'   voicing thresholds for detecting pitch candidates with three different
#'   methods: autocorrelation, cepstrum, and BaNa algorithm (see Details). Note
#'   that HNR is calculated even for unvoiced frames.
#' @param autocorSmooth the width of smoothing interval (in bins) for
#'   finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'   rate 44100 and smaller odd numbers for lower values of sampling rate
#' @param cepSmooth the width of smoothing interval (in bins) for finding
#'   peaks in the cepstrum. Defaults to 31 for sampling rate 44100 and smaller
#'   odd numbers for lower values of sampling rate
#' @param cepZp zero-padding of the spectrum used for cepstral pitch detection
#'   (final length of spectrum after zero-padding in points, e.g. 2 ^ 13)
#' @param specPeak,specHNRslope when looking for putative harmonics in
#'   the spectrum, the threshold for peak detection is calculated as
#'   \code{specPeak * (1 - HNR * specHNRslope)}
#' @param specSmooth the width of window for detecting peaks in the spectrum, Hz
#' @param specMerge pitch candidates within \code{specMerge} semitones are
#'   merged with boosted certainty
#' @param specSinglePeakCert (0 to 1) if F0 is calculated based on a single
#'   harmonic ratio (as opposed to several ratios converging on the same
#'   candidate), its certainty is taken to be \code{specSinglePeakCert}
#' @param shortestSyl the smallest length of a voiced segment (ms) that
#'   constitutes a voiced syllable (shorter segments will be replaced by NA, as
#'   if unvoiced)
#' @param shortestPause the smallest gap between voiced syllables (ms) that
#'   means they shouldn't be merged into one voiced syllable
#' @param interpolWin,interpolTol,interpolCert control the behavior of
#'   interpolation algorithm when postprocessing pitch candidates. To turn off
#'   interpolation, set \code{interpolWin} to NULL. See
#'   \code{soundgen:::pathfinder} for details.
#' @param pathfinding method of finding the optimal path through pitch
#'   candidates: 'none' = best candidate per frame, 'fast' = simple heuristic,
#'   'slow' = annealing. See \code{soundgen:::pathfinder}
#' @param annealPars a list of control parameters for postprocessing of
#'   pitch contour with SANN algorithm of \code{\link[stats]{optim}}. This is
#'   only relevant if \code{pathfinding = 'slow'}
#' @param certWeight (0 to 1) in pitch postprocessing, specifies how much we
#'   prioritize the certainty of pitch candidates vs. pitch jumps / the internal
#'   tension of the resulting pitch curve
#' @param snakeStep optimized path through pitch candidates is further
#'   processed to minimize the elastic force acting on pitch contour. To
#'   disable, set \code{snakeStep} to NULL
#' @param snakePlot if TRUE, plots the snake
#' @param smooth,smoothVars if \code{smooth} is a positive number, outliers of
#'   the variables in \code{smoothVars} are adjusted with median smoothing.
#'   \code{smooth} of 1 corresponds to a window of ~100 ms and tolerated
#'   deviation of ~4 semitones. To disable, set \code{smooth} to NULL
#' @param summary if TRUE, returns only a summary of the measured acoustic
#'   variables (mean, median and SD). If FALSE, returns a list containing
#'   frame-by-frame values
#' @param plot if TRUE, produces a spectrogram with pitch contour overlaid
#' @param savePath if a valid path is specified, a plot is saved in this
#'   folder (defaults to NA)
#' @param specPlot a list of graphical parameters passed to
#'   \code{\link{spectrogram}}. Set to \code{NULL} to suppress plotting just the
#'   spectrogram
#' @param candPlot a list of graphical parameters for displaying
#'   individual pitch candidates. Set to \code{NULL} or \code{NA} to suppress
#' @param pitchPlot a list of graphical parameters for displaying the
#'   final pitch contour. Set to \code{NULL} or \code{NA} to suppress
#' @return If \code{summary = TRUE}, returns a dataframe with one row and three
#'   column per acoustic variable (mean / median / SD). If \code{summary =
#'   FALSE}, returns a dataframe with one row per FFT frame and one column per
#'   acoustic variable. The best guess at the pitch contour considering all
#'   available information is stored in the variable called "pitch". In
#'   addition, the output contains a number of other acoustic descriptors and
#'   pitch estimates by separate algorithms included in \code{pitchMethods}. See
#'   the vignette on acoustic analysis for a full explanation of returned
#'   measures.
#' @export
#' @examples
#' \dontrun{
#' sound1 = soundgen(sylLen = 900, pitchAnchors = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noiseAnchors = list(time = c(0, 900), value = c(-40, 00)),
#'   temperature = 0)
#' # playme(sound1, 16000)
#' a1 = analyze(sound1, samplingRate = 16000, plot = TRUE)
#' # or, to improve the quality of postprocessing:
#' a1 = analyze(sound1, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
#' median(a1$pitch, na.rm = TRUE)  # 614 Hz
#' # (can vary, since postprocessing is stochastic)
#' # compare to the true value:
#' median(getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
#'   value = c(300, 900, 400, 2300)), len = 1000))
#'
#' # the same pitch contour, but harder b/c of subharmonics and jitter
#' sound2 = soundgen(sylLen = 900, temperature = 0,
#'                   pitchAnchors = list(time = c(0, .3, .8, 1),
#'                                       value = c(300, 900, 400, 2300)),
#'                   noiseAnchors = list(time = c(0, 900), value = c(-40, 20)),
#'                   subDep = 100, jitterDep = 0.5, pitchEffectsAmount = 100)
#' sound2 = soundgen(sylLen = 900, pitchAnchors = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noiseAnchors = list(time = c(0, 900), value = c(-40, 20)),
#'   subDep = 100, jitterDep = 0.5, pitchEffects_amount = 100, temperature = 0)
#' # playme(sound2, 16000)
#' a2 = analyze(sound2, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
#' # many candidates are off, but the overall contour should be mostly accurate
#'
#' # Fancy plotting options:
#' a = analyze(sound2, samplingRate = 16000, plot = TRUE,
#'   specPlot = list(xlab = 'Time, ms', colorTheme = 'seewave', contrast = .8),
#'   candPlot = list(cex = 3, col = c('gray70', 'yellow', 'purple', 'maroon')),
#'   pitchPlot = list(col = 'black', lty = 3, lwd = 3))
#'
#'# Plot pitch candidates w/o a spectrogram
#' a = analyze(sound2, samplingRate = 16000, plot = TRUE, specPlot = NA)
#' }
analyze = function(x,
                   samplingRate = NULL,
                   silence = 0.04,
                   windowLength = 50,
                   step = NULL,
                   overlap = 50,
                   wn = 'gaussian',
                   zp = 0,
                   cutFreq = 6000,
                   nFormants = 3,
                   pitchMethods = c('autocor', 'spec', 'dom'),
                   entropyThres = 0.6,
                   pitchFloor = 75,
                   pitchCeiling = 3500,
                   priorMean = HzToSemitones(300),
                   priorSD = 6,
                   priorPlot = FALSE,
                   nCands = 1,
                   minVoicedCands = 'autom',
                   domThres = 0.1,
                   domSmooth = 220,
                   autocorThres = 0.7,
                   autocorSmooth = NULL,
                   cepThres = 0.3,
                   cepSmooth = NULL,
                   cepZp = 0,
                   specThres = 0.3,
                   specPeak = 0.35,
                   specSinglePeakCert = 0.4,
                   specHNRslope = 0.8,
                   specSmooth = 150,
                   specMerge = 1,
                   shortestSyl = 20,
                   shortestPause = 60,
                   interpolWin = 3,
                   interpolTol = 0.3,
                   interpolCert = 0.3,
                   pathfinding = c('none', 'fast', 'slow')[2],
                   annealPars = list(maxit = 5000, temp = 1000),
                   certWeight = .5,
                   snakeStep = 0.05,
                   snakePlot = FALSE,
                   smooth = 1,
                   smoothVars = c('pitch', 'dom'),
                   summary = FALSE,
                   plot = TRUE,
                   savePath = NA,
                   specPlot = list(
                     contrast = .2,
                     brightness = 0,
                     ylim = c(0, 5)
                   ),
                   pitchPlot = list(
                     col = rgb(0, 0, 1, .75),
                     lwd = 3
                   ),
                   candPlot = list(
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
  if (!is.numeric(entropyThres) || entropyThres < 0 || entropyThres > 1) {
    entropyThres = 0.6
    warning('"entropyThres" must be between 0 and 1; defaulting to 0.6')
  }
  duration = length(sound) / samplingRate
  if (!is.numeric(windowLength) || windowLength <= 0 ||
      windowLength > (duration * 1000)) {
    windowLength = 50
    warning('"windowLength" must be between 0 and sound_duration ms;
            defaulting to 50 ms')
  }
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  # to ensure that the window length in points is a power of 2, say 2048 or 1024:
  # windowLength_points = 2^round (log(windowLength * samplingRate /1000)/log(2), 0)
  if (!is.numeric(step)) {
    if (!is.numeric(overlap) || overlap < 0 || overlap > 99) {
      overlap = 50
      warning('If "step" is not specified, overlap must be between 0 and 99%',
              '; overlap reset to 50%')
    } else {
      step = windowLength * (1 - overlap / 100)
    }
  } else {
    if (is.numeric(overlap) && overlap != 50) {  # step specified, overlap != default
      warning('"overlap" is ignored if "step" is not NULL')
    }
  }
  if (step <= 0 || step > (duration * 1000)) {
    step = windowLength / 2
    warning('"step" must be between 0 and sound_duration ms;
            defaulting to windowLength / 2')
  }
  if (step > windowLength) {
    warning(paste('"step" should normally not be larger than "windowLength" ms:',
                  'you are skipping parts of the sound!'))
  }
  supported_wn = c('bartlett', 'blackman', 'flattop', 'gaussian',
                   'hamming', 'hanning', 'rectangle')
  if (!wn %in% supported_wn) {
    wn = 'gaussian'
    warning(paste('Implemented "wn":',
                  paste(supported_wn, collapse = ', '),
                  '. Defaulting to "gaussian"'))
  }
  if (!is.numeric(zp)) {
    zp = 0
  } else if (zp < 0) {
    zp = 0
    warning('"zp" must be non-negative; defaulting to 0')
  }
  if (!is.numeric(cutFreq) || cutFreq <= 0) {
    cutFreq = samplingRate / 2
    warning(paste('"cutFreq" must be positive;',
                  'defaulting to samplingRate / 2'))
  }
  if (!is.numeric(pitchFloor) || pitchFloor <= 0 ||
      pitchFloor > samplingRate / 2) {
    pitchFloor = 1
    warning(paste('"pitchFloor" must be between 0 and pitchCeiling;',
                  'defaulting to 1 Hz'))
  } # 1 Hz ~ 4 octraves below C0
  if (!is.numeric(pitchCeiling) || pitchCeiling > samplingRate / 2) {
    pitchCeiling = samplingRate / 2  # Nyquist
    warning(paste('"pitchCeiling" must be between 0 and Nyquist;',
                  'defaulting to samplingRate / 2'))
  }
  if (pitchFloor > pitchCeiling) {
    pitchFloor = 1
    pitchCeiling = samplingRate / 2
    warning(paste('"pitchFloor" cannot be above "pitchCeiling";',
                  'defaulting to 1 Hz and samplingRate / 2, respectively'))
  }
  if (is.numeric(priorMean) &&
      (semitonesToHz(priorMean) > samplingRate / 2 ||
       semitonesToHz(priorMean) <= 0)) {
    priorMean = HzToSemitones(300)
    warning(paste('"priorMean" must be between 0 and Nyquist;',
                  'defaulting to HzToSemitones(300); set to NULL to disable prior'))
  }
  if (is.numeric(priorMean) &&
      (!is.numeric(priorSD)) || priorSD <= 0) {
    priorSD = 6
    warning('"priorSD" must be positive; defaulting to 6 semitones')
  }
  if (!is.numeric(nCands) || nCands < 1) {
    nCands = 1
    warning('"nCands" must be a positive integer; defaulting to 1')
  } else if (!is.integer(nCands)) {
    nCands = round(nCands)
  }

  if (!is.numeric(domThres) || domThres < 0 || domThres > 1) {
    domThres = 0.1
    warning('"domThres" must be between 0 and 1; defaulting to 0.1')
  }
  if (!is.numeric(autocorThres) || autocorThres < 0 || autocorThres > 1) {
    autocorThres = 0.7
    warning('"autocorThres" must be between 0 and 1; defaulting to 0.7')
  }
  if (!is.numeric(cepThres) || cepThres < 0 || cepThres > 1) {
    cepThres = 0.3
    warning('"cepThres" must be between 0 and 1; defaulting to 0.3')
  }
  if (!is.numeric(specThres) || specThres < 0 || specThres > 1) {
    specThres = 0.3
    warning('"specThres" must be between 0 and 1; defaulting to 0.3')
  }
  if (!is.numeric(specPeak) || specPeak < 0 || specPeak > 1) {
    specPeak = 0.35
    warning('"specPeak" must be between 0 and 1; defaulting to 0.35')
  }
  if (!is.numeric(specSinglePeakCert) || specSinglePeakCert < 0 ||
      specSinglePeakCert > 1) {
    specSinglePeakCert = 0.4
    warning('"specSinglePeakCert" must be between 0 and 1; defaulting to 0.4')
  }
  if (!is.numeric(specMerge) || specMerge < 0) {
    specMerge = 1
    warning('"specMerge" must be non-negative; defaulting to 1 semitone')
  }

  if (!is.numeric(shortestSyl) || shortestSyl < 0) {
    shortestSyl = 0
    warning('shortestSyl must be non-negative; defaulting to 0')
  }
  if (shortestSyl > duration * 1000) {
    warning('"shortestSyl" is longer than the sound')
  }
  if (!is.numeric(shortestPause) || shortestPause < 0) {
    shortestPause = 0
    warning('shortestPause must be a non-negative number; defaulting to 0')
  }
  if (shortestPause > 0 && is.numeric(interpolWin) &&
      interpolWin * step < shortestPause / 2) {
    interpolWin = ceiling(shortestPause / 2 / step)
    warning(paste('"interpolWin" reset to', interpolWin,
                  ': interpolation must be able to bridge merged voiced fragments'))
  }
  if (is.numeric(interpolWin) &
      (!is.numeric(interpolTol) || interpolTol <= 0)) {
    interpolTol = 0.3
    warning('"interpolTol" must be positive; defaulting to 0.3')
  }
  if (is.numeric(interpolWin) &
      (!is.numeric(interpolCert) || interpolCert < 0 | interpolCert > 1)) {
    interpolCert = 0.3
    warning('"interpolTol" must be between 0 and 1; defaulting to 0.3')
  }
  if (!pathfinding %in% c('none', 'fast', 'slow')) {
    pathfinding = 'fast'
    warning(paste('Implemented "pathfinding": "none", "fast", "slow";',
                  'defaulting to "fast"'))
  }
  if (!is.numeric(certWeight) || certWeight < 0 | certWeight > 1) {
    certWeight = 0.5
    warning('"certWeight" must be between 0 and 1; defaulting to 0.5')
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

  if (plot == TRUE && is.list(specPlot)) {
    plot_spec = TRUE
  } else {
    plot_spec = FALSE
    specPlot = list()  # otherwise can't run do.call('spec') below
  }

  s = do.call('spectrogram', c(
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
    specPlot
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
  cond_entropy = ampl > silence & entropy < entropyThres

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

  ## FORMANTS
  framesToAnalyze = which(cond_silence)
  formants = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
  colnames(formants) = paste0('f', rep(1:nFormants, each = 2),
                              rep(c('_freq', '_width'), nFormants))
  for (i in framesToAnalyze) {
    ff = try(phonTools::findformants(frameBank[, i],
                                     fs = samplingRate,
                                     verify = FALSE),
             silent = TRUE)
    if (class(ff) != 'try-error' && is.list(ff)) {
      temp = matrix(NA, nrow = nFormants, ncol = 2)
      availableRows = 1:min(nFormants, nrow(ff))
      temp[availableRows, ] = as.matrix(ff[availableRows, ])
      formants[i, ] = matrix(t(temp), nrow = 1)
    }
  }

  ## PITCH and other spectral analysis of each frame from fft
  # set up an empty nested list to save values in - this enables us to analyze
  # only the non-silent and not-too-noisy frames but still have a consistently
  # formatted output
  frameInfo = rep(list(list(
    'pitch_array' = data.frame(
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
      'peakFreqCut' = NA,
      'meanFreq' = NA,
      'quartile25' = NA,
      'quartile50' = NA,
      'quartile75' = NA,
      'specSlope' = NA
    )
  )), ncol(s))

  for (i in framesToAnalyze) {
    # for each frame that satisfies our condition, do spectral analysis (NB: we
    # do NOT analyze frames that are too quiet, so we only get NA's for those
    # frames, no meanFreq, dom etc!)
    frameInfo[[i]] = analyzeFrame(
      frame = s[, i],
      autoCorrelation = autocorBank[, i],
      samplingRate = samplingRate,
      trackPitch = cond_entropy[i],
      pitchMethods = pitchMethods,
      cutFreq = cutFreq,
      autocorThres = autocorThres,
      autocorSmooth = autocorSmooth,
      cepThres = cepThres,
      cepSmooth = cepSmooth,
      cepZp = cepZp,
      specThres = specThres,
      specPeak = specPeak,
      specHNRslope = specHNRslope,
      specSmooth = specSmooth,
      specMerge = specMerge,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling,
      domThres = domThres,
      domSmooth = domSmooth,
      specSinglePeakCert = specSinglePeakCert,
      nCands = nCands
    )
  }

  # Store the descriptives provided by function analyzeFrame in a dataframe
  result = lapply(frameInfo, function(y) y[['summaries']])
  result = data.frame(matrix(unlist(result), nrow=length(frameInfo), byrow=TRUE))
  colnames(result) = names(frameInfo[[1]]$summaries)
  result = cbind(result, formants)
  result$entropy = entropy
  result$ampl = ampl
  result$time = round(seq(
    step / 2,  # windowLength_points / 2 / samplingRate,
    duration * 1000 - step / 2,
    length.out = nrow(result)
  ),
  0)
  result$duration = duration
  c = ncol(result)
  result = result[, c(rev((c-3):c), 1:(c-4))]  # change the order of columns

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
  if (is.numeric(priorMean) & is.numeric(priorSD)) {
    shape = priorMean ^ 2 / priorSD ^ 2
    rate = priorMean / priorSD ^ 2
    prior_normalizer = max(dgamma(
      seq(HzToSemitones(pitchFloor), HzToSemitones(pitchCeiling), length.out = 100),
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
  if (!is.numeric(minVoicedCands) || minVoicedCands < 1 ||
      minVoicedCands > length(pitchMethods)) {
    if ('dom' %in% pitchMethods && length(pitchMethods) > 1) {
      # since dom is usually defined, we want at least one more pitch candidate
      # (unless dom is the ONLY method that the user wants for pitch tracking)
      minVoicedCands = 2
    } else {
      minVoicedCands = 1
    }
  }
  voicedSegments = findVoicedSegments(
    pitchCands,
    shortestSyl = shortestSyl,
    shortestPause = shortestPause,
    minVoicedCands = minVoicedCands,
    step = step,
    samplingRate = samplingRate
  )

  # for each syllable, impute NA's and find a nice path through pitch candidates
  pitchFinal = rep(NA, ncol(pitchCands))
  if (nrow(voicedSegments) > 0) {
    # if we have found at least one putatively voiced syllable
    for (syl in 1:nrow(voicedSegments)) {
      myseq = voicedSegments$segmentStart[syl]:voicedSegments$segmentEnd[syl]
      # compute the optimal path through pitch candidates
      pitchFinal[myseq] = pathfinder(
        pitchCands = pitchCands[, myseq, drop = FALSE],
        pitchCert = pitchCert[, myseq, drop = FALSE],
        certWeight = certWeight,
        pathfinding = pathfinding,
        annealPars = annealPars,
        interpolWin = interpolWin,
        interpolTol = interpolTol,
        interpolCert = interpolCert,
        snakeStep = snakeStep,
        snakePlot = snakePlot
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
  if (is.numeric(smooth) && smooth > 0) {
    points_per_sec = nrow(result) / duration
    # smooth of 1 means that smoothing window is ~100 ms
    smoothing_ww = round(smooth * points_per_sec / 10, 0)
    # the larger smooth, the heavier the smoothing (lower tolerance
    # threshold before values are replaced by median over smoothing window).
    # smooth of 1 gives smoothingThres of 4 semitones
    smoothingThres = 4 / smooth
    result[smoothVars] = medianSmoother(result[smoothVars],
                                        smoothing_ww = smoothing_ww,
                                        smoothingThres = smoothingThres)
  }

  ## Having decided upon the pitch for each frame, we save certain measurements
  # only for voiced frames (with non-NA pitch)
  voiced_idx = which(!is.na(result$pitch))
  unvoiced_idx = which(is.na(result$pitch))
  result$amplVoiced = NA
  result$amplVoiced[voiced_idx] = result$ampl[voiced_idx]
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

  # Arrange columns in alphabetical order (except the first two)
  result = result[, c(colnames(result)[1:2],
                      sort(colnames(result)[3:ncol(result)]))]

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
      if (is.list(candPlot)) {
        if (is.null(candPlot$levels)) {
          candPlot$levels = c('autocor', 'cep', 'spec', 'dom')
        }
        if (is.null(candPlot$col)) {
          candPlot$col = c('green', 'violet', 'red', 'orange')
        }
        if (is.null(candPlot$pch)) {
          candPlot$pch = c(16, 7, 2, 3)
        }
        if (is.null(candPlot$cex)) {
          candPlot$cex = 2
        }
        pitchSource_1234 = matrix(match(pitchSource, candPlot$levels),
                                  ncol = ncol(pitchSource))
        for (r in 1:nrow(pitchCands)) {
          points(
            x = result$time,
            y = pitchCands[r, ] / 1000,
            col = candPlot$col[pitchSource_1234[r, ]],
            pch = candPlot$pch[pitchSource_1234[r, ]],
            cex = pitchCert[r, ] * candPlot$cex
          )
        }
      }
      # add the final pitch contour to the plot
      if (is.list(pitchPlot)) {
        do.call('lines', c(list(
          x = result$time,
          y = result$pitch / 1000
        ),
        pitchPlot)
        )
      }
    }
  }
  if (is.character(savePath)) {
    dev.off()
  }

  # a separate plot of the prior
  if (priorPlot) {
    freqs = seq(1, HzToSemitones(samplingRate / 2), length.out = 1000)
    prior = dgamma(freqs, shape = shape, rate = rate) / prior_normalizer
    plot(semitonesToHz(freqs), prior, type = 'l', xlab = 'Frequency, Hz',
         ylab = 'Multiplier of certainty', main = 'Prior belief in pitch values')
  }

  if (summary) {
    vars = colnames(result)[3:ncol(result)]
    vars = vars[vars != 'voiced']  # except dur, time and voiced
    out = as.data.frame(matrix(
      ncol = 2 + 3 * length(vars),
      nrow = 1
    ))
    colnames(out)[c(1:2)] = c('duration', 'voiced')
    for (c in 1:length(vars)) {
      # specify how to summarize pitch etc values for each frame within each file
      # - save mean, median, sd, ... "2+2*c-1": "2" because of dur/voiced above,
      # "+3*c" because for each acoustic variable, we save mean, median and sd
      colnames(out)[2 + 3 * c - 2] = paste0(vars[c], '_', 'mean')
      colnames(out)[2 + 3 * c - 1] = paste0(vars[c], '_', 'median')
      colnames(out)[2 + 3 * c] = paste0(vars[c], '_', 'sd')
    }
    # which columns in the output of pitch_per_sound to save as median + sd
    myseq = (1:length(vars)) + 2
    out$duration = result$duration[1]  # duration, ms
    out$voiced = mean(result$voiced)  # proportion of voiced frames

    for (v in 1:length(myseq)) {
      myvar = colnames(result)[myseq[v]]
      out[1, 3 * v] = mean(result[, myvar], na.rm = TRUE)
      out[1, 3 * v + 1] = median(result[, myvar], na.rm = TRUE)
      out[1, 3 * v + 2] = sd(result[, myvar], na.rm = TRUE)
    }
  } else {
    out = result
  }

  return(out)
}


#' Analyze sound
#'
#' Acoustic analysis of all .wav files in a folder.
#' @param myfolder full path to target folder
#' @param verbose if TRUE, reports progress and estimated time left
#' @inheritParams analyze
#' @inheritParams spectrogram
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
#' s = analyzeFolder(myfolder, verbose = TRUE)  # ~ 15-30 minutes!
#'
#' # Check accuracy: import manually verified pitch values (our "key")
#' key = pitchManual  # a vector of 260 floats
#' trial = s$pitch_median
#' cor(key, trial, use = 'pairwise.complete.obs')
#' plot(log(key), log(trial))
#' abline(a=0, b=1, col='red')
#' }
analyzeFolder = function(myfolder,
                         verbose = TRUE,
                         samplingRate = NULL,
                         silence = 0.04,
                         windowLength = 50,
                         step = NULL,
                         overlap = 50,
                         wn = 'gaussian',
                         zp = 0,
                         cutFreq = 6000,
                         nFormants = 3,
                         pitchMethods = c('autocor', 'spec', 'dom'),
                         entropyThres = 0.6,
                         pitchFloor = 75,
                         pitchCeiling = 3500,
                         priorMean = HzToSemitones(300),
                         priorSD = 6,
                         priorPlot = FALSE,
                         nCands = 1,
                         minVoicedCands = 'autom',
                         domThres = 0.1,
                         domSmooth = 220,
                         autocorThres = 0.7,
                         autocorSmooth = NULL,
                         cepThres = 0.3,
                         cepSmooth = NULL,
                         cepZp = 0,
                         specThres = 0.3,
                         specPeak = 0.35,
                         specSinglePeakCert = 0.4,
                         specHNRslope = 0.8,
                         specSmooth = 150,
                         specMerge = 1,
                         shortestSyl = 20,
                         shortestPause = 60,
                         interpolWin = 3,
                         interpolTol = 0.3,
                         interpolCert = 0.3,
                         pathfinding = c('none', 'fast', 'slow')[2],
                         annealPars = list(maxit = 5000, temp = 1000),
                         certWeight = .5,
                         snakeStep = 0.05,
                         snakePlot = FALSE,
                         smooth = 1,
                         smoothVars = c('pitch', 'dom'),
                         summary = TRUE,
                         plot = FALSE,
                         savePath = NA,
                         specPlot = list(
                           contrast = .2,
                           brightness = 0,
                           ylim = c(0, 5)
                         ),
                         pitchPlot = list(
                           col = rgb(0, 0, 1, .75),
                           lwd = 3
                         ),
                         candPlot = list(
                           levels = c('autocor', 'spec', 'dom', 'cep'),
                           col = c('green', 'red', 'orange', 'violet'),
                           pch = c(16, 2, 3, 7),
                           cex = 2
                         )) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav", full.names = TRUE)
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = apply(as.matrix(filenames), 1, function(x) file.info(x)$size)

  # as.list(match.call()) also works, but we want to get default args as well,
  # since plot should default to TRUE for analyze() and FALSE for analyzeFolder(),
  # and summary vice versa.
  # See https://stackoverflow.com/questions/14397364/match-call-with-default-arguments
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  myPars = myPars[names(myPars) != 'myfolder' &  # exclude these two args
                  names(myPars) != 'verbose']

  result = list()
  for (i in 1:length(filenames)) {
    result[[i]] = do.call(analyze, c(filenames[i], myPars))
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }

  # prepare output
  if (summary == TRUE) {
    output = as.data.frame(t(sapply(result, rbind)))
    output$sound = apply(matrix(1:length(filenames)), 1, function(x) {
      tail(unlist(strsplit(filenames[x], '/')), 1)
    })
    output = output[, c('sound', colnames(output)[1:(ncol(output) - 1)])]
  } else {
    output = result
    names(output) = filenames
  }

  return (output)
}
