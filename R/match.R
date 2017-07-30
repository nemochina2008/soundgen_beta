## (temp): FIND SOUNDGEN SETTINGS TO REPRODUCE AN EXISTING SOUND ##

# TODO: test match with different settings, write vignette section on matching


#' Reproduce sound (experimental)
#'
#' Attempts to find \code{\link{soundgen}} settings that would reproduce an
#' existing sound by mutating control parameters and measuring fit to target.
#' The optimization algorithm is simple hill climbing. Disclaimer: this function
#' is experimental and may or may not work for particular tasks. It is intended
#' as a supplement to - not replacement of - manual optimization (see the
#' vignette on sound production for examples).
#' @return Returns a list of length 2: \code{$history} contains the tried
#'   parameter values together their fit to target \code{$sim}, and \code{$pars}
#'   contains a list of the final - hopefully best - parameter settings.
#' @param target the sound we want to reproduce using soundgen: path to a .wav
#'   file or numeric vector
#' @inheritParams spectrogram
#' @param pars arguments to \code{\link{soundgen}} that we are attempting to
#'   optimize
#' @param init values of other arguments to soundgen that are fixed at
#'   non-default values
#' @param simMeasure method of comparing mel-transformed spectra of two sounds:
#'   "cor" = average Pearson's correlation of mel-transformed spectra of
#'   individual FFT frames; "cosine" = same as "cor" but with cosine similarity
#'   instead of Pearson's correlation; "pixel" = absolute difference between
#'   each point in the two spectra; "dtw" = discrete time warp with
#'   \code{\link[dtw]{dtw}}
#' @param probMutation the probability of a parameter mutating per iteration
#' @param stepVariance scale factor for calculating the size of mutations
#' @param maxIter maximum number of mutated sounds produced without improving
#'   the fit to target
#' @param minExpectedDelta minimum improvement in fit to target required to
#'   accept the new sound candidate
#' @param verbose if TRUE, plays back the accepted candidate at each iteration
#'   and reports the outcome
#' @param padWith compared spectra are padded to have the same number of
#'   columns. Padding with zeroes ... ? and with NA ...?
#' @param penalizeLengthDif if TRUE, sounds of different length are considered
#'   to be less similar; if FALSE, only the overlapping parts of two sounds are
#'   compared
#' @param throwaway parts of the spectrum quieter than \code{throwaway} dB are
#'   not compared
#' @examples
#'
reproduceSound = function(target,
                          samplingRate = NULL,
                          pars,
                          init = NULL,
                          simMeasure = c('cor', 'cosine', 'pixel', 'dtw'),
                          probMutation = .25,
                          stepVariance = 0.1,
                          maxIter = 50,
                          minExpectedDelta = 0.001,
                          windowLength = 40,
                          overlap = 50,
                          step = NULL,
                          verbose = TRUE,
                          padWith = NA,
                          penalizeLengthDif = TRUE,
                          throwaway = -120) {

  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  targetSpec = getMelSpec(target,
                          samplingRate = samplingRate,
                          windowLength = windowLength,
                          overlap = overlap,
                          step = step,
                          throwaway = throwaway,
                          plot = TRUE)

  ## initialize
  # start with default par values
  parDefault = defaults[pars]

  # analyse the target and update the default pars
  as = segment(target, samplingRate = samplingRate, plot = FALSE)
  aa = analyze(target, samplingRate = samplingRate, plot = FALSE)

  parDefault$sylLen = mean(as$syllables$dur)
  p = as.numeric(na.omit(aa$pitch))
  p = downsample(p, srNew = 5, srOld = 1 / step * 1000)  # downsample F0 measures to 5 Hz
  parDefault$pitchAnchors = data.frame(time = seq(0, 1, length.out = length(p)),
                                       value = p)

  # replace defaults with user-provided values, if any
  if (!is.null(init)) {
    for (i in 1:length(init)) {
      if (!names(init)[i] %in% names(defaults)) {
        stop(paste('init parameter not recognized:', init[i]))
      }
      parDefault[[names(init)[i]]] = init[[i]]
    }
  }

  # calculate simMeasure of initial par values
  output = list(list(pars = parDefault, sim = NA))
  parLoop = parDefault
  cand = try(do.call(soundgen, parLoop), silent = FALSE)
  if (class(cand) == 'try-error') {
    stop ('Invalid initial pars')
  }
  output[[1]]$sim = compareSounds(
    target = NULL,
    targetSpec = targetSpec,
    cand = cand,
    samplingRate = samplingRate,
    simMeasure = simMeasure,
    windowLength = windowLength,
    overlap = overlap,
    step = step,
    padWith = padWith,
    penalizeLengthDif = penalizeLengthDif,
    throwaway = throwaway,
    summary = TRUE
  )

  # iteratively mutate pars and save par values that improve distance
  i = 1
  while (i < maxIter) {
    parMut = parLoop
    # choose pars to mutate
    idx_mut = rbinom(n = length(pars),
                     size = 1,
                     prob = probMutation)
    idx_mut_bin = which(idx_mut == 1)
    parsToMutate = pars[idx_mut_bin]
    if (length(parsToMutate) == 0) {
      # need to mutate at least one par
      parsToMutate = sample(pars, size = 1)
    }

    # prepare a list of mutated par values to feed to the sound generator
    for (p in parsToMutate) {
      if (is.numeric(parMut[[p]])) {  # continuous pars
        l = permittedValues[p, 'low']
        h = permittedValues[p, 'high']
        parMut[[p]] = rnorm_bounded(
          n = 1,
          mean = as.numeric(parLoop[p]),
          low = l,
          high = h,
          sd = (h - l) * stepVariance,
          roundToInteger = (p %in% pars_to_round)
        )
      } else if (is.list(parMut[[p]])) {  # anchors
        parMut[[p]] = wiggleAnchors(df = parLoop[[p]],
                                    temperature = stepVariance,
                                    temp_coef = 0.1,
                                    low = c(0, 0),
                                    high = c(1, 3500),
                                    wiggleAllRows = FALSE)
      }
    }

    # generate a new sound based on the mutated pars and compare it to the reference spectrogram
    cand = try(do.call(soundgen, parMut), silent = FALSE)
    # playme(candVector, samplingRate)
    if (class(candVector) == 'try-error') {
      distance_new = -Inf
      delta = -Inf
    } else {
      sim_new = compareSounds(
        target = NULL,
        targetSpec = targetSpec,
        cand = cand,
        samplingRate = samplingRate,
        simMeasure = simMeasure,
        windowLength = windowLength,
        overlap = overlap,
        step = step,
        padWith = padWith,
        penalizeLengthDif = penalizeLengthDif,
        throwaway = throwaway,
        summary = TRUE
      )
      delta = sim_new - output[[length(output)]]$sim  # want to maximize similarity
    }

    condition = try(delta > minExpectedDelta)
    if (class(condition) == 'try-error' | is.na(condition)) {
      print(parLoop) # debugging
      break
    } else if (condition) {
      i = 1  # reset the count of iterations
      output = c(output, list(list(pars = parMut,
                                   sim = sim_new)))
      parLoop = parMut
      if (verbose) {
        print (paste('Best similarity: ', round(output[[length(output)]]$sim, 4)))
        playme(candVector, samplingRate)
      }
    } else {
      parMut = parLoop # back to previous step
    }
    i = i + 1
  }

  if (verbose) {
    if (length(output) == 1) {
      print ('Failed to improve! Try increasing maxIter.')
    } else {
      print ('Improved')
    }
  }

  return (list(history = output, pars = output[[length(output)]]$pars))
}


#' Compare sounds (experimental)
#'
#' Computes similarity between two sounds based on correlating mel-transformed
#' spectra (auditory spectra).
#' @inheritParams match
#' @param targetSpec if already calculated, the target auditory spectrum can be
#'   provided
#' @param summary if TRUE, returns the mean of similarity values calculated by
#'   all methods in \code{simMeasure}
#' @examples
#' target = soundgen(sylLen = 500,
#'                   pitchAnchors = data.frame(time = c(0, 0.1, 0.9, 1),
#'                                             value = c(100, 150, 135, 100)),
#'                   temperature = 0)
#' targetSpec = soundToAudiogram(target, samplingRate = 16000)

#' parsToTry = list(
#'   list(sylLen = 300,
#'        pitchAnchors = data.frame(time = c(0, 0.3, 0.7, 1),
#'                                  value = c(100, 110, 105, 100))),
#'   list(sylLen = 300,
#'        pitchAnchors = data.frame(time = c(0, 0.1, 0.9, 1),
#'                                  value = c(100, 150, 135, 100))),
#'   list(sylLen = 500,
#'        pitchAnchors = data.frame(time = c(0, 0.3, 0.7, 1),
#'                                  value = c(100, 110, 105, 100))),
#'   list(sylLen = 500,
#'        pitchAnchors = data.frame(time = c(0, 0.1, 0.9, 1),
#'                                  value = c(100, 150, 135, 100)))
#' )
#'
#' sounds = list()
#' for (s in 1:length(parsToTry)) {
#'   sounds[[length(sounds) + 1]] = do.call(soundgen, c(parsToTry[[s]],
#'                                                    list(temperature = 0)))
#' }
#'
#' fitness = c('cor', 'cosine', 'pixel', 'dtw')
#' df = matrix(NA, nrow = length(parsToTry), ncol = length(fitness))
#' colnames(df) = fitness
#' df = as.data.frame(df)
#' for (i in 1:nrow(df)) {
#'   df[i, ] = compareSounds(target = NULL,
#'                           targetSpec = targetSpec,
#'                           cand = sounds[[i]],
#'                           samplingRate = 16000,
#'                           padWith = NA,
#'                           penalizeLengthDif = TRUE,
#'                           fitness = fitness,
#'                           summary = FALSE)
#' }
#' df$av = rowMeans(df, na.rm = TRUE)
#' df
compareSounds = function(target,
                         targetSpec = NULL,
                         cand,
                         samplingRate = NULL,
                         simMeasure = c('cor', 'cosine', 'pixel', 'dtw')[1],
                         windowLength = 40,
                         overlap = 50,
                         step = NULL,
                         padWith = NA,
                         penalizeLengthDif = TRUE,
                         throwaway = -120,
                         summary = TRUE) {
  # extract spectrums
  if (is.null(targetSpec)) {
    targetSpec = getMelSpec(target,
                            samplingRate = samplingRate,
                            windowLength = windowLength,
                            overlap = overlap,
                            step = step,
                            throwaway = throwaway)
  }
  candSpec = getMelSpec(cand,
                        samplingRate = samplingRate,
                        windowLength = windowLength,
                        overlap = overlap,
                        step = step,
                        throwaway = throwaway)

  # make sure the number of columns (frames) is equal for comparing the two
  # spectrograms by padding with zeroes (silence)
  if (ncol(targetSpec) < ncol(candSpec)) {
    targetSpec = matchColumns(matrix_short = targetSpec,
                              len = ncol(candSpec),
                              padWith = padWith)
  } else if (ncol(targetSpec) > ncol(candSpec)) {
    candSpec = matchColumns(matrix_short = candSpec,
                            len = ncol(targetSpec),
                            padWith = padWith)
  }

  # correlate column by column
  sim_by_column = matrix(NA, nrow = ncol(targetSpec), ncol = length(simMeasure))
  colnames(sim_by_column) = simMeasure
  sim_by_column = as.data.frame(sim_by_column)
  for (c in 1:ncol(targetSpec)) {
    if ('cor' %in% simMeasure) {
      sim_by_column$cor[c] = suppressWarnings(
        cor(targetSpec[,c], candSpec[,c], use = 'na.or.complete')
      )
    }
    if ('cosine' %in% simMeasure) {
      sim_by_column$cosine[c] = crossprod(targetSpec[,c], candSpec[,c]) /
        sqrt(crossprod(targetSpec[,c]) * crossprod(candSpec[,c]))
    }
    if ('pixel' %in% simMeasure) {
      sim_by_column$pixel[c] = 1 - mean(abs(targetSpec[,c] - candSpec[,c]))
    }
    if ('dtw' %in% simMeasure) {
      d = try(dtw::dtw(targetSpec[,c], candSpec[,c],
                       distance.only = TRUE)$normalizedDistance, silent = TRUE)
      if (class(d) == 'try-error') d = NA
      sim_by_column$dtw[c] = 1 - d
    }
  }

  if (penalizeLengthDif) {
    out = apply(sim_by_column, 2, function(x) {
      # if two sounds are of different length, similarity is reduced
      sum(x, na.rm = TRUE) / length(x)
    })
  } else {
    # length difference has no effect on similarity (e.g., it just tells us
    # how well the shorter sound fits the middle part of the longer sound)
    out = colMeans(sim_by_column, na.rm = TRUE)
  }
  if (summary) {
    out = mean(out[simMeasure], na.rm = TRUE)
  }
  return(out)
}


#' Mel-transformed spectrum
#'
#' Internal soundgen function
#'
#' Takes a .wav file or a waveform as numeric vector + samplingRate and returns
#' mel-transformed spectrum (auditory spectrum). See \code{\link{match}}.
#' @inheritParams match
#' @param plot if TRUE, plots the spectrum
getMelSpec = function(s,
                      samplingRate = NULL,
                      windowLength = 40,
                      overlap = 50,
                      step = NULL,
                      throwaway = -120,
                      plot = FALSE) {
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  throwaway01 = 2 ^ (throwaway / 10)

  if (is.character(s)) {
    sWave = readWave(s)
  } else if (is.numeric(s)) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    }
    sWave = tuneR::Wave(s, samp.rate = samplingRate, bit = 16)
  }

  spec = t(tuneR::melfcc(
    sWave,
    wintime = windowLength / 1000,
    hoptime = step / 1000,
    maxfreq = samplingRate / 2,
    nbands = 100 * windowLength / 20,
    spec_out = T
  )$aspectrum)
  spec = spec[, colMeans(spec, na.rm = TRUE) > throwaway01]  # strip empty frames
  spec = log01(spec)  # log-transform and normalize

  if (plot) {
    # show the spectrum of the target
    seewave::filled.contour.modif2(
      x = seq(1, ncol(spec) * step,
              length.out = ncol(spec)),
      y = 1:nrow(spec),
      z = t(spec),
      levels = seq(0, 1, length = 30),
      color.palette = function(x) gray(seq(1, 0, length.out = x))
    )
  }
  return(spec)
}
