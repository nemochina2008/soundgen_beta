### UTILITIES FOR ACOUSTIC ANALYSIS ###

#' Simple peak detection
#'
#' Internal soundgen function.
#'
#' Peak detection with \code{\link[zoo]{rollapply}}. Less versatile but x 10
#' faster than \code{\link[seewave]{fpeaks}}.
#' @param x input vector
#' @param threshold threshold for peak detection
#' @examples
#' soundgen:::isCentral.localMax(c(1,1,3,2,1), 2.5)
isCentral.localMax = function(x, threshold) {
  middle = ceiling(length(x) / 2)
  return(which.max(x) == middle & x[middle] > threshold)
}


#' Find voiced segments
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contours. Merges voiced
#' segments at least \code{shortest_syl} ms long and separated by less than
#' \code{shortest_pause} ms.
#' @param pitchCands matrix of possible pitch values per column. One column is
#'   one fft frame, one row is one pitch candidate
#' @inheritParams analyze
#' @param samplingRate sampling rate (Hz)
#' @param min_voiced_cands a frame is considered to be voiced if at least this
#'   many pitch candidates are not NA. Defaults to 2: since dom is usually
#'   defined, in practice this means that we also want at least one other pitch
#'   candidate (autocor, cep or BaNa)
#' @return Returns a dataframe specifying where each voiced segment starts and
#'   ends (in fft frames, not ms!)
findVoicedSegments = function(pitchCands,
                              shortest_syl = 20,
                              shortest_pause = 50,
                              step,
                              samplingRate,
                              min_voiced_cands = 2) {
  putativelyVoiced = apply(pitchCands, 2, function(x) {
    ifelse(
      sum(!is.na(x)) >= min_voiced_cands,
      1,
      NA
    )})
  # the smallest number of consecutive non-NA pitch values that constitute a
  # voiced segment; but at least 2
  noRequired = max(2, ceiling (shortest_syl / step))
  # the greatest number of NA values that we tolerate before we say a new voiced
  # syllable begins
  noToleratedNA = floor(shortest_pause / step)

  # find and save separately all voiced segments
  segmentStart = numeric()
  segmentEnd = numeric()
  i = 1
  while (i < (length(putativelyVoiced) - noRequired + 1)) {
    # find beginning
    while (i < (length(putativelyVoiced) - noRequired + 1)) {
      if (sum(!is.na(putativelyVoiced[i:(i + noRequired - 1)])) == noRequired) {
        segmentStart = c(segmentStart, i)
        break
      }
      i = i + 1
    }
    # find end
    if (length(segmentEnd) < length(segmentStart)) {
      while (i < (length(putativelyVoiced) - noToleratedNA + 1)) {
        if (sum(putativelyVoiced[i:(i + noToleratedNA)], na.rm = TRUE) == 0) {
          segmentEnd = c(segmentEnd, i - 1)
          i = i - 1
          break
        }
        i = i + 1
      }
      if (length(segmentEnd) < length(segmentStart)) {
        # if the end is not found, take the last non-NA value
        segmentEnd = c (segmentEnd, tail(which(!is.na(putativelyVoiced)), 1))
        break
      }
    }
    i = i + 1
  }
  return (data.frame(segmentStart = segmentStart, segmentEnd = segmentEnd))
}


#' Analyze fft frame
#'
#' Internal soundgen function.
#'
#' This function performs the heavy lifting of pitch tracking and acoustic
#' analysis in general: it takes the spectrum of a single fft frame as input and
#' analyzes it.
#' @param frame the real part of the spectrum of a frame, as returned by
#'   \code{\link[stats]{fft}}
#' @param autoCorrelation pre-calculated autocorrelation of the input frame
#'   (computationally more efficient than to do it here)
#' @param samplingRate sampling rate (Hz)
#' @param trackPitch if TRUE, attempt to find F0 in this frame (FALSE if entropy
#'   is above some threshold - specified in \code{\link{analyze}})
#' @inheritParams analyze
#' @return Returns a list with two components: $pitch_array contains pitch
#'   candidates for the frame, and $summaries contains other acoustic predictors
#'   like HNR, specSlope, etc.
analyzeFrame = function(frame,
                        autoCorrelation = NULL,
                        samplingRate = 44100,
                        trackPitch = TRUE,
                        pitch_methods = c('autocor', 'cep', 'spec', 'dom'),
                        cutoff_freq = 6000,
                        dom_threshold = 0.1,
                        dom_smoothing_width = NULL,
                        voiced_threshold_autocor = 0.75,
                        autocor_smoothing_width = NULL,
                        voiced_threshold_cep = 0.45,
                        zpCep = 2 ^ 13,
                        voiced_threshold_spec = 0.45,
                        specPitchThreshold_nullNA = 0.8,
                        pitchSpec_only_peak_weight = 0.6,
                        width_spec = 100,
                        slope_spec = .1,
                        merge_semitones = 1,
                        pitch_floor = 75,
                        pitch_ceiling = 3500,
                        nCands = 1) {
  ### DESCRIPTIVES
  meanSpec = data.frame ('freq' = 1000 * as.numeric(names(frame)),
                         'amp' = frame)
  amplitude = sum(frame)
  peakFreq = meanSpec$freq [which.max(frame)] # absolute peak
  meanFreq = meanSpec$freq[min(which(cumsum(frame) > amplitude / 2))] # center of mass

  # Cut spectral band from pitch_floor to cutoff_freq Hz
  meanSpec_cut = meanSpec[meanSpec$freq > pitch_floor &
                          meanSpec$freq < cutoff_freq,] # Above 5-6 kHz or so,
  # spectral energy depends too much on the original sampling rate, noises etc.
  # Besides, those frequencies are not super relevant to human vocalizations in
  # any case. So we cut away all info above 5 kHz before we calculate quartiles
  # of spectral energy
  peakFreq_cut = meanSpec_cut$freq[which.max(frame)] # peak frequency under cutoff_freq
  amplitude_cut = sum(meanSpec_cut$amp)
  # first quartile of spectral energy distribution in the band from pitch_floor
  # to cutoff_freq kHz
  quartile25 = meanSpec_cut$freq [min(which(cumsum(meanSpec_cut$amp) >=
                                              0.25 * amplitude_cut))]
  # second quartile (same as mean freq within this spectral band)
  quartile50 = meanSpec_cut$freq [min(which(cumsum(meanSpec_cut$amp) >=
                                              0.5 * amplitude_cut))]
  # third quartile. Note: half the energy in the band from pitch_floor to
  # cutoff_freq kHz lies between quartile25 and quartile75
  quartile75 = meanSpec_cut$freq [min(which(cumsum(meanSpec_cut$amp) >=
                                              0.75 * amplitude_cut))]
  specSlope = summary(lm(amp ~ freq, data = meanSpec_cut))$coef[2, 1]

  ## PITCH TRACKING
  frame = frame / max(frame) # plot (frame, type='l')
  bin = samplingRate / 2 / length(frame) # the width of one bin in spectrogram,
  # in Hz (~20 Hz for 44100 Hz with 50 ms window and zp = 0)

  # lowest dominant frequency band
  if (trackPitch && 'dom' %in% pitch_methods) {
    d = getDom(frame = frame,
               samplingRate = samplingRate,
               bin = bin,
               dom_smoothing_width = dom_smoothing_width,
               dom_threshold = dom_threshold,
               pitch_floor = pitch_floor
    )
    pitch_array = d$dom_array
    dom = d$dom
  } else {
    pitch_array = data.frame(
      'pitchCand' = numeric(),
      'pitchAmpl' = numeric(),
      'source' = character(),
      stringsAsFactors = FALSE,
      row.names = NULL
    )    # initialize an empty dataframe
    dom = NA
  }

  # autocorrelation (PRAAT)
  if (trackPitch && 'autocor' %in% pitch_methods) {
    pa = getPitchAutocor(autoCorrelation = autoCorrelation,
                         voiced_threshold_autocor = voiced_threshold_autocor,
                         autocor_smoothing_width = autocor_smoothing_width,
                         pitch_floor = pitch_floor,
                         pitch_ceiling = pitch_ceiling,
                         samplingRate = samplingRate,
                         nCands = nCands)
    if(!is.null(pa$pitchAutocor_array)) {
      pitch_array = rbind(pitch_array, pa$pitchAutocor_array)
    }
    HNR = pa$HNR
  } else {
    HNR = NA
  }

  # cepstrum
  if (trackPitch && 'cep' %in% pitch_methods) {
    pitchCep_array = getPitchCep(frame = frame,
                                 zpCep = zpCep,
                                 samplingRate = samplingRate,
                                 pitch_floor = pitch_floor,
                                 pitch_ceiling = pitch_ceiling,
                                 voiced_threshold_cep = voiced_threshold_cep)
    if(!is.null(pitchCep_array)) pitch_array = rbind(pitch_array, pitchCep_array)
  }

  # spectral: ratios of harmonics (BaNa)
  if (trackPitch && 'spec' %in% pitch_methods) {
    pitchSpec_array = getPitchSpec(frame = frame,
                                   width_spec = width_spec,
                                   slope_spec = slope_spec,
                                   bin = bin,
                                   HNR = NULL,
                                   voiced_threshold_spec = voiced_threshold_spec,
                                   specPitchThreshold_nullNA = specPitchThreshold_nullNA,
                                   pitchSpec_only_peak_weight = pitchSpec_only_peak_weight,
                                   pitch_floor = pitch_floor,
                                   pitch_ceiling = pitch_ceiling,
                                   merge_semitones = merge_semitones,
                                   nCands = nCands
    )
    if(!is.null(pitchSpec_array)) pitch_array = rbind(pitch_array, pitchSpec_array)
  }

  # some adjustments of pitch candidates
  if (nrow(pitch_array) > 0) {
    pitch_array[, 1:2] = apply(pitch_array[, 1:2], 2, function(x) as.numeric(x))
    # otherwise they become characters after rbind
  }
  if (nrow(pitch_array[pitch_array$source == 'dom', ]) > 0 & !is.na(HNR)) {
    pitch_array$pitchAmpl[pitch_array$source == 'dom'] =
      1 / (1 + exp(3 * HNR - 1)) # dom is worth more for noisy sounds,
    # but its weight approaches ~0.2 as HNR approaches 1
    # (NB: this is before HNR is converted to dB). Visualization:
    # a = seq(0, 1, length.out = 100)
    # b = 1 / (1 + exp(3 * a - 1))
    # plot (a, b, ylim = c(0, 1))
  }

  return (list(
    'pitch_array' = pitch_array,
    'summaries' = data.frame(
      HNR = HNR,
      dom = dom,
      peakFreq = peakFreq,
      peakFreq_cut = peakFreq_cut,
      meanFreq = meanFreq,
      quartile25 = quartile25,
      quartile50 = quartile50,
      quartile75 = quartile75,
      specSlope = specSlope
    )
  ))
}


#' Get lowest dominant frequency band
#'
#' Internal soundgen function.
#'
#' Calculate the lowest frequency band in the spectrum above pitch_floor whose
#' power exceeds a certain threshold.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param bin the width of one bin in spectrogram, Hz
#' @return Returns a list of $dom (NA or numeric) and $dom_array
#'   (either NULL or a dataframe of pitch candidates).
getDom = function(frame,
                  samplingRate,
                  bin,
                  dom_smoothing_width = NULL,
                  dom_threshold,
                  pitch_floor
                  ) {
  dom_array = data.frame(
    'pitchCand' = numeric(),
    'pitchAmpl' = numeric(),
    'source' = character(),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  dom = NA
  if (!is.numeric(dom_smoothing_width)) {
    dom_smoothing_width = 2 * ceiling(12 * 20 / bin / 2) - 1
    # width of smoothing interval, chosen to be always ~220 Hz, regardless of
    # bin, but an uneven number. Chosen by iterative optimization to produce dom
    # values as close as possible to manually verified pitch
  }

  # find peaks in the smoothed spectrum
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = dom_smoothing_width,
                        align = 'center',
                        function(x) {
    isCentral.localMax(x, threshold = dom_threshold)
  })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  pitch_floor_idx = which(as.numeric(names(frame)) > pitch_floor / 1000)[1]

  if (length(idx) > 0) {
    # lowest dominant freq band - we take the first frequency in the spectrum at
    # least /dom_threshold/ % of the amplitude of peak frequency, but high
    # enough to be above pitch_floor
    dom = as.numeric(names(frame)[idx[idx > pitch_floor_idx][1]]) * 1000
    dom_array = data.frame(
      'pitchCand' = dom,
      'pitchAmpl' = frame[idx[1]],
      'source' = 'dom',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  return (list(dom_array = dom_array, dom = dom))
}


#' Autocorrelation pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by looking for peaks in the autocorrelation
#' function (time domain analysis). Modified PRAAT's algorithm. See Boersma, P.
#' (1993). Accurate short-term analysis of the fundamental frequency and the
#' harmonics-to-noise ratio of a sampled sound. In Proceedings of the institute
#' of phonetic sciences (Vol. 17, No. 1193, pp. 97-110).
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @return Returns a list of $HNR (NA or numeric) and $pitchAutocor_array
#'   (either NULL or a dataframe of pitch candidates).
getPitchAutocor = function(autoCorrelation,
                           autocor_smoothing_width = NULL,
                           voiced_threshold_autocor,
                           pitch_floor,
                           pitch_ceiling,
                           samplingRate,
                           nCands) {
  # autoCorrelation = autocorBank[, 13]
  pitchAutocor_array = NULL
  a = data.frame ('freq' = as.numeric(names(autoCorrelation)),
                  'amp' = autoCorrelation)
  rownames(a) = NULL
  a = a[a$freq > pitch_floor &
          a$freq < pitch_ceiling, , drop = FALSE] # plot(a[,2], type='l')
  HNR = max(a$amp) # HNR is here defined as the maximum autocorrelation
  # within the specified pitch range. It is also measured for the frames which
  # are later classified as unvoiced (i.e. HNR can be <voiced_threshold)
  if (!is.numeric(autocor_smoothing_width)) {
    autocor_smoothing_width = 2 * ceiling(7 * samplingRate / 44100 / 2) - 1
    # width of smoothing interval, chosen to be proportionate to samplingRate (7
    # for samplingRate 44100), but always an uneven number. Chosen by iterative
    # optimization to produce dom values as close as possible to manually
    # verified pitch
    # for(i in seq(16000, 60000, length.out = 10)) {
    #   print(paste(round(i), ':', 2 * ceiling(7 * i / 44100 / 2) - 1))
    # }
  }

  # find peaks in the corrected autocorrelation function
  a_zoo = zoo::as.zoo(a$amp)
  temp = zoo::rollapply(a_zoo,
                        width = autocor_smoothing_width,
                        align = 'center',
                        function(x) {
    isCentral.localMax(x, threshold = voiced_threshold_autocor)
    # width = 7 chosen by optimization, but it doesn't make that much difference anyhow
  })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  autocorPeaks = a[idx, ]

  if (nrow(autocorPeaks) > 0) {
    # if some peaks are found...
    # we are only interested in frequencies above half of the best candidate
    # (b/c otherwise we get false subharmonics)
    bestFreq = autocorPeaks$freq[which(autocorPeaks$amp >
                                         0.975 * max(autocorPeaks$amp))[1]]
    # bestFreq = autocorPeaks$freq[which.max(autocorPeaks$amp)]
    if (!is.na(bestFreq)) {
      autocorPeaks = try(autocorPeaks[autocorPeaks$freq > bestFreq / 1.8,
                                      , drop = FALSE], silent = TRUE)
      # otherwise we get false subharmonics
      autocorPeaks = try(autocorPeaks[order(autocorPeaks$amp, decreasing = TRUE),
                                      , drop = FALSE], silent = TRUE)
    }
    if (class(autocorPeaks) != 'try-error') {
      if (nrow(autocorPeaks) > 0) {
        # if some peaks satisfy all criteria, return them:
        pitchAutocor_array = data.frame (
          'pitchCand' = autocorPeaks [1:min(nrow(autocorPeaks), nCands), 1],
          # save n candidates of pitchAutocor, convert to Hz
          'pitchAmpl' = autocorPeaks[1:min(nrow(autocorPeaks), nCands), 2],
          # save amplitudes corresponding to each pitchAutocor candidate
          'source' = 'autocor',
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    }
  }
  # very occasionally HNR can be calculated as 1.01 etc. To prevent this nonsense:
  if (!is.na(HNR) & HNR >= 1) {
    HNR = 0.9999
  } # analogous to 40 dB

  return(list(pitchAutocor_array = pitchAutocor_array,
              HNR = HNR))
}


#' Cepstral pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by looking for peaks in the cepstrum.
#' See http://www.phon.ucl.ac.uk/courses/spsci/matlab/lect10.html
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @return Returns either NULL or a dataframe of pitch candidates.
getPitchCep = function(frame,
                       zpCep,
                       samplingRate,
                       pitch_floor,
                       pitch_ceiling,
                       voiced_threshold_cep) {
  # See
  pitchCep_array = NULL

  if (zpCep < length(frame)) {
    frameZP = frame
  } else {
    frameZP = c(frame, rep(0, (zpCep - length(frame))))
  }

  # fft of fft, whatever you call it - cepstrum or smth else
  cepstrum = abs(fft(frameZP)) # plot(frameZP, type='l')
  cepstrum = cepstrum / max(cepstrum) # plot (cepstrum, type='l')
  b = cbind (1:(length(cepstrum) %/% 2),
             cepstrum[1:(length(cepstrum) %/% 2)]) # plot (b, type='l')
  b[, 1] = samplingRate / (1:nrow(b)) / 2 * (length(frameZP) / length(frame))
  # NB: divide by 2 because it's another fft, not inverse fft (cf. pitchAutocor)
  highestRelevantPeak = which(b[, 1] < pitch_ceiling)[1] # if we are interested
  # in high frequencies, we need to be able to detect closely spaced peaks in
  # the cepstrum, thus we choose a relatively narrow width of the window for
  # finding peaks. And vice versa. If we took a width double the
  # highestRelevantPeak, we would fail to find the very first peak (cepstrum
  # starts with 1, like autocor), so we just take width = highestRelevantPeak
  b = b[b[, 1] > pitch_floor & b[, 1] < pitch_ceiling, ]

  # find peaks
  a_zoo = zoo::as.zoo(b[, 2])
  temp = zoo::rollapply(a_zoo,
                        width = highestRelevantPeak,
                        align = 'center',
                        function(x)
    isCentral.localMax(x, threshold = voiced_threshold_cep))
  idx = zoo::index(temp)[zoo::coredata(temp)]

  absCepPeak = try (idx[which.max(b[idx, 2])], silent = TRUE)
  if (class(absCepPeak) != 'try-error') {
    idx = idx[b[idx, 1] > b[absCepPeak, 1] / 1.8]
  } # to avoid false subharmonics
  # plot (b, type = 'l')
  # points(b[idx, 1], b[idx, 2])
  acceptedCepPeak = idx[which.max(b[idx, 2])] # the highest cepstrum peak within pitch range
  cepstrumPeaks = data.frame ('freq' = b[acceptedCepPeak, 1],
                              'amp' = b[acceptedCepPeak, 2])

  if (nrow(cepstrumPeaks) > 0) {
    # if some peaks are found...
    pitchCep_array = data.frame(
      'pitchCand' = cepstrumPeaks$freq[1],
      'pitchAmpl' = cepstrumPeaks$amp[1],
      'source' = 'cep',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    # because cepstrum really stinks for frequencies above ~1 kHz, mostly
    # picking up formants or just plain noise, we discount confidence in
    # high-pitch cepstral estimates
    pitchCep_array$pitchAmpl = pitchCep_array$pitchAmpl /
      (1 + 5 / (1 + exp(2 - .25 * HzToSemitones(pitchCep_array$pitchCand / pitch_floor))))
    # visualization: a = seq(pitch_floor, pitch_ceiling, length.out = 100)
    # b = 1 + 5 / (1 + exp(2 - .25 * HzToSemitones(a / pitch_floor)))
    # plot (a, b, type = 'l')
  }
  return (pitchCep_array)
}


#' BaNa pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by calculating ratios of putative harmonics
#' (frequency domain analysis, ~ modified BaNa algorithm). See Ba et al. (2012)
#' "BaNa: A hybrid approach for noise resilient pitch detection." Statistical
#' Signal Processing Workshop (SSP), 2012 IEEE.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param bin the width of spectral bin in \code{frame}, Hz
#' @param HNR harmonics-to-noise ratio returned by \code{\link{getPitchAutocor}}
#' @return Returns either NULL or a dataframe of pitch candidates.
getPitchSpec = function(frame,
                        width_spec,
                        slope_spec,
                        bin,
                        HNR = NULL,
                        voiced_threshold_spec,
                        specPitchThreshold_nullNA,
                        pitchSpec_only_peak_weight,
                        pitch_floor,
                        pitch_ceiling,
                        merge_semitones,
                        nCands
                        ) {
  pitchSpec_array = NULL
  width = 2 * ceiling((width_spec / bin + 1) * 20 / bin / 2) - 1 # to be always ~100 Hz,
  # regardless of bin, but an odd number
  if (!is.numeric(HNR)) {
    specPitchThreshold = specPitchThreshold_nullNA # if HNR is NA, the sound is
    # probably a mess, so we play safe by only looking at very strong harmonics
  } else {
    # for noisy sounds the threshold is high to avoid false sumharmonics etc,
    # for tonal sounds it is low to catch weak harmonics
    specPitchThreshold = specPitchThreshold_nullNA * (1 - HNR * slope_spec)
  }

  # find peaks in the spectrum (hopefully harmonics)
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = width,
                        align = 'center',
                        function(x) {
    isCentral.localMax(x, threshold = specPitchThreshold)
    # plot(zoo::as.zoo(frame), type='l')
  })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  specPeaks = data.frame ('freq' = as.numeric(names(frame)[idx]) * 1000,
                          'amp' = frame[idx])

  if (nrow(specPeaks) == 1) {
    if (specPeaks[1, 1] < pitch_ceiling & specPeaks[1, 1] > pitch_floor) {
      pitchSpec = specPeaks[1, 1]
      pitchAmpl = pitchSpec_only_peak_weight
      pitchSpec_array = data.frame(
        'pitchCand' = pitchSpec,
        'pitchAmpl' = pitchSpec_only_peak_weight,
        'source' = 'spec',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  } else if (nrow(specPeaks) > 1) {
    # analyze five lowest harmonics
    specPeaks = specPeaks [1:min(5, nrow(specPeaks)), ]

    # A modified version of BaNa algorithm follows
    seq1 = 2:nrow(specPeaks)
    seq2 = 1:(nrow(specPeaks) - 1)
    n = length(seq1) * length(seq2)
    temp = data.frame (
      'harmonicA' = rep(0, n),
      'harmonicB' = rep(0, n),
      'AtoB_ratio' = rep(0, n)
    )
    counter = 1
    for (i in seq1) {
      for (j in seq2) {
        temp$harmonicA[counter] = i
        temp$harmonicB[counter] = j
        # get ratios of discovered harmonics to each other
        temp$AtoB_ratio[counter] = specPeaks$freq[i] / specPeaks$freq[j]
        counter = counter + 1
      }
    }
    temp = temp[temp$harmonicA > temp$harmonicB, ]

    pitchCand = numeric()
    for (i in 1:nrow(temp)) {
      # for each ratio that falls within the limits specified outside this
      # function in a dataframe called "ratios", calculate the corresponding
      # pitch. If several ratios suggest the same pitch, that's our best guess
      divLow = BaNa_ratios$divide_lower_by[temp$AtoB_ratio[i] > BaNa_ratios$value_low &
                                             temp$AtoB_ratio[i] < BaNa_ratios$value_high]
      pitchCand = c(pitchCand,
                    as.numeric(specPeaks$freq[temp$harmonicB[i]] / divLow))
    }
    # add pitchCand based on the most common distances between harmonics
    # pitchCand = c(pitchCand, diff(specPeaks[,1]))

    pitchCand = sort (pitchCand [pitchCand > pitch_floor &
                                   pitchCand < pitch_ceiling])
    if (length(pitchCand) > 0) {
      pitchSpec_array = data.frame(
        'pitchCand' = pitchCand,
        'specAmplIdx' = 1,
        'source' = 'spec',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      c = 1
      while (c + 1 <= nrow(pitchSpec_array)) {
        if (abs(log2(pitchSpec_array$pitchCand[c] /
                     pitchSpec_array$pitchCand[c + 1])) < merge_semitones / 12) {
          # merge cands within merge_semitones into one "super-candidate"
          # and give this new super-candidate a certainty boost
          pitchSpec_array$specAmplIdx[c] = pitchSpec_array$specAmplIdx[c] + 1
          pitchSpec_array$pitchCand[c] = mean(c(
            pitchSpec_array$pitchCand[c],
            pitchSpec_array$pitchCand[c + 1]
          ))
          pitchSpec_array = pitchSpec_array[-(c + 1), ]
        } else {
          c = c + 1
        }
      }
      pitchSpec_array$pitchAmpl = pitchSpec_only_peak_weight +
        (1 / (1 + exp(-(pitchSpec_array$specAmplIdx - 1))) - 0.5) * 2 *
        (1 - pitchSpec_only_peak_weight) # normalization. Visualization:
      # a = 1:15
      # b = pitchSpec_only_peak_weight + (1 / (1 + exp(-(a - 1))) - 0.5) * 2 *
      # (1 - pitchSpec_only_peak_weight)
      # plot(a, b, type = 'l')
      pitchSpec_array = pitchSpec_array[
        order(pitchSpec_array$pitchAmpl,  decreasing = TRUE),
        c('pitchCand', 'pitchAmpl', 'source')
        ]
    }
  }
  if (!is.null(pitchSpec_array) && sum(!is.na(pitchSpec_array)) > 0) {
    pitchSpec_array = pitchSpec_array[pitchSpec_array$pitchAmpl > voiced_threshold_spec,
                                      , drop = FALSE]
    # how many pitchSpec candidates to use (max)
    pitchSpec_array = pitchSpec_array[1:min(nrow(pitchSpec_array), nCands), ]
  }

  return(pitchSpec_array)
}
