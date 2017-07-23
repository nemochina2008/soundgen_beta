### UTILITIES FOR ACOUSTIC ANALYSIS ###

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
                        dom_smoothing = 220,
                        autocor_threshold = 0.75,
                        autocor_smoothing = NULL,
                        cep_threshold = 0.45,
                        cep_smoothing = 3,
                        cep_zp = 2 ^ 13,
                        spec_threshold = 0.45,
                        spec_peak = 0.8,
                        spec_singlePeakCert = 0.6,
                        spec_smoothing = 100,
                        spec_peak_HNRslope = .1,
                        spec_merge = 1,
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
               dom_smoothing = dom_smoothing,
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
                         autocor_threshold = autocor_threshold,
                         autocor_smoothing = autocor_smoothing,
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
                                 cep_zp = cep_zp,
                                 samplingRate = samplingRate,
                                 pitch_floor = pitch_floor,
                                 pitch_ceiling = pitch_ceiling,
                                 cep_threshold = cep_threshold,
                                 cep_smoothing = cep_smoothing,
                                 nCands = nCands)
    if(!is.null(pitchCep_array)) pitch_array = rbind(pitch_array, pitchCep_array)
  }

  # spectral: ratios of harmonics (BaNa)
  if (trackPitch && 'spec' %in% pitch_methods) {
    pitchSpec_array = getPitchSpec(frame = frame,
                                   spec_smoothing = spec_smoothing,
                                   spec_peak_HNRslope = spec_peak_HNRslope,
                                   bin = bin,
                                   HNR = NULL,
                                   spec_threshold = spec_threshold,
                                   spec_peak = spec_peak,
                                   spec_singlePeakCert = spec_singlePeakCert,
                                   pitch_floor = pitch_floor,
                                   pitch_ceiling = pitch_ceiling,
                                   spec_merge = spec_merge,
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
                  dom_smoothing,
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
  # width of smoothing interval (in bins), forced to be an odd number
  dom_smoothing_bins = 2 * ceiling(dom_smoothing / bin / 2) - 1

  # find peaks in the smoothed spectrum
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = dom_smoothing_bins,
                        align = 'center',
                        function(x) {
    isCentral.localMax(x, threshold = dom_threshold)
  })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  pitch_floor_idx = which(as.numeric(names(frame)) > pitch_floor / 1000)[1]
  idx = idx[idx > pitch_floor_idx]

  if (length(idx) > 0) {
    # lowest dominant freq band - we take the first frequency in the spectrum at
    # least /dom_threshold/ % of the amplitude of peak frequency, but high
    # enough to be above pitch_floor
    dom = as.numeric(names(frame)[idx[1]]) * 1000
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
                           autocor_smoothing = NULL,
                           autocor_threshold,
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
  if (!is.numeric(autocor_smoothing)) {
    autocor_smoothing = 2 * ceiling(7 * samplingRate / 44100 / 2) - 1
    # width of smoothing interval, chosen to be proportionate to samplingRate (7
    # for samplingRate 44100), but always an odd number.
    # for(i in seq(16000, 60000, length.out = 10)) {
    #   print(paste(round(i), ':', 2 * ceiling(7 * i / 44100 / 2) - 1))
    # }
  }

  # find peaks in the corrected autocorrelation function
  a_zoo = zoo::as.zoo(a$amp)
  temp = zoo::rollapply(a_zoo,
                        width = autocor_smoothing,
                        align = 'center',
                        function(x) {
    isCentral.localMax(x, threshold = autocor_threshold)
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
                       cep_zp,
                       samplingRate,
                       pitch_floor,
                       pitch_ceiling,
                       cep_threshold,
                       cep_smoothing = NULL,
                       nCands) {
  pitchCep_array = NULL
  if (!is.numeric(cep_smoothing)) {
    cep_smoothing = 2 * ceiling(31 * samplingRate / 44100 / 2) - 1
  }

  if (cep_zp < length(frame)) {
    frameZP = frame
  } else {
    zp = rep(0, (cep_zp - length(frame)) / 2)
    frameZP = c(zp, frame, zp)
    cep_smoothing = cep_smoothing * round(cep_zp / length(frame))
  }

  # fft of fft, whatever you call it - cepstrum or smth else
  cepstrum = abs(fft(frameZP)) # plot(frameZP, type = 'l')
  cepstrum = cepstrum / max(cepstrum) # plot (cepstrum, type = 'l')
  l = length(cepstrum) %/% 2
  b = data.frame(
    # NB: divide by 2 because it's another fft, not inverse fft (cf. pitchAutocor)
    freq = samplingRate / (1:l) / 2 * (length(frameZP) / length(frame)),
    cep = cepstrum[1:l]
  )
  b = b[b$freq > pitch_floor & b$freq < pitch_ceiling, ]
  # plot(b, type = 'l')

  # find peaks
  a_zoo = zoo::as.zoo(b$cep)
  temp = zoo::rollapply(a_zoo,
                        width = cep_smoothing,
                        align = 'center',
                        function(x)
    isCentral.localMax(x, threshold = cep_threshold))
  idx = zoo::index(temp)[zoo::coredata(temp)]

  absCepPeak = try(idx[which.max(b$cep[idx])], silent = TRUE)
  if (class(absCepPeak) != 'try-error') {
    idx = idx[b$freq[idx] > b$freq[absCepPeak] / 1.8]
  } # to avoid false subharmonics
  # plot (b, type = 'l')
  # points(b$freq[idx], b$cep[idx])
  idx = idx[order(b$cep[idx], decreasing = TRUE)]
  acceptedCepPeaks = idx[1:min(length(idx), nCands)]

  if (length(acceptedCepPeaks) > 0) {
    # if some peaks are found...
    pitchCep_array = data.frame(
      'pitchCand' = b$freq[acceptedCepPeaks],
      'pitchAmpl' = b$cep[acceptedCepPeaks],
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
                        spec_smoothing,
                        spec_peak_HNRslope,
                        bin,
                        HNR = NULL,
                        spec_threshold,
                        spec_peak,
                        spec_singlePeakCert,
                        pitch_floor,
                        pitch_ceiling,
                        spec_merge,
                        nCands
                        ) {
  pitchSpec_array = NULL
  width = 2 * ceiling((spec_smoothing / bin + 1) * 20 / bin / 2) - 1 # to be always ~100 Hz,
  # regardless of bin, but an odd number
  if (!is.numeric(HNR)) {
    specPitchThreshold = spec_peak # if HNR is NA, the sound is
    # probably a mess, so we play safe by only looking at very strong harmonics
  } else {
    # for noisy sounds the threshold is high to avoid false sumharmonics etc,
    # for tonal sounds it is low to catch weak harmonics
    specPitchThreshold = spec_peak * (1 - HNR * spec_peak_HNRslope)
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
      pitchAmpl = spec_singlePeakCert
      pitchSpec_array = data.frame(
        'pitchCand' = pitchSpec,
        'pitchAmpl' = spec_singlePeakCert,
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
                     pitchSpec_array$pitchCand[c + 1])) < spec_merge / 12) {
          # merge cands within spec_merge into one "super-candidate"
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
      pitchSpec_array$pitchAmpl = spec_singlePeakCert +
        (1 / (1 + exp(-(pitchSpec_array$specAmplIdx - 1))) - 0.5) * 2 *
        (1 - spec_singlePeakCert) # normalization. Visualization:
      # a = 1:15
      # b = spec_singlePeakCert + (1 / (1 + exp(-(a - 1))) - 0.5) * 2 *
      # (1 - spec_singlePeakCert)
      # plot(a, b, type = 'l')
      pitchSpec_array = pitchSpec_array[
        order(pitchSpec_array$pitchAmpl, decreasing = TRUE),
        c('pitchCand', 'pitchAmpl', 'source')
        ]
    }
  }
  if (!is.null(pitchSpec_array) && sum(!is.na(pitchSpec_array)) > 0) {
    pitchSpec_array = pitchSpec_array[pitchSpec_array$pitchAmpl > spec_threshold,
                                      , drop = FALSE]
    # how many pitchSpec candidates to use (max)
    pitchSpec_array = pitchSpec_array[1:min(nrow(pitchSpec_array), nCands), ]
  }

  return(pitchSpec_array)
}
