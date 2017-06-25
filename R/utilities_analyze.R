### UTILITIES FOR ACOUSTIC ANALYSIS ###

#' Simple peak detection
#'
#' Internal soundgen function.
#'
#' Peak detection with \code{\link[zoo]{rollapply}}. Less versatile but x 10
#' faster than \code{\link[seewave]{fpeaks}}.
#' @param x input vector
#' @param threshold threshold for peak detection
#' @example isCentral.localMax(c(1,1,3,2,1), 2.5)
isCentral.localMax = function(x, threshold) {
  middle = ceiling(length(x) / 2)
  return(which.max(x) == middle & x[middle] > threshold)
}


findVoicedSegments = function(pitchCands,
                              shortestSyllable = 20,
                              shortestBreak = 50,
                              step,
                              samplingRate,
                              pitchCandsPerFrameToQualifyAsPutativelyVoiced = 2) {
  # internal helper function for postprocessing. Merges voiced segments at least /shortestSyllable/ ms long and separated by less than /shortestBreak/ ms

  putativelyVoiced = apply (pitchCands, 2, function(x)
    ifelse(
      sum(!is.na(x)) >= pitchCandsPerFrameToQualifyAsPutativelyVoiced,
      1,
      NA
    )) #
  noRequired = max (2, ceiling (shortestSyllable / step)) # the smallest number of consecutive non-NA pitch values that constitute a voiced segment; but at least 2
  noToleratedNA = floor (shortestBreak / step) # the greatest number of NA values that we tolerate before we say a new voiced syllable begins

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
        if (sum(putativelyVoiced[i:(i + noToleratedNA)], na.rm = T) == 0) {
          segmentEnd = c(segmentEnd, i - 1)
          i = i - 1
          break
        }
        i = i + 1
      }
      if (length(segmentEnd) < length(segmentStart)) {
        # if the end is not found, take the last non-NA value
        segmentEnd = c (segmentEnd, max(which(!is.na(
          putativelyVoiced
        ))))
        break
      }
    }
    i = i + 1
  }
  return (data.frame(segmentStart = segmentStart, segmentEnd = segmentEnd))
}


analyzeFrame = function (frame,
                         autoCorrelation = NULL,
                         samplingRate = 44100,
                         windowLength_points = 2048,
                         wn = 'gaussian',
                         zp = 0,
                         zpCep = 2 ^ 13,
                         cutoff_dom = 6000,
                         voiced_threshold_autocor = 0.75,
                         voiced_threshold_cep = 0.45,
                         voiced_threshold_spec = 0.45,
                         specPitchThreshold_nullNA = 0.8,
                         slopeSpec = 0.75,
                         widthSpec = 100,
                         pitch_floor = 75,
                         pitch_ceiling = 3500,
                         silence = 0.03,
                         dom_threshold = 0.1,
                         pitchSpec_only_peak_weight = 0.6,
                         autoCorrelation_filter = NULL,
                         entropyThreshold = 0.9,
                         maxNoCands = 1) {
  ## internal helper function. Analyzes a frame and returns several measures of pitch plus other acoustic features.
  # frame: a simple vector of normalized amplitudes: readWave(soundfile)@left / max(readWave(soundfile))
  # autoCorrelation: pre-calculated autocorrelation of a frame supplied by function analyzeSound ()

  # just some initializing
  HNR = NA
  pitch_array = data.frame (
    'pitchCand' = NA,
    'pitchAmpl' = NA,
    'source' = NA,
    stringsAsFactors = F,
    row.names = NULL
  )
  pitchAutocor_array = pitchCep_array = pitchSpec_array = dom_array = data.frame (
    'pitchCand' = numeric(),
    'pitchAmpl' = numeric(),
    'source' = character(),
    stringsAsFactors = F,
    row.names = NULL
  )
  summaries = data.frame (
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

  ### DESCRIPTIVES
  meanSpec = data.frame ('freq' = 1000 * as.numeric(names(frame)), 'amp' =
                           frame)
  amplitude = sum(frame)
  peakFreq = meanSpec$freq [which.max(frame)] # absolute peak - peak frequency
  meanFreq = meanSpec$freq[min(which(cumsum(frame) > amplitude / 2))] # spectral center of mass

  # Cut spectral band from pitch_floor to cutoff_dom Hz
  meanSpec_cut = meanSpec [meanSpec$freq > pitch_floor &
                             meanSpec$freq < cutoff_dom,] # Above 5-6 kHz or so, spectral energy depends too much on the original sampling rate, noises etc. Besides, those frequencies are not super relevant to human vocalizations in any case. So we cut away all info above 5 kHz before we calculate quartiles of spectral energy
  peakFreq_cut = meanSpec_cut$freq[which.max(frame)] # peak frequency under 5 kHz
  amplitude_cut = sum(meanSpec_cut$amp)
  quartile25 = meanSpec_cut$freq [min(which(cumsum(meanSpec_cut$amp) >= 0.25 * amplitude_cut))] # first quartile of spectral energy distribution in the band from pitch_floor to cutoff_dom kHz
  quartile50 = meanSpec_cut$freq [min(which(cumsum(meanSpec_cut$amp) >= 0.5 * amplitude_cut))] # second quartile (same as mean freq within this spectral band)
  quartile75 = meanSpec_cut$freq [min(which(cumsum(meanSpec_cut$amp) >= 0.75 * amplitude_cut))] # third quertile. Note: half the energy in the band from pitch_floor to cutoff_dom kHz lies between quartile25 and quartile75
  specSlope = summary(lm(amp ~ freq, data = meanSpec_cut))$coef[2, 1]

  # get lowest dominant frequency bands
  dom = NA
  frame = frame / max(frame) # plot (frame, type='l')
  frame_zoo = as.zoo(frame)
  bin = samplingRate / 2 / length(frame) # the width of one bin in spectrogram, in Hz (~20 Hz for 44100 Hz with 50 ms window and zp=0)
  width = 2 * ceiling(12 * 20 / bin / 2) - 1 # width of smoothing interval, chosen to be always ~220 Hz, regardless of bin, but an uneven number. Chosen by iterative optimization to produce dom values as close as possible to manually verified pitch (see file domTraining.R)
  temp = rollapply(frame_zoo, width = width, align = 'center', function(x)
    isCentral.localMax(x, threshold = dom_threshold))
  idx = index(temp)[coredata(temp)]
  pitch_floor_idx = which (as.numeric(names(frame)) > pitch_floor / 1000)[1]

  if (length(idx) > 0) {
    dom = as.numeric(names(frame)[idx[idx > pitch_floor_idx][1]]) * 1000 # lowest dominant freq band - we take the first frequency in the spectrum at least /dom_threshold/ % of the amplitude of peak frequency, but high enough to be above pitch_floor
    dom_array = data.frame (
      'pitchCand' = dom,
      'pitchAmpl' = frame[idx[1]],
      'source' = 'dom',
      stringsAsFactors = F,
      row.names = NULL
    )
  }

  ### PITCH TRACKING

  ## PITCH_AUTOCORRELATION. Try to detect pitch with autocorrelation function (time domain analysis). Modified PRAAT's algorithm (P. Boersma)
  # autoCorrelation = autocorBank[,21]
  a = data.frame ('freq' = as.numeric(names(autoCorrelation)), 'amp' = autoCorrelation)
  rownames(a) = NULL
  a = a[a$freq > pitch_floor &
          a$freq < pitch_ceiling, , drop = F] # plot(a[,2], type='l')
  HNR = max(a$amp) # HNR is here defined as the maximum autocorrelation within the specified pitch range. It is also measured for the frames which are later classified as unvoiced (i.e. HNR can be <voiced_threshold)

  # find peaks in the corrected autocorrelation function
  a_zoo = as.zoo(a$amp)
  temp = rollapply(a_zoo, width = 7, align = 'center', function(x)
    isCentral.localMax(x, threshold = voiced_threshold_autocor)) # width=7 chosen by optimization, but it doesn't make that much difference anyhow
  idx = index(temp)[coredata(temp)]
  autocorPeaks = a[idx, ]

  if (nrow(autocorPeaks) > 0) {
    # if some peaks are found...
    # we are only interested in frequencies above half of the best candidate (b/c otherwise we get false subharmonics)
    bestFreq = autocorPeaks$freq[which(autocorPeaks$amp > 0.975 * max(autocorPeaks$amp))[1]]
    # bestFreq = autocorPeaks$freq[which.max(autocorPeaks$amp)]
    if (!is.na(bestFreq)) {
      autocorPeaks = try(autocorPeaks[autocorPeaks$freq > bestFreq / 1.8, , drop =
                                        F], silent = T)
      # otherwise we get false subharmonics
      autocorPeaks = try(autocorPeaks[order(autocorPeaks$amp, decreasing = T), , drop =
                                        F], silent = T)
    }
    if (class(autocorPeaks) != 'try-error') {
      if (nrow(autocorPeaks) > 0) {
        # if some peaks satisfy all criteria, return them:
        pitchAutocor_array = data.frame (
          'pitchCand' = autocorPeaks [1:min(nrow(autocorPeaks), maxNoCands), 1],
          # save n candidates of pitchAutocor, convert to Hz
          'pitchAmpl' = autocorPeaks[1:min(nrow(autocorPeaks), maxNoCands), 2],
          # save amplitudes corresponding to each pitchAutocor candidate
          'source' = 'autocor',
          stringsAsFactors = F,
          row.names = NULL
        )
      }
    }
  }
  # very occasionally HNR can be calculated as 1.01 etc. To prevent this nonsense:
  if (!is.na(HNR) & HNR >= 1) {
    HNR = 0.9999
  } # analogous to 40 dB
  ## END OF PITCH_AUTOCORRELATION


  ## PITCH_CEPSTRAL. Try to detect pitch by using ~cepstrum (fft of the spectrum). See http://www.phon.ucl.ac.uk/courses/spsci/matlab/lect10.html; modified (see file cepstrum-2.R)
  if (zpCep < length(frame)) {
    frameZP = frame
  } else {
    frameZP = c (frame, rep(0, (zpCep - length(frame))))
  }

  cepstrum = abs(fft(frameZP)) # plot(frameZP, type='l') # fft of fft, whatever you call it - cepstrum or smth else
  cepstrum = cepstrum / max(cepstrum) # plot (cepstrum, type='l')
  b = cbind (1:(length(cepstrum) %/% 2), cepstrum[1:(length(cepstrum) %/%
                                                       2)]) # plot (b, type='l')
  b[, 1] = samplingRate / (1:nrow(b)) / 2 * (length(frameZP) / length(frame)) # NB: divide by 2 because it's another fft, not reverse fft (cf. pitchAutocor)
  highestRelevantPeak = which(b[, 1] < pitch_ceiling)[1] # if we are interested in high frequencies, we need to be able to detect closely spaced peaks in the cepstrum, thus we choose a relatively narrow width of the window for finding peaks. And vice versa. If we took a width double the highestRelevantPeak, we would fail to find the very first peak (cepstrum starts with 1, like autocor), so we just take width=highestRelevantPeak
  b = b[b[, 1] > pitch_floor & b[, 1] < pitch_ceiling,]

  # find peaks
  a_zoo = as.zoo(b[, 2])
  temp = rollapply(a_zoo, width = highestRelevantPeak, align = 'center', function(x)
    isCentral.localMax(x, threshold = voiced_threshold_cep))
  idx = index(temp)[coredata(temp)]

  absCepPeak = try (idx[which.max(b[idx, 2])], silent = T)
  if (class(absCepPeak) != 'try-error') {
    idx = idx[b[idx, 1] > b[absCepPeak, 1] / 1.8]
  } # to avoid false subharmonics
  # plot (b, type='l'); points(b[idx,1], b[idx,2])
  acceptedCepPeak = idx[which.max(b[idx, 2])] # the highest cepstrum peak within pitch range
  cepstrumPeaks = data.frame ('freq' = b[acceptedCepPeak, 1], 'amp' = b[acceptedCepPeak, 2])

  if (nrow(cepstrumPeaks) > 0) {
    # if some peaks are found...
    pitchCep_array = data.frame (
      'pitchCand' = cepstrumPeaks$freq[1],
      'pitchAmpl' = cepstrumPeaks$amp[1],
      'source' = 'cepstrum',
      stringsAsFactors = F,
      row.names = NULL
    )
    # because cepstrum really stinks for frequencies above ~1 kHz, mostly picking up formants or just plain noise, we discount confidence in high-pitch cepstral estimates
    pitchCep_array$pitchAmpl = pitchCep_array$pitchAmpl / (1 + 5 / (1 +
                                                                      exp(
                                                                        14 - 3 * log2(pitchCep_array$pitchCand / pitch_floor)
                                                                      ))) # visualization: a = seq(pitch_floor, pitch_ceiling, length.out=100); b = 1+5/(1+exp(14-3*log2(a/pitch_floor))); plot (a,b, type='l')
  }
  ## END OF PITCH_CEPSTRAL


  ## PITCH_SPECTRAL. Try to detect pitch by finding peaks in the spectrum (frequency domain analysis, ~ modified BaNa algorithm)
  width = 2 * ceiling((widthSpec / bin + 1) * 20 / bin / 2) - 1 # to be always ~100 Hz, regardless of bin, but an odd number
  if (is.na(HNR)) {
    specPitchThreshold = specPitchThreshold_nullNA # if HNR is NA, the sound is probably a mess, so we play safe by only looking at very strong harmonics
  } else {
    specPitchThreshold = specPitchThreshold_nullNA - specPitchThreshold_nullNA *
      HNR * slopeSpec  # for noisy sounds the threshold is high to avoid false sumharmonics etc, for tonal sounds it is low to catch weak harmonics
  }
  temp = rollapply(frame_zoo, width = width, align = 'center', function(x)
    isCentral.localMax(x, threshold = specPitchThreshold)) # plot(frame_zoo, type='l')
  idx = index(temp)[coredata(temp)]
  specPeaks = data.frame ('freq' = as.numeric(names(frame)[idx]) * 1000, 'amp' = frame[idx])

  if (nrow(specPeaks) == 1) {
    if (specPeaks[1, 1] < pitch_ceiling & specPeaks[1, 1] > pitch_floor) {
      pitchSpec = specPeaks[1, 1]
      pitchAmpl = pitchSpec_only_peak_weight
      pitchSpec_array = data.frame(
        'pitchCand' = pitchSpec,
        'pitchAmpl' = pitchSpec_only_peak_weight,
        'source' = 'spec',
        stringsAsFactors = F,
        row.names = NULL
      )
    }
  } else if (nrow(specPeaks) > 1) {
    #
    specPeaks = specPeaks [1:min(5, nrow(specPeaks)), ] # analyze five lowest harmonics

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
        temp$AtoB_ratio[counter] = specPeaks$freq[i] / specPeaks$freq[j] # get ratios of discovered harmonics to each other
        counter = counter + 1
      }
    }
    temp = temp[temp$harmonicA > temp$harmonicB, ]

    pitchCand = numeric()
    for (i in 1:nrow(temp)) {
      pitchCand = c(pitchCand,
                    as.numeric(specPeaks$freq[temp$harmonicB[i]] / ratios$divide_lower_by [temp$AtoB_ratio[i] >
                                                                                             ratios$value_low &
                                                                                             temp$AtoB_ratio[i] < ratios$value_high])) # for each ratio that falls within the limits specified outside this function in a dataframe called "ratios", calculate the corresponding pitch. If several ratios suggest the same pitch, that's our best guess
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
        stringsAsFactors = F,
        row.names = NULL
      )
      c = 1
      while (c + 1 <= nrow(pitchSpec_array)) {
        if (abs(log2(
          pitchSpec_array$pitchCand[c] / pitchSpec_array$pitchCand[c + 1]
        )) < 1 / 12) {
          # merge cands within 1 semitone into one "super-candidate" and give this new super-candidate a certainty boost
          pitchSpec_array$specAmplIdx[c] = pitchSpec_array$specAmplIdx[c] + 1
          pitchSpec_array$pitchCand[c] = mean (c(
            pitchSpec_array$pitchCand[c],
            pitchSpec_array$pitchCand[c + 1]
          ))
          pitchSpec_array = pitchSpec_array[-(c + 1), ]
        } else {
          c = c + 1
        }
      }
      pitchSpec_array$pitchAmpl = pitchSpec_only_peak_weight + (1 / (1 +
                                                                       exp(-(
                                                                         pitchSpec_array$specAmplIdx - 1
                                                                       ))) - 0.5) * 2 * (1 - pitchSpec_only_peak_weight) # normalization. Visualization: a=1:15; b = pitchSpec_only_peak_weight + (1/(1+exp(-(a-1)))-0.5)*2 * (1-pitchSpec_only_peak_weight); plot(a,b,type='l')
      pitchSpec_array = pitchSpec_array[order(pitchSpec_array$pitchAmpl, decreasing =
                                                T), c('pitchCand', 'pitchAmpl', 'source')]
    }
  }
  if (sum(!is.na(pitchSpec_array)) > 0) {
    pitchSpec_array = pitchSpec_array[pitchSpec_array$pitchAmpl > voiced_threshold_spec, , drop =
                                        F]
    pitchSpec_array = pitchSpec_array[1:min(nrow(pitchSpec_array), maxNoCands), ]  # how many pitchSpec candidates to use (max)
  }
  ## END OF PITCH_SPECTRAL

  ## merge all pitch candidates in a single list
  pitch_array = rbind (pitchAutocor_array,
                       pitchCep_array,
                       pitchSpec_array,
                       dom_array)
  if (nrow(pitch_array) > 0) {
    pitch_array[, 1:2] = apply (pitch_array[, 1:2], 2, function(x)
      as.numeric (x)) # otherwise become characters after rbind
  }
  if (nrow(pitch_array[pitch_array$source == 'dom', ]) > 0 &
      !is.na(HNR)) {
    pitch_array$pitchAmpl[pitch_array$source == 'dom'] = 1 / (1 + exp(3 * HNR -
                                                                        1)) # dom is worth more for noisy sounds, but its weight approaches ~0.2 as HNR approaches 1 (NB: this is before HNR is converted to dB). Visualization: a = seq(0,1,length.out=100); b = 1/(1+exp(3*a-1)); plot (a,b, ylim=c(0,1))
  }

  return (list (
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

# for debugging:
# n = which(as.numeric(colnames(s))>100) [1]  # to find the frame number for time in ms
# frame = s[,n]; autoCorrelation = autocorBank[,n]
# a = analyzeFrame (frame=frame, autoCorrelation=autoCorrelation, maxNoCands=2, pitch_ceiling=15000)

