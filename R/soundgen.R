## TODO: analyze() describe new plotting pars
# spec() xlab not added if osc = FALSE
# add choice of pitch detection algorithms (defaults to all)
# Export a nice fade-in/out function, with linear/exp/log/smooth fade. It could call getSmoothContour() for generating the contour.
# pitch post-processing: document pathfinder properly; check snake - maybe break once the curve stops changing to speed things up

#' soundgen: A package for parametric synthesis of voice and acoustic analysis.
#'
#' The soundgen package provides ? categories of important functions:
#' generateBout, bla-bla and bla-bla.
#'
#' @section Functions for voice synthesis
#' The function called generateBout ...
#'
#' @docType package
#' @name soundgen
#' @import stats graphics utils grDevices
NULL

# devtools::use_data(BaNa_ratios, notes_dict, internal = TRUE, overwrite = TRUE)



#' Generate a sound
#'
#' Generates a bout of one or more syllables with pauses between them. Two basic components are synthesized: the harmonic component (the sum of sine waves with frequencies that are multiples of the fundamental frequency) and the noise component. Both components can be filtered with independently specified formants. Intonation and amplitude contours can be applied both within each syllable and across multiple syllables. Suggested application: synthesis of animal or human non-linguistic vocalizations. For more information, see \url{http://cogsci.se/soundgen.html}
#'
#' Details: put some parts of the manual here.
#' @param repeatBout the number of times the bout should be repeated
#' @param nSyl the number of syllables in the bout. Intonation, amplitude, and formants contours span multiple syllables, but not multiple bouts (see Details)
#' @param sylDur_mean average duration of each syllable, ms
#' @param pauseDur_mean average duration of pauses between syllables, ms
#' @param pitchAnchors dataframe specifying the time (ms) and value (Hz) of pitch anchors. These anchors are used to create a smooth contour of fundamental frequency f0 (pitch) within one syllable (see Details)
#' @param pitchAnchors_global unlike \code{pitchAnchors}, this dataframe is used to create a smooth contour of average f0 across multiple syllables
#' @param temperature hyperparameter for regulating the amount of stochasticity in sound generation (see Details)
#' @param maleFemale hyperparameter for shifting f0 contour, formants, and vocalTract_length to make the speaker appear more male (-1...0) or more female (0...+1).
#' @param creakyBreathy hyperparameter for a rough adjustment of voice quality from creaky (-1) to breathy (+1)
#' @param noiseAmount hyperparameter for regulating the (approximate) proportion of sound with different noise regimes (none / subharmonics only / subharmonics and jitter), 0 to 100\%. 0\% = no noise; 100\% = the entire sound has jitter + subharmonics. Ignored if temperature=0
#' @param noiseIntensity hyperparameter for regulating the intensity of subharmonics and jitter, 0 to 100\% (50\% = jitter and subharmonics are as specified, <50\% weaker, >50\% stronger). Ignored if temperature=0
#' @param jitterDep cycle-to-cycle random pitch variation (semitones)
#' @param jitterLength_ms duration of pitch jump (ms). Use a low value for harsh noise, a high value for irregular vibrato or shaky voice
#' @param vibratoFreq the rate of regular pitch modulation, or vibrato (Hz)
#' @param vibratoDep the depth of vibrato, semitones
#' @param shimmerDep random variation in amplitude between individual glottal cycles (0 to 100\% of original amplitude of each cycle)
#' @param attackLen duration of fade-in/ fade-out at each end of syllable and breathing noise (ms)
#' @param rolloff_exp basic rolloff at a constant rate of \code{rolloff_exp}
#'   db/octave (exponential decay). See \code{\link{getRolloff}} for more details
#' @param rolloff_exp_delta basic rolloff changes from lower to upper harmonics
#'   (regardless of f0) by \code{rolloff_exp_delta} dB/oct. For example, we can
#'   get steeper rolloff in the upper part of the spectrum
#' @param quadratic_delta an optional quadratic term affecting only the first
#'   \code{quadratic_nHarm} harmonics. The middle harmonic of the first
#'   \code{quadratic_nHarm} harmonics is amplified or dampened by
#'   \code{quadratic_delta} dB relative to the basic exponential decay.
#' @param quadratic_nHarm the number of harmonics affected by
#'   \code{quadratic_delta}
#' @param adjust_rolloff_per_kHz rolloff changes linearly with f0 by
#'   \code{adjust_rolloff_per_kHz} dB/kHz. For ex., -6 dB/kHz gives a 6 dB
#'   steeper basic rolloff as f0 goes up by 1000 Hz
#' @param rolloff_lip the effect of lip radiation on source spectrum, dB/oct (the default of +6 dB/oct produces a high-frequency boost when the mouth is open)
#' @param exactFormants either a character string like "aaui" referring to default presets for speaker "M1" or a list of formant frequencies, amplitude, and bandwidth (see ex. below). exactFormants=NA defaults to schwa. Time stamps for exactFormants and mouthOpening can be specified in ms, percent of duration, or whatever - the scale doesn't matter, since duration is determined by length(ampl). See \code{\link{getSpectralEnvelope}} for more details
#' @param formantStrength scale factor of formant amplitude
#' @param extraFormants_ampl the amplitude of additional formants added above the highest specified formant (only if temperature > 0)
#' @param vocalTract_length used for calculating formant dispersion and formant transitions as the mouth opens and closes (specified in cm)
#' @param g0 target frequency of subharmonics (lower than f0, adjusted dynamically so f0 is always a multiple of g0)
#' @param sideband_width_hz regulates how quickly the strength of subharmonics
#'   fades as they move away from harmonics in f0 stack. Low values produce
#'   narrow sidebands, high values produce uniformly strong subharmonics
#' @param min_epoch_length_ms minimum duration of each epoch with unchanging
#'   subharmonics regime, in ms
#' @param trill_dep amplitude modulation depth (0 to 1). 0: no change;1: amplitude modulation with amplitude range equal to the dynamic range of the sound
#' @param trill_freq amplitude modulation frequency, Hz
#' @param breathingAnchors dataframe specifying the time (ms) and frequency (Hz) of breathing anchors
#' @param exactFormants_unvoiced the same as \code{exactFormants}, but for the noise component instead of the harmonic component. If NA (default), the noise component will be filtered through the same formants as the harmonic component, approximating breathing noise [h]
#' @param rolloff_breathing rolloff of breathing noise, dB/octave. It is analogous to \code{rolloff_exp}, but while \code{rolloff_exp} applies to the harmonic component, \code{rolloff_breathing} applies to the noise component
#' @param mouthAnchors dataframe specifying the time (ms) and value (0 to 1) of mouth-opening anchors
#' @param amplAnchors dataframe specifying the time (ms) and value (0 to 1) of amplitude anchors
#' @param amplAnchors_global dataframe specifying the time (ms) and value (0 to 1) of global amplitude anchors, i.e. spanning multiple syllables
#' @param samplingRate sampling frequency (Hz)
#' @param windowLength_points Fourier window length (points)
#' @param overlap Fourier window overlap (points)
#' @param addSilence silence before and after the bout (ms)
#' @param pitch_floor,pitch_ceiling lower/upper bounds of fundamental frequency
#' @param pitchSamplingRate sampling frequency of the pitch contour only. Low values can decrease processing time (Hz). A rule of thumb is to set this to the same value as \code{pitch_ceiling}
#' @param plotSpectro if TRUE, plots a spectrogram
#' @param playSound if TRUE, plays the synthesized sound
#' @param savePath full path for saving the output, e.g. '~/Downloads/temp.wav'. If NA (default), doesn't save anything
#' @param ... other plotting parameters
#' @export
#' @return Returns the synthesized waveform as a numeric vector.
#' @examples
#' sound = generateBout(playSound = TRUE)
#' spec (sound, samplingRate = 16000, osc = TRUE)
#  # playme(sound, samplingRate = 16000)

#' # unless temperature is 0, the sound is different every time
#' for (i in 1:3) sound = generateBout(playSound = TRUE, temperature = .2)
#'
#' # Bouts versus syllables. Compare:
#' sound = generateBout (exactFormants = 'uai', repeatBout = 3, playSound = TRUE)
#' sound = generateBout (exactFormants = 'uai', nSyl = 3, playSound = TRUE)
#'
#' # Intonation contours per syllable and globally:
#' sound = generateBout (nSyl = 5, sylDur_mean = 200, pauseDur_mean = 140,
#'   playSound = TRUE, pitchAnchors = data.frame(
#'     time = c(0, 0.65, 1), value = c(977, 1540, 826)),
#'   pitchAnchors_global = data.frame(time = c(0, .5, 1), value = c(-6, 7, 0)))
#'
#' # Subharmonics in sidebands (noisy scream, chimpanzee-like)
#' sound = generateBout (noiseAmount = 100, g0 = 75, sideband_width_hz = 130,
#'   pitchAnchors = data.frame(
#'     time = c(0, .3, .9, 1), value = c(1200, 1547, 1487, 1154)),
#'   sylDur_mean = 800, samplingRate = 16000,
#'   playSound = TRUE, plotSpectro = TRUE)
#'
#' # Jitter and mouth opening (bark, dog-like)
#' sound = generateBout (repeatBout = 2, sylDur_mean = 160, pauseDur_mean = 100,
#'   noiseAmount = 100, g0 = 100, sideband_width_hz = 60, jitterDep = 1,
#'   pitchAnchors = data.frame(time = c(0, 0.52, 1), value = c(559, 785, 557)),
#'   mouthAnchors = data.frame(time = c(0, 0.5, 1), value = c(0, 0.5, 0)),
#'   vocalTract_length = 5, samplingRate = 16000, playSound = TRUE)
generateBout = function(repeatBout = 1,
                        nSyl = 1,
                        sylDur_mean = 300,
                        pauseDur_mean = 200,
                        pitchAnchors = data.frame(time = c(0, .1, .9, 1),
                                                  value = c(100, 150, 135, 100)),
                        pitchAnchors_global = NA,
                        temperature = 0.025,
                        maleFemale = 0,
                        creakyBreathy = 0,
                        noiseAmount = 0,
                        noiseIntensity = 50,
                        jitterDep = 3,
                        jitterLength_ms = 1,
                        vibratoFreq = 5,
                        vibratoDep = 0,
                        shimmerDep = 0,
                        attackLen = 50,
                        rolloff_exp = -12,
                        rolloff_exp_delta = -12,
                        quadratic_delta = 0,
                        quadratic_nHarm = 3,
                        adjust_rolloff_per_kHz = -6,
                        rolloff_lip = 6,
                        exactFormants = list(f1 = list(time = 0, freq = 860,
                                                       amp = 30, width = 120),
                                             f2 = list(time = 0, freq = 1280,
                                                       amp = 40, width = 120),
                                             f3 = list(time = 0, freq = 2900,
                                                       amp = 25, width = 200)),
                        formantStrength = 1,
                        extraFormants_ampl = 30,
                        vocalTract_length = 15.5,
                        g0 = 100,
                        sideband_width_hz = 100,
                        min_epoch_length_ms = 300,
                        trill_dep = 0,
                        trill_freq = 30,
                        breathingAnchors = NA,
                        exactFormants_unvoiced = NA,
                        rolloff_breathing = -6,
                        mouthAnchors = data.frame(time = c(0, 1),
                                                  value = c(.5, .5)),
                        amplAnchors = NA,
                        amplAnchors_global = NA,
                        samplingRate = 16000,
                        windowLength_points = 2048,
                        overlap = 75,
                        addSilence = 100,
                        pitch_floor = 50,
                        pitch_ceiling = 3500,
                        pitchSamplingRate = 3500,
                        plotSpectro = FALSE,
                        playSound = FALSE,
                        savePath = NA,
                        ...) {
  # force anchor lists to dataframe
  if (class(pitchAnchors) == 'list') pitchAnchors = as.data.frame(pitchAnchors)
  if (class(pitchAnchors_global) == 'list') pitchAnchors_global = as.data.frame(pitchAnchors_global)
  if (class(amplAnchors) == 'list') amplAnchors = as.data.frame(amplAnchors)
  if (class(amplAnchors_global) == 'list') amplAnchors_global = as.data.frame(amplAnchors_global)
  if (class(mouthAnchors) == 'list') mouthAnchors = as.data.frame(mouthAnchors)
  if (class(breathingAnchors) == 'list') breathingAnchors = as.data.frame(breathingAnchors)

  # adjust parameters according to the specified hyperparameters
  if (creakyBreathy < 0) {
    # for creaky voice
    noiseAmount = min(100, noiseAmount - creakyBreathy * 50)
    jitterDep = max(0, jitterDep - creakyBreathy / 2)
    shimmerDep = max(0, shimmerDep - creakyBreathy * 5)
    sideband_width_hz = sideband_width_hz * 2 ^ (-creakyBreathy)
  } else if (creakyBreathy > 0) {
    # for breathy voice, add breathing
    if(is.na(breathingAnchors)) breathingAnchors$time = c(0, sylDur_mean)
    breathingAnchors$value = breathingAnchors$value + creakyBreathy * 120
    breathingAnchors$value[breathingAnchors$value >
                             permittedValues['breathing_ampl', 'high']] =
      permittedValues['breathing_ampl', 'high']
  }
  # adjust rolloff for both creaky and breathy voices
  rolloff_exp = rolloff_exp - creakyBreathy * 10
  rolloff_exp_delta = rolloff_exp_delta - creakyBreathy * 5
  g0 = 2 * (g0 - 50) / (1 + exp(-.1 * (50 - noiseIntensity))) + 50
  # g0 unchanged for noiseIntensity=50%, raised for lower and
  # lowered for higher noise intensities. Max set at 2*g0-50, min at 50 Hz.
  # Illustration: g0=250; noiseIntensity=0:100; plot(noiseIntensity,
  #   2 * (g0 - 50) / (1 + exp(-.1 * (50 - noiseIntensity))) + 50, type = 'l')
  jitterDep = 2 * jitterDep / (1 + exp(.1 * (50 - noiseIntensity)))
  # Illustration: jitterDep = 1.5; noiseIntensity = 0:100;
  # plot(noiseIntensity, 2 * jitterDep / (1 + exp(.1 * (50 - noiseIntensity))),
  #   type = 'l')
  if (maleFemale != 0) {
    # adjust pitch and formants along the male-female dimension
    # pitch varies by 1 octave up or down
    pitchAnchors$value = pitchAnchors$value * 2 ^ maleFemale
    if (!is.null(exactFormants) && !is.na(exactFormants)) {
      for (f in 1:length(exactFormants)) {
        # formants vary by 25% up or down:
        #   see http://www.santiagobarreda.com/vignettes/v1/v1.html)
        exactFormants[[f]]$freq = exactFormants[[f]]$freq * 1.25 ^ maleFemale
      }
    }
    # vocalTract_length varies by ±25% from the average
    vocalTract_length = vocalTract_length * (1 - .25 * maleFemale)
  }

  # generateBout() normally expects a list of formant values,
  # but a string is also ok for demonstration purposes
  # (dictionary for caller 1 is used to interpret)
  if (class(exactFormants) == 'character') {
    exactFormants = convertStringToFormants(exactFormants)
  }
  if (class(exactFormants[[1]]) == 'list') {
    exactFormants = lapply(exactFormants, as.data.frame)
  }

  # stochastic rounding of the number of syllables and repeatBouts
  #   (eg for nSyl = 2.5, we'll have 2 or 3 syllables with equal probs)
  #   NB: this is very useful for morphing
  if (!is.integer(nSyl)) {
    nSyl = floor(nSyl) + rbinom(1, 1, nSyl - floor(nSyl))
  }
  if (!is.integer(repeatBout)) {
    repeatBout = floor(repeatBout) +
                 rbinom(1, 1, repeatBout - floor(repeatBout))
  }

  # prepare a list of pars for calling generateHarmonics()
  pars_to_vary = c(
    'noiseIntensity',
    'attackLen',
    'jitterDep',
    'shimmerDep',
    'rolloff_exp',
    'rolloff_exp_delta',
    'formantStrength',
    'min_epoch_length_ms',
    'g0',
    'sideband_width_hz'
  )
  # don't add noiseAmount, otherwise there is no simple way to remove noise at temp>0
  pars_to_round = c('attackLen', 'g0', 'sideband_width_hz')
  pars_list = list(
    'attackLen' = attackLen,
    'jitterDep' = jitterDep,
    'jitterLength_ms' = jitterLength_ms,
    'vibratoFreq' = vibratoFreq,
    'vibratoDep' = vibratoDep,
    'shimmerDep' = shimmerDep,
    'creakyBreathy' = creakyBreathy,
    'rolloff_exp' = rolloff_exp,
    'rolloff_exp_delta' = rolloff_exp_delta,
    'adjust_rolloff_per_kHz' = adjust_rolloff_per_kHz,
    'quadratic_delta' = quadratic_delta,
    'quadratic_nHarm' = quadratic_nHarm,
    'temperature' = temperature,
    'formantStrength' = formantStrength,
    'min_epoch_length_ms' = min_epoch_length_ms,
    'g0' = g0,
    'sideband_width_hz' = sideband_width_hz,
    'rolloff_lip' = rolloff_lip,
    'trill_dep' = trill_dep,
    'trill_freq' = trill_freq,
    'noiseAmount' = noiseAmount,
    'noiseIntensity' = noiseIntensity,
    'pitch_floor' = pitch_floor,
    'pitch_ceiling' = pitch_ceiling,
    'pitchSamplingRate' = pitchSamplingRate,
    'samplingRate' = samplingRate,
    'windowLength_points' = windowLength_points,
    'overlap' = overlap
  )
  pars_syllable = pars_list
  if (sum(!is.na(pitchAnchors_global)) > 0 && nSyl > 1) {
    pitchDeltas = 2 ^ (
      getDiscreteContour(
        len = nSyl,
        anchors = pitchAnchors_global,
        method = 'spline',
        plot = FALSE
      ) / 12
    )
  } else {
    pitchDeltas = rep(1, nSyl)
  }
  pitchAnchors$time = pitchAnchors$time - min(pitchAnchors$time)
  pitchAnchors$time = pitchAnchors$time / max(pitchAnchors$time) # 0 to 1
  wiggleBreathing = temperature > 0 && !is.na(breathingAnchors) &&
    sum(breathingAnchors$value > throwaway_dB) > 0
  wiggleAmpl_per_syl = temperature > 0 &&
                       !is.na(amplAnchors) &&
                       sum(amplAnchors$value < -throwaway_dB) > 0

  # START OF BOUT GENERATION
  for (b in 1:repeatBout) {
    # syllable segmentation
    sylDur_s = rnorm_bounded(
      n = 1,
      mean = sylDur_mean,
      low = permittedValues['sylDur_mean', 'low'],
      high = permittedValues['sylDur_mean', 'high'],
      sd = (permittedValues['sylDur_mean', 'high'] -
           permittedValues['sylDur_mean', 'low']) * temperature / 50,
      roundToInteger = FALSE
    )
    pauseDur_s = rnorm_bounded(
      n = 1,
      mean = pauseDur_mean,
      low = permittedValues['pauseDur_mean', 'low'],
      high = permittedValues['pauseDur_mean', 'high'],
      sd = (permittedValues['pauseDur_mean', 'high'] -
           permittedValues['pauseDur_mean', 'low']) * temperature / 50,
      roundToInteger = FALSE
    )
    syllables = divideIntoSyllables (
      sylDur_mean = sylDur_s,
      nSyl = nSyl,
      pauseDur_mean = pauseDur_s,
      sylDur_min = permittedValues['sylDur_mean', 'low'],
      sylDur_max = permittedValues['sylDur_mean', 'high'],
      pauseDur_min = permittedValues['pauseDur_mean', 'low'],
      pauseDur_max = permittedValues['pauseDur_mean', 'high'],
      temperature = temperature
    )
    syllableStartIdx = round(syllables[, 'start'] * samplingRate / 1000)
    syllableStartIdx[1] = 1
    # if noise is added before the voiced part of each syllable
    #   (negative time anchors) or starts later than the voiced part,
    #   we need to shift noise insertion points
    if (!is.na(breathingAnchors) && breathingAnchors$time[1] != 0) {
      shift = -round(breathingAnchors$time[1] * samplingRate / 1000)
      if (breathingAnchors$time[1] < 0) {
        # only the first syllableStartIdx is shifted, because that changes
        # the sound length and the remaining syllableStartIdx, if any,
        # are already shifted appropriately
        syllableStartIdx[1] = syllableStartIdx - shift
      } else {
        syllableStartIdx = syllableStartIdx - shift # shift for each syllable
      }
    }
    # end of syllable segmentation

    # START OF SYLLABLE GENERATION
    voiced = vector()
    unvoiced = list()
    breathingAnchors_syl = list()

    for (s in 1:nrow(syllables)) {
      # wiggle par values for this particular syllable, making sure
      #   they are within the permitted range for each variable
      pitchAnchors_per_syl = pitchAnchors
      amplAnchors_per_syl = amplAnchors

      if (temperature > 0) {
        # OR if (temperature>0 & nrow(syllables)>1)
        # if you don't want to mess with single-syllable vocalizations
        for (p in 1:length(pars_to_vary)) {
          l = permittedValues[pars_to_vary[p], 'low']
          h = permittedValues[pars_to_vary[p], 'high']
          pars_syllable[pars_to_vary[p]] = rnorm_bounded(
            n = 1,
            mean = as.numeric(pars_list[pars_to_vary[p]]),
            low = l,
            high = h,
            sd = (h - l) * temperature / 10,
            roundToInteger = (pars_to_vary[p] %in% pars_to_round)
          )
          # /10 to have less variation in the spectral pars vs.
          # duration of separate syllables
        }
        pitchAnchors_per_syl = wiggleAnchors(
          df = pitchAnchors_per_syl,
          temperature = temperature,
          low = c(0, permittedValues['pitch', 'low']),
          high = c(1, permittedValues['pitch', 'high']),
          temp_coef = pitchAnchorsWiggle_per_temp
        )
        if (wiggleBreathing) {
          breathingAnchors_syl[[s]] = wiggleAnchors(
            df = breathingAnchors,
            temperature = temperature,
            low = c(-Inf, permittedValues['breathing_ampl', 'low']),
            high = c(+Inf, permittedValues['breathing_ampl', 'high']),
            wiggleAllRows = TRUE,
            temp_coef = breathingAnchorsWiggle_per_temp
          )
        }
        if (wiggleAmpl_per_syl) {
          amplAnchors_per_syl = wiggleAnchors(
            df = amplAnchors_per_syl,
            temperature = temperature,
            low = c(0, 0),
            high = c(1,-throwaway_dB),
            temp_coef = amplAnchorsWiggle_per_temp
          )
        }
      }

      # generate smooth pitch contour for this particular syllable
      dur_syl = as.numeric(syllables[s, 'end'] - syllables[s, 'start'])
      pitchContour_syl = getSmoothContour(
        anchors = pitchAnchors_per_syl,
        len = round(dur_syl * pitchSamplingRate / 1000),
        samplingRate = pitchSamplingRate,
        value_floor = pitch_floor,
        value_ceiling = pitch_ceiling,
        thisIsPitch = TRUE
      ) * pitchDeltas[s]
      # plot(pitchContour_syl, type = 'l')

      # generate the voiced part
      if (dur_syl < permittedValues['sylDur_mean', 'low'] |
          (!is.na(breathingAnchors) && min(breathingAnchors$value) >= 40)) {
        # only synthesize voiced part if breathing is weaker than 40 dB
        #   and the voiced part is long enough to bother synthesizing it
        syllable = rep(0, round(dur_syl * samplingRate / 1000))
      } else {
        syllable = try(do.call(generateHarmonics,  c(pars_syllable,
          list(pitch = pitchContour_syl, amplAnchors = amplAnchors_per_syl))))
        # the actual synthesis is here
      }
      # spec (syllable, samplingRate = samplingRate)
      # playme(syllable, samplingRate = samplingRate)
      if (class(syllable) == 'try-error') {
        stop ('Failed to generate the new syllable!')
      }
      if (sum(is.na(syllable)) > 0) {
        stop ('The new syllable contains NA values!')
      }

      # generate pause for all but the last syllable
      if (s < nrow(syllables)) {
        pause = rep(0, floor((syllables[s + 1, 1] - syllables[s, 2]) *
                               samplingRate / 1000))
      } else {
        pause = numeric()
      }

      # add syllable and pause to the growing sound
      voiced = c(voiced, syllable, pause)

      # generate the unvoiced part, but don't add it to the sound just yet
      if (!is.na(breathingAnchors) &&
          sum(breathingAnchors$value > throwaway_dB) > 0) {
        # adjust breathingAnchors$time to match the actual syllable duration
        breathingAnchors_syl[[s]] = breathingAnchors
        breathingAnchors_syl[[s]]$time[breathingAnchors_syl[[s]]$time > 0] =
          breathingAnchors_syl[[s]]$time[breathingAnchors_syl[[s]]$time > 0] *
          dur_syl / sylDur_mean
        # negative time anchors are not changed: the pre-aspiration length
        # is constant, regardless of the actual syllable duration.
        # However, positive time anchors are proportional to the actual
        # syllable duration re the average expected duration (which the user
        # sees in the UI when choosing time anchors)
        unvoicedDur_syl = round(diff(range(breathingAnchors_syl[[s]]$time)) *
                          samplingRate / 1000)

        # calculate noise spectrum
        if (is.na(exactFormants_unvoiced[1])) {
          spectralEnvelope_unvoiced = NA
        } else {
          movingFormants = max(unlist(lapply(exactFormants_unvoiced, length))) > 1 |
            sum(mouthAnchors$value != .5) > 0 # are noise formants moving, as opposed to constant?
          nInt = ifelse (movingFormants,
                    round(diff(range(breathingAnchors_syl[[s]]$time)) / 10),
                    1) # the number of different noise spectra,
          # allowing one column (noise spectrum) per 10 ms of audio
          spectralEnvelope_unvoiced = getSpectralEnvelope(
            nr = windowLength_points / 2,
            nc = nInt,
            exactFormants = exactFormants_unvoiced,
            formantStrength = formantStrength,
            extraFormants_ampl = extraFormants_ampl,
            rolloff_lip = rolloff_lip,
            mouthAnchors = mouthAnchors,
            temperature = temperature,
            samplingRate = samplingRate,
            vocalTract_length = vocalTract_length
          )
          # image(t(spectralEnvelope_unvoiced))
        }

        # synthesize the unvoiced part
        unvoiced[[s]] = generateNoise(
          len = unvoicedDur_syl,
          breathingAnchors = breathingAnchors_syl[[s]],
          rolloff_breathing = rolloff_breathing,
          attackLen = attackLen,
          samplingRate = samplingRate,
          windowLength_points = windowLength_points,
          overlap = overlap,
          filter_breathing = spectralEnvelope_unvoiced
        )
        # plot(unvoiced[[s]], type = 'l')
      }
    }
    # plot (voiced, type = 'l')
    # spec (voiced, samplingRate = samplingRate, osc = TRUE)
    # playme(voiced, samplingRate = samplingRate)
    # END OF SYLLABLE GENERATION

    # if the unvoiced noise is of type "breathing" (the same formants as in
    #   the voiced part), we mix voiced+unvoiced BEFORE filtering the sound,
    #   otherwise we filter first and then mix voiced+unvoiced
    sound = voiced
    if (length(unvoiced) > 0 && is.na(exactFormants_unvoiced)) {
      for (s in 1:length(unvoiced)) {
        sound = addVectors(sound, unvoiced[[s]],
                           insertionPoint = syllableStartIdx[s])
      }
    }
    # plot (sound, type = 'l')
    # spec (sound, samplingRate = samplingRate)
    # playme(sound, samplingRate = samplingRate)

    # for polysyllabic vocalizations, apply amplitude envelope (if specified)
    #   over the entire bout and normalize to -1...+1
    if (!is.na(amplAnchors_global) &&
        length(which(amplAnchors_global$value < -throwaway_dB)) > 0) {
      # convert from dB to linear multiplier
      amplAnchors_global$value = 2 ^ (amplAnchors_global$value / 10)
      amplEnvelope = getSmoothContour(
        anchors = amplAnchors_global,
        len = length(sound),
        value_floor = 0,
        value_ceiling = -throwaway_dB,
        samplingRate = samplingRate
      )  # plot(amplEnvelope)
      sound = sound * amplEnvelope
    }

    # prepare vocal tract filter (formants + some spectral noise + lip radiation)
    if (sum(sound) == 0) {
      # ie if we didn't synthesize a voiced syllable (unvoiced part only) -
      #   otherwise fft glitches
      sound_filtered = sound
    } else {
      # for very short sounds, make sure the analysis window is no more
      #   than half the sound's length
      windowLength_points = min(windowLength_points, floor(length(sound) / 2))
      step = seq(1,
                 max(1, (length(sound) - windowLength_points)),
                 windowLength_points - (overlap * windowLength_points / 100))
      nc = length(step) # number of windows for fft
      nr = windowLength_points / 2 # number of frequency bins for fft
      movingFormants = max(unlist(lapply(exactFormants, length))) > 1 |
        sum(mouthAnchors$value != .5) > 0 # are formants moving or constant?
      nInt = ifelse (movingFormants, nc, 1)
      spectralEnvelope = getSpectralEnvelope(
        nr = nr,
        nc = nInt,
        exactFormants = exactFormants,
        formantStrength = formantStrength,
        extraFormants_ampl = extraFormants_ampl,
        rolloff_lip = rolloff_lip,
        mouthAnchors = mouthAnchors,
        temperature = temperature,
        samplingRate = samplingRate,
        vocalTract_length = vocalTract_length
      )
      # image(t(spectralEnvelope))

      # fft and filtering
      z <- seewave::stft(
        wave = as.matrix(sound),
        f = samplingRate,
        wl = windowLength_points,
        zp = 0,
        step = step,
        wn = 'hamming',
        fftw = FALSE,
        scale = TRUE,
        complex = TRUE
      )
      if (movingFormants) {
        z = z * spectralEnvelope
      } else {
        z = apply (z, 2, function(x)
          x * spectralEnvelope)
      }

      # inverse fft
      sound_filtered = as.numeric(
        seewave::istft(
          z,
          f = samplingRate,
          ovlp = overlap,
          wl = windowLength_points,
          output = "matrix"
        )
      )
      sound_filtered = sound_filtered / max(sound_filtered) # normalize
    }
    # spec (sound_filtered, samplingRate = samplingRate)
    # playme(sound_filtered, samplingRate = samplingRate)

    # add the separately filtered noise back into the sound at the appropriate time points AFTER filtering the sound
    if (length(unvoiced) > 0 && !is.na(exactFormants_unvoiced)) {
      for (s in 1:length(unvoiced)) {
        sound_filtered = addVectors(sound_filtered, unvoiced[[s]],
                                    insertionPoint = syllableStartIdx[s])
      }
    } # plot(sound_filtered, type = 'l')

    # trill - rapid regular amplitude modulation
    if (trill_dep > 0) {
      trill = 1 - sin (2 * pi * (1:length(sound_filtered)) /
              samplingRate * trill_freq) * trill_dep / 2
      # plot (trill, type='l')
    } else {
      trill = 1
    }
    sound_filtered = sound_filtered * trill

    # grow bout
    if (b == 1) {
      bout = sound_filtered
    } else {
      bout = c(bout,
               rep(0, pauseDur_mean * samplingRate / 1000),
               sound_filtered)
    }
  }

  # add some silence before and after the entire bout
  if (!is.na(addSilence)) {
    n = round(samplingRate / 1000 * addSilence)
    bout = c(rep(0, n), bout, rep(0, n))
  }

  if (playSound) {
    playme (bout, samplingRate = samplingRate)
    # spec (sound_filtered, samplingRate = samplingRate, osc = TRUE)
  }
  if (!is.na(savePath)) {
    seewave::savewav(bout, filename = savePath, f = samplingRate)
  }
  if (plotSpectro) {
    spec (bout, samplingRate = samplingRate)
  }
  return (bout)
}
