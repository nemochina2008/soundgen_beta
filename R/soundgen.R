# Top-level functions for generating sounds
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
#' @importFrom graphics abline axis layout lines par points rect text
#' @importFrom stats approx dgamma dnorm fft loess median na.omit predict quantile rbinom rgamma rnorm runif spline var
#' @importFrom utils head tail
#' @importFrom grDevices gray
NULL



#' Generate a continuous tonal sound
#'
#' Internal soundgen function.
#'
#' Returns one continuous, unfiltered, voiced syllable consisting of several sine waves.
#' @param pitch a contour of fundamental frequency (numeric vector). NB: for computational efficiency, provide the pitch contour at a reduced sampling rate pitchSamplingRate, eg 3500 points/s. The pitch contour will be upsampled before synthesis.
#' @inheritParams generateBout
#' @examples
#' pitch=soundgen:::getSmoothContour(len=3500,
#'   anchors=data.frame('time'=c(0,1), 'ampl'=c(200,300)))
#' plot(pitch)
#' sound = soundgen:::generateSyllable(pitch, samplingRate=16000)
#' # playme(sound, samplingRate=16000) # no formants yet
generateSyllable = function(pitch, attackLen=50, noiseAmount=0, noiseIntensity=0, jitterDep=0, jitterLength_ms=1, vibratoFreq=100, vibratoDep=0, shimmerDep=0, creakyBreathy=0, rolloff_exp=-18, rolloff_exp_delta=-2, adjust_rolloff_per_kHz=-6, quadratic_delta=0, quadratic_nHarm=3, formantStrength=1, temperature=0, min_epoch_length_ms=300, g0=100, sideband_width_hz=0, rolloff_lip=6, trill_dep=0, trill_freq=30, amplAnchors=NA, overlap=75, windowLength_points=2048, samplingRate=44100, pitch_floor=75, pitch_ceiling=3500, pitchSamplingRate=3500) {
  ## PRE-SYNTHESIS EFFECTS (NB: the order in which effects are added is NOT arbitrary!)
  # vibrato (performed on pitch, not pitch_per_gc!)
  if (vibratoDep>0){
    vibrato = 2^(sin(2*pi*(1:length(pitch))*vibratoFreq/pitchSamplingRate) * vibratoDep/12) # plot(vibrato[], type='l')
    pitch = pitch * vibrato
  } # plot (pitch, type='l')

  # transform f0 per s to f0 per glottal cycle
  gc = getGlottalCycles(pitch, samplingRate=pitchSamplingRate) # our "glottal cycles"
  pitch_per_gc = pitch[gc]
  nGC = length(pitch_per_gc)

  # generate a short amplitude contour to adjust rolloff_exp per glottal cycle
  if (!is.na(amplAnchors) && length(which(amplAnchors$ampl < -throwaway_dB))>0){
    amplContour = getSmoothContour(anchors=amplAnchors, len=nGC, ampl_floor=0, ampl_ceiling=-throwaway_dB, samplingRate=samplingRate) # plot (amplContour, type='l')
    amplContour = amplContour/abs(throwaway_dB) - 1
    rolloff_exp_ampl = amplContour * rolloff_per_ampl
  } else {
    rolloff_exp_ampl = 0
  }

  # get a random walk for intra-syllable variation
  if (temperature > 0){
    rw = getRandomWalk (len=nGC, rw_range=temperature, trend=c(randomWalk_trendStrength,-randomWalk_trendStrength), rw_smoothing=.3) # plot (rw, type='l')
    rw_0_100 = rw-min(rw); rw_0_100 = rw_0_100/max(rw_0_100)*100 # plot (rw_0_100, type='l')
    rw_bin = getBinaryRandomWalk (rw_0_100, noiseAmount=noiseAmount, minLength=ceiling(min_epoch_length_ms/1000*pitch_per_gc)) # minLength is min_epoch_length_ms / period_ms, where period_ms = 1000/pitch_per_gc
    # plot(rw_bin)
    rw = rw - mean(rw) + 1 # change mean(rw) to 1
    vocalFry_on = rw_bin>0 # when is vocal fry on? For ex., rw_bin==1 sets vocal fry on only in regime 1, while rw_bin>0 sets vocal fry on in regimes 1 and 2 (ie together with jitter)
    jitter_on = shimmer_on = rw_bin==2
  } else {
    rw = rep(1, nGC)
    vocalFry_on = jitter_on = shimmer_on = rep(1, nGC)
  }

  # calculate jitter (random variation of F0)
  if (jitterDep>0 & noiseAmount>0){
    ratio = pitch_per_gc * jitterLength_ms / 1000 # the number of gc that make up one jitter period (vector of length nGC)
    idx = 1
    i = 1
    while (i < nGC){
      i = tail(idx,1)+ratio[i]
      idx = c(idx, i)
    }
    idx = round(idx)
    idx = idx[idx <= nGC] # pitch for these gc will be wiggled
    idx = unique(idx)

    jitter = 2^(rnorm(n=length(idx), mean=0, sd=jitterDep/12)*rw[idx]*jitter_on[idx]) # plot(jitter, type='l')
    # jitter_per_gc = approx(jitter, n=nGC, x=idx, method='constant')$y # plot(jitter_per_gc, type='l')
    jitter_per_gc = spline(jitter, n=nGC, x=idx)$y # plot(jitter_per_gc, type='l')
    # jitter = 2^(rnorm(n=length(pitch_per_gc), mean=0, sd=jitterDep/12)*rw*jitter_on) # plot(jitter, type='l')
    pitch_per_gc = pitch_per_gc * jitter_per_gc # plot(pitch_per_gc, type='l')
  }

  # calculate random drift of F0 (essentially the same as jitter but slow, ie with smoothing)
  if (temperature>0) {
    # # illustration of the effects of temperature and number of gc's on the amount of smoothing applied to the random drift of f0
    # library(reshape2)
    # library(plot3D)
    # df = expand.grid(temperature = seq(0,1,length.out=30), n_gc = seq(1,1000,length.out=30))
    # df$rw_smoothing = .9 - df$temperature/8 - 1.2/(1+exp(-.008*(df$n_gc-10)))+.6 # 10 gc is "neutral"
    # out_pred = as.matrix(dcast(df, temperature~n_gc, value.var="rw_smoothing"))
    # rownames(out_pred) = seq(0,1,length.out=30)
    # out_pred = out_pred[,-1]
    # persp3D (as.numeric(rownames(out_pred)), as.numeric(colnames(out_pred)), out_pred, theta=40, phi=50, zlab='rw_smoothing', xlab='Temperature', ylab='# of glottal cycles', colkey=F, ticktype='detailed', cex.axis=0.75)
    drift = getRandomWalk (len=length(pitch_per_gc), rw_range=temperature*pitchDrift_per_temp + length(pitch_per_gc)/1000/12, rw_smoothing=.9 - temperature*pitchDriftWiggle_per_temp - 1.2/(1+exp(-.008*(length(pitch_per_gc)-10)))+.6, method='spline') # we get a separate random walk for this slow drift of intonation. Its smoothness vs wiggleness depends on temperature and duration (in glottal cycles). For ex., temperature*2 means that pitch will vary within one octave if temperature==1
    drift = 2^(drift - mean(drift)) # plot (drift, type='l')
    pitch_per_gc = pitch_per_gc * drift  # plot(pitch_per_gc, type='l')
  }

  # as a result of adding pitch effects, F0 might have dropped to indecent values, so we double-check and flatten
  pitch_per_gc[pitch_per_gc>pitch_ceiling] = pitch_ceiling
  pitch_per_gc[pitch_per_gc<pitch_floor] = pitch_floor

  # prepare the harmonic stack
  nHarmonics=ceiling(samplingRate/2-min(pitch_per_gc))/min(pitch_per_gc) # calculate the number of harmonics to generate (from lowest pitch to nyquist)
  rolloff_source = getRolloff(pitch_per_gc=pitch_per_gc, nHarmonics=nHarmonics, rolloff_exp=(rolloff_exp+rolloff_exp_ampl)*rw^3, rolloff_exp_delta=rolloff_exp_delta*rw^3, adjust_rolloff_per_kHz=adjust_rolloff_per_kHz*rw, quadratic_delta=quadratic_delta, quadratic_nHarm=quadratic_nHarm, samplingRate=samplingRate) # NB: this whole pitch_per_gc trick is purely for computational efficiency - the entire pitch contour can be fed in, but then it takes up to 1 s / s audio
  # image(t(rolloff_source))

  # add shimmer (random variation in amplitude)
  if (shimmerDep>0 & noiseAmount>0){
    shimmer = 2^(rnorm (n=ncol(rolloff_source), mean=0, sd=shimmerDep/100)*rw*shimmer_on) # plot(shimmer, type='l')
    rolloff_source = t(t(rolloff_source)*shimmer) # multiplies the first column of rolloff_source by shimmer[1], the second column by shimmer[2], etc
  }

  # add vocal fry (subharmonics)
  if (sideband_width_hz>0 & noiseAmount>0) {
    vocalFry = getVocalFry (rolloff=rolloff_source, pitch_per_gc=pitch_per_gc, g0=g0*rw^4, sideband_width_hz=sideband_width_hz*rw^4*vocalFry_on, min_epoch_length_ms=min_epoch_length_ms)
    rolloff_source = vocalFry$rolloff # list of matrices
    epochs = vocalFry$epochs # dataframe
  } else { # if we don't need to add vocal fry
    rolloff_source = list(rolloff_source)
    epochs = data.frame ('start'=1, 'end'=length(pitch_per_gc))
  }

  ## WAVEFORM GENERATION
  up = upsample(pitch_per_gc, samplingRate=samplingRate) # upsample pitch contour to full resolution (samplingRate). Uses a more sophisticated but still very fast version of linear interpolation, which takes into account the variable length of glottal cycles
  pitch_upsampled = up$pitch
  gc_upsampled = up$gc
  integr = cumsum(pitch_upsampled)/samplingRate # plot (integr[1:10000], type='l')
  waveform = 0

  # synthesize one epoch at a time and join by cross-fading
  for (e in 1:nrow(epochs)){
    idx_gc_up = gc_upsampled[epochs$start[e] : (epochs$end[e]+1)]
    idx_up = min(idx_gc_up):max(idx_gc_up)
    waveform_epoch = rep(0, length(idx_up))
    integr_epoch = integr[idx_up]
    rolloff_epoch = rolloff_source[[e]] # rolloff_source MUST be a list!

    for (h in 1:nrow(rolloff_epoch)){ # NB: rolloff_source can have fewer harmonics that nHarmonics, since weak harmonics are discarded for computational efficiency. Or it can have a lot more than nHarmonics, if we add vocal fry
      times_f0 = as.numeric(rownames(rolloff_epoch)[h]) # freq of harmonic h as a multiple of f0
      am_upsampled = approx(rolloff_epoch[h,], n=length(idx_up), x=idx_gc_up[-length(idx_gc_up)])$y # plot(am_upsampled, type='l')
      waveform_epoch = waveform_epoch + sin(2*pi*integr_epoch*times_f0) * am_upsampled # the actual waveform synthesis happens in this one line
      # # normalize amplitude to avoid abrupt amplitude jumps as subharmonics regime changes
      # if (e>1){
      #   max_prev = max(tail(ampl, samplingRate*.1)) # over the last ... ms of the previous epoch
      #   max_new = max(head(ampl_epoch, samplingRate*.1)) # over the first ... ms of the new epoch
      #   ampl_epoch = ampl_epoch * max_prev/max_new
      #   RMS_prev = sqrt(mean(tail(ampl, samplingRate*.1)^2)) # over the last ... ms of the previous epoch
      #   RMS_new = sqrt(mean(head(ampl_epoch, samplingRate*.1)^2)) # over the first ... ms of the new epoch
      #   ampl_epoch = ampl_epoch * RMS_prev/RMS_new
      # }
    }
    waveform = crossFade(waveform, waveform_epoch, length_ms=15, samplingRate=samplingRate) # longer length_ms provides for smoother transitions, but it shortens the resulting sound. Short transitions preserve sound duration but may create a click where two epochs are joined
  }
  # sum(is.na(waveform))
  # plot(waveform[], type='l')
  # spectro_denoised (waveform, samplingRate=samplingRate, osc=T); playme(waveform, samplingRate=samplingRate) # meanspec(waveform, f=samplingRate)


  ## POST-SYNTHESIS EFFECTS
  # apply amplitude envelope and normalize to be on the same scale as breathing
  if (!is.na(amplAnchors) && length(which(amplAnchors$ampl < -throwaway_dB))>0){
    amplAnchors$ampl = 2^(amplAnchors$ampl/10) # convert from dB to linear multiplier
    amplEnvelope = getSmoothContour(anchors=amplAnchors, len=length(waveform), ampl_floor=0, samplingRate=samplingRate) # plot (amplEnvelope, type='l')
    waveform = waveform * amplEnvelope
  }
  waveform = waveform/max(waveform)

  # add attack
  if (attackLen>0){
    waveform = fadeInOut(waveform, length_fade=floor(attackLen*samplingRate/1000)) # plot(waveform, type='l')
  }

  # pitch drift is accompanied by amplitude drift
  if (temperature>0){
    drift_upsampled = approx(drift, n=length(waveform), x=gc_upsampled[-length(gc_upsampled)])$y # plot(drift_upsampled, type='l')
  } else {
    drift_upsampled = 1
  }
  waveform = waveform * drift_upsampled
  # playme(waveform, samplingRate=samplingRate); spectro_denoised (waveform, samplingRate=samplingRate)
  return (waveform)
}


#' Generate a sound
#'
#' Generates a bout of one or more syllables with pauses between them. Two basic components are synthesized: the harmonic component (the sum of sine waves with frequencies that are multiples of the fundamental frequency) and the noise component. Both components can be filtered with independently specified formants. Intonation and amplitude contours can be applied both within each syllable and across multiple syllables. Suggested application: synthesis of animal or human non-linguistic vocalizations. For more information, see \url{http://cogsci.se/soundgen.html}
#'
#' Details: put some parts of the manual here.
#' @param repeatBout the number of times the bout should be repeated
#' @param nSyl the number of syllables in the bout. Intonation, amplitude, and formants contours span multiple syllables, but not multiple bouts (see Details)
#' @param sylDur_mean average duration of each syllable, ms
#' @param pauseDur_mean average duration of pauses between syllables, ms
#' @param pitchAnchors dataframe specifying the time (ms) and frequency (Hz) of pitch anchors. These anchors are used to create a smooth contour of fundamental frequency f0 (pitch) within one syllable (see Details)
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
#' @param mouthAnchors dataframe specifying the time (ms) and size (0 to 1) of mouth-opening anchors
#' @param amplAnchors dataframe specifying the time (ms) and amplitude (0 to 1) of amplitude anchors
#' @param amplAnchors_global dataframe specifying the time (ms) and amplitude (0 to 1) of global amplitude anchors, i.e. spanning multiple syllables
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
#' sound = generateBout(playSound = T)
#' spectro_denoised (sound, samplingRate=16000, osc=T)
#  # playme(sound, samplingRate=16000)

#' # unless temperature is 0, the sound is different every time
#' for (i in 1:3) sound = generateBout(playSound = T, temperature = .2)
#'
#' # Bouts versus syllables. Compare:
#' sound = generateBout (exactFormants='uai', repeatBout=3, playSound=T)
#' sound = generateBout (exactFormants='uai', nSyl=3, playSound=T)
#'
#' # Intonation contours per syllable and globally:
#' sound = generateBout (nSyl=5, sylDur_mean=200, pauseDur_mean=140, playSound=T,
#'   pitchAnchors=data.frame(time=c(0,0.65,1), ampl=c(977,1540,826)),
#'   pitchAnchors_global=data.frame(time=c(0,.5,1),ampl=c(-6,7,0)))
#'
#' # Subharmonics in sidebands (noisy scream, chimpanzee-like)
#' sound = generateBout (noiseAmount=100, g0=75, sideband_width_hz=130,
#'   pitchAnchors=data.frame(time=c(0,.3,.9,1), ampl=c(1200,1547,1487,1154)),
#'   sylDur_mean=800, samplingRate=16000,
#'   playSound=T, plotSpectro=T)
#'
#' # Jitter and mouth opening (bark, dog-like)
#' sound = generateBout (repeatBout=2, sylDur_mean=160, pauseDur_mean=100,
#'   noiseAmount=100, g0=100, sideband_width_hz=60, jitterDep=1,
#'   pitchAnchors=data.frame(time=c(0,0.52,1), ampl=c(559,785,557)),
#'   mouthAnchors=data.frame(time=c(0,0.5,1), ampl=c(0,0.5,0)),
#'   vocalTract_length=5, samplingRate=16000, playSound=T)
generateBout = function(repeatBout=1, nSyl=1, sylDur_mean=300, pauseDur_mean=200,
                        pitchAnchors=data.frame('time'=c(0, .1, .9, 1),
                                                'ampl'=c(100, 150, 135, 100)),
                        pitchAnchors_global=NA,
                        temperature=0.025, maleFemale=0, creakyBreathy=0,
                        noiseAmount=0, noiseIntensity=50,
                        jitterDep=0, jitterLength_ms=1,
                        vibratoFreq=5, vibratoDep=0,
                        shimmerDep=0, attackLen=50,
                        rolloff_exp=-12, rolloff_exp_delta=-12,
                        quadratic_delta=0, quadratic_nHarm=3,
                        adjust_rolloff_per_kHz=-6, rolloff_lip=6,
                        exactFormants=list(
                          f1=data.frame( time=c(0), freq=c(860), amp=c(30), width=c(120)),
                          f2=data.frame( time=c(0), freq=c(1280), amp=c(40), width=c(120))),
                        formantStrength=1, extraFormants_ampl=30, vocalTract_length=15.5,
                        g0=100, sideband_width_hz=0, min_epoch_length_ms=300,
                        trill_dep=0, trill_freq=30,
                        breathingAnchors=data.frame('time'=c(0,300),
                                                    'ampl'=c(throwaway_dB,throwaway_dB)),
                        exactFormants_unvoiced=NA, rolloff_breathing=-6,
                        mouthAnchors=data.frame(time=c(0,1),ampl=c(.5,.5)),
                        amplAnchors=NA,
                        amplAnchors_global=NA,
                        samplingRate=16000, windowLength_points=2048, overlap=75,
                        addSilence=100,
                        pitch_floor=50, pitch_ceiling=3500, pitchSamplingRate=3500,
                        plotSpectro=F, playSound=F, savePath=NA, ...){

  # adjust parameters according to the specified hyperparameters
  if (creakyBreathy<0) { # for creaky voice
    noiseAmount = min(100, noiseAmount - creakyBreathy*50)
    jitterDep = max(0, jitterDep - creakyBreathy/2)
    shimmerDep = max(0, shimmerDep - creakyBreathy*5)
    sideband_width_hz = sideband_width_hz*2^(-creakyBreathy)
  } else if (creakyBreathy>0){ # for breathy voice, add breathing
    breathingAnchors$ampl = breathingAnchors$ampl + creakyBreathy*120
  }
  rolloff_exp = rolloff_exp - creakyBreathy*10   # adjust for both creaky and breathy voices
  rolloff_exp_delta = rolloff_exp_delta - creakyBreathy*5
  g0 = 2*(g0-50)/(1+exp(-.1*(50-noiseIntensity)))+50 # g0 unchanged for noiseIntensity=50%, raised for lower and lowered for higher noise intensities. Max set at 2*g0-50, min at 50 Hz. Illustration: g0=250; noiseIntensity=0:100; plot(noiseIntensity, 2*(g0-50)/(1+exp(-.1*(50-noiseIntensity)))+50, type='l')
  jitterDep = 2*jitterDep/(1+exp(.1*(50-noiseIntensity))) # Illustration: jitterDep=1.5; noiseIntensity=0:100; plot(noiseIntensity, 2*jitterDep/(1+exp(.1*(50-noiseIntensity))), type='l')
  if (maleFemale != 0){ # adjust pitch and formants along the male-female dimension
    pitchAnchors$ampl = pitchAnchors$ampl*2^maleFemale  # pitch varies by 1 octave up or down
    if (!is.null(exactFormants) && !is.na(exactFormants)){
      for (f in 1:length(exactFormants)){
        exactFormants[[f]]$freq = exactFormants[[f]]$freq*1.25^maleFemale
      } # formants vary by 25% up or down: see http://www.santiagobarreda.com/vignettes/v1/v1.html)
    }
    vocalTract_length = vocalTract_length *(1-.25*maleFemale) # varies by ±25% from the average
  }

  # generateBout() normally expects a list of formant values, but a string is also ok for demonstration purposes (dictionary for caller 1 is used to interpret)
  if (class(exactFormants)=='character'){
    exactFormants = convertStringToFormants(exactFormants)
  }

  # stochastic number of syllables (eg for nSyl=2.5, we'll have 2 or 3 syllables with equal probs)
  if (!is.integer(nSyl)) nSyl = floor(nSyl) + rbinom (1, 1, nSyl-floor(nSyl))
  if (!is.integer(repeatBout)) repeatBout = floor(repeatBout) + rbinom (1, 1, repeatBout-floor(repeatBout))

  # prepare a list of pars for calling generateSyllable()
  pars_to_vary = c('noiseIntensity', 'attackLen', 'jitterDep', 'shimmerDep', 'rolloff_exp', 'rolloff_exp_delta', 'formantStrength', 'min_epoch_length_ms', 'g0', 'sideband_width_hz') # don't add noiseAmount, otherwise there is no simple way to remove noise at temp>0
  pars_to_round = c('attackLen', 'g0', 'sideband_width_hz')
  pars_list = list('attackLen'=attackLen, 'jitterDep'=jitterDep, 'jitterLength_ms'=jitterLength_ms, 'vibratoFreq'=vibratoFreq, 'vibratoDep'=vibratoDep, 'shimmerDep'=shimmerDep, 'creakyBreathy'=creakyBreathy, 'rolloff_exp'=rolloff_exp, 'rolloff_exp_delta'=rolloff_exp_delta, 'adjust_rolloff_per_kHz'=adjust_rolloff_per_kHz, 'quadratic_delta'=quadratic_delta, 'quadratic_nHarm'=quadratic_nHarm, 'temperature'=temperature, 'formantStrength'=formantStrength, 'min_epoch_length_ms'=min_epoch_length_ms, 'g0'=g0, 'sideband_width_hz'=sideband_width_hz, 'rolloff_lip'=rolloff_lip, 'trill_dep'=trill_dep, 'trill_freq'=trill_freq, 'noiseAmount'=noiseAmount, 'noiseIntensity'=noiseIntensity, 'pitch_floor'=pitch_floor, 'pitch_ceiling'=pitch_ceiling, 'pitchSamplingRate'=pitchSamplingRate, 'samplingRate'=samplingRate, 'windowLength_points'=windowLength_points, 'overlap'=overlap)
  pars_syllable = pars_list
  if (sum(!is.na(pitchAnchors_global))>0 && nSyl>1){
    pitchDeltas = 2^(getDiscreteContour (len=nSyl, anchors=pitchAnchors_global, method='spline', plot=F)/12)
  } else {
    pitchDeltas = rep(1, nSyl)
  }
  pitchAnchors$time = pitchAnchors$time-min(pitchAnchors$time)
  pitchAnchors$time = pitchAnchors$time/max(pitchAnchors$time) # strictly 0 to 1
  wiggleBreathing = temperature>0 && !is.na(breathingAnchors) && sum(breathingAnchors$ampl > throwaway_dB)>0
  wiggleAmpl_per_syl = temperature>0 && !is.na(amplAnchors) && sum(amplAnchors$ampl < -throwaway_dB)>0


  # START OF BOUT GENERATION
  for (b in 1:repeatBout){
    # syllable segmentation
    syllables = divideIntoSyllables (sylDur_mean=rnorm_bounded(n=1, mean=sylDur_mean, low=permittedValues['sylDur_mean','low'], high=permittedValues['sylDur_mean','high'], sd=(permittedValues['sylDur_mean','high']-permittedValues['sylDur_mean','low'])*temperature/50, roundToInteger=F), nSyl=nSyl, pauseDur_mean=rnorm_bounded(n=1, mean=pauseDur_mean, low=permittedValues['pauseDur_mean','low'], high=permittedValues['pauseDur_mean','high'], sd=(permittedValues['pauseDur_mean','high']-permittedValues['pauseDur_mean','low'])*temperature/50, roundToInteger=F), sylDur_min=permittedValues['sylDur_mean','low'], sylDur_max=permittedValues['sylDur_mean','high'], pauseDur_min=permittedValues['pauseDur_mean','low'], pauseDur_max=permittedValues['pauseDur_mean','high'], temperature=temperature)
    syllableStartIdx = round(syllables[,'start']*samplingRate/1000)
    syllableStartIdx[1] = 1
    if (!is.na(breathingAnchors) && breathingAnchors$time[1] != 0){ # if noise is added before the voiced part of each syllable (negative time anchors) or starts later than the voiced part, we need to shift noise insertion points
      shift = -round(breathingAnchors$time[1]*samplingRate/1000)
      if (breathingAnchors$time[1]<0){
        syllableStartIdx[1] = syllableStartIdx-shift # only the first syllableStartIdx is shifted, because that changes the sound length and the remaining syllableStartIdx, if any, are already shifted appropriately
      } else {
        syllableStartIdx = syllableStartIdx-shift # shift for each syllable
      }
    }


    # START OF SYLLABLE GENERATION
    voiced = vector()
    unvoiced = list()
    breathingAnchors_syl = list()

    for (s in 1:nrow(syllables)){
      # wiggle par values for this particular syllable, making sure they are within the permitted range for each variable
      pitchAnchors_per_syl = pitchAnchors
      amplAnchors_per_syl = amplAnchors

      if (temperature>0) { # OR if (temperature>0 & nrow(syllables)>1) if you don't want to mess with single-syllable vocalizations
        for (p in 1:length(pars_to_vary)){
          l = permittedValues[pars_to_vary[p], 'low']
          h = permittedValues[pars_to_vary[p], 'high']
          pars_syllable[pars_to_vary[p]] = rnorm_bounded(n=1, mean=as.numeric(pars_list[pars_to_vary[p]]), low=l, high=h, sd=(h-l)*temperature/10, roundToInteger=(pars_to_vary[p] %in% pars_to_round))  # /10 to have less variation in the spectral pars vs. duration of separate syllables
        }
        pitchAnchors_per_syl = wiggleAnchors(df=pitchAnchors_per_syl, temperature=temperature, low=c(0,permittedValues['pitch','low']), high=c(1, permittedValues['pitch','high']), temp_coef=pitchAnchorsWiggle_per_temp)
        if (wiggleBreathing) breathingAnchors_syl[[s]] = wiggleAnchors(df=breathingAnchors, temperature=temperature, low=c(-Inf,permittedValues['breathing_ampl','low']), high=c(+Inf, permittedValues['breathing_ampl','high']), wiggleAllRows=T, temp_coef=breathingAnchorsWiggle_per_temp)
        if (wiggleAmpl_per_syl) amplAnchors_per_syl = wiggleAnchors(df=amplAnchors_per_syl, temperature=temperature, low=c(0,0), high=c(1, -throwaway_dB), temp_coef=amplAnchorsWiggle_per_temp)
      }

      # generate smooth pitch contour for this particular syllable
      dur_syl = as.numeric (syllables[s,'end']-syllables[s,'start'])
      pitchContour_syl = getSmoothContour(anchors=pitchAnchors_per_syl, len=round(dur_syl*pitchSamplingRate/1000), samplingRate=pitchSamplingRate, ampl_floor=pitch_floor, ampl_ceiling=pitch_ceiling, thisIsPitch=T) * pitchDeltas[s] # plot (pitchContour_syl, type='l')

      # generate the voiced part
      if (dur_syl<permittedValues['sylDur_mean','low'] | (!is.na(breathingAnchors) && min(breathingAnchors$ampl) >= 40) ){ # only synthesize voiced part if breathing is weaker than 40 dB and the voiced part is long enough to bother synthesizing it
        syllable = rep(0, round(dur_syl*samplingRate/1000))
      } else {
        syllable = try ( do.call(generateSyllable,c(list(pitch=pitchContour_syl, amplAnchors=amplAnchors_per_syl), pars_syllable)) )  # the actual synthesis is here
      }
      # spectro_denoised (syllable, samplingRate=samplingRate); playme(syllable, samplingRate=samplingRate)
      # if (class(syllable)=='try-error') stop (print ('Failed to generate the new syllable!'))
      # if (sum(is.na(syllable))>0) stop (print('The new syllable contains NA values!'))

      # generate pause for all but the last syllable
      if (s < nrow(syllables)) {
        pause = rep(0, floor( (syllables[s+1,1]-syllables[s,2])*samplingRate/1000 ))
      } else {
        pause = numeric()
      }

      # add syllable and pause to the growing sound
      voiced = c(voiced, syllable, pause)

      # generate the unvoiced part, but don't add it to the sound just yet
      if (!is.na(breathingAnchors) && sum(breathingAnchors$ampl>throwaway_dB)>0){
        # adjust breathingAnchors$time to match the actual syllable duration
        breathingAnchors_syl[[s]] = breathingAnchors
        breathingAnchors_syl[[s]]$time[breathingAnchors_syl[[s]]$time>0] = breathingAnchors_syl[[s]]$time[breathingAnchors_syl[[s]]$time>0] * dur_syl/sylDur_mean # negative time anchors are not changed - the pre-aspiration length is constant, regardless of the actual syllable duration. However, positive time anchors are proportional to the actual syllable duration re the average expected duration (which the user sees in the UI when choosing time anchors)
        unvoicedDur_syl = round ( diff(range(breathingAnchors_syl[[s]]$time)) * samplingRate/1000 )

        # calculate noise spectrum
        if (is.na(exactFormants_unvoiced)){
          spectralEnvelope_unvoiced = NA
        } else {
          movingFormants = max(unlist(lapply(exactFormants_unvoiced, nrow)))>1 | sum(mouthAnchors$ampl != .5)>0 # are noise formants moving, as opposed to constant?
          nInt = ifelse (movingFormants, round(diff(range(breathingAnchors_syl[[s]]$time))/10), 1) # the number of different noise spectra, allowing one column (noise spectrum) per 10 ms of audio
          spectralEnvelope_unvoiced = getSpectralEnvelope(nr=windowLength_points/2, nc=nInt, exactFormants=exactFormants_unvoiced, formantStrength=formantStrength, extraFormants_ampl=extraFormants_ampl, rolloff_lip=rolloff_lip, mouthAnchors=mouthAnchors, temperature=temperature, samplingRate=samplingRate, vocalTract_length=vocalTract_length) # image(t(spectralEnvelope_unvoiced))
        }

        # synthesize the unvoiced part
        unvoiced[[s]] = getBreathing (len=unvoicedDur_syl, breathingAnchors=breathingAnchors_syl[[s]], rolloff_breathing=rolloff_breathing, attackLen=attackLen, samplingRate=samplingRate, windowLength_points=windowLength_points, overlap=overlap, filter_breathing=spectralEnvelope_unvoiced) # plot(unvoiced[[s]], type='l')
      }
    }
    # END OF SYLLABLE GENERATION
    # plot (voiced, type='l')
    # spectro_denoised (voiced, samplingRate=samplingRate, osc=T); playme(voiced, samplingRate=samplingRate)

    # if the unvoiced noise is of type "breathing" (the same formants as in the voiced part), we mix voiced+unvoiced BEFORE filtering the sound, otherwise we filter first and then mix voiced+unvoiced
    sound = voiced
    if (length(unvoiced)>0 && is.na(exactFormants_unvoiced)){
      for (s in 1:length(unvoiced)){
        sound = addVectors(sound, unvoiced[[s]], insertionPoint=syllableStartIdx[s])
      }
    }
    # plot (sound, type='l')
    # spectro_denoised (sound, samplingRate=samplingRate); playme(sound, samplingRate=samplingRate)

    # for polysyllabic vocalizations, apply amplitude envelope (if specified) over the entire bout and normalize to -1...+1
    if (!is.na(amplAnchors_global) && length(which(amplAnchors_global$ampl < -throwaway_dB))>0){
      amplAnchors_global$ampl = 2^(amplAnchors_global$ampl/10) # convert from dB to linear multiplier
      amplEnvelope = getSmoothContour(anchors=amplAnchors_global, len=length(sound), ampl_floor=0, ampl_ceiling=-throwaway_dB, samplingRate=samplingRate) # plot(amplEnvelope)
      sound = sound * amplEnvelope
    }

    # prepare vocal tract filter (formants + some spectral noise + lip radiation) (consider moving to generateBout to accommodate multiple syllables with pauses while still allowing variable, not constant, formants)
    if (sum(sound)==0){ # ie if we didn't synthesize a voiced syllable (unvoiced part only) - otherwise fft glitches
      sound_filtered = sound
    } else {
      windowLength_points = min(windowLength_points, floor(length(sound)/2)) # for very short sounds, make sure the analysis window is no more than half the sound's length
      step = seq(1, max(1, (length(sound)-windowLength_points)), windowLength_points - (overlap * windowLength_points/100))
      nc = length(step) # number of windows for fft
      nr = windowLength_points/2 # number of frequency bins for fft
      movingFormants = max(unlist(lapply(exactFormants, nrow)))>1 | sum(mouthAnchors$ampl != .5)>0 # are formants moving or constant?
      nInt = ifelse (movingFormants, nc, 1)
      spectralEnvelope = getSpectralEnvelope(nr=nr, nc=nInt, exactFormants=exactFormants, formantStrength=formantStrength, extraFormants_ampl=extraFormants_ampl, rolloff_lip=rolloff_lip, mouthAnchors=mouthAnchors, temperature=temperature, samplingRate=samplingRate, vocalTract_length=vocalTract_length)
      # image(t(spectralEnvelope))

      # fft and filtering
      z <- seewave::stft(wave=as.matrix(sound), f=samplingRate, wl=windowLength_points, zp=0, step=step, wn='hamming', fftw=F, scale=T, complex=T)
      if (movingFormants){
        z = z * spectralEnvelope
      } else {
        z = apply (z, 2, function(x)x*spectralEnvelope)
      }

      # inverse fft
      sound_filtered = as.numeric (seewave::istft(z, f=samplingRate, ovlp=overlap, wl=windowLength_points, output="matrix"))
      sound_filtered = sound_filtered / max(sound_filtered) # normalize
    }
    # spectro_denoised (sound_filtered, samplingRate=samplingRate); playme(sound_filtered, samplingRate=samplingRate)

    # add the separately filtered noise back into the sound at the appropriate time points AFTER filtering the sound
    if (length(unvoiced)>0 && !is.na(exactFormants_unvoiced)){
      for (s in 1:length(unvoiced)){
        sound_filtered = addVectors(sound_filtered, unvoiced[[s]], insertionPoint=syllableStartIdx[s])
      }
    } # plot(sound_filtered, type='l')

    # trill - rapid regular amplitude modulation
    if (trill_dep>0){
      trill = 1-sin (2*pi*(1:length(sound_filtered))/samplingRate*trill_freq) * trill_dep/2 # plot (trill, type='l')
    } else {
      trill = 1
    }
    sound_filtered = sound_filtered * trill

    # grow bout
    if (b==1){
      bout = sound_filtered
    } else {
      bout = c(bout, rep(0,pauseDur_mean*samplingRate/1000), sound_filtered)
    }
  }

  # add some silence before and after the entire bout
  if (!is.na(addSilence)) {
    n = round(samplingRate/1000*addSilence)
    bout = c ( rep(0,n), bout, rep(0,n) )
  }

  if (playSound){
    playme (bout, samplingRate=samplingRate) # spectro_denoised (sound_filtered, samplingRate=samplingRate, osc=T)
  }
  if (!is.na(savePath)){
    seewave::savewav(bout, filename = savePath, f = samplingRate)
  }
  if (plotSpectro){
    spectro_denoised (bout, samplingRate = samplingRate)
  }
  return (bout)
}

